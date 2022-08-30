#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::access_specifier::AccessSpecifier;
use bbl_clang::cursor::{Cursor, USR};
use bbl_clang::translation_unit::TranslationUnit;
use log::*;
use std::fmt::Display;
use ustr::Ustr;

use crate::ast::{
    get_namespaces_for_decl, get_qualified_name, IndexMapKey, MethodId, TypeAliasId, AST,
};
use crate::error::{self, ExtractClassError};
use crate::function::{extract_method, MethodTemplateSpecialization};
use crate::qualtype::extract_type;
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use crate::{function::Method, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ClassBindKind {
    OpaquePtr,
    OpaqueBytes,
    ValueType,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct MethodSpecializationId(pub usize);

impl MethodSpecializationId {
    pub fn new(id: usize) -> MethodSpecializationId {
        MethodSpecializationId(id)
    }
}

impl From<MethodSpecializationId> for usize {
    fn from(id: MethodSpecializationId) -> Self {
        id.0
    }
}

pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Method>,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
    /// List of the specializations made from this class if it is templated
    pub(crate) specializations: Vec<TypeAliasId>,

    pub(crate) ignore: bool,
    pub(crate) rename: Option<String>,
    pub(crate) bind_kind: ClassBindKind,
    pub(crate) specialized_methods: Vec<MethodTemplateSpecialization>,
    pub(crate) is_pod: bool,
}

impl ClassDecl {
    pub fn new(
        usr: USR,
        name: String,
        fields: Vec<Field>,
        methods: Vec<Method>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        is_pod: bool,
    ) -> ClassDecl {
        ClassDecl {
            usr,
            name,
            fields,
            methods,
            namespaces,
            template_parameters,
            specializations: Vec::new(),
            ignore: false,
            rename: None,
            bind_kind: if is_pod {
                ClassBindKind::ValueType
            } else {
                ClassBindKind::OpaquePtr
            },
            specialized_methods: Vec::new(),
            is_pod,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_pod(&self) -> bool {
        self.is_pod
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn template_parameters(&self) -> &[TemplateParameterDecl] {
        &self.template_parameters
    }

    pub fn is_templated(&self) -> bool {
        !self.template_parameters.is_empty()
    }

    pub fn is_specialized(&self) -> bool {
        !self.specializations.is_empty()
    }

    pub fn bind_kind(&self) -> &ClassBindKind {
        &self.bind_kind
    }

    /// Set the [`ClassBindKind`] of this class, which affects how it is represented in C.
    ///
    /// # Params
    /// * `bind_kind` - The [`ClassBindKind`] to use for this class
    /// * `force_members` - If `true`, will try to set any [`Field`]s of this class that are themselves classes to the
    /// same `bind_kind`.
    ///
    /// # Errors
    /// * [`Error::ClassHasNonValueTypeFields`] if `bind_kind` is [`ClassBindKind::ValueType`], `force_members_to_match`
    /// is false, and this class has fields which are non-value-type classes.
    /// * [`Error::ClassHasIncompatibleFields`] if `bind_kind` is [`ClassBindKind::ValueType`], `force_members_to_match`
    /// is true, but the field classes cannot be converted to value types.
    pub(crate) fn set_bind_kind(&mut self, bind_kind: ClassBindKind) {
        self.bind_kind = bind_kind
    }

    pub fn could_be_valuetype(&self, ast: &AST) -> Result<bool> {
        for field in &self.fields {
            if !field.qual_type().is_valuetype(ast)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub fn specialized_methods(&self) -> &[MethodTemplateSpecialization] {
        &self.specialized_methods
    }

    pub fn set_ignore(&mut self, ignore: bool) {
        self.ignore = true;
    }

    pub fn set_rename(&mut self, name: &str) {
        self.rename = Some(name.to_string());
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        class_template_args: Option<&[Option<TemplateType>]>,
    ) {
        if !self.template_parameters.is_empty() {
            self.pretty_print_template(depth, ast, class_template_args);
            return;
        }

        let indent = format!("{:width$}", "", width = depth * 2);

        let pod = if self.is_pod { "[POD]" } else { "" };

        println!("+ ClassDecl {} {}", self.usr, pod);

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        let name = if let Some(ref rename) = self.rename {
            rename
        } else {
            &self.name
        };

        println!("{indent}class {ns_string}::{} {{", name);

        for method in &self.methods {
            method.pretty_print(
                depth + 1,
                ast,
                &self.template_parameters,
                class_template_args,
            );
        }

        for field in &self.fields {
            println!(
                "{indent}{indent}{};",
                field.format(ast, &self.template_parameters, class_template_args)
            );
        }

        println!("{indent}}}");
    }

    pub fn format(&self, ast: &AST, template_args: Option<&[Option<TemplateType>]>) -> String {
        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        format!("{ns_string}::{}{template}", self.name)
    }

    fn pretty_print_template(
        &self,
        depth: usize,
        ast: &AST,
        template_args: Option<&[Option<TemplateType>]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!("+ ClassTemplate {}", self.usr);

        let template_decl = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "template <{}>\n{indent}",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        println!(
            "{indent}{template_decl}class {ns_string}::{}{template} {{",
            self.name
        );

        for method in &self.methods {
            method.pretty_print(depth + 1, ast, &self.template_parameters, template_args);
        }

        println!("{indent}}}");
    }

    pub fn find_method(&self, ast: &AST, signature: &str) -> Result<(MethodId, &Method)> {
        let mut matches = Vec::new();

        for (method_id, method) in self.methods.iter().enumerate() {
            if method
                .signature(ast, &self.template_parameters, None)
                .contains(signature)
            {
                matches.push((method_id, method));
            }
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.methods.len());
                for method in self.methods.iter() {
                    let sig = method.signature(ast, &self.template_parameters, None);
                    let dist = levenshtein::levenshtein(&sig, signature);
                    distances.push((dist, sig));
                }

                distances.sort_by(|a, b| a.0.cmp(&b.0));

                error!(
                    "Could not find method matching signature: \"{}\"",
                    signature
                );
                error!("Did you mean one of:");
                for (_, sug) in distances.iter().take(3) {
                    error!("  {sug}");
                }

                Err(Error::MethodNotFound)
            }
            1 => Ok((MethodId::new(matches[0].0), matches[0].1)),
            _ => {
                error!("Multiple matches found for signature \"{signature}\":");

                for (_, method) in matches {
                    error!(
                        "  {}",
                        method.signature(ast, &self.template_parameters, None)
                    );
                }

                Err(Error::MultipleMatches)
            }
        }
    }

    pub fn get_method(&self, id: MethodId) -> &Method {
        &self.methods[id.get()]
    }

    pub fn rename_method(&mut self, method_id: MethodId, new_name: &str) {
        self.methods[method_id.get()].rename(new_name);
    }

    pub fn ignore_method(&mut self, method_id: MethodId) {
        self.methods[method_id.get()].ignore();
    }

    pub fn specialize_method(
        &mut self,
        method_id: MethodId,
        name: &str,
        template_arguments: Vec<Option<TemplateType>>,
    ) -> Result<MethodSpecializationId> {
        let method_decl = &mut self.methods[method_id.get()];

        let usr = USR::new(&format!("{}_{name}", method_decl.usr().as_str()));

        let id = self.specialized_methods.len();

        let mts = MethodTemplateSpecialization {
            specialized_decl: method_id,
            usr,
            name: name.into(),
            template_arguments,
            namespaces: Vec::new(),
        };

        self.specialized_methods.push(mts);

        let id = MethodSpecializationId(id);

        method_decl.specializations.push(id);

        Ok(id)
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub fn extract_class_decl(
    class_template: Cursor,
    depth: usize,
    tu: &TranslationUnit,
    namespaces: &[USR],
) -> Result<ClassDecl> {
    let indent = format!("{:width$}", "", width = depth * 2);

    debug!("{indent}extract_class_decl({})", class_template.usr());

    let namespaces = get_namespaces_for_decl(class_template);

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();

    let members = class_template.children();
    let mut index = 0;
    for member in members {
        debug!("member {:?}", member);
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
                // TODO: Doesn't seem to be a way to get a type default
                let name = member.display_name();
                template_parameters.push(TemplateParameterDecl::Type { name, index });
                index += 1;
            }
            CursorKind::NonTypeTemplateParameter => {
                for child in &member.children() {
                    debug!("    memebr child {:?}", child);
                    match child.kind() {
                        CursorKind::IntegerLiteral => {
                            let name = member.display_name();

                            for lit_child in &child.children() {}

                            template_parameters.push(TemplateParameterDecl::Integer {
                                name,
                                default: Some(tu.token(child.location()).spelling()),
                                index,
                            });
                            index += 1;
                        }
                        _ => unimplemented!(),
                    }
                }
            }
            CursorKind::TemplateTemplateParameter => unimplemented!(),
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                        match extract_method(member, depth + 1, &template_parameters) {
                            Ok(method) => methods.push(method),
                            Err(e) => {
                                error!(
                                    "Could not extract method {member:?} from class {class_template:?}: {e:?}"
                                );
                            }
                        }
                    }
                } else {
                    error!(
                        "Could not get access specifier from member {}",
                        member.display_name()
                    );
                }
            }
            CursorKind::FieldDecl => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                        let field =
                            extract_field(member, depth, &template_parameters).map_err(|e| {
                                ExtractClassError::FailedToExtractField {
                                    class: class_template.display_name(),
                                    name: member.display_name(),
                                    source: Box::new(e),
                                }
                            })?;
                        fields.push(field);
                    }
                } else {
                    error!(
                        "Could not get access specifier from member {}",
                        member.display_name()
                    );
                }
            }
            _ => {
                debug!("{indent}  {member:?}");
                for child in member.children() {
                    debug!("{indent}    {child:?}");
                    match child.kind() {
                        CursorKind::TypeRef => {
                            if let Ok(c) = child.referenced() {
                                debug!("{indent}    -> {c:?}");
                            }
                        }
                        CursorKind::ParmDecl => {
                            if let Ok(ty) = child.ty() {
                                debug!("{indent}      type {ty:?}")
                            }

                            for c in child.children() {
                                debug!("{indent}      {c:?}");
                            }
                        }
                        CursorKind::CompoundStmt => {
                            for stmt in child.children() {
                                debug!("{indent}      {stmt:?}");
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    let name = class_template.spelling();

    // If this is a template decl we won't be able to get a type from it so just set POD to false
    let is_pod = if template_parameters.is_empty() {
        class_template.ty()?.is_pod()
    } else {
        false
    };

    debug!("Got new ClassDecl {name}");

    Ok(ClassDecl::new(
        class_template.usr(),
        class_template.spelling(),
        fields,
        methods,
        namespaces,
        template_parameters,
        is_pod,
    ))
}

pub fn extract_field(
    c_field: Cursor,
    depth: usize,
    class_template_parameters: &[TemplateParameterDecl],
) -> Result<Field> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let template_parameters = class_template_parameters
        .iter()
        .map(|t| t.name().to_string())
        .collect::<Vec<_>>();

    let ty = c_field.ty()?;
    let qual_type = extract_type(ty, &template_parameters)?;

    Ok(Field {
        name: c_field.display_name(),
        qual_type,
    })
}

pub struct Field {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Field {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn qual_type(&self) -> &QualType {
        &self.qual_type
    }

    fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        format!(
            "{}: {}",
            self.name,
            self.qual_type
                .format(ast, class_template_parameters, class_template_args)
        )
    }
}

/// Choose the type replacement for the give `TemplateParameterDecl`
pub(crate) fn specialize_template_parameter(
    decl: &TemplateParameterDecl,
    args: Option<&[Option<TemplateType>]>,
) -> String {
    if let Some(args) = args {
        if let Some(arg) = args.get(decl.index()) {
            if let Some(arg) = arg {
                match arg {
                    TemplateType::Type(name) => return name.to_string(),
                    TemplateType::Integer(name) => return name.clone(),
                };
            }
        } else if let TemplateParameterDecl::Integer {
            default: Some(value),
            ..
        } = decl
        {
            // check if we have a non-type parameter with a default
            return value.clone();
        }
    }

    decl.default_name()
}
