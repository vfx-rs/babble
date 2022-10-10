#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::access_specifier::AccessSpecifier;
use bbl_clang::cli_args;
use bbl_clang::cursor::{CurClassDecl, Cursor, USR};
use bbl_clang::translation_unit::TranslationUnit;
use log::*;
use std::fmt::{Debug, Display};
use tracing::instrument;
use ustr::Ustr;
use backtrace::Backtrace;

use crate::ast::{get_namespaces_for_decl, get_qualified_name, MethodId, TypeAliasId, AST, ClassTemplateSpecializationId, dump_cursor, dump_cursor_until};
use crate::function::{extract_method, MethodTemplateSpecialization};
use crate::index_map::IndexMapKey;
use crate::{parse_string_and_extract_ast, AllowList, class};
use crate::qualtype::extract_type;
use crate::stdlib::{create_std_string, create_std_vector, create_std_unique_ptr, create_std_function};
use crate::templates::{TemplateParameterDecl, TemplateArgument, extract_class_template_specialization};
use crate::{function::Method, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
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

#[derive(Default)]
pub struct NeedsImplicit {
    pub ctor: bool,
    pub copy_ctor: bool,
    pub move_ctor: bool,
    pub copy_assign: bool,
    pub move_assign: bool,
    pub dtor: bool,
}

impl Debug for NeedsImplicit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}{}{}{}{}{}]",
            if self.ctor {"ctor "} else {""},
            if self.copy_ctor {"cctor "} else {""},
            if self.move_ctor {"mctor "} else {""},
            if self.copy_assign {"cass "} else {""},
            if self.move_assign {"mass "} else {""},
            if self.dtor {"dtor "} else {""},
        )
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
    pub(crate) specializations: Vec<ClassTemplateSpecializationId>,

    pub(crate) ignore: bool,
    pub(crate) rename: Option<String>,
    pub(crate) bind_kind: ClassBindKind,
    pub(crate) specialized_methods: Vec<MethodTemplateSpecialization>,
    pub(crate) is_pod: bool,

    pub needs_implicit: NeedsImplicit,
}

impl std::fmt::Debug for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ClassDecl {usr} {name} rename={rename:?} {bind_kind:?} is_pod={pod} ignore={ignore} needs={needs:?} template_parameters={template_parameters:?} specializations={specializations:?} namespaces={namespaces:?}", 
            usr=self.usr, 
            name=self.name(),
            rename = self.rename,
            bind_kind = self.bind_kind(),
            pod = self.is_pod,
            ignore = self.ignore,
            needs = self.needs_implicit,
            specializations = self.specializations,
            template_parameters = self.template_parameters(),
            namespaces = self.namespaces(),
        )?;

        for field in self.fields() {
            writeln!(f, "Field {:?}", field)?;
        }

        for method in self.methods() {
            writeln!(f, "{:?}", method)?;
        }

        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
impl ClassDecl {
    pub fn new(
        usr: USR,
        name: String,
        fields: Vec<Field>,
        methods: Vec<Method>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        is_pod: bool,
        needs_implicit: NeedsImplicit,
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
            needs_implicit,
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

    pub fn needs_implicit_ctor(&self) -> bool {
        self.needs_implicit.ctor
    }

    pub fn needs_implicit_copy_ctor(&self) -> bool {
        self.needs_implicit.copy_ctor
    }

    pub fn needs_implicit_move_ctor(&self) -> bool {
        self.needs_implicit.move_ctor
    }

    pub fn needs_implicit_dtor(&self) -> bool {
        self.needs_implicit.dtor
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
        class_template_args: Option<&[TemplateArgument]>,
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

    pub fn format(&self, ast: &AST, template_args: Option<&[TemplateArgument]>) -> String {
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
        template_args: Option<&[TemplateArgument]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| {
                ast.get_namespace(*u)
                    .unwrap_or_else(|| panic!("Failed to get namespace with usr {u}"))
                    .name
                    .clone()
            })
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

                Err(Error::MethodNotFound{name: signature.to_string(), backtrace: Backtrace::new()})
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

                Err(Error::MultipleMatches{name: signature.to_string(), backtrace: Backtrace::new()})
            }
        }
    }

    pub fn find_methods(&self, ast: &AST, signature: &str) -> Result<(Vec<MethodId>, Vec<&Method>)> {
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

                Err(Error::MethodNotFound{name: signature.to_string(), backtrace: Backtrace::new()})
            }
            _ => {
                Ok(
                    matches.into_iter().map(|m| {
                        (MethodId::new(m.0), m.1)
                    }).unzip()
                )
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
        template_arguments: Vec<TemplateArgument>,
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

#[instrument(skip(tu, ast, already_visited), level = "trace")]
pub fn extract_class_decl(
    class_decl: CurClassDecl,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    allow_list: &AllowList,
) -> Result<USR> {
    // Check for std:: types we're going to extract manually here
    if class_decl.display_name().starts_with("basic_string<") {
        debug!("Extracting basic_string {}", class_decl.usr());
        return create_std_string(class_decl.into(), ast, already_visited);
    } else if class_decl.display_name().starts_with("vector<") {
        return create_std_vector(class_decl, ast, already_visited, tu, allow_list);
    } else if class_decl.display_name().starts_with("unique_ptr<") {
        return create_std_unique_ptr(class_decl, ast, already_visited, tu, allow_list);
    } else if class_decl.display_name().starts_with("function<") {
        unreachable!("std::function should have been extracted in type extraction");
        return create_std_function(class_decl, ast, already_visited, tu, allow_list);
    }

    if class_decl.specialized_template().is_ok() {
        // this is a class template specialization, handle it separately
        debug!("extract_class_decl: {} is a CTS", class_decl.usr());
        return extract_class_template_specialization(class_decl, already_visited, ast, tu, allow_list);
    }

    if already_visited.contains(&class_decl.usr()) {
        debug!("extract_class_decl: already visited {}", class_decl.usr());
        return Ok(class_decl.usr());
    } else {
        already_visited.push(class_decl.usr());
    }

    let cd = extract_class_decl_inner(class_decl, tu, ast, already_visited, allow_list, None)?;

    debug!("extract_class_decl: inserting {}", class_decl.usr());
    if class_decl.kind() == CursorKind::ClassTemplate && cd.template_parameters.is_empty() {
        error!("class {:?} is a ClassTemplate but has no template parameters. is def: {}", class_decl, class_decl.is_definition());
    }

    ast.insert_class(cd);

    Ok(class_decl.usr())
}

fn extract_class_decl_inner(class_decl: CurClassDecl, tu: &TranslationUnit, ast: &mut AST, already_visited: &mut Vec<USR>, allow_list: &AllowList, qname_override: Option<&str>) -> Result<ClassDecl> {
    let namespaces = get_namespaces_for_decl(class_decl.into(), tu, ast, already_visited)?;
    let class_decl_qualified_name = get_qualified_name(&class_decl.display_name(), &namespaces, ast)?;

    debug!(
        "extract_class_decl({}) {}",
        class_decl.usr(),
        class_decl_qualified_name,
    );

    // First, trawl the bases for all their methods we'll want to inherit
    let mut base_methods = Vec::new();
    let mut base_constructors = Vec::new();
    for c_base in class_decl.children_of_kind(CursorKind::CXXBaseSpecifier, false) {
        if let Ok(c_base_decl) = c_base.referenced() {
            match c_base_decl.kind() {
                CursorKind::ClassDecl | CursorKind::StructDecl | CursorKind::ClassTemplate => {
                    // we pass a class name override through when extracting the base class members. This will turn 
                    // Base::method() into Class::method(). Thus the allow list will work as expected, and still allow
                    // us to avoid adding the Base class to the AST while lofting up its members into Class
                    let base = extract_class_decl_inner(c_base_decl.try_into()?, tu, ast, already_visited, allow_list, qname_override.or(Some(&class_decl_qualified_name)) )?;
                    let access = c_base.cxx_access_specifier()?;

                    for method in &base.methods {
                        if method.is_any_constructor() {
                            // we store constructors regardless of their access since we want to know about private constructors
                            // in order to not generate implicit versions for them
                            base_constructors.push(method.clone());
                        } else if (!method.is_destructor() && access == AccessSpecifier::Public) {
                            base_methods.push(method.clone());
                        }
                    }
                }
                CursorKind::TypeAliasTemplateDecl => {
                    dump_cursor_until(class_decl.into(), tu, 2);
                    unimplemented!("cannot handle base of TypeAliasTemplateDecl {c_base_decl:?}");
                }
                _ => {
                    dump_cursor(class_decl.into(), tu);
                    unimplemented!("cannot handle base {c_base_decl:?}");
                }
            }
        }
    }

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();

    let class_name = class_decl.spelling();
    let members = class_decl.children();
    let mut index = 0;
    let mut has_private_fields = false;

    let needs_implicit = NeedsImplicit {
        ctor: class_decl.cxxrecord_needs_implicit_default_constructor(),
        copy_ctor: class_decl.cxxrecord_needs_implicit_copy_constructor(),
        move_ctor: class_decl.cxxrecord_needs_implicit_move_constructor(),
        copy_assign: class_decl.cxxrecord_needs_implicit_copy_assignment(),
        move_assign: class_decl.cxxrecord_needs_implicit_move_assignment(),
        dtor: class_decl.cxxrecord_needs_implicit_destructor(),
    };

    for member in members {
        let member_qualified_name = if let Some(qname) = qname_override {
            // allow the subclass to override the qualified name when extracting so we can pull stuff up in the allow list
            format!("{}::{}", qname, member.display_name())
        } else {
            format!("{}::{}", class_decl_qualified_name, member.display_name())
        };

        debug!("member {}", member_qualified_name);
        // always allow template parameters so we don't have to explicitly allow them
        // if !allow_list.allows(&member_qualified_name) && !matches!(member.kind(), CursorKind::TemplateTemplateParameter | CursorKind::TemplateTypeParameter | CursorKind::NonTypeTemplateParameter) {
        //     debug!("  denied");
        //     continue;
        // }


        if let Ok(access) = member.cxx_access_specifier() {
            // if access != AccessSpecifier::Public {
            //     continue;
            // }
        } else {
            warn!("Failed to get access specifier for member {}", member.display_name());
            // continue;
            // return Err(Error::FailedToGetAccessSpecifierFor(member.display_name()));
        }
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
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
                    if access == AccessSpecifier::Public && allow_list.allows(&member_qualified_name){
                        match extract_method(
                            member,
                            &template_parameters,
                            already_visited,
                            tu,
                            ast,
                            allow_list,
                            &class_name,
                        ) {
                            Ok(method) => methods.push(method),
                            Err(e) => {
                                return Err(Error::FailedToExtractMethod {
                                    name: member.display_name(),
                                    source: Box::new(e),
                                })
                            }
                        }
                    }
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor{name: member.display_name(), backtrace: Backtrace::new()});
                }
            }
            CursorKind::FieldDecl => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                    } else {
                        has_private_fields = true;
                        // don't extract private fields because we don't want them for the API and they probably contain
                        // all sorts of nastiness.
                        continue;
                    }
                    debug!("    field {}", member.display_name());
                    let field = extract_field(
                        member,
                        &template_parameters,
                        already_visited,
                        ast,
                        tu,
                        allow_list,
                    )
                    .map_err(|e| {
                        Error::FailedToExtractField {
                            class: class_decl.display_name(),
                            name: member.display_name(),
                            source: Box::new(e),
                        }
                    })?;
                    fields.push(field);
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor{name: member.display_name(), backtrace: Backtrace::new()});
                }
            }
            _ => {
                debug!("  {member:?}");
                for child in member.children() {
                    debug!("    {child:?}");
                    match child.kind() {
                        CursorKind::TypeRef => {
                            if let Ok(c) = child.referenced() {
                                debug!("    -> {c:?}");
                            }
                        }
                        CursorKind::ParmDecl => {
                            if let Ok(ty) = child.ty() {
                                debug!("      type {ty:?}")
                            }

                            for c in child.children() {
                                debug!("      {c:?}");
                            }
                        }
                        CursorKind::CompoundStmt => {
                            for stmt in child.children() {
                                debug!("      {stmt:?}");
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    // add the inherited methods if they're not overriden in this class
    'outer: for base_method in base_methods {
        for method in &methods {
            if method.signature_matches(&base_method) {
                continue 'outer;
            }
        }

        methods.push(base_method);
    }

    // do the same thing for constructors and update the ROF
    'outer: for base_constructor in base_constructors {
        for method in &methods {
            if method.signature_matches(&base_constructor) {
                continue 'outer;
            }
        }

        methods.push(base_constructor);
    }


    let name = class_decl.spelling();

    // If this is a template decl we won't be able to get a type from it so just set POD to false
    // Note that we have a slightly different definition of POD from cpp: we do not consider records with private fields
    // to be POD as they're not correctly representable in C.
    // TODO(AL): well, actually... they're not in C but they are in Rust. Though if you have a private field you probably
    // have a non-trivial constructor, but we should probably not force that here
    let is_pod = if template_parameters.is_empty() {
        match class_decl.ty() {
            Ok(ty) => ty.is_pod() && !has_private_fields,
            Err(e) => {
                error!("Could not get type from {class_decl:?}");
                false
            }
        }
    } else {
        false
    };

    let cd = ClassDecl::new(
        class_decl.usr(),
        class_decl.spelling(),
        fields,
        methods,
        namespaces,
        template_parameters,
        is_pod,
        needs_implicit,
    );

    Ok(cd)
}

// TODO(AL): fix these once we've patched libclang
fn method_is_copy_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}

fn method_is_move_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}

pub fn extract_field(
    c_field: Cursor,
    class_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<Field> {
    let template_parameters = class_template_parameters
        .iter()
        .map(|t| t.name().to_string())
        .collect::<Vec<_>>();

    let ty = c_field.ty()?;
    let qual_type = extract_type(
        ty,
        &template_parameters,
        already_visited,
        ast,
        tu,
        allow_list,
    )?;

    Ok(Field {
        name: c_field.display_name(),
        qual_type,
    })
}

pub struct Field {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name(), self.qual_type())
    }
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
        class_template_args: Option<&[TemplateArgument]>,
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
    args: Option<&[TemplateArgument]>,
) -> String {
    if let Some(args) = args {
        if let Some(arg) = args.get(decl.index()) {
            match arg {
                TemplateArgument::Type(qt) => return qt.name.clone(),
                TemplateArgument::Integral(i) => return format!("{i}"),
                _ => format!("{arg:?}")
            };
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

#[cfg(test)]

mod tests {
    use bbl_clang::cli_args;
    use indoc::indoc;

    use crate::{class::ClassBindKind, error::Error, parse_string_and_extract_ast, AllowList, init_log};

    #[test]
    fn extract_pod() -> Result<(), Error> {
        // test that a POD extracts as a valuetype
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Class {
            public:
                int a;
                float b;
            };
        "#
            ),
            &cli_args()?,
            true,
            None,
            &AllowList::default(),
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
        ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
        Field a: int
        Field b: float

    "#
            )
        );

        let class_id = ast.find_class("Class")?;
        let class = &ast.classes()[class_id];
        assert!(matches!(class.bind_kind(), ClassBindKind::ValueType));

        Ok(())
    }

    #[test]
    fn extract_non_pod() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            // test that adding a private field to a POD forces opaqueptr
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                    int a;
                public:
                    float b;
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
    
        "#
                )
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }

    #[test]
    fn extract_non_pod2() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            // test that adding a constructor forces non-pod
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                public:
                    Class();
                    float b;
                };
                }
        
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
                    Method DefaultConstructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@Class# Class rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

        "#
                )
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }

    #[test]
    fn extract_inherited1() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    namespace detail {
                        class Base {
                            int a;
                        public:
                            float b;
                            Base() = delete;
                            Base(int a, float b);
                            void base_do_thing();
                            virtual void do_thing();
                        };
                    }

                    class Class : public detail::Base {
                    public:
                        float c;
                        void derived_do_thing();
                        void do_thing() override;
                    };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![
                    "^Class$".to_string(),
                    "^Class.*$".to_string()
                    ]),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@N@detail detail None
                    Namespace c:@N@detail@S@Base Base None
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor ] template_parameters=[] specializations=[] namespaces=[]
                    Field c: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@derived_do_thing# derived_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=true pure_virtual=false specializations=[] Function c:@S@Class@F@do_thing# do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method DefaultConstructor deleted=true const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base# Base rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base#I#f# Base rename=Some("ctor") ignore=false return=void args=[a: int, b: float] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]

        "#
                )
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));
            Ok(())
        })
    }

    #[test]
    fn extract_inherited2() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Base {
                    int a;
                    Base();
                    Base(const Base&);
                    Base(Base&&);
                public:
                    float b;
                    void base_do_thing();
                };

                class Class : public Base {
                public:
                    float c;
                    void derived_do_thing();
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Base Base None
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Base Base rename=None OpaquePtr is_pod=false ignore=false needs=[dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Base]

                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field c: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@derived_do_thing# derived_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Base]

        "#
                )
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }
}
