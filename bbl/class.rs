#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::{
    CX_CXXAccessSpecifier, CX_CXXInvalidAccessSpecifier, CX_CXXPrivate, CX_CXXProtected,
    CX_CXXPublic,
};
use log::*;
use std::fmt::Display;

use crate::ast::{MethodId, AST};
use crate::cursor_kind::CursorKind;
use crate::function::extract_method;
use crate::qualtype::extract_type;
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use crate::{cursor::USR, function::Method, qualtype::QualType};
use crate::{error, Cursor, TranslationUnit};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ClassBindKind {
    OpaquePtr,
    OpaqueBytes,
    ValueType,
}

pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Method>,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,

    pub(crate) ignore: bool,
    pub(crate) rename: Option<String>,
    pub(crate) bind_kind: ClassBindKind,

}

impl ClassDecl {
    pub fn new(
        usr: USR,
        name: String,
        fields: Vec<Field>,
        methods: Vec<Method>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
    ) -> ClassDecl {
        ClassDecl {
            usr,
            name,
            fields,
            methods,
            namespaces,
            template_parameters,
            ignore: false,
            rename: None,
            bind_kind: ClassBindKind::OpaquePtr,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn usr(&self) -> USR {
        self.usr
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

        println!("+ ClassDecl {}", self.usr);

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
                    .map(|t| specialize_template_parameter(t, template_args).to_string())
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
                    .map(|t| specialize_template_parameter(t, template_args).to_string())
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
                    .map(|t| specialize_template_parameter(t, template_args).to_string())
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
            1 => Ok((MethodId(matches[0].0), matches[0].1)),
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

    pub fn rename_method(&mut self, method_id: MethodId, new_name: &str) {
        self.methods[method_id.0].rename(new_name);
    }

    pub fn ignore_method(&mut self, method_id: MethodId) {
        self.methods[method_id.0].ignore();
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

// walk back up through a cursor's semantic parents and add them as namespaces
pub fn walk_namespaces(c: Result<Cursor>, namespaces: &mut Vec<USR>) {
    if let Ok(c) = c {
        if c.kind() != CursorKind::TranslationUnit { 
            namespaces.push(c.usr());
            walk_namespaces(c.semantic_parent(), namespaces);
        }
    }
}

pub fn get_namespaces_for_decl(c: Cursor) -> Vec<USR> {
    let mut namespaces = Vec::new();
    walk_namespaces(c.semantic_parent(), &mut namespaces);
    namespaces.reverse();
    namespaces
}

pub fn extract_class_decl(
    class_template: Cursor,
    depth: usize,
    tu: &TranslationUnit,
    namespaces: &Vec<USR>,
) -> ClassDecl {
    let indent = format!("{:width$}", "", width = depth * 2);

    debug!("{indent}extract_class_decl({})", class_template.usr());

    let namespaces = get_namespaces_for_decl(class_template);
    println!("SEM NS: {namespaces:?}");

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
                        if let Ok(method) = extract_method(member, depth + 1, &template_parameters)
                        {
                            methods.push(method);
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
                        let field = extract_field(member, depth, &template_parameters);
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

    ClassDecl::new(
        class_template.usr(),
        class_template.spelling(),
        fields,
        methods,
        namespaces.clone(),
        template_parameters,
    )
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AccessSpecifier {
    Public,
    Protected,
    Private,
}

impl TryFrom<CX_CXXAccessSpecifier> for AccessSpecifier {
    type Error = crate::error::Error;

    fn try_from(value: CX_CXXAccessSpecifier) -> Result<Self, Self::Error> {
        match value {
            CX_CXXInvalidAccessSpecifier => Err(Error::InvalidAccessSpecifier),
            CX_CXXPublic => Ok(AccessSpecifier::Public),
            CX_CXXProtected => Ok(AccessSpecifier::Protected),
            CX_CXXPrivate => Ok(AccessSpecifier::Private),
            _ => unreachable!(),
        }
    }
}

pub fn extract_field(
    c_field: Cursor,
    depth: usize,
    class_template_parameters: &[TemplateParameterDecl],
) -> Field {
    let indent = format!("{:width$}", "", width = depth * 2);

    let template_parameters = class_template_parameters
        .iter()
        .map(|t| t.name().to_string())
        .collect::<Vec<_>>();

    let ty = c_field.ty().unwrap();
    let qual_type = extract_type(ty, &template_parameters).unwrap();

    Field {
        name: c_field.display_name(),
        qual_type,
    }
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
