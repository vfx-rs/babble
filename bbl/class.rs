#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::{
    CX_CXXAccessSpecifier, CX_CXXInvalidAccessSpecifier, CX_CXXPrivate, CX_CXXProtected,
    CX_CXXPublic,
};
use log::*;
use std::fmt::Display;

use crate::ast::AST;
use crate::cursor_kind::CursorKind;
use crate::function::extract_method;
use crate::qualtype::extract_type;
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use crate::{cursor::USR, function::Method, qualtype::QualType};
use crate::{error, Cursor};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Method>,
    pub(crate) namespaces: Vec<USR>,

    ignore: bool,
    rename: Option<String>,
}

impl ClassDecl {
    pub fn new(
        usr: USR,
        name: String,
        fields: Vec<Field>,
        methods: Vec<Method>,
        namespaces: Vec<USR>,
    ) -> ClassDecl {
        ClassDecl {
            usr,
            name,
            fields,
            methods,
            namespaces,
            ignore: false,
            rename: None,
        }
    }

    pub fn set_ignore(&mut self, ignore: bool) {
        self.ignore = true;
    }

    pub fn set_rename(&mut self, name: &str) {
        self.rename = Some(name.to_string());
    }

    pub fn method(
        &self,
        signature: &str,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> Result<USR> {
        for method in &self.methods {
            if method.format(ast, class_template_parameters, class_template_args) == signature {
                return Ok(method.function.usr.clone());
            }
        }

        Err(Error::MethodNotFound)
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) {
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
                class_template_parameters,
                class_template_args,
            );
        }

        for field in &self.fields {
            println!(
                "{indent}{indent}{};",
                field.format(ast, class_template_parameters, class_template_args)
            );
        }

        println!("{indent}}}");
    }

    pub fn format(&self, ast: &AST) -> String {
        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        format!("{ns_string}::{}", self.name)
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub fn extract_class_decl(class_decl: Cursor, depth: usize, namespaces: &Vec<USR>) -> ClassDecl {
    let indent = format!("{:width$}", "", width = depth * 2);

    debug!("{indent}extract_class_decl({})", class_decl.usr());

    let mut methods = Vec::new();
    let mut fields = Vec::new();

    let members = class_decl.children();
    for member in members {
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
                let t = member.display_name();
                warn!("Found TemplateTypeParameter {} on ClassDecl", t);
            }
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                        if let Ok(method) = extract_method(member, depth + 1, &[]) {
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
                        let field = extract_field(member, depth, &[]);
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
        class_decl.usr(),
        class_decl.spelling(),
        fields,
        methods,
        namespaces.clone(),
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
