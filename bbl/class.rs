use log::*;
use std::fmt::Display;

use crate::Cursor;
use crate::ast::AST;
use crate::cursor_kind::CursorKind;
use crate::function::extract_method;
use crate::{cursor::USR, qualtype::QualType, function::Method};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;


pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<QualType>,
    pub(crate) methods: Vec<Method>,
}

impl ClassDecl {
    pub fn pretty_print(&self, depth: usize, binding: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        println!("{indent}class {} {{", self.name);

        for method in &self.methods {
            method.pretty_print(depth + 1, binding);
        }

        println!("{indent}}}");
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}


pub fn extract_class_decl(class_decl: Cursor, depth: usize) -> ClassDecl {
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
                if let Ok(method) = extract_method(member, depth + 1, &[]) {
                    methods.push(method);
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

    ClassDecl {
        usr: class_decl.usr(),
        name: class_decl.spelling(),
        fields,
        methods,
    }
}