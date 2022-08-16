use crate::{
    ast::AST, class::ClassDecl, cursor_kind::CursorKind, function::extract_method,
    template_argument::{TemplateParameterDecl, TemplateType}, Cursor, TranslationUnit, cursor::USR,
};
use log::*;
use std::fmt::Display;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct ClassTemplate {
    pub(crate) class_decl: ClassDecl,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
}

impl ClassTemplate {
    pub fn pretty_print(&self, depth: usize, binding: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let template_decl = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "template <typename {}>\n{indent}",
                self.template_parameters
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(", typename ")
            )
        };

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        println!(
            "{indent}{template_decl}class {}{template} {{",
            self.class_decl.name
        );

        for method in &self.class_decl.methods {
            method.pretty_print(depth + 1, binding);
        }

        println!("{indent}}}");
    }
}

impl Display for ClassTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.class_decl.name)?;
        if !self.template_parameters.is_empty() {
            write!(
                f,
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

pub fn extract_class_template(
    class_template: Cursor,
    depth: usize,
    tu: &TranslationUnit,
) -> ClassTemplate {
    let indent = format!("{:width$}", "", width = depth * 2);

    debug!("{indent}extract_class_template({})", class_template.usr());

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();

    let members = class_template.children();
    for member in members {
        debug!("member {:?}", member);
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
                // TODO: Doesn't seem to be a way to get a type default
                let name = member.display_name();
                template_parameters.push(TemplateParameterDecl::Type { name });
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
                            })
                        }
                        _ => unimplemented!(),
                    }
                }
            }
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(method) = extract_method(member, depth + 1, &template_parameters) {
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

    ClassTemplate {
        class_decl: ClassDecl {
            usr: class_template.usr(),
            name: class_template.spelling(),
            fields,
            methods,
        },
        template_parameters,
    }
}

pub struct ClassTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones. 
    /// 
    /// Revisit and maybe we want to make that a hard error
    pub(crate) args: Vec<Option<TemplateType>>,
}
