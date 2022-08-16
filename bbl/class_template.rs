use crate::{
    ast::AST,
    class::{self, ClassDecl},
    cursor::USR,
    cursor_kind::CursorKind,
    function::extract_method,
    record::Record,
    template_argument::{TemplateParameterDecl, TemplateType},
    Cursor, TranslationUnit,
};
use log::debug;
use std::fmt::Display;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct ClassTemplate {
    pub(crate) class_decl: ClassDecl,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
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
            } else if let TemplateParameterDecl::Integer {
                default: Some(value),
                ..
            } = decl
            {
                // check if we have a non-type parameter with a default
                return value.clone();
            }
        }
    }

    decl.default_name()
}

impl ClassTemplate {
    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        template_args: Option<&[Option<TemplateType>]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        println!("+ ClassTemplate {}", self.class_decl.usr);

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
            "{indent}{template_decl}class {}{template} {{",
            self.class_decl.name
        );

        for method in &self.class_decl.methods {
            method.pretty_print(depth + 1, ast, &self.template_parameters, template_args);
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

impl ClassTemplateSpecialization {
    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .args
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();
        println!(
            "+ ClassTemplateSpecialization {} of ({}) with <{}>",
            self.name,
            self.usr,
            args.join(", ")
        );

        // this will be complicated...
        let rec = ast.records.get(&self.specialized_decl).unwrap();
        match rec {
            Record::ClassTemplate(ct) => ct.pretty_print(depth, ast, Some(&self.args)),
            _ => unreachable!(),
        }
    }
}
