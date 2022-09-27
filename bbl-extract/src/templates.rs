#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::{
    cursor::{CurClassDecl, CurClassTemplate, USR},
    template_argument::TemplateArgumentKind,
    translation_unit::TranslationUnit,
};
use tracing::log::{trace, debug};

use crate::{
    ast::{get_namespaces_for_decl, get_qualified_name, AST},
    class::{extract_class_decl, ClassBindKind},
    qualtype::{extract_type, QualType}, AllowList,
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
};

pub fn extract_class_template_specialization(
    c_class_decl: CurClassDecl,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<USR> {
    trace!("extract_class_template_specialization: {c_class_decl:?}");
    if already_visited.contains(&c_class_decl.usr()) {
        return Ok(c_class_decl.usr());
    } else {
        already_visited.push(c_class_decl.usr());
    }

    let template_arguments = extract_template_args(c_class_decl, already_visited, ast, tu, allow_list)?;
    let namespaces = get_namespaces_for_decl(c_class_decl.into(), tu, ast, already_visited)?;

    let specialized_decl: CurClassTemplate = c_class_decl
        .specialized_template()
        .map_err(|_| Error::ClassDeclIsNotSpecialization(c_class_decl.usr()))?
        .try_into()?;
    debug!("extract_class_template_specialization: got specialized decl {specialized_decl:?}");

    extract_class_decl(
        specialized_decl.as_class_decl(),
        tu,
        ast,
        already_visited,
        allow_list,
    )?;

    let name = regex::Regex::new("(?:[^a-zA-Z0-9])+")
        .unwrap()
        .replace_all(&c_class_decl.display_name(), "_")
        .to_string();

    ast.insert_class_template_specialization(ClassTemplateSpecialization {
        specialized_decl: specialized_decl.usr(),
        usr: c_class_decl.usr(),
        name,
        template_arguments,
        namespaces,
    });

    Ok(c_class_decl.usr())
}

pub fn extract_template_args(
    c_class_decl: CurClassDecl,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<Vec<TemplateArgument>> {
    let mut result = Vec::new();
    let num_template_args = c_class_decl.num_template_arguments();
    if num_template_args < 1 {
        return Err(Error::TooFewTemplateArguments {
            usr: c_class_decl.usr(),
            num: num_template_args,
        });
    }

    for i in 0..num_template_args {
        match c_class_decl.template_argument_kind(i as u32)? {
            TemplateArgumentKind::Type => {
                let ty = c_class_decl.template_argument_type(i as u32)?;
                result.push(TemplateArgument::Type(extract_type(
                    ty,
                    &[],
                    already_visited,
                    ast,
                    tu,
                    allow_list,
                )?))
            }
            TemplateArgumentKind::Integral => result.push(TemplateArgument::Integral(
                c_class_decl.template_argument_value(i as u32),
            )),
            _ => unimplemented!("Not handling template arguments other than int or type"),
        }
    }

    Ok(result)
}

pub struct ClassTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) template_arguments: Vec<TemplateArgument>,
    pub(crate) namespaces: Vec<USR>,
}

impl Debug for ClassTemplateSpecialization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ClassTemplateSpecialization {usr} {name} specialized_decl={specialized_decl} template_arguments={template_arguments:?} namespaces={namespaces:?}",
            usr=self.usr,
            name=self.name(),
            specialized_decl = self.specialized_decl,
            template_arguments = self.template_arguments(),
            namespaces = self.namespaces(),
        )
    }
}

impl ClassTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[TemplateArgument] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn bind_kind(&self, ast: &AST) -> Result<ClassBindKind> {
        ast.get_class(self.specialized_decl)
            .map(|class| *class.bind_kind())
            .ok_or(Error::ClassNotFound(self.specialized_decl.to_string()))
    }

    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ ClassTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        let class = ast.get_class(self.specialized_decl).unwrap();
        class.pretty_print(depth, ast, Some(self.template_arguments()));
    }

    pub fn format(&self, ast: &AST) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let class = ast.get_class(self.specialized_decl).unwrap();
        class.format(ast, Some(self.template_arguments()))
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }
}

#[derive(Debug)]
pub struct FunctionTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) template_arguments: Vec<TemplateArgument>,
    pub(crate) namespaces: Vec<USR>,
}

impl FunctionTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[TemplateArgument] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ FunctionTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.pretty_print(
            depth,
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        );
    }

    pub fn format(&self, ast: &AST, outer_template_parameters: &[TemplateParameterDecl]) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.format(
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        )
    }
}

#[derive(Clone)]
pub enum TemplateArgument {
    Null,
    Type(QualType),
    Declaration,
    NullPtr,
    Integral(i64),
    Template,
    TemplateExpansion,
    Expression,
    Pack,
}

impl Debug for TemplateArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TemplateArgument::*;
        match self {
            Null => write!(f, "Null"),
            Type(qt) => write!(f, "{qt:?}"),
            Declaration => write!(f, "Declaration"),
            NullPtr => write!(f, "NullPtr"),
            Integral(i) => write!(f, "{i}"),
            Template => write!(f, "Template"),
            TemplateExpansion => write!(f, "TemplateExpansion"),
            Expression => write!(f, "Expression"),
            Pack => write!(f, "Pack"),
        }
    }
}

/// A template parameter as defined in a class- or function-template declaration
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TemplateParameterDecl {
    Type {
        name: String,
        index: usize,
    },
    Integer {
        name: String,
        index: usize,
        default: Option<String>,
    },
}

impl TemplateParameterDecl {
    pub fn typ(name: &str, index: usize) -> TemplateParameterDecl {
        TemplateParameterDecl::Type {
            name: name.into(),
            index,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            TemplateParameterDecl::Type { name, .. } => name,
            TemplateParameterDecl::Integer { name, .. } => name,
        }
    }

    pub fn default_name(&self) -> String {
        match self {
            TemplateParameterDecl::Type { name, .. } => format!("typename {name}"),
            TemplateParameterDecl::Integer { name, .. } => format!("integer {name}"),
        }
    }

    pub fn index(&self) -> usize {
        match self {
            TemplateParameterDecl::Type { index, .. } => *index,
            TemplateParameterDecl::Integer { index, .. } => *index,
        }
    }
}

impl Display for TemplateParameterDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateParameterDecl::Type { name, .. } => write!(f, "{name}"),
            TemplateParameterDecl::Integer { name, default, .. } => {
                write!(f, "{name}")?;
                if let Some(default) = default {
                    write!(f, "={default}")?;
                }
                Ok(())
            }
        }
    }
}

impl Debug for TemplateParameterDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateParameterDecl::Type { name, .. } => write!(f, "Type({name})"),
            TemplateParameterDecl::Integer { name, default, .. } => {
                write!(f, "Int({name}")?;
                if let Some(default) = default {
                    write!(f, "={default}")?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}
