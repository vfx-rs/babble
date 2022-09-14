#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use crate::qualtype::QualType;

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
};

pub struct TemplateArgument {
    pub name: String,
    pub tty: TemplateType,
}

#[derive(Clone)]
pub enum TemplateType {
    Type(QualType),
    Integer(String),
    // .. more here later...
}

impl Display for TemplateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateType::Type(qt) => write!(f, "Type({qt})"),
            TemplateType::Integer(value) => write!(f, "Integer({value})"),
        }
    }
}

impl Debug for TemplateType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

/// A template parameter as defined in a class- or function-template declaration
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
