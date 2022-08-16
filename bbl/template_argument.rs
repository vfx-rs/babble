#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::*;

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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TemplateArgumentKind {
    Null,
    Type,
    Declaration,
    NullPtr,
    Integral,
    Template,
    TemplateExpansion,
    Expression,
    Pack,
}

impl TryFrom<CXTemplateArgumentKind> for TemplateArgumentKind {
    type Error = Error;

    fn try_from(value: CXTemplateArgumentKind) -> Result<Self, Self::Error> {
        if value == CXTemplateArgumentKind_Invalid {
            Err(Error::InvalidTemplateArgumentKind)
        } else {
            Ok(match value {
                CXTemplateArgumentKind_Null => TemplateArgumentKind::Null,
                CXTemplateArgumentKind_Type => TemplateArgumentKind::Type,
                CXTemplateArgumentKind_Declaration => TemplateArgumentKind::Declaration,
                CXTemplateArgumentKind_NullPtr => TemplateArgumentKind::NullPtr,
                CXTemplateArgumentKind_Integral => TemplateArgumentKind::Integral,
                CXTemplateArgumentKind_Template => TemplateArgumentKind::Template,
                CXTemplateArgumentKind_TemplateExpansion => TemplateArgumentKind::TemplateExpansion,
                CXTemplateArgumentKind_Expression => TemplateArgumentKind::Expression,
                CXTemplateArgumentKind_Pack => TemplateArgumentKind::Pack,
                _ => unreachable!(),
            })
        }
    }
}

impl From<TemplateArgumentKind> for CXTemplateArgumentKind {
    fn from(k: TemplateArgumentKind) -> Self {
        match k {
            TemplateArgumentKind::Null => CXTemplateArgumentKind_Null,
            TemplateArgumentKind::Type => CXTemplateArgumentKind_Type,
            TemplateArgumentKind::Declaration => CXTemplateArgumentKind_Declaration,
            TemplateArgumentKind::NullPtr => CXTemplateArgumentKind_NullPtr,
            TemplateArgumentKind::Integral => CXTemplateArgumentKind_Integral,
            TemplateArgumentKind::Template => CXTemplateArgumentKind_Template,
            TemplateArgumentKind::TemplateExpansion => CXTemplateArgumentKind_TemplateExpansion,
            TemplateArgumentKind::Expression => CXTemplateArgumentKind_Expression,
            TemplateArgumentKind::Pack => CXTemplateArgumentKind_Pack,
        }
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
