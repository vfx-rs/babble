#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::*;

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use std::convert::TryFrom;

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
            Ok(
                match value {
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
                }
            )
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