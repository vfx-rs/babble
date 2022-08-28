#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::*;

use crate::error::Error;

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
