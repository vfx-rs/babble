#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::*;

use crate::error::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExceptionSpecificationKind {
    /// The cursor has no exception specification.
    None,
    /// The cursor has exception specification throw()
    DynamicNone,
    /// The cursor has exception specification throw(T1, T2)
    Dynamic,
    /// The cursor has exception specification throw(...).
    MSAny,
    /// The cursor has exception specification basic noexcept.
    BasicNoexcept,
    /// The cursor has exception specification computed noexcept.
    ComputedNoexcept,
    /// The exception specification has not yet been evaluated.
    Unevaluated,
    /// The exception specification has not yet been instantiated.
    Uninstantiated,
    /// The exception specification has not been parsed yet.
    Unparsed,
    /// The cursor has a __declspec(nothrow) exception specification.
    NoThrow,
}

impl ExceptionSpecificationKind {
    /// Does this kind represent a definite noexcept?
    pub fn is_noexcept(&self) -> bool {
        use ExceptionSpecificationKind::*;
        match self {
            BasicNoexcept | NoThrow => true,
            _ => false,
        }
    }
}

impl TryFrom<CXCursor_ExceptionSpecificationKind> for ExceptionSpecificationKind {
    type Error = crate::error::Error;

    fn try_from(value: CXCursor_ExceptionSpecificationKind) -> Result<Self, Self::Error> {
        match value {
            CXCursor_ExceptionSpecificationKind_None => Ok(ExceptionSpecificationKind::None),
            CXCursor_ExceptionSpecificationKind_DynamicNone => {
                Ok(ExceptionSpecificationKind::DynamicNone)
            }
            CXCursor_ExceptionSpecificationKind_Dynamic => Ok(ExceptionSpecificationKind::Dynamic),
            CXCursor_ExceptionSpecificationKind_MSAny => Ok(ExceptionSpecificationKind::MSAny),
            CXCursor_ExceptionSpecificationKind_BasicNoexcept => {
                Ok(ExceptionSpecificationKind::BasicNoexcept)
            }
            CXCursor_ExceptionSpecificationKind_ComputedNoexcept => {
                Ok(ExceptionSpecificationKind::ComputedNoexcept)
            }
            CXCursor_ExceptionSpecificationKind_Unevaluated => {
                Ok(ExceptionSpecificationKind::Unevaluated)
            }
            CXCursor_ExceptionSpecificationKind_Uninstantiated => {
                Ok(ExceptionSpecificationKind::Uninstantiated)
            }
            CXCursor_ExceptionSpecificationKind_Unparsed => {
                Ok(ExceptionSpecificationKind::Unparsed)
            }
            CXCursor_ExceptionSpecificationKind_NoThrow => Ok(ExceptionSpecificationKind::NoThrow),
            _ => Err(Error::InvalidExceptionSpecificationKind),
        }
    }
}
