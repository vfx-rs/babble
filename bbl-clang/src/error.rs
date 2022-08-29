#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::{CXErrorCode, CXError_Success};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    InvalidCursor,
    InvalidType,
    InvalidTemplateArgumentKind,
    InvalidAccessSpecifier,
    InvalidExceptionSpecificationKind,
    TypeUnexposed,
    Failure,
    Crashed,
    InvalidArguments,
    ASTReadError,
    NulError(#[from] std::ffi::NulError),
    InvalidPath,
    IoError(#[from] std::io::Error),
    ParseError,
    ClangBinaryNotFound,
    FailedToRunClang(std::io::Error),
    NonUTF8Output(std::string::FromUtf8Error),
    FailedToParseOutput(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait CXErrorCodeEx {
    fn cxerror(&self) -> CXErrorCode;

    fn to_result(&self) -> Result<(), Error> {
        match self.cxerror() as i32 {
            CXError_Success => Ok(()),
            1 => Err(Error::Failure),
            2 => Err(Error::Crashed),
            3 => Err(Error::InvalidArguments),
            4 => Err(Error::ASTReadError),
            _ => unreachable!(),
        }
    }
}

impl CXErrorCodeEx for CXErrorCode {
    fn cxerror(&self) -> CXErrorCode {
        *self
    }
}
