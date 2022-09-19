#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::{CXErrorCode, CXError_Success};

use crate::{compilation_database::CompilationDatabaseError, cursor_kind::CursorKind};

#[derive(Debug)]
pub enum Error {
    InvalidCursor,
    FailedToConvertCursorKind{from: CursorKind, to: CursorKind, backtrace: backtrace::Backtrace},
    InvalidType(backtrace::Backtrace),
    InvalidTemplateArgumentKind,
    InvalidAccessSpecifier,
    InvalidExceptionSpecificationKind,
    TypeUnexposed,
    Failure,
    Crashed,
    InvalidArguments,
    ASTReadError,
    NulError(std::ffi::NulError),
    InvalidPath,
    IoError(std::io::Error),
    ParseError,
    ClangBinaryNotFound,
    FailedToRunClang(std::io::Error),
    FailedToRunCMake(std::io::Error),
    NonUTF8Output(std::string::FromUtf8Error),
    FailedToParseOutput(String),
    CMakeError { stdout: String, stderr: String },
    CompilationDatabaseError(CompilationDatabaseError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<std::ffi::NulError> for Error {
    fn from(e: std::ffi::NulError) -> Self {
        Error::NulError(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IoError(e)
    }
}

impl std::error::Error for Error {}

impl From<CompilationDatabaseError> for Error {
    fn from(e: CompilationDatabaseError) -> Self {
        Error::CompilationDatabaseError(e)
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
