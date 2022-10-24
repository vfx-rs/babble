#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use crate::{compilation_database::CompilationDatabaseError, cursor_kind::CursorKind};
use bbl_util::Trace;
use clang_sys::{CXErrorCode, CXError_Success};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("libclang returned an invalid cursor")]
    InvalidCursor,
    #[error("Could not convert cursor from \"{from}\" to \"{to}\"")]
    FailedToConvertCursorKind {
        from: CursorKind,
        to: CursorKind,
        source: Trace,
    },
    #[error("libclang returned an invalid type\n")]
    InvalidType,
    #[error("Invalid template argument kind")]
    InvalidTemplateArgumentKind,
    #[error("Invalid access specifier")]
    InvalidAccessSpecifier,
    #[error("Invalid exception specification kind")]
    InvalidExceptionSpecificationKind,
    #[error("Type unexposed")]
    TypeUnexposed,
    #[error("libclang failure")]
    Failure,
    #[error("libclang crashed")]
    Crashed,
    #[error("libclang invalid arguments")]
    InvalidArguments,
    #[error("libclang AST read error")]
    ASTReadError,
    #[error("Null bytes in C string")]
    NulError(#[from] std::ffi::NulError),
    #[error("Invalid path")]
    InvalidPath,
    #[error("I/O Error")]
    IoError(#[from] std::io::Error),
    #[error("Parsing error")]
    ParseError,
    #[error("Could not find clang binary")]
    ClangBinaryNotFound,
    #[error("Failed to run clang process")]
    FailedToRunClang(std::io::Error),
    #[error("Failed to run cmake process")]
    FailedToRunCMake(std::io::Error),
    #[error("Tried to create a String from invalid UTF-8")]
    NonUTF8Output(#[from] std::string::FromUtf8Error),
    #[error("Failed to parse process output")]
    FailedToParseOutput(String),
    #[error("CMake failed:\n{stdout}\n{stderr}")]
    CMakeError { stdout: String, stderr: String },
    #[error("Error loading compilation database")]
    CompilationDatabaseError(#[source] CompilationDatabaseError),
    #[error("The given type is not a function where a function is expected")]
    TypeIsNotFunction,
}

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
