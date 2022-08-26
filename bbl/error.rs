#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::{CXErrorCode, CXError_Success};

use crate::cursor::USR;
#[derive(Debug)]
pub enum Error {
    InvalidCursor,
    InvalidType,
    InvalidTemplateArgumentKind,
    InvalidAccessSpecifier,
    TypeUnexposed,
    Failure,
    Crashed,
    InvalidArguments,
    ASTReadError,
    NulError(std::ffi::NulError),
    InvalidPath,
    IoError(std::io::Error),
    ParseError,
    RecordNotFound,
    NamespaceNotFound,
    MethodNotFound,
    MultipleMatches,
    FunctionNotFound(String),
    ClassOrNamespaceNotFound(USR),
    TranslateFunction {
        name: String,
        source: TranslateArgumentError,
    },
    TranslateField {
        name: String,
        source: TranslateTypeError,
    },
}

#[derive(Debug)]
pub struct TranslateArgumentError {
    pub name: String,
    pub source: TranslateTypeError,
}

impl std::fmt::Display for TranslateArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => write!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for TranslateArgumentError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.source)
    }
}

#[derive(Debug)]
pub enum TranslateTypeError {
    TemplateParmNotFound(String),
    TemplateArgNotFound(String),
    InvalidTemplateArgumentKind(String),
}

impl std::error::Error for TranslateTypeError {}

impl std::fmt::Display for TranslateTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => write!(f, "{:?}", self),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => write!(f, "{:?}", self),
        }
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

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::TranslateField { source, .. } => {
                Some(source)
            }
            Self::TranslateFunction { source, .. } => Some(source),
            _ => None,
        }
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
