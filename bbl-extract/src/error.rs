#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::cursor::USR;

#[derive(Debug)]
pub enum Error {
    ClangError(bbl_clang::error::Error),
    RecordNotFound,
    NamespaceNotFound(String),
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
    FailedToGetTemplateRefFrom(String),
    FailedToGetTypeFrom(String),
    NoMatchingTemplateParameter(String),
    IoError(std::io::Error),
}

impl From<bbl_clang::error::Error> for Error {
    fn from(e: bbl_clang::error::Error) -> Self {
        Error::ClangError(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IoError(e)
    }
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

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::ClangError(e) => Some(e),
            Self::IoError(e) => Some(e),
            Self::TranslateField { source, .. } => Some(source),
            Self::TranslateFunction { source, .. } => Some(source),
            _ => None,
        }
    }
}
