#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::cursor::USR;

use backtrace::Backtrace;

#[derive(Debug)]
pub enum Error {
    ClangError(bbl_clang::error::Error),
    ClassNotFound {
        name: String,
        backtrace: Backtrace,
    },
    NamespaceNotFound {
        name: String,
        backtrace: Backtrace,
    },
    MethodNotFound {
        name: String,
        backtrace: Backtrace,
    },
    MultipleMatches {
        name: String,
        backtrace: Backtrace,
    },
    FunctionNotFound {
        name: String,
        backtrace: Backtrace,
    },
    TypeAliasNotFound {
        usr: USR,
        backtrace: Backtrace,
    },
    ClassOrNamespaceNotFound {
        usr: USR,
        backtrace: Backtrace,
    },
    FailedToGetTemplateRefFrom {
        name: String,
        backtrace: Backtrace,
    },
    FailedToGetTypeFrom {
        name: String,
        backtrace: Backtrace,
    },
    FailedToGetNamespaceRefFrom {
        name: String,
        backtrace: Backtrace,
    },
    FailedToGetCursor {
        backtrace: Backtrace,
    },
    TemplateArgExtraction {
        name: String,
        backtrace: Backtrace,
    },
    NoMatchingTemplateParameter {
        name: String,
        backtrace: Backtrace,
    },
    NoMatchingTemplateArgument {
        name: String,
        index: usize,
        backtrace: Backtrace,
    },
    IoError(std::io::Error),
    FailedToExtractClass {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractClassTemplateSpecialization {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractResult {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractTemplateArgs {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToExtractTypedef {
        usr: USR,
        backtrace: Backtrace,
    },
    FailedToExtractTypeAlias {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToGetQualifiedNameFor {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    ClassCannotBeValueType {
        name: String,
        backtrace: Backtrace,
    },
    FailedToGetAccessSpecifierFor {
        name: String,
        backtrace: Backtrace,
    },
    ClassDeclIsNotSpecialization {
        usr: USR,
        backtrace: Backtrace,
    },
    TooFewTemplateArguments {
        usr: USR,
        num: i32,
        backtrace: Backtrace,
    },
    FailedToExtractField {
        class: String,
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    Unsupported {
        description: String,
    }
}

impl Error {
    // returns true if the cause of this error is ultimately that there's an unsupported feature
    pub fn is_unsupported(&self) -> bool {
        match self {
            crate::Error::Unsupported { .. } => return true,
            _ => (),
        }

        use std::error::Error;
        if let Some(e) = self.source() {
            if let Some(e) = e.downcast_ref::<crate::error::Error>() {
                e.is_unsupported()
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use Error::*;
        match self {
            FailedToExtractClass { source, .. }
            | FailedToExtractClassTemplateSpecialization { source, .. }
            | FailedToExtractArgument { source, .. }
            | FailedToExtractResult { source, .. }
            | FailedToExtractTemplateArgs { source, .. }
            | FailedToExtractFunction { source, .. }
            | FailedToExtractMethod { source, .. }
            | FailedToExtractTypeAlias { source, .. }
            | FailedToGetQualifiedNameFor { source, .. }
            | FailedToExtractField { source, .. } => Some(source.as_ref()),
            ClangError(e) => Some(e),
            IoError(e) => Some(e),
            _ => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
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
