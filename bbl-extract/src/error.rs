#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::cursor::USR;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Clang error")]
    ClangError(bbl_clang::error::Error),
    #[error("Could not find class \"{0}\" in AST")]
    ClassNotFound(String),
    #[error("Could not find namespace \"{0}\" in AST")]
    NamespaceNotFound(String),
    #[error("Could not find method")]
    MethodNotFound,
    #[error("Multiple matches for the given method name were found")]
    MultipleMatches,
    #[error("Could not find function \"{0}\" in AST")]
    FunctionNotFound(String),
    #[error("Could not find type alias \"{0}\" in AST")]
    TypeAliasNotFound(USR),
    #[error("Could not find a class or namespace with USR \"{0}\" in AST")]
    ClassOrNamespaceNotFound(USR),
    #[error("Could not translate function \"{name}\"")]
    TranslateFunction {
        name: String,
        source: TranslateArgumentError,
    },
    #[error("Could not translate field \"{name}\"")]
    TranslateField {
        name: String,
        source: TranslateTypeError,
    },
    #[error("Failed to get the template reference from \"{0}\"")]
    FailedToGetTemplateRefFrom(String),
    #[error("Failed to get the type from \"{0}\"")]
    FailedToGetTypeFrom(String),
    #[error("Failed to get the namespace reference from \"{0}\"")]
    FailedToGetNamespaceRefFrom(String),
    #[error("Failed to get cursor")]
    FailedToGetCursor,
    #[error("Failed to extract the template argument from \"{0}\"")]
    TemplateArgExtraction(String),
    #[error("Could not find a matching template parameter for template argument \"{0}\"")]
    NoMatchingTemplateParameter(String),
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
    #[error("Could not extract class")]
    FailedToExtractClass(#[from] ExtractClassError),
    #[error("Could not extract class template specialization \"{name}\"")]
    FailedToExtractClassTemplateSpecialization{name: String, source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract argument \"{name}\"")]
    FailedToExtractArgument{name: String, source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract result")]
    FailedToExtractResult{source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract template args")]
    FailedToExtractTemplateArgs{source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract function \"{name}\"")]
    FailedToExtractFunction{name: String, source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract method \"{name}\"")]
    FailedToExtractMethod{name: String, source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Could not extract typedef \"{0}\"")]
    FailedToExtractTypedef(USR),
    #[error("Could not extract type alias \"{name}\"")]
    FailedToExtractTypeAlias{name: String, source: Box<dyn std::error::Error + 'static + Send + Sync>},
    #[error("Failed to get qualified name for \"{name}\"")]
    FailedToGetQualifiedNameFor {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Cannot set class \"{0}\" to value type as one of its fields is not value type. Try fixing that first.")]
    ClassCannotBeValueType(String),
}

impl From<bbl_clang::error::Error> for Error {
    fn from(e: bbl_clang::error::Error) -> Self {
        Error::ClangError(e)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ExtractClassError {
    #[error("Failed to extract field \"{name}\" from class \"{class}\"")]
    FailedToExtractField {
        class: String,
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum TranslateArgumentError {
    #[error("Failed to translate type \"{name}\"")]
    FailedToTranslateType {
        name: String,
        source: TranslateTypeError,
    },
    #[error("Failed to get class from ref \"{0}\"")]
    FailedToGetClassFromRef(USR),
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
        write!(f, "{:?}", self)
    }
}

/*
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
*/