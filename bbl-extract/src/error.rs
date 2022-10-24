#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::cursor::USR;

use bbl_util::Trace;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("bbl-clang error")]
    ClangError(#[from] bbl_clang::error::Error),
    #[error("Could not find class \"{name}\"")]
    ClassNotFound { name: String, source: Trace },
    #[error("Could not find namespace \"{name}\"")]
    NamespaceNotFound { name: String, source: Trace },
    #[error("Could not find method \"{name}\"")]
    MethodNotFound { name: String, source: Trace },
    #[error("Found multiple matches for \"{name}\"")]
    MultipleMatches { name: String, source: Trace },
    #[error("Could not find function \"{name}\"")]
    FunctionNotFound { name: String, source: Trace },
    #[error("Could not find typedef \"{usr}\"")]
    TypedefNotFound { usr: USR, source: Trace },
    #[error("Could not find a class or namespace with USR \"{usr}\"")]
    ClassOrNamespaceNotFound { usr: USR, source: Trace },
    #[error("Failed to get a template ref from \"{name}\"")]
    FailedToGetTemplateRefFrom { name: String, source: Trace },
    #[error("Failed to get type from \"{name}\"")]
    FailedToGetTypeFrom { name: String, source: Trace },
    #[error("Failed to get a namespace ref from \"{name}\"")]
    FailedToGetNamespaceRefFrom { name: String, source: Trace },
    #[error("Failed to get cursor")]
    FailedToGetCursor { source: Trace },
    #[error("Failed to extract template arguments from \"{name}\"")]
    TemplateArgExtraction { name: String, source: Trace },
    #[error("No matching template parameter for argument \"{name}\"")]
    NoMatchingTemplateParameter { name: String, source: Trace },
    #[error("No matching template argument for parameter \"{name}\" as index {index}")]
    NoMatchingTemplateArgument {
        name: String,
        index: usize,
        source: Trace,
    },
    #[error("I/O error")]
    IoError(#[from] std::io::Error),
    #[error("Failed to extract class \"{usr}\"")]
    FailedToExtractClass {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract class template \"{usr}\"")]
    FailedToExtractClassTemplate {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract base class {base} of {usr}")]
    FailedToExtractBaseClass {
        base: USR,
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract enum \"{usr}\"")]
    FailedToExtractEnum {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract class template specialization \"{name}\"")]
    FailedToExtractClassTemplateSpecialization {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract argument \"{name}\"")]
    FailedToExtractArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract return value \"{name}\"")]
    FailedToExtractResult {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract template arguments")]
    FailedToExtractTemplateArgs {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract function \"{name}\"")]
    FailedToExtractFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract method \"{name}\"")]
    FailedToExtractMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract typedef \"{usr}\"")]
    FailedToExtractTypedef {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to extract type \"{name}\"")]
    FailedToExtractType {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to get qualified name for \"{name}\"")]
    FailedToGetQualifiedNameFor {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Tried to make class \"{name}\" a value type but it cannot be")]
    ClassCannotBeValueType { name: String, source: Trace },
    #[error("Failed to get access specifier for \"{name}\"")]
    FailedToGetAccessSpecifierFor { name: String, source: Trace },
    #[error("Class decl \"{usr}\" is not a specialization")]
    ClassDeclIsNotSpecialization { usr: USR, source: Trace },
    #[error("Too few template arguments ({num}) on \"{usr}\"")]
    TooFewTemplateArguments { usr: USR, num: i32, source: Trace },
    #[error("Failed to extract field \"{name}\" on \"{class}\"")]
    FailedToExtractField {
        class: String,
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Unsupported construct: {description}")]
    Unsupported { description: String, source: Trace },
}

impl Error {
    // returns true if the cause of this error is ultimately that there's an unsupported feature
    pub fn is_unsupported(&self) -> bool {
        if let crate::Error::Unsupported { .. } = self {
            return true;
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

// impl std::error::Error for Error {
//     fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//         use Error::*;
//         match self {
//             FailedToExtractClass { source, .. }
//             | FailedToExtractClassTemplateSpecialization { source, .. }
//             | FailedToExtractArgument { source, .. }
//             | FailedToExtractResult { source, .. }
//             | FailedToExtractTemplateArgs { source, .. }
//             | FailedToExtractFunction { source, .. }
//             | FailedToExtractMethod { source, .. }
//             | FailedToExtractTypeAlias { source, .. }
//             | FailedToGetQualifiedNameFor { source, .. }
//             | FailedToExtractField { source, .. } => Some(source.as_ref()),
//             ClangError(e) => Some(e),
//             IoError(e) => Some(e),
//             _ => None,
//         }
//     }
// }

// impl std::fmt::Display for Error {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{self:?}")
//     }
// }

// impl From<bbl_clang::error::Error> for Error {
//     fn from(e: bbl_clang::error::Error) -> Self {
//         Error::ClangError(e)
//     }
// }

// impl From<std::io::Error> for Error {
//     fn from(e: std::io::Error) -> Self {
//         Error::IoError(e)
//     }
// }
