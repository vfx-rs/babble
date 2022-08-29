use bbl_clang::{cursor::USR, ty::TypeKind};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Clang error")]
    ClangError(#[from] bbl_clang::error::Error),
    #[error("Extraction error")]
    ExtractError(#[from] bbl_extract::error::Error),
    #[error("Failed to generate function \"{name}\"")]
    FailedToGenerateFunction {
        name: String,
        source: FunctionGenerationError,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("Could not find type ref from \"{0}\"")]
    TypeRefNotFound(USR),
    #[error("Unknown type \"{0:?}\"")]
    UnknownType(TypeKind),
    #[error("Failed to get qualified name from class \"{name}\"")]
    FailedToGetQualifiedName {
        name: String,
        source: bbl_extract::error::Error,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum ArgumentError {
    #[error("Error resolving type")]
    TypeError(#[from] TypeError),
}

#[derive(Debug, thiserror::Error)]
pub enum StructGenerationError {
    #[error("Failed to generate field \"{name}\"")]
    FailedToGenerateField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FunctionGenerationError {
    #[error("Failed to generate function signature")]
    FailedToGenerateSignature(TypeError),
    #[error("Failed to generate cpp function call")]
    FailedToGenerateCall(TypeError),
    #[error("Failed to function argument \"{name}\"")]
    FailedToGenerateArg { name: String, source: ArgumentError },
}

