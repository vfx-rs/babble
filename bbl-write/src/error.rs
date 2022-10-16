use bbl_clang::{cursor::USR, ty::TypeKind};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Clang error")]
    ClangError(#[from] bbl_clang::error::Error),
    #[error("Extraction error")]
    ExtractError(#[from] bbl_extract::error::Error),
    #[error("Translation error")]
    TranslationError(#[from] bbl_translate::error::Error),
    #[error("Failed to generate function \"{name}\"")]
    FailedToGenerateFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to generate typedef \"{name}\"")]
    FailedToGenerateTypedef {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to generate function signature \"{name}\"")]
    FailedToGenerateSignature {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to generate function call \"{name}\"")]
    FailedToGenerateCall {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to generate function argument \"{name}\"")]
    FailedToGenerateArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("String formatting error while generating")]
    FormatError(#[from] std::fmt::Error),
    #[error("Failed to generate cmake project")]
    FailedToGenerateCMake {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to run cmake")]
    FailedToRunCMake(#[from] std::io::Error),
    #[error("CMake configuration failed: \n{stdout}\n\n{stderr}")]
    FailedToConfigureCMake { stdout: String, stderr: String },
    #[error("CMake build failed: \n{stdout}\n\n{stderr}")]
    FailedToBuildCMake { stdout: String, stderr: String },
    #[error("CMake installation failed: \n{stdout}\n\n{stderr}")]
    FailedToInstallCMake { stdout: String, stderr: String },
    #[error("Failed to find type from USR {0}")]
    FailedToFindTyperef(USR),
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
    },
}

#[derive(Debug, thiserror::Error)]
pub enum FunctionGenerationError {
    #[error("Failed to generate function signature")]
    FailedToGenerateSignature(TypeError),
    #[error("Failed to generate cpp function call")]
    FailedToGenerateCall(TypeError),
    #[error("Failed to function argument \"{name}\"")]
    FailedToGenerateArg { name: String, source: ArgumentError },
    #[error("String formatting error while generating")]
    FormatError(#[from] std::fmt::Error),
}
