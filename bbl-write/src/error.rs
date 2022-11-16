use bbl_clang::cursor::USR;
use bbl_util::Trace;

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
    #[error("Failed to generate result type")]
    FailedToGenerateResultType {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to generatefunction prototype")]
    FailedToGenerateFunctionProto {
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
    #[error("Failed to generate field \"{name}\"")]
    FailedToGenerateField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("String formatting error while generating")]
    FormatError(#[from] std::fmt::Error),
    #[error("Failed to generate cmake project")]
    FailedToGenerateCMake {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to run cmake in {cwd} with args {args:?}")]
    FailedToRunCMake {
        cwd: String,
        args: Vec<String>,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("CMake configuration failed: \n{stdout}\n\n{stderr}")]
    FailedToConfigureCMake { stdout: String, stderr: String },
    #[error("CMake build failed: \n{stdout}\n\n{stderr}")]
    FailedToBuildCMake { stdout: String, stderr: String },
    #[error("CMake installation failed: \n{stdout}\n\n{stderr}")]
    FailedToInstallCMake { stdout: String, stderr: String },
    #[error("Failed to find type from USR {usr}")]
    FailedToFindTyperef { usr: USR, source: Trace },
    #[error("Failed to get qualified name from class \"{name}\"")]
    FailedToGetQualifiedName {
        name: String,
        source: bbl_extract::error::Error,
    },
    #[error("Failed to write rust module to path \"{module_path}")]
    FailedToWriteRustModule {
        module_path: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
}
