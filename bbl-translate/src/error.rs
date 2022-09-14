use bbl_clang::cursor::USR;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Could not translate function \"{name}\"")]
    TranslateFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Could not translate field \"{name}\"")]
    TranslateField {
        name: String,
        source: TranslateTypeError,
    },
    #[error("Failed to get class from ref \"{0}\"")]
    FailedToGetClassFromRef(USR),
    #[error("Failed to translate type \"{name}\"")]
    FailedToTranslateType {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Extraction error")]
    Extraction(#[from] bbl_extract::error::Error),
    #[error("Clang error")]
    Clang(#[from] bbl_clang::error::Error),
    #[error("Could not find class \"{0}\"")]
    ClassNotFound(String),
    #[error("Could not find function \"{0}\"")]
    FunctionNotFound(String),
    #[error("Could not find a struct or typedef for \"{0}\"")]
    RefNotFound(USR),
    #[error("Failed to format field \"{name}\"")]
    FailedToFormatField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to format struct \"{name}\"")]
    FailedToFormatStruct {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to format function \"{name}\"")]
    FailedToFormatFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to format argument \"{name}\"")]
    FailedToFormatArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to get qualified name for \"{usr}\"")]
    FailedToGetQualifiedName {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum TranslateTypeError {
    #[error("Could not find template parameter \"{0}\"")]
    TemplateParmNotFound(String),
    #[error("Could not find template argument \"{0}\"")]
    TemplateArgNotFound(String),
    #[error("Unsupported template argument kind \"{0}\"")]
    InvalidTemplateArgumentKind(String),
}
