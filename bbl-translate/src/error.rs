use backtrace::Backtrace;
use bbl_clang::cursor::USR;

use bbl_util::Trace;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Failed to translate class \"{name}\"")]
    FailedToTranslateClass {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate class template specialization \"{name}\"")]
    FailedToTranslateClassTemplateSpecialization {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate enum \"{name}\"")]
    FailedToTranslateEnum {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate function prototype \"{name}\"")]
    FailedToTranslateFunctionProto {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate function \"{name}\"")]
    TranslateFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate return type")]
    TranslateResult {
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate argument \"{name}\"")]
    TranslateArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate method \"{name}\"")]
    TranslateMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate specialized method \"{name}\"")]
    TranslateSpecializedMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate field \"{name}\"")]
    TranslateField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to get class from USR {usr}")]
    FailedToGetClassFromRef { usr: USR, source: Trace },
    #[error("Failed to translate type \"{name}\"")]
    FailedToTranslateType {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to create expression for argument \"{name}\"")]
    FailedToCreateArgumentExpr {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Tried to pass a type by value that could not be")]
    ImproperPassByValue(Trace),
    #[error("bbl-extract error")]
    Extraction(#[from] bbl_extract::error::Error),
    #[error("bbl-clang error")]
    Clang(#[from] bbl_clang::error::Error),
    #[error("formatting error")]
    Format(#[from] std::fmt::Error),
    #[error("Could not find class \"{name}\"")]
    ClassNotFound { name: String, source: Trace },
    #[error("Could not find function \"{name}\"")]
    FunctionNotFound { name: String, source: Trace },
    #[error("Could not find ref \"{usr}\"")]
    RefNotFound { usr: USR, source: Trace },
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
    #[error("Failed to cast argument \"{name}\"")]
    FailedToCastArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to format argument \"{name}\"")]
    FailedToFormatArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to get qualified name for {usr}")]
    FailedToGetQualifiedName {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to get bind kind for {usr}")]
    FailedToGetBindKind {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Failed to translate typedef {usr}")]
    FailedToTranslateTypedef {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    #[error("Could not find template parameter \"{name}\"")]
    TemplateParmNotFound { name: String, source: Trace },
    #[error("Could not find template argument \"{name}\"")]
    TemplateArgNotFound { name: String, source: Trace },
    #[error("Invalid kind on template argument \"{name}\"")]
    InvalidTemplateArgumentKind { name: String, source: Trace },
    #[error("Unsupported feature: \"{description}\"")]
    Unsupported { description: String, source: Trace },
    #[error("Tried to translate an unspecialized template parameter \"{name}\"")]
    TriedToTranslateTemplateParmeter { name: String, source: Trace },
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

#[derive(Debug)]
pub enum TranslateTypeError {
    TemplateParmNotFound { name: String, backtrace: Backtrace },
    TemplateArgNotFound { name: String, backtrace: Backtrace },
    InvalidTemplateArgumentKind { name: String, backtrace: Backtrace },
}

impl std::fmt::Display for TranslateTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TemplateParmNotFound { name, .. } => {
                write!(f, "Could not find template parameter {name}")
            }
            Self::TemplateArgNotFound { name, .. } => {
                write!(f, "Could not find template argument {name}")
            }
            Self::InvalidTemplateArgumentKind { name, .. } => {
                write!(f, "Invalid template argument kind {name}")
            }
        }
    }
}

impl std::error::Error for TranslateTypeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}
