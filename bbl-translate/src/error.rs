use backtrace::Backtrace;
use bbl_clang::cursor::USR;

#[derive(Debug)]
pub enum Error {
    FailedToTranslateClass{
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToTranslateClassTemplateSpecialization {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToTranslateEnum {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToTranslateFunctionProto {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    TranslateFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    TranslateMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    TranslateSpecializedMethod {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    TranslateField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToGetClassFromRef(USR),
    FailedToTranslateType {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    Extraction(bbl_extract::error::Error),
    Clang(bbl_clang::error::Error),
    ClassNotFound {
        name: String,
        backtrace: Backtrace,
    },
    FunctionNotFound {
        name: String,
        backtrace: Backtrace,
    },
    RefNotFound {
        usr: USR,
        backtrace: Backtrace,
    },
    FailedToFormatField {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToFormatStruct {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToFormatFunction {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToCastArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToFormatArgument {
        name: String,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToGetQualifiedName {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToGetBindKind {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    FailedToTranslateTypedef {
        usr: USR,
        source: Box<dyn std::error::Error + 'static + Send + Sync>,
    },
    TemplateParmNotFound {
        name: String,
        backtrace: Backtrace,
    },
    TemplateArgNotFound {
        name: String,
        backtrace: Backtrace,
    },
    InvalidTemplateArgumentKind {
        name: String,
        backtrace: Backtrace,
    },
    Unsupported {
        description: String,
    },
    TriedToTranslateTemplateParmeter {
        name: String,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::FailedToTranslateClass{ name, .. } => {
                write!(f, "Failed to translate class {name}")
            }
            Error::FailedToTranslateClassTemplateSpecialization { name, .. } => {
                write!(f, "Failed to translate class template specialization {name}")
            }
            Error::FailedToTranslateEnum { name, .. } => {
                write!(f, "Failed to translate enum {name}")
            }
            Error::FailedToTranslateFunctionProto { name, .. } => {
                write!(f, "Failed to translate function proto {name}")
            }
            Error::TranslateFunction { name, .. } => {
                write!(f, "Failed to translate function {name}")
            }
            Error::TranslateMethod { name, .. } => {
                write!(f, "Failed to translate method {name}")
            }
            Error::TranslateSpecializedMethod { name, .. } => {
                write!(f, "Failed to translate specialized method {name}")
            }
            Error::TranslateField { name, .. } => write!(f, "Failed to translate field {name}"),
            Error::FailedToGetClassFromRef(usr) => {
                write!(f, "Failed to get a class with USR {usr}")
            }
            Error::FailedToTranslateType { name, .. } => {
                write!(f, "Failed to translate type {name}")
            }
            Error::Extraction(_) => write!(f, "Extraction error"),
            Error::Clang(_) => write!(f, "Clang error"),
            Error::ClassNotFound { name, .. } => write!(f, "Could not find class {name}"),
            Error::FunctionNotFound { name, .. } => write!(f, "Could not find function {name}"),
            Error::RefNotFound { usr, .. } => write!(f, "Could not find ref with usr {usr}"),
            Error::FailedToFormatField { name, .. } => write!(f, "Failed to format field {name}"),
            Error::FailedToFormatStruct { name, .. } => write!(f, "Failed to format struct {name}"),
            Error::FailedToFormatFunction { name, .. } => {
                write!(f, "Failed to format function {name}")
            }
            Error::FailedToFormatArgument { name, .. } => {
                write!(f, "Failed to format argument {name}")
            }
            Error::FailedToCastArgument { name, .. } => {
                write!(f, "Failed to cast argument {name}")
            }
            Error::FailedToGetQualifiedName { usr, .. } => {
                write!(f, "Failed to get qualified name from usr {usr}")
            }
            Error::FailedToGetBindKind { usr, .. } => {
                write!(f, "Failed to get bind kind from usr {usr}")
            }
            Error::FailedToTranslateTypedef { usr, .. } => {
                write!(f, "Failed to translate typedef from usr {usr}")
            }
            Error::TemplateParmNotFound { name, .. } => {
                write!(f, "Could not find template parameter {name}")
            }
            Error::TemplateArgNotFound { name, .. } => {
                write!(f, "Could not find template argument {name}")
            }
            Error::InvalidTemplateArgumentKind { name, .. } => {
                write!(f, "Invalid template argument kind {name}")
            }
            Error::Unsupported { description } => {
                write!(f, "Unsupported: {description}")
            }
            Error::TriedToTranslateTemplateParmeter { name } => {
                write!(f, "Tried to translate a template parameter: {name}")
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use Error::*;
        match self {
            TranslateFunction { source, .. }
            | TranslateMethod { source, .. }
            | TranslateSpecializedMethod { source, .. }
            | TranslateField { source, .. }
            | FailedToTranslateType { source, .. }
            | FailedToFormatField { source, .. }
            | FailedToFormatArgument { source, .. }
            | FailedToFormatStruct { source, .. }
            | FailedToFormatFunction { source, .. }
            | FailedToGetQualifiedName { source, .. }
            | FailedToGetBindKind { source, .. }
            | FailedToCastArgument { source, .. }
            | FailedToTranslateClass { source, .. }
            | FailedToTranslateClassTemplateSpecialization { source, .. }
            | FailedToTranslateEnum { source, .. }
            | FailedToTranslateFunctionProto { source, .. }
            | FailedToTranslateTypedef { source, .. } => Some(source.as_ref()),
            Extraction(e) => Some(e),
            Clang(e) => Some(e),
            _ => None,
        }
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

impl From<bbl_extract::error::Error> for Error {
    fn from(e: bbl_extract::error::Error) -> Self {
        Error::Extraction(e)
    }
}

impl From<bbl_clang::error::Error> for Error {
    fn from(e: bbl_clang::error::Error) -> Self {
        Error::Clang(e)
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
