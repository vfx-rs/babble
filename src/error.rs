

#[derive(Debug)]
pub enum Error {
    InvalidCursor,
    InvalidType,
    InvalidTemplateArgumentKind,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => write!(f, "{:?}", self)
        }
    }
}

impl std::error::Error for Error {
    
}