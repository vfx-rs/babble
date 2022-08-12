use std::fmt::Display;

use clang_sys::{CXDiagnostic, clang_defaultDiagnosticDisplayOptions, clang_formatDiagnostic};

use crate::CXStringEx;

pub struct Diagnostic {
    pub(crate) inner: CXDiagnostic,
}

impl Diagnostic {
    pub fn format(&self) -> String {
        unsafe {
            let opt = clang_defaultDiagnosticDisplayOptions();
            clang_formatDiagnostic(self.inner, opt).to_string()
        }
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}