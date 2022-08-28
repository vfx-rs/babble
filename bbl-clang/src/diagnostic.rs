#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use std::fmt::Display;

use clang_sys::{
    clang_defaultDiagnosticDisplayOptions, clang_formatDiagnostic, clang_getDiagnosticSeverity,
    CXDiagnostic, CXDiagnosticSeverity, CXDiagnostic_Error, CXDiagnostic_Fatal,
    CXDiagnostic_Ignored, CXDiagnostic_Note, CXDiagnostic_Warning,
};

use crate::string::CXStringEx;

#[derive(Clone)]
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

    pub fn severity(&self) -> Severity {
        unsafe { clang_getDiagnosticSeverity(self.inner).into() }
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Ignored = 0,
    Note,
    Warning,
    Error,
    Fatal,
}

impl From<CXDiagnosticSeverity> for Severity {
    fn from(s: CXDiagnosticSeverity) -> Self {
        match s {
            CXDiagnostic_Ignored => Severity::Ignored,
            CXDiagnostic_Note => Severity::Note,
            CXDiagnostic_Warning => Severity::Warning,
            CXDiagnostic_Error => Severity::Error,
            CXDiagnostic_Fatal => Severity::Fatal,
            _ => unreachable!(),
        }
    }
}

impl From<Severity> for CXDiagnosticSeverity {
    fn from(s: Severity) -> Self {
        match s {
            Severity::Ignored => CXDiagnostic_Ignored,
            Severity::Note => CXDiagnostic_Note,
            Severity::Warning => CXDiagnostic_Warning,
            Severity::Error => CXDiagnostic_Error,
            Severity::Fatal => CXDiagnostic_Fatal,
        }
    }
}
