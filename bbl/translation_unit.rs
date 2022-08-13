use crate::diagnostic::Diagnostic;
use crate::token::{SourceLocation, Token};
use crate::CXStringEx;

use super::cursor::{cursor, Cursor};
use super::error::Error;
use clang_sys::*;

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct TranslationUnit {
    pub(crate) inner: CXTranslationUnit,
}

impl TranslationUnit {
    pub fn get_cursor(&self) -> Result<Cursor> {
        let cur = unsafe { clang_getTranslationUnitCursor(self.inner) };

        cursor(cur)
    }

    pub fn spelling(&self) -> String {
        unsafe { clang_getTranslationUnitSpelling(self.inner).to_string() }
    }

    pub fn token(&self, loc: SourceLocation) -> Token {
        unsafe {
            let tk = clang_getToken(self.inner, loc.inner);
            Token {
                inner: *tk,
                tu: self,
            }
        }
    }

    pub fn num_diagnostics(&self) -> u32 {
        unsafe { clang_getNumDiagnostics(self.inner) }
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut result = Vec::new();
        for i in 0..self.num_diagnostics() {
            unsafe {
                let d = clang_getDiagnostic(self.inner, i);
                result.push(Diagnostic { inner: d })
            }
        }
        result
    }
}

// Just leak for now to avoid lifetimes
// impl Drop for TranslationUnit {
//     fn drop(&mut self) {
//         unsafe { clang_disposeTranslationUnit(self.inner) }
//     }
// }
