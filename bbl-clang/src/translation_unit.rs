use std::fmt::Debug;
use std::os::raw::{c_uint, c_void};

use crate::diagnostic::Diagnostic;
use crate::file::File;
use crate::string::CXStringEx;
use crate::token::{SourceLocation, Token};

use super::cursor::{cursor, Cursor};
use super::error::Error;
use clang_sys::*;

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone)]
pub struct TranslationUnit {
    pub(crate) inner: CXTranslationUnit,
}

impl TranslationUnit {
    pub fn get_cursor(&self) -> Result<Cursor> {
        let cur = unsafe { clang_getTranslationUnitCursor(self.inner) };

        cursor(cur)
    }

    pub fn get_cursor_at_location(&self, location: &SourceLocation) -> Result<Cursor> {
        let cur = unsafe { clang_getCursor(self.inner, location.inner) };
        cursor(cur)
    }

    pub fn spelling(&self) -> String {
        unsafe {
            clang_getTranslationUnitSpelling(self.inner)
                .to_string()
                .expect("null string")
        }
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

    pub fn get_inclusions<F>(&self, callback: F)
    where
        F: FnMut(File, &[SourceLocation]),
    {
        unsafe {
            let (closure, trampoline) = unpack_inclusion_visitor_closure(callback);
            clang_getInclusions(self.inner, trampoline, closure);
        }
    }
}

impl Debug for TranslationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.spelling())
    }
}

pub type InclusionVisitor = extern "C" fn(CXFile, *mut CXSourceLocation, c_uint, CXClientData);

unsafe fn unpack_inclusion_visitor_closure<F>(closure: F) -> (*mut c_void, InclusionVisitor)
where
    F: FnMut(File, &[SourceLocation]),
{
    extern "C" fn trampoline<F>(
        included_file: CXFile,
        inclusion_stack: *mut CXSourceLocation,
        include_len: c_uint,
        client_data: CXClientData,
    ) where
        F: FnMut(File, &[SourceLocation]),
    {
        let closure: &mut F = unsafe { &mut *(client_data as *mut F) };
        (*closure)(
            File {
                inner: included_file,
            },
            unsafe {
                std::slice::from_raw_parts(
                    inclusion_stack as *const SourceLocation,
                    include_len as usize,
                )
            },
        );
    }

    let cb = Box::new(closure);
    let cb = Box::leak(cb);

    (cb as *mut F as *mut c_void, trampoline::<F>)
}

// Just leak for now to avoid lifetimes
// impl Drop for TranslationUnit {
//     fn drop(&mut self) {
//         unsafe { clang_disposeTranslationUnit(self.inner) }
//     }
// }
