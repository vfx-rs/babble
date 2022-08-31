use std::fmt::{Display, Debug};

use clang_sys::{clang_getSpellingLocation, clang_getTokenSpelling, CXSourceLocation, CXToken, CXSourceRange, clang_getTokenExtent};

use crate::{file::File, string::CXStringEx, translation_unit::TranslationUnit};

pub struct Token<'tu> {
    pub(crate) inner: CXToken,
    pub(crate) tu: &'tu TranslationUnit,
}

impl<'tu> Token<'tu> {
    pub fn spelling(&self) -> String {
        unsafe { clang_getTokenSpelling(self.tu.inner, self.inner).to_string() }
    }

    pub fn extent(&self) -> SourceRange {
        SourceRange {
            inner: unsafe { clang_getTokenExtent(self.tu.inner, self.inner)}
        }
    }
}

#[repr(C)]
pub struct SourceRange {
    pub(crate) inner: CXSourceRange,
}

#[derive(Copy, Clone)]
#[repr(C)]
pub struct SourceLocation {
    pub(crate) inner: CXSourceLocation,
}

impl Debug for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.spelling_location())
    }
}

impl SourceLocation {
    pub fn spelling_location(&self) -> FileLocation {
        unsafe {
            let mut cxfile = std::ptr::null_mut();
            let mut line = 0u32;
            let mut column = 0u32;

            clang_getSpellingLocation(
                self.inner,
                &mut cxfile,
                &mut line,
                &mut column,
                std::ptr::null_mut(),
            );

            FileLocation {
                file: File { inner: cxfile },
                line,
                column,
            }
        }
    }
}

#[derive(Debug)]
pub struct FileLocation {
    file: File,
    line: u32,
    column: u32,
}

impl Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file.file_name(), self.line, self.column)
    }
}
