use std::fmt::Debug;

use clang_sys::*;

use crate::string::CXStringEx;

pub struct File {
    pub(crate) inner: CXFile,
}

impl Debug for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.file_name())
    }
}

impl File {
    pub fn file_name(&self) -> String {
        unsafe { clang_getFileName(self.inner).to_string() }
    }
}
