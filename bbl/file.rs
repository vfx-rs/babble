use clang_sys::*;

use crate::CXStringEx;

pub struct File {
    pub(crate) inner: CXFile,
}

impl File {
    pub fn file_name(&self) -> String {
        unsafe { clang_getFileName(self.inner).to_string() }
    }
}
