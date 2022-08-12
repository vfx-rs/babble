use clang_sys::*;
use std::ffi::CStr;

pub trait CXStringEx {
    fn as_cxstring(&self) -> CXString;

    fn to_string(&self) -> String {
        unsafe {
            let cstr = CStr::from_ptr(clang_getCString(self.as_cxstring()));
            let result = cstr.to_string_lossy().to_string();
            clang_disposeString(self.as_cxstring());
            result
        }
    }
}

impl CXStringEx for CXString {
    fn as_cxstring(&self) -> CXString {
        *self
    }
}

