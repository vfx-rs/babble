pub mod ffi;

use std::{ffi::{CStr, CString}, ops::Deref};

pub struct CppString {
    inner: *mut ffi::std_string,
}

impl CppString {
    pub fn new() -> Self {
        unsafe {
            let mut inner = std::ptr::null_mut();
            ffi::std_string_ctor(&mut inner);
            CppString { inner }
        }
    }

    pub fn from(s: &str) -> Self {
        unsafe {
            let mut inner = std::ptr::null_mut();
            let cstr = CString::new(s).unwrap();
            ffi::std_string_from_char_ptr(&mut inner, cstr.as_ptr());
            CppString { inner }
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            let mut char_ptr = std::ptr::null();
            ffi::std_string_c_str(self.inner, &mut char_ptr);
            CStr::from_ptr(char_ptr).to_str().unwrap()
        }
    }
}

impl Drop for CppString {
    fn drop(&mut self) {
        unsafe {
            ffi::std_string_dtor(self.inner)
        }
    }
}

impl From<CppString> for String {
    fn from(cs: CppString) -> Self {
        cs.as_str().to_string()
    }
}

impl Deref for CppString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

pub fn main() {
    let result = unsafe {
        let mut tc = ffi::Test_Class{a: 0};

        let name = CppString::from("VFX Rustaceans");

        let mut out = CppString::new();
        ffi::Test_Class_take_string(&mut tc, out.inner, name.inner);

        out.to_string()
    };

    println!("{}", result);
}