use std::ffi::CString;
use std::path::Path;

use clang_sys::{
    clang_createIndex, clang_createTranslationUnitFromSourceFile, clang_parseTranslationUnit,
    CXIndex,
};

use crate::error::Error;
use crate::translation_unit::TranslationUnit;
type Result<T, E = Error> = std::result::Result<T, E>;

use log::*;

pub struct Index {
    inner: CXIndex,
}

impl Index {
    pub fn new() -> Index {
        let inner = unsafe { clang_createIndex(0, 0) };
        Index { inner }
    }

    pub fn parse_translation_unit<P: AsRef<Path>, S: AsRef<str>>(
        &self,
        filename: P,
        args: &[S],
    ) -> Result<TranslationUnit> {
        let cfilename = CString::new(
            filename
                .as_ref()
                .as_os_str()
                .to_str()
                .ok_or(Error::InvalidPath)?,
        )?;

        let cargs = args
            .iter()
            .map(|a| CString::new(a.as_ref()).map_err(Error::from))
            .collect::<Result<Vec<_>>>()?;

        let cstrargs: Vec<_> = cargs.iter().map(|a| a.as_ptr()).collect();

        let tu = unsafe {
            clang_parseTranslationUnit(
                self.inner,
                cfilename.as_ptr(),
                cstrargs.as_ptr(),
                cstrargs.len() as i32,
                std::ptr::null_mut(),
                0,
                0,
            )
        };

        if tu.is_null() {
            error!(
                "translation unit {} failed to parse. We don't have any good diagnostics here. Maybe check the args: {cargs:?}",
                filename.as_ref().display()
            );
            Err(Error::ParseError)
        } else {
            Ok(TranslationUnit { inner: tu })
        }
    }

    pub fn create_translation_unit<P: AsRef<Path>, S: AsRef<str>>(
        &self,
        filename: P,
        args: &[S],
    ) -> Result<TranslationUnit> {
        let cfilename = CString::new(
            filename
                .as_ref()
                .as_os_str()
                .to_str()
                .ok_or(Error::InvalidPath)?,
        )?;

        let cargs = args
            .iter()
            .map(|a| CString::new(a.as_ref()).map_err(Error::from))
            .collect::<Result<Vec<_>>>()?;

        let cstrargs: Vec<_> = cargs.iter().map(|a| a.as_ptr()).collect();

        let tu = unsafe {
            clang_createTranslationUnitFromSourceFile(
                self.inner,
                cfilename.as_ptr(),
                cstrargs.len() as i32,
                cstrargs.as_ptr(),
                0,
                std::ptr::null_mut(),
            )
        };

        if tu.is_null() {
            error!(
                "translation unit {} failed to parse. We don't have any good diagnostics here. Maybe check the args: {cargs:?}",
                filename.as_ref().display()
            );
            Err(Error::ParseError)
        } else {
            Ok(TranslationUnit { inner: tu })
        }
    }
}

impl Default for Index {
    fn default() -> Self {
        Self::new()
    }
}

// We'll just leak the index for now to avoid dealing with lifetimes yet
// impl Drop for Index {
//     fn drop(&mut self) {
//         unsafe {
//             clang_disposeIndex(self.inner);
//         }
//     }
// }
