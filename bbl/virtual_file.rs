use clang_sys::{
    clang_VirtualFileOverlay_addFileMapping, clang_VirtualFileOverlay_create,
    clang_VirtualFileOverlay_dispose, CXVirtualFileOverlay,
};
use log::debug;
use std::{
    collections::hash_map::DefaultHasher,
    ffi::CString,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct VirtualFileOverlay {
    inner: CXVirtualFileOverlay,
}

impl VirtualFileOverlay {
    pub fn new() -> VirtualFileOverlay {
        unsafe {
            VirtualFileOverlay {
                inner: clang_VirtualFileOverlay_create(0),
            }
        }
    }

    pub fn add_file_mapping<P1: AsRef<Path>, P2: AsRef<Path>>(
        &mut self,
        virtual_path: P1,
        real_path: P2,
    ) -> Result<()> {
        let virt = CString::new(
            virtual_path
                .as_ref()
                .as_os_str()
                .to_str()
                .ok_or(Error::InvalidPath)?,
        )?;
        let real = CString::new(
            real_path
                .as_ref()
                .as_os_str()
                .to_str()
                .ok_or(Error::InvalidPath)?,
        )?;

        unsafe {
            let result =
                clang_VirtualFileOverlay_addFileMapping(self.inner, virt.as_ptr(), real.as_ptr())
                    as i32;
            if result == 0 {
                Ok(())
            } else {
                println!("Got error: {}", result);
                Err(Error::InvalidType)
            }
        }
    }

    pub fn add_memory_file<P1: AsRef<Path>>(
        &mut self,
        virtual_path: P1,
        file_contents: &str,
    ) -> Result<()> {
        let mut filename = std::env::temp_dir();
        let mut s = DefaultHasher::new();
        file_contents.hash(&mut s);
        let base = format!("{:x}.cpp", s.finish());
        filename.push(base);
        debug!("Writing temp file {}", filename.display());
        std::fs::write(&filename, file_contents)?;

        self.add_file_mapping(virtual_path, &filename)
    }
}

impl Drop for VirtualFileOverlay {
    fn drop(&mut self) {
        unsafe { clang_VirtualFileOverlay_dispose(self.inner) }
    }
}

pub fn write_temp_file(file_contents: &str) -> Result<PathBuf, std::io::Error> {
    let mut filename = std::env::temp_dir();
    let mut s = DefaultHasher::new();
    file_contents.hash(&mut s);
    let base = format!("{:x}.cpp", s.finish());
    filename.push(base);
    println!("Writing file {}", filename.display());
    std::fs::write(&filename, file_contents)?;

    Ok(filename)
}

#[cfg(test)]
mod tests {
    use crate::index::Index;
    use crate::*;

    #[test]
    fn test_virtual_file() -> Result<(), crate::error::Error> {
        let contents = r#"
        #include <stddef.h>
        class TestVirtual {size_t a;};
        "#;
        let path = super::write_temp_file(contents)?;

        let index = Index::new();
        let tu = index.parse_translation_unit(
            &path,
            &["-std=c++14", "-I/usr/include", "-I/usr/local/include"],
        )?;

        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }

        let cur = tu.get_cursor()?;
        let children = cur.children();
        for child in children {
            println!("{child:?}")
        }

        Ok(())
    }
}
