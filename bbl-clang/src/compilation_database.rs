use std::{ffi::CString, path::Path};

use clang_sys::*;

use crate::{string::CXStringEx};

#[derive(Debug, thiserror::Error)]
pub enum CompilationDatabaseError {
    #[error("Cannot load compilation database from directory \"{0}\"")]
    CannotLoadDatabase(String),
}

pub struct CompilationDatabase {
    inner: CXCompilationDatabase,
}

impl CompilationDatabase {
    pub fn from_directory<P: AsRef<Path>>(
        build_directory: P,
    ) -> Result<CompilationDatabase, CompilationDatabaseError> {
        let dir = build_directory
            .as_ref()
            .as_os_str()
            .to_str()
            .expect("could not convert build directory to c string");
        let c_dir = CString::new(dir).unwrap();

        let mut err = 0;
        let inner = unsafe {
            clang_CompilationDatabase_fromDirectory(c_dir.as_ptr(), &mut err)
        };

        if err != CXCompilationDatabase_NoError {
            return Err(CompilationDatabaseError::CannotLoadDatabase(dir.to_string()));
        }

        Ok(CompilationDatabase { inner })
    }

    pub fn get_compile_commands<P: AsRef<Path>>(&self, filename: P) -> CompileCommands {
        let filename = filename
            .as_ref()
            .as_os_str()
            .to_str()
            .expect("could not convert filename to c string");
        let c_filename = CString::new(filename).unwrap();

        let inner = unsafe {
            clang_CompilationDatabase_getCompileCommands(self.inner, c_filename.as_ptr())
        };

        CompileCommands { inner }
    }
}

impl Drop for CompilationDatabase {
    fn drop(&mut self) {
        unsafe { clang_CompilationDatabase_dispose(self.inner) }
    }
}

pub struct CompileCommands {
    inner: CXCompileCommands,
}

impl CompileCommands {
    pub fn get_commands(&self) -> Vec<CompileCommand> {
        let mut result = Vec::new();
        for i in 0..unsafe { clang_CompileCommands_getSize(self.inner) } {
            result.push({
                CompileCommand {
                    inner: unsafe { clang_CompileCommands_getCommand(self.inner, i) },
                    _commands: self,
                }
            });
        }

        result
    }
}

impl Drop for CompileCommands {
    fn drop(&mut self) {
        unsafe { clang_CompileCommands_dispose(self.inner) }
    }
}

pub struct CompileCommand<'a> {
    inner: CXCompileCommand,
    _commands: &'a CompileCommands,
}

impl<'a> CompileCommand<'a> {
    pub fn get_arguments(&self) -> Vec<String> {
        let mut result = Vec::new();
        for i in 0..unsafe { clang_CompileCommand_getNumArgs(self.inner) } {
            result.push(unsafe { clang_CompileCommand_getArg(self.inner, i).to_string() });
        }
        result
    }
}
