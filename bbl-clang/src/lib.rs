use std::path::PathBuf;

use diagnostic::Severity;

pub mod access_specifier;
pub mod compilation_database;
pub mod cursor;
pub mod cursor_kind;
pub mod diagnostic;
pub mod error;
pub mod exception;
pub mod file;
pub mod index;
pub mod string;
pub mod template_argument;
pub mod token;
pub mod translation_unit;
pub mod ty;
pub mod virtual_file;
pub mod printing_policy;

/// Convenience function to parse a C++ string with the given compiler arguments and optionally log diagnostics
///
/// This creates a temporary file in `std::env::temp_dir()` with the file contents passed
pub fn parse_string_to_tu<S1: AsRef<str>, S: AsRef<str>>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<translation_unit::TranslationUnit, error::Error> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = index::Index::new();
    let tu = index.parse_translation_unit(path, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => log::debug!("{}", d),
                Severity::Note => log::info!("{}", d),
                Severity::Warning => log::warn!("{}", d),
                Severity::Error | Severity::Fatal => log::error!("{}", d),
            }
        }
    }

    Ok(tu)
}

pub fn get_clang_binary() -> Result<String, error::Error> {
    use error::Error;
    use std::process::Command;

    // First check if we've specified it explicitly
    if let Ok(path) = std::env::var("CLANG_PATH") {
        if Command::new(&path).output().is_ok() {
            println!("Got clang from CLANG_PATH");
            return Ok(path);
        }
    }

    // Next check if there's a pointer to llvm-config
    if let Ok(config_path) = std::env::var("LLVM_CONFIG_PATH") {
        if let Ok(output) = Command::new(&config_path).arg("--bindir").output() {
            let stdout = String::from_utf8(output.stdout).map_err(Error::NonUTF8Output)?;

            let line = stdout
                .lines()
                .next()
                .ok_or_else(|| Error::FailedToParseOutput(stdout.clone()))?;

            println!("Got clang from LLVM_CONFIG_PATH");
            return Ok(PathBuf::from(line).join("clang").display().to_string());
        }
    }

    // Finally just check if it's in the PATH
    if Command::new("clang").output().is_ok() {
        println!("Got clang from PATH");
        return Ok("clang".to_string());
    }

    Err(Error::ClangBinaryNotFound)
}

/// Try and get the default cli arguments (clang resources, /usr/include etc) for the current OS
pub fn get_default_cli_args() -> Result<Vec<String>, error::Error> {
    use error::Error;
    use std::process::Command;

    let clang_bin = get_clang_binary()?;

    let output = Command::new(&clang_bin)
        .arg("-print-resource-dir")
        .output()
        .map_err(Error::FailedToRunClang)?;
    let stdout = String::from_utf8(output.stdout).map_err(Error::NonUTF8Output)?;

    let line = stdout
        .lines()
        .next()
        .ok_or_else(|| Error::FailedToParseOutput(stdout.clone()))?;

    let mut result = vec!["-resource-dir".to_string(), line.to_string()];

    #[cfg(unix)]
    {
        result.push("-I/usr/include".to_string());
        result.push("-I/usr/local/include".to_string());
    }

    Ok(result)
}

pub fn cli_args() -> Result<Vec<String>, error::Error> {
    Ok(get_default_cli_args()?.into_iter().collect())
}

pub fn cli_args_with<S: AsRef<str>>(args: &[S]) -> Result<Vec<String>, error::Error> {
    Ok(get_default_cli_args()?
        .into_iter()
        .chain(args.iter().map(|s| s.as_ref().to_string()))
        .collect())
}

#[cfg(test)]
mod tests {
    use crate::{error::Error, get_default_cli_args};

    #[test]
    fn test_default_cli_args() -> Result<(), Error> {
        let _ = get_default_cli_args()?;

        Ok(())
    }
}
