#![allow(unused)] // REMOVE

use cursor::USR;

use std::fmt::Display;
use std::path::Path;

use log::*;

pub mod ast;
pub mod class;
pub mod cursor;
pub mod function;
pub mod index;
pub mod namespace;
pub mod qualtype;
pub mod template_argument;
pub mod type_alias;
pub mod virtual_file;
use ast::{extract_ast, AST, dump, extract_ast_from_namespace};
pub use cursor::{ChildVisitResult, Cursor};
pub mod cursor_kind;
pub mod error;
use error::Error;
pub mod translation_unit;
pub use translation_unit::TranslationUnit;
pub mod string;
pub use string::CXStringEx;

use crate::{cursor_kind::CursorKind, template_argument::TemplateType};
pub mod token;

pub mod diagnostic;
pub mod ty;
pub use diagnostic::Severity;
pub mod file;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Convenience function to parse a file with the given compiler arguments and optionally log diagnostics
pub fn parse_file<P: AsRef<Path>, S: AsRef<str>>(
    filename: P,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let index = index::Index::new();
    let tu = index.parse_translation_unit(filename, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    Ok(tu)
}

/// Convenience function to parse a C++ string with the given compiler arguments and optionally log diagnostics
///
/// This creates a temporary file in `std::env::temp_dir()` with the file contents passed
pub fn parse_string_to_tu<S1: AsRef<str>, S: AsRef<str>>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = index::Index::new();
    let tu = index.parse_translation_unit(path, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    Ok(tu)
}

pub fn parse_string_and_extract_ast<S1: AsRef<str>, S: AsRef<str>>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
    namespace: Option<&str>,
) -> Result<AST> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = index::Index::new();
    let tu = index.parse_translation_unit(path, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    let cur = tu.get_cursor()?;

    let mut ast = AST::new();
    let mut already_visited = Vec::new();

    if let Some(namespace) = namespace {
        ast = extract_ast_from_namespace(namespace, cur, &tu);
    } else {
        extract_ast(cur, 0, 100, &mut already_visited, &mut ast, &tu, Vec::new());
    }


    Ok(ast)
}

pub fn parse_string_and_dump_ast<S1: AsRef<str>, S: AsRef<str>>(
    contents: S1,
    cli_args: &[S],
    namespace: Option<&str>,
    log_diagnostics: bool,
) -> Result<()> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = index::Index::new();
    let tu = index.parse_translation_unit(path, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    let mut already_visited = Vec::new();
    if let Some(namespace) = namespace {
        let children =
            tu.get_cursor()?
                .children_of_kind_with_name(CursorKind::Namespace, &namespace, true);
        for child in children {
            dump(child, 0, 20, &mut already_visited, &tu);
        }
    } else {
        dump(tu.get_cursor()?, 0, 20, &mut already_visited, &tu);
    }


    Ok(())
}

use ty::{Type, TypeKind};

#[cfg(test)]
pub(crate) fn get_test_filename(base: &str) -> String {
    std::path::PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
        .join("testdata")
        .join(base)
        .as_os_str()
        .to_string_lossy()
        .to_string()
}
