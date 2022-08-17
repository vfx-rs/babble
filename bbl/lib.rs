#![allow(unused)] // REMOVE

use cursor::USR;

use std::fmt::Display;
use std::path::Path;

use log::*;

pub mod cursor;
pub mod index;
pub mod template_argument;
pub mod virtual_file;
pub mod ast;
pub mod record;
pub mod function;
pub mod qualtype;
pub mod class;
pub mod class_template;
pub mod type_alias;
pub mod namespace;
use ast::{AST, extract_ast};
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
pub fn parse_string<S1: AsRef<str>, S: AsRef<str>>(
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

use ty::{Type, TypeKind};

pub fn ast_from_namespace(name: &str, c_tu: Cursor, tu: &TranslationUnit) -> AST {
    let ns = if name.is_empty() {
        c_tu.children()
    } else {
        c_tu.children_of_kind_with_name(CursorKind::Namespace, name, true)
    };

    let mut binding = AST::new();
    let namespaces = Vec::new();
    let mut already_visited = Vec::new();
    for cur in ns {
        extract_ast(cur.clone(), 0, 10, &mut already_visited, &mut binding, &tu, namespaces.clone());
    }

    binding
}

#[cfg(test)]
pub(crate) fn get_test_filename(base: &str) -> String {
    std::path::PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
        .join("testdata")
        .join(base)
        .as_os_str()
        .to_string_lossy()
        .to_string()
}
