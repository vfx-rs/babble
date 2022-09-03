#![allow(unused)] // REMOVE

use std::fmt::Display;
use std::path::Path;

use bbl_clang::{
    cursor_kind::CursorKind, diagnostic::Severity, index::Index, translation_unit::TranslationUnit,
    virtual_file,
};
use log::*;

pub mod ast;
pub mod class;
pub mod function;
pub mod index_map;
pub mod namespace;
pub mod qualtype;
pub mod template_argument;
pub mod type_alias;
use ast::{dump, extract_ast, extract_ast_from_namespace, Include, AST};
pub mod error;
use error::Error;
use tracing::instrument;

use crate::template_argument::TemplateType;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Convenience function to parse a file with the given compiler arguments and optionally log diagnostics
#[instrument]
pub fn parse_file<P: AsRef<Path> + std::fmt::Debug, S: AsRef<str> + std::fmt::Debug>(
    filename: P,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let index = Index::new();
    let tu = index.create_translation_unit(filename, cli_args)?;

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

#[instrument]
pub fn parse_file_and_extract_ast<
    P: AsRef<Path> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
    path: P,
    cli_args: &[S],
    log_diagnostics: bool,
    namespace: Option<&str>,
) -> Result<AST> {
    let index = Index::new();
    let tu = index.create_translation_unit(path, cli_args)?;

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

    let ast = extract_ast_from_namespace(namespace, cur, &tu)?;

    Ok(ast)
}

#[instrument]
pub fn parse_string_and_extract_ast<
    S1: AsRef<str> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
    namespace: Option<&str>,
) -> Result<AST> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    parse_file_and_extract_ast(&path, cli_args, log_diagnostics, namespace)
}

#[instrument]
pub fn parse_string_and_dump_ast<
    S1: AsRef<str> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
    contents: S1,
    cli_args: &[S],
    namespace: Option<&str>,
    log_diagnostics: bool,
) -> Result<()> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = Index::new();
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
                .children_of_kind_with_name(CursorKind::Namespace, namespace, true);
        for child in children {
            dump(child, 0, 20, &mut already_visited, &tu);
        }
    } else {
        dump(tu.get_cursor()?, 0, 20, &mut already_visited, &tu);
    }

    Ok(())
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
