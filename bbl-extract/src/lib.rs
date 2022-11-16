#![allow(clippy::too_many_arguments)]

use std::path::Path;

use bbl_clang::{
    cursor::USR, cursor_kind::CursorKind, diagnostic::Severity, index::Index,
    translation_unit::TranslationUnit, virtual_file,
};
use class::OverrideList;
use log::*;

pub mod ast;
pub mod class;
pub mod enm;
pub mod function;
pub mod index_map;
pub mod namespace;
pub mod qualtype;
pub mod stdlib;
pub mod templates;
pub mod typedef;
use ast::{dump, extract_ast_from_namespace, AST};
pub mod error;
use error::Error;
use regex::RegexSet;
use tracing::instrument;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Convenience function to parse a file with the given compiler arguments and optionally log diagnostics
#[instrument(level = "trace")]
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

#[instrument(level = "trace")]
pub fn parse_file_and_extract_ast<
    P: AsRef<Path> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
    path: P,
    cli_args: &[S],
    log_diagnostics: bool,
    namespace: Option<&str>,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    header_str: &str,
    stop_on_error: bool,
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

    let ast = extract_ast_from_namespace(
        namespace,
        cur,
        &tu,
        allow_list,
        class_overrides,
        header_str,
        stop_on_error,
    )?;

    Ok(ast)
}

#[instrument(level = "trace")]
pub fn parse_string<S1: AsRef<str> + std::fmt::Debug, S: AsRef<str> + std::fmt::Debug>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    parse_file(&path, cli_args, log_diagnostics)
}

#[instrument(level = "trace")]
pub fn parse_string_and_extract_ast<
    S1: AsRef<str> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
    namespace: Option<&str>,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<AST> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    parse_file_and_extract_ast(
        &path,
        cli_args,
        log_diagnostics,
        namespace,
        allow_list,
        class_overrides,
        "",
        stop_on_error,
    )
}

#[instrument(level = "trace")]
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

    let skip_kinds = &[];
    let mut already_visited = Vec::new();
    if let Some(namespace) = namespace {
        let children =
            tu.get_cursor()?
                .children_of_kind_with_name(CursorKind::Namespace, namespace, true);
        for child in children {
            dump(
                child,
                0,
                20,
                &mut already_visited,
                &tu,
                skip_kinds,
                None,
                true,
            );
        }
    } else {
        dump(
            tu.get_cursor()?,
            0,
            20,
            &mut already_visited,
            &tu,
            skip_kinds,
            None,
            true,
        );
    }

    Ok(())
}

#[derive(Debug)]
pub struct AllowList {
    regexes: RegexSet,
    /// If false, actually a block list
    allow: bool,
}

impl Default for AllowList {
    fn default() -> Self {
        AllowList {
            regexes: RegexSet::empty(),
            allow: false,
        }
    }
}

impl AllowList {
    pub fn new(prefixes: Vec<String>) -> AllowList {
        let regexes = RegexSet::new(&prefixes).unwrap();
        AllowList {
            regexes,
            allow: true,
        }
    }

    pub fn block_list(prefixes: Vec<String>) -> AllowList {
        let regexes = RegexSet::new(&prefixes).unwrap();
        AllowList {
            regexes,
            allow: false,
        }
    }

    pub fn allows(&self, name: &str) -> bool {
        if self.regexes.is_empty() {
            return true;
        }

        if self.regexes.is_match(name) {
            self.allow
        } else {
            !self.allow
        }
    }
}
