#![allow(unused)] // REMOVE

use std::fmt::Display;
use std::path::Path;

use bbl_clang::{
    cursor_kind::CursorKind, diagnostic::Severity, index::Index, translation_unit::TranslationUnit,
    virtual_file,
};
use class::OverrideList;
use env_logger::fmt::Color;
use log::*;

pub mod ast;
pub mod class;
pub mod function;
pub mod index_map;
pub mod namespace;
pub mod qualtype;
pub mod stdlib;
pub mod templates;
pub mod typedef;
pub mod enm;
use ast::{dump, extract_ast, extract_ast_from_namespace, Include, AST};
pub mod error;
use error::Error;
use regex::{Regex, RegexSet};
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

    let ast = extract_ast_from_namespace(namespace, cur, &tu, allow_list, class_overrides)?;

    Ok(ast)
}

#[instrument(level = "trace")]
pub fn parse_string<
    S1: AsRef<str> + std::fmt::Debug,
    S: AsRef<str> + std::fmt::Debug,
>(
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
) -> Result<AST> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    parse_file_and_extract_ast(&path, cli_args, log_diagnostics, namespace, allow_list, class_overrides)
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
            dump(child, 0, 20, &mut already_visited, &tu, skip_kinds, None);
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
        );
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


pub(crate) fn init_log() {
    use std::io::Write;

    let _ = env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
        .format_timestamp(None)
        .format(|buf, record| -> Result<(), std::io::Error> {
            let mut level_style = buf.style();
            match record.level() {
                Level::Trace => level_style.set_color(Color::Blue),
                Level::Debug => level_style.set_color(Color::White),
                Level::Info => level_style.set_color(Color::Cyan),
                Level::Warn => level_style.set_color(Color::Yellow),
                Level::Error => level_style.set_color(Color::Red),
            };

            writeln!(
                buf,
                "{} [{}:{}] {}",
                level_style.value(record.level()),
                record.file().unwrap_or(""),
                record.line().unwrap_or(0),
                record.args()
            )
        })
        // .is_test(true)
        .try_init();
}


#[derive(Debug)]
pub struct AllowList {
    regexes: RegexSet,
    /// If false, actually a block list
    allow: bool,
}

impl Default for AllowList {
    fn default() -> Self {
        AllowList { regexes: RegexSet::empty(), allow: false }
    }
}

impl AllowList {
    pub fn new(prefixes: Vec<String>) -> AllowList {
        let regexes = RegexSet::new(&prefixes).unwrap();
        AllowList { regexes, allow: true }
    }

    pub fn block_list(prefixes: Vec<String>) -> AllowList {
        let regexes = RegexSet::new(&prefixes).unwrap();
        AllowList { regexes, allow: false }
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