use diagnostic::Severity;

pub mod access_specifier;
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
