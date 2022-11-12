use anyhow::Result;
use bbl_clang::{
    cli_args_with, cursor::ChildVisitResult, cursor_kind::CursorKind, index::Index,
    virtual_file::configure_bind_project,
};
use clap::{Parser, ValueEnum};
use log::{debug, error, info, warn};
use std::{
    fmt::Display,
    path::{Component, Path, PathBuf},
};

#[derive(Parser)]
struct Args {
    /// Path to the CMake binding project (i.e. the directory containing the CMakeLists.txt)
    #[clap(value_name = "PROJECT_PATH")]
    project_path: String,

    /// Path to the root include directory of the library to bind
    #[clap(value_name = "INCLUDE_PATH")]
    include_path: String,

    /// Path to find cmake packages. Overrides CMAKE_PREFIX_PATH
    #[clap(long, value_parser)]
    cmake_prefix_path: Option<String>,

    /// Verbosity of the output
    #[clap(short, long, arg_enum, value_parser)]
    verbosity: Option<Verbosity>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(v) = args.verbosity {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(&v.to_string()))
            .format_timestamp(None)
            .init();
    } else {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug"))
            .format_timestamp(None)
            .init();
    }

    let cmakelists = Path::new(&args.project_path).join("CMakeLists.txt");

    if !cmakelists.is_file() {
        anyhow::bail!(
            "Could not find CMakeLists.txt in provided path \"{}\"",
            args.project_path
        );
    }

    let file_commands = configure_bind_project(&args.project_path, args.cmake_prefix_path)?;

    if file_commands.is_empty() {
        anyhow::bail!("No compile commands found in project")
    }

    let fc = &file_commands[0];

    for entry in walkdir::WalkDir::new(&args.include_path)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.path().is_dir() {
            continue;
        }

        let index = Index::new();
        match entry
            .path()
            .extension()
            .map(|os| os.to_string_lossy().to_lowercase())
        {
            Some(ex) if ["h", "hh", "hpp", "hxx"].contains(&ex.as_str()) => {
                let rel_path = diff_paths(entry.path(), &args.include_path).unwrap();
                println!("######## {}", rel_path.display());
                let tu =
                    index.create_translation_unit(fc.filename(), &cli_args_with(fc.args())?)?;

                let c_tu = tu.get_cursor()?;

                c_tu.visit_children(|c, _| {
                    let decl_fn = c
                        .location()
                        .spelling_location()
                        .file
                        .file_name()
                        .unwrap_or_else(|| "NULL".to_string());

                    if c.display_name() == "Marker" {
                        println!("GOT MARKER: {c:?}");
                    }

                    if decl_fn != entry.path().to_string_lossy() {
                        return ChildVisitResult::Recurse;
                    }

                    match c.kind() {
                        CursorKind::ClassDecl => {
                            println!("   {c:?} -- {decl_fn}");
                            ChildVisitResult::Continue
                        }
                        CursorKind::ClassTemplate => {
                            println!("   {c:?} -- {decl_fn}");
                            ChildVisitResult::Continue
                        }
                        _ => {
                            println!("   {c:?}");
                            ChildVisitResult::Recurse
                        }
                    }
                });
            }
            _ => (),
        }
    }

    Ok(())
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Verbosity {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl Display for Verbosity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Verbosity::Trace => {
                write!(f, "trace")
            }
            Verbosity::Debug => {
                write!(f, "debug")
            }
            Verbosity::Info => {
                write!(f, "info")
            }
            Verbosity::Warn => {
                write!(f, "warn")
            }
            Verbosity::Error => {
                write!(f, "error")
            }
        }
    }
}
/// Construct a relative path from a provided base directory path to the provided path.
///
/// ```rust
/// use pathdiff::diff_paths;
/// use std::path::*;
///
/// let baz = "/foo/bar/baz";
/// let bar = "/foo/bar";
/// let quux = "/foo/bar/quux";
/// assert_eq!(diff_paths(bar, baz), Some("../".into()));
/// assert_eq!(diff_paths(baz, bar), Some("baz".into()));
/// assert_eq!(diff_paths(quux, baz), Some("../quux".into()));
/// assert_eq!(diff_paths(baz, quux), Some("../baz".into()));
/// assert_eq!(diff_paths(bar, quux), Some("../".into()));
///
/// assert_eq!(diff_paths(&baz, &bar.to_string()), Some("baz".into()));
/// assert_eq!(diff_paths(Path::new(baz), Path::new(bar).to_path_buf()), Some("baz".into()));
/// ```
pub fn diff_paths<P, B>(path: P, base: B) -> Option<PathBuf>
where
    P: AsRef<Path>,
    B: AsRef<Path>,
{
    let path = path.as_ref();
    let base = base.as_ref();

    if path.is_absolute() != base.is_absolute() {
        if path.is_absolute() {
            Some(PathBuf::from(path))
        } else {
            None
        }
    } else {
        let mut ita = path.components();
        let mut itb = base.components();
        let mut comps: Vec<Component> = vec![];
        loop {
            match (ita.next(), itb.next()) {
                (None, None) => break,
                (Some(a), None) => {
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
                (None, _) => comps.push(Component::ParentDir),
                (Some(a), Some(b)) if comps.is_empty() && a == b => (),
                (Some(a), Some(b)) if b == Component::CurDir => comps.push(a),
                (Some(_), Some(b)) if b == Component::ParentDir => return None,
                (Some(a), Some(_)) => {
                    comps.push(Component::ParentDir);
                    for _ in itb {
                        comps.push(Component::ParentDir);
                    }
                    comps.push(a);
                    comps.extend(ita.by_ref());
                    break;
                }
            }
        }
        Some(comps.iter().map(|c| c.as_os_str()).collect())
    }
}

#[cfg(feature = "camino")]
mod utf8_paths {
    use camino::{Utf8Component, Utf8Path, Utf8PathBuf};

    /// Construct a relative UTF-8 path from a provided base directory path to the provided path.
    ///
    /// ```rust
    /// # extern crate camino;
    /// use camino::*;
    /// use pathdiff::diff_utf8_paths;
    ///
    /// let baz = "/foo/bar/baz";
    /// let bar = "/foo/bar";
    /// let quux = "/foo/bar/quux";
    /// assert_eq!(diff_utf8_paths(bar, baz), Some("../".into()));
    /// assert_eq!(diff_utf8_paths(baz, bar), Some("baz".into()));
    /// assert_eq!(diff_utf8_paths(quux, baz), Some("../quux".into()));
    /// assert_eq!(diff_utf8_paths(baz, quux), Some("../baz".into()));
    /// assert_eq!(diff_utf8_paths(bar, quux), Some("../".into()));
    ///
    /// assert_eq!(diff_utf8_paths(&baz, &bar.to_string()), Some("baz".into()));
    /// assert_eq!(diff_utf8_paths(Utf8Path::new(baz), Utf8Path::new(bar).to_path_buf()), Some("baz".into()));
    /// ```
    #[cfg_attr(docsrs, doc(cfg(feature = "camino")))]
    pub fn diff_utf8_paths<P, B>(path: P, base: B) -> Option<Utf8PathBuf>
    where
        P: AsRef<Utf8Path>,
        B: AsRef<Utf8Path>,
    {
        let path = path.as_ref();
        let base = base.as_ref();

        if path.is_absolute() != base.is_absolute() {
            if path.is_absolute() {
                Some(Utf8PathBuf::from(path))
            } else {
                None
            }
        } else {
            let mut ita = path.components();
            let mut itb = base.components();
            let mut comps: Vec<Utf8Component> = vec![];
            loop {
                match (ita.next(), itb.next()) {
                    (None, None) => break,
                    (Some(a), None) => {
                        comps.push(a);
                        comps.extend(ita.by_ref());
                        break;
                    }
                    (None, _) => comps.push(Utf8Component::ParentDir),
                    (Some(a), Some(b)) if comps.is_empty() && a == b => (),
                    (Some(a), Some(b)) if b == Utf8Component::CurDir => comps.push(a),
                    (Some(_), Some(b)) if b == Utf8Component::ParentDir => return None,
                    (Some(a), Some(_)) => {
                        comps.push(Utf8Component::ParentDir);
                        for _ in itb {
                            comps.push(Utf8Component::ParentDir);
                        }
                        comps.push(a);
                        comps.extend(ita.by_ref());
                        break;
                    }
                }
            }
            Some(comps.iter().map(|c| c.as_str()).collect())
        }
    }
}
