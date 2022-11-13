use anyhow::Result;
use bbl_clang::{
    access_specifier::AccessSpecifier,
    cli_args_with,
    cursor::{ChildVisitResult, Cursor, USR},
    cursor_kind::CursorKind,
    index::Index,
    virtual_file::{configure_bind_project, write_temp_file},
};
use clap::{Parser, ValueEnum};
use log::{debug, error, info, warn};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
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

    /// Namespaces to extract from
    #[clap(short, long, value_parser)]
    namespaces: Vec<String>,

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

    let mut includes = String::new();

    for entry in walkdir::WalkDir::new(&args.include_path)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if entry.path().is_dir() {
            continue;
        }

        match entry
            .path()
            .extension()
            .map(|os| os.to_string_lossy().to_lowercase())
        {
            Some(ex) if ["h", "hh", "hpp", "hxx"].contains(&ex.as_str()) => {
                let rel_path = diff_paths(entry.path(), &args.include_path).unwrap();
                writeln!(&mut includes, "#include <{}>", rel_path.display())?;
            }
            _ => (),
        }
    }

    let temp_file = write_temp_file(&includes)?;

    let index = Index::new();
    let tu = index.create_translation_unit(&temp_file, &cli_args_with(fc.args())?)?;

    let c_tu = tu.get_cursor()?;

    let mut class_decls = HashMap::new();

    for child in c_tu.children() {
        if child.kind() == CursorKind::Namespace && args.namespaces.contains(&child.display_name())
        {
            walk_ast(child, child.display_name(), &mut class_decls);
        }
    }

    /*
    c_tu.visit_children(|c, _| {
        let decl_fn = c
            .location()
            .spelling_location()
            .file
            .file_name()
            .unwrap_or_else(|| "NULL".to_string());

        let namespace_names = c.

        if matches!(
            c.kind(),
            CursorKind::ClassDecl
                | CursorKind::ClassTemplate
                | CursorKind::ClassTemplatePartialSpecialization
                | CursorKind::StructDecl
        ) && decl_fn.starts_with(&args.include_path)
        {
            class_decls.insert(format!("{c:?}"));
        }

        ChildVisitResult::Recurse

        // if decl_fn != entry.path().to_string_lossy() {
        //     return ChildVisitResult::Recurse;
        // }

        // match c.kind() {
        //     CursorKind::ClassDecl => {
        //         println!("   {c:?} -- {decl_fn}");
        //         ChildVisitResult::Continue
        //     }
        //     CursorKind::ClassTemplate => {
        //         println!("   {c:?} -- {decl_fn}");
        //         ChildVisitResult::Continue
        //     }
        //     _ => {
        //         println!("   {c:?}");
        //         ChildVisitResult::Recurse
        //     }
        // }
    });
    */

    let mut class_decls = class_decls
        .into_iter()
        .map(|(_, c)| c)
        .collect::<Vec<Class>>();

    class_decls.sort_by(|c1, c2| c1.qname().cmp(c2.qname()));

    for c in &class_decls {
        println!(
            "{} - {}",
            c.qname(),
            if let Some(p) = diff_paths(c.filename(), &args.include_path) {
                p
            } else {
                PathBuf::from(c.filename())
            }
            .display()
        );

        for m in c.methods() {
            println!("    {m}");
        }
    }

    Ok(())
}

fn walk_ast(c: Cursor, namespace: String, class_decls: &mut HashMap<USR, Class>) {
    for child in c.children() {
        match child.kind() {
            CursorKind::ClassDecl | CursorKind::ClassTemplate | CursorKind::StructDecl => {
                let qname = format!("{namespace}::{}", child.display_name());

                #[allow(clippy::map_entry)]
                if !class_decls.contains_key(&child.usr()) {
                    let filename = c
                        .location()
                        .spelling_location()
                        .file
                        .file_name()
                        .unwrap_or_else(|| "NULL".to_string());

                    let mut methods = Vec::new();
                    get_methods(child, &mut methods);

                    class_decls.insert(
                        child.usr(),
                        Class::new(child.display_name(), qname.clone(), filename, methods),
                    );
                }

                walk_ast(child, qname, class_decls);
            }
            CursorKind::Namespace => {
                let child_ns = format!("{namespace}::{}", child.display_name());
                walk_ast(child, child_ns, class_decls);
            }
            _ => walk_ast(child, namespace.clone(), class_decls),
        }
    }
}

#[derive(Clone)]
struct Class {
    name: String,
    qname: String,
    filename: String,
    methods: Vec<String>,
}

impl Class {
    fn new(name: String, qname: String, filename: String, methods: Vec<String>) -> Self {
        Self {
            name,
            qname,
            filename,
            methods,
        }
    }

    fn name(&self) -> &str {
        self.name.as_ref()
    }

    fn qname(&self) -> &str {
        self.qname.as_ref()
    }

    fn filename(&self) -> &str {
        self.filename.as_ref()
    }

    fn methods(&self) -> &[String] {
        self.methods.as_ref()
    }
}

fn get_methods(c_class: Cursor, methods: &mut Vec<String>) {
    for c in c_class.children() {
        if matches!(
            c.kind(),
            CursorKind::CXXMethod | CursorKind::FunctionTemplate
        ) {
            let access = c.cxx_access_specifier().unwrap_or(AccessSpecifier::Public);
            if access == AccessSpecifier::Public {
                methods.push(c.display_name());
            }
        }
    }

    // now do any bases
    for c_base in c_class.children_of_kind(CursorKind::CXXBaseSpecifier, false) {
        if let Ok(c_base_decl) = c_base.referenced() {
            match c_base_decl.kind() {
                CursorKind::ClassDecl | CursorKind::StructDecl | CursorKind::ClassTemplate => {
                    debug!("Extracting base {c_base_decl:?}");
                    let access = c_base
                        .cxx_access_specifier()
                        .unwrap_or(AccessSpecifier::Public);

                    if access == AccessSpecifier::Public {
                        get_methods(c_base_decl, methods);
                    }
                }
                _ => warn!("Unhandled base {c_base_decl:?} of class {c_class:?}"),
            }
        }
    }
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
