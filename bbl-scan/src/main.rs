use anyhow::{Context, Result};
use bbl_clang::{
    access_specifier::AccessSpecifier,
    cli_args_with,
    cursor::{ChildVisitResult, Cursor, USR},
    cursor_kind::CursorKind,
    index::Index,
    ty::Type,
    virtual_file::configure_temp_cmake_project,
};
use bbl_util::{read_build_config, BuildConfig};
use clap::{Parser, ValueEnum};
use convert_case::{Case, Casing};
use log::{error, warn};
use std::{
    collections::HashMap,
    fmt::{Display, Write},
    path::{Component, Path, PathBuf},
};

#[derive(Parser)]
struct Args {
    /// Path to the CMake binding project to write (i.e. the directory containing the CMakeLists.txt)
    #[clap(value_name = "PROJECT_PATH")]
    project_path: String,

    /// Path to the root include directory of the library to bind
    /// e.g. if one of your headers is at "~/mylib/include/foo/bar.h" you would pass "~/mylib/include" here
    #[clap(value_name = "HEADER_PATH")]
    header_path: String,

    /// Name of the project
    #[clap(long, value_parser)]
    project_name: Option<String>,

    /// Path to find cmake packages. Overrides CMAKE_PREFIX_PATH
    #[clap(long, value_parser)]
    cmake_prefix_path: Option<String>,

    /// packages to pass to find_package command
    #[clap(short, long, value_parser)]
    package: Vec<String>,

    /// libraries to pass to target_link_libraries command
    /// Note: nothing is every built and linked, these will just be used for gathering extra include paths
    #[clap(short, long, value_parser)]
    link_library: Vec<String>,

    /// Extra include paths
    /// Note: these are not scanned, they are just added to the target_include_directories
    #[clap(short, long, value_parser)]
    include: Vec<String>,

    /// C++ standard to use for parsing when scanning the headers, e.g. "14"
    #[clap(long, value_parser)]
    cxx_standard: Option<String>,

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

    let mut includes = String::new();

    // Interpret the PROJECT_PATH as either a path to the build_config.json the user has already written, or a directory
    // containing it (or where we'll write one if the user has just passed command-line arguments)
    let args_project_path = Path::new(&args.project_path);
    let (build_config_path, project_path) = match args_project_path.file_name() {
        Some(filename) if filename.to_string_lossy() == "build_config.json" => {
            let build_config_path = args_project_path;
            (
                build_config_path.to_owned(),
                build_config_path.parent().unwrap(),
            )
        }
        Some(_) if args_project_path.is_dir() => (
            args_project_path.join("build_config.json"),
            args_project_path,
        ),
        _ => {
            anyhow::bail!(
                "Provided PROJECT_PATH was not a build_config.json or a directory: {}",
                args_project_path.display()
            );
        }
    };

    let mut build_config = BuildConfig::default();

    // try to read the config from the project dir in case the user has created one there in advance
    if build_config_path.is_file() {
        println!("Reading build config from {}", build_config_path.display());
        build_config = match read_build_config(&build_config_path) {
            Ok(bc) => bc,
            Err(e) => {
                panic!(
                    "Failed to read build config from {}: {}",
                    build_config_path.display(),
                    e
                );
            }
        }
    } else {
        println!(
            "No build config at {}. Creating one from arguments",
            build_config_path.display()
        );
    }

    // populate the build config from the arguments - overriding the json config if it was present, or just filling out
    // the config if not
    if let Some(project_name) = args.project_name {
        build_config.project_name = project_name;
    } else if build_config.project_name.is_empty() || build_config.project_name == "bind" {
        println!(
            "build config project name is {} - overriding",
            build_config.project_name
        );
        build_config.project_name = Path::new(&project_path)
            .components()
            .last()
            .expect("empty project path")
            .as_os_str()
            .to_string_lossy()
            .to_string();
    }

    if let Some(s) = args.cxx_standard {
        build_config.cxx_standard = s;
    }

    build_config
        .find_packages
        .extend(args.package.iter().cloned());
    build_config
        .link_libraries
        .extend(args.link_library.iter().cloned());
    build_config
        .extra_includes
        .extend(args.include.iter().cloned());
    build_config.extra_includes.push(args.header_path.clone());
    build_config
        .namespaces
        .extend(args.namespaces.iter().cloned());

    build_config.extra_includes.sort();
    build_config.extra_includes.dedup();

    // Gather all the header files under the provided header path into one big series of include directives to parse
    for entry in walkdir::WalkDir::new(&args.header_path)
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
                let rel_path = diff_paths(entry.path(), &args.header_path).unwrap();
                writeln!(&mut includes, "#include <{}>", rel_path.display())?;
            }
            _ => (),
        }
    }

    // Configure the temp cmake project for the big list of includes to get compile commands to use when parsing
    let (temp_file, temp_args) = configure_temp_cmake_project(
        &includes,
        &build_config.find_packages,
        &build_config.link_libraries,
        &build_config.extra_includes,
        args.cmake_prefix_path.as_ref(),
        &build_config.cxx_standard,
    )?;

    // split off the namespace renames and create clean namespaces for searching
    let mut namespace_renames = Vec::new();
    let mut namespaces = Vec::new();
    for ns in &build_config.namespaces {
        if ns.contains('=') {
            let mut toks = ns.split('=');
            let original = toks.next().unwrap().to_string();
            let rename = toks.next().unwrap().to_string();
            namespaces.push(original.clone());
            namespace_renames.push((original, rename));
        } else {
            namespaces.push(ns.clone());
        }
    }

    // extract everything starting at the selected namespaces
    let index = Index::new();
    let tu = index.create_translation_unit(&temp_file, &cli_args_with(&temp_args)?)?;
    let c_tu = tu.get_cursor()?;
    let mut class_decls = HashMap::new();
    c_tu.visit_children(|c, _| {
        if c.kind() == CursorKind::Namespace {
            let qname = qualified_name(c, c.display_name());
            if namespaces.contains(&qname) {
                walk_ast(c, qname, &mut class_decls);
            }
        }
        ChildVisitResult::Recurse
    });

    // partition decls into modules
    let mut modules: Vec<ModuleFile> = Vec::new();

    let mut class_decls = class_decls
        .into_iter()
        .map(|(_, class)| class)
        .collect::<Vec<_>>();

    class_decls.sort_by(|a, b| a.name.cmp(&b.name));

    for class in class_decls.into_iter() {
        if let Some(relpath) = diff_paths(class.filename(), &args.header_path) {
            if let Some(m) = modules.iter_mut().find(|m| m.relpath == relpath) {
                m.classes.push(class);
            } else {
                modules.push(ModuleFile::new(relpath.clone(), vec![class]));
            }
        } else {
            warn!(
                "Class {class:?} is in file {} which is not in include path {}. Ignoring",
                class.filename(),
                args.header_path
            );
        }
    }

    // Apply namespace renames to the types
    modules.sort_by(|a, b| a.relpath.cmp(&b.relpath));
    for module in &mut modules {
        let mut module_namespace_renames = Vec::new();

        for nsr in &namespace_renames {
            for class in &mut module.classes {
                if class.rename_namespace(&nsr.0, &nsr.1) && !module_namespace_renames.contains(nsr)
                {
                    module_namespace_renames.push(nsr.clone());
                }
            }
        }

        module.namespace_renames = module_namespace_renames;
    }

    // Binding project will be in {project_name}-bind subdirectory
    let project_root =
        PathBuf::from(&project_path).join(format!("{}-bind", build_config.project_name));

    let mut cpp_files = Vec::new();

    for m in &modules {
        let mut body = String::new();

        writeln!(&mut body, "#include <{}>\n", m.relpath.display())?;
        writeln!(&mut body, "#include <bbl.hpp>\n")?;

        writeln!(&mut body, "BBL_MODULE({}) {{", m.module_name.join("::"))?;

        let mut namespaces_used = Vec::new();
        for nsr in &m.namespace_renames {
            if namespaces_used.contains(&nsr.1) {
                writeln!(
                    &mut body,
                    "    {{\n        namespace {} = {};\n    }}",
                    nsr.1, nsr.0
                )?;
            } else {
                writeln!(&mut body, "    namespace {} = {};", nsr.1, nsr.0)?;
                namespaces_used.push(nsr.1.clone());
            }
        }
        writeln!(&mut body)?;

        for c in &m.classes {
            writeln!(&mut body, "    bbl::Class<{}>()", c.qname())?;

            for method in c.methods() {
                if method.access != AccessSpecifier::Public {
                    continue;
                }

                let cast = if method.overload_count > 1 {
                    format!("{}\n            ", method.cast(c.qname()))
                } else {
                    String::new()
                };

                let template_msg = if method.is_template {
                    "        // template method - explicitly instantiate it with desired types\n"
                } else {
                    ""
                };

                writeln!(
                    &mut body,
                    "{template_msg}        .m({cast}&{}::{})",
                    c.qname(),
                    method.name()
                )?;
            }

            writeln!(&mut body, "    ;\n")?;
        }

        writeln!(&mut body, "}}\n")?;

        // println!("{}", body);

        let mut output_path = project_root.clone();
        let mut cpp_rel_path = PathBuf::new();
        for comp in &m.module_name {
            cpp_rel_path.push(comp);
        }
        cpp_rel_path = cpp_rel_path.with_extension("cpp");

        output_path.push(&cpp_rel_path);
        let output_dir = output_path.parent().expect("broken project path");
        std::fs::create_dir_all(output_dir)?;

        std::fs::write(&output_path, body)?;

        cpp_files.push(cpp_rel_path);
    }

    let cxx_standard_str = format!("set(CMAKE_CXX_STANDARD {})", build_config.cxx_standard);

    let find_package_str = build_config
        .find_packages
        .iter()
        .map(|p| format!("find_package({p} REQUIRED)"))
        .collect::<Vec<_>>()
        .join("\n");

    let src_str = cpp_files
        .iter()
        .map(|f| format!("{}", f.as_os_str().to_string_lossy()))
        .collect::<Vec<_>>()
        .join("\n    ");

    let target_link_libraries_str = if build_config.link_libraries.is_empty() {
        "".to_string()
    } else {
        format!(
            "target_link_libraries({} {})",
            build_config.project_name,
            build_config.link_libraries.join(" ")
        )
    };

    let extra_includes_str = if build_config.extra_includes.is_empty() {
        "".to_string()
    } else {
        format!(
            "target_include_directories({} PUBLIC {})",
            build_config.project_name,
            build_config.extra_includes.join(" ")
        )
    };

    // finally, write the binding cmake project
    let cmake_contents = format!(
        r#"cmake_minimum_required(VERSION 3.5)

project({0})

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

{cxx_standard_str}

{find_package_str}

add_library({0} STATIC 
    {src_str}
    )

{target_link_libraries_str}
{extra_includes_str}
target_include_directories({0} PUBLIC ${{CMAKE_CURRENT_LIST_DIR}})
"#,
        build_config.project_name,
    );

    // println!("{cmake_contents}");

    std::fs::write(project_root.join("CMakeLists.txt"), cmake_contents).with_context(|| {
        format!(
            "Could not write {}",
            project_root.join("CMakeLists.txt").display()
        )
    })?;
    std::fs::write(
        project_root.join("bbl.hpp"),
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../include/bbl.hpp")),
    )?;

    // write the build config back out again to include the arguments passed on the command line (if any)
    bbl_util::write_build_config(&build_config_path, &build_config)?;

    Ok(())
}

fn walk_ast(c: Cursor, namespace: String, class_decls: &mut HashMap<USR, Class>) {
    for child in c.children() {
        match child.kind() {
            CursorKind::ClassDecl | CursorKind::ClassTemplate | CursorKind::StructDecl => {
                let qname = format!("{namespace}::{}", child.display_name());

                // Don't extract anything we know is not public
                let access = child.cxx_access_specifier();
                if matches!(
                    access,
                    Ok(AccessSpecifier::Private) | Ok(AccessSpecifier::Protected)
                ) {
                    continue;
                }

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

                    let mut overload_counts = HashMap::<String, usize>::new();
                    for method in &methods {
                        *overload_counts.entry(method.name.clone()).or_default() += 1;
                    }

                    for method in methods.iter_mut() {
                        method.overload_count = *overload_counts.get(&method.name).unwrap();
                    }

                    methods.sort_by(|a, b| a.name.cmp(&b.name));

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

#[derive(Debug, Clone)]
struct ModuleFile {
    relpath: PathBuf,
    module_name: Vec<String>,
    classes: Vec<Class>,
    namespace_renames: Vec<(String, String)>,
}

impl ModuleFile {
    pub fn new(relpath: PathBuf, classes: Vec<Class>) -> Self {
        let module_name = relpath
            .with_extension("")
            .components()
            .map(|c| c.as_os_str().to_string_lossy().to_case(Case::Snake))
            .collect::<Vec<_>>();

        Self {
            relpath,
            module_name,
            classes,
            namespace_renames: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
struct Class {
    name: String,
    qname: String,
    filename: String,
    methods: Vec<Method>,
}

impl Class {
    fn new(name: String, qname: String, filename: String, methods: Vec<Method>) -> Self {
        Self {
            name,
            qname,
            filename,
            methods,
        }
    }

    fn qname(&self) -> &str {
        self.qname.as_ref()
    }

    fn filename(&self) -> &str {
        self.filename.as_ref()
    }

    fn methods(&self) -> &[Method] {
        self.methods.as_ref()
    }

    fn rename_namespace(&mut self, original: &str, new: &str) -> bool {
        let mut renamed = false;
        if self.qname.contains(&original) {
            renamed = true;
            self.qname = self.qname.replace(original, new);
        }

        for method in &mut self.methods {
            renamed |= method.rename_namespace(original, new);
        }

        renamed
    }
}

#[derive(Debug, Clone, Eq, PartialOrd, Ord)]
struct Method {
    name: String,
    result: String,
    args: Vec<Argument>,
    is_const: bool,
    is_static: bool,
    overload_count: usize,
    access: AccessSpecifier,
    is_template: bool,
}

impl PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.result == other.result
            && self.args == other.args
            && self.is_const == other.is_const
            && self.is_static == other.is_static
    }
}

impl Method {
    fn cast(&self, class_name: &str) -> String {
        let args = self
            .args
            .iter()
            .map(|a| a.ty.clone())
            .collect::<Vec<String>>()
            .join(", ");

        let is_const = if self.is_const { " const" } else { "" };

        let receiver = if self.is_static {
            "*".to_string()
        } else {
            format!("{class_name}::*")
        };

        format!("({} ({receiver})({args}){is_const})", self.result())
    }

    fn rename_namespace(&mut self, original: &str, new: &str) -> bool {
        let mut renamed = false;
        if self.result.contains(original) {
            renamed = true;
            self.result = self.result.replace(original, new);
        }

        for arg in &mut self.args {
            if arg.ty.contains(original) {
                renamed = true;
                arg.ty = arg.ty.replace(original, new);
            }
        }

        renamed
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|a| format!("{a}"))
            .collect::<Vec<String>>()
            .join(", ");

        let is_const = if self.is_const { " const" } else { "" };
        let over = if self.overload_count > 1 {
            format!("({})", self.overload_count)
        } else {
            "".to_string()
        };

        write!(
            f,
            "{}({args}){is_const} -> {} {over}",
            self.name(),
            self.result()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Argument {
    name: String,
    ty: String,
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl Method {
    fn name(&self) -> &str {
        self.name.as_ref()
    }

    fn result(&self) -> &str {
        self.result.as_ref()
    }
}

fn get_methods(c_class: Cursor, parent_methods: &mut Vec<Method>) {
    let mut methods = Vec::new();

    for c in c_class.children() {
        if matches!(
            c.kind(),
            CursorKind::CXXMethod | CursorKind::FunctionTemplate
        ) {
            if let Ok(access) = c.cxx_access_specifier() {
                if !c.cxx_method_is_deleted() {
                    let new_method = get_method(c, access);

                    if !parent_methods.contains(&new_method) {
                        methods.push(new_method);
                    }
                }
            } else {
                error!("Could not get access specifier for {c:?}");
            }
        } else if c.kind() == CursorKind::UsingDeclaration {
            // TODO(AL): see if we can make this prettier
            if let Ok(access) = c.cxx_access_specifier() {
                if access == AccessSpecifier::Public {
                    if let Some(odr) = c.first_child_of_kind(CursorKind::OverloadedDeclRef) {
                        for decl in odr.overloaded_decls().unwrap() {
                            if matches!(
                                decl.kind(),
                                CursorKind::CXXMethod | CursorKind::FunctionTemplate
                            ) {
                                let new_method = get_method(decl, access);

                                if !parent_methods.contains(&new_method) {
                                    methods.push(new_method);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    parent_methods.extend_from_slice(&methods);

    // now do any bases
    for c_base in c_class.children_of_kind(CursorKind::CXXBaseSpecifier, false) {
        if let Ok(c_base_decl) = c_base.referenced() {
            match c_base_decl.kind() {
                CursorKind::ClassDecl | CursorKind::StructDecl | CursorKind::ClassTemplate => {
                    let access = c_base
                        .cxx_access_specifier()
                        .unwrap_or(AccessSpecifier::Public);

                    if access == AccessSpecifier::Public {
                        get_methods(c_base_decl, parent_methods);
                    }
                }
                _ => warn!("Unhandled base {c_base_decl:?} of class {c_class:?}"),
            }
        }
    }
}

fn get_method(c: Cursor, access: AccessSpecifier) -> Method {
    let result = c
        .result_ty()
        .map_or_else(|_| String::from("ERR"), get_type_name);

    let num_args = c.num_arguments().unwrap_or(0);
    let mut args = Vec::new();

    for i in 0..num_args {
        let a = c.argument(i).unwrap();
        let name = c.spelling();
        let ty = a.ty().map_or_else(|_| String::from("ERR"), get_type_name);

        args.push(Argument { name, ty });
    }

    Method {
        name: c.spelling(),
        result,
        args,
        is_const: c.cxx_method_is_const(),
        is_static: c.cxx_method_is_static(),
        overload_count: 0,
        access,
        // TODO(AL): This doesn't work - why? None of the methods say they've got template args
        is_template: c.num_template_arguments() > 0,
    }
}

fn get_type_name(ty: Type) -> String {
    ty.fully_qualified_name()
}

fn qualified_name(c: Cursor, name: String) -> String {
    let mut name = name;
    let parent = c.canonical().unwrap().semantic_parent().unwrap();
    if parent.kind() != CursorKind::TranslationUnit {
        name = format!("{}::{name}", parent.spelling());
        name = qualified_name(parent, name);
    }

    name
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
// Copy/pasted from: https://github.com/Manishearth/pathdiff
// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
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
