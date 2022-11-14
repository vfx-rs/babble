use anyhow::Result;
use bbl_clang::{
    cli_args_with, diagnostic::Severity, index::Index, virtual_file::configure_bind_project,
};
use bbl_extract::{
    ast::AST,
    bind::{self, extract_ast_from_binding_tu},
};
use bbl_translate::translate_cpp_ast_to_c;
use bbl_write::{
    cmake::build_project,
    gen_c::gen_c,
    gen_rust_ffi::{write_rust_ffi, write_rust_ffi_module},
};
use clap::{Parser, ValueEnum};
use log::{debug, error, info, warn};
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Parser)]
struct Args {
    /// Path to the CMake binding project (i.e. the directory containing the CMakeLists.txt)
    #[clap(value_name = "PROJECT_PATH")]
    project_path: String,

    /// Path to find cmake packages. Overrides CMAKE_PREFIX_PATH
    #[clap(long, value_parser)]
    cmake_prefix_path: Option<String>,

    /// Output path to write the C and Rust projects
    #[clap(short, long, value_parser)]
    output_path: Option<String>,

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

    let file_commands = configure_bind_project(&args.project_path, args.cmake_prefix_path.clone())?;

    let mut ast = AST::new();
    let mut already_visited = Vec::new();

    for fc in &file_commands {
        let index = Index::new();
        let tu = index.create_translation_unit(fc.filename(), &cli_args_with(fc.args())?)?;
        let mut has_error = false;
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => {
                    has_error = true;
                    error!("{}", d)
                }
            }
        }

        if has_error {
            anyhow::bail!("C++ errors detected. Cannot continue.");
        }

        let cur = tu.get_cursor()?;
        extract_ast_from_binding_tu(cur, &mut already_visited, &mut ast, &tu, true)?;
    }

    let ast = ast.monomorphize()?;
    let c_ast = translate_cpp_ast_to_c(&ast, true)?;

    let output_path = if let Some(p) = args.output_path {
        p
    } else {
        args.project_path.clone()
    };

    let project_name = Path::new(&output_path)
        .components()
        .last()
        .unwrap()
        .as_os_str()
        .to_string_lossy();

    let cmake_prefix_path = args.cmake_prefix_path.map(|s| PathBuf::from(&s));

    build_project(
        project_name.as_ref(),
        &output_path,
        &c_ast,
        &[],                          // options.find_packages,
        &[],                          // options.link_libraries,
        &[],                          // options.extra_includes,
        cmake_prefix_path.as_deref(), // options.cmake_prefix_path.as_deref(),
        "14",                         // options.cxx_standard,
    )?;

    // now that the cmake project has built successfully let's translate the c ast to rust and write out the ffi module
    let module_path = Path::new(&output_path)
        .join("ffi.rs")
        .to_string_lossy()
        .to_string();
    write_rust_ffi_module(&module_path, &c_ast)?;

    // copy to the source tree (or somewhere else) if we asked to
    // if let Some(copy_to) = copy_to {
    //     std::fs::copy(module_path, copy_to).unwrap();
    // }

    let c_project_name = format!("c{}", project_name);

    // link
    println!(
        "cargo:rustc-link-search=native={}/{}/install/lib",
        std::env::var("OUT_DIR").unwrap(),
        c_project_name
    );
    println!("cargo:rustc-link-lib=static={}", c_project_name);

    #[cfg(target_os = "macos")]
    println!("cargo:rustc-link-lib=dylib=cxx");

    #[cfg(target_os = "linux")]
    println!("cargo:rustc-link-lib=dylib=stdc++");

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
