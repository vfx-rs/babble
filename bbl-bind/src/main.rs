use anyhow::Result;
use bbl_clang::{
    cli_args_with, diagnostic::Severity, index::Index, virtual_file::configure_bind_project,
};
use bbl_extract::{ast::AST, bind::extract_ast_from_binding_tu};
use bbl_translate::translate_cpp_ast_to_c;
use bbl_write::{gen_c::gen_c, gen_rust_ffi::write_rust_ffi};
use clap::{Parser, ValueEnum};
use log::{debug, error, info, warn};
use std::{fmt::Display, path::Path};

#[derive(Parser)]
struct Args {
    /// Path to the CMake binding project (i.e. the directory containing the CMakeLists.txt)
    #[clap(value_name = "PROJECT_PATH")]
    project_path: String,

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

    // println!("{ast:?}");
    let ast = ast.monomorphize()?;

    let c_ast = translate_cpp_ast_to_c(&ast, true)?;
    println!("{c_ast:?}");

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    let mut ffi = String::new();
    write_rust_ffi(&mut ffi, &c_ast)?;

    println!("FFI:\n---------\n{ffi}");

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
