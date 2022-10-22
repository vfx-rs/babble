use anyhow::Result;
use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use bbl::*;

use clap::{Parser, ValueEnum};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Verbosity {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Parser)]
struct Args {
    /// Filename to parse to extract AST
    #[clap(short, long, value_parser)]
    filename: Option<String>,

    /// C++ snippet to extract
    #[clap(short, long, value_parser)]
    source: Option<String>,

    /// Optional namespace to restrict AST extraction to
    #[clap(short, long, value_parser)]
    namespace: Option<String>,

    /// Verbosity of the output
    #[clap(short, long, arg_enum, value_parser)]
    verbosity: Option<Verbosity>,

    /// Args to pass straight to clang
    #[clap(last = true, value_parser)]
    clang_args: Vec<String>,

    /// cmake find_packages
    #[clap(short, long, value_parser)]
    packages: Vec<String>,

    /// cmake target_link_libraries
    #[clap(short, long, value_parser)]
    link_libraries: Vec<String>,

    /// cmake target_compile_definitions
    #[clap(short, long, value_parser)]
    compile_definitions: Vec<String>,

    /// allowed elements
    #[clap(short, long, value_parser)]
    allow_list: Vec<String>,

    /// blocked elements
    #[clap(short, long, value_parser)]
    block_list: Vec<String>,
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

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(v) = args.verbosity {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or(&v.to_string()))
            .format_timestamp(None)
            .init();
    } else {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
            .format_timestamp(None)
            .init();
    }

    // Point CMake to our library. In a real project we would probably expect this to be done by setting CMAKE_PREFIX_PATH
    // directly in the environment, or perhaps with a config file
    let cmake_prefix_path = PathBuf::from(std::env::var("CMAKE_PREFIX_PATH").unwrap());

    let find_packages: Vec<&str> = args.packages.iter().map(|p| p.as_str()).collect();
    let link_libraries: Vec<&str> = args.link_libraries.iter().map(|p| p.as_str()).collect();
    let compile_definitions: Vec<&str> = args
        .compile_definitions
        .iter()
        .map(|p| p.as_str())
        .collect();
    let allow_list = AllowList::new(args.allow_list.clone());
    println!("ALLOW LIST: {:?}", allow_list);

    let options = BindOptions {
        // We use CMake to configure the compilation and linking of our shim library, so need to point CMAKE_PREFIX_PATH
        // to find the target cpp library as well as provide the library name for find_package() and the actual targets
        // to link against
        cmake_prefix_path: Some(cmake_prefix_path),
        find_packages: &find_packages,
        link_libraries: &link_libraries,
        compile_definitions: &compile_definitions,
        allow_list,
        // We can limit our extraction to a single namespace in the target library. This is usually a good idea to
        // avoid doing extra work (bbl-extract will extract everything it finds, even if it's never used, and the less
        // c++ it has to exract, the less likely it is to choke on constructs we haven't implemented yet)
        limit_to_namespace: args.namespace.as_deref(),
        ..Default::default()
    };

    // parse the given cpp snippet, which just includes the header of the library we want to bind, giving us an AST
    let ast = if let Some(_filename) = args.filename {
        unimplemented!()
    } else if let Some(source) = args.source {
        parse(&source, &options)?
    } else {
        panic!("need filename or source");
    };

    // Now that we have the AST, we can manipulate it, for example to give an external name to the versioned internal
    // namespace, "Test_1_0". We could also ignore and rename methods, try and override bind kinds of classes etc.
    // let ns = ast.find_namespace("pxrInternal_v0_22__pxrReserved__")?;
    // ast.rename_namespace(ns, "pxr");

    // Next we monomorphize it to expand any template parameters
    let ast = ast.monomorphize()?;

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let ffi_path = Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("src")
        .join("ffi.rs")
        .to_string_lossy()
        .to_string();

    // Now bind the AST, which will write, compile and link a shim library, and create the rust ffi binding
    // we also copy the generated ffi.rs into the source tree. This isn't hygienic but using the "correct" method of
    // include!'ing it into the source stops rust-analyzer from working on it, which is worse.
    bind("usd", &out_dir, Some(&ffi_path), &ast, &options)?;

    Ok(())
}
