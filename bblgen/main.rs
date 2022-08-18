use std::fmt::Display;

use bbl::*;
use log::*;

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
    #[clap(value_parser)]
    filename: String,

    /// Optional namespace to restrict AST extraction to
    #[clap(short, long, value_parser)]
    namespace: Option<String>,

    /// Verbosity of the output
    #[clap(short, long, arg_enum, value_parser)]
    verbosity: Option<Verbosity>,

    /// Args to pass straight to clang
    #[clap(last=true, value_parser)]
    clang_args: Vec<String>,
}

impl Display for Verbosity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Verbosity::Trace => { write!(f, "trace") },
            Verbosity::Debug => { write!(f, "debug") },
            Verbosity::Info => { write!(f, "info") },
            Verbosity::Warn => { write!(f, "warn") },
            Verbosity::Error => { write!(f, "error") },
        }
    }
}

impl Verbosity {
    fn to_string(&self) -> String {
        format!("{}", self)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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

    let tu = parse_file(args.filename, 
        &[            
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
            "-I/home/anders/packages/openexr/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include/Imath",
        ],
        true)?;

    let cur = tu.get_cursor()?;


    let ast = ast_from_namespace(&args.namespace.unwrap_or("".to_string()), cur, &tu);

    /*
    #[rustfmt::skip]
    let ast = AstExtractor::new()
        .namespace("Imath_3_1")
            .record("Vec3<T>")
                .method("operator^").ignore()
            .specialize("T", "float")
        .extract();    
    */


    debug!("\n\n");
    ast.pretty_print(0);

    Ok(())
}
