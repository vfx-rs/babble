use std::fmt::Display;

use bbl_clang::cli_args_with;
use bbl_clang::cursor_kind::CursorKind;
use bbl_extract::{ast::dump, *};

use clap::{Parser, ValueEnum};

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

    /// Max depth to traverse the AST
    #[clap(short, long, value_parser, default_value_t = 20)]
    max_depth: usize,

    /// Whether to show macro definitions or not
    #[clap(long, value_parser, default_value_t = false)]
    show_macro_definitions: bool,

    /// Args to pass to clang
    #[clap(short, long, value_parser)]
    includes: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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

    let includes = args
        .includes
        .iter()
        .map(|i| format!("-I{i}"))
        .collect::<Vec<String>>();

    let cargs = cli_args_with(&includes)?;

    let tu = if let Some(filename) = args.filename {
        parse_file(filename, &cargs, true)?
    } else if let Some(source) = args.source {
        parse_string(source, &cargs, true)?
    } else {
        panic!("Must supply either filename or source argument")
    };

    println!("INCLUDES");
    tu.get_inclusions(|_file, locations| {
        if locations.len() == 1 {
            for location in locations {
                println!("    {:?}", tu.get_cursor_at_location(location));
                println!("    {}", tu.token(*location).spelling());
            }
        }
    });

    let mut skip_kinds = vec![];
    if !args.show_macro_definitions {
        skip_kinds.push(CursorKind::MacroDefinition)
    }

    let mut already_visited = Vec::new();
    if let Some(namespace) = args.namespace {
        let children =
            tu.get_cursor()?
                .children_of_kind_with_name(CursorKind::Namespace, &namespace, true);
        for child in children {
            dump(
                child,
                0,
                args.max_depth,
                &mut already_visited,
                &tu,
                &skip_kinds,
                None,
            );
        }
    } else {
        dump(
            tu.get_cursor()?,
            0,
            args.max_depth,
            &mut already_visited,
            &tu,
            &skip_kinds,
            None,
        );
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
