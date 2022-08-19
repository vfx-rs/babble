use std::fmt::Display;

use bbl::{*, cursor_kind::CursorKind, cursor::USR};

use clap::{Parser, ValueEnum};

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

    /// Max depth to traverse the AST
    #[clap(short, long, value_parser)]
    max_depth: Option<usize>,
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

    let tu = parse_file(args.filename, 
        &[            
            // "-cc1",
            // "-triple",
            // "x86_64-unknown-linux-gnu", 
            // "-emit-obj",
            // "-mrelax-all",
            // "--mrelax-relocations",
            // "-disable-free",
            // "-clear-ast-before-backend",
            // "-disable-llvm-verifier",
            // "-discard-value-names",
            // "-main-file-name",
            // "args.cpp",
            // "-mrelocation-model",
            // "static",
            // "-mframe-pointer=all",
            "-fmath-errno",
            "-ffp-contract=on",
            "-fno-rounding-math",
            // "-mconstructor-aliases",
            // "-funwind-tables=2",
            // "-target-cpu",
            // "x86-64",
            // "-tune-cpu",
            // "generic",
            // "-mllvm",
            // "-treat-scalable-fixed-error-as-warning",
            // "-debugger-tuning=gdb",
            // "-fcoverage-compilation-dir=/home/anders/code/clang_args/build",
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            // "-internal-isystem",
            // "/usr/lib/gcc/x86_64-linux-gnu/9/../../../../include/c++/9",
            // "-internal-isystem",
            // "/usr/lib/gcc/x86_64-linux-gnu/9/../../../../include/x86_64-linux-gnu/c++/9",
            // "-internal-isystem",
            // "/usr/lib/gcc/x86_64-linux-gnu/9/../../../../include/c++/9/backward",
            // "-internal-isystem",
            // "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0/include",
            // "-internal-isystem",
            // "/usr/local/include",
            // "-internal-isystem",
            // "/usr/lib/gcc/x86_64-linux-gnu/9/../../../../x86_64-linux-gnu/include",
            // "-internal-externc-isystem",
            // "/usr/include/x86_64-linux-gnu",
            // "-internal-externc-isystem",
            // "/include",
            // "-internal-externc-isystem",
            // "/usr/include",
            // "-fdeprecated-macro",
            // "-fdebug-compilation-dir=/home/anders/code/clang_args/build",
            // "-ferror-limit",
            // "19",
            // "-fgnuc-version=4.2.1",
            // "-fcxx-exceptions",
            // "-fexceptions",
            // "-fcolor-diagnostics",
            // "-faddrsig",
            // "-D__GCC_HAVE_DWARF2_CFI_ASM=1",
            // "-x",
            // "c++",

            "-std=c++14",
            // "-I/usr/include",
            // "-I/usr/local/include",
            "-I/home/anders/packages/openexr/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include/Imath",
        ],
        true)?;

    let max_depth = args.max_depth.unwrap_or(6);

    let mut already_visited = Vec::new();
    if let Some(namespace) = args.namespace {
        let children = tu.get_cursor()?.children_of_kind_with_name(CursorKind::Namespace, &namespace, true);
        for child in children {
            dump(child, 0, max_depth, &mut already_visited, &tu);
        }
    } else {
        dump(tu.get_cursor()?, 0, max_depth, &mut already_visited, &tu);
    }


    Ok(())
}

fn dump(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
) {
    if depth > max_depth {
        println!("⋱");
        return;
    }
    let indent = format!("{:width$}", "", width = depth * 4);

    let template_args = if c.num_template_arguments() != -1 {
        format!("[{}]", c.num_template_arguments())
    } else {
        "".to_string()
    };

    match c.kind() {
        CursorKind::IntegerLiteral => {
            println!("{}: {}", c.kind(), tu.token(c.location()).spelling());
        }
        _ => println!("{}: {} {} {}", c.kind(), c.display_name(), c.usr(), template_args),
    }

    if let Ok(ty) = c.ty() {
        let args = ty.template_argument_types()
                                        .map(|v| {
                                            v.iter()
                                             .map(|t| if let Some(t) = t { format!("{}", t)} else { "NonType".to_string() } )
                                             .collect::<Vec<String>>()
                                        });

        let template_args = if let Some(args) = args {
            format!("<{}>", args.join(", "))
        } else {
            "".to_string()
        };
        let indent = format!("{:width$}", "", width = depth.saturating_sub(1) * 4 + 2);
        println!("{indent}𝜏 {}: {} {}", ty.spelling(), ty.kind(), template_args);
    }


    if let Ok(cr) = c.referenced() {
        if cr != c {
            if already_visited.contains(&cr.usr()) {
                let template_args = if c.num_template_arguments() != -1 {
                    format!("[{}]", c.num_template_arguments())
                } else {
                    "".to_string()
                };

                println!("{indent}↪ {}: {} {} {} 🗸", cr.kind(), cr.display_name(), cr.usr(), template_args);
            } else {
                if !cr.usr().is_empty() {
                    already_visited.push(cr.usr());
                }
                print!("{indent}↪ ");
                dump(cr, depth + 1, max_depth, already_visited, tu);
            }
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        if !child.usr().is_empty() {
            already_visited.push(child.usr());
        }

        let icon = match child.kind() {
            CursorKind::ClassDecl => "●",
            CursorKind::ClassTemplate => "○",
            CursorKind::FunctionDecl => "ƒ",
            CursorKind::FunctionTemplate => "ⓕ",
            CursorKind::CXXMethod => "ɱ",
            _ => "▸",
        };

        print!("{indent}{icon} ");

        dump(child, depth + 1, max_depth, already_visited, tu);
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
