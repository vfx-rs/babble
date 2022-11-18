use anyhow::{Context, Result};
use bbl_clang::{
    cli_args_with, diagnostic::Severity, index::Index, virtual_file::configure_bind_project,
};
use bbl_extract::ast::AST;
use bbl_translate::{translate_cpp_ast_to_c, CAST};
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

mod bind;
use bind::extract_ast_from_binding_tu;

use crate::bind::extract_ast_from_binding;

#[derive(Parser)]
struct Args {
    /// Path to the build_config.json file containing build settings
    /// The structure of the JSON file should contain the following:
    /// {
    ///     "build_system": "cmake",        //< only option allowed for now
    ///     "project_name": "<string>"      //< base name of project. This app will generate project-c, project-sys etc.
    ///     "find_packages": ["<string>"]   //< list of strings to pass as cmake find_package() commands
    ///     "link_libraries": ["<string>"]  //< list of strings to pass to the target_link_libraries() command
    ///     "extra_includes": ["<string">]  //< list of paths to add to a target_include_directories() commands
    ///     "cxx_standard": "<string>"      //< c++ standard to use, passed to set(CMAKE_CXX_STANDARD)
    /// }
    #[clap(value_name = "CONFIG_PATH")]
    config_path: String,

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
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
            .format_timestamp(None)
            .init();
    }

    let config_path = Path::new(&args.config_path);
    if !config_path.is_file() {
        anyhow::bail!(
            "Provided build_config path {} does not exist or is not a file",
            config_path.display()
        );
    }

    let build_config = match bbl_util::read_build_config(&config_path) {
        Ok(bc) => bc,
        Err(e) => {
            anyhow::bail!("Could not read build_config: {e}");
        }
    };

    let project_root = match config_path.parent() {
        Some(p) => p,
        None => anyhow::bail!(
            "Could not get parent directory of config path {}",
            config_path.display()
        ),
    };

    let bind_project_path = project_root.join(format!("{}-bind", build_config.project_name));

    let cmakelists = Path::new(&bind_project_path).join("CMakeLists.txt");

    if !cmakelists.is_file() {
        anyhow::bail!(
            "Could not find CMakeLists.txt in path \"{}\"",
            bind_project_path.display(),
        );
    }

    let file_commands = configure_bind_project(&bind_project_path, args.cmake_prefix_path.clone())?;

    let ast = extract_ast_from_binding(&file_commands)?;

    let ast = ast.monomorphize()?;
    let c_ast = translate_cpp_ast_to_c(&ast, true)?;

    let cmake_prefix_path = args.cmake_prefix_path.map(|s| PathBuf::from(&s));

    build_project(
        build_config.project_name.as_ref(),
        &project_root,
        &c_ast,
        &build_config.find_packages,  // options.find_packages,
        &build_config.link_libraries, // options.link_libraries,
        &build_config.extra_includes, // options.extra_includes,
        cmake_prefix_path.as_deref(), // options.cmake_prefix_path.as_deref(),
        build_config.cxx_standard,    // options.cxx_standard,
    )?;

    let c_project_name = format!("{}-c", build_config.project_name);
    let c_project_path = project_root.join(&c_project_name);

    let ffi_project_name = format!("{}-sys", build_config.project_name);

    write_rust_ffi_project(&c_ast, project_root, &c_project_name, &ffi_project_name)?;

    Ok(())
}

fn write_rust_ffi_project(
    c_ast: &CAST,
    project_root: &Path,
    c_project_name: &str,
    ffi_project_name: &str,
) -> Result<()> {
    let ffi_project_path = project_root.join(&ffi_project_name);
    let cargo_toml_path = ffi_project_path.join("Cargo.toml");
    let src_path = ffi_project_path.join("src");
    let module_path = src_path.join("lib.rs");
    let build_rs_path = ffi_project_path.join("build.rs");

    std::fs::create_dir_all(&src_path)
        .with_context(|| format!("Failed to create directory {}", src_path.display()))?;
    write_rust_ffi_module(&module_path, c_ast)?;

    // write Cargo.toml
    std::fs::write(
        &cargo_toml_path,
        format!(
            r#"[package]
name = "{ffi_project_name}"
version = "0.1.0"
edition = "2021"

"#
        ),
    )?;

    // write build.rs
    std::fs::write(
        &build_rs_path,
        format!(
            r#"
fn main() {{
    println!("cargo:rustc-link-search=native={}/install/lib");
    println!("cargo:rustc-link-lib=static={c_project_name}");

    #[cfg(target_os = "macos")]
    println!("cargo:rustc-link-lib=dylib=cxx");

    #[cfg(target_os = "linux")]
    println!("cargo:rustc-link-lib=dylib=stdc++");
}}  
"#,
            project_root.join(&c_project_name).display()
        ),
    )?;

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
