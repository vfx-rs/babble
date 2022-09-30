use std::path::{Path, PathBuf};

use bbl_clang::{cli_args_with, virtual_file::configure_temp_cmake_project};
use bbl_extract::{parse_file_and_extract_ast};
use bbl_write::{cmake::build_project, gen_c::gen_c, gen_rust_ffi::write_rust_ffi_module};

pub use bbl_extract::{ast::{AST, ClassId, MethodId}, templates::TemplateArgument, qualtype::QualType};
pub use bbl_extract::AllowList;
pub use bbl_translate::translate_cpp_ast_to_c;
use tracing::debug;

pub fn parse(header: &str, options: &BindOptions) -> Result<AST, Error> {
    let (source_filename, mut args) = configure_temp_cmake_project(
        header,
        options.find_packages,
        options.link_libraries,
        options.cmake_prefix_path.as_ref(),
    )?;

    for a in options.clang_args {
        args.push(a.to_string());
    }

    let clang_args = cli_args_with(&args)?;

    let ast = parse_file_and_extract_ast(
        &source_filename,
        &clang_args,
        options.log_diagnostics,
        options.limit_to_namespace,
        &options.allow_list,
    )?;

    Ok(ast)
}

pub fn bind(
    project_name: &str,
    output_directory: &str,
    copy_to: Option<&str>,
    ast: &AST,
    options: &BindOptions,
) -> Result<(), Error> {
    let c_ast = translate_cpp_ast_to_c(ast)?;

    let (c_header, c_source) = gen_c(project_name, &c_ast)?;
    debug!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    build_project(
        project_name,
        output_directory,
        &c_ast,
        options.find_packages,
        options.link_libraries,
        options.compile_definitions,
        options.cmake_prefix_path.as_deref(),
    )?;

    // now that the cmake project has built successfully let's translate the c ast to rust and write out the ffi module
    let module_path = Path::new(output_directory)
        .join("ffi.rs")
        .to_string_lossy()
        .to_string();
    write_rust_ffi_module(&module_path, &c_ast)?;

    // copy to the source tree (or somewhere else) if we asked to
    if let Some(copy_to) = copy_to {
        std::fs::copy(module_path, copy_to).unwrap();
    }

    let c_project_name = format!("{}-c", project_name);

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

pub struct BindOptions<'a, 'b, 'c, 'd, 'e> {
    pub cmake_prefix_path: Option<PathBuf>,
    pub find_packages: &'a [&'a str],
    pub link_libraries: &'b [&'b str],
    pub clang_args: &'c [&'c str],
    pub limit_to_namespace: Option<&'d str>,
    pub compile_definitions: &'e [&'e str],
    pub allow_list: AllowList,
    pub log_diagnostics: bool,
}

impl<'a, 'b, 'c, 'd, 'e> Default for BindOptions<'a, 'b, 'c, 'd, 'e> {
    fn default() -> Self {
        BindOptions {
            cmake_prefix_path: None,
            find_packages: &[],
            link_libraries: &[],
            compile_definitions: &[],
            clang_args: &[],
            log_diagnostics: true,
            limit_to_namespace: None,
            allow_list: AllowList::default(),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Internal clang error")]
    Clang(#[from] bbl_clang::error::Error),
    #[error("Error extracting AST")]
    Extract(#[from] bbl_extract::error::Error),
    #[error("Error translating AST")]
    Translate(#[from] bbl_translate::error::Error),
    #[error("Error writing project")]
    Write(#[from] bbl_write::error::Error),
}

pub fn source_iter(
    error: &impl std::error::Error,
) -> impl Iterator<Item = &(dyn std::error::Error + 'static)> {
    SourceIter {
        current: error.source(),
    }
}

struct SourceIter<'a> {
    current: Option<&'a (dyn std::error::Error + 'static)>,
}

impl<'a> Iterator for SourceIter<'a> {
    type Item = &'a (dyn std::error::Error + 'static);

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current;
        self.current = self.current.and_then(std::error::Error::source);
        current
    }
}
