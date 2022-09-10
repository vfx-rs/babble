use std::path::PathBuf;

use bbl_clang::{cli_args_with, virtual_file::write_temp_cmake_project};
use bbl_extract::parse_file_and_extract_ast;
use bbl_write::{cmake::build_project, gen_c::gen_c};

pub use bbl_extract::ast::AST;
pub use bbl_translate::translate_cpp_ast_to_c;

pub fn parse(header: &str, options: &BindOptions) -> Result<AST, Error> {
    let (source_filename, mut args) = write_temp_cmake_project(
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
    )?;

    Ok(ast)
}

pub fn bind(project_name: &str, output_directory: &str, ast: &AST, options: &BindOptions) -> Result<(), Error> {
    let c_ast = translate_cpp_ast_to_c(ast)?;

    let (c_header, c_source) = gen_c(project_name, ast, &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    build_project(
        project_name,
        output_directory,
        ast,
        &c_ast,
        options.find_packages,
        options.link_libraries,
        options.cmake_prefix_path.as_deref(),
    )?;

    // now that the cmake project has built successfully let's translate the c ast to rust and write out the ffi module

    Ok(())
}

pub struct BindOptions<'a> {
    pub cmake_prefix_path: Option<PathBuf>,
    pub find_packages: &'a [&'static str],
    pub link_libraries: &'a [&'static str],
    pub clang_args: &'a [&'static str],
    pub log_diagnostics: bool,
    pub limit_to_namespace: Option<&'static str>,
}

impl<'a> Default for BindOptions<'a> {
    fn default() -> Self {
        BindOptions {
            cmake_prefix_path: None,
            find_packages: &[],
            link_libraries: &[],
            clang_args: &[],
            log_diagnostics: true,
            limit_to_namespace: None,
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
