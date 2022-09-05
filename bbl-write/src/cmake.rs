use std::path::Path;

use bbl_extract::ast::AST;
use bbl_translate::CAST;

use crate::error::Error;
use crate::gen_c;

use std::fmt::Write;

pub fn build_project<P: AsRef<Path>>(
    project_name: &str,
    output_directory: P,
    ast: &AST,
    c_ast: &CAST,
    find_packages: &[&str],
    link_libraries: &[&str],
    cmake_prefix_path: Option<&Path>,
) -> Result<(), Error> {
    let output_directory = output_directory.as_ref().join(project_name);
    if !output_directory.exists() {
        std::fs::create_dir_all(&output_directory).map_err(|e| Error::FailedToGenerateCMake {
            source: Box::new(e),
        })?;
    }

    let cmakelists_path = output_directory.join("CMakeLists.txt");
    let header_path = output_directory.join(format!("{}.h", project_name));
    let source_path = output_directory.join(format!("{}.cpp", project_name));

    let (header_contents, source_contents) = gen_c(project_name, ast, c_ast)?;

    let source_contents = format!("#include \"{project_name}.h\"\n\n{source_contents}");

    std::fs::write(header_path, header_contents).map_err(|e| Error::FailedToGenerateCMake {
        source: Box::new(e),
    })?;
    std::fs::write(source_path, source_contents).map_err(|e| Error::FailedToGenerateCMake {
        source: Box::new(e),
    })?;

    let mut find_packages_str = String::new();
    for package in find_packages {
        writeln!(&mut find_packages_str, "find_package({package})")?;
    }

    let mut link_libraries_str = String::new();
    if !link_libraries.is_empty() {
        link_libraries_str = format!("target_link_libraries({project_name} PUBLIC\n");
        for lib in link_libraries {
            writeln!(&mut link_libraries_str, "  {lib}")?;
        }

        writeln!(&mut link_libraries_str, ")")?;
    }

    let prefix_str = if let Some(cmake_prefix_path) = cmake_prefix_path {
        format!(
            "set(CMAKE_PREFIX_PATH \"{}\")",
            cmake_prefix_path.display()
        )
    } else {
        String::new()
    };

    std::fs::write(
        cmakelists_path,
        format!(
            r#"cmake_minimum_required(VERSION 3.5)
project({project_name})

{prefix_str}

{find_packages_str}

add_library({project_name} STATIC {project_name}.cpp)
{link_libraries_str}
"#
        ),
    )
    .map_err(|e| Error::FailedToGenerateCMake {
        source: Box::new(e),
    })?;

    Ok(())
}
