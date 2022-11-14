use std::path::Path;
use std::process::Command;

use bbl_translate::CAST;

use crate::error::Error;
use crate::gen_c::gen_c;

use std::fmt::Write;

pub fn build_project<P: AsRef<Path>>(
    project_name: &str,
    output_directory: P,
    c_ast: &CAST,
    find_packages: &[&str],
    link_libraries: &[&str],
    extra_includes: &[&str],
    cmake_prefix_path: Option<&Path>,
    cxx_standard: &str,
) -> Result<(), Error> {
    let project_name = format!("{project_name}-c");

    let output_directory = output_directory.as_ref().join(&project_name);
    if !output_directory.exists() {
        std::fs::create_dir_all(&output_directory).map_err(|e| Error::FailedToGenerateCMake {
            source: Box::new(e),
        })?;
    }

    let cmakelists_path = output_directory.join("CMakeLists.txt");
    let header_path = output_directory.join(format!("{}.h", &project_name));
    let source_path = output_directory.join(format!("{}.cpp", &project_name));

    let (header_contents, source_contents) = gen_c(&project_name, c_ast)?;

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

    let extra_includes_str = if extra_includes.is_empty() {
        "".to_string()
    } else {
        format!(
            "target_include_directories({project_name} PUBLIC {}",
            extra_includes.join(", ")
        )
    };

    let prefix_str = if let Some(cmake_prefix_path) = cmake_prefix_path {
        format!("set(CMAKE_PREFIX_PATH \"{}\")", cmake_prefix_path.display()).replace('\\', "/")
    } else {
        String::new()
    };

    let build_dir = output_directory.join("build");
    let install_dir = format!("{}", output_directory.join("install").display()).replace('\\', "/");

    std::fs::write(
        cmakelists_path,
        format!(
            r#"cmake_minimum_required(VERSION 3.5)
project({project_name})

{prefix_str}

set(CMAKE_INSTALL_PREFIX {})
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_CXX_STANDARD {cxx_standard})

{find_packages_str}

add_library({project_name} STATIC {project_name}.cpp)
{link_libraries_str}
{extra_includes_str}

install(TARGETS {project_name} EXPORT {project_name} DESTINATION lib)
install(EXPORT {project_name} DESTINATION lib/cmake)
"#,
            install_dir
        ),
    )
    .map_err(|e| Error::FailedToGenerateCMake {
        source: Box::new(e),
    })?;

    // configure the project
    let configure_output = Command::new("cmake")
        .args(&[
            "-B",
            build_dir.as_os_str().to_string_lossy().as_ref(),
            output_directory.as_os_str().to_string_lossy().as_ref(),
        ])
        .output()?;

    if !configure_output.status.success() {
        return Err(Error::FailedToConfigureCMake {
            stdout: std::str::from_utf8(&configure_output.stdout)
                .expect("UTF-8 error parsing cmake stdout")
                .to_string(),
            stderr: std::str::from_utf8(&configure_output.stderr)
                .expect("UTF-8 error parsing cmake stderr")
                .to_string(),
        });
    }

    // build the project
    let build_output = Command::new("cmake")
        .args(&[
            "--build",
            build_dir.as_os_str().to_string_lossy().as_ref(),
            "--config",
            "Release",
        ])
        .output()?;

    if !build_output.status.success() {
        return Err(Error::FailedToBuildCMake {
            stdout: std::str::from_utf8(&build_output.stdout)
                .expect("UTF-8 error parsing cmake stdout")
                .to_string(),
            stderr: std::str::from_utf8(&build_output.stderr)
                .expect("UTF-8 error parsing cmake stderr")
                .to_string(),
        });
    }

    // install the project
    let install_output = Command::new("cmake")
        .args(&[
            "--install",
            build_dir.as_os_str().to_string_lossy().as_ref(),
        ])
        .output()?;

    if !install_output.status.success() {
        return Err(Error::FailedToInstallCMake {
            stdout: std::str::from_utf8(&install_output.stdout)
                .expect("UTF-8 error parsing cmake stdout")
                .to_string(),
            stderr: std::str::from_utf8(&install_output.stderr)
                .expect("UTF-8 error parsing cmake stderr")
                .to_string(),
        });
    }

    Ok(())
}
