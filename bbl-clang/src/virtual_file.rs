use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};

use log::error;

use crate::compilation_database::CompilationDatabase;

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

/// Write out a temporary file we can pass to libclang to compile.
///
/// Will write to $OUT_DIR if it is set (i.e. if we are being called from build.rs), or std::env::temp_dir() otherwise
pub fn write_temp_file(file_contents: &str) -> Result<PathBuf, std::io::Error> {
    let mut filename = if let Ok(dir) = std::env::var("OUT_DIR") {
        PathBuf::from(&dir)
    } else {
        std::env::temp_dir()
    };

    let mut s = DefaultHasher::new();
    file_contents.hash(&mut s);
    let base = format!("{:x}.cpp", s.finish());
    filename.push(base);
    std::fs::write(&filename, file_contents)?;

    Ok(filename)
}

/// Write a temporary CMake project with a single-file executable containing the `file_contents` that we can use to
/// get the compiler arguments from its generated compile_commands.json
pub fn configure_temp_cmake_project<P: AsRef<Path>>(
    file_contents: &str,
    find_packages: &[&str],
    link_libraries: &[&str],
    extra_includes: &[&str],
    cmake_prefix_path: Option<P>,
    cxx_standard: &str,
) -> Result<(PathBuf, Vec<String>)> {
    let mut dirname = if let Ok(dir) = std::env::var("OUT_DIR") {
        PathBuf::from(&dir)
    } else {
        std::env::temp_dir()
    };

    let mut s = DefaultHasher::new();
    file_contents.hash(&mut s);
    find_packages.hash(&mut s);
    link_libraries.hash(&mut s);

    let project_dir = format!("bbl-{:x}", s.finish());
    dirname.push(project_dir);

    let build_dir = dirname.join("build");

    let query_dir = build_dir
        .join(".cmake")
        .join("api")
        .join("v1")
        .join("query");
    let query_file = query_dir.join("codemodel-v2");

    match std::fs::create_dir_all(&query_dir) {
        Ok(_) => Ok(()),
        Err(e) => {
            if e.kind() == std::io::ErrorKind::AlreadyExists {
                Ok(())
            } else {
                Err(e)
            }
        }
    }?;
    std::fs::write(&query_file, "")?;

    let mut source_filename = dirname.clone();
    source_filename.push("main.cpp");
    std::fs::write(&source_filename, file_contents)?;

    let mut cmakelists = dirname.clone();
    cmakelists.push("CMakeLists.txt");

    let mut find_packages_str = String::new();
    for package in find_packages {
        find_packages_str = format!("{find_packages_str}find_package({package})\n");
    }

    let mut link_libraries_str = String::new();
    for lib in link_libraries {
        link_libraries_str = format!("{link_libraries_str}{lib} ");
    }

    let link_libraries_str = if link_libraries.is_empty() {
        "".to_string()
    } else {
        format!(
            "target_link_libraries(babble_get_args {})",
            link_libraries.join(" ")
        )
    };

    let extra_includes_str = extra_includes.join(" ");
    let include_directories_str = if extra_includes.is_empty() {
        "".to_string()
    } else {
        format!("target_include_directories(babble_get_args PUBLIC {extra_includes_str})")
    };

    std::fs::write(
        &cmakelists,
        format!(
            r#"
cmake_minimum_required(VERSION 3.5)
project(babble_get_args)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD {cxx_standard})

{find_packages_str}

add_library(babble_get_args STATIC main.cpp)
{link_libraries_str}
{include_directories_str}
    "#
        ),
    )?;

    #[cfg(windows)]
    let args = [".", "-B", "build", "-G", "Ninja"];

    #[cfg(unix)]
    let args = [".", "-B", "build"];

    let output = if let Some(pp) = cmake_prefix_path {
        std::process::Command::new("cmake")
            .args(&args)
            .current_dir(&dirname)
            .env("CMAKE_PREFIX_PATH", pp.as_ref())
            .output()
            .map_err(Error::FailedToRunCMake)?
    } else {
        std::process::Command::new("cmake")
            .args(&args)
            .current_dir(&dirname)
            .output()
            .map_err(Error::FailedToRunCMake)?
    };

    if !output.status.success() {
        error!(
            "Failed to run cmake:\n{}\n\n{}",
            std::str::from_utf8(&output.stdout).expect("UTF-8 error parsing cmake stdout"),
            std::str::from_utf8(&output.stderr).expect("UTF-8 error parsing cmake stderr")
        );
        return Err(Error::CMakeError {
            stdout: std::str::from_utf8(&output.stdout)
                .expect("UTF-8 error parsing cmake stdout")
                .to_string(),
            stderr: std::str::from_utf8(&output.stderr)
                .expect("UTF-8 error parsing cmake stderr")
                .to_string(),
        });
    }

    let compilation_db = CompilationDatabase::from_directory(&build_dir)?;
    let commands = compilation_db.get_compile_commands(&source_filename);
    let command_vec = commands.get_commands();
    let mut it_raw_args = command_vec[0].get_arguments().into_iter().skip(1);

    let mut args = Vec::new();
    'outer: while let Some(arg) = it_raw_args.next() {
        for pattern in ["-c", "-o"] {
            if arg.starts_with(pattern) {
                // skip this arg and its value
                it_raw_args.nth(1);
                continue 'outer;
            }
        }

        if arg.starts_with("--driver-mode") || arg == source_filename.as_os_str().to_string_lossy()
        {
            continue;
        }

        args.push(arg);
    }

    Ok((source_filename, args))
}

#[derive(Debug, Clone)]
pub struct FileCommands {
    filename: String,
    args: Vec<String>,
}

impl FileCommands {
    pub fn filename(&self) -> &str {
        self.filename.as_ref()
    }

    pub fn args(&self) -> &[String] {
        self.args.as_ref()
    }
}

pub fn configure_bind_project(
    bind_project_dir: impl AsRef<Path>,
    cmake_prefix_path: Option<impl AsRef<Path>>,
) -> Result<Vec<FileCommands>> {
    let mut dirname = if let Ok(dir) = std::env::var("OUT_DIR") {
        PathBuf::from(&dir)
    } else {
        std::env::temp_dir()
    };

    let mut s = DefaultHasher::new();
    bind_project_dir.as_ref().hash(&mut s);
    dirname.push(format!("bbl-{:x}", s.finish()));
    let build_dir = dirname.join("build");

    if build_dir.exists() {
        std::fs::remove_dir_all(&build_dir)?;
    }

    #[cfg(windows)]
    let args = [
        ".",
        "-B",
        build_dir.as_os_str().to_str().unwrap(),
        "-G",
        "Ninja",
        "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
    ];

    #[cfg(unix)]
    let args = [
        ".",
        "-B",
        build_dir.as_os_str().to_str().unwrap(),
        "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
    ];

    let output = if let Some(pp) = cmake_prefix_path {
        std::process::Command::new("cmake")
            .args(&args)
            .current_dir(bind_project_dir.as_ref())
            .env("CMAKE_PREFIX_PATH", pp.as_ref())
            .output()
            .map_err(Error::FailedToRunCMake)?
    } else {
        std::process::Command::new("cmake")
            .args(&args)
            .current_dir(bind_project_dir.as_ref())
            .output()
            .map_err(Error::FailedToRunCMake)?
    };

    if !output.status.success() {
        error!(
            "Failed to configure cmake binding project:\n{}\n\n{}",
            std::str::from_utf8(&output.stdout).expect("UTF-8 error parsing cmake stdout"),
            std::str::from_utf8(&output.stderr).expect("UTF-8 error parsing cmake stderr")
        );
        return Err(Error::CMakeError {
            stdout: std::str::from_utf8(&output.stdout)
                .expect("UTF-8 error parsing cmake stdout")
                .to_string(),
            stderr: std::str::from_utf8(&output.stderr)
                .expect("UTF-8 error parsing cmake stderr")
                .to_string(),
        });
    }

    let compilation_db = CompilationDatabase::from_directory(&build_dir)?;
    let compile_commands = compilation_db.get_all_compile_commands();

    let mut result = Vec::new();
    for cc in compile_commands.get_commands() {
        let filename = cc.get_filename();
        let args = filter_args(&filename, cc.get_arguments());
        result.push(FileCommands { filename, args });
    }

    Ok(result)
}

fn filter_args(source_filename: &str, raw_args: Vec<String>) -> Vec<String> {
    let mut it_raw_args = raw_args.into_iter().skip(1);

    let mut args = Vec::new();
    'outer: while let Some(arg) = it_raw_args.next() {
        for pattern in ["-c", "-o"] {
            if arg.starts_with(pattern) {
                // skip this arg and its value
                it_raw_args.nth(1);
                continue 'outer;
            }
        }

        if arg.starts_with("--driver-mode") || arg == source_filename {
            continue;
        }

        args.push(arg);
    }

    args
}

#[cfg(test)]
mod tests {
    use crate::index::Index;
    use crate::*;
    use log::*;

    use super::{configure_bind_project, configure_temp_cmake_project};

    #[test]
    fn test_virtual_file() -> Result<(), crate::error::Error> {
        let contents = r#"
        #include <stddef.h>
        class TestVirtual {size_t a;};
        "#;
        let path = super::write_temp_file(contents)?;

        let index = Index::new();
        let tu = index.parse_translation_unit(
            &path,
            &["-std=c++14", "-I/usr/include", "-I/usr/local/include"],
        )?;

        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }

        let cur = tu.get_cursor()?;
        let children = cur.children();
        for child in children {
            println!("{child:?}")
        }

        Ok(())
    }

    #[test]
    fn test_write_temp_cmake_project() -> Result<(), crate::error::Error> {
        let contents = r#"
#include <string>
#include <Imath/ImathVec.h>

namespace Test {
class A {
public:
    int b;
};
}
        "#;

        let cmake_prefix_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("testdata")
            .join("imath");

        let (_, _args) = configure_temp_cmake_project(
            contents,
            &["Imath 3.1 REQUIRED"],
            &["Imath::Imath"],
            &[],
            Some(cmake_prefix_path),
            "11",
        )?;

        Ok(())
    }

    #[test]
    fn test_bind_project() -> Result<(), crate::error::Error> {
        let testdata_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("testdata");

        let imath_path = testdata_path.join("imath");
        let take_string_path = testdata_path.join("take_string");

        let cmake_prefix_path = std::env::join_paths(&[imath_path, take_string_path]).unwrap();

        let bind_project_dir = testdata_path.join("bind_project");

        let source_args = configure_bind_project(bind_project_dir, Some(cmake_prefix_path))?;

        println!("{source_args:?}");

        Ok(())
    }
}
