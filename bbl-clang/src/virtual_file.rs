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
pub fn write_temp_cmake_project<P: AsRef<Path>>(
    file_contents: &str,
    find_packages: &[&str],
    link_libraries: &[&str],
    cmake_prefix_path: Option<P>,
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

    let link_libraries_str = if link_libraries_str.is_empty() {
        "".to_string()
    } else {
        format!("target_link_libraries({link_libraries_str})")
    };

    std::fs::write(
        &cmakelists,
        format!(
            r#"
cmake_minimum_required(VERSION 3.5)
project(babble_get_args)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

{find_packages_str}

add_library(babble_get_args STATIC main.cpp)
target_link_libraries(babble_get_args {link_libraries_str})
    "#
        ),
    )?;

    let output = if let Some(pp) = cmake_prefix_path {
        std::process::Command::new("cmake")
            .args([".", "-B", "build"])
            .current_dir(&dirname)
            .env("CMAKE_PREFIX_PATH", pp.as_ref())
            .output()?
    } else {
        std::process::Command::new("cmake")
            .args([".", "-B", "build"])
            .current_dir(&dirname)
            .output()?
    };

    if !output.status.success() {
        error!(
            "Failed to run cmake:\n{}\n\n{}",
            std::str::from_utf8(&output.stdout).expect("UTF-8 error parsing cmake stdout"),
            std::str::from_utf8(&output.stderr).expect("UTF-8 error parsing cmake stderr")
        );
        return Err(Error::CMakeError);
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

#[cfg(test)]
mod tests {
    use crate::index::Index;
    use crate::*;
    use log::*;

    use super::write_temp_cmake_project;

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

        let (_, _args) = write_temp_cmake_project(
            contents,
            &["Imath 3.1 REQUIRED"],
            &["Imath::Imath"],
            Some(cmake_prefix_path),
        )?;

        Ok(())
    }
}
