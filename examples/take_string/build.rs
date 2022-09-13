use bbl::{parse, bind, BindOptions};
use std::path::{PathBuf, Path};
use anyhow::Result;

pub fn main() -> Result<()> {
    let cmake_prefix_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent().unwrap()
        .parent().unwrap()
        .join("testdata")
        .join("take_string");

    let options = BindOptions {
            // We use CMake to configure the compilation and linking of our shim library, so need to point CMAKE_PREFIX_PATH 
            // to find the target cpp library as well as provide the library name for find_package() and the actual targets
            // to link against
            cmake_prefix_path: Some(cmake_prefix_path),
            find_packages: &["take_string REQUIRED"],
            link_libraries: &["take_string::take_string"],
            // We can limit our extraction to a single namespace in the target library. This is usually a good idea to 
            // avoid doing extra work (bbl-extract will extract everything it finds, even if it's never used)
            limit_to_namespace: Some("Test_1_0"),
            ..Default::default()
        };

    // parse the given cpp snippet, which just includes the header of the library we want to bind, giving us an AST
    let mut ast = parse(
"#include <take_string.hpp>\n",
        &options,
    )?;

    // Now that we have the AST, we can manipulate it, for example to give an external name to the versioned internal
    // namespace, "Test_1_0"
    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let ffi_path = Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("src").join("ffi.rs").to_string_lossy().to_string();
    // panic!("cargo:warn=out_dir: {out_dir}");

    // Now bind the AST, which will write, compile and link a shim library, and create the rust ffi binding
    bind("take_string", &out_dir, Some(&ffi_path), &ast, &options)?;

    Ok(())
}
