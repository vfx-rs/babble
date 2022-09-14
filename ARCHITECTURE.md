# Workspace Structure

babble is split into several crates, each representing a different stage in the binding process. The following describes a thousand-foot view of the data flow as the bindings are created.

## bbl-clang
High-level bindings over the [clang-sys crate](https://crates.io/crates/clang-sys).

This provides more ergonomic wrappers over the C types, to make it more pleasant to work with libclang. You shouldn't need to dive in here unless you need to expose a missing function or type from libclang.

## bbl-extract
Uses `bbl-clang` to parse C++ code in a file or in a string* and generates a flattened AST representation from it.

The main function to look at here is `bbl_extract::ast::extract_ast()`, which recursively walks clang's AST, extracting classes, methods, fields, functions and typedefs, and storing information about them into a flat representation in the `bbl_extract::ast::AST` struct.

The actual entry point when initiating a binding is `bbl_extract::ast::extract_ast_from_namespace()` which just sets up some working data, and handles finding the root namespace from the translation unit to restrict parsing to.

Note that the `std::` namespace is explicitly ignored because it is a nightmare. stdlib types are extracted manually (see `bbl_extract::stdlib`) when they are encountered. Currently only `std::string` is supported.

Once the AST is created, it can be modified before handing off to translation. See for example `bbl_translate::tests::binding::test_binding_rename()`.

> [*] Note: parsing strings is supported by writing the string to a file in a temporary directory, then parsing that file. This seems to be the only way to get libclang to parse it correctly. This is done in `bbl_clang::virtual_file`_

## bbl-translate
Once the AST is extracted, it is handed off to `bbl_translate::translate_cpp_ast_to_c()`, where `bbl_extract::ast::AST` is converted to `bbl_translate::CAST`.

The basic premise is as follows:
- For each class:
    - Translate its fields to their equivalent c types
    - Separate out its methods as functions
        - Move the return type to an out parameter and replace with an integer return to indicate error code
        - Insert a self parameter
        - Generate the necessary expressions to call the original function and do all the required casting, handle exceptions etc.

Template classes are specialized by repeating this process for each specialization. The required template arguments are taken from any typedefs in the target library (or the binding source file), or specified during the AST modification step after extraction.

## bbl-write
The finished `CAST` is passed to `bbl_write::build_project()` which uses `bbl_write::gen_c::gen_c()` and `bbl_write::gen_rust_ffi::gen_rust_ffi()` to generate the C and Rust bindings, respectively. `build_project()` then writes a CMake project for building the C library shim, and configures and builds it by calling out to CMake.

See `bbl_write::tests::write::build_take_std_string()` for an example.

## bbl
All this is wrapped up into the simple `bbl::parse()` and `bbl::bind()` functions, which are intended to be called from a binding project's `build.rs`. `bind()` also prints the necessary link statements to stdout for cargo.

See `examples/take_string` to see how this is used in a simple binding project.

> Note that library inspection is not implemented yet, so only header-only libraries are supported. The hard work of parsing project files was already done for cppmm so can just be ported over.