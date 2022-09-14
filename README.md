# Introduction
babble creates C and Rust bindings to C++ libraries.

See `ARCHITECTURE.md` for an architectural overview.

# Requirements
You will need the clang+llvm-14 package for your system downloaded from here: https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.0
Version 13 will also work, other versions have not been tested.

You will also need a compiler and CMake-3. We've tested gcc-9.4 on Ubuntu-20.04 and VS-2019 on Windows 11.

# Building
You will need to point the crates to your clang installation. There are three ways to do this:

1. Have the clang executable in your `PATH`
2. Set the `CLANG_PATH` environment variable to point to the clang executable
3. Set the `LLVM_CONFIG_PATH` environment variable to point to the llvm-config executable

Then, just run `cargo test`

# Examples
Currently there is a single example building a simple, header-only library:
```bash
> cd examples/take_string
> cargo run
# Output
Hello, VFX Rustaceans!
```

The library in question is found in `testdata/take_string`. You can see it's a single header file but with a CMake config setup so that bbl can find it, parse it and build it using the cmake config and the generated `compile_commands.json`:

```c++
// file: testdata/take_string/include/take_string.hpp
#pragma once 

#include <string>
#include <iostream>

namespace Test_1_0 {

class Class {
public:
    int a;

    std::string take_string(const std::string& s) {
        return std::string("Hello, ") + s + "!";
    }
};
}
```

The binding is set up in the example's `build.rs`:
```rust
// file: examples/take_string/build.rs
use anyhow::Result;
use bbl::{bind, parse, BindOptions};
use std::path::{Path, PathBuf};

pub fn main() -> Result<()> {
    // Point CMake to our library. In a real project we would probably expect this to be done by setting CMAKE_PREFIX_PATH
    // directly in the environment, or perhaps with a config file
    let cmake_prefix_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
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
        // avoid doing extra work (bbl-extract will extract everything it finds, even if it's never used, and the less
        // c++ it has to extract, the less likely it is to choke on constructs we haven't implemented yet)
        limit_to_namespace: Some("Test_1_0"),
        ..Default::default()
    };

    // parse the given cpp snippet, which just includes the header of the library we want to bind, giving us an AST
    let mut ast = parse("#include <take_string.hpp>\n", &options)?;

    // Now that we have the AST, we can manipulate it, for example to give an external name to the versioned internal
    // namespace, "Test_1_0". We could also ignore and rename methods, try and override bind kinds of classes etc.
    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let out_dir = std::env::var("OUT_DIR").unwrap();
    let ffi_path = Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("src")
        .join("ffi.rs")
        .to_string_lossy()
        .to_string();

    // Now bind the AST, which will write, compile and link a shim library, and create the rust ffi binding
    // we also copy the generated ffi.rs into the source tree. This isn't hygienic but using the "correct" method of 
    // include!'ing it into the source stops rust-analyzer from working on it, which is worse.
    bind("take_string", &out_dir, Some(&ffi_path), &ast, &options)?;

    Ok(())
}
```

The example then has a simple high-level wrapper around the binding to make a nice interface, and calls into it to check that passing and returning `std::string`s works as expected:

```rust
// file: examples/take_string/src/main.rs
pub mod ffi;

use std::{ffi::{CStr, CString}, ops::Deref};

pub struct CppString {
    inner: *mut ffi::std_string,
}

impl CppString {
    pub fn new() -> Self {
        unsafe {
            let mut inner = std::ptr::null_mut();
            ffi::std_string_ctor(&mut inner);
            CppString { inner }
        }
    }

    pub fn from(s: &str) -> Self {
        unsafe {
            let mut inner = std::ptr::null_mut();
            let cstr = CString::new(s).unwrap();
            ffi::std_string_from_char_ptr(&mut inner, cstr.as_ptr());
            CppString { inner }
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe {
            let mut char_ptr = std::ptr::null();
            ffi::std_string_c_str(self.inner, &mut char_ptr);
            CStr::from_ptr(char_ptr).to_str().unwrap()
        }
    }
}

impl Drop for CppString {
    fn drop(&mut self) {
        unsafe {
            ffi::std_string_dtor(self.inner)
        }
    }
}

impl From<CppString> for String {
    fn from(cs: CppString) -> Self {
        cs.as_str().to_string()
    }
}

impl Deref for CppString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

pub fn main() {
    let result = unsafe {
        let mut tc = ffi::Test_Class{a: 0};

        let name = CppString::from("VFX Rustaceans");

        let mut out = CppString::new();
        ffi::Test_Class_take_string(&mut tc, out.inner, name.inner);

        out.to_string()
    };

    println!("{}", result);
}
```

