mod common;

use std::path::PathBuf;

use bbl_clang::{cli_args, cli_args_with, virtual_file::configure_temp_cmake_project};
use bbl_extract::{class::ClassBindKind, parse_file_and_extract_ast, parse_string_and_extract_ast};
use bbl_translate::translate_cpp_ast_to_c;
use common::run_test;

use bbl_write::{cmake::build_project, error::Error, gen_c::gen_c};
use tracing::debug;

use crate::common::init_log;

#[test]
fn write_simple_class() -> Result<(), Error> {
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {
class Class {
public:
    int method1();
    void method2(const Class& c);
};
}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 2);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

    Ok(())
}

#[test]
fn write_simple_valuetype() -> Result<(), Error> {
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {

class Class {
public:
    int a;
    float b;
};

}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    // let class = ast.find_class("Test_1_0::Class")?;
    // ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 0);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

    Ok(())
}

#[test]
fn write_valuetype_pass_by_value() -> Result<(), Error> {
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {

class Class {
public:
    int a;
    float b;
};

void fun(Class c);

}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 1);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

    Ok(())
}

#[test]
fn write_opaqueptr_pass_by_value() -> Result<(), Error> {
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {
class Class {
    int _c; // will force opaqueptr as no longer POD
public:
    int a;
    float b;
};
void fun(Class c);
}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 1);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_nested_valuetype() -> Result<(), Error> {
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {
class A {
public:
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 2);
    assert_eq!(c_ast.functions.len(), 0);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_nested_valuetype_with_forced_member() -> Result<(), Error> {
    init_log();
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test_1_0 {
class A {
public:
    A(const A&); // presence of copy ctor means no POD any more, but we *know* it's a trivial constructor
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
        &cli_args()?,
        true,
        None,
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let class = ast.find_class("Test_1_0::A")?;
    ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 2);
    assert_eq!(c_ast.functions.len(), 1);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_includes() -> Result<(), Error> {
    init_log();
    let mut ast = parse_string_and_extract_ast(
        r#"
#include <stddef.h>
#include <string>

namespace Test_1_0 {
class A {
public:
    A(const A&); // presence of copy ctor means no POD any more, but we *know* it's a trivial constructor
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
        &cli_args()?,
        true,
        Some("Test_1_0"),
    )?;

    ast.pretty_print(0);

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let class = ast.find_class("Test_1_0::A")?;
    ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    println!("ast has {} includes", ast.includes().len());
    println!("cast has {} includes", c_ast.includes.len());
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 2);
    assert_eq!(c_ast.functions.len(), 1);

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn take_std_string_by_value() -> Result<(), Error> {
    run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            r#"
    #include <string>

    namespace Test_1_0 {
    class Class {
    public:
        std::string take_string(std::string s);
    };
    }
            "#,
            &cli_args()?,
            true,
            Some("Test_1_0"),
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0)?;

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 7);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_take_std_string() -> Result<(), Error> {
    run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            r#"
    #include <string>

    namespace Test_1_0 {
    class Class {
    public:
        Class() {}
        std::string take_string(const std::string& s) const;
        float returns_float() const;
        Class returns_class();
        const Class& returns_class_ref() const;
        const Class* returns_class_ptr() const;
        static std::string static_take_string(std::string s);
    };
    }
            "#,
            &cli_args()?,
            true,
            Some("Test_1_0"),
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0)?;

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 13);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn build_take_std_string() -> Result<(), Error> {
    run_test(|| {
        let contents = "#include <take_string.hpp>\n";

        let cmake_prefix_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("testdata")
            .join("take_string");

        let find_packages = ["take_string REQUIRED"];
        let link_libraries = ["take_string::take_string"];

        let (source_filename, args) = configure_temp_cmake_project(
            contents,
            &find_packages,
            &link_libraries,
            Some(&cmake_prefix_path),
        )?;

        let mut ast = parse_file_and_extract_ast(
            &source_filename,
            &cli_args_with(&args)?,
            true,
            Some("Test_1_0"),
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0)?;

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 7);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        build_project(
            "build_take_std_string",
            "/tmp",
            &c_ast,
            &find_packages,
            &link_libraries,
            Some(&cmake_prefix_path),
        )?;

        Ok(())
    })
}

#[tracing::instrument]
fn fun_b(arg: &str) {}

#[tracing::instrument]
fn fun_a(arg: &str) {
    fun_b("blah blah blah");
}

#[cfg(feature = "telemetry")]
#[test]
fn test_tracing() {
    use opentelemetry::global;
    use tracing::span;
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::Registry;

    global::set_text_map_propagator(opentelemetry_jaeger::Propagator::new());
    let tracer = opentelemetry_jaeger::new_pipeline()
        .install_simple()
        .unwrap();

    // Create a tracing layer with the configured tracer
    let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

    // Use the tracing subscriber `Registry`, or any other subscriber
    // that impls `LookupSpan`
    let subscriber = Registry::default().with(telemetry);

    // Trace executed code
    tracing::subscriber::with_default(subscriber, || {
        // Spans will be sent to the configured OpenTelemetry exporter
        let root = span!(tracing::Level::TRACE, "app_start", work_units = 2);
        let _enter = root.enter();

        tracing::error!("This event will be logged in the root span.");

        fun_a("sooooo");
    });
    global::shutdown_tracer_provider(); // sending remaining spans
}
