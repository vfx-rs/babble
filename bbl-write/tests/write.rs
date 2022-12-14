mod common;

use std::path::PathBuf;

use bbl_clang::{cli_args, cli_args_with, virtual_file::configure_temp_cmake_project};
use bbl_extract::{
    class::{ClassBindKind, OverrideList},
    parse_file_and_extract_ast, parse_string_and_extract_ast, AllowList,
};
use bbl_translate::translate_cpp_ast_to_c;
use common::run_test;

use bbl_write::{cmake::build_project, error::Error, gen_c::gen_c, gen_rust_ffi::write_rust_ffi};

use crate::common::init_log;

use indoc::indoc;

#[test]
fn write_simple_class() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 5);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    })
}

#[test]
fn write_simple_template() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {

template <typename T>
class Class {
public:
    T t;
    int method1(const T&);
    void method2(const Class& c);
};

typedef Class<float> ClassFloat;
}
            "#,
            &cli_args()?,
            true,
            None,
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        // assert_eq!(c_ast.structs.len(), 1);
        // assert_eq!(c_ast.functions.len(), 5);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    })
}

#[test]
fn write_simple_valuetype() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 3);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    })
}

#[test]
fn write_valuetype_pass_by_value() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 4);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    })
}

#[test]
fn write_opaqueptr_pass_by_value() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 5);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_nested_valuetype() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 6);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_nested_valuetype_with_forced_member() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let class = ast.find_class("Test_1_0::A")?;
        ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 5);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_includes() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let class = ast.find_class("Test_1_0::A")?;
        ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 5);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn take_std_string_by_value() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 10);

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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        println!("{ast:?}");

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 14);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_implicit_ctor() -> Result<(), Error> {
    run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Class {
            };

            class Class2 {
            };
        "#
            ),
            &cli_args_with(&["-std=c++11"])?,
            true,
            None,
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        println!("{ast:?}");
        /*
            assert_eq!(
                format!("{ast:?}"),
                indoc!(
                    r#"
            Namespace c:@S@Class Class None
            ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false rof=[ctor ] template_parameters=[] specializations=[] namespaces=[]
            Field b: float
            Method DefaultConstructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@Class# Class rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

        "#
                )
            );
            */

        let class_id = ast.find_class("Class2")?;
        ast.class_set_bind_kind(class_id, ClassBindKind::OpaquePtr)?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        // assert_eq!(c_ast.structs.len(), 2);
        // assert_eq!(c_ast.functions.len(), 13);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_inherited() -> Result<(), Error> {
    run_test(|| {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Base {
                int a;
            public:
                float b;
                Base() = delete;
                Base(int a, float b);
                void base_do_thing();
            };

            class Class : public Base {
            public:
                float c;
                void derived_do_thing() const;
            };
        "#
            ),
            &cli_args_with(&["-std=c++11"])?,
            true,
            None,
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        println!("{ast:?}");
        /*
            assert_eq!(
                format!("{ast:?}"),
                indoc!(
                    r#"
            Namespace c:@S@Class Class None
            ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false rof=[ctor ] template_parameters=[] specializations=[] namespaces=[]
            Field b: float
            Method DefaultConstructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@Class# Class rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

        "#
                )
            );
            */

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        // assert_eq!(c_ast.structs.len(), 2);
        // assert_eq!(c_ast.functions.len(), 13);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_take_std_string_fun() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            r#"
    #include <string>

    namespace Test_1_0 {
    std::string take_string(const std::string& s);
    }
            "#,
            &cli_args()?,
            true,
            Some("Test_1_0"),
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 7);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn build_take_std_string() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
            &AllowList::default(),
            &OverrideList::default(),
            contents,
            true,
        )?;

        println!("{ast:?}");

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 11);

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        build_project(
            "build_take_std_string",
            "/tmp",
            &c_ast,
            &find_packages,
            &link_libraries,
            &[],
            Some(&cmake_prefix_path),
        )?;

        Ok(())
    })
}

#[test]
fn write_enum() -> Result<(), Error> {
    init_log();

    let mut ast = parse_string_and_extract_ast(
        indoc!(
            r#"
            namespace Test_1_0 {
            enum class Numbered {
                First = 1,
                Second,
                Third = 3,
            };

            enum class Unnumbered {
                First,
                Second,
                Third,
            };

            void take_enum(Numbered n, Unnumbered u);
            }
        "#
        ),
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
        &OverrideList::default(),
        true,
    )?;

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let ast = ast.monomorphize()?;
    let c_ast = translate_cpp_ast_to_c(&ast)?;

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_vector() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        indoc!(
            r#"
            #include <vector>

            namespace Test_1_0 {
            class Class {
                float c;
            public:
            };

            typedef std::vector<Class> ClassVector;
            }
            "#
        ),
        &cli_args()?,
        true,
        None,
        &AllowList::new(vec!["^Test_1_0".to_string()]),
        &OverrideList::default(),
        true,
    )?;

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let ast = ast.monomorphize()?;
    let c_ast = translate_cpp_ast_to_c(&ast)?;

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_unique_ptr() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        indoc!(
            r#"
            #include <memory>

            namespace Test_1_0 {
            class Class {
                float c;
            public:
            };

            typedef std::unique_ptr<Class> ClassPtr;
            }
            "#
        ),
        &cli_args()?,
        true,
        None,
        &AllowList::new(vec!["^Test_1_0".to_string()]),
        &OverrideList::default(),
        true,
    )?;

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let ast = ast.monomorphize()?;
    let c_ast = translate_cpp_ast_to_c(&ast)?;

    let (c_header, c_source) = gen_c("test", &c_ast)?;
    println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

    Ok(())
}

#[test]
fn write_std_function() -> Result<(), bbl_util::Error> {
    bbl_util::run_test(|| {
        let mut ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            #include <functional>

            namespace Test_1_0 {
            using PropertyPredicateFunc = std::function<bool(const char* name)>;

            void take_function(const PropertyPredicateFunc& predicate = {});
            }
            "#
            ),
            &cli_args()?,
            true,
            None,
            &AllowList::new(vec!["^Test_1_0".to_string()]),
            &OverrideList::default(),
            true,
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_nested_template() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
                #include <memory>

                namespace Test {
                    template <class T>
                    struct HandleTo {
                        typedef std::unique_ptr<T> Handle;
                    };

                    class Class;
                    typedef HandleTo<Class>::Handle ClassHandle;
                    class Class {
                    public:
                        ClassHandle create();
                    };

                }
            "#
            ),
            &cli_args()?,
            true,
            None,
            &AllowList::new(vec![r#"^Test::.*$"#.to_string()]),
            &OverrideList::default(),
            true,
        )?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_template_typedef_member() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
                namespace Test {
                    template <class T>
                    struct Handle {
                        typedef T Value;
                        typedef T* Pointer;

                        Pointer get1();
                        Value* get2();
                    };

                    class Class;
                    typedef Handle<Class> ClassHandle;
                    class Class {
                    public:
                        ClassHandle create();
                    };

                }
            "#
            ),
            &cli_args()?,
            true,
            None,
            &AllowList::new(vec![r#"^Test::.*$"#.to_string()]),
            &OverrideList::default(),
            true,
        )?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    })
}

#[test]
fn write_size_t() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
                #include <stddef.h>
                size_t take_size_t(size_t);
            "#
            ),
            &cli_args()?,
            true,
            None,
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        let ast = ast.monomorphize()?;
        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        let (c_header, c_source) = gen_c("test", &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        let mut ffi = String::new();
        write_rust_ffi(&mut ffi, &c_ast)?;

        println!("FFI:\n---------\n{ffi}");

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
