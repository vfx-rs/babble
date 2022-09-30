mod common;

use std::path::Path;

use bbl_clang::{cli_args, cli_args_with, virtual_file::configure_temp_cmake_project};
use bbl_extract::templates::TemplateArgument;
use bbl_extract::{AllowList, parse_file_and_extract_ast, parse_string_and_extract_ast, qualtype::QualType};

use bbl_translate::error::Error;

use bbl_translate::translate_cpp_ast_to_c;
use log::error;
use indoc::indoc;

#[test]
fn test_binding_rename() -> Result<(), Error> {
    common::init_log();
    let mut ast = parse_string_and_extract_ast(
        r#"

class Class {
    int a;
public:
    float b;
    int method(float a);
    int method(int a);
    int method(unsigned int a);
};
    
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    let class = ast.find_class("Class")?;

    let method = ast.find_method(class, "method(float) -> int")?;
    ast.rename_method(class, method, "method_float");

    let method = ast.find_method(class, "method(int) -> int")?;
    ast.rename_method(class, method, "method_int");

    let method = ast.find_method(class, "method(unsigned int) -> int")?;
    ast.ignore_method(class, method);

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0)?;

    Ok(())
}

#[test]
fn test_binding_pass_class() -> Result<(), Error> {
    common::init_log();
    let ast = parse_string_and_extract_ast(
        r#"

namespace Test {
class A {
};

class B {
public:
    void take_a(const A& a) const;
    void take_a(A& a);
};

}
    
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    println!("{ast:?}");

    let class = ast.find_class("B")?;

    let _method = ast.find_method(class, "take_a(const A &)")?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0)?;

    Ok(())
}

#[test]
fn test_binding_vec3() -> Result<(), Error> {
    common::init_log();
    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Imath_3_1 {

class Vec3;

class Vec3
{    
public:
    float x, y, z;
    constexpr float& operator[] (int i) noexcept;
    constexpr const float& operator[] (int i) const noexcept;
    Vec3() noexcept;
    /// Initialize to a scalar `(a,a,a)`
    constexpr explicit Vec3 (float a) noexcept;
    /// Initialize to given elements `(a,b,c)`
    constexpr Vec3 (float a, float b, float c) noexcept;
    /// Copy constructor
    constexpr Vec3 (const Vec3& v) noexcept;
    /// Assignment
    constexpr const Vec3& operator= (const Vec3& v) noexcept;
    /// Destructor
    ~Vec3() noexcept = default;
    /// Return a raw pointer to the array of values
    float* getValue() noexcept;
    /// Return a raw pointer to the array of values
    const float* getValue() const noexcept;
    /// Equality
    constexpr bool operator== (const Vec3& v) const noexcept;
    /// Inequality
    constexpr bool operator!= (const Vec3& v) const noexcept;
    /// Compare two matrices and test if they are "approximately equal":
    /// @return True if the coefficients of this and `m` are the same
    /// with an absolute error of no more than e
    constexpr bool equalWithAbsError (const Vec3& v, float e) const noexcept;
    constexpr bool equalWithRelError (const Vec3& v, float e) const noexcept;
    constexpr float dot (const Vec3& v) const noexcept;
    constexpr float operator^ (const Vec3& v) const noexcept;
    constexpr Vec3 cross (const Vec3& v) const noexcept;
    constexpr const Vec3& operator%= (const Vec3& v) noexcept;
    constexpr Vec3 operator% (const Vec3& v) const noexcept;
    constexpr const Vec3& operator+= (const Vec3& v) noexcept;
    constexpr Vec3 operator+ (const Vec3& v) const noexcept;
    constexpr const Vec3& operator-= (const Vec3& v) noexcept;
    constexpr Vec3 operator- (const Vec3& v) const noexcept;
    constexpr Vec3 operator-() const noexcept;
    constexpr const Vec3& negate() noexcept;
    constexpr const Vec3& operator*= (const Vec3& v) noexcept;
    constexpr const Vec3& operator*= (float a) noexcept;
    constexpr Vec3 operator* (const Vec3& v) const noexcept;
    constexpr Vec3 operator* (float a) const noexcept;
    constexpr const Vec3& operator/= (const Vec3& v) noexcept;
    constexpr const Vec3& operator/= (float a) noexcept;
    constexpr Vec3 operator/ (const Vec3& v) const noexcept;
    constexpr Vec3 operator/ (float a) const noexcept;
    float length() const noexcept;
    constexpr float length2() const noexcept;
    const Vec3& normalize() noexcept;
    /// Normalize in place. If length()==0, throw an exception.
    const Vec3& normalizeExc();
    /// Return a normalized vector. Does not modify *this. Throw an
    /// exception if length()==0.
    Vec3 normalizedExc() const;
    const Vec3& normalizeNonNull() noexcept;
    Vec3 normalized() const noexcept; // does not modify *this
    constexpr static float baseTypeLowest() noexcept;
    constexpr static unsigned int dimensions() noexcept { return 3; }
    /// The base type: In templates that accept a parameter `V`, you
    /// can refer to `T` as `V::BaseType`
    typedef float BaseType;
};

}

        "#,
        &cli_args()?,
        true,
        Some("Imath_3_1"),
        &AllowList::default(),
    )?;

    println!("{ast:?}");

    let namespace = ast.find_namespace("Imath_3_1")?;
    ast.rename_namespace(namespace, "Imath");

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0)?;

    // This will have to do for now
    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 42);

    Ok(())
}

#[test]
fn test_binding_error_not_found() -> Result<(), Error> {
    common::init_log();
    let ast = parse_string_and_extract_ast(
        r#"

class Class {
    int a;
public:
    float b;
    int method(float a);
    int method(int a);
    int method(unsigned int a);
};
    
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "foo()");
    assert!(matches!(
        method,
        Err(bbl_extract::error::Error::MethodNotFound)
    ));

    println!("{ast:?}");

    Ok(())
}

#[test]
fn test_binding_error_multiple() -> Result<(), Error> {
    common::init_log();

    let ast = parse_string_and_extract_ast(
        r#"

class Class {
    int a;
public:
    float b;
    int method(float a);
    int method(int a);
    int method(unsigned int a);
};
    
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method");
    assert!(matches!(
        method,
        Err(bbl_extract::error::Error::MultipleMatches)
    ));

    println!("{ast:?}");

    Ok(())
}

#[test]
fn test_binding_template() -> anyhow::Result<()> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        r#"

namespace Test {

template <typename T>
class Class {
public:
    T a;
    T return_t();
    void take_t(const T*) const;
};

}
    
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    println!("{ast:?}");

    let class = ast.find_class("Class")?;
    ast.specialize_class(
        class,
        "ClassFloat",
        vec![TemplateArgument::Type(QualType::float())],
    )?;

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    Ok(())
}

#[test]
fn bind_function() -> Result<(), Error> {
    common::init_log();

    let ast = parse_string_and_extract_ast(
        r#"
namespace Test {
int basic_function(int&& a, float*);
}
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    assert_eq!(c_ast.structs.len(), 0);
    assert_eq!(c_ast.functions.len(), 1);
    c_ast.pretty_print(0)?;

    Ok(())
}

#[test]
fn bind_function_template() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test {
template <typename T>
T function_template(T&& a, float*);
}
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    println!("{ast:?}");
    let function = ast.find_function("function_template")?;
    ast.specialize_function(
        function,
        "function_float",
        vec![TemplateArgument::Type(QualType::float())],
    )?;

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 0);
    assert_eq!(c_ast.functions.len(), 1);

    Ok(())
}

#[test]
fn bind_method_template() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test {
class Class {
public:
    template <typename T>
    T method_template(T&& a, float*);
};
}
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    println!("{ast:?}");
    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method_template")?;
    ast.specialize_method(
        class,
        method,
        "method_float",
        vec![TemplateArgument::Type(QualType::float())],
    )?;

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0)?;

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 1);

    Ok(())
}

#[test]
fn bind_method_template_ret() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test {
class Class {
public:
    template <typename T>
    const T* method_template(const T&);
};
}
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method_template")?;
    ast.specialize_method(
        class,
        method,
        "method_float",
        vec![TemplateArgument::Type(QualType::float())],
    )?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    println!("{c_ast:?}");


    assert_eq!(
        format!("{c_ast:?}"),
        indoc!(
            r#"
            CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
            CFunction Test_Class_method_float Test_Class_method_float([this_: c:@N@Test@S@Class*, result: Float const**, arg: Float const*])  -> Int
            "#
        )
    );

    Ok(())
}

#[test]
fn bind_templated_ctor() -> Result<(), Error> {
    common::init_log();

    let mut ast = parse_string_and_extract_ast(
        r#"
namespace Test {
class Class {
public:
    template <typename T>
    Class(const T&);
};
}
        "#,
        &cli_args()?,
        true,
        None,
        &AllowList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "Class(const T &)")?;
    ast.specialize_method(
        class,
        method,
        "ctor_float",
        vec![TemplateArgument::Type(QualType::float())],
    )?;

    println!("{ast:?}");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    println!("{c_ast:?}");
    assert_eq!(
        format!("{c_ast:?}"),
        indoc!(r#"
            CStruct c:@N@Test@S@Class Test_Class Test_Class OpaquePtr fields=[]
            CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class**])  -> Int
            CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class**, rhs: c:@N@Test@S@Class const* const])  -> Int
            CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class**, rhs: c:@N@Test@S@Class const*])  -> Int
            CFunction Test_Class_dtor Test_Class_dtor([this_: c:@N@Test@S@Class*])  -> Int
            CFunction Test_Class_ctor_float Test_Class_ctor_float([result: c:@N@Test@S@Class**, arg: Float const*])  -> Int
        "#
    ));

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 5);

    Ok(())
}

#[test]
fn use_cmake_for_args() -> Result<(), crate::Error> {
    common::init_log();

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

    let (filename, args) = configure_temp_cmake_project::<&Path>(
        contents,
        &["Imath 3.1 REQUIRED"],
        &["Imath::Imath"],
        None,
    )?;

    let ast =
        parse_file_and_extract_ast(&filename, &cli_args_with(&args)?, true, Some("Test"), &AllowList::default())?;
    println!("{ast:?}");

    Ok(())
}

#[test]
fn take_std_string() -> Result<(), Error> {
    common::init_log();

    let clo = || -> Result<(), Error> {
        let ast = parse_string_and_extract_ast(
            r#"
#include <string>

namespace Test {
class Class {
public:
    std::string take_string(const std::string& s);
};
}
        "#,
            &cli_args()?,
            true,
            Some("Test"),
            &AllowList::default(),
        )?;

        println!("{ast:?}");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 7);

        Ok(())
    };

    clo().map_err(|err| {
        error!("{err}");
        for e in source_iter(&err) {
            error!("  because: {e}")
        }

        err
    })
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

fn source_iter(
    error: &impl std::error::Error,
) -> impl Iterator<Item = &(dyn std::error::Error + 'static)> {
    SourceIter {
        current: error.source(),
    }
}
