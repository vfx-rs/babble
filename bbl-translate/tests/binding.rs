mod common;

use bbl_extract::{
    error::Error, parse_string_and_extract_ast, qualtype::QualType, template_argument::TemplateType,
};

use bbl_translate::translate_cpp_ast_to_c;

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    let class = ast.find_class("Class")?;

    let method = ast.find_method(class, "method(a: Float) -> Int")?;
    ast.rename_method(class, method, "method_float");

    let method = ast.find_method(class, "method(a: Int) -> Int")?;
    ast.rename_method(class, method, "method_int");

    let method = ast.find_method(class, "method(a: UInt) -> Int")?;
    ast.ignore_method(class, method);

    ast.pretty_print(0);

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    ast.pretty_print(0);

    let class = ast.find_class("B")?;

    let _method = ast.find_method(class, "take_a(a: Test::A&)")?;

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        Some("Imath_3_1"),
    )?;

    ast.pretty_print(0);

    let namespace = ast.find_namespace("Imath_3_1")?;
    ast.rename_namespace(namespace, "Imath");

    let c_ast = translate_cpp_ast_to_c(&ast)?;

    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "foo()");
    assert!(matches!(method, Err(Error::MethodNotFound)));

    ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method");
    assert!(matches!(method, Err(Error::MultipleMatches)));

    ast.pretty_print(0);

    Ok(())
}

#[test]
fn test_binding_template() -> Result<(), Error> {
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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    let class = ast.find_class("Class")?;
    ast.specialize_class(
        class,
        "ClassFloat",
        vec![Some(TemplateType::Type(QualType::float()))],
    )?;

    ast.pretty_print(0);

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    ast.pretty_print(0);

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    assert_eq!(c_ast.structs.len(), 0);
    assert_eq!(c_ast.functions.len(), 1);
    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    ast.pretty_print(0);
    let function = ast.find_function("function_template")?;
    ast.specialize_function(
        function,
        "function_float",
        vec![Some(TemplateType::Type(QualType::float()))],
    )?;

    ast.pretty_print(0);

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0);

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
        &[
            "-resource-dir",
            "/home/anders/packages/llvm/14.0.0/lib/clang/14.0.0",
            "-std=c++14",
            "-I/usr/include",
            "-I/usr/local/include",
        ],
        true,
        None,
    )?;

    ast.pretty_print(0);
    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method_template")?;
    ast.specialize_method(
        class,
        method,
        "method_float",
        vec![Some(TemplateType::Type(QualType::float()))],
    )?;

    ast.pretty_print(0);

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    c_ast.pretty_print(0);

    assert_eq!(c_ast.structs.len(), 1);
    assert_eq!(c_ast.functions.len(), 1);

    Ok(())
}
