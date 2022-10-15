mod common;

use std::path::Path;

use bbl_clang::{cli_args, cli_args_with, virtual_file::configure_temp_cmake_project};
use bbl_extract::class::OverrideList;
use bbl_extract::templates::TemplateArgument;
use bbl_extract::{
    parse_file_and_extract_ast, parse_string_and_extract_ast, qualtype::QualType, AllowList,
};

use bbl_translate::error::Error;

use bbl_translate::translate_cpp_ast_to_c;
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
        &OverrideList::default(),
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
        &OverrideList::default(),
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
        &OverrideList::default(),
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
        &OverrideList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "foo()");
    assert!(matches!(
        method,
        Err(bbl_extract::error::Error::MethodNotFound { .. })
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
        &OverrideList::default(),
    )?;

    let class = ast.find_class("Class")?;
    let method = ast.find_method(class, "method");
    assert!(matches!(
        method,
        Err(bbl_extract::error::Error::MultipleMatches { .. })
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
        &OverrideList::default(),
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
        &OverrideList::default(),
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
        &OverrideList::default(),
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
fn translate_method_template() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        &OverrideList::default(),
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

        bbl_util::compare(
            &format!("{c_ast:?}"),
            indoc!(
                r#"
            CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
            CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class*])  -> Int
            CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const* const])  -> Int
            CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const*])  -> Int
            CFunction Test_Class_method_float Test_Class_method_float([this_: c:@N@Test@S@Class*, result: Float*, a: Float*, arg: Float*])  -> Int
        "#
            ),
        )?;

        Ok(())
    })
}

#[test]
fn translate_method_template_ret() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        &OverrideList::default(),
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

        bbl_util::compare(
            &format!("{c_ast:?}"),
            indoc!(
                r#"
        CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
        CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class*])  -> Int
        CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const* const])  -> Int
        CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const*])  -> Int
        CFunction Test_Class_method_float Test_Class_method_float([this_: c:@N@Test@S@Class*, result: Float const**, arg: Float const*])  -> Int
    "#
            ),
        )?;

        Ok(())
    })
}

#[test]
fn translate_templated_ctor() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        &OverrideList::default(),
        )?;

        let class = ast.find_class("Class")?;
        let method = ast.find_method(class, "Class(const T &)")?;
        ast.specialize_method(
            class,
            method,
            "ctor_float",
            vec![TemplateArgument::Type(QualType::float())],
        )?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");
        bbl_util::compare(
            &format!("{c_ast:?}"),
            indoc!(
                r#"
        CStruct c:@N@Test@S@Class Test_Class Test_Class OpaquePtr fields=[]
        CFunction Test_Class_dtor Test_Class_dtor([this_: c:@N@Test@S@Class*])  -> Int
        CFunction Test_Class_ctor_float Test_Class_ctor_float([result: c:@N@Test@S@Class**, arg: Float const*])  -> Int
    "#
            ),
        )?;

        Ok(())
    })
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

    let ast = parse_file_and_extract_ast(
        &filename,
        &cli_args_with(&args)?,
        true,
        Some("Test"),
        &AllowList::default(),
        &OverrideList::default(),
    )?;
    println!("{ast:?}");

    Ok(())
}

#[test]
fn translate_take_std_string() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        &OverrideList::default(),
        )?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;

        println!("{c_ast:?}");
        bbl_util::compare(&format!("{c_ast:?}"), indoc!(r#"
            CStruct c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C std_string std_string OpaquePtr fields=[]
            CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
            CTypedef std_string std_string c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C
            CFunction std_string_ctor std_string_ctor([result: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C**])  -> Int
            CFunction std_string_from_char_ptr std_string_from_char_ptr([result: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C**, char_ptr: Char_S const*])  -> Int
            CFunction std_string_dtor std_string_dtor([this_: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C*])  -> Int
            CFunction std_string_copy_ctor std_string_copy_ctor([result: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C**, other: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C const* const])  -> Int
            CFunction std_string_move_ctor std_string_move_ctor([result: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C**, other: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C*])  -> Int
            CFunction std_string_c_str std_string_c_str([this_: c:@N@std@N@__cxx11@S@basic_string>#C#$@N@std@S@char_traits>#C#$@N@std@S@allocator>#C const*, result: Char_S const**])  -> Int
            CFunction Test_Class_take_string Test_Class_take_string([this_: c:@N@Test@S@Class*, result: c:@N@std@T@string*, s: c:@N@std@T@string const*])  -> Int
            CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class*])  -> Int
            CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const* const])  -> Int
            CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const*])  -> Int
            Include { name: "string", bracket: "<" }
        "#))?;

        Ok(())
    })
}

#[test]
fn translate_enum() -> Result<(), Error> {
    common::init_log();

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
    )?;

    let ns = ast.find_namespace("Test_1_0")?;
    ast.rename_namespace(ns, "Test");

    let c_ast = translate_cpp_ast_to_c(&ast)?;
    println!("{c_ast:?}");
    assert_eq!(
        format!("{c_ast:?}"),
        indoc!(
            r#"
            CFunction Test_1_0_take_enum Test_take_enum([n: c:@N@Test_1_0@E@Numbered, u: c:@N@Test_1_0@E@Unnumbered])  -> Int
            CEnum Test_1_0_Numbered Test_Numbered [First=1 Second=2 Third=3 ]
            CEnum Test_1_0_Unnumbered Test_Unnumbered [First=0 Second=1 Third=2 ]
            "#
        )
    );

    Ok(())
}

#[test]
fn translate_vector() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        bbl_util::compare(
            &format!("{c_ast:?}"),
            indoc!(
                r#"
            CStruct c:@N@Test_1_0@S@Class Test_1_0_Class Test_Class OpaquePtr fields=[]
            CStruct c:@N@std@ST>2#T#T@vector std_vector_Test_1_0_Class_ std_vector_Test_1_0_Class_ OpaquePtr fields=[]
            CTypedef Test_1_0_ClassVector Test_ClassVector c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_
            CFunction Test_1_0_Class_ctor Test_Class_ctor([result: c:@N@Test_1_0@S@Class**])  -> Int
            CFunction Test_1_0_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test_1_0@S@Class**, rhs: c:@N@Test_1_0@S@Class const* const])  -> Int
            CFunction Test_1_0_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test_1_0@S@Class**, rhs: c:@N@Test_1_0@S@Class const*])  -> Int
            CFunction Test_1_0_Class_dtor Test_Class_dtor([this_: c:@N@Test_1_0@S@Class*])  -> Int
            CFunction std_vector_Test_1_0_Class__ctor std_vector_Test_1_0_Class__ctor([result: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_**])  -> Int
            CFunction std_vector_Test_1_0_Class__from_begin_and_end std_vector_Test_1_0_Class__from_begin_and_end([result: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_**, begin: c:@N@Test_1_0@S@Class const*, end: c:@N@Test_1_0@S@Class const*])  -> Int
            CFunction std_vector_Test_1_0_Class__data std_vector_Test_1_0_Class__data([this_: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ const*, result: c:@N@Test_1_0@S@Class const**])  -> Int
            CFunction std_vector_Test_1_0_Class__data_mut std_vector_Test_1_0_Class__data_mut([this_: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_*, result: c:@N@Test_1_0@S@Class**])  -> Int
            CFunction std_vector_Test_1_0_Class__size std_vector_Test_1_0_Class__size([this_: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ const*, result: ULongLong*])  -> Int
            CFunction std_vector_Test_1_0_Class__copy_ctor std_vector_Test_1_0_Class__copy_ctor([result: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_**, rhs: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ const* const])  -> Int
            CFunction std_vector_Test_1_0_Class__move_ctor std_vector_Test_1_0_Class__move_ctor([result: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_**, rhs: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ const*])  -> Int
            CFunction std_vector_Test_1_0_Class__dtor std_vector_Test_1_0_Class__dtor([this_: c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_*])  -> Int
            Include { name: "vector", bracket: "<" }
            "#
            ),
        )?;

        Ok(())
    })
}

#[test]
fn translate_unique_ptr() -> bbl_util::Result<()> {
    bbl_util::run_test(|| {
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
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;

        println!("{c_ast:?}");
        bbl_util::compare(&format!("{c_ast:?}"), indoc!(r#"
            CStruct c:@N@Test_1_0@S@Class Test_1_0_Class Test_Class OpaquePtr fields=[]
            CStruct c:@N@std@ST>2#T#T@unique_ptr std_unique_ptr_Test_1_0_Class_ std_unique_ptr_Test_1_0_Class_ OpaquePtr fields=[]
            CTypedef Test_1_0_ClassPtr Test_ClassPtr c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_
            CFunction Test_1_0_Class_ctor Test_Class_ctor([result: c:@N@Test_1_0@S@Class**])  -> Int
            CFunction Test_1_0_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test_1_0@S@Class**, rhs: c:@N@Test_1_0@S@Class const* const])  -> Int
            CFunction Test_1_0_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test_1_0@S@Class**, rhs: c:@N@Test_1_0@S@Class const*])  -> Int
            CFunction Test_1_0_Class_dtor Test_Class_dtor([this_: c:@N@Test_1_0@S@Class*])  -> Int
            CFunction std_unique_ptr_Test_1_0_Class__ctor std_unique_ptr_Test_1_0_Class__ctor([result: c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_**])  -> Int
            CFunction std_unique_ptr_Test_1_0_Class__get std_unique_ptr_Test_1_0_Class__get([this_: c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_ const*, result: c:@N@Test_1_0@S@Class const**])  -> Int
            CFunction std_unique_ptr_Test_1_0_Class__get_mut std_unique_ptr_Test_1_0_Class__get_mut([this_: c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_*, result: c:@N@Test_1_0@S@Class**])  -> Int
            CFunction std_unique_ptr_Test_1_0_Class__dtor std_unique_ptr_Test_1_0_Class__dtor([this_: c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_*])  -> Int
            Include { name: "memory", bracket: "<" }
        "#))?;

        Ok(())
    })
}

#[test]
fn translate_std_function() -> Result<(), bbl_util::Error> {
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
        )?;

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;

        println!("{c_ast:?}");
        bbl_util::compare(
            &format!("{c_ast:?}"),
            indoc!(
                r#"
                CTypedef Test_1_0_PropertyPredicateFunc Test_PropertyPredicateFunc Bool(*)(Char_S const*, )*
                CFunction Test_1_0_take_function Test_take_function([predicate: c:@N@Test_1_0@PropertyPredicateFunc const])  -> Int
                Include { name: "functional", bracket: "<" }
            "#
            ),
        )
    })
}

#[test]
fn translate_nested_template() -> bbl_util::Result<()> {
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
        )?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("{c_ast:?}");

        bbl_util::compare(&format!("{c_ast:?}"), indoc!(r#"
            CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
            CStruct c:@N@std@ST>2#T#T@unique_ptr std_unique_ptr_Test_Class_ std_unique_ptr_Test_Class_ OpaquePtr fields=[]
            CTypedef Test_HandleTo_Test_Class_Handle Test_HandleTo_Test_Class_Handle c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_
            CTypedef Test_ClassHandle Test_ClassHandle c:96a54560111dec54.cpp@N@Test@S@HandleTo>#$@N@Test@S@Class@T@Handle
            CFunction Test_Class_create Test_Class_create([this_: c:@N@Test@S@Class*, result: c:96a54560111dec54.cpp@N@Test@T@ClassHandle*])  -> Int
            CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class*])  -> Int
            CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const* const])  -> Int
            CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const*])  -> Int
            CFunction std_unique_ptr_Test_Class__ctor std_unique_ptr_Test_Class__ctor([result: c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_**])  -> Int
            CFunction std_unique_ptr_Test_Class__get std_unique_ptr_Test_Class__get([this_: c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_ const*, result: c:@N@Test@S@Class const**])  -> Int
            CFunction std_unique_ptr_Test_Class__get_mut std_unique_ptr_Test_Class__get_mut([this_: c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_*, result: c:@N@Test@S@Class**])  -> Int
            CFunction std_unique_ptr_Test_Class__dtor std_unique_ptr_Test_Class__dtor([this_: c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_*])  -> Int
            Include { name: "memory", bracket: "<" }
        "#))
    })
}