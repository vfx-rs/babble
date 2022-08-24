use crate::ast::AST;

pub struct Binding {
    ast: AST,
}

impl Binding {
    pub fn new(ast: AST) -> Self {
        Binding { ast }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{extract_ast, AST, dump},
        error::Error,
        parse_string_and_extract_ast, parse_string_to_tu,
        translate::translate_cpp_ast_to_c, parse_string_and_dump_ast,
    };

    fn init_log() {
        let _ = env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
            .format_timestamp(None)
            .is_test(true)
            .try_init();
    }

    use super::Binding;

    #[test]
    fn test_binding_rename() -> Result<(), Error> {
        init_log();
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

        let c_ast = translate_cpp_ast_to_c(&ast);

        c_ast.pretty_print(0);

        Ok(())
    }

    #[test]
    fn test_binding_pass_class() -> Result<(), Error> {
        init_log();
        let mut ast = parse_string_and_extract_ast(
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

        let method = ast.find_method(class, "take_a(a: Test::A&)")?;

        let c_ast = translate_cpp_ast_to_c(&ast);

        c_ast.pretty_print(0);

        Ok(())
    }

    #[test]
    fn test_binding_vec3() -> Result<(), Error> {
        init_log();
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
            Some("Imath_3_1")
        )?;

        ast.pretty_print(0);

        let namespace = ast.find_namespace("Imath_3_1")?;
        ast.rename_namespace(namespace, "Imath");

        // let class = ast.find_class("B")?;

        // let method = ast.find_method(class, "take_a(a: Test::A&)")?;

        let c_ast = translate_cpp_ast_to_c(&ast);

        c_ast.pretty_print(0);

        Ok(())
    }

    #[test]
    fn test_binding_error_not_found() -> Result<(), Error> {
        init_log();
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
        let method = ast.find_method(class, "foo()");
        assert!(matches!(method, Err(Error::MethodNotFound)));

        ast.pretty_print(0);

        let bind = Binding::new(ast);

        Ok(())
    }

    #[test]
    fn test_binding_error_multiple() -> Result<(), Error> {
        init_log();

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
        let method = ast.find_method(class, "method");
        assert!(matches!(method, Err(Error::MultipleMatches)));

        ast.pretty_print(0);

        let bind = Binding::new(ast);

        Ok(())
    }
}
