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
        ast::{extract_ast, AST},
        error::Error,
        parse_string_and_extract_ast, parse_string_to_tu,
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
        )?;

        let class = ast.find_class("Class")?;

        let method = ast.find_method(class, "method(a: Float) -> Int")?;
        ast.rename_method(class, method, "method_float");

        let method = ast.find_method(class, "method(a: Int) -> Int")?;
        ast.rename_method(class, method, "method_int");

        let method = ast.find_method(class, "method(a: UInt) -> Int")?;
        ast.ignore_method(class, method);


        ast.pretty_print(0);

        let bind = Binding::new(ast);

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
        )?;

        let class = ast.find_class("Class")?;
        let method = ast.find_method(class, "method");
        assert!(matches!(method, Err(Error::MultipleMatches)));


        ast.pretty_print(0);

        let bind = Binding::new(ast);

        Ok(())
    }
}
