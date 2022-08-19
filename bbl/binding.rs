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
    use crate::{parse_string_to_tu, error::Error, ast::{extract_ast, AST}, parse_string_and_extract_ast};

    use super::Binding;


    #[test]
    fn test_binding_simple() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(r#"

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
        true)?;

        let rec = ast.class("Class")?;


        ast.pretty_print(0);


        let bind = Binding::new(ast);



        Ok(())
    }
}