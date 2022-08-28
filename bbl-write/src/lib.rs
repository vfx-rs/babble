use std::fmt::Display;

use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{ast::AST, class::ClassBindKind, function::Method, qualtype};
use bbl_translate::{CArgument, CFunction, CFunctionSource, CQualType, CStruct, CTypeRef, CAST};

#[derive(Debug)]
pub enum Error {
    ClangError(bbl_clang::error::Error),
    ExtractError(bbl_extract::error::Error),
    FailedToGenerateFunction {
        name: String,
        source: FunctionGenerationError,
    },
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<bbl_clang::error::Error> for Error {
    fn from(e: bbl_clang::error::Error) -> Self {
        Error::ClangError(e)
    }
}

impl From<bbl_extract::error::Error> for Error {
    fn from(e: bbl_extract::error::Error) -> Self {
        Error::ExtractError(e)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::FailedToGenerateFunction { source, .. } => Some(source),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    TypeRefNotFound(USR),
    UnknownType(TypeKind),
    FailedToGetQualifiedName {
        name: String,
        source: bbl_extract::error::Error,
    },
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for TypeError {}

#[derive(Debug)]
pub enum ArgumentError {
    TypeError(TypeError),
}

impl From<TypeError> for ArgumentError {
    fn from(e: TypeError) -> Self {
        ArgumentError::TypeError(e)
    }
}

impl Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ArgumentError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ArgumentError::TypeError(e) => Some(e),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum FunctionGenerationError {
    FailedToGenerateSignature(TypeError),
    FailedToGenerateCall(TypeError),
    FailedToGenerateArg { name: String, source: ArgumentError },
}

impl Display for FunctionGenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for FunctionGenerationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            FunctionGenerationError::FailedToGenerateArg { source, .. } => Some(source),
            _ => None,
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn generate_c(ast: &AST, c_ast: &CAST) -> Result<(String, String)> {
    let mut header = String::new();
    let mut source = String::new();

    for st in c_ast.structs.iter() {
        let decl = generate_struct_decl(st, ast)?;
        header = format!("{header}{decl}\n")
    }

    for fun in c_ast.functions.iter() {
        let decl = generation_fun_decl(fun, ast, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source,
            }
        })?;

        let defn = generate_fun_defn(fun, ast, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source,
            }
        })?;

        header = format!("{header}{decl}\n");
        source = format!("{source}{defn}\n");
    }

    Ok((header, source))
}

fn generation_fun_signature(fun: &CFunction, ast: &AST, c_ast: &CAST) -> Result<String, TypeError> {
    let mut result = format!("{} {}", gen_c_type(&fun.result, c_ast), fun.name_private);

    let arg_str = fun
        .arguments
        .iter()
        .map(|a| format!("{} {}", gen_c_type(&a.qual_type, c_ast), a.name))
        .collect::<Vec<String>>();

    Ok(format!("{result}({})", arg_str.join(", ")))
}

fn generation_fun_decl(
    fun: &CFunction,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{};\n",
        generation_fun_signature(fun, ast, c_ast)
            .map_err(|e| FunctionGenerationError::FailedToGenerateSignature(e))?
    );

    if fun.name_private != fun.name_public {
        result = format!(
            "{result}#define {} {};\n",
            fun.name_public, fun.name_private
        );
    }

    Ok(result)
}

fn get_cast(qual_type: &CQualType, ast: &AST, c_ast: &CAST) -> Result<Option<String>, TypeError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) => Ok(None),
        CTypeRef::Pointer(pointee_qt) => {
            if let Some(s) = get_cast(&pointee_qt, ast, c_ast)? {
                Ok(Some(format!("{s}*")))
            } else {
                Ok(None)
            }
        }
        CTypeRef::Ref(usr) => {
            let class = ast
                .get_class(*usr)
                .ok_or(TypeError::TypeRefNotFound(*usr))?;
            Ok(Some(format!(
                "{}",
                class.get_qualified_name(ast).map_err(|source| {
                    TypeError::FailedToGetQualifiedName {
                        name: class.name().to_string(),
                        source,
                    }
                })?
            )))
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk)),
    }
}

fn generate_arg_pass(
    arg_name: &str,
    qual_type: &CQualType,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, ArgumentError> {
    // NS::Class    -> *(NS::Class*)&arg
    // NS::Class*   -> (NS::Class*)arg
    // NS::Class&   -> *(NS::Class*)arg
    // NS::Class*&  -> *(NS::Class**)arg
    match qual_type.type_ref() {
        CTypeRef::Builtin(_tk) => Ok(arg_name.to_string()),
        CTypeRef::Pointer(pointee_qt) => {
            let deref = if qual_type.needs_deref() { "*" } else { "" };
            if let Some(cast) = get_cast(qual_type, ast, c_ast)? {
                Ok(format!("{deref}({cast}){arg_name}"))
            } else {
                Ok(format!("{deref}{arg_name}"))
            }
        }
        CTypeRef::Ref(usr) => {
            if let Some(cast) = get_cast(qual_type, ast, c_ast)? {
                Ok(format!("*({cast}*)&{arg_name}"))
            } else {
                Ok(format!("{arg_name}"))
            }
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk))?,
    }
}

fn get_cpp_call(fun: &CFunction, ast: &AST) -> Result<String, TypeError> {
    match fun.source {
        CFunctionSource::Function(cpp_fun_id) => {
            let cpp_fun = ast.functions().index(cpp_fun_id.into());
            Ok(cpp_fun.name().to_string())
        }
        CFunctionSource::Method((class_id, method_id)) => {
            let class = ast.classes().index(class_id.into());
            let method: &Method = &class.methods()[method_id.0];

            let qname = method.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: class.name().to_string(),
                    source: e,
                }
            })?;

            if method.is_static() {
                Ok(qname)
            } else {
                Ok(format!("(({}*)self)->{}", qname, method.name()))
            }
        }
        CFunctionSource::SpecializedMethod((class_id, method_id)) => {
            let class = ast.classes().index(class_id.into());
            let spec_method = &class.specialized_methods()[method_id.0];
            let temp_method = &class.methods()[spec_method.specialized_decl().0];

            let qname = temp_method.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: class.name().to_string(),
                    source: e,
                }
            })?;

            if temp_method.is_static() {
                Ok(qname)
            } else {
                Ok(format!("(({}*)self)->{}", qname, temp_method.name()))
            }
        }
    }
}

fn generate_fun_defn(
    fun: &CFunction,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{} {{\n",
        generation_fun_signature(fun, ast, c_ast)
            .map_err(|e| FunctionGenerationError::FailedToGenerateSignature(e))?
    );
    let indent = "    ";

    let cast = get_cast(&fun.result, ast, c_ast)
        .map_err(|e| FunctionGenerationError::FailedToGenerateArg {
            name: "[result]".to_string(),
            source: ArgumentError::TypeError(e),
        })?
        .map(|s| format!("({s})"))
        .unwrap_or("".to_string());

    result = format!(
        "{result}{indent}return {cast}{}",
        get_cpp_call(fun, ast).map_err(|e| FunctionGenerationError::FailedToGenerateCall(e))?
    );


    let arg_str = if fun.arguments.is_empty() || fun.arguments.len() == 1 &&  fun.arguments[0].is_self {
        "();".to_string()
    } else {
        let mut args = Vec::new();
        for arg in &fun.arguments {
            if arg.is_self {
                continue;
            }

            match generate_arg_pass(&arg.name, &arg.qual_type, ast, c_ast) {
                Ok(s) => args.push(format!("{indent}{indent}{s}")),
                Err(source) => {
                    return Err(FunctionGenerationError::FailedToGenerateArg {
                        name: arg.name.to_string(),
                        source,
                    })
                }
            }
        }

        format!("(\n{}\n{indent});", args.join(",\n"))
    };

    result = format!("{result}{arg_str}\n");

    Ok(format!("{result}}}\n"))
}

fn gen_c_type(qt: &CQualType, c_ast: &CAST) -> String {
    match qt.type_ref() {
        CTypeRef::Builtin(tk) => {
            match tk {
                TypeKind::Bool => "bool".to_string(),
                TypeKind::Char_S => "char".to_string(),
                TypeKind::Char_U => "unsigned char".to_string(),
                TypeKind::Double => "double".to_string(),
                TypeKind::Float => "float".to_string(),
                TypeKind::Int => "int".to_string(),
                TypeKind::Long => "long".to_string(),
                TypeKind::LongDouble => "long double".to_string(),
                TypeKind::LongLong => "long long".to_string(),
                TypeKind::Short => "short".to_string(),
                TypeKind::UChar => "unsigned char".to_string(),
                TypeKind::UInt => "unsigned int".to_string(),
                TypeKind::ULong => "unsigned long".to_string(),
                TypeKind::ULongLong => "unsigned long long".to_string(),
                TypeKind::UShort => "unsigned short".to_string(),
                TypeKind::Void => "void".to_string(),
                _ => qt.format(c_ast)
            }
        }
        _ => qt.format(c_ast)
    }

}

fn generate_struct_decl(st: &CStruct, ast: &AST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_decl(st, ast),
        _ => todo!(),
    }
}

fn generate_opaqueptr_decl(st: &CStruct, ast: &AST) -> Result<String> {
    Ok(format!(
        "struct {0};\ntypedef {0} {1};\n",
        st.name_private, st.name_public
    ))
}


#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use bbl_extract::parse_string_and_extract_ast;
    use bbl_translate::translate_cpp_ast_to_c;

    use crate::{generate_c, Error};

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
            &cli_args(&[])?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 2);

        let (c_header, c_source) = generate_c(&ast, &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    }
}
