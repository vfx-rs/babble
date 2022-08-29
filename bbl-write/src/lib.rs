use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind, function::Method};
use bbl_translate::{CFunction, CFunctionSource, CQualType, CStruct, CTypeRef, CAST};

pub mod error;
use error::{ArgumentError, Error, FunctionGenerationError, TypeError};

type Result<T, E = Error> = std::result::Result<T, E>;

/// Generate the c library code (itself c++ but with a c header)
/// 
/// # Returns
/// A tuple containing two [`String`]s, the first of which is the header source, the second is the implementation source
pub fn gen_c(ast: &AST, c_ast: &CAST) -> Result<(String, String)> {
    let mut header = String::new();
    let mut source = String::new();

    for st in c_ast.structs.iter() {
        let decl = generate_struct_declaration(st, ast, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    for fun in c_ast.functions.iter() {
        let decl =
            gen_function_declaration(fun, c_ast).map_err(|source| Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source,
            })?;

        let defn = gen_function_definition(fun, ast, c_ast).map_err(|source| {
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

/// Generate the signature for this function
fn gen_function_signature(
    fun: &CFunction,
    c_ast: &CAST,
    use_public_names: bool,
) -> Result<String, TypeError> {
    let result = format!(
        "{} {}",
        gen_c_type(&fun.result, c_ast, use_public_names),
        fun.name_private
    );

    let arg_str = fun
        .arguments
        .iter()
        .map(|a| {
            format!(
                "{} {}",
                gen_c_type(&a.qual_type, c_ast, use_public_names),
                a.name
            )
        })
        .collect::<Vec<String>>();

    Ok(format!("{result}({})", arg_str.join(", ")))
}

/// Generate the declaration of this function
/// 
/// This is both the function signature and the #define to give it a public name
fn gen_function_declaration(fun: &CFunction, c_ast: &CAST) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{};\n",
        gen_function_signature(fun, c_ast, true)
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

/// Generate the cast expression for converting the c type to its cpp counterpart
fn gen_cast(qual_type: &CQualType, ast: &AST, c_ast: &CAST) -> Result<Option<String>, TypeError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) => Ok(None),
        CTypeRef::Pointer(pointee_qt) => {
            if let Some(s) = gen_cast(&pointee_qt, ast, c_ast)? {
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

/// Generate the c++ required to cast and pass an argument from c to the cpp fuction call
///
/// For example:
/// NS::Class    -> *(NS::Class*)&arg
/// NS::Class*   -> (NS::Class*)arg
/// NS::Class&   -> *(NS::Class*)arg
/// NS::Class*&  -> *(NS::Class**)arg
fn generate_arg_pass(
    arg_name: &str,
    qual_type: &CQualType,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, ArgumentError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_tk) => Ok(arg_name.to_string()),
        CTypeRef::Pointer(_) => {
            // needs_deref() will be true if this pointer was a reference in the cpp ast, or if it's a conversion of a
            // non-value-type passed by value
            let deref = if qual_type.needs_deref() { "*" } else { "" };
            if let Some(cast) = gen_cast(qual_type, ast, c_ast)? {
                Ok(format!("{deref}({cast}){arg_name}"))
            } else {
                Ok(format!("{deref}{arg_name}"))
            }
        }
        CTypeRef::Ref(_) => {
            if let Some(cast) = gen_cast(qual_type, ast, c_ast)? {
                Ok(format!("*({cast}*)&{arg_name}"))
            } else {
                Ok(format!("{arg_name}"))
            }
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk))?,
    }
}

/// Generate the cpp function call expression, including casting all arguments
fn gen_cpp_call(fun: &CFunction, ast: &AST) -> Result<String, TypeError> {
    match fun.source {
        CFunctionSource::Function(cpp_fun_id) => {
            let cpp_fun = ast.functions().index(cpp_fun_id.into());
            Ok(cpp_fun
                .get_qualified_name(ast)
                .map_err(|e| TypeError::FailedToGetQualifiedName {
                    name: cpp_fun.name().to_string(),
                    source: e,
                })?
                .to_string())
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

/// Generate the definition of this function
/// 
/// This means calling its cpp counterpart and generating all necessary casting
fn gen_function_definition(
    fun: &CFunction,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{} {{\n",
        gen_function_signature(fun, c_ast, false)
            .map_err(|e| FunctionGenerationError::FailedToGenerateSignature(e))?
    );
    let indent = "    ";

    // Get the cast expression to cast the cpp return type to c
    let cast = gen_cast(&fun.result, ast, c_ast)
        .map_err(|e| FunctionGenerationError::FailedToGenerateArg {
            name: "[result]".to_string(),
            source: ArgumentError::TypeError(e),
        })?
        .map(|s| format!("({s})"))
        .unwrap_or("".to_string());

    // Append the call to the cpp function
    result = format!(
        "{result}{indent}return {cast}{}",
        gen_cpp_call(fun, ast).map_err(|e| FunctionGenerationError::FailedToGenerateCall(e))?
    );

    // Get all the arguments for the call
    let arg_str =
        if fun.arguments.is_empty() || fun.arguments.len() == 1 && fun.arguments[0].is_self {
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

/// Generate the spelling for this C type
fn gen_c_type(qt: &CQualType, c_ast: &CAST, use_public_names: bool) -> String {
    match qt.type_ref() {
        CTypeRef::Builtin(tk) => match tk {
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
            _ => qt.format(c_ast, use_public_names),
        },
        _ => qt.format(c_ast, use_public_names),
    }
}

/// Generate the declaration of this struct
fn generate_struct_declaration(st: &CStruct, ast: &AST, c_ast: &CAST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_declaration(st, ast),
        ClassBindKind::ValueType => generate_valuetype_declaration(st, ast, c_ast),
        _ => todo!(),
    }
}

/// Generate the declaration for a valuetype struct
/// 
/// This means generating all fields
fn generate_valuetype_declaration(st: &CStruct, ast: &AST, c_ast: &CAST) -> Result<String> {
    let ind = "    ";
    let mut result = format!("struct {} {{\n", st.name_private);

    for field in &st.fields {
        result = format!(
            "{result}{ind}{} {};\n",
            gen_c_type(field.qual_type(), c_ast, true),
            field.name()
        );
    }

    result = format!("{result}}};\n");
    result = format!("{result}typedef {} {};\n", st.name_private, st.name_public);

    Ok(result)
}

/// Generate the declaration for an opaqueptr struct
/// 
/// This is basically just a forward declaration since the type is only ever represented by a pointer to it
fn generate_opaqueptr_declaration(st: &CStruct, ast: &AST) -> Result<String> {
    Ok(format!(
        "struct {0};\ntypedef {0} {1};\n",
        st.name_private, st.name_public
    ))
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use bbl_extract::{class::ClassBindKind, parse_string_and_extract_ast};
    use bbl_translate::translate_cpp_ast_to_c;

    use crate::{gen_c, Error};

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

        let (c_header, c_source) = gen_c(&ast, &c_ast)?;
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
            &cli_args(&[])?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        // let class = ast.find_class("Test_1_0::Class")?;
        // ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 0);

        let (c_header, c_source) = gen_c(&ast, &c_ast)?;
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
            &cli_args(&[])?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        // let class = ast.find_class("Test_1_0::Class")?;
        // ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c(&ast, &c_ast)?;
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
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c(&ast, &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    }
}
