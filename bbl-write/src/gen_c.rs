use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind, function::Method, index_map::IndexMapKey};
use bbl_translate::{
    cfunction::{CArgument, CFunction, CFunctionSource},
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    ctypedef::CTypedef,
    CAST,
};

use crate::error::{ArgumentError, Error, TypeError};
use tracing::{debug, instrument, trace};

use std::fmt::Write;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Generate the c library code (itself c++ but with a c header)
///
/// # Returns
/// A tuple containing two [`String`]s, the first of which is the header source, the second is the implementation source
#[instrument(level = "trace", skip(ast, c_ast))]
pub fn gen_c(module_name: &str, ast: &AST, c_ast: &CAST) -> Result<(String, String)> {
    let module_name_upper = module_name.to_uppercase().replace("-", "_");
    let mut header = format!("#ifndef __{}_H__\n", module_name_upper);
    header = format!("{header}#define __{}_H__\n\n", module_name_upper);
    header = format!("{header}#ifdef __cplusplus\nextern \"C\" {{\n#endif\n\n");

    let mut source = c_ast
        .includes
        .iter()
        .map(|i| i.get_statement())
        .collect::<Vec<_>>()
        .join("\n");

    writeln!(&mut source, "\n\n#include <utility>")?;
    writeln!(&mut source, "#include <exception>\n")?;

    // Generate forward declarations for each struct first
    header = format!("{header}/* Forward declarations */\n");
    for st in c_ast.structs.iter() {
        let decl = generate_struct_forward_declaration(st, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;

    // Generate the declaration for each struct
    header = format!("{header}/* Struct declarations */\n");
    for st in c_ast.structs.iter() {
        let decl = generate_struct_declaration(st, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;

    // Generate typedefs
    header = format!("{header}/* Typedefs */\n");
    // for td in c_ast.typedefs.iter() {
    //     let decl = generate_typedef(td, c_ast)?;
    //     header = format!("{header}{decl}\n")
    // }

    writeln!(&mut header)?;

    // Generate the declaration and definition for each function
    header = format!("{header}/* Function declarations */\n");
    for fun in c_ast.functions.iter() {
        let decl = gen_function_declaration(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source: Box::new(source),
            }
        })?;

        let defn = gen_function_definition(fun, ast, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source: Box::new(source),
            }
        })?;

        header = format!("{header}{decl}\n");
        source = format!("{source}{defn}\n");
    }

    header = format!("{header}\n#ifdef __cplusplus\n}}\n#endif\n\n");

    header = format!("{header}\n#endif /* ifdef __{}_H__ */\n", module_name_upper);

    Ok((header, source))
}

/// Generate the signature for this function
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_signature(
    fun: &CFunction,
    c_ast: &CAST,
    use_public_names: bool,
) -> Result<String, Error> {
    let result = format!(
        "{} {}",
        gen_c_type(&fun.result, c_ast, use_public_names)?,
        fun.name_private
    );

    let arg_str = fun
        .arguments
        .iter()
        .map(|a| {
            format!(
                "{} {}",
                gen_c_type(&a.qual_type, c_ast, use_public_names).unwrap(),
                a.name
            )
        })
        .collect::<Vec<String>>();

    Ok(format!("{result}({})", arg_str.join(", ")))
}

/// Generate the declaration of this function
///
/// This is both the function signature and the #define to give it a public name
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_declaration(fun: &CFunction, c_ast: &CAST) -> Result<String, Error> {
    let mut result = format!(
        "{};\n",
        gen_function_signature(fun, c_ast, true).map_err(|e| Error::FailedToGenerateSignature {
            name: fun.name_private.clone(),
            source: Box::new(e)
        })?
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
#[instrument(level = "trace", skip(ast, c_ast))]
fn gen_cast(qual_type: &CQualType, ast: &AST, c_ast: &CAST) -> Result<Option<String>, TypeError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) => Ok(None),
        CTypeRef::Pointer(pointee_qt) => {
            if let Some(s) = gen_cast(pointee_qt, ast, c_ast)? {
                Ok(Some(format!("{s}*")))
            } else {
                Ok(None)
            }
        }
        CTypeRef::Ref(usr) => {
            // ref might be to a class directly, or to a typedef
            if let Some(class) = ast.get_class(*usr) {
                Ok(Some(class.get_qualified_name(ast).map_err(|source| {
                    TypeError::FailedToGetQualifiedName {
                        name: class.name().to_string(),
                        source,
                    }
                })?))
            } else if let Some(cts) = ast.get_type_alias(*usr) {
                Ok(Some(cts.get_qualified_name(ast).map_err(|source| {
                    TypeError::FailedToGetQualifiedName {
                        name: cts.name().to_string(),
                        source,
                    }
                })?))
            } else {
                Err(TypeError::TypeRefNotFound(*usr))
            }
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
#[instrument(level = "trace", skip(ast, c_ast))]
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
                Ok(arg_name.to_string())
            }
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk))?,
    }
}

/// Generate the cpp function call expression, including casting all arguments
#[instrument(level = "trace", skip(ast))]
fn gen_cpp_call(fun: &CFunction, ast: &AST) -> Result<String, TypeError> {
    match fun.source {
        CFunctionSource::Function(cpp_fun_id) => {
            let cpp_fun = &ast.functions()[cpp_fun_id];
            Ok(cpp_fun.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: cpp_fun.name().to_string(),
                    source: e,
                }
            })?)
        }
        CFunctionSource::Method((class_id, method_id)) => {
            // TODO(AL): if this is a class template specialiation, we need to use the specialized name here in order to
            // get the typedef name rather then the underlying name
            let class = &ast.classes()[class_id];
            let method: &Method = &class.methods()[method_id.get()];

            let qname = fun
                .method_info
                .as_ref()
                .map(|info| info.class_qname.clone())
                .unwrap(); // safe to unwrap here because we only ever populate method_info for methods

            if method.is_static() {
                Ok(qname)
            } else if method.is_any_constructor() {
                match class.bind_kind() {
                    ClassBindKind::ValueType => Ok(qname),
                    ClassBindKind::OpaquePtr => Ok(format!("new {qname}")),
                    ClassBindKind::OpaqueBytes => {
                        todo!("Handle opaque bytes")
                    }
                }
            } else {
                Ok(format!("(({}*)self)->{}", qname, method.name()))
            }
        }
        CFunctionSource::SpecializedMethod((class_id, method_id)) => {
            let class = &ast.classes()[class_id];
            let spec_method = &class.specialized_methods()[method_id.0];
            let temp_method = &class.methods()[spec_method.specialized_decl().get()];

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
#[instrument(level = "trace", skip(ast, c_ast))]
fn gen_function_definition(fun: &CFunction, ast: &AST, c_ast: &CAST) -> Result<String, Error> {
    let mut body = format!(
        "{} {{\n",
        gen_function_signature(fun, c_ast, false).map_err(|e| {
            Error::FailedToGenerateSignature {
                name: fun.name_private.clone(),
                source: Box::new(e),
            }
        })?
    );
    // TODO (AL): clean up this nastiness
    let mut indent = "    ";
    let mut indent2 = "        ";
    let mut indent3 = "            ";

    if !fun.is_noexcept {
        writeln!(&mut body, "{indent}try {{")?;
        indent = "        ";
        indent2 = "            ";
        indent3 = "                ";
    }

    let result_is_moved = if let Some(result) = fun.return_value() {
        // Get the cast expression to cast the cpp return type to c
        let cast = gen_cast(&result.qual_type, ast, c_ast)
            .map_err(|e| Error::FailedToGenerateArgument {
                name: result.name.clone(),
                source: Box::new(e),
            })?
            .map(|s| format!("({s})"))
            .unwrap_or_else(|| "".to_string());

        // Append the call to the cpp function
        write!(
            &mut body,
            "{indent}*({cast}{}) = std::move(\n{indent2}{}",
            result.name,
            gen_cpp_call(fun, ast).map_err(|e| Error::FailedToGenerateCall {
                name: fun.name_private.clone(),
                source: Box::new(e)
            })?
        )?;

        true
    } else {
        write!(
            &mut body,
            "{indent}{}",
            gen_cpp_call(fun, ast).map_err(|e| Error::FailedToGenerateCall {
                name: fun.name_private.clone(),
                source: Box::new(e)
            })?
        )?;

        false
    };

    // Get all the arguments for the call
    gen_call_arguments(&mut body, fun, ast, c_ast)?;

    writeln!(
        &mut body,
        "{indent}{};\n{indent}return 0;",
        if result_is_moved { ")" } else { "" }
    )?;

    if !fun.is_noexcept {
        indent = "    ";
        indent2 = "        ";
        writeln!(&mut body, "{indent}}} catch (std::exception& e) {{")?;
        writeln!(&mut body, "{indent2}return 1;")?;
        writeln!(&mut body, "{indent}}}")?;
    }

    writeln!(&mut body, "}}")?;

    Ok(body)
}

/// Create the call arguments string
fn gen_call_arguments(body: &mut String, fun: &CFunction, ast: &AST, c_ast: &CAST) -> Result<()> {
    let indent = "        ";
    let indent2 = "            ";
    let indent3 = "                ";

    let arguments: Vec<&CArgument> = fun
        .arguments
        .iter()
        .filter(|a| !(a.is_result || a.is_self))
        .collect();

    if arguments.is_empty() {
        writeln!(body, "()")?;
    } else {
        let mut args = Vec::new();
        for arg in arguments {
            match generate_arg_pass(&arg.name, &arg.qual_type, ast, c_ast) {
                Ok(s) => args.push(format!("{indent3}{s}")),
                Err(source) => {
                    return Err(Error::FailedToGenerateArgument {
                        name: arg.name.to_string(),
                        source: Box::new(source),
                    })
                }
            }
        }

        writeln!(body, "(\n{}\n{indent2})\n", args.join(",\n"),)?;
    };

    Ok(())
}

/// Generate the spelling for this C type
#[instrument(level = "trace", skip(c_ast))]
fn gen_c_type(qt: &CQualType, c_ast: &CAST, use_public_names: bool) -> Result<String> {
    Ok(match qt.type_ref() {
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
            _ => qt.format(c_ast, use_public_names)?,
        },
        CTypeRef::Ref(usr) => {
            // first check to see if there's a direct class reference
            if let Some(st) = c_ast.get_struct(*usr) {
                st.format(use_public_names)
            } else if let Some(td) = c_ast.get_typedef(*usr) {
                // no struct with this USR, see if there's a typedef instead
                td.name_external.clone()
            } else {
                unimplemented!("no struct or typedef")
            }
        }
        _ => qt.format(c_ast, use_public_names)?,
    })
}

/// Generate a forwad declaration for this struct
#[instrument(level = "trace", skip(c_ast))]
fn generate_struct_forward_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    Ok(format!("typedef struct {0} {0};", st.name_internal))
}

/// Generate the declaration of this struct
#[instrument(level = "trace", skip(c_ast))]
fn generate_struct_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_declaration(st),
        ClassBindKind::ValueType => generate_valuetype_declaration(st, c_ast),
        _ => todo!(),
    }
}

#[instrument(level = "trace", skip(c_ast))]
fn generate_typedef(td: &CTypedef, c_ast: &CAST) -> Result<String> {
    Ok(format!(
        "typedef {} {};",
        c_ast
            .get_struct(td.typ)
            .ok_or_else(|| Error::FailedToGenerateTypedef {
                name: td.name_external.clone(),
                source: Box::new(TypeError::TypeRefNotFound(td.typ))
            })?
            .name_internal,
        td.name_external
    ))
}

/// Generate the declaration for a valuetype struct
///
/// This means generating all fields
#[instrument(level = "trace", skip(c_ast))]
fn generate_valuetype_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    let ind = "    ";
    let mut result = format!("struct {} {{\n", st.name_internal);

    for field in &st.fields {
        result = format!(
            "{result}{ind}{} {};\n",
            gen_c_type(field.qual_type(), c_ast, true)?,
            field.name()
        );
    }

    result = format!("{result}}};\n");

    if st.name_external != st.name_internal {
        result = format!(
            "{result}typedef {} {};\n",
            st.name_internal, st.name_external
        );
    }

    Ok(result)
}

/// Generate the declaration for an opaqueptr struct
///
/// This is basically just a forward declaration since the type is only ever represented by a pointer to it
#[instrument(level = "trace")]
fn generate_opaqueptr_declaration(st: &CStruct) -> Result<String> {
    if st.name_external != st.name_internal {
        Ok(format!(
            "typedef {0} {1};\n",
            st.name_internal, st.name_external
        ))
    } else {
        Ok("".to_string())
    }
}
