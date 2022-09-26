use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind};
use bbl_translate::{
    cfunction::{CFunction, Expr},
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    ctypedef::CTypedef,
    CAST,
};

use crate::error::{Error, TypeError};
use tracing::instrument;

use std::fmt::Write;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Generate the c library code (itself c++ but with a c header)
///
/// # Returns
/// A tuple containing two [`String`]s, the first of which is the header source, the second is the implementation source
#[instrument(level = "trace", skip(c_ast))]
pub fn gen_c(module_name: &str, c_ast: &CAST) -> Result<(String, String)> {
    let module_name_upper = module_name.to_uppercase().replace('-', "_");
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
        let decl = generate_struct_forward_declaration(st)?;
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
    for td in c_ast.typedefs.iter() {
        let decl = generate_typedef(td, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;

    // Generate the declaration and definition for each function
    header = format!("{header}/* Function declarations */\n");
    for fun in c_ast.functions.iter() {
        let decl = gen_function_declaration(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_internal.clone(),
                source: Box::new(source),
            }
        })?;

        let defn = gen_function_definition(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_internal.clone(),
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
        fun.name_internal
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
            name: fun.name_internal.clone(),
            source: Box::new(e)
        })?
    );

    if fun.name_internal != fun.name_external {
        result = format!(
            "{result}#define {} {};\n",
            fun.name_external, fun.name_internal
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

fn write_expr(body: &mut String, expr: &Expr, depth: usize) -> Result<()> {
    match expr {
        Expr::Compound(stmts) => {
            writeln!(body, " {{")?;
            for expr in stmts {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, expr, depth + 1)?;
                writeln!(body, ";")?;
            }
            writeln!(body, "{:width$}}}", "", width = depth * 4)?;
        }
        Expr::CppMethodCall {
            receiver,
            function,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            write!(body, "->{function}(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            for arg in arguments {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, arg, depth + 1)?;
                writeln!(body)?;
            }
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppStaticMethodCall {
            receiver,
            function,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            write!(body, "::{function}(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            for arg in arguments {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, arg, depth + 1)?;
                writeln!(body)?;
            }
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppFunctionCall {
            function,
            arguments,
        } => {
            write!(body, "{function}(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            for arg in arguments {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, arg, depth + 1)?;
                writeln!(body)?;
            }
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppConstructor {
            receiver,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            write!(body, "(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            for arg in arguments {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, arg, depth + 1)?;
                writeln!(body)?;
            }
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::Cast { to_type, value } => {
            write!(body, "(({to_type})")?;
            write_expr(body, value, depth)?;
            write!(body, ")")?;
        }
        Expr::Deref { value } => {
            write!(body, "*")?;
            write_expr(body, value, depth)?;
        }
        Expr::AddrOf { value } => {
            write!(body, "&")?;
            write_expr(body, value, depth)?;
        }
        Expr::Move(expr) => {
            write!(body, "std::move(")?;
            write_expr(body, expr, depth)?;
            write!(body, ")")?;
        }
        Expr::New(expr) => {
            write!(body, "new ")?;
            write_expr(body, expr, depth)?;
        }
        Expr::Delete(expr) => {
            write!(body, "delete ")?;
            write_expr(body, expr, depth)?;
        }
        Expr::Assignment { left, right } => {
            write_expr(body, left, depth)?;
            write!(body, " = ")?;
            write_expr(body, right, depth)?;
        }
        Expr::Token(v) => {
            write!(body, "{v}")?;
        }
        Expr::TryCatch {
            try_block,
            catch_blocks,
        } => {
            writeln!(body, "try {{")?;
            write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
            write_expr(body, try_block, depth + 1)?;
            writeln!(body, ";")?;
            write!(body, "{:width$}}}", "", width = depth * 4)?;

            for catch in catch_blocks {
                write!(body, " catch (")?;
                write_expr(body, &catch.exception, depth)?;
                writeln!(body, ") {{")?;
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, &catch.stmt, depth + 1)?;
                writeln!(body, ";")?;
                write!(body, "{:width$}}}", "", width = depth * 4)?;
            }
        }
        Expr::Return(expr) => {
            write!(body, "return ")?;
            write_expr(body, expr, depth)?;
        }
        _ => (),
    }

    Ok(())
}

/// Generate the definition of this function
///
/// This means calling its cpp counterpart and generating all necessary casting
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_definition(fun: &CFunction, c_ast: &CAST) -> Result<String, Error> {
    let mut body = gen_function_signature(fun, c_ast, false).map_err(|e| {
        Error::FailedToGenerateSignature {
            name: fun.name_internal.clone(),
            source: Box::new(e),
        }
    })?;

    write_expr(&mut body, &fun.body, 0)?;

    Ok(body)
}

/// Generate the spelling for this C type
#[instrument(level = "trace", skip(c_ast))]
fn gen_c_type(qt: &CQualType, c_ast: &CAST, use_public_names: bool) -> Result<String> {
    let const_ = if qt.is_const() { " const" } else { "" };
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
            _ => unimplemented!("need to implement builtin {tk}"),
        },
        CTypeRef::Ref(usr) => {
            // first check to see if there's a direct class reference
            if let Some(st) = c_ast.get_struct(*usr) {
                format!("{}{const_}", st.format(use_public_names))
            } else if let Some(td) = c_ast.get_typedef(*usr) {
                // no struct with this USR, see if there's a typedef instead
                format!("{}{const_}", td.name_external.clone())
            } else {
                unimplemented!("no struct or typedef for {usr}")
            }
        }
        CTypeRef::Pointer(pointee) => {
            format!("{}*{const_}", gen_c_type(pointee, c_ast, use_public_names)?)
        }
        _ => unimplemented!("Need to implement {qt:?}"),
    })
}

/// Generate a forwad declaration for this struct
#[instrument(level = "trace")]
fn generate_struct_forward_declaration(st: &CStruct) -> Result<String> {
    Ok(format!("typedef struct {0} {0};", st.name_internal))
}

/// Generate the declaration of this struct
#[instrument(level = "trace", skip(c_ast))]
fn generate_struct_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_declaration(st),
        ClassBindKind::ValueType => generate_valuetype_declaration(st, c_ast),
        _ => todo!("Handle opaquebytes"),
    }
}

#[instrument(level = "trace", skip(c_ast))]
fn generate_typedef(td: &CTypedef, c_ast: &CAST) -> Result<String> {
    Ok(format!(
        "typedef {} {};",
        gen_c_type(&td.underlying_type, c_ast, true)?,
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
