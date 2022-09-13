use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind};
use bbl_translate::{
    cfunction::CFunction,
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    CAST,
};

use crate::error::Error;

use std::{borrow::Cow, fmt::Write};

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn write_rust_ffi_module(
    c_output_directory: &str,
    module_path: &str,
    c_ast: &CAST,
) -> Result<(), Error> {
    // write the rust ffi module
    let mut ffi_source = String::new();
    write_rust_ffi(&mut ffi_source, c_ast)?;
    println!("{}", ffi_source);

    std::fs::write(module_path, &ffi_source)?;

    Ok(())
}

pub fn write_rust_ffi(source: &mut String, c_ast: &CAST) -> Result<()> {
    for st in c_ast.structs.iter() {
        write_struct_external(source, st, c_ast)?;
    }

    writeln!(source)?;
    for fun in c_ast.functions.iter() {
        write_function_external(source, fun, c_ast)?;
        writeln!(source)?;
    }

    writeln!(source)?;

    writeln!(source, "mod internal {{")?;

    writeln!(source)?;

    writeln!(source, "use std::os::raw::*;")?;
    writeln!(source)?;

    for st in c_ast.structs.iter() {
        write_struct_internal(source, st, c_ast)?;
    }

    writeln!(source)?;
    writeln!(source, "extern \"C\" {{")?;

    writeln!(source)?;
    for fun in c_ast.functions.iter() {
        write_function_internal(source, fun, c_ast)?;
        writeln!(source)?;
    }

    writeln!(source, "}} // extern C")?;
    writeln!(source, "}} // mod internal")?;

    Ok(())
}

fn write_function_internal(source: &mut String, fun: &CFunction, c_ast: &CAST) -> Result<()> {
    write!(source, "pub fn {}(", fun.name_private)?;

    let mut first = true;
    for arg in fun.arguments.iter() {
        if !first {
            write!(source, ", ")?;
        } else {
            first = false;
        }

        write!(source, "{}: ", sanitize_name(&arg.name))?;
        write_type(source, &arg.qual_type, c_ast)?;
    }

    write!(source, ")")?;

    if fun.has_return_value() {
        write!(source, " -> ")?;
        write_type(source, &fun.result, c_ast)?;
    }

    writeln!(source, ";")?;

    Ok(())
}

fn write_function_external(source: &mut String, fun: &CFunction, c_ast: &CAST) -> Result<()> {
    let external = if fun.name_public != fun.name_private {
        &fun.name_public
    } else {
        &fun.name_private
    };

    writeln!(
        source,
        "pub use internal::{} as {};",
        fun.name_private, external
    )?;

    Ok(())
}

fn write_struct_internal(source: &mut String, st: &CStruct, c_ast: &CAST) -> Result<()> {
    match st.bind_kind {
        ClassBindKind::OpaquePtr => {
            write!(
                source,
                r#"
#[repr(C)]
pub struct {0} {{
    _unused: [u8; 0],
}}
"#,
                st.name_internal
            )?;
        }
        ClassBindKind::ValueType => {
            writeln!(source, "#[repr(C)]")?;
            writeln!(source, "pub struct {} {{", st.name_internal)?;

            for field in &st.fields {
                write!(source, "    pub {}: ", sanitize_name(field.name()))?;
                write_type(source, field.qual_type(), c_ast)?;
                writeln!(source, ",")?;
            }

            writeln!(source, "}}")?;
        }
        ClassBindKind::OpaqueBytes => todo!("Handle opaquebytes"),
    }

    if st.name_external != st.name_internal {
        writeln!(
            source,
            "pub type {} = {};",
            st.name_external, st.name_internal
        )?;
    }
    Ok(())
}

fn write_struct_external(source: &mut String, st: &CStruct, c_ast: &CAST) -> Result<()> {
    let external = if st.name_external != st.name_internal {
        &st.name_external
    } else {
        &st.name_internal
    };

    writeln!(
        source,
        "pub use internal::{} as {};",
        st.name_internal, external
    )?;

    Ok(())
}

fn write_type(source: &mut String, qt: &CQualType, c_ast: &CAST) -> Result<()> {
    match qt.type_ref() {
        CTypeRef::Builtin(tk) => write!(
            source,
            "{}",
            match tk {
                TypeKind::Bool => "c_bool",
                TypeKind::Char_S => "c_char",
                TypeKind::Char_U => "c_uchar",
                TypeKind::Double => "c_double",
                TypeKind::Float => "c_float",
                TypeKind::Int => "c_int",
                TypeKind::Long => "c_long",
                TypeKind::LongDouble => "c_longdouble",
                TypeKind::LongLong => "c_longlong",
                TypeKind::Short => "c_short",
                TypeKind::UChar => "c_uchar",
                TypeKind::UInt => "c_uint",
                TypeKind::ULong => "c_ulong",
                TypeKind::ULongLong => "c_ulonglong",
                TypeKind::UShort => "c_ushort",
                TypeKind::Void => "c_void",
                _ => unimplemented!("need to implement builtin {tk}"),
            }
        )?,
        CTypeRef::Pointer(pointee) => {
            let const_ = if pointee.is_const() { "const" } else { "mut" };
            write!(source, "*{} ", const_)?;
            write_type(source, pointee, c_ast)?;
        }
        CTypeRef::Ref(usr) => {
            // first check to see if there's a direct class reference
            if let Some(st) = c_ast.get_struct(*usr) {
                write!(source, "{}", st.format(true))?;
            } else if let Some(td) = c_ast.get_typedef(*usr) {
                // no struct with this USR, see if there's a typedef instead
                write!(source, "{}", td.name_external.clone())?;
            } else {
                unimplemented!("no struct or typedef")
            }
        }
        CTypeRef::Unknown(tk) => unimplemented!("unknown typekind in write_type: {tk}"),
    }

    Ok(())
}

fn sanitize_name<'a>(name: &'a str) -> Cow<'a, str> {
    match name {
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern" | "false"
        | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move"
        | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" | "super"
        | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while" | "async" | "await"
        | "dyn" | "astract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv"
        | "typeof" | "unsized" | "virtual" | "yield" | "try" | "union" => format!("{name}_").into(),
        _ => name.into(),
    }
}
