use bbl_clang::ty::TypeKind;
use bbl_extract::class::ClassBindKind;
use bbl_translate::{
    cfunction::CFunction,
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    ctypedef::CTypedef,
    CAST, cenum::CEnum,
};

use crate::error::Error;

use std::{borrow::Cow, fmt::Write};

use indoc::indoc;

type Result<T, E = Error> = std::result::Result<T, E>;

pub fn write_rust_ffi_module(module_path: &str, c_ast: &CAST) -> Result<(), Error> {
    // write the rust ffi module
    let mut ffi_source = String::new();
    write_rust_ffi(&mut ffi_source, c_ast)?;
    println!("{}", ffi_source);

    std::fs::write(module_path, &ffi_source)?;

    Ok(())
}

pub fn write_rust_ffi(source: &mut String, c_ast: &CAST) -> Result<()> {
    writeln!(
        source,
        indoc!(
            r#"
            #![allow(non_snake_case)]
            #![allow(non_camel_case_types)]
            #![allow(non_upper_case_globals)]
            #![allow(unused_imports)]
        "#
        )
    )?;

    // typedefs
    for td in c_ast.typedefs.iter() {
        write_typedef_external(source, td)?;
    }
    writeln!(source)?;

    // enums
    for enm in c_ast.enums.iter() {
        write_enum_external(source, enm)?;
    }
    writeln!(source)?;

    // structs
    for st in c_ast.structs.iter() {
        write_struct_external(source, st)?;
    }
    writeln!(source)?;

    // functions
    for fun in c_ast.functions.iter() {
        write_function_external(source, fun)?;
        writeln!(source)?;
    }

    writeln!(source)?;

    writeln!(source, "mod internal {{")?;

    writeln!(source)?;

    writeln!(source, "use std::os::raw::*;")?;
    writeln!(source)?;

    // typedefs
    for td in c_ast.typedefs.iter() {
        write_typedef_internal(source, td, c_ast)?;
    }
    writeln!(source)?;

    // enums
    for enm in c_ast.enums.iter() {
        write_enum_internal(source, enm)?;
    }
    writeln!(source)?;

    // struct definitions
    for st in c_ast.structs.iter() {
        write_struct_internal(source, st, c_ast)?;
    }

    writeln!(source)?;
    writeln!(source, "extern \"C\" {{")?;

    writeln!(source)?;

    // function definitions
    for fun in c_ast.functions.iter() {
        write_function_internal(source, fun, c_ast)?;
        writeln!(source)?;
    }

    writeln!(source, "}} // extern C")?;
    writeln!(source, "}} // mod internal")?;

    Ok(())
}

fn write_enum_external(source: &mut String, enm: &CEnum) -> Result<()> {
    writeln!(source, "pub use internal::{} as {};", enm.name_internal, enm.name_external)?;
    Ok(())
}

fn write_enum_internal(source: &mut String, enm: &CEnum) -> Result<()> {
    writeln!(source, "#[derive(Debug, Copy, Clone)]")?;
    writeln!(source, "pub enum {} {{", enm.name_internal)?;
    for var in &enm.variants {
        writeln!(source, "    {} = {},", var.0, var.1)?;
    }
    writeln!(source, "}}")?;

    Ok(())
}

fn write_typedef_external(source: &mut String, td: &CTypedef) -> Result<()> {
    if let CTypeRef::Pointer(p) = td.underlying_type.type_ref() {
        if let CTypeRef::FunctionProto {..} = p.type_ref() {
            write!(
                source,
                "pub use internal::{} as {};",
                td.name_external, td.name_external
            )?;
        }
    }

    Ok(())
}

fn write_typedef_internal(source: &mut String, td: &CTypedef, c_ast: &CAST) -> Result<()> {
    if let CTypeRef::Pointer(p) = td.underlying_type.type_ref() {
        if let CTypeRef::FunctionProto { result, args } = p.type_ref() {
            write!(source, "pub type {} = fn(", td.name_external)?;

            let mut first = true;
            for arg in args {
                if !first {
                    write!(source, ", ")?;
                } else {
                    first = false;
                }

                write_type(source, arg, c_ast)?;
            }

            write!(source, ") -> ")?;
            write_type(source, result.as_ref(), c_ast)?;
            write!(source, ";")?;
        }
    }

    Ok(())
}

fn write_function_internal(source: &mut String, fun: &CFunction, c_ast: &CAST) -> Result<()> {
    write!(source, "pub fn {}(", fun.name_internal)?;

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

fn write_function_external(source: &mut String, fun: &CFunction) -> Result<()> {
    let external = if fun.name_external != fun.name_internal {
        &fun.name_external
    } else {
        &fun.name_internal
    };

    writeln!(
        source,
        "pub use internal::{} as {};",
        fun.name_internal, external
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

fn write_struct_external(source: &mut String, st: &CStruct) -> Result<()> {
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
            } else if let Some(enm) = c_ast.get_enum(*usr) {
                write!(source, "{}", enm.name_internal)?;
            } else {
                unimplemented!("no struct or typedef {usr}")
            }
        }
        CTypeRef::FunctionProto { result, args } => {
            unimplemented!("Writing function prototype")
        }
        CTypeRef::Template(parm) => {
            panic!("Unexpanded template {parm}")
        }
        CTypeRef::Unknown(tk) => unimplemented!("unknown typekind in write_type: {tk}"),
    }

    Ok(())
}

fn sanitize_name(name: &str) -> Cow<'_, str> {
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
