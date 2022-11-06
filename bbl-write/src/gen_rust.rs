use bbl_clang::ty::TypeKind;
use bbl_translate::to_rust::{Expr, RMethod, RStruct, RTypeRef, RAST};
use std::fmt::Write;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub fn write_rust(source: &mut String, rast: &RAST) -> Result<()> {
    for st in rast.structs().iter() {
        write_struct(source, st, rast)?;
        writeln!(source)?;
    }

    Ok(())
}

fn write_struct(source: &mut String, st: &RStruct, rast: &RAST) -> Result<()> {
    writeln!(source, "pub struct {} {{", st.name())?;
    writeln!(source, "}}")?;

    if !st.methods().is_empty() {
        writeln!(source, "\nimpl {} {{", st.name())?;

        for method in st.methods() {
            write_method(source, method, rast)?;
            writeln!(source)?;
        }

        writeln!(source, "}}\n")?;
    }

    Ok(())
}

fn write_method(source: &mut String, method: &RMethod, rast: &RAST) -> Result<()> {
    let s_args = method
        .arguments()
        .iter()
        .map(|arg| {
            let mut s_arg = String::new();
            if !arg.ty().is_self() {
                write!(s_arg, "{}: ", arg.name())?;
            }
            write_type(&mut s_arg, arg.ty(), rast)?;
            Ok(s_arg)
        })
        .collect::<Result<Vec<String>>>()?;

    write!(
        source,
        "    pub fn {}({})",
        method.name(),
        s_args.join(", ")
    )?;

    if let Some(result) = method.result() {
        write!(source, " -> ")?;
        write_type(source, result, rast)?;
    }

    write!(source, " ")?;
    write_expr(source, method.body(), 1)?;

    Ok(())
}

fn write_type(source: &mut String, ty: &RTypeRef, rast: &RAST) -> Result<()> {
    use RTypeRef::*;
    match ty {
        Builtin(tk) => write_builtin(source, *tk)?,
        SelfParam => write!(source, "self")?,
        Ref(usr) | Typedef(usr) => {
            write!(source, "{}", rast.get_typename(*usr)?)?;
        }
        Reference { is_mut, pointee } => {
            write!(source, "&{}", if *is_mut { "mut " } else { "" })?;
            write_type(source, pointee, rast)?;
        }
    }

    Ok(())
}

fn write_builtin(source: &mut String, tk: TypeKind) -> Result<()> {
    match tk {
        TypeKind::Bool => write!(source, "bool")?,
        TypeKind::Char_S => write!(source, "i8")?,
        TypeKind::Char_U => write!(source, "u8")?,
        TypeKind::Double => write!(source, "f64")?,
        TypeKind::Float => write!(source, "f32")?,
        TypeKind::Int => write!(source, "i32")?,
        TypeKind::Long => write!(source, "i64")?,
        TypeKind::LongDouble => write!(source, "f64")?,
        TypeKind::LongLong => write!(source, "i64")?,
        TypeKind::Short => write!(source, "i16")?,
        TypeKind::UChar => write!(source, "u8")?,
        TypeKind::UInt => write!(source, "u32")?,
        TypeKind::ULong => write!(source, "u64")?,
        TypeKind::ULongLong => write!(source, "u64")?,
        TypeKind::UShort => write!(source, "u16")?,
        _ => todo!("Unhandled TypeKind for writing: {tk}"),
    }

    Ok(())
}

fn write_expr(body: &mut String, e: &Expr, depth: usize) -> Result<()> {
    match e {
        Expr::Token(s) => write!(body, "{s}")?,
        Expr::Block(stmts) => {
            writeln!(body, "{{")?;
            for stmt in stmts {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, stmt, depth + 1)?;
            }
            writeln!(body, "{:width$}}}", "", width = depth * 4)?;
        }
        Expr::Unsafe(expr) => {
            write!(body, "unsafe ")?;
            write_expr(body, expr, depth)?;
        }
        Expr::Let {
            name,
            value,
            is_mut,
        } => {
            write!(body, "let {}{name} = ", if *is_mut { "mut " } else { "" })?;
            write_expr(body, value, depth)?;
            writeln!(body, ";")?;
        }
        Expr::FunctionCall { name, args } => {
            if args.is_empty() {
                write!(body, "{name}()")?;
            } else {
                writeln!(body, "{name}(")?;
                for arg in args {
                    write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                    write_expr(body, arg, depth + 1)?;
                    writeln!(body, ",")?;
                }
                write!(body, "{:width$}", "", width = depth * 4)?;
                write!(body, ")")?;
            }
        }
        Expr::Ref { target, is_mut } => {
            write!(body, "&{}", if *is_mut { "mut " } else { "" })?;
            write_expr(body, target, depth)?;
        }
        Expr::As { src, dst } => {
            write_expr(body, src, depth)?;
            write!(body, " as ")?;
            write_expr(body, dst, depth)?;
        }
        Expr::Star { is_mut, target } => {
            write!(body, "*{}", if *is_mut { "mut " } else { "const " })?;
            write_expr(body, target, depth)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use bbl_extract::qualtype::QualType;
    use bbl_extract::templates::TemplateArgument;
    use bbl_extract::{class::OverrideList, parse_string_and_extract_ast, AllowList};

    use bbl_translate::to_rust::translate_cpp_ast_to_rust;
    use bbl_translate::translate_cpp_ast_to_c;

    use crate::gen_rust::write_rust;

    use indoc::indoc;

    #[test]
    fn write_rust_pass_class() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
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
                true,
            )?;

            let ast = ast.monomorphize()?;
            let c_ast = translate_cpp_ast_to_c(&ast, true)?;
            let rast = translate_cpp_ast_to_rust(&ast, &c_ast)?;

            let mut source = String::new();
            write_rust(&mut source, &rast)?;

            println!("{source}");

            Ok(())
        })
    }

    #[test]
    fn write_rust_method_template_ret() -> bbl_util::Result<()> {
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

/*
pub struct Test_Class {
    inner: *mut ffi::Test_Class,
}

impl Test_Class {
    pub fn method_float(&mut self, arg: &f32) -> &f32 {
        let mut result = std::ptr::null_mut();
        unsafe {
            let _return_code = ffi::Test_Class_method_float(
                self.inner, 
                &mut result as *mut f32 as *mut c_float, 
                arg as *const f32 as *const c_float,
            );
        }
    }
}
*/
            "#,
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            let class = ast.find_class("Class")?;
            let method = ast.find_method(class, "method_template")?;
            ast.specialize_method(
                class,
                method,
                "method_float",
                vec![TemplateArgument::Type(QualType::float())],
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");

            let c_ast = translate_cpp_ast_to_c(&ast, true)?;

            bbl_util::compare(
                &format!("{c_ast:?}"),
                indoc!(
                    r#"
            CStruct c:@N@Test@S@Class Test_Class Test_Class ValueType fields=[]
            CFunction Test_Class_method_float Test_Class_method_float([this_: c:@N@Test@S@Class*, result: Float const**, arg: Float const*])  -> Int
            CFunction Test_Class_ctor Test_Class_ctor([result: c:@N@Test@S@Class*])  -> Int
            CFunction Test_Class_copy_ctor Test_Class_copy_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const* const])  -> Int
            CFunction Test_Class_move_ctor Test_Class_move_ctor([result: c:@N@Test@S@Class*, rhs: c:@N@Test@S@Class const*])  -> Int
        "#
                ),
            )?;

            let rast = translate_cpp_ast_to_rust(&ast, &c_ast)?;

            let mut source = String::new();
            write_rust(&mut source, &rast)?;

            println!("{source}");

            Ok(())
        })
    }
}
