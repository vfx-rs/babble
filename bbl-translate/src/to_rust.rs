use std::borrow::Cow;

use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    ast::{MonoAST, AST},
    class::{ClassBindKind, ClassDecl},
    function::Method,
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::{QualType, TypeRef},
};
use bbl_util::Trace;
use hashbrown::HashSet;
use std::fmt::Write;
use tracing::warn;

use crate::{
    cfunction::CArgument,
    ctype::{CQualType, CTypeRef},
    error::Error,
    CAST,
};
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct RAST {
    structs: UstrIndexMap<RStruct, RStructId>,
}

impl RAST {
    pub fn structs(&self) -> &UstrIndexMap<RStruct, RStructId> {
        &self.structs
    }

    pub fn get_typename(&self, usr: USR) -> Result<&str> {
        self.structs
            .get(&usr.into())
            .map(|st| st.name())
            .ok_or_else(|| Error::FailedToGetClassFromRef {
                usr,
                source: Trace::new(),
            })
    }
}

impl std::fmt::Debug for RAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for st in self.structs.iter() {
            writeln!(f, "{st:?}")?;
        }

        Ok(())
    }
}

pub fn translate_cpp_ast_to_rust(ast: &MonoAST, c_ast: &CAST) -> Result<RAST> {
    let mut structs = UstrIndexMap::new();

    for class in ast.classes().iter() {
        // if this is a template class, we'll ignore it and translate its specializations instead
        if class.is_templated() {
            if !class.is_specialized() {
                warn!(
                    "class {} is templated but has no specializations and so will be ignored",
                    class.get_qualified_name(ast)?
                );
            }
            continue;
        }

        let st = translate_class(class, ast, c_ast).map_err(|e| Error::FailedToTranslateClass {
            name: class.name().to_string(),
            source: Box::new(e),
        })?;
        structs.insert(st.usr().into(), st);
    }

    Ok(RAST { structs })
}

fn translate_class(class: &ClassDecl, ast: &AST, c_ast: &CAST) -> Result<RStruct> {
    let st_name = sanitize_name(class.name());
    let usr = class.usr();

    let mut methods = Vec::new();
    for method in class.methods().iter() {
        if method.is_templated() {
            if !method.is_specialized() {
                warn!(
                    "Method {} is templated but has no specializations and so will be ignored",
                    method.name()
                );
            }
            continue;
        }

        methods.push(translate_method(class, method, ast, c_ast).map_err(|e| {
            Error::TranslateMethod {
                name: method.name().to_string(),
                source: Box::new(e),
            }
        })?);
    }

    Ok(RStruct {
        name: st_name.to_string(),
        usr,
        methods,
    })
}

fn translate_method(
    _class: &ClassDecl,
    method: &Method,
    ast: &AST,
    c_ast: &CAST,
) -> Result<RMethod> {
    let name = method.name().to_string();
    let usr = method.usr();

    let result = if let TypeRef::Builtin(TypeKind::Void) = method.result().type_ref {
        None
    } else {
        Some(
            translate_type(method.result(), ast).map_err(|e| Error::TranslateResult {
                source: Box::new(e),
            })?,
        )
    };

    let mut arguments = Vec::new();
    if !method.is_static() {
        arguments.push(RArgument {
            name: "[self]".to_string(),
            ty: RTypeRef::Reference {
                is_mut: !method.is_const(),
                pointee: Box::new(RTypeRef::SelfParam),
            },
        })
    }

    let has_result = !matches!(method.result().type_ref, TypeRef::Builtin(TypeKind::Void));

    let mut used_argument_names = HashSet::new();
    used_argument_names.insert("_return_code".to_string()); //< we'll use this later

    // first generate all the arguments
    for arg in method.arguments() {
        let arg_name = get_unique_argument_name(arg.name(), &mut used_argument_names);

        arguments.push(RArgument {
            name: arg_name.clone(),
            ty: translate_type(arg.qual_type(), ast).map_err(|e| Error::TranslateArgument {
                name: arg_name,
                source: Box::new(e),
            })?,
        })
    }

    let c_fn = c_ast
        .get_function(method.usr())
        .ok_or_else(|| Error::FunctionNotFound {
            name: method.usr().to_string(),
            source: Trace::new(),
        })?;

    let mut body = Vec::new();
    let mut rust_call_arguments = arguments.clone();
    if has_result {
        rust_call_arguments.insert(
            1,
            RArgument {
                name: "result".to_string(),
                ty: RTypeRef::Builtin(TypeKind::Void),
            },
        )
    }
    let mut call_args = Vec::new();

    // now generate the call expressions
    for (rarg, carg) in rust_call_arguments.iter().zip(c_fn.arguments.iter()) {
        call_args.push(create_argument_expr(rarg, carg, ast, c_ast)?);
    }

    let call_expr = Expr::FunctionCall {
        name: format!("ffi::{}", c_fn.name_external),
        args: call_args,
    };

    let return_code_expr = Expr::Let {
        name: "_return_code".to_string(),
        value: Box::new(call_expr),
        is_mut: false,
    };

    body.push(Expr::Unsafe(Box::new(Expr::Block(vec![return_code_expr]))));

    Ok(RMethod {
        name,
        usr,
        result,
        arguments,
        body: Expr::Block(body),
    })
}

fn create_argument_expr(
    rarg: &RArgument,
    carg: &CArgument,
    ast: &AST,
    c_ast: &CAST,
) -> Result<Expr> {
    match carg.qual_type().type_ref() {
        CTypeRef::Builtin(_) => Ok(Expr::Token(rarg.name().to_string())),
        CTypeRef::Ref(usr) => {
            let st = c_ast
                .get_struct(*usr)
                .ok_or_else(|| Error::FailedToCreateArgumentExpr {
                    name: carg.name().to_string(),
                    source: Box::new(Error::ClassNotFound {
                        name: usr.to_string(),
                        source: Trace::new(),
                    }),
                })?;

            match st.bind_kind {
                ClassBindKind::ValueType => Ok(Expr::Token(rarg.name().to_string())),
                _ => Err(Error::FailedToCreateArgumentExpr {
                    name: carg.name().to_string(),
                    source: Box::new(Error::ImproperPassByValue(Trace::new())),
                }),
            }
        }
        CTypeRef::Pointer(_) => {
            let (rstar, cstar) = create_star_exprs(carg.qual_type(), ast, c_ast)?;
            Ok(Expr::As {
                src: Box::new(Expr::As {
                    src: Box::new(Expr::Token(rarg.name().to_string())),
                    dst: Box::new(rstar),
                }),
                dst: Box::new(cstar),
            })
        }
        CTypeRef::FunctionProto { result: _, args: _ } => todo!(),
        CTypeRef::Template(_) => todo!(),
    }
}

// create the pair of nested as exprs needed to cast a rust reference to a C pointer
fn create_star_exprs(cty: &CQualType, ast: &AST, c_ast: &CAST) -> Result<(Expr, Expr)> {
    match cty.type_ref() {
        CTypeRef::Builtin(tk) => {
            let mut rb = String::new();
            let mut cb = String::new();
            write_rust_builtin(&mut rb, *tk)?;
            write_ffi_builtin(&mut cb, *tk)?;

            Ok((Expr::Token(rb), Expr::Token(cb)))
        }
        CTypeRef::Ref(usr) => {
            let rname = sanitize_name(ast.get_typeref_name(*usr).unwrap());
            let cname = c_ast.get_typeref_name_external(*usr).unwrap();

            Ok((
                Expr::Token(rname.to_string()),
                Expr::Token(cname.to_string()),
            ))
        }
        CTypeRef::Pointer(pointee) => {
            let is_mut = !pointee.is_const();

            create_star_exprs(pointee, ast, c_ast).map(|(rin, cin)| {
                (
                    Expr::Star {
                        is_mut,
                        target: Box::new(rin),
                    },
                    Expr::Star {
                        is_mut,
                        target: Box::new(cin),
                    },
                )
            })
        }
        CTypeRef::FunctionProto { result: _, args: _ } => todo!(),
        CTypeRef::Template(_) => todo!(),
    }
}

fn write_rust_builtin(source: &mut String, tk: TypeKind) -> Result<()> {
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

fn write_ffi_builtin(source: &mut String, tk: TypeKind) -> Result<()> {
    write!(
        source,
        "{}",
        match tk {
            TypeKind::Bool => "bool",
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
    )?;

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Token(String),
    Block(Vec<Expr>),
    Unsafe(Box<Expr>),
    Let {
        name: String,
        value: Box<Expr>,
        is_mut: bool,
    },
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    Ref {
        target: Box<Expr>,
        is_mut: bool,
    },
    As {
        src: Box<Expr>,
        dst: Box<Expr>,
    },
    Star {
        is_mut: bool,
        target: Box<Expr>,
    },
}

fn translate_type(qt: &QualType, ast: &AST) -> Result<RTypeRef> {
    let result = match &qt.type_ref {
        TypeRef::Builtin(tk) => RTypeRef::Builtin(*tk),
        TypeRef::Ref(usr) => RTypeRef::Ref(*usr),
        TypeRef::Pointer(pointee)
        | TypeRef::LValueReference(pointee)
        | TypeRef::RValueReference(pointee) => {
            let is_mut = !pointee.is_const;
            RTypeRef::Reference {
                is_mut,
                pointee: Box::new(translate_type(pointee, ast)?),
            }
        }
        _ => {
            return Err(Error::TriedToTranslateTemplateParmeter {
                name: qt.name.clone(),
                source: Trace::new(),
            })
        }
    };

    Ok(result)
}

fn get_unique_argument_name(name: &str, used_argument_names: &mut HashSet<String>) -> String {
    let mut result = name.to_string();
    if result.is_empty() {
        result = "arg".into();
    }

    result = sanitize_name(&result).to_string();

    let mut i = 0;
    while used_argument_names.contains(&result) {
        result = format!("{name}{i}");
        i += 1
    }

    result
}

pub fn sanitize_name(name: &str) -> Cow<'_, str> {
    match name {
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum" | "extern" | "false"
        | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "match" | "mod" | "move"
        | "mut" | "pub" | "ref" | "return" | "self" | "Self" | "static" | "struct" | "super"
        | "trait" | "true" | "type" | "unsafe" | "use" | "where" | "while" | "async" | "await"
        | "dyn" | "astract" | "become" | "box" | "do" | "final" | "macro" | "override" | "priv"
        | "typeof" | "unsized" | "virtual" | "yield" | "try" | "union" => format!("{name}_").into(),
        _ => regex::Regex::new("(?:[^a-zA-Z0-9])+")
            .unwrap()
            .replace_all(name, "_"),
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RStruct {
    name: String,
    usr: USR,
    methods: Vec<RMethod>,
}

impl RStruct {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn methods(&self) -> &[RMethod] {
        self.methods.as_ref()
    }
}

impl std::fmt::Debug for RStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "RStruct {} {}", self.usr, self.name)?;

        for method in &self.methods {
            writeln!(f, "{method:?}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RStructId(usize);

impl RStructId {
    pub fn new(id: usize) -> RStructId {
        RStructId(id)
    }
}

impl IndexMapKey for RStructId {
    fn get(&self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RMethod {
    name: String,
    usr: USR,
    result: Option<RTypeRef>,
    arguments: Vec<RArgument>,
    body: Expr,
}

impl RMethod {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn result(&self) -> Option<&RTypeRef> {
        self.result.as_ref()
    }

    pub fn arguments(&self) -> &[RArgument] {
        self.arguments.as_ref()
    }

    pub fn body(&self) -> &Expr {
        &self.body
    }
}

impl std::fmt::Debug for RMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RMethod {} {} result={:?} arguments={:?}",
            self.name, self.usr, self.result, self.arguments
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct RArgument {
    name: String,
    ty: RTypeRef,
}

impl RArgument {
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn ty(&self) -> &RTypeRef {
        &self.ty
    }
}

impl std::fmt::Debug for RArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name, self.ty)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum RTypeRef {
    Builtin(TypeKind),
    SelfParam,
    Ref(USR),
    Typedef(USR),
    Reference {
        is_mut: bool,
        pointee: Box<RTypeRef>,
    },
}

impl RTypeRef {
    pub fn is_self(&self) -> bool {
        match self {
            RTypeRef::SelfParam => true,
            RTypeRef::Reference { pointee, .. } => pointee.is_self(),
            _ => false,
        }
    }
}

impl std::fmt::Debug for RTypeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RTypeRef::*;
        match self {
            Builtin(tk) => write!(f, "{tk}"),
            SelfParam => write!(f, "self"),
            Ref(usr) | Typedef(usr) => write!(f, "{usr}"),
            Reference { is_mut, pointee } => {
                write!(f, "&{}{:?}", if *is_mut { "mut " } else { "" }, pointee)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use bbl_extract::{class::OverrideList, parse_string_and_extract_ast, AllowList};

    use super::translate_cpp_ast_to_rust;
    use crate::translate_cpp_ast_to_c;

    #[test]
    fn translate_rust_pass_class() -> bbl_util::Result<()> {
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

            let c_ast = translate_cpp_ast_to_c(&ast)?;
            let rast = translate_cpp_ast_to_rust(&ast, &c_ast)?;

            println!("{rast:?}");

            Ok(())
        })
    }
}
