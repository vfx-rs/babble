use std::borrow::Cow;

use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    ast::{MonoAST, AST},
    class::ClassDecl,
    function::{self, Method},
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::{QualType, TypeRef},
};
use hashbrown::HashSet;
use tracing::warn;

use crate::{error::Error, CAST};
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
            .ok_or(Error::FailedToGetClassFromRef(usr))
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

        methods.push(
            translate_method(method, ast, c_ast).map_err(|e| Error::TranslateMethod {
                name: method.name().to_string(),
                source: Box::new(e),
            })?,
        );
    }

    Ok(RStruct {
        name: st_name.to_string(),
        usr,
        methods,
    })
}

fn translate_method(method: &Method, ast: &AST, c_ast: &CAST) -> Result<RMethod> {
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

    let has_result = matches!(method.result().type_ref, TypeRef::Builtin(TypeKind::Void));

    let mut c_arg_offset = 0;
    if !method.is_static() {
        c_arg_offset += 1;
    }
    if has_result {
        c_arg_offset += 1
    }

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
        .ok_or(Error::FunctionNotFound {
            name: method.usr().to_string(),
            backtrace: backtrace::Backtrace::new(),
        })?;

    let mut body = Vec::new();
    let function_args = Vec::new();

    // now generate the call expressions
    for arg in &arguments {}

    let call_expr = Expr::FunctionCall {
        name: format!("ffi::{}", c_fn.name_external),
        args: function_args,
    };

    let return_code_expr = Expr::Let {
        name: "_return_code".to_string(),
        value: Box::new(call_expr),
        is_mut: false,
    };

    body.push(
        Expr::Unsafe(
            Box::new(
                Expr::Block(
                    vec![
                        return_code_expr
                    ]
                )
            )
        )
    );

    Ok(RMethod {
        name,
        usr,
        result,
        arguments,
        body: Expr::Block(body),
    })
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
            )?;

            let ast = ast.monomorphize()?;

            let c_ast = translate_cpp_ast_to_c(&ast)?;
            let rast = translate_cpp_ast_to_rust(&ast, &c_ast)?;

            println!("{rast:?}");

            Ok(())
        })
    }
}
