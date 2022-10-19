use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    ast::AST,
    class::ClassDecl,
    function::Method,
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::{QualType, TypeRef},
};
use tracing::warn;

use crate::{error::Error, sanitize_name, CAST};
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

pub fn translate_cpp_ast_to_rust(ast: &AST, c_ast: &CAST) -> Result<RAST> {
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
                warn!("Method {} is templated but has no specializations and so will be ignored", method.name());
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

    for arg in method.arguments() {
        arguments.push(RArgument {
            name: arg.name().to_string(),
            ty: translate_type(arg.qual_type(), ast).map_err(|e| Error::TranslateArgument {
                name: arg.name().to_string(),
                source: Box::new(e),
            })?,
        })
    }

    Ok(RMethod {
        name,
        usr,
        result,
        arguments,
    })
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

    use crate::translate_cpp_ast_to_c;
    use super::translate_cpp_ast_to_rust;

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

            let c_ast = translate_cpp_ast_to_c(&ast)?;
            let rast = translate_cpp_ast_to_rust(&ast, &c_ast)?;

            println!("{rast:?}");

            Ok(())
        })
    }
}
