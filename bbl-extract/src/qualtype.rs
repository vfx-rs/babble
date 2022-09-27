use bbl_clang::{
    cursor::{Cursor, USR},
    cursor_kind::CursorKind,
    translation_unit::TranslationUnit,
    ty::{Type, TypeKind},
};
use std::{convert::TryInto, fmt::Display};
use tracing::{debug, error, info, instrument, trace, warn};

use crate::{
    ast::AST,
    class::{extract_class_decl, specialize_template_parameter, ClassBindKind},
    error::Error,
    templates::{TemplateArgument, TemplateParameterDecl},
    typedef::extract_typedef_decl, AllowList,
};
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeRef {
    Builtin(TypeKind),
    Ref(USR),
    Pointer(Box<QualType>),
    LValueReference(Box<QualType>),
    RValueReference(Box<QualType>),
    TemplateTypeParameter(String),
    TemplateNonTypeParameter(String),
    Unknown(TypeKind),
}

impl TypeRef {
    pub fn is_builtin(&self) -> bool {
        use TypeRef::*;
        match self {
            Builtin(_) => true,
            Pointer(p) => p.type_ref.is_builtin(),
            LValueReference(p) => p.type_ref.is_builtin(),
            RValueReference(p) => p.type_ref.is_builtin(),
            _ => false,
        }
    }

    pub fn is_valuetype(&self, ast: &AST) -> Result<bool> {
        use TypeRef::*;
        let result = match self {
            Builtin(_) => true,
            Ref(usr) => {
                let class = ast
                    .get_class(*usr)
                    .ok_or(Error::ClassOrNamespaceNotFound(*usr))?;
                *class.bind_kind() == ClassBindKind::ValueType
            }
            Pointer(p) => p.type_ref.is_valuetype(ast)?,
            LValueReference(p) => p.type_ref.is_valuetype(ast)?,
            RValueReference(p) => p.type_ref.is_valuetype(ast)?,
            _ => false,
        };

        Ok(result)
    }

    pub fn get_bind_kind(&self, ast: &AST) -> Result<ClassBindKind> {
        use TypeRef::*;
        match self {
            Builtin(_) => Ok(ClassBindKind::ValueType),
            Ref(usr) => {
                if let Some(class) = ast.get_class(*usr) {
                    Ok(*class.bind_kind())
                } else if let Some(cts) = ast.get_class_template_specialization(*usr) {
                    cts.bind_kind(ast)
                } else if let Some(td) = ast.get_type_alias(*usr) {
                    todo!()
                } else {
                    Err(Error::ClassNotFound(usr.to_string()))
                }
            }
            Pointer(p) => p.type_ref.get_bind_kind(ast),
            LValueReference(p) => p.type_ref.get_bind_kind(ast),
            RValueReference(p) => p.type_ref.get_bind_kind(ast),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct QualType {
    pub name: String,
    pub is_const: bool,
    pub type_ref: TypeRef,
}

impl std::fmt::Debug for QualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.name,
            if self.is_const { " const" } else { "" }
        )
    }
}

impl QualType {
    pub fn unknown(tk: TypeKind) -> Self {
        QualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: TypeRef::Unknown(tk),
        }
    }

    pub fn get_bind_kind(&self, ast: &AST) -> Result<ClassBindKind> {
        self.type_ref.get_bind_kind(ast)
    }

    pub fn float() -> Self {
        QualType {
            name: "float".into(),
            is_const: false,
            type_ref: TypeRef::Builtin(TypeKind::Float),
        }
    }

    pub fn char(is_const: bool) -> Self {
        QualType {
            name: "char".into(),
            is_const,
            type_ref: TypeRef::Builtin(TypeKind::Char_S),
        }
    }

    pub fn void() -> Self {
        QualType {
            name: "void".into(),
            is_const: false,
            type_ref: TypeRef::Builtin(TypeKind::Void),
        }
    }

    pub fn lvalue_reference(name: &str, pointee: QualType) -> QualType {
        QualType {
            name: name.to_string(),
            is_const: true,
            type_ref: TypeRef::LValueReference(Box::new(pointee)),
        }
    }

    pub fn rvalue_reference(name: &str, pointee: QualType) -> QualType {
        QualType {
            name: name.to_string(),
            is_const: false,
            type_ref: TypeRef::LValueReference(Box::new(pointee)),
        }
    }

    pub fn pointer(name: &str, pointee: QualType) -> QualType {
        QualType {
            name: name.to_string(),
            is_const: false,
            type_ref: TypeRef::Pointer(Box::new(pointee)),
        }
    }

    pub fn type_ref(name: &str, is_const: bool, usr: USR) -> QualType {
        QualType {
            name: name.to_string(),
            is_const,
            type_ref: TypeRef::Ref(usr),
        }
    }

    pub fn underlying_decl_recursive(&self) -> Option<USR> {
        match self.type_ref {
            TypeRef::LValueReference(ref pointee)
            | TypeRef::Pointer(ref pointee)
            | TypeRef::RValueReference(ref pointee) => pointee.underlying_decl_recursive(),
            TypeRef::Ref(usr) => Some(usr),
            _ => None,
        }
    }

    pub fn is_valuetype(&self, ast: &AST) -> Result<bool> {
        match self.type_ref {
            TypeRef::Builtin(_)
            | TypeRef::Pointer(_)
            | TypeRef::LValueReference(_)
            | TypeRef::RValueReference(_) => Ok(true),
            TypeRef::Ref(usr) => Ok(matches!(
                ast.get_class(usr)
                    .ok_or_else(|| Error::ClassNotFound(usr.as_str().to_string()))?
                    .bind_kind(),
                ClassBindKind::ValueType
            )),
            _ => Ok(false),
        }
    }

    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[TemplateArgument]>,
    ) -> String {
        let result = String::new();

        let result = if self.is_const {
            format!("{result}const ")
        } else {
            result
        };

        let result = match &self.type_ref {
            TypeRef::Builtin(tk) => {
                format!("{result}{}", tk.spelling())
            }
            TypeRef::Pointer(pointee) => format!(
                "{result}{}*",
                pointee.format(ast, class_template_parameters, class_template_args)
            ),
            TypeRef::LValueReference(pointee) => format!(
                "{result}{}&",
                pointee.format(ast, class_template_parameters, class_template_args)
            ),
            TypeRef::RValueReference(pointee) => {
                format!(
                    "{result}{}&&",
                    pointee.format(ast, class_template_parameters, class_template_args)
                )
            }
            TypeRef::Ref(usr) => {
                let name = ast
                    .get_class(*usr)
                    .map(|r| r.format(ast, class_template_args))
                    .unwrap_or_else(|| usr.to_string());
                format!("{result}{}", name)
            }
            TypeRef::TemplateTypeParameter(t) | TypeRef::TemplateNonTypeParameter(t) => {
                format!(
                    "{result}{}",
                    specialize_template_type(t, class_template_parameters, class_template_args)
                )
            }
            TypeRef::Unknown(tk) => {
                format!("{result}UNKNOWN({})", tk.spelling())
            }
        };

        result
    }
}

/// Given a template parameter in type position for which we only know the name, try to look up the type to replace it
/// with
#[instrument(level = "trace")]
fn specialize_template_type(
    t: &str,
    class_template_parameters: &[TemplateParameterDecl],
    class_template_args: Option<&[TemplateArgument]>,
) -> String {
    // find `t` in the parameters, then use that to find the arg, if it exists
    for decl in class_template_parameters {
        if t == decl.name() {
            return specialize_template_parameter(decl, class_template_args);
        }
    }

    t.to_string()
}

impl Display for QualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        match &self.type_ref {
            TypeRef::Builtin(tk) => {
                write!(f, "{}", tk.spelling())
            }
            TypeRef::Pointer(pointee) => write!(f, "{}*", *pointee),
            TypeRef::LValueReference(pointee) => write!(f, "{}&", *pointee),
            TypeRef::RValueReference(pointee) => {
                write!(f, "{}&&", *pointee)
            }
            TypeRef::Ref(usr) => {
                write!(f, "{}", usr)
            }
            TypeRef::TemplateTypeParameter(t) => {
                write!(f, "{}", t)
            }
            TypeRef::TemplateNonTypeParameter(t) => {
                write!(f, "{}", t)
            }
            TypeRef::Unknown(tk) => {
                write!(f, "UNKNOWN({})", tk.spelling())
            }
        }
    }
}

/// Extract a qualified type from a clang Type
#[instrument(skip(already_visited, ast, tu), level = "trace")]
pub fn extract_type(
    ty: Type,
    template_parameters: &[String],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<QualType> {
    let is_const = ty.is_const_qualified();
    let name = if is_const {
        if let Some(s) = ty.spelling().strip_prefix("const ") {
            s.to_string()
        } else {
            ty.spelling()
        }
    } else {
        ty.spelling()
    };

    if ty.is_builtin() {
        trace!("got builtin {:?}", ty);
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Builtin(ty.kind()),
        })
    } else if let Ok(c_decl) = ty.type_declaration() {
        trace!(
            "type {name} has decl {spelling} {usr}",
            spelling = c_decl.spelling(),
            usr = c_decl.usr()
        );
        // extract here if we need to
        let u_ref = match c_decl.kind() {
            CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => {
                extract_typedef_decl(c_decl.try_into()?, already_visited, ast, tu, allow_list)?
            }
            CursorKind::ClassDecl => {
                extract_class_decl(c_decl.try_into()?, tu, ast, already_visited, allow_list)?
            }
            CursorKind::TypeRef => unimplemented!("Should extract class here?"),
            _ => unimplemented!("Unhandled type decl {:?}", c_decl),
        };

        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Ref(u_ref),
        })
    } else {
        match ty.kind() {
            TypeKind::Pointer => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters, already_visited, ast, tu, allow_list)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::Pointer(Box::new(ty_ref)),
                })
            }
            TypeKind::LValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters, already_visited, ast, tu, allow_list)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::LValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::RValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters, already_visited, ast, tu, allow_list)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::RValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::Unexposed => {
                if template_parameters.contains(&name) {
                    Ok(QualType {
                        name: name.clone(),
                        is_const,
                        type_ref: TypeRef::TemplateTypeParameter(name),
                    })
                } else {
                    error!(
                        "Got unexposed for {name} with no matching template parmaeter in {:?}",
                        template_parameters
                    );
                    Err(Error::NoMatchingTemplateParameter(name))
                }
            }
            _ => {
                error!("Unhandled {:?}", ty);
                Ok(QualType::unknown(ty.kind()))
            }
        }
    }
}
