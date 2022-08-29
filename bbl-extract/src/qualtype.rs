use bbl_clang::{
    cursor::{Cursor, USR},
    cursor_kind::CursorKind,
    ty::{Type, TypeKind},
};
use log::*;
use std::fmt::Display;

use crate::{
    ast::AST,
    class::{specialize_template_parameter, ClassBindKind},
    error::Error,
    template_argument::{TemplateParameterDecl, TemplateType},
};
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone)]
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
                let class = ast.get_class(*usr).ok_or(Error::ClassOrNamespaceNotFound(*usr))?;
                *class.bind_kind() == ClassBindKind::ValueType
            }
            Pointer(p) => p.type_ref.is_valuetype(ast)?,
            LValueReference(p) => p.type_ref.is_valuetype(ast)?,
            RValueReference(p) => p.type_ref.is_valuetype(ast)?,
            _ => false,
        };

        Ok(result)
    }
}

#[derive(Clone)]
pub struct QualType {
    pub name: String,
    pub is_const: bool,
    pub type_ref: TypeRef,
}

impl QualType {
    pub fn unknown(tk: TypeKind) -> Self {
        QualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: TypeRef::Unknown(tk),
        }
    }

    pub fn float() -> Self {
        QualType {
            name: "float".into(),
            is_const: false,
            type_ref: TypeRef::Builtin(TypeKind::Float),
        }
    }

    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
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
                    .unwrap_or(usr.to_string());
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
fn specialize_template_type(
    t: &str,
    class_template_parameters: &[TemplateParameterDecl],
    class_template_args: Option<&[Option<TemplateType>]>,
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

/// Get a qualified type from a reference to a type
pub fn extract_type_from_typeref(c_tr: Cursor) -> Result<QualType> {
    if let Ok(c_ref) = c_tr.referenced() {
        let c_ref =
            if c_ref.kind() == CursorKind::ClassDecl || c_ref.kind() == CursorKind::ClassTemplate {
                c_ref.canonical()?
            } else {
                c_ref
            };

        let is_const = if let Ok(ty) = c_tr.ty() {
            ty.is_const_qualified()
        } else {
            false
        };

        match c_ref.kind() {
            CursorKind::ClassDecl | CursorKind::ClassTemplate => {
                let c_ref = c_ref.canonical()?;
                Ok(QualType {
                    name: c_ref.spelling(),
                    is_const,
                    type_ref: TypeRef::Ref(c_ref.usr()),
                })
            }
            CursorKind::TemplateTypeParameter => Ok(QualType {
                name: c_ref.spelling(),
                is_const,
                type_ref: TypeRef::TemplateTypeParameter(c_ref.spelling()),
            }),
            _ => {
                error!("unhandled type {}", c_tr.display_name());
                Ok(QualType::unknown(c_ref.ty()?.kind()))
            }
        }
    } else {
        error!(
            "could not get referenced type from TypeRef {}",
            c_tr.display_name()
        );
        Ok(QualType::unknown(c_tr.ty()?.kind()))
    }
}

/// Extract a qualified type from a clang Type
pub fn extract_type(ty: Type, template_parameters: &[String]) -> Result<QualType> {
    let is_const = ty.is_const_qualified();
    let name = ty.spelling();

    if ty.is_builtin() {
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Builtin(ty.kind()),
        })
    } else if let Ok(c_ref) = ty.type_declaration() {
        debug!("type {name} has decl {} {}", c_ref.spelling(), c_ref.usr());
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Ref(c_ref.usr()),
        })
    } else {
        match ty.kind() {
            TypeKind::Pointer => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::Pointer(Box::new(ty_ref)),
                })
            }
            TypeKind::LValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::LValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::RValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::RValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::Unexposed => {
                let name = if is_const {
                    name.strip_prefix("const ").unwrap().to_string()
                } else {
                    name
                };
                if template_parameters.contains(&name) {
                    Ok(QualType {
                        name: name.clone(),
                        is_const,
                        type_ref: TypeRef::TemplateTypeParameter(name.clone()),
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
