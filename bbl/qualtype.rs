use log::*;
use std::fmt::Display;

use crate::{error::Error, ty::{TypeKind, Type}, cursor::USR, ast::AST, Cursor, cursor_kind::CursorKind};
type Result<T, E = Error> = std::result::Result<T, E>;

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

pub struct QualType {
    name: String,
    is_const: bool,
    type_ref: TypeRef,
}

impl QualType {
    pub fn unknown(tk: TypeKind) -> Self {
        QualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: TypeRef::Unknown(tk),
        }
    }

    pub fn format(&self, ast: &AST) -> String {
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
            TypeRef::Pointer(pointee) => format!("{result}{}*", pointee.format(ast)),
            TypeRef::LValueReference(pointee) => format!("{result}{}&", pointee.format(ast)),
            TypeRef::RValueReference(pointee) => {
                format!("{result}{}&&", pointee.format(ast))
            }
            TypeRef::Ref(usr) => {
                let name = ast
                    .records
                    .get(usr)
                    .map(|r| format!("{}", r.name()))
                    .unwrap_or(usr.0.clone());
                format!("{result}{}", name)
            }
            TypeRef::TemplateTypeParameter(t) => {
                format!("{result}{}", t)
            }
            TypeRef::TemplateNonTypeParameter(t) => {
                format!("{result}{}", t)
            }
            TypeRef::Unknown(tk) => {
                format!("{result}UNKNOWN({})", tk.spelling())
            }
        };

        result
    }
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

pub fn qualtype_from_typeref(c_tr: Cursor) -> Result<QualType> {
    if let Ok(c_ref) = c_tr.referenced() {
        let c_ref =
            if c_ref.kind() == CursorKind::ClassDecl || c_ref.kind() == CursorKind::ClassTemplate {
                c_ref.canonical().unwrap()
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
                let c_ref = c_ref.canonical().unwrap();
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
                Ok(QualType::unknown(c_ref.ty().unwrap().kind()))
            }
        }
    } else {
        error!(
            "could not get referenced type from TypeRef {}",
            c_tr.display_name()
        );
        Ok(QualType::unknown(c_tr.ty().unwrap().kind()))
    }
}

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
                    Err(Error::TypeUnexposed)
                }
            }
            _ => {
                error!("Unhandled {:?}", ty);
                Ok(QualType::unknown(ty.kind()))
            }
        }
    }
}
