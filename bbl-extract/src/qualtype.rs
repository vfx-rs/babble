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
    template_argument::{TemplateParameterDecl, TemplateType},
    type_alias::extract_typedef_decl,
};
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
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
}

#[derive(Clone)]
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
#[instrument(skip(depth), level = "trace")]
pub fn extract_type_from_typeref(c_tr: Cursor, depth: usize) -> Result<QualType> {
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
            CursorKind::TypedefDecl => Ok(QualType {
                name: c_ref.spelling(),
                is_const,
                type_ref: TypeRef::Ref(c_ref.usr()),
            }),
            _ => {
                let loc = c_tr.location().spelling_location();
                error!(
                    "{:width$}[{file}:{line}] unhandled type {c_tr:?} is a {c_ref:?}",
                    "",
                    width = depth * 2,
                    file = loc.file.file_name(),
                    line = loc.line,
                    c_tr = c_tr,
                    c_ref = c_ref
                );
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
#[instrument(skip(depth, already_visited, ast, tu), level = "trace")]
pub fn extract_type(
    ty: Type,
    depth: usize,
    template_parameters: &[String],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
) -> Result<QualType> {
    let is_const = ty.is_const_qualified();
    let name = ty.spelling();

    if ty.is_builtin() {
        trace!("got builtin {:?}", ty);
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Builtin(ty.kind()),
        })
    } else if let Ok(c_ref) = ty.type_declaration() {
        trace!(
            "{:width$}type {name} has decl {spelling} {usr}",
            "",
            width = 2 * depth,
            spelling = c_ref.spelling(),
            usr = c_ref.usr()
        );
        // extract here if we need to
        match c_ref.kind() {
            CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => {
                extract_typedef_decl(c_ref.try_into()?, depth + 1, already_visited, ast, tu)?;
            }
            CursorKind::ClassDecl => {
                if !already_visited.contains(&c_ref.usr()) {
                    already_visited.push(c_ref.usr());
                    let cd = extract_class_decl(
                        c_ref,
                        depth + 1,
                        tu,
                        &Vec::new(),
                        ast,
                        already_visited,
                    )?;
                    ast.insert_class(cd);
                }
            }
            CursorKind::TypeRef => warn!("Should extract class here"),
            _ => warn!("Unhandled type decl {:?}", c_ref),
        }

        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Ref(c_ref.usr()),
        })
    } else {
        match ty.kind() {
            TypeKind::Pointer => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(
                    pointee,
                    depth + 1,
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                )?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::Pointer(Box::new(ty_ref)),
                })
            }
            TypeKind::LValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(
                    pointee,
                    depth + 1,
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                )?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::LValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::RValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(
                    pointee,
                    depth + 1,
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                )?;
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
