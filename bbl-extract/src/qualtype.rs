use backtrace::Backtrace;
use bbl_clang::{
    cursor::{CurTypedef, Cursor, USR},
    cursor_kind::CursorKind,
    translation_unit::TranslationUnit,
    ty::{Type, TypeKind},
};
use std::{convert::TryInto, fmt::Display};
use tracing::{debug, error, info, instrument, trace, warn};

use crate::{
    ast::AST,
    class::{extract_class_decl, specialize_template_parameter, ClassBindKind},
    enm::extract_enum,
    error::Error,
    templates::{TemplateArgument, TemplateParameterDecl},
    typedef::extract_typedef_decl,
    AllowList,
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
    FunctionProto {
        result: Box<QualType>,
        args: Vec<QualType>,
    },
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
                let class = ast.get_class(*usr).ok_or(Error::ClassOrNamespaceNotFound {
                    usr: *usr,
                    backtrace: Backtrace::new(),
                })?;
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
                } else if let Some(proto) = ast.get_function_proto(*usr) {
                    Ok(ClassBindKind::ValueType)
                } else if let Some(td) = ast.get_type_alias(*usr) {
                    todo!()
                } else {
                    Err(Error::ClassNotFound {
                        name: usr.to_string(),
                        backtrace: Backtrace::new(),
                    })
                }
            }
            Pointer(p) => p.type_ref.get_bind_kind(ast),
            LValueReference(p) => p.type_ref.get_bind_kind(ast),
            RValueReference(p) => p.type_ref.get_bind_kind(ast),
            FunctionProto { .. } => Ok(ClassBindKind::ValueType),
            _ => unreachable!(),
        }
    }

    pub fn refers_to_std_function(&self, ast: &AST) -> bool {
        match self {
            TypeRef::Builtin(_)
            | TypeRef::TemplateNonTypeParameter(_)
            | TypeRef::TemplateTypeParameter(_)
            | TypeRef::Unknown(_) => false,
            TypeRef::LValueReference(p) | TypeRef::Pointer(p) | TypeRef::RValueReference(p) => {
                p.type_ref.refers_to_std_function(ast)
            }
            TypeRef::Ref(usr) => {
                if let Some(proto) = ast.get_function_proto(*usr) {
                    proto.name().starts_with("function_")
                } else if let Some(td) = ast.get_type_alias(*usr) {
                    td.underlying_type().type_ref.refers_to_std_function(ast)
                } else {
                    false
                }
            }
            TypeRef::FunctionProto { .. } => false,
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

    pub fn builtin(kind: TypeKind, is_const: bool) -> Self {
        QualType {
            name: kind.spelling(),
            is_const,
            type_ref: TypeRef::Builtin(kind),
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

    pub fn template_parameter(name: &str, parm: &str, is_const: bool) -> QualType {
        QualType {
            name: name.to_string(),
            is_const,
            type_ref: TypeRef::TemplateTypeParameter(parm.to_string()),
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
                    .ok_or_else(|| Error::ClassNotFound {
                        name: usr.as_str().to_string(),
                        backtrace: Backtrace::new()
                    })?
                    .bind_kind(),
                ClassBindKind::ValueType
            )),
            _ => Ok(false),
        }
    }

    pub fn is_template(&self) -> bool {
        match &self.type_ref {
            TypeRef::TemplateNonTypeParameter(_) | TypeRef::TemplateTypeParameter(_) => true,
            TypeRef::Builtin(_)
            | TypeRef::Ref(_)
            | TypeRef::Unknown(_)
            | TypeRef::FunctionProto { .. } => false,
            TypeRef::Pointer(p) | TypeRef::LValueReference(p) | TypeRef::RValueReference(p) => {
                p.is_template()
            }
        }
    }

    pub fn template_parameter_name(&self) -> Option<&str> {
        match &self.type_ref {
            TypeRef::TemplateNonTypeParameter(name) | TypeRef::TemplateTypeParameter(name) => {
                Some(name.as_str())
            }
            TypeRef::Builtin(_)
            | TypeRef::Ref(_)
            | TypeRef::Unknown(_)
            | TypeRef::FunctionProto { .. } => None,
            TypeRef::Pointer(p) | TypeRef::LValueReference(p) | TypeRef::RValueReference(p) => {
                p.template_parameter_name()
            }
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
                format!("{result}{}", builtin_spelling(tk))
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
            TypeRef::FunctionProto { result, args } => {
                format!(
                    "{}(*)({})",
                    result.format(ast, class_template_parameters, class_template_args),
                    args.iter()
                        .map(|a| a.format(ast, class_template_parameters, class_template_args))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeRef::Unknown(tk) => {
                format!("{result}UNKNOWN({})", tk.spelling())
            }
        };

        result
    }
}

fn builtin_spelling(tk: &TypeKind) -> String {
    match tk {
        TypeKind::Bool => "bool".to_string(),
        TypeKind::Char_S => "char".to_string(),
        TypeKind::Char_U => "unsigned char".to_string(),
        TypeKind::Double => "double".to_string(),
        TypeKind::Float => "float".to_string(),
        TypeKind::Int => "int".to_string(),
        TypeKind::Long => "long".to_string(),
        TypeKind::LongDouble => "long double".to_string(),
        TypeKind::LongLong => "long long".to_string(),
        TypeKind::Short => "short".to_string(),
        TypeKind::UChar => "unsigned char".to_string(),
        TypeKind::UInt => "unsigned int".to_string(),
        TypeKind::ULong => "unsigned long".to_string(),
        TypeKind::ULongLong => "unsigned long long".to_string(),
        TypeKind::UShort => "unsigned short".to_string(),
        TypeKind::Void => "void".to_string(),
        _ => unimplemented!(),
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
            TypeRef::FunctionProto { result, args } => {
                write!(f, "{}(*)(", *result)?;
                for a in args {
                    write!(f, "{}, ", a)?;
                }
                write!(f, ")")
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
    debug!("extract_type {ty:?} {template_parameters:?}");
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
        debug!(
            "type {name} has decl {spelling} {usr}",
            spelling = c_decl.spelling(),
            usr = c_decl.usr()
        );

        // special-case extract std::function as its function prototype
        if c_decl.display_name().starts_with("function<") {
            let ty = c_decl.template_argument_type(0)?;
            if ty.kind() != TypeKind::FunctionProto {
                panic!(
                    "Got type kind {:?} instead of FunctionProto for {c_decl:?}",
                    ty.kind()
                );
            }

            extract_std_function_as_pointer(ty, ast, already_visited, tu, allow_list)
        } else {
            // extract underlying decl here
            let u_ref = match c_decl.kind() {
                CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => {
                    debug!("is typedef decl");
                    extract_typedef_decl(c_decl.try_into()?, already_visited, ast, tu, allow_list, template_parameters)?
                }
                CursorKind::ClassDecl | CursorKind::StructDecl => {
                    debug!("is class decl");
                    extract_class_decl(c_decl.try_into()?, tu, ast, already_visited, allow_list)?
                }
                CursorKind::TypeRef => unimplemented!("Should extract class here?"),
                CursorKind::EnumDecl => extract_enum(c_decl, ast, already_visited, tu)?,
                _ => unimplemented!("Unhandled type decl {:?}", c_decl),
            };

            Ok(QualType {
                name,
                is_const,
                type_ref: TypeRef::Ref(u_ref),
            })
        }
    } else {
        match ty.kind() {
            TypeKind::Pointer => {
                let pointee = ty.pointee_type()?;
                let ty_ref = extract_type(
                    pointee,
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                    allow_list,
                )?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::Pointer(Box::new(ty_ref)),
                })
            }
            TypeKind::LValueReference => {
                let pointee = ty.pointee_type()?;
                if let Ok(c_decl) = pointee.type_declaration() {
                    if matches!(
                        c_decl.kind(),
                        CursorKind::TypeAliasDecl | CursorKind::TypedefDecl
                    ) {
                        let c_decl: CurTypedef = c_decl.try_into()?;
                        let ty = c_decl.underlying_type()?;
                        if ty.spelling().starts_with("std::function<") {
                            // Force-extract the function as a function pointer here, lofting it out of the reference
                            // as we want to promote a `std::function<> const&` to a value pass of a function pointer
                            // and let C++ automatically construct the target std::function
                            return extract_type(
                                pointee,
                                template_parameters,
                                already_visited,
                                ast,
                                tu,
                                allow_list,
                            );
                        }
                    }
                }

                let ty_ref = extract_type(
                    pointee,
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                    allow_list,
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
                    template_parameters,
                    already_visited,
                    ast,
                    tu,
                    allow_list,
                )?;
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
                    println!("NARGS: {}", ty.num_template_arguments());
                    Err(Error::NoMatchingTemplateParameter {
                        name,
                        backtrace: Backtrace::new(),
                    })
                }
            }
            TypeKind::FunctionProto => {
                extract_function_pointer(ty, ast, already_visited, tu, allow_list)
            }
            _ => {
                error!("Unhandled {:?}", ty);
                error!("{:?}", backtrace::Backtrace::new());
                Ok(QualType::unknown(ty.kind()))
            }
        }
    }
}

pub fn extract_function_pointer(
    ty: Type,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<QualType> {
    if ty.kind() != TypeKind::FunctionProto {
        panic!(
            "Got type kind {:?} instead of FunctionProto in extract_function_proto for {}",
            ty.kind(),
            ty.spelling(),
        );
    }

    let name = ty.spelling();

    let result = extract_type(ty.result_type()?, &[], already_visited, ast, tu, allow_list)?;
    let num_args = ty.num_arg_types()?;
    let mut args = Vec::new();
    for i in 0..num_args {
        args.push(extract_type(
            ty.arg_type(i)?,
            &[],
            already_visited,
            ast,
            tu,
            allow_list,
        )?);
    }

    Ok(QualType {
        name,
        is_const: false,
        type_ref: TypeRef::FunctionProto {
            result: Box::new(result),
            args,
        },
    })
}


/// Extract a std::function as a function pointer
pub fn extract_std_function_as_pointer(
    ty: Type,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<QualType> {
    if ty.kind() != TypeKind::FunctionProto {
        panic!(
            "Got type kind {:?} instead of FunctionProto in extract_function_proto for {}",
            ty.kind(),
            ty.spelling(),
        );
    }

    let name = ty.spelling();

    let result = extract_type(ty.result_type()?, &[], already_visited, ast, tu, allow_list)?;
    let num_args = ty.num_arg_types()?;
    let mut args = Vec::new();
    for i in 0..num_args {
        args.push(extract_type(
            ty.arg_type(i)?,
            &[],
            already_visited,
            ast,
            tu,
            allow_list,
        )?);
    }

    let proto = QualType {
        name: name.clone(),
        is_const: false,
        type_ref: TypeRef::FunctionProto {
            result: Box::new(result),
            args,
        },
    };

    Ok(QualType::pointer(&name, proto))
}
