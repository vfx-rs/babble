use bbl_clang::{
    cursor::{CurTypedef, Cursor, USR},
    cursor_kind::CursorKind,
    translation_unit::TranslationUnit,
    ty::{Type, TypeKind},
};
use bbl_util::Trace;
use hashbrown::HashSet;
use std::{convert::TryInto, fmt::Display};
use tracing::{debug, error, instrument, trace, warn};

use crate::{
    ast::{dump_cursor, dump_cursor_until, AST},
    class::{self, extract_class_decl, specialize_template_parameter, ClassBindKind, OverrideList},
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
    Typedef(USR),
    Pointer(Box<QualType>),
    LValueReference(Box<QualType>),
    RValueReference(Box<QualType>),
    TemplateTypeParameter(String),
    TemplateNonTypeParameter(String),
    FunctionProto {
        result: Box<QualType>,
        args: Vec<QualType>,
    },
}

impl TypeRef {
    /// Returns `true` if type (or the type pointed to if this is a pointer or reference) is a builtin
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

    /// Returns `true` if type (or the type pointed to if this is a pointer or reference) is bound as a value type
    pub fn is_valuetype(&self, ast: &AST) -> Result<bool> {
        use TypeRef::*;
        let result = match self {
            Builtin(_) => true,
            Ref(usr) => {
                let class = ast
                    .get_class(*usr)
                    .ok_or_else(|| Error::ClassOrNamespaceNotFound {
                        usr: *usr,
                        source: Trace::new(),
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

    /// Get the bind kind for this type (or the type pointed to if this is a pointer or reference)
    pub fn get_bind_kind(&self, ast: &AST) -> Result<ClassBindKind> {
        use TypeRef::*;
        match self {
            Builtin(_) => Ok(ClassBindKind::ValueType),
            Ref(usr) => {
                if let Some(class) = ast.get_class(*usr) {
                    Ok(*class.bind_kind())
                } else if let Some(cts) = ast.get_class_template_specialization(*usr) {
                    cts.bind_kind(ast)
                } else if ast.get_function_proto(*usr).is_some() {
                    Ok(ClassBindKind::ValueType)
                } else if let Some(td) = ast.get_type_alias(*usr) {
                    td.underlying_type().type_ref.get_bind_kind(ast)
                } else {
                    Err(Error::ClassNotFound {
                        name: usr.to_string(),
                        source: Trace::new(),
                    })
                }
            }
            Typedef(usr) => {
                if let Some(class) = ast.get_class(*usr) {
                    Ok(*class.bind_kind())
                } else if let Some(cts) = ast.get_class_template_specialization(*usr) {
                    cts.bind_kind(ast)
                } else if ast.get_function_proto(*usr).is_some() {
                    Ok(ClassBindKind::ValueType)
                } else if let Some(td) = ast.get_type_alias(*usr) {
                    td.underlying_type().type_ref.get_bind_kind(ast)
                } else {
                    Err(Error::ClassNotFound {
                        name: usr.to_string(),
                        source: Trace::new(),
                    })
                }
            }
            Pointer(p) => p.type_ref.get_bind_kind(ast),
            LValueReference(p) => p.type_ref.get_bind_kind(ast),
            RValueReference(p) => p.type_ref.get_bind_kind(ast),
            FunctionProto { .. } => Ok(ClassBindKind::ValueType),
            _ => unreachable!("cannot get bind kind for TypeRef {self:?}"),
        }
    }

    /// Does this type refer (or point to) a std::function?
    pub fn refers_to_std_function(&self, ast: &AST) -> bool {
        match self {
            TypeRef::Builtin(_)
            | TypeRef::TemplateNonTypeParameter(_)
            | TypeRef::TemplateTypeParameter(_) => false,
            TypeRef::LValueReference(p) | TypeRef::Pointer(p) | TypeRef::RValueReference(p) => {
                p.type_ref.refers_to_std_function(ast)
            }
            TypeRef::Typedef(usr) => {
                if let Some(td) = ast.get_type_alias(*usr) {
                    td.underlying_type().type_ref.refers_to_std_function(ast)
                } else {
                    false
                }
            }
            TypeRef::Ref(usr) => {
                if let Some(proto) = ast.get_function_proto(*usr) {
                    proto.name().starts_with("function_")
                } else {
                    false
                }
            }
            TypeRef::FunctionProto { .. } => false,
        }
    }

    /// Is the type (or the type that it points to) templated?
    pub fn is_template(&self, ast: &AST) -> bool {
        match self {
            TypeRef::TemplateTypeParameter(_) => true,
            TypeRef::LValueReference(p) | TypeRef::RValueReference(p) | TypeRef::Pointer(p) => {
                p.is_template(ast)
            }
            TypeRef::Typedef(usr) => ast
                .get_type_alias(*usr)
                .unwrap()
                .underlying_type()
                .is_template(ast),
            _ => false,
        }
    }
}

/// Represents a reference to a (maybe) const-qualified type in the AST.
///
/// Forms a tree of references to represent typedefs, pointer chains etc.
#[derive(Clone, PartialEq, Eq)]
pub struct QualType {
    /// Used for display purposes only
    pub name: String,
    /// Is the type const?
    pub is_const: bool,
    /// The referenced type
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
    pub fn is_template(&self, ast: &AST) -> bool {
        self.type_ref.is_template(ast)
    }

    /// Expand any `template_parameters` in this QualType with the matching `template_arguments`
    pub fn replace_templates(
        &mut self,
        template_parameters: &[TemplateParameterDecl],
        template_arguments: &[TemplateArgument],
        matched_parameters: &mut HashSet<String>,
        ast: &AST,
    ) -> Result<bool> {
        use TypeRef::*;
        match &mut self.type_ref {
            TemplateTypeParameter(parm_name) => {
                let mut parm_index = None;
                for parm in template_parameters {
                    match parm {
                        TemplateParameterDecl::Type { name, index } => {
                            if name == parm_name {
                                matched_parameters.insert(name.clone());
                                parm_index = Some(*index)
                            }
                        }
                        TemplateParameterDecl::Integer { .. } => (),
                    }
                }

                // It's not an error to not find the parm index. In the case of a tempated method on a templated class,
                // we'll replace the templates in two phases (once by specializing the method, and the other the class)
                if let Some(parm_index) = parm_index {
                    match &template_arguments[parm_index] {
                        TemplateArgument::Type(qt) => {
                            let is_const = self.is_const || qt.is_const;
                            *self = qt.clone();
                            self.is_const = is_const;
                            return Ok(true);
                        }
                        TemplateArgument::Null => todo!(),
                        TemplateArgument::Declaration => todo!(),
                        TemplateArgument::NullPtr => todo!(),
                        TemplateArgument::Integral(_) => todo!(),
                        TemplateArgument::Template => todo!(),
                        TemplateArgument::TemplateExpansion => todo!(),
                        TemplateArgument::Expression => todo!(),
                        TemplateArgument::Pack => todo!(),
                    }
                }

                Ok(false)
            }
            Builtin(_) => Ok(false),
            Ref(usr) => {
                if let Some(class) = ast.get_class(*usr) {
                    if class.is_templated() {
                        // find the specialization that matches the given template arguments
                        if let Some(usr_spec) = class.get_specialization(template_arguments) {
                            self.type_ref = TypeRef::Ref(usr_spec);
                            self.name = format!(
                                "{}{}",
                                ast.get_class_template_specialization(usr_spec)
                                    .unwrap()
                                    .name(),
                                if self.is_const { " const" } else { "" }
                            );

                            Ok(true)
                        } else {
                            // TODO(AL): Is there any way of doing this immediately here? Can't modify the AST because
                            // we're deep in a loop over it
                            Err(Error::ClassTemplateSpecializationNotFound {
                                usr: *usr,
                                template_arguments: template_arguments.to_vec(),
                            })
                        }
                    } else {
                        Ok(false)
                    }
                } else {
                    todo!("Handle type for {usr}")
                }
            }
            Typedef(usr) => {
                let td = ast
                    .get_type_alias(*usr)
                    .ok_or_else(|| Error::TypedefNotFound {
                        usr: *usr,
                        source: Trace::new(),
                    })?;
                if td.underlying_type().is_template(ast) {
                    let mut qt = td.underlying_type().clone();
                    qt.replace_templates(
                        template_parameters,
                        template_arguments,
                        matched_parameters,
                        ast,
                    )?;
                    let is_const = qt.is_const || self.is_const;
                    *self = qt;
                    self.is_const = is_const;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Pointer(p) => {
                if p.replace_templates(
                    template_parameters,
                    template_arguments,
                    matched_parameters,
                    ast,
                )? {
                    // need to rename the type for display purposes
                    let name = format!("{}*{}", p.name, if self.is_const { " const" } else { "" });
                    self.name = name;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            LValueReference(p) => {
                if p.replace_templates(
                    template_parameters,
                    template_arguments,
                    matched_parameters,
                    ast,
                )? {
                    // need to rename the type for display purposes
                    let name = format!("{}&", p.name);
                    self.name = name;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            RValueReference(p) => {
                if p.replace_templates(
                    template_parameters,
                    template_arguments,
                    matched_parameters,
                    ast,
                )? {
                    // need to rename the type for display purposes
                    let name = format!("{}&&", p.name);
                    self.name = name;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            TemplateNonTypeParameter(_) => todo!(),
            FunctionProto { .. } => todo!(),
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
                        source: Trace::new(),
                    })?
                    .bind_kind(),
                ClassBindKind::ValueType
            )),
            _ => Ok(false),
        }
    }

    pub fn template_parameter_name(&self) -> Option<&str> {
        match &self.type_ref {
            TypeRef::TemplateNonTypeParameter(name) | TypeRef::TemplateTypeParameter(name) => {
                Some(name.as_str())
            }
            TypeRef::Builtin(_)
            | TypeRef::Ref(_)
            | TypeRef::Typedef(_)
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
            TypeRef::Typedef(usr) => {
                let name = ast
                    .get_type_alias(*usr)
                    .map(|_| "TYPEDEF".to_string())
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
            TypeRef::Typedef(usr) => {
                write!(f, "Typedef({})", usr)
            }
        }
    }
}

/// Extract a qualified type from a clang Type
#[instrument(skip(already_visited, ast, tu), level = "trace")]
pub fn extract_type(
    ty: Type,
    template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
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

    if name.contains("std::enable_if") {
        return Err(Error::Unsupported {
            description: "std::enable_if is unsupported".to_string(),
            source: Trace::new(),
        });
    }

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

            extract_std_function_as_pointer(
                ty,
                ast,
                already_visited,
                tu,
                allow_list,
                class_overrides,
                stop_on_error,
            )
        } else {
            // extract underlying decl here
            match c_decl.kind() {
                CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => {
                    debug!("    is typedef decl");
                    // TODO(AL): preserve stddef types here (size_t, uint32_t etc)
                    let is_const = ty.is_const_qualified();
                    let td: CurTypedef = c_decl.try_into()?;
                    let underlying_type = td.underlying_type()?;
                    let mut qt = extract_type(
                        underlying_type,
                        template_parameters,
                        already_visited,
                        ast,
                        tu,
                        allow_list,
                        class_overrides,
                        stop_on_error,
                    )?;
                    qt.is_const = qt.is_const || is_const;
                    Ok(qt)
                }
                CursorKind::ClassDecl | CursorKind::StructDecl => {
                    debug!("    is class decl");
                    let u_ref = extract_class_decl(
                        c_decl.try_into()?,
                        tu,
                        ast,
                        already_visited,
                        allow_list,
                        class_overrides,
                        None,
                        false,
                        stop_on_error,
                    )
                    .map_err(|e| Error::FailedToExtractClass {
                        usr: c_decl.usr(),
                        source: Box::new(e),
                    })?;

                    Ok(QualType {
                        name,
                        is_const,
                        type_ref: TypeRef::Ref(u_ref),
                    })
                }
                CursorKind::EnumDecl => {
                    debug!("    is enum decl");
                    let u_ref = extract_enum(c_decl, ast, already_visited, tu).map_err(|e| {
                        Error::FailedToExtractEnum {
                            usr: c_decl.usr(),
                            source: Box::new(e),
                        }
                    })?;
                    Ok(QualType {
                        name,
                        is_const,
                        type_ref: TypeRef::Ref(u_ref),
                    })
                }
                CursorKind::TypeRef => unimplemented!("Should extract class here?"),
                CursorKind::ClassTemplate => {
                    debug!("    is class template");
                    let usr = extract_class_decl(
                        c_decl.try_into()?,
                        tu,
                        ast,
                        already_visited,
                        allow_list,
                        class_overrides,
                        None,
                        false,
                        stop_on_error,
                    )?;

                    Ok(QualType {
                        name,
                        is_const,
                        type_ref: TypeRef::Ref(usr),
                    })
                }
                _ => {
                    dump_cursor_until(c_decl, tu, 2, true);
                    Err(Error::Unsupported {
                        description: format!(
                            "Unsupported declaration while extracting type {c_decl:?}"
                        ),
                        source: Trace::new(),
                    })
                }
            }
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
                    class_overrides,
                    stop_on_error,
                )
                .map_err(|e| Error::FailedToExtractType {
                    name: pointee.spelling(),
                    source: Box::new(e),
                })?;
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
                                class_overrides,
                                stop_on_error,
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
                    class_overrides,
                    stop_on_error,
                )
                .map_err(|e| Error::FailedToExtractType {
                    name: pointee.spelling(),
                    source: Box::new(e),
                })?;

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
                    class_overrides,
                    stop_on_error,
                )
                .map_err(|e| Error::FailedToExtractType {
                    name: pointee.spelling(),
                    source: Box::new(e),
                })?;

                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::RValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::Unexposed => {
                if template_parameters.iter().any(|p| p.name() == name) {
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
                    Err(Error::NoMatchingTemplateParameter {
                        name,
                        source: Trace::new(),
                    })
                }
            }
            TypeKind::FunctionProto => extract_function_pointer(
                ty,
                ast,
                already_visited,
                tu,
                allow_list,
                class_overrides,
                stop_on_error,
            ),
            TypeKind::Elaborated => {
                println!("elaborated");
                let named = ty.named_type()?;
                println!("named: {named:?}");
                if let Ok(uty) = named.underlying_type() {
                    extract_type(
                        uty,
                        template_parameters,
                        already_visited,
                        ast,
                        tu,
                        allow_list,
                        class_overrides,
                        stop_on_error,
                    )
                } else {
                    let decl = named.type_declaration()?;
                    println!("decl: {decl:?}");
                    panic!();
                }
            }
            TypeKind::SubstTemplateTypeParm => extract_type(
                ty.replacement_type()?,
                template_parameters,
                already_visited,
                ast,
                tu,
                allow_list,
                class_overrides,
                stop_on_error,
            ),
            _ => unimplemented!("Unimplemented extraction for {ty:?}"),
        }
    }
}

pub fn extract_function_pointer(
    ty: Type,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<QualType> {
    if ty.kind() != TypeKind::FunctionProto {
        panic!(
            "Got type kind {:?} instead of FunctionProto in extract_function_proto for {}",
            ty.kind(),
            ty.spelling(),
        );
    }

    let name = ty.spelling();

    let result = extract_type(
        ty.result_type()?,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;
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
            class_overrides,
            stop_on_error,
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
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<QualType> {
    if ty.kind() != TypeKind::FunctionProto {
        panic!(
            "Got type kind {:?} instead of FunctionProto in extract_function_proto for {}",
            ty.kind(),
            ty.spelling(),
        );
    }

    let name = ty.spelling();

    let result = extract_type(
        ty.result_type()?,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;
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
            class_overrides,
            stop_on_error,
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
