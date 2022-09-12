use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    qualtype::{QualType, TypeRef},
    template_argument::{TemplateParameterDecl, TemplateType},
};
use tracing::{error, instrument, trace};

use crate::error::{Error, TranslateTypeError};
type Result<T, E = Error> = std::result::Result<T, E>;

use crate::CAST;

#[derive(Debug, Clone)]
pub enum CTypeRef {
    Builtin(TypeKind),
    Ref(USR),
    Pointer(Box<CQualType>),
    Unknown(TypeKind),
}

#[derive(Debug, Clone)]
pub struct CQualType {
    pub(crate) name: String,
    pub(crate) is_const: bool,
    pub(crate) type_ref: CTypeRef,
    pub(crate) cpp_type_ref: TypeRef,
    pub(crate) needs_deref: bool,
    pub(crate) needs_move: bool,
    pub(crate) needs_alloc: bool,
}

impl CQualType {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_ref(&self) -> &CTypeRef {
        &self.type_ref
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.type_ref, CTypeRef::Pointer(_))
    }

    pub fn needs_deref(&self) -> bool {
        self.needs_deref
    }

    pub fn needs_move(&self) -> bool {
        self.needs_move
    }

    pub fn cpp_type_ref(&self) -> &TypeRef {
        &self.cpp_type_ref
    }

    pub fn unknown(tk: TypeKind) -> Self {
        CQualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            needs_deref: false,
            needs_move: false,
            type_ref: CTypeRef::Unknown(tk),
            cpp_type_ref: TypeRef::Unknown(tk),
            needs_alloc: false,
        }
    }

    #[instrument(level = "trace", skip(ast))]
    pub fn format(&self, ast: &CAST, use_public_names: bool) -> Result<String> {
        let const_ = if self.is_const { " const" } else { "" };

        match &self.type_ref {
            CTypeRef::Builtin(tk) => Ok(format!("{}{const_}", tk.spelling())),
            CTypeRef::Pointer(pointee) => Ok(format!(
                "{}*{const_}",
                pointee.format(ast, use_public_names)?
            )),
            CTypeRef::Ref(usr) => {
                if let Some(st) = ast.get_struct(*usr) {
                    Ok(st.format(use_public_names))
                } else if let Some(td) = ast.get_typedef(*usr) {
                    // no struct with this USR, see if there's a typedef instead
                    Ok(format!("{}{const_}", td.name_external))
                } else {
                    Err(Error::RefNotFound(*usr))
                }
            }
            CTypeRef::Unknown(tk) => Ok(format!("UNKNOWN({}){const_}", tk.spelling())),
        }
    }

    pub fn int(name: &str, is_const: bool) -> CQualType {
        CQualType {
            name: name.to_string(),
            cpp_type_ref: TypeRef::Builtin(TypeKind::Int),
            type_ref: CTypeRef::Builtin(TypeKind::Int),
            is_const,
            needs_alloc: false,
            needs_deref: false,
            needs_move: false,
        }
    }

    pub fn pointer(
        name: &str,
        cpp_type_ref: TypeRef,
        c_qual_type: CQualType,
        is_const: bool,
        needs_deref: bool,
        needs_move: bool,
        needs_alloc: bool,
    ) -> CQualType {
        CQualType {
            name: name.to_string(),
            is_const,
            type_ref: CTypeRef::Pointer(Box::new(c_qual_type)),
            cpp_type_ref,
            needs_deref,
            needs_move,
            needs_alloc,
        }
    }
}

impl Display for CQualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        match &self.type_ref {
            CTypeRef::Builtin(tk) => {
                write!(f, "{}", tk.spelling())
            }
            CTypeRef::Pointer(pointee) => write!(f, "{}*", *pointee),
            CTypeRef::Ref(usr) => {
                write!(f, "{}", usr)
            }
            CTypeRef::Unknown(tk) => {
                write!(f, "UNKNOWN({})", tk.spelling())
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct TypeReplacements {
    replacements: Vec<(USR, USR)>,
}

impl Deref for TypeReplacements {
    type Target = Vec<(USR, USR)>;

    fn deref(&self) -> &Self::Target {
        &self.replacements
    }
}

impl DerefMut for TypeReplacements {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.replacements
    }
}

impl TypeReplacements {
    pub fn replace(&self, usr: USR) -> USR {
        for tup in &self.replacements {
            if tup.0 == usr {
                return tup.1;
            }
        }

        usr
    }
}

#[instrument(level = "trace")]
pub fn translate_qual_type(
    qual_type: &QualType,
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    type_replacements: &TypeReplacements,
) -> Result<CQualType, TranslateTypeError> {
    match &qual_type.type_ref {
        TypeRef::Builtin(tk) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Builtin(*tk),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: false,
            needs_alloc: false,
        }),
        TypeRef::Pointer(qt) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                qt.as_ref(),
                template_parms,
                template_args,
                type_replacements,
            )?)),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: false,
            needs_alloc: false,
        }),
        TypeRef::LValueReference(qt) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                qt.as_ref(),
                template_parms,
                template_args,
                type_replacements,
            )?)),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: true,
            needs_move: false,
            needs_alloc: false,
        }),
        TypeRef::RValueReference(qt) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                qt.as_ref(),
                template_parms,
                template_args,
                type_replacements,
            )?)),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: true,
            needs_alloc: false,
        }),
        TypeRef::Ref(usr) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Ref(type_replacements.replace(*usr)),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: false,
            needs_alloc: false,
        }),
        TypeRef::TemplateTypeParameter(parm_name) => {
            // find the parameter with the given name in the params list, then get the matching arg here
            let parm_index = template_parms
                .iter()
                .position(|p| p.name() == parm_name)
                .ok_or_else(|| TranslateTypeError::TemplateParmNotFound(parm_name.into()))?;

            if parm_index >= template_args.len() {
                Err(TranslateTypeError::TemplateArgNotFound(parm_name.into()))
            } else {
                match &template_args[parm_index] {
                    Some(TemplateType::Type(tty)) => {
                        translate_qual_type(tty, template_parms, template_args, type_replacements)
                    }
                    Some(TemplateType::Integer(_n)) => {
                        todo!()
                    }
                    None => Err(TranslateTypeError::InvalidTemplateArgumentKind(
                        parm_name.into(),
                    )),
                }
            }
        }
        _ => {
            error!(
                "Not yet implemented qual_type translation for {:?}",
                qual_type.type_ref
            );
            todo!()
        }
    }
}
