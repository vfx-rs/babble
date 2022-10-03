use std::{
    fmt::{Write, Display},
    ops::{Deref, DerefMut},
};

use backtrace::Backtrace;
use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    qualtype::{QualType, TypeRef},
    templates::{TemplateArgument, TemplateParameterDecl},
};
use tracing::{error, instrument};

use crate::error::{Error, TranslateTypeError};
type Result<T, E = Error> = std::result::Result<T, E>;

use crate::CAST;

#[derive(Debug, Clone)]
pub enum CTypeRef {
    Builtin(TypeKind),
    Ref(USR),
    Pointer(Box<CQualType>),
    FunctionProto {
        result: Box<CQualType>,
        args: Vec<CQualType>,
    },
    Unknown(TypeKind),
}

#[derive(Clone)]
pub struct CQualType {
    pub(crate) name: String,
    pub(crate) is_const: bool,
    pub(crate) type_ref: CTypeRef,
    pub(crate) cpp_type_ref: TypeRef,
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

    pub fn is_function_proto(&self, c_ast: &CAST) -> bool {
        if let CTypeRef::Ref(usr) = self.type_ref {
            c_ast.get_function_proto(usr).is_some()
        } else if let CTypeRef::Pointer(ref pointee) = self.type_ref {
            pointee.is_function_proto(c_ast)
        } else {
            false
        }
    }

    pub fn cpp_type_ref(&self) -> &TypeRef {
        &self.cpp_type_ref
    }

    pub fn unknown(tk: TypeKind) -> Self {
        CQualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: CTypeRef::Unknown(tk),
            cpp_type_ref: TypeRef::Unknown(tk),
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
                    Err(Error::RefNotFound {
                        usr: *usr,
                        backtrace: Backtrace::new(),
                    })
                }
            }
            CTypeRef::FunctionProto { result, args } => {
                let mut s = String::new();
                write!(s, "{}(*)(", **result).unwrap();
                for arg in args {
                    write!(s, "{}, ", arg).unwrap();
                }
                write!(s, ")").unwrap();
                Ok(s)
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
        }
    }

    pub fn pointer(
        name: &str,
        cpp_type_ref: TypeRef,
        c_qual_type: CQualType,
        is_const: bool,
    ) -> CQualType {
        CQualType {
            name: name.to_string(),
            is_const,
            type_ref: CTypeRef::Pointer(Box::new(c_qual_type)),
            cpp_type_ref,
        }
    }
}

impl Display for CQualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let const_ = if self.is_const { " const" } else { "" };

        match &self.type_ref {
            CTypeRef::Builtin(tk) => {
                write!(f, "{}{const_}", tk.spelling())
            }
            CTypeRef::Pointer(pointee) => write!(f, "{}*{const_}", *pointee),
            CTypeRef::Ref(usr) => {
                write!(f, "{}{const_}", usr)
            }
            CTypeRef::FunctionProto { result, args } => {
                write!(f, "{}(*)(", **result)?;
                for arg in args {
                    write!(f, "{}, ", arg)?
                }
                write!(f, ")")
            }
            CTypeRef::Unknown(tk) => {
                write!(f, "UNKNOWN({})", tk.spelling())
            }
        }
    }
}

impl std::fmt::Debug for CQualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let const_ = if self.is_const { " const" } else { "" };

        match &self.type_ref {
            CTypeRef::Builtin(tk) => {
                write!(f, "{}{const_}", tk.spelling())
            }
            CTypeRef::Pointer(pointee) => write!(f, "{}*{const_}", *pointee),
            CTypeRef::Ref(usr) => {
                write!(f, "{}{const_}", usr)
            }
            CTypeRef::FunctionProto { result, args } => {
                write!(f, "{:?}(*)(", **result)?;
                for arg in args {
                    write!(f, "{:?}, ", arg)?
                }
                write!(f, ")")
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
    template_args: &[TemplateArgument],
    type_replacements: &TypeReplacements,
) -> Result<CQualType> {
    match &qual_type.type_ref {
        TypeRef::Builtin(tk) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Builtin(*tk),
            cpp_type_ref: qual_type.type_ref.clone(),
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
        }),
        TypeRef::Ref(usr) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Ref(type_replacements.replace(*usr)),
            cpp_type_ref: qual_type.type_ref.clone(),
        }),
        TypeRef::TemplateTypeParameter(parm_name) => {
            // find the parameter with the given name in the params list, then get the matching arg here
            let parm_index = template_parms
                .iter()
                .position(|p| p.name() == parm_name)
                .ok_or_else(|| Error::TemplateParmNotFound {
                    name: parm_name.into(),
                    backtrace: Backtrace::new(),
                })?;

            if parm_index >= template_args.len() {
                Err(Error::TemplateArgNotFound {
                    name: parm_name.into(),
                    backtrace: Backtrace::new(),
                })
            } else {
                match &template_args[parm_index] {
                    TemplateArgument::Type(tty) => {
                        let mut tty = tty.clone();
                        if qual_type.is_const {
                            tty.is_const = true;
                        }
                        translate_qual_type(&tty, template_parms, template_args, type_replacements)
                    }
                    TemplateArgument::Integral(_n) => {
                        todo!()
                    }
                    _ => Err(Error::InvalidTemplateArgumentKind {
                        name: parm_name.into(),
                        backtrace: Backtrace::new(),
                    }),
                }
            }
        }
        TypeRef::FunctionProto { result, args } => {
            let c_result = translate_qual_type(
                result.deref(),
                template_parms,
                template_args,
                type_replacements,
            )?;
            let c_args = args
                .iter()
                .map(|a| translate_qual_type(a, template_parms, template_args, type_replacements))
                .collect::<Result<Vec<CQualType>>>()?;

            Ok(CQualType {
                name: qual_type.name.clone(),
                is_const: false,
                type_ref: CTypeRef::FunctionProto {
                    result: Box::new(c_result),
                    args: c_args,
                },
                cpp_type_ref: qual_type.type_ref.clone(),
            })
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
