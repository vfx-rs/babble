use std::{
    fmt::{Display, Write},
    ops::{Deref, DerefMut},
};

use backtrace::Backtrace;
use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    ast::AST,
    qualtype::{QualType, TypeRef},
    templates::{TemplateArgument, TemplateParameterDecl},
};
use tracing::{error, instrument};

use crate::error::Error;
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
    Template(String),
    Unknown(TypeKind),
}

impl CTypeRef {
    pub fn is_template(&self, c_ast: &CAST) -> bool {
        match self {
            CTypeRef::Template(_) => true,
            CTypeRef::Ref(usr) => {
                if let Some(td) = c_ast.get_typedef(*usr) {
                    td.is_template(c_ast)
                } else {
                    false
                }
            }
            CTypeRef::Pointer(p) => p.is_template(c_ast),
            _ => false,
        }
    }
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

    pub fn is_template(&self, c_ast: &CAST) -> bool {
        self.type_ref.is_template(c_ast)
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
            CTypeRef::Template(parm) => {
                write!(f, "{parm}{const_}")
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
            CTypeRef::Template(parm) => {
                write!(f, "{parm}{const_}")
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
    ast: &AST,
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
                ast,
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
                ast,
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
                ast,
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
        TypeRef::Typedef(usr) => {
            let td = ast.get_type_alias(*usr).unwrap();
            if td.underlying_type().is_template_typedef(ast) {
                // expand the template now
                translate_qual_type(
                    td.underlying_type(),
                    ast,
                    template_parms,
                    template_args,
                    type_replacements,
                )
            } else {
                // otherwise, just make a reference to the typedef as we'll use it directly
                Ok(CQualType {
                    name: qual_type.name.clone(),
                    is_const: qual_type.is_const,
                    type_ref: CTypeRef::Ref(type_replacements.replace(*usr)),
                    cpp_type_ref: qual_type.type_ref.clone(),
                })
            }
        }
        TypeRef::TemplateTypeParameter(parm_name) => expand_template(
            qual_type,
            ast,
            parm_name,
            template_parms,
            template_args,
            type_replacements,
        ),
        TypeRef::FunctionProto { result, args } => {
            let c_result = translate_qual_type(
                result.deref(),
                ast,
                template_parms,
                template_args,
                type_replacements,
            )?;
            let c_args = args
                .iter()
                .map(|a| {
                    translate_qual_type(a, ast, template_parms, template_args, type_replacements)
                })
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

fn expand_template(
    qual_type: &QualType,
    ast: &AST,
    parm_name: &str,
    template_parms: &[TemplateParameterDecl],
    template_args: &[TemplateArgument],
    type_replacements: &TypeReplacements,
) -> Result<CQualType> {
    // find the parameter with the given name in the params list, then get the matching arg here
    let parm_index = template_parms
        .iter()
        .position(|p| p.name() == parm_name)
        .unwrap_or(std::usize::MAX);

    if parm_index >= template_args.len() {
        // Could not find a template arg matching this parameter, store the parameter for delayed application
        Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Template(parm_name.to_string()),
            cpp_type_ref: qual_type.type_ref.clone(),
        })
    } else {
        match &template_args[parm_index] {
            TemplateArgument::Type(tty) => {
                let mut tty = tty.clone();
                if qual_type.is_const {
                    tty.is_const = true;
                }
                translate_qual_type(&tty, ast, template_parms, template_args, type_replacements)
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
