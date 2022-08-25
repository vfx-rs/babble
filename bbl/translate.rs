use std::{fmt::Display, str::FromStr};

use hashbrown::HashSet;
use log::{error, warn};
use ustr::Ustr;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use crate::template_argument::TemplateParameterDecl;
use crate::{
    ast::{ClassId, FunctionId, MethodId, UstrIndexMap, AST},
    class::{ClassBindKind, ClassDecl},
    cursor::USR,
    function::{Argument, Method},
    qualtype::{QualType, TypeRef},
    template_argument::TemplateType,
    ty::TypeKind,
    type_alias::TypeAlias,
};

pub enum CTypeRef {
    Builtin(TypeKind),
    Ref(USR),
    Pointer(Box<CQualType>),
    Unknown(TypeKind),
}

pub struct CQualType {
    name: String,
    is_const: bool,
    type_ref: CTypeRef,
    cpp_type_ref: TypeRef,
}

impl CQualType {
    pub fn unknown(tk: TypeKind) -> Self {
        CQualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: CTypeRef::Unknown(tk),
            cpp_type_ref: TypeRef::Unknown(tk),
        }
    }

    pub fn format(&self, ast: &CAST) -> String {
        let result = String::new();

        let result = if self.is_const {
            format!("{result}const ")
        } else {
            result
        };

        let result = match &self.type_ref {
            CTypeRef::Builtin(tk) => {
                format!("{result}{}", tk.spelling())
            }
            CTypeRef::Pointer(pointee) => format!("{result}{}*", pointee.format(ast)),
            CTypeRef::Ref(usr) => {
                let name = ast
                    .get_struct(*usr)
                    .map(|r| r.format(ast))
                    .unwrap_or(usr.to_string());
                format!("{result}{}", name)
            }
            CTypeRef::Unknown(tk) => {
                format!("{result}UNKNOWN({})", tk.spelling())
            }
        };

        result
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

pub struct CArgument {
    pub(crate) name: String,
    pub(crate) qual_type: CQualType,
}

impl Display for CArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl CArgument {
    fn format(&self, ast: &CAST) -> String {
        format!("{}: {}", self.name, self.qual_type.format(ast))
    }
}

pub enum CFunctionSource {
    /// This function was translated from a method
    Method((ClassId, MethodId)),
    /// This function was translated from a c++ function
    Function(FunctionId),
}

pub struct CFunction {
    /// The name of the function with internal namespace baked in, e.g. Imath_3_1_V3f_dot
    pub name_private: String,
    /// The name of the function that will be used for a public #define, e.g. Imath_V3f_dot
    pub name_public: String,
    /// The return type of the function
    pub result: CQualType,
    // The function arguments
    pub arguments: Vec<CArgument>,
    /// Where this function came from
    pub source: CFunctionSource,
}

impl CFunction {
    pub fn pretty_print(&self, depth: usize, ast: &CAST) {
        let args = self
            .arguments
            .iter()
            .map(|a| a.format(ast))
            .collect::<Vec<String>>();
        let arg_str = args.join(", ");

        println!(
            "{}({arg_str}) -> {};",
            self.name_private,
            self.result.format(ast)
        );
        if self.name_private != self.name_public {
            println!("#define {} {}", self.name_public, self.name_private);
        }
    }
}

pub struct CField {
    pub(crate) name: String,
    pub(crate) qual_type: CQualType,
}

impl Display for CField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl CField {
    fn format(&self, ast: &CAST) -> String {
        format!("{}: {}", self.name, self.qual_type.format(ast))
    }
}

pub struct CStruct {
    /// The name of the struct with internal namespace baked in, e.g. Imath_3_1_V3f
    pub name_private: String,
    /// The name of the struct that will be used for a public #define, e.g. Imath_V3f
    pub name_public: String,
    pub fields: Vec<CField>,
    pub bind_kind: ClassBindKind,
    /// The class from whence this struct came
    pub class_id: ClassId,
    pub usr: USR,
}

impl CStruct {
    pub fn format(&self, ast: &CAST) -> String {
        format!("struct {}", self.name_private)
    }

    pub fn pretty_print(&self, depth: usize, ast: &CAST) {
        println!("typedef struct {{");

        for field in self.fields.iter() {
            println!("  {};", field.format(ast));
        }

        println!("}} {};", self.name_private);

        if self.name_private != self.name_public {
            println!("typedef {} {};", self.name_private, self.name_public);
        }
    }
}

pub struct CAST {
    pub structs: UstrIndexMap<CStruct>,
    pub functions: UstrIndexMap<CFunction>,
}

impl CAST {
    pub fn get_struct(&self, usr: USR) -> Option<&CStruct> {
        self.structs.get(&usr.0)
    }

    pub fn pretty_print(&self, depth: usize) {
        for st in self.structs.iter() {
            st.pretty_print(depth, self);
        }
        println!("");

        for fun in self.functions.iter() {
            fun.pretty_print(depth, self);
        }
        println!("");

        println!(
            "{} structs and {} functions",
            self.structs.len(),
            self.functions.len()
        );
    }
}

pub fn translate_qual_type(
    qual_type: &QualType,
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
) -> Result<CQualType> {
    match &qual_type.type_ref {
        TypeRef::Builtin(tk) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Builtin(*tk),
            cpp_type_ref: qual_type.type_ref.clone(),
        }),
        TypeRef::Pointer(qt) | TypeRef::LValueReference(qt) | TypeRef::RValueReference(qt) => {
            Ok(CQualType {
                name: qual_type.name.clone(),
                is_const: qual_type.is_const,
                type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                    qt.as_ref(),
                    template_parms,
                    template_args,
                )?)),
                cpp_type_ref: qual_type.type_ref.clone(),
            })
        }
        TypeRef::Ref(usr) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Ref(*usr),
            cpp_type_ref: qual_type.type_ref.clone(),
        }),
        TypeRef::TemplateTypeParameter(parm_name) => {
            // find the parameter with the given name in the params list, then get the matching arg here
            let parm_index = template_parms
                .iter()
                .position(|p| p.name() == parm_name)
                .ok_or(Error::TemplateParmNotFound(parm_name.into()))?;

            if parm_index >= template_args.len() {
                Err(Error::TemplateArgNotFound(parm_name.into()))
            } else {
                match &template_args[parm_index] {
                    Some(TemplateType::Type(tty)) => {
                        translate_qual_type(tty, template_parms, template_args)
                    }
                    Some(TemplateType::Integer(n)) => {
                        todo!()
                    }
                    None => return Err(Error::InvalidTemplateArgumentKind),
                }
            }
        }
        _ => todo!(),
    }
}

pub fn translate_arguments(
    arguments: &Vec<Argument>,
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    used_argument_names: &mut HashSet<String>,
) -> Result<Vec<CArgument>> {
    let mut result = Vec::new();

    for arg in arguments {
        let qual_type = match translate_qual_type(&arg.qual_type, template_parms, template_args) {
            Ok(qt) => qt,
            Err(e) => {
                error!("Error translating argument {}: {e}", arg.name);
                return Err(e);
            }
        };

        // We need to create an argument name if none is specified in the header
        let name = if arg.name.is_empty() {
            "arg"
        } else {
            &arg.name
        };

        result.push(CArgument {
            name: get_unique_argument_name(name, used_argument_names),
            qual_type,
        });
    }

    Ok(result)
}

/// Given a C++ entity name, and a public and private prefix, generate the equivalent names for the C API, possibly
/// uniquified by adding an incrementing suffix if the names are already taken
pub fn get_c_names(
    cpp_name: &str,
    prefix_public: &str,
    prefix_private: &str,
    used_names: &mut HashSet<String>,
) -> (String, String) {
    let prefix_public = if prefix_public.is_empty() {
        prefix_private.to_string()
    } else {
        prefix_public.to_string()
    };

    let mut c_name_public = format!("{prefix_public}{}", cpp_name);
    let mut c_name_private = format!("{prefix_private}{}", cpp_name);

    let mut i = 1;
    while used_names.contains(&c_name_private) {
        c_name_private = format!("{prefix_private}{}_{}", cpp_name, i);
        i += 1;
    }
    used_names.insert(c_name_private.clone());
    if i > 1 {
        warn!("Renaming {cpp_name} to {c_name_private}");
    }

    // if the private and public prefixes are the same then we don't want to uniquify the public name, just use the
    // private name (which will itself already be unique by the block above)
    if prefix_private == prefix_public {
        c_name_public = c_name_private.clone();
    } else {
        i = 1;
        while used_names.contains(&c_name_public) {
            c_name_public = format!("{prefix_public}{}_{}", cpp_name, i);
            i += 1;
        }
        used_names.insert(c_name_public.clone());
        if i > 1 {
            warn!("Renaming {cpp_name} to {c_name_public}");
        }
    }

    (c_name_public, c_name_private)
}

pub fn translate_cpp_ast_to_c(ast: &AST) -> Result<CAST> {
    let mut structs = UstrIndexMap::new();
    let mut functions = UstrIndexMap::new();

    let mut used_names = HashSet::new();

    for (class_id, class) in ast.classes.iter().enumerate() {
        // if this is a template class, we'll ignore it and translate its specializations instead
        // TODO (AL): do we want to use a separate type for ClassTemplate again?
        if !class.template_parameters.is_empty() {
            continue;
        }

        translate_class(
            ast,
            ClassId(class_id),
            class,
            &[],
            &mut structs,
            &mut functions,
            &mut used_names,
        );
    }

    for (type_alias_id, type_alias) in ast.type_aliases.iter().enumerate() {
        match type_alias {
            TypeAlias::ClassTemplateSpecialization(cts) => translate_class_template_specialization(
                ast,
                cts,
                &mut structs,
                &mut functions,
                &mut used_names,
            )?,
            _ => todo!(),
        }
    }

    Ok(CAST { structs, functions })
}

fn translate_class_template_specialization(
    ast: &AST,
    cts: &crate::type_alias::ClassTemplateSpecialization,
    structs: &mut UstrIndexMap<CStruct>,
    functions: &mut UstrIndexMap<CFunction>,
    used_names: &mut HashSet<String>,
) -> Result<()> {
    let class_id = ast
        .classes
        .get_id(&cts.specialized_decl.0)
        .ok_or(Error::RecordNotFound)?;
    let class = ast.classes.index(*class_id);

    translate_class(
        ast,
        ClassId(*class_id),
        class,
        &cts.args,
        structs,
        functions,
        used_names,
    );

    Ok(())
}

pub fn translate_class(
    ast: &AST,
    class_id: ClassId,
    class: &ClassDecl,
    template_args: &[Option<TemplateType>],
    structs: &mut UstrIndexMap<CStruct>,
    functions: &mut UstrIndexMap<CFunction>,
    used_names: &mut HashSet<String>,
) -> Result<()> {
    // build the namespace prefix
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, &class.namespaces);

    // get unique, prefixed names for the struct
    let (st_c_name_public, st_c_name_private) = get_c_names(
        class.name(),
        &ns_prefix_public,
        &ns_prefix_private,
        used_names,
    );

    // translate the fields
    let mut fields = Vec::new();
    for field in class.fields.iter() {
        let name = field.name.clone();
        let qual_type = match translate_qual_type(
            &field.qual_type,
            &class.template_parameters,
            template_args,
        ) {
            Ok(qt) => qt,
            Err(e) => {
                error!(
                    "Could not translate field {name} of class {}: {e}",
                    class.name()
                );
                return Err(e);
            }
        };

        fields.push(CField { name, qual_type })
    }

    structs.insert(
        class.usr().0,
        CStruct {
            name_public: st_c_name_public.clone(),
            name_private: st_c_name_private.clone(),
            fields,
            bind_kind: class.bind_kind,
            class_id: class_id,
            usr: class.usr(),
        },
    );

    // Now the generated struct name becomes the prefix for any methods it has
    let st_prefix_public = format!("{}_", st_c_name_public);
    let st_prefix_private = format!("{}_", st_c_name_private);

    // translate the class's methods to functions
    for (method_id, method) in class.methods.iter().enumerate() {
        let c_function = translate_method(
            ast,
            class_id,
            class,
            &class.template_parameters,
            template_args,
            MethodId(method_id),
            method,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_public,
            &st_c_name_private,
            used_names,
        )?;

        functions.insert(method.usr().0, c_function);
    }

    Ok(())
}

fn get_unique_argument_name(name: &str, used_argument_names: &mut HashSet<String>) -> String {
    let mut result = name.to_string();

    let mut i = 0;
    while used_argument_names.contains(&result) {
        result = format!("{name}{i}");
        i += 1
    }

    result
}

pub fn translate_method(
    ast: &AST,
    class_id: ClassId,
    class: &ClassDecl,
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    method_id: MethodId,
    method: &Method,
    st_prefix_public: &str,
    st_prefix_private: &str,
    st_c_name_public: &str,
    st_c_name_private: &str,
    used_names: &mut HashSet<String>,
) -> Result<CFunction> {
    let source = CFunctionSource::Method((class_id, method_id));
    let result = translate_qual_type(&method.function.result, template_parms, template_args)?;

    let mut used_argument_names = HashSet::new();

    let mut arguments = translate_arguments(
        &method.function.arguments,
        template_parms,
        template_args,
        &mut used_argument_names,
    )?;

    // insert self pointer
    if !method.is_static {
        let qt = CQualType {
            name: format!("{}*", st_c_name_private),
            is_const: false,
            type_ref: CTypeRef::Pointer(Box::new(CQualType {
                name: st_c_name_private.into(),
                is_const: method.is_const,
                type_ref: CTypeRef::Ref(class.usr()),
                cpp_type_ref: TypeRef::Ref(class.usr()),
            })),
            cpp_type_ref: TypeRef::Pointer(Box::new(QualType {
                name: class.name.clone(),
                is_const: method.is_const,
                type_ref: TypeRef::Ref(class.usr()),
            })),
        };
        arguments.insert(
            0,
            CArgument {
                name: get_unique_argument_name("self", &mut used_argument_names),
                qual_type: qt,
            },
        );
    }

    let fn_name = if let Some(name) = &method.function.replacement_name {
        name
    } else {
        &method.function.name
    };

    let (fn_name_public, fn_name_private) =
        get_c_names(fn_name, st_prefix_public, st_prefix_private, used_names);

    Ok(CFunction {
        name_private: fn_name_private,
        name_public: fn_name_public,
        result,
        arguments,
        source,
    })
}

pub fn build_namespace_prefix(ast: &AST, namespaces: &[USR]) -> (String, String) {
    let mut ns_prefix_private = String::new();
    let mut ns_prefix_public = String::new();
    for uns in namespaces {
        let ns = ast
            .get_namespace(*uns)
            .expect(&format!("Could not get namespace {}", uns));

        // The private namespace name is always taken from its decl
        ns_prefix_private = format!("{ns_prefix_private}{}_", ns.name);

        // If the namespace has been renamed for public consumption, apply the new name
        ns_prefix_public = if let Some(name) = &ns.rename {
            format!("{ns_prefix_public}{}_", name)
        } else {
            format!("{ns_prefix_public}{}_", ns.name)
        };
    }

    (ns_prefix_public, ns_prefix_private)
}
