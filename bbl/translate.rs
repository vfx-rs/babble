use std::{fmt::Display, str::FromStr};

use hashbrown::HashSet;
use log::warn;
use ustr::Ustr;

use crate::{
    ast::{ClassId, FunctionId, MethodId, UstrIndexMap, AST},
    class::ClassBindKind,
    cursor::USR,
    function::Argument,
    qualtype::{QualType, TypeRef},
    ty::TypeKind,
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
    pub fields: UstrIndexMap<CField>,
    pub bind_kind: ClassBindKind,
    /// The class from whence this struct came
    pub class_id: ClassId,
}

impl CStruct {
    pub fn format(&self, ast: &CAST) -> String {
        format!("struct {}", self.name_private)
    }

    pub fn pretty_print(&self, depth: usize) {
        println!("typdef struct {0} {{}} {0}_t;", self.name_private);
        if self.name_private != self.name_public {
            println!("#define {} {}", self.name_public, self.name_private);
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
            st.pretty_print(depth);
        }
        println!("");

        for fun in self.functions.iter() {
            fun.pretty_print(depth, self);
        }
        println!("");
    }
}

pub fn translate_qual_type(qual_type: &QualType) -> CQualType {
    match &qual_type.type_ref {
        TypeRef::Builtin(tk) => CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Builtin(*tk),
            cpp_type_ref: qual_type.type_ref.clone(),
        },
        TypeRef::Pointer(qt) | TypeRef::LValueReference(qt) | TypeRef::RValueReference(qt) => {
            CQualType {
                name: qual_type.name.clone(),
                is_const: qual_type.is_const,
                type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(qt.as_ref()))),
                cpp_type_ref: qual_type.type_ref.clone(),
            }
        }
        TypeRef::Ref(usr) => CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Ref(*usr),
            cpp_type_ref: qual_type.type_ref.clone(),
        },
        _ => todo!(),
    }
}

pub fn translate_arguments(arguments: &Vec<Argument>) -> Vec<CArgument> {
    let mut result = Vec::new();
    for arg in arguments {
        result.push(CArgument {
            name: arg.name.clone(),
            qual_type: translate_qual_type(&arg.qual_type),
        });
    }

    result
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
    // private name
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

pub fn translate_cpp_ast_to_c(ast: &AST) -> CAST {
    let mut structs = UstrIndexMap::new();
    let mut functions = UstrIndexMap::new();

    let mut used_names = HashSet::new();

    // Do all the classes first in one loop so we've got the struct definitions for when we do the methods
    for (class_id, class) in ast.classes.iter().enumerate() {
        // build the namespace prefix
        let mut ns_prefix_private = String::new();
        let mut ns_prefix_public = String::new();
        for uns in &class.namespaces {
            let ns = ast
                .get_namespace(*uns)
                .expect(&format!("Could not get namespace {}", uns));

            // The private namespace name is always taken from its decl
            ns_prefix_private = format!("{ns_prefix_private}{}_", ns.name);

            // If the namespace has been renamed for public consumption, apply the new name
            let ns_prefix_public = if let Some(name) = &ns.rename {
                format!("{ns_prefix_public}{}_", name)
            } else {
                format!("{ns_prefix_public}{}_", ns.name)
            };
        }
        let ns_prefix_private = ns_prefix_private;

        let (st_c_name_public, st_c_name_private) = get_c_names(
            class.name(),
            &ns_prefix_public,
            &ns_prefix_private,
            &mut used_names,
        );

        structs.insert(
            class.usr().0,
            CStruct {
                name_public: st_c_name_public.clone(),
                name_private: st_c_name_private.clone(),
                fields: UstrIndexMap::new(),
                bind_kind: class.bind_kind,
                class_id: ClassId(class_id),
            },
        );
    }

    // Now do the methods
    for (class_id, class) in ast.classes.iter().enumerate() {
        let st = structs.get(&class.usr().0).unwrap();

        // Now the generated struct name becomes the prefix for any methods it has
        let st_prefix_public = format!("{}_", st.name_public);
        let st_prefix_private = format!("{}_", st.name_private);

        for (method_id, method) in class.methods.iter().enumerate() {
            let source = CFunctionSource::Method((ClassId(class_id), MethodId(method_id)));
            let result = translate_qual_type(&method.function.result);
            let arguments = translate_arguments(&method.function.arguments);

            let fn_name = if let Some(name) = &method.function.replacement_name {
                name
            } else {
                &method.function.name
            };

            let (fn_name_public, fn_name_private) = get_c_names(
                fn_name,
                &st_prefix_public,
                &st_prefix_private,
                &mut used_names,
            );

            functions.insert(
                method.usr().0,
                CFunction {
                    name_private: fn_name_private,
                    name_public: fn_name_public,
                    result,
                    arguments,
                    source,
                },
            );
        }
    }

    CAST { structs, functions }
}
