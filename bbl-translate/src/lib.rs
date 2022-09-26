pub mod cfunction;
pub mod cstruct;
pub mod ctype;
pub mod ctypedef;
pub mod error;

use bbl_clang::cursor::USR;
use bbl_extract::ast::Include;
use bbl_extract::index_map::UstrIndexMap;
use cfunction::{translate_function, CFunction, CFunctionId};
use cstruct::{translate_class, CStruct, CStructId};
use ctype::TypeReplacements;
use ctypedef::{
    translate_class_template_specialization, translate_function_template_specialization, CTypedef,
    CTypedefId, translate_typedef,
};
use hashbrown::HashSet;

use error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use bbl_extract::{
    ast::{ClassId, FunctionId, AST},
};
use tracing::{debug, warn};
use std::fmt::Debug;

pub struct CAST {
    pub structs: UstrIndexMap<CStruct, CStructId>,
    pub typedefs: UstrIndexMap<CTypedef, CTypedefId>,
    pub functions: UstrIndexMap<CFunction, CFunctionId>,
    pub includes: Vec<Include>,
}

impl Debug for CAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for st in self.structs.iter() {
            writeln!(f, "{st:?}")?;
        }

        for td in self.typedefs.iter() {
            writeln!(f, "{td:?}")?;
        }

        for fun in self.functions.iter() {
            writeln!(f, "{fun:?}")?;
        }

        for inc in self.includes.iter() {
            writeln!(f, "{inc:?}")?;
        }


        Ok(())
    }
}

impl CAST {
    pub fn get_struct(&self, usr: USR) -> Option<&CStruct> {
        self.structs.get(&usr.into())
    }

    pub fn get_typedef(&self, usr: USR) -> Option<&CTypedef> {
        self.typedefs.get(&usr.into())
    }

    pub fn pretty_print(&self, depth: usize) -> Result<()> {
        for inc in self.includes.iter() {
            println!("{}", inc.get_statement());
        }

        for st in self.structs.iter() {
            st.pretty_print(depth, self)
                .map_err(|e| Error::FailedToFormatStruct {
                    name: st.name_internal.clone(),
                    source: Box::new(e),
                })?;
        }
        println!();

        for fun in self.functions.iter() {
            fun.pretty_print(depth, self)
                .map_err(|e| Error::FailedToFormatFunction {
                    name: fun.name_internal.clone(),
                    source: Box::new(e),
                })?;
        }
        println!();

        println!(
            "{} structs and {} functions",
            self.structs.len(),
            self.functions.len()
        );

        Ok(())
    }
}

/// Given a C++ entity name, and a public and private prefix, generate the equivalent names for the C API, possibly
/// uniquifi
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
    let mut typedefs = UstrIndexMap::new();
    let mut functions = UstrIndexMap::new();

    let mut used_names = HashSet::new();

    let type_replacements = TypeReplacements::default();

    for (class_id, class) in ast.classes().iter().enumerate() {
        debug!("translating {class:?}");
        // if this is a template class, we'll ignore it and translate its specializations instead
        if class.is_templated() {
            if !class.is_specialized() {
                warn!(
                    "class {} is templated but has no specializations and so will be ignored",
                    class.get_qualified_name(ast)?
                );
            }
            continue;
        }

        translate_class(
            ast,
            ClassId::new(class_id),
            class,
            &[],
            &mut structs,
            &mut functions,
            &mut used_names,
            &type_replacements,
        )?;
    }

    for cts in ast.class_template_specializations().iter() {
        translate_class_template_specialization(
            ast,
            cts,
            &mut structs,
            &mut typedefs,
            &mut functions,
            &mut used_names,
        )?;
    }

    for fts in ast.function_template_specializations().iter() {
        translate_function_template_specialization(ast, fts, &mut functions, &mut used_names)?;
    }

    for (_type_alias_id, td) in ast.type_aliases().iter().enumerate() {
        translate_typedef(ast, td, &mut typedefs)?;
    }

    for (function_id, function) in ast.functions().iter().enumerate() {
        if function.is_templated() {
            if !function.is_specialized() {
                warn!(
                    "function {} is templated but has no specializations and so will be ignored",
                    function.get_qualified_name(ast)?
                );
            }
            continue;
        }

        translate_function(
            ast,
            FunctionId::new(function_id),
            function,
            &mut functions,
            &mut used_names,
            &[],
            &type_replacements,
        )?;
    }

    Ok(CAST {
        structs,
        typedefs,
        functions,
        includes: ast.includes().to_vec(),
    })
}

pub fn build_namespace_prefix(ast: &AST, namespaces: &[USR]) -> Result<(String, String)> {
    let mut ns_prefix_private = String::new();
    let mut ns_prefix_public = String::new();
    for uns in namespaces {
        let names = ast.get_class_or_namespace_names(*uns)?;

        // The private namespace name is always taken from its decl
        ns_prefix_private = format!("{ns_prefix_private}{}_", names.0);

        // If the namespace has been renamed for public consumption, apply the new name
        ns_prefix_public = if let Some(name) = names.1 {
            format!("{ns_prefix_public}{}_", name)
        } else {
            format!("{ns_prefix_public}{}_", names.0)
        };
    }

    Ok((ns_prefix_public, ns_prefix_private))
}
