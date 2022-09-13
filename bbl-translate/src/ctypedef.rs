use bbl_clang::cursor::USR;
use bbl_extract::{
    ast::{ClassId, FunctionId, AST},
    index_map::{IndexMapKey, UstrIndexMap},
    type_alias::{ClassTemplateSpecialization, FunctionTemplateSpecialization},
};
use hashbrown::HashSet;
use tracing::instrument;

use crate::{
    cfunction::{translate_function, CFunction, CFunctionId},
    cstruct::{CStruct, CStructId, translate_class_template},
    error::Error,
    ctype::TypeReplacements,
};

#[instrument(level = "trace", skip(ast, functions, used_names))]
pub fn translate_function_template_specialization(
    ast: &AST,
    fts: &FunctionTemplateSpecialization,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
) -> Result<(), Error> {
    let function_id = ast
        .functions()
        .get_id(&fts.specialized_decl().into())
        .map(|id| FunctionId::new(*id))
        .ok_or_else(|| Error::FunctionNotFound(fts.specialized_decl().to_string()))?;
    let function = &ast.functions()[function_id];

    let type_replacements = TypeReplacements::default();

    translate_function(
        ast,
        function_id,
        function,
        functions,
        used_names,
        fts.template_arguments(),
        &type_replacements,
    )?;

    Ok(())
}

#[instrument(level = "trace", skip(ast, structs, _typedefs, functions, used_names))]
pub fn translate_class_template_specialization(
    ast: &AST,
    cts: &ClassTemplateSpecialization,
    structs: &mut UstrIndexMap<CStruct, CStructId>,
    _typedefs: &mut UstrIndexMap<CTypedef, CTypedefId>,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
) -> Result<(), Error> {
    let class_id = ast
        .classes()
        .get_id(&cts.specialized_decl().into())
        .map(|id| ClassId::new(*id))
        .ok_or_else(|| Error::ClassNotFound(cts.specialized_decl().as_str().to_string()))?;
    let class = &ast.classes()[class_id];

    // let (ns_prefix_external, ns_prefix_internal) = build_namespace_prefix(ast, cts.namespaces())?;

    // get unique, prefixed names for the typedef
    // TODO (AL): need to resolve this properly: we're currently storing the name of the typedef class template 
    // specialization and then using this name when creating the monomorphizations of the class. This works well when
    // other functions and types only refer to the CTS by its typedef name, but if they use the underlying class template
    // name, we'll still have references to the underlying template instead of the typedef, and we'll need to patch those
    // too.
    /*
    let (td_c_name_external, td_c_name_internal) = get_c_names(
        cts.name(),
        &ns_prefix_external,
        &ns_prefix_internal,
        used_names,
    );

    typedefs.insert(
        cts.usr().into(),
        CTypedef {
            name_external: td_c_name_external,
            name_internal: td_c_name_internal,
            usr: cts.usr(),
            typ: cts.specialized_decl(),
        },
    );
    */

    translate_class_template(
        ast,
        class_id,
        class,
        cts.template_arguments(),
        structs,
        functions,
        used_names,
        cts,
    )?;

    Ok(())
}

#[derive(Debug)]
pub struct CTypedef {
    pub name_external: String,
    pub name_internal: String,
    pub usr: USR,
    /// The underlying type that this typedef refers to
    pub typ: USR,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CTypedefId(usize);

impl CTypedefId {
    pub fn new(id: usize) -> CTypedefId {
        CTypedefId(id)
    }
}

impl IndexMapKey for CTypedefId {
    fn get(&self) -> usize {
        self.0
    }
}
