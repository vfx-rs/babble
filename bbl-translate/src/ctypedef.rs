use bbl_clang::cursor::USR;
use bbl_extract::{
    ast::{ClassId, FunctionId, AST},
    index_map::{IndexMapKey, UstrIndexMap},
    templates::{ClassTemplateSpecialization, FunctionTemplateSpecialization},
    typedef::Typedef,
};
use bbl_util::Trace;
use hashbrown::HashSet;
use tracing::{debug, error, instrument};

use crate::{
    build_namespace_prefix,
    cfunction::{translate_function, CFunction, CFunctionId},
    cstruct::{translate_class_template, CStruct, CStructId},
    ctype::{translate_qual_type, CQualType, TypeReplacements},
    error::Error,
    sanitize_name, CAST,
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
        .ok_or_else(|| Error::FunctionNotFound {
            name: fts.specialized_decl().to_string(),
            source: Trace::new(),
        })?;
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
        .ok_or_else(|| Error::ClassNotFound {
            name: cts.specialized_decl().as_str().to_string(),
            source: Trace::new(),
        })?;
    let class = &ast.classes()[class_id];

    debug!(
        "Translating CTS {} with args {:?}",
        cts.name(),
        cts.template_arguments()
    );
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

pub fn translate_typedef(
    ast: &AST,
    td: &Typedef,
    typedefs: &mut UstrIndexMap<CTypedef, CTypedefId>,
) -> Result<(), Error> {
    let (ns_prefix_external, ns_prefix_internal) = build_namespace_prefix(ast, td.namespaces())?;

    // we don't sgenerate unique names for typedefs, because it's perfectly fine to typedef one name to itself, and this
    // allows us to seamlessly handle manual implementations for typedef'd templated std types like std::string
    let td_c_name_external = format!("{ns_prefix_external}{}", td.name());
    let td_c_name_internal = format!("{ns_prefix_internal}{}", td.name());

    let td_c_name_external = sanitize_name(&td_c_name_external);
    let td_c_name_internal = sanitize_name(&td_c_name_internal);

    let type_replacements = TypeReplacements::default();
    let underlying_type =
        translate_qual_type(td.underlying_type(), ast, &[], &[], &type_replacements).map_err(
            |e| {
                error!("type failed {:?}", td.underlying_type());
                Error::FailedToTranslateTypedef {
                    usr: td.usr(),
                    source: Box::new(e),
                }
            },
        )?;

    typedefs.insert(
        td.usr().into(),
        CTypedef {
            name_external: td_c_name_external.to_string(),
            name_internal: td_c_name_internal.to_string(),
            usr: td.usr(),
            underlying_type,
        },
    );

    // match translate_qual_type(td.underlying_type(), &[], &[], &type_replacements) {
    //     Ok(underlying_type) => {
    //         typedefs.insert(
    //             td.usr().into(),
    //             CTypedef {
    //                 name_external: td_c_name_external.to_string(),
    //                 name_internal: td_c_name_internal.to_string(),
    //                 usr: td.usr(),
    //                 underlying_type,
    //             },
    //         );
    //     }
    //     Err(e) => error!("Failed to translate typedef {}: {}", td.usr(), e),
    // }

    Ok(())
}

pub struct CTypedef {
    pub name_external: String,
    pub name_internal: String,
    pub usr: USR,
    /// The underlying type that this typedef refers to
    pub underlying_type: CQualType,
}

impl CTypedef {
    pub fn is_template(&self, c_ast: &CAST) -> bool {
        self.underlying_type.is_template(c_ast)
    }
}

impl std::fmt::Debug for CTypedef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CTypedef {} {} {:?}",
            self.name_internal, self.name_external, self.underlying_type
        )
    }
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
