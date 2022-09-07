use std::fmt::Display;

use bbl_clang::cursor::USR;
use bbl_extract::{class::{ClassBindKind, ClassDecl, MethodSpecializationId}, ast::{ClassId, AST, MethodId}, index_map::{IndexMapKey, UstrIndexMap}, template_argument::TemplateType};
use hashbrown::HashSet;
use tracing::{instrument, error, warn};

use crate::{ctype::{CQualType, translate_qual_type}, CAST, cfunction::{CFunction, CFunctionId, translate_method, CFunctionSource}, error::Error, build_namespace_prefix, get_c_names};

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
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn qual_type(&self) -> &CQualType {
        &self.qual_type
    }

    fn format(&self, ast: &CAST, use_public_names: bool) -> String {
        format!(
            "{}: {}",
            self.name,
            self.qual_type.format(ast, use_public_names)
        )
    }
}

pub struct CStruct {
    /// The name of the struct with internal namespace baked in, e.g. Imath_3_1_V3f
    pub name_internal: String,
    /// The name of the struct that will be used for an external-facing #define, e.g. Imath_V3f
    pub name_external: String,
    pub fields: Vec<CField>,
    pub bind_kind: ClassBindKind,
    /// The class from whence this struct came
    pub class_id: ClassId,
    pub usr: USR,
}

impl CStruct {
    pub fn format(&self, use_public_name: bool) -> String {
        if use_public_name {
            &self.name_external
        } else {
            &self.name_internal
        }
        .to_string()
    }

    pub fn pretty_print(&self, _depth: usize, ast: &CAST) {
        println!("typedef struct {{");

        for field in self.fields.iter() {
            println!("  {};", field.format(ast, false));
        }

        println!("}} {};", self.name_internal);

        if self.name_internal != self.name_external {
            println!("typedef {} {};", self.name_internal, self.name_external);
        }
    }
}

#[instrument(skip(ast, structs, functions, used_names), level="trace")]
pub fn translate_class(
    ast: &AST,
    class_id: ClassId,
    class: &ClassDecl,
    template_args: &[Option<TemplateType>],
    structs: &mut UstrIndexMap<CStruct, CStructId>,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
) -> Result<(), Error> {
    // build the namespace prefix
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, class.namespaces())?;

    // get unique, prefixed names for the struct
    let (st_c_name_public, st_c_name_private) = get_c_names(
        class.name(),
        &ns_prefix_public,
        &ns_prefix_private,
        used_names,
    );

    // translate the fields
    let mut fields = Vec::new();
    for field in class.fields().iter() {
        let name = field.name().to_string();
        let qual_type = match translate_qual_type(
            field.qual_type(),
            class.template_parameters(),
            template_args,
        ) {
            Ok(qt) => qt,
            Err(e) => {
                error!(
                    "Could not translate field {name} of class {}: {e}",
                    class.name()
                );
                return Err(Error::TranslateField { name, source: e });
            }
        };

        fields.push(CField { name, qual_type })
    }

    structs.insert(
        class.usr().into(),
        CStruct {
            name_external: st_c_name_public.clone(),
            name_internal: st_c_name_private.clone(),
            fields,
            bind_kind: *class.bind_kind(),
            class_id,
            usr: class.usr(),
        },
    );

    // Now the generated struct name becomes the prefix for any methods it has
    let st_prefix_public = format!("{}_", st_c_name_public);
    let st_prefix_private = format!("{}_", st_c_name_private);

    // translate the class's methods to functions
    for (method_id, method) in class.methods().iter().enumerate() {
        if method.is_templated() {
            // if the method itself has template parameters, then skip it and we'll deal with specializations
            // separately
            if !method.is_specialized() {
                warn!(
                    "method {} is templated but has no specializations and so will be ignored",
                    method.get_qualified_name(ast)?
                );
            }
            continue;
        }

        let c_function = translate_method(
            class,
            class.template_parameters(),
            template_args,
            CFunctionSource::Method((class_id, MethodId::new(method_id))),
            method,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
        )?;

        functions.insert(method.usr().into(), c_function);
    }

    for (spec_method_id, spec_method) in class.specialized_methods().iter().enumerate() {
        let combined_template_args = template_args
            .iter()
            .cloned()
            .chain(spec_method.template_arguments().iter().cloned())
            .collect::<Vec<_>>();
        let source = CFunctionSource::SpecializedMethod((
            class_id,
            MethodSpecializationId::new(spec_method_id),
        ));
        let method = class.get_method(spec_method.specialized_decl());

        let c_function = translate_method(
            class,
            class.template_parameters(),
            &combined_template_args,
            source,
            method,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
        )?;

        functions.insert(method.usr().into(), c_function);
    }

    Ok(())
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CStructId(usize);

impl CStructId {
    pub fn new(id: usize) -> CStructId {
        CStructId(id)
    }
}

impl IndexMapKey for CStructId {
    fn get(&self) -> usize {
        self.0
    }
}
