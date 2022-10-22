use std::fmt::Display;

use bbl_clang::{cursor::USR, exception::ExceptionSpecificationKind};
use bbl_extract::{
    ast::{ClassId, MethodId, AST},
    class::{ClassBindKind, ClassDecl, MethodSpecializationId},
    function::{Argument, Const, Deleted, Method, MethodKind, PureVirtual, Static, Virtual},
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::QualType,
    templates::{ClassTemplateSpecialization, TemplateArgument},
};
use hashbrown::HashSet;
use tracing::{error, instrument, trace, warn};

use crate::{
    build_namespace_prefix,
    cfunction::{translate_method, CFunction, CFunctionId, CFunctionSource},
    ctype::{translate_qual_type, CQualType, TypeReplacements},
    error::Error,
    get_c_names, sanitize_name,
};
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct CField {
    pub(crate) name: String,
    pub(crate) qual_type: CQualType,
}

impl Display for CField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl std::fmt::Debug for CField {
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

impl std::fmt::Debug for CStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CStruct {} {} {} {:?} fields={:?}",
            self.usr, self.name_internal, self.name_external, self.bind_kind, self.fields
        )
    }
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip(ast, structs, functions, used_names), level = "trace")]
pub fn translate_class(
    ast: &AST,
    class_id: ClassId,
    class: &ClassDecl,
    template_args: &[TemplateArgument],
    structs: &mut UstrIndexMap<CStruct, CStructId>,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
    type_replacements: &TypeReplacements,
) -> Result<(), Error> {
    trace!("Translate class {class:?}");
    // build the namespace prefix
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, class.namespaces())?;

    let st_name = sanitize_name(class.name());
    let ns_prefix_public = sanitize_name(&ns_prefix_public);
    let ns_prefix_private = sanitize_name(&ns_prefix_private);

    // get unique, prefixed names for the struct
    let (st_c_name_public, st_c_name_private) =
        get_c_names(&st_name, &ns_prefix_public, &ns_prefix_private, used_names);

    // translate the fields
    let mut fields = Vec::new();
    for field in class.fields().iter() {
        let name = field.name().to_string();
        let qual_type = match translate_qual_type(
            field.qual_type(),
            ast,
            class.template_parameters(),
            template_args,
            type_replacements,
        ) {
            Ok(qt) => qt,
            Err(e) => {
                error!(
                    "Could not translate field {name} of class {}: {e}",
                    class.name()
                );
                return Err(Error::TranslateField {
                    name,
                    source: Box::new(e),
                });
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

    let qname = class.get_qualified_name(ast)?;

    // check if we need to insert implicitly defined default constructors
    let implicit_methods = generate_implicit_methods(class);

    // translate the class's methods to functions
    for (method_id, method) in class.methods().iter().chain(&implicit_methods).enumerate() {
        if method.is_templated() {
            // if the method itself has template parameters, then skip it and we'll deal with specializations
            // separately
            if !method.is_specialized() {
                warn!(
                    "method {} is templated but has no specializations and so will be ignored",
                    method.signature(ast, class.template_parameters(), None)
                );
            }
            continue;
        }

        if method.is_deleted() {
            continue;
        }

        match translate_method(
            class,
            class.template_parameters(),
            template_args,
            CFunctionSource::Method((class_id, MethodId::new(method_id))),
            method,
            &qname,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
            type_replacements,
            None,
        ) {
            Ok(c_function) => {
                functions.insert(method.usr().into(), c_function);
            }
            Err(e) => {
                if e.is_unsupported() {
                    error!(
                        "Failed to translate method {}::{} due to unsupported feature: {e:?}",
                        class.name(),
                        method.name()
                    );
                    continue;
                } else {
                    return Err(Error::TranslateMethod {
                        name: format!("{}::{}", class.name(), method.name()),
                        source: Box::new(e),
                    });
                }
            }
        }
    }

    /*
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
        if method.is_deleted() {
            continue;
        }

        let c_function = translate_method(
            class,
            class.template_parameters(),
            &combined_template_args,
            source,
            method,
            &qname,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
            type_replacements,
            Some(spec_method.name()),
        )
        .map_err(|e| Error::TranslateSpecializedMethod {
            name: method.name().to_string(),
            source: Box::new(e),
        })?;

        functions.insert(method.usr().into(), c_function);
    }
    */

    Ok(())
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip(ast, structs, functions, used_names), level = "trace")]
pub fn translate_class_template(
    ast: &AST,
    class_id: ClassId,
    class: &ClassDecl,
    template_args: &[TemplateArgument],
    structs: &mut UstrIndexMap<CStruct, CStructId>,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
    cts: &ClassTemplateSpecialization,
) -> Result<(), Error> {
    trace!("Translate class template {class:?} with specialization {cts:?}");
    // replace any references to the templated class to the specialization in methods and fields
    let mut type_replacements = TypeReplacements::default();
    type_replacements.push((class.usr(), cts.usr()));

    trace!("type replacements are {type_replacements:?}");

    // build the namespace prefix
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, cts.namespaces())?;

    let st_name = sanitize_name(cts.name());
    let ns_prefix_public = sanitize_name(&ns_prefix_public);
    let ns_prefix_private = sanitize_name(&ns_prefix_private);

    // get unique, prefixed names for the struct
    let (st_c_name_public, st_c_name_private) =
        get_c_names(&st_name, &ns_prefix_public, &ns_prefix_private, used_names);

    trace!("got names {st_c_name_public}, {st_c_name_private}");

    // translate the fields
    let mut fields = Vec::new();
    for field in class.fields().iter() {
        let name = field.name().to_string();
        let qual_type = match translate_qual_type(
            field.qual_type(),
            ast,
            class.template_parameters(),
            cts.template_arguments(),
            &type_replacements,
        ) {
            Ok(qt) => qt,
            Err(e) => {
                error!(
                    "Could not translate field {name} of class {}: {e}",
                    class.name()
                );
                return Err(Error::TranslateField {
                    name,
                    source: Box::new(e),
                });
            }
        };

        fields.push(CField { name, qual_type })
    }

    structs.insert(
        cts.usr().into(),
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

    let qname = cts.get_qualified_name(ast)?;

    // check if we need to insert implicitly defined default constructors
    let implicit_methods = generate_implicit_methods(class);

    // translate the class's methods to functions
    for (method_id, method) in class.methods().iter().chain(&implicit_methods).enumerate() {
        if method.is_templated() {
            // if the method itself has template parameters, then skip it and we'll deal with specializations
            // separately
            if !method.is_specialized() {
                warn!(
                    "method {method:?} is templated but has no specializations and so will be ignored"
                );
            }
            continue;
        }

        if method.is_deleted() {
            continue;
        }

        let c_function = translate_method(
            class,
            class.template_parameters(),
            cts.template_arguments(),
            CFunctionSource::Method((class_id, MethodId::new(method_id))),
            method,
            &qname,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
            &type_replacements,
            None,
        )?;

        functions.insert(method.usr().into(), c_function);
    }

    for (spec_method_id, spec_method) in class.specialized_methods().iter().enumerate() {
        let combined_template_args = cts
            .template_arguments()
            .iter()
            .cloned()
            .chain(spec_method.template_arguments().iter().cloned())
            .collect::<Vec<_>>();
        let source = CFunctionSource::SpecializedMethod((
            class_id,
            MethodSpecializationId::new(spec_method_id),
        ));
        let method = class.get_method(spec_method.specialized_decl());
        if method.is_deleted() {
            continue;
        }

        let c_function = translate_method(
            class,
            class.template_parameters(),
            &combined_template_args,
            source,
            method,
            &qname,
            &st_prefix_public,
            &st_prefix_private,
            &st_c_name_private,
            used_names,
            ast,
            &type_replacements,
            Some(spec_method.name()),
        )
        .map_err(|e| Error::TranslateSpecializedMethod {
            name: method.name().to_string(),
            source: Box::new(e),
        })?;

        functions.insert(method.usr().into(), c_function);
    }

    Ok(())
}

fn generate_implicit_methods(class: &ClassDecl) -> Vec<Method> {
    let mut implicit_methods = Vec::new();
    if class.needs_implicit_ctor() {
        let mut namespaces = class.namespaces().to_vec();
        namespaces.push(class.usr());
        implicit_methods.push(Method::new(
            USR::new(&format!("{}#implicit_ctor", class.usr())),
            "ctor".to_string(),
            MethodKind::DefaultConstructor,
            QualType::void(),
            Vec::new(),
            None,
            namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Static(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ));
    }

    if class.needs_implicit_copy_ctor() {
        let mut namespaces = class.namespaces().to_vec();
        namespaces.push(class.usr());
        implicit_methods.push(Method::new(
            USR::new(&format!("{}#implicit_copy_ctor", class.usr())),
            "copy_ctor".to_string(),
            MethodKind::CopyConstructor,
            QualType::void(),
            vec![Argument::new(
                "rhs",
                QualType::lvalue_reference(
                    &format!("{} const&", class.name()),
                    QualType::type_ref(class.name(), true, class.usr()),
                ),
            )],
            None,
            namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Static(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ));
    }

    if class.needs_implicit_move_ctor() {
        let mut namespaces = class.namespaces().to_vec();
        namespaces.push(class.usr());
        implicit_methods.push(Method::new(
            USR::new(&format!("{}#implicit_move_ctor", class.usr())),
            "move_ctor".to_string(),
            MethodKind::MoveConstructor,
            QualType::void(),
            vec![Argument::new(
                "rhs",
                QualType::rvalue_reference(
                    &format!("{}&&", class.name()),
                    QualType::type_ref(class.name(), true, class.usr()),
                ),
            )],
            None,
            namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Static(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ));
    }

    // We only insert a destructor if the class is not a ValueType - if it needs a destructor it won't be a POD
    // TODO(AL): revisit this - if we force a value type we should probably allow calling the destructor directly?
    if class.needs_implicit_dtor() && *class.bind_kind() == ClassBindKind::OpaquePtr {
        let mut namespaces = class.namespaces().to_vec();
        namespaces.push(class.usr());
        implicit_methods.push(Method::new(
            USR::new(&format!("{}#implicit_dtor", class.usr())),
            "dtor".to_string(),
            MethodKind::Destructor,
            QualType::void(),
            Vec::new(),
            None,
            namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Static(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ));
    }

    implicit_methods
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
