use std::fmt::Display;

use bbl_clang::ty::TypeKind;
use bbl_extract::{
    ast::{ClassId, FunctionId, MethodId, AST},
    class::{ClassBindKind, MethodSpecializationId, ClassDecl},
    function::{Argument, Function, Method},
    qualtype::{QualType, TypeRef},
    template_argument::{TemplateParameterDecl, TemplateType}, index_map::{UstrIndexMap, IndexMapKey},
};
use hashbrown::HashSet;
use tracing::{error, instrument, trace};

use crate::{
    ctype::{translate_qual_type, CQualType, CTypeRef},
    get_c_names, CAST, build_namespace_prefix,
};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct CArgument {
    pub name: String,
    pub qual_type: CQualType,
    pub is_self: bool,
    pub is_result: bool,
}

impl Display for CArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl CArgument {
    fn format(&self, ast: &CAST, use_public_names: bool) -> String {
        format!(
            "{}: {}",
            self.name,
            self.qual_type.format(ast, use_public_names)
        )
    }
}

#[derive(Debug)]
pub enum CFunctionSource {
    /// This function was translated from a method
    Method((ClassId, MethodId)),
    /// This function was translated from a c++ function
    Function(FunctionId),
    SpecializedMethod((ClassId, MethodSpecializationId)),
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
    /// Is the cpp source of this function definitely not going to throw?
    pub is_noexcept: bool,
}

impl CFunction {
    pub fn pretty_print(&self, _depth: usize, ast: &CAST) {
        let args = self
            .arguments
            .iter()
            .map(|a| a.format(ast, false))
            .collect::<Vec<String>>();
        let arg_str = args.join(", ");

        println!(
            "{}({arg_str}) -> {};",
            self.name_private,
            self.result.format(ast, false)
        );
        if self.name_private != self.name_public {
            println!("#define {} {}", self.name_public, self.name_private);
        }
    }
}

#[instrument(skip(used_argument_names, ast), level = "trace")]
pub fn translate_arguments(
    arguments: &[Argument],
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    used_argument_names: &mut HashSet<String>,
    ast: &AST,
) -> Result<Vec<CArgument>, Error> {
    let mut result = Vec::new();

    for arg in arguments {
        let mut qual_type = translate_qual_type(arg.qual_type(), template_parms, template_args)
            .map_err(|e| {
                error!("Error translating argument {}: {e}", arg.name());
                Error::FailedToTranslateType {
                    name: arg.name().to_string(),
                    source: Box::new(e),
                }
            })?;

        trace!(
            "Translated qual_type {:?} as {:?}",
            arg.qual_type(),
            qual_type
        );

        // if the argument is a pass-by-value of a non-POD type we need to force it to be passed as a pointer and
        // deref'd on the other side
        // TODO: we'll need to move it if it's a non-copyable type
        let do_pass_by_pointer = if let CTypeRef::Ref(usr) = &qual_type.type_ref {
            let class = ast
                .get_class(*usr)
                .ok_or(Error::FailedToGetClassFromRef(*usr))?;

            !matches!(class.bind_kind(), ClassBindKind::ValueType)
        } else {
            false
        };

        if do_pass_by_pointer {
            let name = qual_type.name().to_string();
            let is_const = qual_type.is_const();
            let cpp_type_ref = qual_type.cpp_type_ref().clone();

            qual_type = CQualType {
                name,
                is_const,
                type_ref: CTypeRef::Pointer(Box::new(qual_type)),
                cpp_type_ref,
                needs_deref: true,
                needs_move: false,
                needs_alloc: false,
            };
        }
        let qual_type = qual_type;

        // We need to create an argument name if none is specified in the header
        let name = if arg.name().is_empty() {
            "arg"
        } else {
            arg.name()
        };

        result.push(CArgument {
            name: get_unique_argument_name(name, used_argument_names),
            qual_type,
            is_self: false,
            is_result: false,
        });
    }

    Ok(result)
}

#[instrument(skip(ast, functions, used_names), level = "trace")]
pub fn translate_function(
    ast: &AST,
    function_id: FunctionId,
    function: &Function,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
    template_args: &[Option<TemplateType>],
) -> Result<()> {
    // build the namespace prefix
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, function.namespaces())?;

    let source = CFunctionSource::Function(function_id);
    let result = translate_qual_type(
        function.result(),
        function.template_parameters(),
        template_args,
    )
    .map_err(|e| Error::TranslateFunction {
        name: function.name().to_string(),
        source: Box::new(Error::FailedToTranslateType {
            name: "[return]".into(),
            source: Box::new(e),
        }),
    })?;

    let mut used_argument_names = HashSet::new();

    let mut arguments = translate_arguments(
        function.arguments(),
        function.template_parameters(),
        template_args,
        &mut used_argument_names,
        ast,
    )
    .map_err(|e| Error::TranslateFunction {
        name: function.name().to_string(),
        source: Box::new(e),
    })?;

    // insert the result as the first argument, since we'll be forcing an int return type in order to return exception
    // status
    if !matches!(result.type_ref, CTypeRef::Builtin(TypeKind::Void)) {
        let result_name = get_unique_argument_name("result", &mut used_argument_names);
        arguments.insert(
            0,
            CArgument {
                name: result_name.clone(),
                qual_type: CQualType {
                    name: format!("{result_name}*"),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                    needs_alloc: false,
                    needs_deref: false,
                    needs_move: false,
                },
                is_self: false,
                is_result: true,
            },
        );
    }

    let fn_name = if let Some(name) = function.replacement_name() {
        name
    } else {
        function.name()
    };

    let (fn_name_public, fn_name_private) =
        get_c_names(fn_name, &ns_prefix_public, &ns_prefix_private, used_names);

    functions.insert(
        function.usr().into(),
        CFunction {
            name_private: fn_name_private,
            name_public: fn_name_public,
            // force an integer return for the error code
            result: CQualType {
                name: "[result]".to_string(),
                cpp_type_ref: TypeRef::Builtin(TypeKind::Int),
                type_ref: CTypeRef::Builtin(TypeKind::Int),
                is_const: false,
                needs_alloc: false,
                needs_deref: false,
                needs_move: false,
            },
            arguments,
            source,
            is_noexcept: false,
        },
    );

    Ok(())
}

#[allow(clippy::too_many_arguments)]
#[instrument(skip(used_names, ast), level = "trace")]
pub fn translate_method(
    class: &ClassDecl,
    class_template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    source: CFunctionSource,
    method: &Method,
    st_prefix_public: &str,
    st_prefix_private: &str,
    st_c_name_private: &str,
    used_names: &mut HashSet<String>,
    ast: &AST,
) -> Result<CFunction> {
    // Concatenate both the class template parameters and any template parameters on the function
    let template_parms = method
        .template_parameters()
        .iter()
        .cloned()
        .chain(class_template_parms.iter().cloned())
        .collect::<Vec<_>>();

    let result =
        translate_qual_type(method.result(), &template_parms, template_args).map_err(|e| {
            Error::TranslateFunction {
                name: format!("{}::{}", class.name(), method.name()),
                source: Box::new(Error::FailedToTranslateType {
                    name: "[return]".into(),
                    source: Box::new(e),
                }),
            }
        })?;

    let mut used_argument_names = HashSet::new();

    let mut arguments = translate_arguments(
        method.arguments(),
        &template_parms,
        template_args,
        &mut used_argument_names,
        ast,
    )
    .map_err(|e| Error::TranslateFunction {
        name: format!("{}::{}", class.name(), method.name()),
        source: Box::new(e),
    })?;

    // insert self pointer
    if !method.is_static() {
        let qt = CQualType {
            name: format!("{}*", st_c_name_private),
            is_const: false,
            type_ref: CTypeRef::Pointer(Box::new(CQualType {
                name: st_c_name_private.into(),
                is_const: method.is_const(),
                type_ref: CTypeRef::Ref(class.usr()),
                cpp_type_ref: TypeRef::Ref(class.usr()),
                needs_deref: false,
                needs_move: false,
                needs_alloc: false,
            })),
            cpp_type_ref: TypeRef::Pointer(Box::new(QualType {
                name: class.name().to_string(),
                is_const: method.is_const(),
                type_ref: TypeRef::Ref(class.usr()),
            })),
            needs_deref: false,
            needs_move: false,
            needs_alloc: false,
        };
        arguments.insert(
            0,
            CArgument {
                name: get_unique_argument_name("self", &mut used_argument_names),
                qual_type: qt,
                is_self: true,
                is_result: false,
            },
        );
    }

    // insert the result as the first argument, since we'll be forcing an int return type in order to return exception
    // status
    if !matches!(result.type_ref, CTypeRef::Builtin(TypeKind::Void)) {
        let result_name = get_unique_argument_name("result", &mut used_argument_names);
        arguments.insert(
            0,
            CArgument {
                name: result_name.clone(),
                qual_type: CQualType {
                    name: format!("{result_name}*"),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                    needs_alloc: false,
                    needs_deref: false,
                    needs_move: false,
                },
                is_self: false,
                is_result: true,
            },
        );
    }

    let fn_name = if let Some(name) = method.replacement_name() {
        name
    } else {
        method.name()
    };

    let (fn_name_public, fn_name_private) =
        get_c_names(fn_name, st_prefix_public, st_prefix_private, used_names);

    Ok(CFunction {
        name_private: fn_name_private,
        name_public: fn_name_public,
        result: CQualType::int("[result]", false),
        arguments,
        source,
        is_noexcept: false,
    })
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CFunctionId(usize);

impl CFunctionId {
    pub fn new(id: usize) -> CFunctionId {
        CFunctionId(id)
    }
}

impl IndexMapKey for CFunctionId {
    fn get(&self) -> usize {
        self.0
    }
}
