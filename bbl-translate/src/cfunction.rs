use std::fmt::Display;

use bbl_clang::ty::TypeKind;
use bbl_extract::{
    ast::{ClassId, FunctionId, MethodId, AST},
    class::{ClassBindKind, ClassDecl, MethodSpecializationId},
    function::{Argument, Function, Method, MethodKind},
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::{QualType, TypeRef},
    template_argument::{TemplateParameterDecl, TemplateType},
};
use hashbrown::HashSet;
use tracing::{error, instrument, trace};

use crate::{
    build_namespace_prefix,
    ctype::{translate_qual_type, CQualType, CTypeRef, TypeReplacements},
    get_c_names, CAST,
};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
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
    fn format(&self, ast: &CAST, use_public_names: bool) -> Result<String> {
        Ok(format!(
            "{}: {}",
            self.name,
            self.qual_type.format(ast, use_public_names).map_err(|e| {
                Error::FailedToFormatArgument {
                    name: self.name.clone(),
                    source: Box::new(e),
                }
            })?
        ))
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

/// Cached information we'll want to know about the method when writing the C function.
/// 
/// We cache this on translation to save doing a whole bunch of AST lookup stuff, and also so that specialized class
/// templates can override the qualified name for the class, i.e. so that when we call back to cpp when writing we can
/// easily call std::string::foo() rather than std::__cxx11::basic_string<char>::foo().
#[derive(Debug)]
pub struct MethodInfo {
    pub kind: MethodKind,
    pub class_qname: String,
}

#[derive(Debug)]
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
    /// if the source function was a method, what kind of method was it?
    pub method_info: Option<MethodInfo>,
    /// Is the cpp source of this function absolutely definitely not going to throw?
    pub is_noexcept: bool,
}

impl CFunction {
    #[instrument(level = "trace", skip(ast))]
    pub fn pretty_print(&self, _depth: usize, ast: &CAST) -> Result<()> {
        let args = self
            .arguments
            .iter()
            .map(|a| a.format(ast, false))
            .collect::<Result<Vec<String>>>()?;
        let arg_str = args.join(", ");

        println!(
            "{}({arg_str}) -> {};",
            self.name_private,
            self.result
                .format(ast, false)
                .map_err(|e| Error::FailedToFormatFunction {
                    name: self.name_private.clone(),
                    source: Box::new(e)
                })?
        );
        if self.name_private != self.name_public {
            println!("#define {} {}", self.name_public, self.name_private);
        }

        Ok(())
    }

    /// Does this function have a return value?
    pub fn has_return_value(&self) -> bool {
        !self.arguments.is_empty() && self.arguments[0].is_result
    }

    pub fn return_value(&self) -> Option<&CArgument> {
        if self.has_return_value() {
            Some(&self.arguments[0])
        } else {
            None
        }
    }

    /// Does this function represent a c++ constructor?
    pub fn is_any_constructor(&self) -> bool {
        if let Some(info) = &self.method_info {
            info.kind.is_any_constructor()
        } else {
            false
        }
    }

    pub fn parent_class_is_opaqueptr(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::OpaquePtr)
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::OpaquePtr)
            }
        }
    }

    pub fn parent_class_is_opaquebytes(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::OpaqueBytes)
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::OpaqueBytes)
            }
        }
    }

    pub fn parent_class_is_valuetype(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::ValueType)
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(ast.classes()[class_id].bind_kind(), ClassBindKind::ValueType)
            }
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
    type_replacements: &TypeReplacements,
) -> Result<Vec<CArgument>, Error> {
    trace!("Translating arguments {:?}", arguments);
    let mut result = Vec::new();

    for arg in arguments {
        let mut qual_type = translate_qual_type(
            arg.qual_type(),
            template_parms,
            template_args,
            type_replacements,
        )
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
    type_replacements: &TypeReplacements,
) -> Result<()> {
    // build the namespace prefix
    trace!("Translating function {:?}", function);
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, function.namespaces())?;

    let source = CFunctionSource::Function(function_id);
    let result = translate_qual_type(
        function.result(),
        function.template_parameters(),
        template_args,
        type_replacements,
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
        type_replacements,
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
            method_info: None,
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
    class_qname: &str,
    st_prefix_public: &str,
    st_prefix_private: &str,
    st_c_name_private: &str,
    used_names: &mut HashSet<String>,
    ast: &AST,
    type_replacements: &TypeReplacements,
) -> Result<CFunction> {
    trace!("translate method {class:?} {method:?} with replacements {type_replacements:?}");
    // Concatenate both the class template parameters and any template parameters on the function
    let template_parms = method
        .template_parameters()
        .iter()
        .cloned()
        .chain(class_template_parms.iter().cloned())
        .collect::<Vec<_>>();

    let result = translate_qual_type(
        method.result(),
        &template_parms,
        template_args,
        type_replacements,
    )
    .map_err(|e| Error::TranslateFunction {
        name: format!("{}::{}", class.name(), method.name()),
        source: Box::new(Error::FailedToTranslateType {
            name: "[return]".into(),
            source: Box::new(e),
        }),
    })?;
    println!("method {} result is {result:?}", method.name());

    let mut used_argument_names = HashSet::new();

    let mut arguments = translate_arguments(
        method.arguments(),
        &template_parms,
        template_args,
        &mut used_argument_names,
        ast,
        type_replacements,
    )
    .map_err(|e| Error::TranslateFunction {
        name: format!("{}::{}", class.name(), method.name()),
        source: Box::new(e),
    })?;

    // insert self pointer
    if !method.is_static()  && !method.is_any_constructor() {
        let qt = CQualType {
            name: format!("{}*", st_c_name_private),
            is_const: false,
            type_ref: CTypeRef::Pointer(Box::new(CQualType {
                name: st_c_name_private.into(),
                is_const: method.is_const(),
                type_ref: CTypeRef::Ref(type_replacements.replace(class.usr())),
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

        match class.bind_kind() {
            ClassBindKind::ValueType => {
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
            ClassBindKind::OpaquePtr => {
                // in the case of an opaque type, we can't pass a pointer to a value in to initialize, because we don't
                // have a value, so it must be a pointer to a pointer
                let cpp_type_ref = result.cpp_type_ref().clone();

                let inner_pointer = CQualType {
                            name: format!("{result_name}*"),
                            cpp_type_ref: cpp_type_ref.clone(),
                            type_ref: CTypeRef::Pointer(Box::new(result)),
                            is_const: false,
                            needs_alloc: false,
                            needs_deref: false,
                            needs_move: false,
                        };

                let outer_pointer = CQualType {
                            name: format!("{result_name}**"),
                            cpp_type_ref, // TODO (AL): do we need to put an actual type here?
                            type_ref: CTypeRef::Pointer(Box::new(inner_pointer)),
                            is_const: false,
                            needs_alloc: false,
                            needs_deref: false,
                            needs_move: false,
                        };

                arguments.insert(
                    0,
                    CArgument {
                        name: result_name,
                        qual_type: outer_pointer,
                        is_self: false,
                        is_result: true,
                    },
                );
            }
            _ => todo!("Handle opaque bytes")
        }

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
        method_info: Some(MethodInfo {
            kind: method.kind(),
            class_qname: class_qname.to_string(),
        }),
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
