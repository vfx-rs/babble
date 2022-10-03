use std::fmt::Display;

use backtrace::Backtrace;
use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{
    ast::{ClassId, FunctionId, MethodId, AST},
    class::{ClassBindKind, ClassDecl, MethodSpecializationId},
    function::{Argument, Function, FunctionProto, Method, MethodKind},
    index_map::{IndexMapKey, UstrIndexMap},
    qualtype::{QualType, TypeRef},
    templates::{TemplateArgument, TemplateParameterDecl},
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

impl std::fmt::Debug for CArgument {
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

pub struct CFunction {
    /// The name of the function with internal namespace baked in, e.g. Imath_3_1_V3f_dot
    pub name_internal: String,
    /// The name of the function that will be used for a public #define, e.g. Imath_V3f_dot
    pub name_external: String,
    /// The return type of the function
    pub result: CQualType,
    // The function arguments
    pub arguments: Vec<CArgument>,
    /// Where this function came from
    pub source: CFunctionSource,
    /// Information about the method we'll want in the writing part later
    pub method_info: Option<MethodInfo>,
    /// Is the cpp source of this function absolutely definitely not going to throw?
    pub is_noexcept: bool,
    /// The body of the function as a tree of statements
    pub body: Expr,
}

impl std::fmt::Debug for CFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CFunction {} {}({:?}) {} -> {:?}",
            self.name_internal,
            self.name_external,
            self.arguments,
            if self.is_noexcept { "noexcept" } else { "" },
            self.result
        )
    }
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
            self.name_internal,
            self.result
                .format(ast, false)
                .map_err(|e| Error::FailedToFormatFunction {
                    name: self.name_internal.clone(),
                    source: Box::new(e)
                })?
        );
        if self.name_internal != self.name_external {
            println!("#define {} {}", self.name_external, self.name_internal);
        }

        Ok(())
    }

    /// Does this function have a return value?
    pub fn has_return_value(&self) -> bool {
        !self.arguments.is_empty() && self.arguments[0].is_result
    }

    /// Get the function's return value, if it has one
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

    /// Returns true if this function represents a cpp method and the class it is a member of is bound as opaqueptr,
    /// false otherwise
    pub fn parent_class_is_opaqueptr(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::OpaquePtr
                )
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::OpaquePtr
                )
            }
        }
    }

    /// Returns true if this function represents a cpp method and the class it is a member of is bound as opaquebytes,
    /// false otherwise
    pub fn parent_class_is_opaquebytes(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::OpaqueBytes
                )
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::OpaqueBytes
                )
            }
        }
    }

    /// Returns true if this function represents a cpp method and the class it is a member of is bound as valuetype,
    /// false otherwise
    pub fn parent_class_is_valuetype(&self, ast: &AST) -> bool {
        match self.source {
            CFunctionSource::Function(_) => false,
            CFunctionSource::Method((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::ValueType
                )
            }
            CFunctionSource::SpecializedMethod((class_id, _)) => {
                matches!(
                    ast.classes()[class_id].bind_kind(),
                    ClassBindKind::ValueType
                )
            }
        }
    }
}

pub struct CFunctionProto {
    pub name: String,
    pub usr: USR,
    pub result: CQualType,
    pub args: Vec<CQualType>,
}

impl std::fmt::Debug for CFunctionProto {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CFunctionProto {} {} ({:?}*)({:?})",
            self.name, self.usr, self.result, self.args
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CFunctionProtoId(usize);

impl CFunctionProtoId {
    pub fn new(id: usize) -> CFunctionProtoId {
        CFunctionProtoId(id)
    }
}

impl IndexMapKey for CFunctionProtoId {
    fn get(&self) -> usize {
        self.0
    }
}

#[instrument(skip(used_argument_names), level = "trace")]
pub fn translate_arguments(
    arguments: &[Argument],
    template_parms: &[TemplateParameterDecl],
    template_args: &[TemplateArgument],
    used_argument_names: &mut HashSet<String>,
    type_replacements: &TypeReplacements,
    ast: &AST,
) -> Result<(Vec<CArgument>, Vec<String>), Error> {
    trace!("Translating arguments {:?}", arguments);
    let mut result = Vec::new();

    let mut used_template_parameters = Vec::new();
    for arg in arguments {
        if let Some(parm) = arg.qual_type().template_parameter_name() {
            used_template_parameters.push(parm.to_string());
        }

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

    Ok((result, used_template_parameters))
}

#[instrument(skip(ast, functions, used_names), level = "trace")]
pub fn translate_function(
    ast: &AST,
    function_id: FunctionId,
    function: &Function,
    functions: &mut UstrIndexMap<CFunction, CFunctionId>,
    used_names: &mut HashSet<String>,
    template_args: &[TemplateArgument],
    type_replacements: &TypeReplacements,
) -> Result<()> {
    // build the namespace prefix
    trace!("Translating function {:?}", function);
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, function.namespaces())?;

    let source = CFunctionSource::Function(function_id);

    let mut body = Vec::new();

    // get the return type
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

    let (mut arguments, used_template_parameter_names) = translate_arguments(
        function.arguments(),
        function.template_parameters(),
        template_args,
        &mut used_argument_names,
        type_replacements,
        ast,
    )
    .map_err(|e| Error::TranslateFunction {
        name: function.name().to_string(),
        source: Box::new(e),
    })?;

    let mut unused_template_parameters = Vec::new();
    // record which method template parameters are unused in the arguments (if any)
    'outer: for p in function.template_parameters() {
        for n in &used_template_parameter_names {
            if n == p.name() {
                continue 'outer;
            }
        }

        unused_template_parameters.push(p.name().to_string());
    }

    // for each unused template parameter, find its matching argument and create a token for it
    let call_template_arguments = unused_template_parameters
        .iter()
        .map(|parm| {
            let parm_index = function
                .template_parameters()
                .iter()
                .position(|p| p.name() == parm)
                .ok_or_else(|| Error::TemplateParmNotFound {
                    name: parm.into(),
                    backtrace: Backtrace::new(),
                })?;

            if parm_index >= template_args.len() {
                Err(Error::TemplateArgNotFound {
                    name: parm.into(),
                    backtrace: Backtrace::new(),
                })
            } else {
                match &template_args[parm_index] {
                    TemplateArgument::Type(tty) => {
                        // Ok(Expr::Token(tty.name.clone()))
                        Ok(Expr::Token(tty.format(ast, &[], Some(template_args))))
                    }
                    TemplateArgument::Integral(n) => Ok(Expr::Token(format!("{n}"))),
                    _ => Err(Error::InvalidTemplateArgumentKind {
                        name: parm.into(),
                        backtrace: Backtrace::new(),
                    }),
                }
            }
        })
        .collect::<Result<Vec<Expr>>>()?;

    // For each argument to the original cpp function, modify its c counterpart to handle opaque api and generate the
    // necessary expressions to cast and deref it correctly back to cpp for passing to the original function.
    //
    // The rules are as follows. Given an argument `Type value`:
    //
    // + Pass by value
    //      o builtin
    //          - just pass it:
    //              `value`
    //      o valuetype
    //          - cast the value to its cpp counterpart by taking its address, casting the pointer, then deref:
    //              `*((Type*)&value)`
    //      o opaqueptr
    //          - we can only pass the CType by pointer, so wrap the CType in an extra pointer, then cast and deref:
    //              `*((Type*)value)`
    // + Pass by pointer
    //      o builtin:
    //          - just pass it:
    //              `value`
    //      o valuetype/opaqueptr
    //          - No changes needed, just cast the pointer:
    //              `(Type*)value
    // + Pass by lvalue/rvalue reference
    //      o builtin:
    //         - add a deref:
    //              `*value`
    //      o valuetype/opaqueptr
    //          - translate_qual_type will have converted the reference to a pointer already, so check the cpp_type_ref to
    //            see if it was a reference. If it was, then cast the pointer and deref:
    //              `*((Type*)value)`
    //
    let mut arg_pass = Vec::new();
    for arg in &mut arguments {
        match arg.qual_type.type_ref() {
            CTypeRef::Builtin(_) | CTypeRef::FunctionProto { .. } => {
                // builtin and function pointers just pass by value
                arg_pass.push(Expr::Token(arg.name.clone()))
            }
            CTypeRef::Pointer(_) => {
                if matches!(
                    arg.qual_type.cpp_type_ref(),
                    TypeRef::LValueReference(_) | TypeRef::RValueReference(_)
                ) {
                    // pointer converted from a reference - cast and deref
                    let to_type = get_cpp_cast_expr(&arg.qual_type, ast)?;
                    let cast = Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(arg.name.clone())),
                    };
                    arg_pass.push(Expr::Deref {
                        value: Box::new(cast),
                    });
                } else {
                    // just a regular pointer
                    let to_type = get_cpp_cast_expr(&arg.qual_type, ast)?;
                    arg_pass.push(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(arg.name.clone())),
                    });
                }
            }
            CTypeRef::Ref(usr) => {
                match get_bind_kind(*usr, ast)? {
                    // user type pass by value. if it's opaqueptr, wrap in a pointer and deref...
                    ClassBindKind::OpaquePtr => {
                        arg.qual_type = CQualType::pointer(
                            &format!("{}*", arg.qual_type.name()),
                            arg.qual_type.cpp_type_ref().clone(),
                            arg.qual_type.clone(),
                            false,
                        );

                        arg_pass.push(Expr::Deref {
                            value: Box::new(Expr::Cast {
                                to_type: get_cpp_cast_expr(&arg.qual_type, ast)?,
                                value: Box::new(Expr::Token(arg.name.clone())),
                            }),
                        });
                    }
                    ClassBindKind::ValueType => {
                        // take the address, cast it, then deref
                        let value = Expr::Token(arg.name.clone());
                        let addr = Expr::AddrOf {
                            value: Box::new(value),
                        };
                        let tmp_qual_type = CQualType::pointer(
                            &format!("{}*", arg.qual_type.name()),
                            arg.qual_type.cpp_type_ref().clone(),
                            arg.qual_type.clone(),
                            false,
                        );
                        let to_type = get_cpp_cast_expr(&tmp_qual_type, ast)?;
                        arg_pass.push(Expr::Deref {
                            value: Box::new(Expr::Cast {
                                to_type,
                                value: Box::new(addr),
                            }),
                        });
                    }
                    _ => todo!("Handle opaquebytes"),
                }
            }
            CTypeRef::Unknown(tk) => {
                panic!("unknown type {tk} when converting argument {}", arg.name);
            }
        }
    }

    // insert the result as the first argument, since we'll be forcing an int return type in order to return exception
    // status. Here we also do the necessary conversion to out pointers depending on the bind kind of the type in question
    //
    // Rules
    //
    // - If the return type is a pointer:
    //      o Add an extra pointer around it and tag the outer pointer as needing a deref
    //        i.e. `*(Type**) result) = cpp()`
    //
    // - If the return type is a reference:
    //      o Add an extra pointer around the translated pointer. The outer pointer needs a deref and the inner
    //        pointer needs the address of the cpp expression taken,
    //        i.e. `*((Type**) result) = &cpp()`
    //
    // - If the return type is a value and kind is valuetype
    //      o Add an extra pointer and tag as deref and move
    //        i.e. `*((Type*) result) = std::move(cpp())`
    //
    // - If the return type is a value and kind is opaqueptr and the method is NOT a constructor
    //      o Add an extra pointer around it and tag as deref
    //        i.e. `*((Type*) result) = std::move(cpp())`
    //
    // - If the return is a value and kind is opaqueptr and the method IS a constructor
    //      o Add an extra pointer around it and tag as deref, insert new expression
    //
    let assign_expr_fn = if !matches!(result.type_ref, CTypeRef::Builtin(TypeKind::Void)) {
        let result_name = get_unique_argument_name("result", &mut used_argument_names);

        let (result_qt, result_expr) = match result.type_ref() {
            CTypeRef::Builtin(_) | CTypeRef::FunctionProto { .. } => {
                // add a pointer to the result, then deref when assigning
                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                (
                    result_qt,
                    Box::new(|call_expr| Expr::Assignment {
                        left: Box::new(result_expr),
                        right: Box::new(call_expr),
                    }) as Box<dyn FnOnce(Expr) -> Expr>,
                )
            }
            CTypeRef::Pointer(_) => {
                let cpp_type_ref = result.cpp_type_ref.clone();

                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                if matches!(
                    cpp_type_ref,
                    TypeRef::LValueReference(_) | TypeRef::RValueReference(_)
                ) {
                    // pointer converted from a reference
                    (
                        result_qt,
                        Box::new(|call_expr| Expr::Assignment {
                            left: Box::new(result_expr),
                            right: Box::new(Expr::AddrOf {
                                value: Box::new(call_expr),
                            }),
                        }) as Box<dyn FnOnce(Expr) -> Expr>,
                    )
                } else {
                    (
                        result_qt,
                        Box::new(|call_expr| Expr::Assignment {
                            left: Box::new(result_expr),
                            right: Box::new(call_expr),
                        }) as Box<dyn FnOnce(Expr) -> Expr>,
                    )
                }
            }
            CTypeRef::Ref(_) => {
                // pass by value of a user type
                // add a pointer to the result, then deref when assigning
                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                (
                    result_qt,
                    Box::new(|call_expr| Expr::Assignment {
                        left: Box::new(result_expr),
                        right: Box::new(Expr::Move(Box::new(call_expr))),
                    }) as Box<dyn FnOnce(Expr) -> Expr>,
                )
            }
            CTypeRef::Unknown(tk) => {
                panic!("Unkown type {tk} while converting result")
            }
        };

        // push the modified argument to the front of the list
        arguments.insert(
            0,
            CArgument {
                name: result_name,
                qual_type: result_qt,
                is_self: false,
                is_result: false,
            },
        );

        Some(result_expr)
    } else {
        None
    };

    let call_expr = Expr::CppFunctionCall {
        function: function.get_qualified_name(ast)?,
        template_arguments: call_template_arguments,
        arguments: arg_pass,
    };

    let inner_block = if let Some(assign_expr_fn) = assign_expr_fn {
        assign_expr_fn(call_expr)
    } else {
        call_expr
    };

    let is_noexcept = function.exception_specification_kind().is_noexcept();

    // generate the try/catch block if this function is not noexcept
    if !is_noexcept {
        let trycatch_expr = Expr::TryCatch {
            try_block: Box::new(inner_block),
            catch_blocks: vec![Catch {
                exception: Expr::Token("std::exception& e".into()),
                stmt: Expr::Return(Box::new(Expr::Token("1".into()))),
            }],
        };

        body.push(trycatch_expr);
    } else {
        body.push(inner_block);
    }

    // finally, push the return statement
    body.push(Expr::Return(Box::new(Expr::Token("0".into()))));

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
            name_internal: fn_name_private,
            name_external: fn_name_public,
            // force an integer return for the error code
            result: CQualType {
                name: "[result]".to_string(),
                cpp_type_ref: TypeRef::Builtin(TypeKind::Int),
                type_ref: CTypeRef::Builtin(TypeKind::Int),
                is_const: false,
            },
            arguments,
            source,
            method_info: None,
            is_noexcept: false,
            body: Expr::Compound(body),
        },
    );

    Ok(())
}

// TODO(AL): This function is a monstrosity. We'll need to refactor it into chunks to be able to reuse pieces for
// translate_function, which does almost the same thing.
#[allow(clippy::too_many_arguments)]
#[instrument(skip(used_names, ast), level = "trace")]
pub fn translate_method(
    class: &ClassDecl,
    class_template_parms: &[TemplateParameterDecl],
    template_args: &[TemplateArgument],
    source: CFunctionSource,
    method: &Method,
    class_qname: &str,
    st_prefix_public: &str,
    st_prefix_private: &str,
    st_c_name_private: &str,
    used_names: &mut HashSet<String>,
    ast: &AST,
    type_replacements: &TypeReplacements,
    specialization_name: Option<&str>,
) -> Result<CFunction> {
    // Concatenate both the class template parameters and any template parameters on the function
    let template_parms = method
        .template_parameters()
        .iter()
        .cloned()
        .chain(class_template_parms.iter().cloned())
        .collect::<Vec<_>>();

    let mut body = Vec::new();

    // get the return type
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

    let mut used_argument_names = HashSet::new();

    let mut unused_template_parameters = Vec::new();

    let (mut arguments, used_template_parameter_names) = translate_arguments(
        method.arguments(),
        &template_parms,
        template_args,
        &mut used_argument_names,
        type_replacements,
        ast,
    )
    .map_err(|e| Error::TranslateFunction {
        name: format!("{}::{}", class.name(), method.name()),
        source: Box::new(e),
    })?;

    // record which method template parameters are unused in the arguments (if any)
    'outer: for p in method.template_parameters() {
        for n in &used_template_parameter_names {
            if n == p.name() {
                continue 'outer;
            }
        }

        unused_template_parameters.push(p.name().to_string());
    }

    // for each unused template parameter, find its matching argument and create a token for it
    let call_template_arguments = unused_template_parameters
        .iter()
        .map(|parm| {
            let parm_index = method
                .template_parameters()
                .iter()
                .position(|p| p.name() == parm)
                .ok_or_else(|| Error::TemplateParmNotFound {
                    name: parm.into(),
                    backtrace: Backtrace::new(),
                })?;

            if parm_index >= template_args.len() {
                Err(Error::TemplateArgNotFound {
                    name: parm.into(),
                    backtrace: Backtrace::new(),
                })
            } else {
                match &template_args[parm_index] {
                    TemplateArgument::Type(tty) => Ok(Expr::Token(tty.format(
                        ast,
                        class_template_parms,
                        Some(template_args),
                    ))),
                    TemplateArgument::Integral(n) => Ok(Expr::Token(format!("{n}"))),
                    _ => Err(Error::InvalidTemplateArgumentKind {
                        name: parm.into(),
                        backtrace: Backtrace::new(),
                    }),
                }
            }
        })
        .collect::<Result<Vec<Expr>>>()?;

    // For each argument to the original cpp function, modify its c counterpart to handle opaque api and generate the
    // necessary expressions to cast and deref it correctly back to cpp for passing to the original function.
    //
    // The rules are as follows. Given an argument `Type value`:
    //
    // + Pass by value
    //      o builtin
    //          - just pass it:
    //              `value`
    //      o valuetype
    //          - cast the value to its cpp counterpart by taking its address, casting the pointer, then deref:
    //              `*((Type*)&value)`
    //      o opaqueptr
    //          - we can only pass the CType by pointer, so wrap the CType in an extra pointer, then cast and deref:
    //              `*((Type*)value)`
    // + Pass by pointer
    //      o builtin:
    //          - just pass it:
    //              `value`
    //      o valuetype/opaqueptr
    //          - No changes needed, just cast the pointer:
    //              `(Type*)value
    // + Pass by lvalue/rvalue reference
    //      o builtin:
    //         - add a deref:
    //              `*value`
    //      o valuetype/opaqueptr
    //          - translate_qual_type will have converted the reference to a pointer already, so check the cpp_type_ref to
    //            see if it was a reference. If it was, then cast the pointer and deref:
    //              `*((Type*)value)`
    //
    let mut arg_pass = Vec::new();
    for arg in &mut arguments {
        match arg.qual_type.type_ref() {
            CTypeRef::Builtin(_) | CTypeRef::FunctionProto { .. } => {
                // builtin pass by value
                arg_pass.push(Expr::Token(arg.name.clone()))
            }
            CTypeRef::Pointer(_) => {
                if matches!(
                    arg.qual_type.cpp_type_ref(),
                    TypeRef::LValueReference(_) | TypeRef::RValueReference(_)
                ) {
                    // pointer converted from a reference - cast and deref
                    let to_type = get_cpp_cast_expr(&arg.qual_type, ast)?;
                    let cast = Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(arg.name.clone())),
                    };
                    arg_pass.push(Expr::Deref {
                        value: Box::new(cast),
                    });
                } else {
                    // just a regular pointer
                    let to_type = get_cpp_cast_expr(&arg.qual_type, ast)?;
                    arg_pass.push(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(arg.name.clone())),
                    });
                }
            }
            CTypeRef::Ref(usr) => {
                match get_bind_kind(*usr, ast)? {
                    // user type pass by value. if it's opaqueptr, wrap in a pointer and deref...
                    ClassBindKind::OpaquePtr => {
                        arg.qual_type = CQualType::pointer(
                            &format!("{}*", arg.qual_type.name()),
                            arg.qual_type.cpp_type_ref().clone(),
                            arg.qual_type.clone(),
                            false,
                        );

                        arg_pass.push(Expr::Deref {
                            value: Box::new(Expr::Cast {
                                to_type: get_cpp_cast_expr(&arg.qual_type, ast)?,
                                value: Box::new(Expr::Token(arg.name.clone())),
                            }),
                        });
                    }
                    ClassBindKind::ValueType => {
                        // take the address, cast it, then deref
                        let value = Expr::Token(arg.name.clone());
                        let addr = Expr::AddrOf {
                            value: Box::new(value),
                        };
                        let tmp_qual_type = CQualType::pointer(
                            &format!("{}*", arg.qual_type.name()),
                            arg.qual_type.cpp_type_ref().clone(),
                            arg.qual_type.clone(),
                            false,
                        );
                        let to_type = get_cpp_cast_expr(&tmp_qual_type, ast)?;
                        arg_pass.push(Expr::Deref {
                            value: Box::new(Expr::Cast {
                                to_type,
                                value: Box::new(addr),
                            }),
                        });
                    }
                    _ => todo!("Handle opaquebytes"),
                }
            }
            CTypeRef::Unknown(tk) => {
                panic!("unknown type {tk} when converting argument {}", arg.name);
            }
        }
    }

    // insert the result as the first argument, since we'll be forcing an int return type in order to return exception
    // status. Here we also do the necessary conversion to out pointers depending on the bind kind of the type in question
    //
    // Rules
    //
    // - If the return type is a pointer:
    //      o Add an extra pointer around it and tag the outer pointer as needing a deref
    //        i.e. `*(Type**) result) = cpp()`
    //
    // - If the return type is a reference:
    //      o Add an extra pointer around the translated pointer. The outer pointer needs a deref and the inner
    //        pointer needs the address of the cpp expression taken,
    //        i.e. `*((Type**) result) = &cpp()`
    //
    // - If the return type is a value and kind is valuetype
    //      o Add an extra pointer and tag as deref and move
    //        i.e. `*((Type*) result) = std::move(cpp())`
    //
    // - If the return type is a value and kind is opaqueptr and the method is NOT a constructor
    //      o Add an extra pointer around it and tag as deref
    //        i.e. `*((Type*) result) = std::move(cpp())`
    //
    // - If the return is a value and kind is opaqueptr and the method IS a constructor
    //      o Add an extra pointer around it and tag as deref, insert new expression
    //
    let assign_expr_fn = if !matches!(result.type_ref, CTypeRef::Builtin(TypeKind::Void)) {
        let result_name = get_unique_argument_name("result", &mut used_argument_names);

        let (result_qt, result_expr) = match result.type_ref() {
            CTypeRef::Builtin(_) | CTypeRef::FunctionProto { .. } => {
                // add a pointer to the result, then deref when assigning
                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                (
                    result_qt,
                    Box::new(|call_expr| Expr::Assignment {
                        left: Box::new(result_expr),
                        right: Box::new(call_expr),
                    }) as Box<dyn FnOnce(Expr) -> Expr>,
                )
            }
            CTypeRef::Pointer(_) => {
                let cpp_type_ref = result.cpp_type_ref.clone();

                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                if matches!(
                    cpp_type_ref,
                    TypeRef::LValueReference(_) | TypeRef::RValueReference(_)
                ) {
                    // pointer converted from a reference
                    (
                        result_qt,
                        Box::new(|call_expr| Expr::Assignment {
                            left: Box::new(result_expr),
                            right: Box::new(Expr::AddrOf {
                                value: Box::new(call_expr),
                            }),
                        }) as Box<dyn FnOnce(Expr) -> Expr>,
                    )
                } else {
                    (
                        result_qt,
                        Box::new(|call_expr| Expr::Assignment {
                            left: Box::new(result_expr),
                            right: Box::new(call_expr),
                        }) as Box<dyn FnOnce(Expr) -> Expr>,
                    )
                }
            }
            CTypeRef::Ref(_) => {
                // pass by value of a user type
                // add a pointer to the result, then deref when assigning
                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: result.cpp_type_ref().clone(),
                    type_ref: CTypeRef::Pointer(Box::new(result)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                (
                    result_qt,
                    Box::new(|call_expr| Expr::Assignment {
                        left: Box::new(result_expr),
                        right: Box::new(Expr::Move(Box::new(call_expr))),
                    }) as Box<dyn FnOnce(Expr) -> Expr>,
                )
            }
            CTypeRef::Unknown(tk) => {
                panic!("Unkown type {tk} while converting result")
            }
        };

        // push the modified argument to the front of the list
        arguments.insert(
            0,
            CArgument {
                name: result_name,
                qual_type: result_qt,
                is_self: false,
                is_result: false,
            },
        );

        Some(result_expr)
    } else if method.is_any_constructor() {
        // we need to add a return type.
        // for value type this will just be an out pointer like a regular return
        // for opaqueptr, it will be a pointer to a pointer, and we'll call new on the cpp side
        let result_name = get_unique_argument_name("result", &mut used_argument_names);

        let usr = type_replacements.replace(class.usr());
        let cpp_qt = QualType::type_ref(class.name(), false, usr);
        let c_qt = CQualType {
            name: st_c_name_private.to_string(),
            type_ref: CTypeRef::Ref(usr),
            cpp_type_ref: TypeRef::Ref(usr),
            is_const: false,
        };

        match class.bind_kind() {
            ClassBindKind::ValueType => {
                let result_qt = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: TypeRef::Pointer(Box::new(cpp_qt)),
                    type_ref: CTypeRef::Pointer(Box::new(c_qt)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                arguments.insert(
                    0,
                    CArgument {
                        name: result_name,
                        qual_type: result_qt,
                        is_self: false,
                        is_result: false,
                    },
                );

                Some(Box::new(|call_expr| Expr::Assignment {
                    left: Box::new(result_expr),
                    right: Box::new(call_expr),
                }) as Box<dyn FnOnce(Expr) -> Expr>)
            }
            ClassBindKind::OpaquePtr => {
                let cpp_ptr = QualType {
                    name: format!("{}*", result.name()),
                    type_ref: TypeRef::Pointer(Box::new(cpp_qt.clone())),
                    is_const: false,
                };

                let c_ptr = CQualType {
                    name: format!("{}*", result.name()),
                    cpp_type_ref: TypeRef::Pointer(Box::new(cpp_qt)),
                    type_ref: CTypeRef::Pointer(Box::new(c_qt)),
                    is_const: false,
                };

                let result_qt = CQualType {
                    name: format!("{}**", result.name()),
                    cpp_type_ref: TypeRef::Pointer(Box::new(cpp_ptr)),
                    type_ref: CTypeRef::Pointer(Box::new(c_ptr)),
                    is_const: false,
                };

                let to_type = get_cpp_cast_expr(&result_qt, ast)?;
                let result_expr = Expr::Deref {
                    value: Box::new(Expr::Cast {
                        to_type,
                        value: Box::new(Expr::Token(result_name.clone())),
                    }),
                };

                arguments.insert(
                    0,
                    CArgument {
                        name: result_name,
                        qual_type: result_qt,
                        is_self: false,
                        is_result: false,
                    },
                );

                Some(Box::new(|call_expr| Expr::Assignment {
                    left: Box::new(result_expr),
                    right: Box::new(Expr::New(Box::new(call_expr))),
                }) as Box<dyn FnOnce(Expr) -> Expr>)
            }
            ClassBindKind::OpaqueBytes => {
                todo!("Handle opaquebytes")
            }
        }
    } else {
        None
    };

    // insert self pointer ahead of all the arguments
    let inner_block = if method.is_static() {
        let call_expr = Expr::CppStaticMethodCall {
            receiver: Box::new(Expr::Token(class_qname.to_string())),
            function: method.name().to_string(),
            template_arguments: call_template_arguments,
            arguments: arg_pass,
        };
        if let Some(assign_expr_fn) = assign_expr_fn {
            assign_expr_fn(call_expr)
        } else {
            call_expr
        }
    } else if method.is_any_constructor() {
        let call_expr = Expr::CppConstructor {
            receiver: Box::new(Expr::Token(class_qname.to_string())),
            template_arguments: call_template_arguments,
            arguments: arg_pass,
        };
        if let Some(assign_expr_fn) = assign_expr_fn {
            assign_expr_fn(call_expr)
        } else {
            call_expr
        }
    } else if method.is_destructor() && get_bind_kind(class.usr(), ast)? == ClassBindKind::OpaquePtr
    {
        let qt = CQualType {
            name: format!("{}*", st_c_name_private),
            is_const: false,
            type_ref: CTypeRef::Pointer(Box::new(CQualType {
                name: st_c_name_private.into(),
                is_const: method.is_const(),
                type_ref: CTypeRef::Ref(type_replacements.replace(class.usr())),
                cpp_type_ref: TypeRef::Ref(class.usr()),
            })),
            cpp_type_ref: TypeRef::Pointer(Box::new(QualType {
                name: class.name().to_string(),
                is_const: method.is_const(),
                type_ref: TypeRef::Ref(class.usr()),
            })),
        };

        let to_type = get_cpp_cast_expr(&qt, ast)?;
        let self_name = get_unique_argument_name("this_", &mut used_argument_names);

        arguments.insert(
            0,
            CArgument {
                name: self_name.clone(),
                qual_type: qt,
                is_self: true,
                is_result: false,
            },
        );

        Expr::Delete(Box::new(Expr::Cast {
            to_type,
            value: Box::new(Expr::Token(self_name)),
        }))
    } else {
        // regular method
        let qt = CQualType {
            name: format!("{}*", st_c_name_private),
            is_const: false,
            type_ref: CTypeRef::Pointer(Box::new(CQualType {
                name: st_c_name_private.into(),
                is_const: method.is_const(),
                type_ref: CTypeRef::Ref(type_replacements.replace(class.usr())),
                cpp_type_ref: TypeRef::Ref(class.usr()),
            })),
            cpp_type_ref: TypeRef::Pointer(Box::new(QualType {
                name: class.name().to_string(),
                is_const: method.is_const(),
                type_ref: TypeRef::Ref(class.usr()),
            })),
        };

        let to_type = get_cpp_cast_expr(&qt, ast)?;
        let self_name = get_unique_argument_name("this_", &mut used_argument_names);

        arguments.insert(
            0,
            CArgument {
                name: self_name.clone(),
                qual_type: qt,
                is_self: true,
                is_result: false,
            },
        );

        let call_expr = Expr::CppMethodCall {
            receiver: Box::new(Expr::Cast {
                to_type,
                value: Box::new(Expr::Token(self_name)),
            }),
            function: method.name().to_string(),
            template_arguments: call_template_arguments,
            arguments: arg_pass,
        };

        // call the closure we might have created earlier to generate the assignment expression (if there's a return value)
        if let Some(assign_expr_fn) = assign_expr_fn {
            assign_expr_fn(call_expr)
        } else {
            call_expr
        }
    };

    let is_noexcept = method.exception_specification_kind().is_noexcept();

    // generate the try/catch block if this function is not noexcept
    if !is_noexcept {
        let trycatch_expr = Expr::TryCatch {
            try_block: Box::new(inner_block),
            catch_blocks: vec![Catch {
                exception: Expr::Token("std::exception& e".into()),
                stmt: Expr::Return(Box::new(Expr::Token("1".into()))),
            }],
        };

        body.push(trycatch_expr);
    } else {
        body.push(inner_block);
    }

    // finally, push the return statement
    body.push(Expr::Return(Box::new(Expr::Token("0".into()))));

    let fn_name = if let Some(name) = specialization_name {
        name
    } else if let Some(name) = method.replacement_name() {
        name
    } else {
        method.name()
    };

    let (fn_name_public, fn_name_private) =
        get_c_names(fn_name, st_prefix_public, st_prefix_private, used_names);

    Ok(CFunction {
        name_internal: fn_name_private,
        name_external: fn_name_public,
        result: CQualType::int("int", false),
        arguments,
        source,
        method_info: Some(MethodInfo {
            kind: method.kind(),
            class_qname: class_qname.to_string(),
        }),
        is_noexcept,
        body: Expr::Compound(body),
    })
}

#[instrument(skip(function_protos), level = "trace")]
pub fn translate_function_proto(
    proto: &FunctionProto,
    function_protos: &mut UstrIndexMap<CFunctionProto, CFunctionProtoId>,
) -> Result<()> {
    let name = proto.name().to_string();

    let result = translate_qual_type(proto.result(), &[], &[], &TypeReplacements::default())?;

    let args = proto
        .args()
        .iter()
        .map(|a| translate_qual_type(a, &[], &[], &TypeReplacements::default()))
        .collect::<Result<Vec<_>>>()?;

    function_protos.insert(
        proto.usr().into(),
        CFunctionProto {
            name,
            usr: proto.usr(),
            result,
            args,
        },
    );

    Ok(())
}

fn get_cpp_cast_expr(qt: &CQualType, ast: &AST) -> Result<String> {
    let result = match qt.type_ref() {
        CTypeRef::Builtin(tk) => Ok(builtin_spelling(tk)),
        CTypeRef::Pointer(pointee) => Ok(format!("{}*", get_cpp_cast_expr(pointee, ast)?)),
        CTypeRef::Ref(usr) => {
            // ref might be to a class directly, or to a typedef
            if let Some(class) = ast.get_class(*usr) {
                Ok(class.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        usr: class.usr(),
                        source: Box::new(source),
                    }
                })?)
            } else if let Some(cts) = ast.get_class_template_specialization(*usr) {
                Ok(cts.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        usr: cts.usr(),
                        source: Box::new(source),
                    }
                })?)
            } else if let Some(cts) = ast.get_type_alias(*usr) {
                Ok(cts.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        usr: cts.usr(),
                        source: Box::new(source),
                    }
                })?)
            } else if let Some(enm) = ast.get_enum(*usr) {
                Ok(enm.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        usr: enm.usr(),
                        source: Box::new(source),
                    }
                })?)
            } else {
                Err(Error::RefNotFound {
                    usr: *usr,
                    backtrace: Backtrace::new(),
                })
            }
        }
        _ => unreachable!(),
    }?;

    if qt.is_const() {
        Ok(format!("{} const", result))
    } else {
        Ok(result)
    }
}

fn get_bind_kind(usr: USR, ast: &AST) -> Result<ClassBindKind> {
    if let Some(class) = ast.get_class(usr) {
        Ok(*class.bind_kind())
    } else if let Some(cts) = ast.get_class_template_specialization(usr) {
        get_bind_kind(cts.specialized_decl(), ast)
    } else if let Some(ta) = ast.get_type_alias(usr) {
        ta.underlying_type()
            .get_bind_kind(ast)
            .map_err(|e| Error::FailedToGetBindKind {
                usr,
                source: Box::new(e),
            })
    } else if ast.get_enum(usr).is_some() || ast.get_function_proto(usr).is_some() {
        Ok(ClassBindKind::ValueType)
    } else {
        Err(Error::RefNotFound {
            usr,
            backtrace: Backtrace::new(),
        })
    }
}

fn builtin_spelling(tk: &TypeKind) -> String {
    match tk {
        TypeKind::Bool => "bool".to_string(),
        TypeKind::Char_S => "char".to_string(),
        TypeKind::Char_U => "unsigned char".to_string(),
        TypeKind::Double => "double".to_string(),
        TypeKind::Float => "float".to_string(),
        TypeKind::Int => "int".to_string(),
        TypeKind::Long => "long".to_string(),
        TypeKind::LongDouble => "long double".to_string(),
        TypeKind::LongLong => "long long".to_string(),
        TypeKind::Short => "short".to_string(),
        TypeKind::UChar => "unsigned char".to_string(),
        TypeKind::UInt => "unsigned int".to_string(),
        TypeKind::ULong => "unsigned long".to_string(),
        TypeKind::ULongLong => "unsigned long long".to_string(),
        TypeKind::UShort => "unsigned short".to_string(),
        TypeKind::Void => "void".to_string(),
        _ => unimplemented!(),
    }
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

#[derive(Debug, Clone)]
pub enum Expr {
    Compound(Vec<Expr>),
    CppFunctionCall {
        function: String,
        template_arguments: Vec<Expr>,
        arguments: Vec<Expr>,
    },
    CppMethodCall {
        receiver: Box<Expr>,
        function: String,
        template_arguments: Vec<Expr>,
        arguments: Vec<Expr>,
    },
    CppStaticMethodCall {
        receiver: Box<Expr>,
        function: String,
        template_arguments: Vec<Expr>,
        arguments: Vec<Expr>,
    },
    CppConstructor {
        receiver: Box<Expr>,
        template_arguments: Vec<Expr>,
        arguments: Vec<Expr>,
    },
    CppDestructor {
        receiver: Box<Expr>,
    },
    Assignment {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Cast {
        to_type: String,
        value: Box<Expr>,
    },
    Deref {
        value: Box<Expr>,
    },
    Ref {
        value: Box<Expr>,
    },
    AddrOf {
        value: Box<Expr>,
    },
    Move(Box<Expr>),
    New(Box<Expr>),
    Delete(Box<Expr>),
    Return(Box<Expr>),
    Token(String),
    TryCatch {
        try_block: Box<Expr>,
        catch_blocks: Vec<Catch>,
    },
}

#[derive(Debug, Clone)]
pub struct Catch {
    pub exception: Expr,
    pub stmt: Expr,
}
