use std::fmt::Display;

use bbl_clang::cursor::USR;
use bbl_clang::ty::TypeKind;
use bbl_extract::function::Function;
use bbl_extract::type_alias::{ClassTemplateSpecialization, FunctionTemplateSpecialization};
use hashbrown::HashSet;
use log::{error, warn};

use bbl_extract::class::MethodSpecializationId;
use bbl_extract::error::{Error, TranslateArgumentError, TranslateTypeError};
type Result<T, E = Error> = std::result::Result<T, E>;

use bbl_extract::template_argument::TemplateParameterDecl;
use bbl_extract::{
    ast::{ClassId, FunctionId, MethodId, UstrIndexMap, AST},
    class::{ClassBindKind, ClassDecl},
    function::{Argument, Method},
    qualtype::{QualType, TypeRef},
    template_argument::TemplateType,
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
    needs_deref: bool,
    needs_move: bool,
}

impl CQualType {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn type_ref(&self) -> &CTypeRef {
        &self.type_ref
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.type_ref, CTypeRef::Pointer(_))
    }

    pub fn needs_deref(&self) -> bool {
        self.needs_deref
    }

    pub fn needs_move(&self) -> bool {
        self.needs_move
    }

    pub fn cpp_type_ref(&self) -> &TypeRef {
        &self.cpp_type_ref
    }

    pub fn unknown(tk: TypeKind) -> Self {
        CQualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            needs_deref: false,
            needs_move: false,
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
                    .map(|r| r.format())
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
    pub name: String,
    pub qual_type: CQualType,
    pub is_self: bool,
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
}

impl CFunction {
    pub fn pretty_print(&self, _depth: usize, ast: &CAST) {
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
    pub fn format(&self) -> String {
        format!("struct {}", self.name_private)
    }

    pub fn pretty_print(&self, _depth: usize, ast: &CAST) {
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
        self.structs.get(&usr.into())
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
) -> Result<CQualType, TranslateTypeError> {
    match &qual_type.type_ref {
        TypeRef::Builtin(tk) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Builtin(*tk),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: false,
        }),
        TypeRef::Pointer(qt) => {
            Ok(CQualType {
                name: qual_type.name.clone(),
                is_const: qual_type.is_const,
                type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                    qt.as_ref(),
                    template_parms,
                    template_args,
                )?)),
                cpp_type_ref: qual_type.type_ref.clone(),
                needs_deref: false,
                needs_move: false,
            })
        }
        TypeRef::LValueReference(qt) => {
            Ok(CQualType {
                name: qual_type.name.clone(),
                is_const: qual_type.is_const,
                type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                    qt.as_ref(),
                    template_parms,
                    template_args,
                )?)),
                cpp_type_ref: qual_type.type_ref.clone(),
                needs_deref: true,
                needs_move: false,
            })
        }
        TypeRef::RValueReference(qt) => {
            Ok(CQualType {
                name: qual_type.name.clone(),
                is_const: qual_type.is_const,
                type_ref: CTypeRef::Pointer(Box::new(translate_qual_type(
                    qt.as_ref(),
                    template_parms,
                    template_args,
                )?)),
                cpp_type_ref: qual_type.type_ref.clone(),
                needs_deref: false,
                needs_move: true,
            })
        }
        TypeRef::Ref(usr) => Ok(CQualType {
            name: qual_type.name.clone(),
            is_const: qual_type.is_const,
            type_ref: CTypeRef::Ref(*usr),
            cpp_type_ref: qual_type.type_ref.clone(),
            needs_deref: false,
            needs_move: false,
        }),
        TypeRef::TemplateTypeParameter(parm_name) => {
            // find the parameter with the given name in the params list, then get the matching arg here
            let parm_index = template_parms
                .iter()
                .position(|p| p.name() == parm_name)
                .ok_or(TranslateTypeError::TemplateParmNotFound(parm_name.into()))?;

            if parm_index >= template_args.len() {
                Err(TranslateTypeError::TemplateArgNotFound(parm_name.into()))
            } else {
                match &template_args[parm_index] {
                    Some(TemplateType::Type(tty)) => {
                        translate_qual_type(tty, template_parms, template_args)
                    }
                    Some(TemplateType::Integer(n)) => {
                        todo!()
                    }
                    None => {
                        return Err(TranslateTypeError::InvalidTemplateArgumentKind(
                            parm_name.into(),
                        ))
                    }
                }
            }
        }
        _ => todo!(),
    }
}

pub fn translate_arguments(
    arguments: &[Argument],
    template_parms: &[TemplateParameterDecl],
    template_args: &[Option<TemplateType>],
    used_argument_names: &mut HashSet<String>,
) -> Result<Vec<CArgument>, TranslateArgumentError> {
    let mut result = Vec::new();

    for arg in arguments {
        let qual_type = translate_qual_type(&arg.qual_type(), template_parms, template_args)
            .map_err(|e| {
                error!("Error translating argument {}: {e}", arg.name());
                TranslateArgumentError {
                    name: arg.name().to_string(),
                    source: e,
                }
            })?;

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
        });
    }

    Ok(result)
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
    let mut functions = UstrIndexMap::new();

    let mut used_names = HashSet::new();

    for (class_id, class) in ast.classes().iter().enumerate() {
        // if this is a template class, we'll ignore it and translate its specializations instead
        // TODO (AL): do we want to use a separate type for ClassTemplate again?
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
        )?;
    }

    for (type_alias_id, type_alias) in ast.type_aliases().iter().enumerate() {
        match type_alias {
            TypeAlias::ClassTemplateSpecialization(cts) => translate_class_template_specialization(
                ast,
                cts,
                &mut structs,
                &mut functions,
                &mut used_names,
            )?,
            TypeAlias::FunctionTemplateSpecialization(fts) => {
                translate_function_template_specialization(
                    ast,
                    fts,
                    &mut functions,
                    &mut used_names,
                )?
            }
            _ => todo!(),
        }
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
        )?;
    }

    Ok(CAST { structs, functions })
}

fn translate_function_template_specialization(
    ast: &AST,
    fts: &FunctionTemplateSpecialization,
    functions: &mut UstrIndexMap<CFunction>,
    used_names: &mut HashSet<String>,
) -> Result<()> {
    let function_id = ast
        .functions()
        .get_id(&fts.specialized_decl().into())
        .ok_or(Error::FunctionNotFound(fts.specialized_decl().to_string()))?;
    let function = ast.functions().index(*function_id);

    translate_function(
        ast,
        FunctionId::new(*function_id),
        function,
        functions,
        used_names,
        fts.template_arguments(),
    )?;

    Ok(())
}

fn translate_class_template_specialization(
    ast: &AST,
    cts: &ClassTemplateSpecialization,
    structs: &mut UstrIndexMap<CStruct>,
    functions: &mut UstrIndexMap<CFunction>,
    used_names: &mut HashSet<String>,
) -> Result<()> {
    let class_id = ast
        .classes()
        .get_id(&cts.specialized_decl().into())
        .ok_or(Error::RecordNotFound)?;
    let class = ast.classes().index(*class_id);

    translate_class(
        ast,
        ClassId::new(*class_id),
        class,
        cts.template_arguments(),
        structs,
        functions,
        used_names,
    )?;

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
    let (ns_prefix_public, ns_prefix_private) = build_namespace_prefix(ast, &class.namespaces())?;

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
            &class.template_parameters(),
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
            name_public: st_c_name_public.clone(),
            name_private: st_c_name_private.clone(),
            fields,
            bind_kind: *class.bind_kind(),
            class_id: class_id,
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
        )?;

        functions.insert(method.usr().into(), c_function);
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

fn translate_function(
    ast: &AST,
    function_id: FunctionId,
    function: &Function,
    functions: &mut UstrIndexMap<CFunction>,
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
        source: TranslateArgumentError {
            name: "[return]".into(),
            source: e,
        },
    })?;

    let mut used_argument_names = HashSet::new();

    let arguments = translate_arguments(
        function.arguments(),
        function.template_parameters(),
        template_args,
        &mut used_argument_names,
    )
    .map_err(|e| Error::TranslateFunction {
        name: function.name().to_string(),
        source: e,
    })?;

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
            result,
            arguments,
            source,
        },
    );

    Ok(())
}

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
) -> Result<CFunction> {
    // Concatenate both the class template parameters and any template parameters on the function
    let template_parms = method
        .template_parameters()
        .iter()
        .cloned()
        .chain(class_template_parms.iter().cloned())
        .collect::<Vec<_>>();

    let result =
        translate_qual_type(&method.result(), &template_parms, template_args).map_err(|e| {
            Error::TranslateFunction {
                name: format!("{}::{}", class.name(), method.name()),
                source: TranslateArgumentError {
                    name: "[return]".into(),
                    source: e,
                },
            }
        })?;

    let mut used_argument_names = HashSet::new();

    let mut arguments = translate_arguments(
        method.arguments(),
        &template_parms,
        template_args,
        &mut used_argument_names,
    )
    .map_err(|e| Error::TranslateFunction {
        name: format!("{}::{}", class.name(), method.name()),
        source: e,
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
            })),
            cpp_type_ref: TypeRef::Pointer(Box::new(QualType {
                name: class.name().to_string(),
                is_const: method.is_const(),
                type_ref: TypeRef::Ref(class.usr()),
            })),
            needs_deref: false,
            needs_move: false,
        };
        arguments.insert(
            0,
            CArgument {
                name: get_unique_argument_name("self", &mut used_argument_names),
                qual_type: qt,
                is_self: true,
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
        result,
        arguments,
        source,
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
