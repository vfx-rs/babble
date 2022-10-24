use bbl_clang::cursor::{Cursor, USR};
use bbl_clang::exception::ExceptionSpecificationKind;
use bbl_clang::translation_unit::TranslationUnit;
use bbl_util::Trace;
use hashbrown::HashSet;
use log::*;
use std::fmt::{Debug, Display};
use tracing::instrument;

use crate::ast::{
    get_namespaces_for_decl, get_qualified_name, FunctionTemplateSpecializationId, MethodId,
};
use crate::class::{MethodSpecializationId, OverrideList};
use crate::index_map::IndexMapKey;
use crate::qualtype::extract_type;
use crate::templates::{TemplateArgument, TemplateParameterDecl};
use crate::AllowList;
use crate::{ast::AST, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, PartialEq, Eq)]
pub struct FunctionProto {
    name: String,
    usr: USR,
    result: QualType,
    args: Vec<QualType>,
    namespaces: Vec<USR>,
}

impl FunctionProto {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn result(&self) -> &QualType {
        &self.result
    }

    pub fn args(&self) -> &[QualType] {
        &self.args
    }

    pub fn new(
        name: String,
        usr: USR,
        result: QualType,
        args: Vec<QualType>,
        namespaces: Vec<USR>,
    ) -> Self {
        FunctionProto {
            name,
            usr,
            result,
            args,
            namespaces,
        }
    }
}

impl std::fmt::Debug for FunctionProto {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FunctionProto {} {} ({:?}*)({:?}) namespaces={:?}",
            self.name, self.usr, self.result, self.args, self.namespaces
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FunctionProtoId(usize);

impl FunctionProtoId {
    pub fn new(id: usize) -> FunctionProtoId {
        FunctionProtoId(id)
    }
}

impl IndexMapKey for FunctionProtoId {
    fn get(&self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Argument {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Debug for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name, self.qual_type)
    }
}

impl Argument {
    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[TemplateArgument]>,
    ) -> String {
        format!(
            "{}: {}",
            self.name,
            self.qual_type
                .format(ast, class_template_parameters, class_template_args)
        )
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn qual_type(&self) -> &QualType {
        &self.qual_type
    }

    pub fn new(name: &str, qual_type: QualType) -> Argument {
        Argument {
            name: name.to_string(),
            qual_type,
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) result: QualType,
    pub(crate) arguments: Vec<Argument>,
    pub(crate) replacement_name: Option<String>,
    pub(crate) ignored: bool,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
    pub(crate) unused_template_arguments: Vec<TemplateArgument>,
    pub(crate) specializations: Vec<FunctionTemplateSpecializationId>,
    pub(crate) exception_specification_kind: ExceptionSpecificationKind,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Function {usr} {name}",
            usr = self.usr,
            name = self.name(),
        )?;

        write!(f, " rename={:?}", self.replacement_name())?;
        write!(f, " ignore={}", self.ignored)?;
        write!(f, " return={:?}", self.result())?;
        write!(f, " args={:?}", self.arguments())?;
        write!(f, " noexcept={:?}", self.exception_specification_kind)?;
        write!(f, " template_parameters={:?}", self.template_parameters())?;
        write!(f, " specializations={:?}", self.specializations)?;
        write!(f, " namespaces={:?}", self.namespaces)?;

        Ok(())
    }
}

impl Function {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        usr: USR,
        name: String,
        result: QualType,
        arguments: Vec<Argument>,
        replacement_name: Option<String>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        exception_specification_kind: ExceptionSpecificationKind,
    ) -> Self {
        Function {
            usr,
            name,
            result,
            arguments,
            replacement_name,
            ignored: false,
            namespaces,
            template_parameters,
            unused_template_arguments: Vec::new(),
            specializations: Vec::new(),
            exception_specification_kind,
        }
    }

    /// Does this function's signiature match `other`?
    ///
    /// In other words, is it identical, except for the USR and the namespaces?
    pub fn signature_matches(&self, other: &Function) -> bool {
        self.name == other.name
            && self.result == other.result
            && self.arguments == other.arguments
            && self.template_parameters == other.template_parameters
            && self.exception_specification_kind == other.exception_specification_kind
    }

    /// Get the name of the function
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Build the qualified name of the function, i.e. with all parent namespace and class scopes
    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }

    /// Get the return type of the function
    pub fn result(&self) -> &QualType {
        &self.result
    }

    /// Get the function's arguments
    pub fn arguments(&self) -> &[Argument] {
        &self.arguments
    }

    /// Get the replacement name, if one has been set
    pub fn replacement_name(&self) -> Option<&str> {
        self.replacement_name.as_deref()
    }

    /// Get the list of parent namespaces (including classes if this is a method)
    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    /// Get the list of template parameters
    pub fn template_parameters(&self) -> &[TemplateParameterDecl] {
        &self.template_parameters
    }

    /// Is this a template function?
    pub fn is_templated(&self) -> bool {
        !self.template_parameters.is_empty()
    }

    /// Has this function template had any specializations made from it?
    pub fn is_specialized(&self) -> bool {
        !self.specializations.is_empty()
    }

    /// What kind of exception specification does this function have?
    pub fn exception_specification_kind(&self) -> ExceptionSpecificationKind {
        self.exception_specification_kind
    }

    /// Get the unique identifier
    pub fn usr(&self) -> USR {
        self.usr
    }

    /// Set a replacement name for this function to be used when translating
    pub fn rename(&mut self, new_name: &str) {
        self.replacement_name = Some(new_name.to_string());
    }

    /// Set this function to be ignored when translating
    pub fn ignore(&mut self) {
        self.ignored = true;
    }

    /// Get the function's signature
    pub fn signature(&self) -> String {
        self.format()
    }

    pub fn format(&self) -> String {
        let mut s = self.name.to_string();

        let args = self
            .arguments
            .iter()
            // .map(|p| p.format(ast, outer_template_parameters, template_args))
            .map(|arg| arg.qual_type().name.clone())
            .collect::<Vec<String>>()
            .join(", ");
        s = format!("{s}({})", args);

        s = format!("{s} -> {}", self.result.name);

        s
    }

    pub fn unused_template_arguments(&self) -> &[TemplateArgument] {
        self.unused_template_arguments.as_ref()
    }
}

#[derive(Clone)]
pub struct Method {
    pub(crate) function: Function,
    pub(crate) kind: MethodKind,
    pub(crate) is_const: bool,
    pub(crate) is_virtual: bool,
    pub(crate) is_pure_virtual: bool,
    pub(crate) is_deleted: bool,
    pub(crate) specializations: Vec<MethodSpecializationId>,
}

impl Debug for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Method {:?} deleted={} const={} virtual={} pure_virtual={} specializations={:?} {:?}",
            self.kind,
            self.is_deleted,
            self.is_const,
            self.is_virtual,
            self.is_pure_virtual,
            self.specializations,
            self.function
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Const(pub bool);

#[derive(Copy, Clone, Debug)]
pub struct Static(pub bool);

#[derive(Copy, Clone, Debug)]
pub struct Virtual(pub bool);

#[derive(Copy, Clone, Debug)]
pub struct PureVirtual(pub bool);

#[derive(Copy, Clone, Debug)]
pub struct Deleted(pub bool);

#[allow(clippy::too_many_arguments)]
impl Method {
    pub fn new(
        usr: USR,
        name: String,
        kind: MethodKind,
        result: QualType,
        arguments: Vec<Argument>,
        rename: Option<String>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        exception_specification_kind: ExceptionSpecificationKind,
        is_const: Const,
        is_virtual: Virtual,
        is_pure_virtual: PureVirtual,
        is_deleted: Deleted,
    ) -> Self {
        Method {
            function: Function::new(
                usr,
                name,
                result,
                arguments,
                rename,
                namespaces,
                template_parameters,
                exception_specification_kind,
            ),
            kind,
            is_const: is_const.0,
            is_virtual: is_virtual.0,
            is_pure_virtual: is_pure_virtual.0,
            is_deleted: is_deleted.0,
            specializations: Vec::new(),
        }
    }

    pub fn unused_template_arguments(&self) -> &[TemplateArgument] {
        self.function.unused_template_arguments()
    }

    pub fn signature_matches(&self, other: &Method) -> bool {
        self.function.signature_matches(&other.function)
            && self.kind == other.kind
            && self.is_const == other.is_const
    }

    pub fn name(&self) -> &str {
        &self.function.name
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        self.function.get_qualified_name(ast)
    }

    pub fn kind(&self) -> MethodKind {
        self.kind
    }

    pub fn is_constructor(&self) -> bool {
        matches!(self.kind, MethodKind::Constructor)
    }

    pub fn is_any_constructor(&self) -> bool {
        self.kind.is_any_constructor()
    }

    pub fn is_copy_constructor(&self) -> bool {
        matches!(self.kind, MethodKind::CopyConstructor)
    }

    pub fn is_move_constructor(&self) -> bool {
        matches!(self.kind, MethodKind::MoveConstructor)
    }

    pub fn is_converting_constructor(&self) -> bool {
        matches!(self.kind, MethodKind::ConvertingConstructor)
    }

    pub fn is_default_constructor(&self) -> bool {
        matches!(self.kind, MethodKind::DefaultConstructor)
    }

    pub fn is_destructor(&self) -> bool {
        matches!(self.kind, MethodKind::Destructor)
    }

    pub fn is_regular_method(&self) -> bool {
        matches!(self.kind, MethodKind::Method)
    }

    pub fn is_deleted(&self) -> bool {
        self.is_deleted
    }

    pub fn result(&self) -> &QualType {
        &self.function.result
    }

    pub fn arguments(&self) -> &[Argument] {
        &self.function.arguments
    }

    pub fn replacement_name(&self) -> Option<&str> {
        self.function.replacement_name.as_deref()
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.function.namespaces
    }

    pub fn template_parameters(&self) -> &[TemplateParameterDecl] {
        self.function.template_parameters()
    }

    pub fn is_templated(&self) -> bool {
        self.function.is_templated()
    }

    pub fn is_specialized(&self) -> bool {
        !self.specializations.is_empty()
    }

    /// What kind of exception specification does this method have?
    pub fn exception_specification_kind(&self) -> ExceptionSpecificationKind {
        self.function.exception_specification_kind()
    }

    pub fn usr(&self) -> USR {
        self.function.usr
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn is_static(&self) -> bool {
        matches!(self.kind, MethodKind::StaticMethod)
    }

    pub fn is_virtual(&self) -> bool {
        self.is_virtual
    }

    pub fn is_pure_virtual(&self) -> bool {
        self.is_pure_virtual
    }

    pub fn rename(&mut self, new_name: &str) {
        self.function.rename(new_name);
    }

    pub fn ignore(&mut self) {
        self.function.ignore();
    }

    pub fn signature(&self) -> String {
        self.format()
    }

    pub fn format(&self) -> String {
        let mut s = self.function.format();

        if self.is_static() {
            s += " static"
        };

        if self.is_virtual {
            s += " virtual"
        };

        if self.is_const {
            s += " const"
        };

        if self.is_pure_virtual {
            s += " = 0"
        };

        s
    }

    pub(crate) fn replace_templates(
        &mut self,
        template_parameters: &[TemplateParameterDecl],
        template_arguments: &[TemplateArgument],
        ast: &AST,
    ) -> Result<Vec<TemplateArgument>> {
        let mut matched_parameters = HashSet::new();
        self.function.result.replace_templates(
            template_parameters,
            template_arguments,
            &mut matched_parameters,
            ast,
        )?;
        // Don't count matched parameters in the return type as we still need to explcitily specify them at the call site
        let mut matched_parameters = HashSet::new();
        for arg in &mut self.function.arguments {
            arg.qual_type.replace_templates(
                template_parameters,
                template_arguments,
                &mut matched_parameters,
                ast,
            )?;
        }

        // now figure out which template parameters are unmatched. We will use these later when calling the method in
        // the shim, e.g. `VtValue::GetValue<float>()`.
        let mut unused_arguments = Vec::new();
        'outer: for parm in template_parameters {
            for mparm in matched_parameters.iter() {
                if mparm == parm.name() {
                    continue 'outer;
                }
            }
            unused_arguments.push(
                find_template_argument_for_parameter(
                    parm.name(),
                    template_parameters,
                    template_arguments,
                )?
                .clone(),
            );
        }

        Ok(unused_arguments)
    }
}

fn find_template_argument_for_parameter<'a>(
    parm: &str,
    template_parameters: &[TemplateParameterDecl],
    template_args: &'a [TemplateArgument],
) -> Result<&'a TemplateArgument> {
    let parm_index = template_parameters
        .iter()
        .position(|p| p.name() == parm)
        .ok_or_else(|| Error::NoMatchingTemplateParameter {
            name: parm.into(),
            source: Trace::new(),
        })?;

    if parm_index >= template_args.len() {
        Err(Error::NoMatchingTemplateArgument {
            name: parm.into(),
            index: parm_index,
            source: Trace::new(),
        })
    } else {
        Ok(&template_args[parm_index])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MethodKind {
    Constructor,
    ConvertingConstructor,
    CopyConstructor,
    DefaultConstructor,
    MoveConstructor,
    Destructor,
    Method,
    StaticMethod,
}

impl MethodKind {
    pub fn is_any_constructor(&self) -> bool {
        matches!(
            self,
            MethodKind::Constructor
                | MethodKind::ConvertingConstructor
                | MethodKind::CopyConstructor
                | MethodKind::DefaultConstructor
                | MethodKind::MoveConstructor
        )
    }
}

pub struct MethodTemplateSpecialization {
    pub(crate) specialized_decl: MethodId,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones.
    ///
    /// Revisit and maybe we want to make that a hard error
    pub(crate) template_arguments: Vec<TemplateArgument>,
    /// The typedef itself is namespaced
    pub(crate) namespaces: Vec<USR>,
}

impl MethodTemplateSpecialization {
    pub fn specialized_decl(&self) -> MethodId {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[TemplateArgument] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }
}

pub fn extract_argument(
    c_arg: Cursor,
    template_parameters: &[String],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
) -> Result<Argument> {
    trace!("extracting arg {c_arg:?}");

    let ty = c_arg.ty()?;
    trace!("  has type {ty:?}");

    let qual_type = extract_type(
        ty,
        template_parameters,
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
    )?;

    Ok(Argument {
        name: c_arg.spelling(),
        qual_type,
    })
}

#[instrument(skip(already_visited, tu, ast), level = "trace")]
pub fn extract_function(
    c_function: Cursor,
    extra_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    ast: &mut AST,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
) -> Result<Function> {
    let c_function = c_function.canonical()?;

    debug!(
        "+ function {} {} {:?}",
        c_function.display_name(),
        if c_function.cxx_method_is_const() {
            "const"
        } else {
            ""
        },
        extra_template_parameters,
    );

    // NOTE (AL) The only reliable way to get at return type and arguments appears to be to inspect the children directly.
    // This is unfortunately a little vague, but appears to be:
    // 1. Any TemplateType/NonTypeTemplateParameters (etc. one would assume)
    // 2. The return type (if it's not a builtin)
    // 3. The arguments
    // So we need to traverse the children, pluck off any template parameters,
    // then the first thing that's *not* a template parameter will be the return type, and everything after that will be an argument
    // let c_function_template_parameters = c_function.children().iter().filter(|c| c.kind() == CursorKind::TemplateTypeParameter || c.kind() == CursorKind::NonTypeTemplateParameter || c.kind() || CursorKind::TemplateTemplateParameter)
    let children = c_function.children();
    debug!("{} children", children.len());
    debug!("{:?}", children);

    let c_template_parameters: Vec<Cursor> = children
        .iter()
        .take_while(|c| {
            c.kind() == CursorKind::TemplateTypeParameter
                || c.kind() == CursorKind::NonTypeTemplateParameter
                || c.kind() == CursorKind::TemplateTemplateParameter
        })
        .map(|c| {
            debug!("Taking {} as template type", c.display_name());
            *c
        })
        .collect();

    // store the function template parameters on the function
    let template_parameters = c_template_parameters
        .iter()
        .enumerate()
        .map(|(index, c)| TemplateParameterDecl::Type {
            name: c.display_name(),
            index,
        })
        .collect::<Vec<_>>();

    let string_template_parameters = extra_template_parameters
        .iter()
        .map(|t| t.name().to_string())
        .chain(c_template_parameters.iter().map(|c| c.display_name()))
        .collect::<Vec<_>>();
    debug!("string template parameters {string_template_parameters:?}");

    let skip = c_template_parameters.len();

    let ty_result = c_function.result_ty()?;
    debug!("result type is {:?}", ty_result);

    let result = extract_type(
        ty_result,
        &string_template_parameters,
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
    )
    .map_err(|e| Error::FailedToExtractResult {
        name: format!("{:?}", ty_result),
        source: Box::new(e),
    })?;

    let num_arguments = match c_function.num_arguments() {
        Ok(n) => n as usize,
        Err(_) => children.len(),
    };
    let mut arguments: Vec<Argument> = Vec::with_capacity(num_arguments);
    let mut i = 0;
    let it = children.iter().skip(skip);
    for c_arg in it {
        debug!("    arg: {}", c_arg.display_name());

        if c_arg.kind() == CursorKind::ParmDecl {
            arguments.push(
                extract_argument(
                    *c_arg,
                    &string_template_parameters,
                    already_visited,
                    ast,
                    tu,
                    allow_list,
                    class_overrides,
                )
                .map_err(|e| Error::FailedToExtractArgument {
                    name: c_arg.display_name(),
                    source: Box::new(e),
                })?,
            );

            i += 1;
            if i == num_arguments {
                break;
            }
        }
    }

    for child in c_function.children() {
        debug!(
            "    - {}: {} {}",
            child.display_name(),
            child.kind(),
            child.usr()
        );
        if child.kind() == CursorKind::TypeRef || child.kind() == CursorKind::TemplateRef {
            if let Ok(c_ref) = child.referenced() {
                let c_ref = if c_ref.kind() == CursorKind::ClassDecl
                    || c_ref.kind() == CursorKind::ClassTemplate
                {
                    c_ref.canonical()?
                } else {
                    c_ref
                };

                debug!(
                    "      - {}: {} {}",
                    c_ref.display_name(),
                    c_ref.kind(),
                    c_ref.usr()
                );
            }
        }
    }

    let replacement_name = get_default_replacement_name(c_function);
    let namespaces = get_namespaces_for_decl(c_function, tu, ast, already_visited)?;
    let name = c_function.spelling();
    let exception_specification_kind = c_function.exception_specification_kind()?;

    Ok(Function::new(
        c_function.usr(),
        name,
        result,
        arguments,
        replacement_name,
        namespaces,
        template_parameters,
        exception_specification_kind,
    ))
}

#[instrument(skip(already_visited, tu, ast), level = "trace")]
pub fn extract_method(
    c_method: Cursor,
    class_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    ast: &mut AST,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    class_name: &str,
) -> Result<Method> {
    let c_method = c_method.canonical()?;

    debug!(
        "+ CXXMethod {} {} {:?}",
        c_method.display_name(),
        if c_method.cxx_method_is_const() {
            "const"
        } else {
            ""
        },
        class_template_parameters,
    );

    let kind = if c_method.cxx_method_is_static() {
        MethodKind::StaticMethod
    } else if c_method.cxx_constructor_is_copy_constructor() {
        MethodKind::CopyConstructor
    } else if c_method.cxx_constructor_is_move_constructor() {
        MethodKind::MoveConstructor
    } else if c_method.cxx_constructor_is_converting_constructor() {
        MethodKind::ConvertingConstructor
    } else if c_method.cxx_constructor_is_default_constructor() {
        MethodKind::DefaultConstructor
    } else if c_method.kind() == CursorKind::Constructor {
        MethodKind::Constructor
    } else if c_method.kind() == CursorKind::Destructor {
        MethodKind::Destructor
    } else if c_method.spelling() == class_name {
        // libclang seems to fail to identify templated constructors
        MethodKind::Constructor
    } else {
        MethodKind::Method
    };

    Ok(Method {
        function: extract_function(
            c_method,
            class_template_parameters,
            already_visited,
            tu,
            ast,
            allow_list,
            class_overrides,
        )
        .map_err(|e| Error::FailedToExtractMethod {
            name: c_method.display_name(),
            source: Box::new(e),
        })?,
        kind,
        is_const: c_method.cxx_method_is_const(),
        is_virtual: c_method.cxx_method_is_virtual(),
        is_pure_virtual: c_method.cxx_method_is_pure_virtual(),
        is_deleted: c_method.cxx_method_is_deleted(),
        specializations: Vec::new(),
    })
}

fn get_default_replacement_name(c_method: Cursor) -> Option<String> {
    match c_method.kind() {
        CursorKind::Constructor => {
            if c_method.cxx_constructor_is_copy_constructor() {
                Some("copy_ctor".into())
            } else if c_method.cxx_constructor_is_move_constructor() {
                Some("move_ctor".into())
            } else if c_method.cxx_constructor_is_move_constructor() {
                Some("default_ctor".into())
            } else {
                Some("ctor".into())
            }
        }
        CursorKind::Destructor => Some("dtor".into()),
        _ => {
            if c_method.display_name().contains("operator+=") {
                Some("op_add_assign".into())
            } else if c_method.display_name().contains("operator+") {
                Some("op_add".into())
            } else if c_method.display_name().contains("operator-=") {
                Some("op_sub_assign".into())
            } else if c_method.display_name().contains("operator-")
                && c_method.num_arguments().unwrap() == 1
            {
                Some("op_sub".into())
            } else if c_method.display_name().contains("operator-")
                && c_method.num_arguments().unwrap() == 0
            {
                Some("op_neg".into())
            } else if c_method.display_name().contains("operator*=") {
                Some("op_mul_assign".into())
            } else if c_method.display_name().contains("operator*") {
                Some("op_mul".into())
            } else if c_method.display_name().contains("operator/=") {
                Some("op_div_assign".into())
            } else if c_method.display_name().contains("operator/") {
                Some("op_div".into())
            } else if c_method.display_name().contains("operator&=") {
                Some("op_bitand_assign".into())
            } else if c_method.display_name().contains("operator&") {
                Some("op_bitand".into())
            } else if c_method.display_name().contains("operator|=") {
                Some("op_bitor_assign".into())
            } else if c_method.display_name().contains("operator|") {
                Some("op_bitor".into())
            } else if c_method.display_name().contains("operator^=") {
                Some("op_bitxor_assign".into())
            } else if c_method.display_name().contains("operator^") {
                Some("op_bitxor".into())
            } else if c_method.display_name().contains("operator%=") {
                Some("op_rem_assign".into())
            } else if c_method.display_name().contains("operator%") {
                Some("op_rem".into())
            } else if c_method.display_name().contains("operator<<=") {
                Some("op_shl_assign".into())
            } else if c_method.display_name().contains("operator<<") {
                Some("op_shl".into())
            } else if c_method.display_name().contains("operator>>=") {
                Some("op_shr_assign".into())
            } else if c_method.display_name().contains("operator>>") {
                Some("op_shr".into())
            } else if c_method.display_name().contains("operator==") {
                Some("op_eq".into())
            } else if c_method.display_name().contains("operator=") {
                Some("op_assign".into())
            } else if c_method.display_name().contains("operator!") {
                Some("op_not".into())
            } else if c_method.display_name().contains("operator[]") {
                Some("op_index".into())
            } else {
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use indoc::indoc;

    use crate::{
        class::{ClassBindKind, OverrideList},
        parse_string_and_extract_ast, AllowList,
    };

    #[test]
    fn extract_static_method() -> bbl_util::Result<()> {
        // test that a POD extracts as a valuetype
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                public:
                    int a;
                    float b;

                    static float static_method(float b);
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field a: int
                    Field b: float
                    Method StaticMethod deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@static_method#f#S static_method rename=None ignore=false return=float args=[b: float] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

                    "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::ValueType));

            Ok(())
        })
    }

    #[test]
    fn extract_static_method_taking_class() -> bbl_util::Result<()> {
        // test that a POD extracts as a valuetype
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                public:
                    int a;
                    float b;

                    static float static_method(Class c);
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field a: int
                    Field b: float
                    Method StaticMethod deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@static_method#$@S@Class#S static_method rename=None ignore=false return=float args=[c: Class] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

                    "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::ValueType));

            Ok(())
        })
    }
}
