use bbl_clang::cursor::{Cursor, USR};
use bbl_clang::exception::ExceptionSpecificationKind;
use bbl_clang::translation_unit::TranslationUnit;
use bbl_clang::ty::TypeKind;
use log::*;
use tracing::instrument;
use std::fmt::Display;

use crate::ast::{get_namespaces_for_decl, get_qualified_name, MethodId, TypeAliasId};
use crate::class::MethodSpecializationId;
use crate::qualtype::{extract_type, extract_type_from_typeref};
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use crate::{ast::AST, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Argument {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Argument {
    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
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
}

pub struct Function {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) result: QualType,
    pub(crate) arguments: Vec<Argument>,
    pub(crate) replacement_name: Option<String>,
    pub(crate) ignored: bool,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
    pub(crate) specializations: Vec<TypeAliasId>,
    pub(crate) exception_specification_kind: ExceptionSpecificationKind,
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
            specializations: Vec::new(),
            exception_specification_kind,
        }
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
    pub fn signature(
        &self,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
        template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        self.format(ast, outer_template_parameters, template_args)
    }

    pub fn format(
        &self,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
        template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        let mut s = self.name.to_string();

        let args = self
            .arguments
            .iter()
            .map(|p| p.format(ast, outer_template_parameters, template_args))
            .collect::<Vec<String>>()
            .join(", ");
        s = format!("{s}({})", args);

        s = format!(
            "{s} -> {}",
            self.result
                .format(ast, outer_template_parameters, template_args)
        );

        s
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
        template_args: Option<&[Option<TemplateType>]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let s = self.format(ast, outer_template_parameters, template_args);

        let mut modify_attrs = Vec::new();
        if self.ignored {
            modify_attrs.push("ignored".to_string());
        }

        if let Some(new_name) = &self.replacement_name {
            modify_attrs.push(format!("rename(\"{new_name}\")"));
        }

        let attr_string = if modify_attrs.is_empty() {
            String::new()
        } else {
            format!("    [[{}]]", modify_attrs.join(", "))
        };

        let template_str = if !self.template_parameters.is_empty() {
            let parms = self
                .template_parameters
                .iter()
                .map(|t| t.name().to_string())
                .collect::<Vec<_>>();
            format!("{indent}template <typename {}>\n", parms.join(", typename"))
        } else {
            String::new()
        };

        println!("{template_str}{indent}{s}{attr_string}");
    }
}

pub struct Method {
    pub(crate) function: Function,
    pub(crate) is_const: bool,
    pub(crate) is_static: bool,
    pub(crate) is_virtual: bool,
    pub(crate) is_pure_virtual: bool,
    pub(crate) specializations: Vec<MethodSpecializationId>,
}

#[allow(clippy::too_many_arguments)]
impl Method {
    pub fn new(
        usr: USR,
        name: String,
        result: QualType,
        arguments: Vec<Argument>,
        rename: Option<String>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        exception_specification_kind: ExceptionSpecificationKind,
        is_const: bool,
        is_static: bool,
        is_virtual: bool,
        is_pure_virtual: bool,
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
            is_const,
            is_static,
            is_virtual,
            is_pure_virtual,
            specializations: Vec::new(),
        }
    }

    pub fn name(&self) -> &str {
        &self.function.name
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        self.function.get_qualified_name(ast)
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
        self.is_static
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

    pub fn signature(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        self.format(ast, class_template_parameters, class_template_args)
    }

    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        let mut s = self
            .function
            .format(ast, class_template_parameters, class_template_args);

        if self.is_static {
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

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let s = self.format(ast, class_template_parameters, class_template_args);

        let mut modify_attrs = Vec::new();
        if self.function.ignored {
            modify_attrs.push("ignored".to_string());
        }

        if let Some(new_name) = &self.function.replacement_name {
            modify_attrs.push(format!("rename(\"{new_name}\")"));
        }

        let attr_string = if modify_attrs.is_empty() {
            String::new()
        } else {
            format!("    [[{}]]", modify_attrs.join(", "))
        };

        println!("{indent}{s}{attr_string}");
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
    pub(crate) template_arguments: Vec<Option<TemplateType>>,
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

    pub fn template_arguments(&self) -> &[Option<TemplateType>] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }
}

pub fn extract_argument(c_arg: Cursor, depth: usize, template_parameters: &[String], already_visited: &mut Vec<USR>, ast: &mut AST, tu: &TranslationUnit) -> Result<Argument> {
    let children = c_arg.children();
    trace!("{:width$}extracting arg {c_arg:?}", "", width = depth * 2);

    let ty = c_arg.ty()?;
    trace!("{:width$}  has type {ty:?}", "", width = depth * 2);

    let qual_type = if ty.is_builtin() || ty.is_pointer() {
        extract_type(ty, depth+1, template_parameters, already_visited, ast, tu)?
    } else if !children.is_empty() {
        match children[0].kind() {
            CursorKind::TypeRef | CursorKind::TemplateRef => {
                extract_type_from_typeref(children[0], depth+1)?
            }
            _ => {
                debug!("other kind {:?}", children[0].kind(),);
                QualType::unknown(children[0].ty()?.kind())
            }
        }
    } else {
        error!("coulnd't do argument type");
        QualType::unknown(children[0].ty()?.kind())
    };

    Ok(Argument {
        name: c_arg.spelling(),
        qual_type,
    })
}

#[instrument(skip(depth, already_visited, tu, ast))]
pub fn extract_function(
    c_function: Cursor,
    depth: usize,
    extra_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    ast: &mut AST,
) -> Result<Function> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let c_function = c_function.canonical()?;

    debug!(
        "{indent}+ CXXMethod {} {}",
        c_function.display_name(),
        if c_function.cxx_method_is_const() {
            "const"
        } else {
            ""
        }
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

    let mut skip = c_template_parameters.len();

    let ty_result = c_function.result_ty()?;
    debug!("result type is {:?}", ty_result);
    let result = if ty_result.is_builtin() || ty_result.is_pointer() || ty_result.kind() == TypeKind::Elaborated {
        extract_type(ty_result, depth+1, &string_template_parameters, already_visited, ast, tu).map_err(|e| Error::FailedToExtractResult{source: Box::new(e)})?
    } else {
        let c_result = children.get(skip).ok_or(Error::FailedToGetCursor)?;

        skip += 1;

        if c_result.kind() == CursorKind::TypeRef || c_result.kind() == CursorKind::TemplateRef {
            extract_type_from_typeref(*c_result, depth+1).map_err(|e| Error::FailedToExtractResult{source: Box::new(e)})?
        } else {
            QualType::unknown(ty_result.kind())
        }
    };

    let num_arguments = match c_function.num_arguments() {
        Ok(n) => n as usize,
        Err(_) => children.len(),
    };
    let mut arguments: Vec<Argument> = Vec::with_capacity(num_arguments);
    let mut i = 0;
    let mut it = children.iter().skip(skip);
    while let Some(c_arg) = it.next() {
        debug!("{indent}    arg: {}", c_arg.display_name());

        if c_arg.kind() == CursorKind::ParmDecl {
            arguments.push(
                extract_argument(*c_arg, depth, &string_template_parameters, already_visited, ast, tu).map_err(|e| {
                    Error::FailedToExtractArgument {
                        name: c_arg.display_name(),
                        source: Box::new(e),
                    }
                })?,
            );

            i += 1;
            if i == num_arguments { break; }
        }
    }

    for child in c_function.children() {
        debug!(
            "{indent}    - {}: {} {}",
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
                    "{indent}      - {}: {} {}",
                    c_ref.display_name(),
                    c_ref.kind(),
                    c_ref.usr()
                );
            }
        }
    }

    let replacement_name = get_default_replacement_name(c_function);
    let namespaces = get_namespaces_for_decl(c_function, tu, ast);
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

#[instrument(skip(depth, already_visited, tu, ast))]
pub fn extract_method(
    c_method: Cursor,
    depth: usize,
    class_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    ast: &mut AST,
) -> Result<Method> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let c_method = c_method.canonical()?;

    debug!(
        "{indent}+ CXXMethod {} {}",
        c_method.display_name(),
        if c_method.cxx_method_is_const() {
            "const"
        } else {
            ""
        }
    );

    Ok(Method {
        function: extract_function(c_method, depth, class_template_parameters, already_visited, tu, ast).map_err(|e| {
            Error::FailedToExtractMethod {
                name: c_method.display_name(),
                source: Box::new(e),
            }
        })?,
        is_const: c_method.cxx_method_is_const(),
        is_static: c_method.cxx_method_is_static(),
        is_virtual: c_method.cxx_method_is_virtual(),
        is_pure_virtual: c_method.cxx_method_is_pure_virtual(),
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
