#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::access_specifier::AccessSpecifier;
use bbl_clang::cli_args;
use bbl_clang::cursor::{CurClassDecl, Cursor, USR};
use bbl_clang::translation_unit::TranslationUnit;
use log::*;
use std::fmt::{Debug, Display};
use tracing::instrument;
use ustr::Ustr;

use crate::ast::{get_namespaces_for_decl, get_qualified_name, MethodId, TypeAliasId, AST, ClassTemplateSpecializationId};
use crate::error::{self, ExtractClassError};
use crate::function::{extract_method, MethodTemplateSpecialization};
use crate::index_map::IndexMapKey;
use crate::parse_string_and_extract_ast;
use crate::qualtype::extract_type;
use crate::stdlib::create_std_string;
use crate::templates::{TemplateParameterDecl, TemplateArgument, extract_class_template_specialization};
use crate::{function::Method, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ClassBindKind {
    OpaquePtr,
    OpaqueBytes,
    ValueType,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct MethodSpecializationId(pub usize);

impl MethodSpecializationId {
    pub fn new(id: usize) -> MethodSpecializationId {
        MethodSpecializationId(id)
    }
}

impl From<MethodSpecializationId> for usize {
    fn from(id: MethodSpecializationId) -> Self {
        id.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MethodState {
    Defined,
    Undefined,
    Defaulted,
    Deleted,
}

impl MethodState {
    pub fn is_defined(&self) -> bool {
        matches!(self, MethodState::Defined)
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, MethodState::Undefined)
    }

    pub fn is_defaulted(&self) -> bool {
        matches!(self, MethodState::Defaulted)
    }

    pub fn is_deleted(&self) -> bool {
        matches!(self, MethodState::Deleted)
    }
}

impl Default for MethodState {
    fn default() -> Self {
        MethodState::Undefined
    }
}

#[derive(Default)]
pub struct RuleOfFive {
    pub(crate) ctor: MethodState,
    pub(crate) copy_ctor: MethodState,
    pub(crate) move_ctor: MethodState,
    pub(crate) copy_assign: MethodState,
    pub(crate) move_assign: MethodState,
    pub(crate) dtor: MethodState,
}

fn write_method_state(
    f: &mut std::fmt::Formatter<'_>,
    name: &str,
    ms: MethodState,
) -> std::fmt::Result {
    use MethodState::*;

    match ms {
        Defined => write!(f, "{} ", name),
        Undefined => Ok(()),
        Defaulted => write!(f, "{}=default ", name),
        Deleted => write!(f, "{}=delete ", name),
    }
}

impl Debug for RuleOfFive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        write_method_state(f, "ctor", self.ctor)?;
        write_method_state(f, "copy_ctor", self.copy_ctor)?;
        write_method_state(f, "move_ctor", self.move_ctor)?;
        write_method_state(f, "copy_assign", self.copy_assign)?;
        write_method_state(f, "move_assign", self.move_assign)?;
        write_method_state(f, "dtor", self.dtor)?;
        write!(f, "]")?;

        Ok(())
    }
}

// TODO(AL): the rules here are complex and version-dependent. Will need to try and add to libclang here to have clang
// decide for us
impl RuleOfFive {
    pub fn needs_implicit_ctor(&self) -> bool {
        self.ctor.is_undefined()
    }

    pub fn needs_implicit_copy_ctor(&self) -> bool {
        self.copy_ctor.is_undefined()
            && !self.move_ctor.is_defined()
            && !self.move_assign.is_defined()
    }

    pub fn needs_implicit_move_ctor(&self) -> bool {
        self.move_ctor.is_undefined()
            && !self.copy_ctor.is_defined()
            && !self.copy_assign.is_defined()
            && !self.move_assign.is_defined()
            && !self.dtor.is_defined()
    }

    pub fn needs_implicit_dtor(&self) -> bool {
        self.dtor.is_undefined()
    }
}

pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Method>,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
    /// List of the specializations made from this class if it is templated
    pub(crate) specializations: Vec<ClassTemplateSpecializationId>,

    pub(crate) ignore: bool,
    pub(crate) rename: Option<String>,
    pub(crate) bind_kind: ClassBindKind,
    pub(crate) specialized_methods: Vec<MethodTemplateSpecialization>,
    pub(crate) is_pod: bool,

    pub(crate) rule_of_five: RuleOfFive,
}

impl std::fmt::Debug for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ClassDecl {usr} {name} rename={rename:?} {bind_kind:?} is_pod={pod} ignore={ignore} rof={rof:?} template_parameters={template_parameters:?} specializations={specializations:?} namespaces={namespaces:?}", 
            usr=self.usr, 
            name=self.name(),
            rename = self.rename,
            bind_kind = self.bind_kind(),
            pod = self.is_pod,
            ignore = self.ignore,
            rof = self.rule_of_five,
            specializations = self.specializations,
            template_parameters = self.template_parameters(),
            namespaces = self.namespaces(),
        )?;

        for field in self.fields() {
            writeln!(f, "Field {:?}", field)?;
        }

        for method in self.methods() {
            writeln!(f, "{:?}", method)?;
        }

        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
impl ClassDecl {
    pub fn new(
        usr: USR,
        name: String,
        fields: Vec<Field>,
        methods: Vec<Method>,
        namespaces: Vec<USR>,
        template_parameters: Vec<TemplateParameterDecl>,
        is_pod: bool,
        rule_of_five: RuleOfFive,
    ) -> ClassDecl {
        ClassDecl {
            usr,
            name,
            fields,
            methods,
            namespaces,
            template_parameters,
            specializations: Vec::new(),
            ignore: false,
            rename: None,
            bind_kind: if is_pod {
                ClassBindKind::ValueType
            } else {
                ClassBindKind::OpaquePtr
            },
            specialized_methods: Vec::new(),
            is_pod,
            rule_of_five,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_pod(&self) -> bool {
        self.is_pod
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn fields(&self) -> &[Field] {
        &self.fields
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn template_parameters(&self) -> &[TemplateParameterDecl] {
        &self.template_parameters
    }

    pub fn is_templated(&self) -> bool {
        !self.template_parameters.is_empty()
    }

    pub fn is_specialized(&self) -> bool {
        !self.specializations.is_empty()
    }

    pub fn bind_kind(&self) -> &ClassBindKind {
        &self.bind_kind
    }

    pub fn rule_of_five(&self) -> &RuleOfFive {
        &self.rule_of_five
    }

    /// Set the [`ClassBindKind`] of this class, which affects how it is represented in C.
    ///
    /// # Params
    /// * `bind_kind` - The [`ClassBindKind`] to use for this class
    /// * `force_members` - If `true`, will try to set any [`Field`]s of this class that are themselves classes to the
    /// same `bind_kind`.
    ///
    /// # Errors
    /// * [`Error::ClassHasNonValueTypeFields`] if `bind_kind` is [`ClassBindKind::ValueType`], `force_members_to_match`
    /// is false, and this class has fields which are non-value-type classes.
    /// * [`Error::ClassHasIncompatibleFields`] if `bind_kind` is [`ClassBindKind::ValueType`], `force_members_to_match`
    /// is true, but the field classes cannot be converted to value types.
    pub(crate) fn set_bind_kind(&mut self, bind_kind: ClassBindKind) {
        self.bind_kind = bind_kind
    }

    pub fn could_be_valuetype(&self, ast: &AST) -> Result<bool> {
        for field in &self.fields {
            if !field.qual_type().is_valuetype(ast)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub fn specialized_methods(&self) -> &[MethodTemplateSpecialization] {
        &self.specialized_methods
    }

    pub fn set_ignore(&mut self, ignore: bool) {
        self.ignore = true;
    }

    pub fn set_rename(&mut self, name: &str) {
        self.rename = Some(name.to_string());
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        class_template_args: Option<&[TemplateArgument]>,
    ) {
        if !self.template_parameters.is_empty() {
            self.pretty_print_template(depth, ast, class_template_args);
            return;
        }

        let indent = format!("{:width$}", "", width = depth * 2);

        let pod = if self.is_pod { "[POD]" } else { "" };

        println!("+ ClassDecl {} {}", self.usr, pod);

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        let name = if let Some(ref rename) = self.rename {
            rename
        } else {
            &self.name
        };

        println!("{indent}class {ns_string}::{} {{", name);

        for method in &self.methods {
            method.pretty_print(
                depth + 1,
                ast,
                &self.template_parameters,
                class_template_args,
            );
        }

        for field in &self.fields {
            println!(
                "{indent}{indent}{};",
                field.format(ast, &self.template_parameters, class_template_args)
            );
        }

        println!("{indent}}}");
    }

    pub fn format(&self, ast: &AST, template_args: Option<&[TemplateArgument]>) -> String {
        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        format!("{ns_string}::{}{template}", self.name)
    }

    fn pretty_print_template(
        &self,
        depth: usize,
        ast: &AST,
        template_args: Option<&[TemplateArgument]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| {
                ast.get_namespace(*u)
                    .unwrap_or_else(|| panic!("Failed to get namespace with usr {u}"))
                    .name
                    .clone()
            })
            .collect::<Vec<String>>()
            .join("::");

        println!("+ ClassTemplate {}", self.usr);

        let template_decl = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "template <{}>\n{indent}",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "<{}>",
                self.template_parameters
                    .iter()
                    .map(|t| specialize_template_parameter(t, template_args))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        println!(
            "{indent}{template_decl}class {ns_string}::{}{template} {{",
            self.name
        );

        for method in &self.methods {
            method.pretty_print(depth + 1, ast, &self.template_parameters, template_args);
        }

        println!("{indent}}}");
    }

    pub fn find_method(&self, ast: &AST, signature: &str) -> Result<(MethodId, &Method)> {
        let mut matches = Vec::new();

        for (method_id, method) in self.methods.iter().enumerate() {
            if method
                .signature(ast, &self.template_parameters, None)
                .contains(signature)
            {
                matches.push((method_id, method));
            }
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.methods.len());
                for method in self.methods.iter() {
                    let sig = method.signature(ast, &self.template_parameters, None);
                    let dist = levenshtein::levenshtein(&sig, signature);
                    distances.push((dist, sig));
                }

                distances.sort_by(|a, b| a.0.cmp(&b.0));

                error!(
                    "Could not find method matching signature: \"{}\"",
                    signature
                );
                error!("Did you mean one of:");
                for (_, sug) in distances.iter().take(3) {
                    error!("  {sug}");
                }

                Err(Error::MethodNotFound)
            }
            1 => Ok((MethodId::new(matches[0].0), matches[0].1)),
            _ => {
                error!("Multiple matches found for signature \"{signature}\":");

                for (_, method) in matches {
                    error!(
                        "  {}",
                        method.signature(ast, &self.template_parameters, None)
                    );
                }

                Err(Error::MultipleMatches)
            }
        }
    }

    pub fn get_method(&self, id: MethodId) -> &Method {
        &self.methods[id.get()]
    }

    pub fn rename_method(&mut self, method_id: MethodId, new_name: &str) {
        self.methods[method_id.get()].rename(new_name);
    }

    pub fn ignore_method(&mut self, method_id: MethodId) {
        self.methods[method_id.get()].ignore();
    }

    pub fn specialize_method(
        &mut self,
        method_id: MethodId,
        name: &str,
        template_arguments: Vec<TemplateArgument>,
    ) -> Result<MethodSpecializationId> {
        let method_decl = &mut self.methods[method_id.get()];

        let usr = USR::new(&format!("{}_{name}", method_decl.usr().as_str()));

        let id = self.specialized_methods.len();

        let mts = MethodTemplateSpecialization {
            specialized_decl: method_id,
            usr,
            name: name.into(),
            template_arguments,
            namespaces: Vec::new(),
        };

        self.specialized_methods.push(mts);

        let id = MethodSpecializationId(id);

        method_decl.specializations.push(id);

        Ok(id)
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[instrument(skip(tu, ast, already_visited), level = "trace")]
pub fn extract_class_decl(
    class_decl: CurClassDecl,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    // Check for std:: types we're going to extract manually here
    if class_decl.display_name().starts_with("basic_string<") {
        debug!("Extracting basic_string {}", class_decl.usr());
        let cd = create_std_string(class_decl.into(), ast);
        let usr = cd.usr();
        ast.insert_class(cd);
        return Ok(usr);
    }

    if class_decl.specialized_template().is_ok() {
        // this is a class template specialization, handle it separately
        return extract_class_template_specialization(class_decl, already_visited, ast, tu);
    }

    if already_visited.contains(&class_decl.usr()) {
        return Ok(class_decl.usr());
    } else {
        already_visited.push(class_decl.usr());
    }

    trace!(
        "extract_class_decl({}) {}",
        class_decl.usr(),
        class_decl.display_name()
    );

    let namespaces = get_namespaces_for_decl(class_decl.into(), tu, ast, already_visited)?;

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();
    let mut rule_of_five = RuleOfFive::default();

    let members = class_decl.children();
    let mut index = 0;
    let mut has_private_fields = false;
    for member in members {
        debug!("member {:?}", member);
        if let Ok(access) = member.cxx_access_specifier() {
            if access != AccessSpecifier::Public {
                continue;
            }
        } else {
            return Err(Error::FailedToGetAccessSpecifierFor(member.display_name()));
        }
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
                let name = member.display_name();
                template_parameters.push(TemplateParameterDecl::Type { name, index });
                index += 1;
            }
            CursorKind::NonTypeTemplateParameter => {
                for child in &member.children() {
                    debug!("    memebr child {:?}", child);
                    match child.kind() {
                        CursorKind::IntegerLiteral => {
                            let name = member.display_name();

                            for lit_child in &child.children() {}

                            template_parameters.push(TemplateParameterDecl::Integer {
                                name,
                                default: Some(tu.token(child.location()).spelling()),
                                index,
                            });
                            index += 1;
                        }
                        _ => unimplemented!(),
                    }
                }
            }
            CursorKind::TemplateTemplateParameter => unimplemented!(),
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                        match extract_method(
                            member,
                            &template_parameters,
                            already_visited,
                            tu,
                            ast,
                        ) {
                            Ok(method) => methods.push(method),
                            Err(e) => {
                                return Err(Error::FailedToExtractMethod {
                                    name: member.display_name(),
                                    source: Box::new(e),
                                })
                            }
                        }
                    }

                    // Get the state (defined, deleted, defaulted) of rule-of-five members so we can figure out which
                    // ones we need to auto-generate later.
                    // TODO(AL): need to trawl base classes here as well...
                    match member.kind() {
                        CursorKind::Constructor if member.cxx_constructor_is_copy_constructor() => {
                            rule_of_five.copy_ctor = get_method_state(member)
                        }
                        CursorKind::Constructor if member.cxx_constructor_is_move_constructor() => {
                            rule_of_five.move_ctor = get_method_state(member)
                        }
                        CursorKind::Constructor => rule_of_five.ctor = get_method_state(member),
                        CursorKind::Destructor => rule_of_five.dtor = get_method_state(member),
                        CursorKind::CXXMethod
                            if member.cxx_method_is_copy_assignment_operator() =>
                        {
                            rule_of_five.copy_assign = get_method_state(member)
                        }
                        CursorKind::CXXMethod
                            if member.cxx_method_is_move_assignment_operator() =>
                        {
                            rule_of_five.move_assign = get_method_state(member)
                        }
                        _ => (),
                    }
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor(member.display_name()));
                }
            }
            CursorKind::FieldDecl => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                        let field = extract_field(
                            member,
                            &template_parameters,
                            already_visited,
                            ast,
                            tu,
                        )
                        .map_err(|e| {
                            ExtractClassError::FailedToExtractField {
                                class: class_decl.display_name(),
                                name: member.display_name(),
                                source: Box::new(e),
                            }
                        })?;
                        fields.push(field);
                    } else {
                        has_private_fields = true;
                    }
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor(member.display_name()));
                }
            }
            _ => {
                debug!("  {member:?}");
                for child in member.children() {
                    debug!("    {child:?}");
                    match child.kind() {
                        CursorKind::TypeRef => {
                            if let Ok(c) = child.referenced() {
                                debug!("    -> {c:?}");
                            }
                        }
                        CursorKind::ParmDecl => {
                            if let Ok(ty) = child.ty() {
                                debug!("      type {ty:?}")
                            }

                            for c in child.children() {
                                debug!("      {c:?}");
                            }
                        }
                        CursorKind::CompoundStmt => {
                            for stmt in child.children() {
                                debug!("      {stmt:?}");
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    let name = class_decl.spelling();

    // If this is a template decl we won't be able to get a type from it so just set POD to false
    // Note that we have a slightly different definition of POD from cpp: we do not consider records with private fields
    // to be POD as they're not correctly representable in C.
    // TODO(AL): well, actually... they're not in C but they are in Rust. Though if you have a private field you probably
    // have a non-trivial constructor, but we should probably not force that here
    let is_pod = if template_parameters.is_empty() {
        match class_decl.ty() {
            Ok(ty) => ty.is_pod() && !has_private_fields,
            Err(e) => {
                error!("Could not get type from {class_decl:?}");
                false
            }
        }
    } else {
        false
    };

    debug!("Got new ClassDecl {name}");
    let cd = ClassDecl::new(
        class_decl.usr(),
        class_decl.spelling(),
        fields,
        methods,
        namespaces,
        template_parameters,
        is_pod,
        rule_of_five,
    );
    ast.insert_class(cd);

    Ok(class_decl.usr())
}

fn get_method_state(method: Cursor) -> MethodState {
    if method.cxx_method_is_defaulted() {
        MethodState::Defaulted
    } else if method.cxx_method_is_deleted() {
        MethodState::Deleted
    } else {
        MethodState::Defined
    }
}

// TODO(AL): fix these once we've patched libclang
fn method_is_copy_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}

fn method_is_move_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}

pub fn extract_field(
    c_field: Cursor,
    class_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
) -> Result<Field> {
    let template_parameters = class_template_parameters
        .iter()
        .map(|t| t.name().to_string())
        .collect::<Vec<_>>();

    let ty = c_field.ty()?;
    let qual_type = extract_type(
        ty,
        &template_parameters,
        already_visited,
        ast,
        tu,
    )?;

    Ok(Field {
        name: c_field.display_name(),
        qual_type,
    })
}

pub struct Field {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Debug for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name(), self.qual_type())
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Field {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn qual_type(&self) -> &QualType {
        &self.qual_type
    }

    fn format(
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
}

/// Choose the type replacement for the give `TemplateParameterDecl`
pub(crate) fn specialize_template_parameter(
    decl: &TemplateParameterDecl,
    args: Option<&[TemplateArgument]>,
) -> String {
    if let Some(args) = args {
        if let Some(arg) = args.get(decl.index()) {
            match arg {
                TemplateArgument::Type(qt) => return qt.name.clone(),
                TemplateArgument::Integral(i) => return format!("{i}"),
                _ => format!("{arg:?}")
            };
        } else if let TemplateParameterDecl::Integer {
            default: Some(value),
            ..
        } = decl
        {
            // check if we have a non-type parameter with a default
            return value.clone();
        }
    }

    decl.default_name()
}

#[cfg(test)]

mod tests {
    use bbl_clang::cli_args;
    use indoc::indoc;

    use crate::{class::ClassBindKind, error::Error, parse_string_and_extract_ast};

    #[test]
    fn extract_pod() -> Result<(), Error> {
        // test that a POD extracts as a valuetype
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Class {
            public:
                int a;
                float b;
            };
        "#
            ),
            &cli_args()?,
            true,
            None,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
        ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]
        Field a: int
        Field b: float

    "#
            )
        );

        let class_id = ast.find_class("Class")?;
        let class = &ast.classes()[class_id];
        assert!(matches!(class.bind_kind(), ClassBindKind::ValueType));

        Ok(())
    }

    #[test]
    fn extract_non_pod() -> Result<(), Error> {
        // test that adding a private field to a POD forces opaqueptr
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Class {
                int a;
            public:
                float b;
            };
        "#
            ),
            &cli_args()?,
            true,
            None,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
        ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]
        Field b: float
 
    "#
            )
        );

        let class_id = ast.find_class("Class")?;
        let class = &ast.classes()[class_id];
        assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

        Ok(())
    }

    #[test]
    fn extract_non_pod2() -> Result<(), Error> {
        // test that adding a constructor forces non-pod
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class Class {
            public:
                Class();
                float b;
            };
            }
    
        "#
            ),
            &cli_args()?,
            true,
            None,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
        Namespace c:@S@Class Class None
        ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false rof=[ctor ] template_parameters=[] specializations=[] namespaces=[]
        Field b: float
        Method DefaultConstructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@Class# Class rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

    "#
            )
        );

        let class_id = ast.find_class("Class")?;
        let class = &ast.classes()[class_id];
        assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

        Ok(())
    }

    #[test]
    fn extract_rof() -> Result<(), Error> {
        // test that adding a constructor forces non-pod
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            class NeedsImplicitAll {
            public:
            };

            class NeedsImplicitCopyCtor {
            public:
                ~NeedsImplicitCopyCtor();
            };

            class NeedsImplicitNone {
            public:
                NeedsImplicitNone();
                NeedsImplicitNone(const NeedsImplicitNone&);
                ~NeedsImplicitNone();
            };

        "#
            ),
            &cli_args()?,
            true,
            None,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
                Namespace c:@S@NeedsImplicitCopyCtor NeedsImplicitCopyCtor None
                Namespace c:@S@NeedsImplicitNone NeedsImplicitNone None
                ClassDecl c:@S@NeedsImplicitAll NeedsImplicitAll rename=None ValueType is_pod=true ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]

                ClassDecl c:@S@NeedsImplicitCopyCtor NeedsImplicitCopyCtor rename=None OpaquePtr is_pod=false ignore=false rof=[dtor ] template_parameters=[] specializations=[] namespaces=[]
                Method Destructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@NeedsImplicitCopyCtor@F@~NeedsImplicitCopyCtor# ~NeedsImplicitCopyCtor rename=Some("dtor") ignore=false return=void args=[] noexcept=Unevaluated template_parameters=[] specializations=[] namespaces=[c:@S@NeedsImplicitCopyCtor]

                ClassDecl c:@S@NeedsImplicitNone NeedsImplicitNone rename=None OpaquePtr is_pod=false ignore=false rof=[ctor copy_ctor dtor ] template_parameters=[] specializations=[] namespaces=[]
                Method DefaultConstructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@NeedsImplicitNone@F@NeedsImplicitNone# NeedsImplicitNone rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@NeedsImplicitNone]
                Method CopyConstructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@NeedsImplicitNone@F@NeedsImplicitNone#&1$@S@NeedsImplicitNone# NeedsImplicitNone rename=Some("copy_ctor") ignore=false return=void args=[: const NeedsImplicitNone &] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@NeedsImplicitNone]
                Method Destructor const=false virtual=false pure_virtual=false specializations=[] Function c:@S@NeedsImplicitNone@F@~NeedsImplicitNone# ~NeedsImplicitNone rename=Some("dtor") ignore=false return=void args=[] noexcept=Unevaluated template_parameters=[] specializations=[] namespaces=[c:@S@NeedsImplicitNone]

                "#
            )
        );

        let class_id = ast.find_class("NeedsImplicitAll")?;
        let class = &ast.classes()[class_id];
        assert!(class.rule_of_five().needs_implicit_ctor());
        assert!(class.rule_of_five().needs_implicit_copy_ctor());
        assert!(class.rule_of_five().needs_implicit_move_ctor());
        assert!(class.rule_of_five().needs_implicit_dtor());

        let class_id = ast.find_class("NeedsImplicitCopyCtor")?;
        let class = &ast.classes()[class_id];
        assert!(class.rule_of_five().needs_implicit_ctor());
        assert!(class.rule_of_five().needs_implicit_copy_ctor());
        assert!(!class.rule_of_five().needs_implicit_move_ctor());
        assert!(!class.rule_of_five().needs_implicit_dtor());

        let class_id = ast.find_class("NeedsImplicitNone")?;
        let class = &ast.classes()[class_id];
        println!("{:?}", class.rule_of_five());
        assert!(!class.rule_of_five().needs_implicit_ctor());
        assert!(!class.rule_of_five().needs_implicit_copy_ctor());
        assert!(!class.rule_of_five().needs_implicit_move_ctor());
        assert!(!class.rule_of_five().needs_implicit_dtor());

        Ok(())
    }
}
