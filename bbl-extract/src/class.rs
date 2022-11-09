#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::access_specifier::AccessSpecifier;
use bbl_clang::cursor::{CurClassDecl, Cursor, USR};
use bbl_clang::translation_unit::TranslationUnit;
use bbl_util::{source_iter, Trace};
use log::*;
use regex::Regex;
use std::fmt::{Debug, Display};
use tracing::instrument;

use crate::ast::{
    dump_cursor, dump_cursor_until, get_namespaces_for_decl, get_qualified_name, MethodId, AST,
};
use crate::function::{extract_method, MethodTemplateSpecialization};
use crate::index_map::IndexMapKey;
use crate::qualtype::extract_type;
use crate::stdlib::{
    create_std_map, create_std_string, create_std_unique_ptr, create_std_unordered_map,
    create_std_vector,
};
use crate::templates::{
    extract_class_template_specialization, TemplateArgument, TemplateParameterDecl,
};
use crate::AllowList;
use crate::{function::Method, qualtype::QualType};
use bbl_clang::cursor_kind::CursorKind;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Default)]
pub enum ClassBindKind {
    #[default]
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

#[derive(Default, Clone)]
pub struct NeedsImplicit {
    pub ctor: bool,
    pub copy_ctor: bool,
    pub move_ctor: bool,
    pub copy_assign: bool,
    pub move_assign: bool,
    pub dtor: bool,
}

impl Debug for NeedsImplicit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}{}{}{}{}{}]",
            if self.ctor { "ctor " } else { "" },
            if self.copy_ctor { "cctor " } else { "" },
            if self.move_ctor { "mctor " } else { "" },
            if self.copy_assign { "cass " } else { "" },
            if self.move_assign { "mass " } else { "" },
            if self.dtor { "dtor " } else { "" },
        )
    }
}

impl NeedsImplicit {
    pub fn all() -> NeedsImplicit {
        NeedsImplicit {
            ctor: true,
            copy_ctor: true,
            move_ctor: true,
            copy_assign: true,
            move_assign: true,
            dtor: true,
        }
    }
}

#[derive(Default)]
pub struct ClassDecl {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) fields: Vec<Field>,
    pub(crate) methods: Vec<Method>,
    pub(crate) namespaces: Vec<USR>,
    pub(crate) template_parameters: Vec<TemplateParameterDecl>,
    /// List of the specializations made from this class if it is templated
    pub(crate) specializations: Vec<(Vec<TemplateArgument>, USR)>,

    pub(crate) ignore: bool,
    pub(crate) rename: Option<String>,
    pub(crate) bind_kind: ClassBindKind,
    pub(crate) specialized_methods: Vec<MethodTemplateSpecialization>,
    pub(crate) is_pod: bool,

    pub needs_implicit: NeedsImplicit,
}

impl std::fmt::Debug for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ClassDecl {usr} {name}",
            usr = self.usr,
            name = self.name(),
        )?;

        write!(f, " rename={:?}", self.rename)?;
        write!(f, " {:?}", self.bind_kind())?;
        write!(f, " is_pod={}", self.is_pod)?;
        write!(f, " ignore={}", self.ignore)?;
        write!(f, " needs={:?}", self.needs_implicit())?;
        write!(f, " template_parameters={:?}", self.template_parameters())?;
        write!(f, " specializations={:?}", self.specializations)?;
        write!(f, " namespaces={:?}", self.namespaces())?;
        writeln!(f)?;

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
        needs_implicit: NeedsImplicit,
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
            needs_implicit,
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

    pub fn add_specialization(&mut self, args: Vec<TemplateArgument>, usr: USR) {
        self.specializations.push((args, usr));
    }

    /// Find the specialization of this class template with template arguments matching `args`
    pub fn get_specialization(&self, args: &[TemplateArgument]) -> Option<USR> {
        for (a, usr) in &self.specializations {
            if a == args {
                return Some(*usr);
            }
        }

        None
    }

    pub fn bind_kind(&self) -> &ClassBindKind {
        &self.bind_kind
    }

    pub fn needs_implicit_ctor(&self) -> bool {
        self.needs_implicit.ctor
    }

    pub fn needs_implicit_copy_ctor(&self) -> bool {
        self.needs_implicit.copy_ctor
    }

    pub fn needs_implicit_move_ctor(&self) -> bool {
        self.needs_implicit.move_ctor
    }

    pub fn needs_implicit_dtor(&self) -> bool {
        self.needs_implicit.dtor
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

    pub fn set_ignore(&mut self, _ignore: bool) {
        self.ignore = true;
    }

    pub fn set_rename(&mut self, name: &str) {
        self.rename = Some(name.to_string());
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

    pub fn find_method(&self, signature: &str) -> Result<(MethodId, &Method)> {
        let mut matches = Vec::new();

        for (method_id, method) in self.methods.iter().enumerate() {
            if method.signature().contains(signature) {
                matches.push((method_id, method));
            }
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.methods.len());
                for method in self.methods.iter() {
                    let sig = method.signature();
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

                Err(Error::MethodNotFound {
                    name: signature.to_string(),
                    source: Trace::new(),
                })
            }
            1 => Ok((MethodId::new(matches[0].0), matches[0].1)),
            _ => {
                error!("Multiple matches found for signature \"{signature}\":");

                for (_, method) in matches {
                    error!("  {}", method.signature());
                }

                Err(Error::MultipleMatches {
                    name: signature.to_string(),
                    source: Trace::new(),
                })
            }
        }
    }

    pub fn find_methods(&self, signature: &str) -> Result<(Vec<MethodId>, Vec<&Method>)> {
        let mut matches = Vec::new();

        for (method_id, method) in self.methods.iter().enumerate() {
            if method.signature().contains(signature) {
                matches.push((method_id, method));
            }
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.methods.len());
                for method in self.methods.iter() {
                    let sig = method.signature();
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

                Err(Error::MethodNotFound {
                    name: signature.to_string(),
                    source: Trace::new(),
                })
            }
            _ => Ok(matches
                .into_iter()
                .map(|m| (MethodId::new(m.0), m.1))
                .unzip()),
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
        _ast: &AST,
    ) -> Result<MethodSpecializationId> {
        let method_decl = &mut self.methods[method_id.get()];

        let usr = USR::new(&format!("{}_{name}", method_decl.usr().as_str()));

        let id = self.specialized_methods.len();
        let id = MethodSpecializationId(id);
        method_decl.specializations.push(id);

        let mts = MethodTemplateSpecialization {
            specialized_decl: method_id,
            usr,
            name: name.into(),
            template_arguments,
            namespaces: Vec::new(),
        };

        self.specialized_methods.push(mts);

        Ok(id)
    }

    pub fn needs_implicit(&self) -> &NeedsImplicit {
        &self.needs_implicit
    }
}

impl Display for ClassDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[instrument(skip(tu, ast, already_visited, overrides), level = "trace")]
pub fn extract_class_decl(
    class_decl: CurClassDecl,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    allow_list: &AllowList,
    overrides: &OverrideList,
    // Allow parents to override the namespaces of the children, e.g. UsingDeclarations
    namespace_override: Option<&[USR]>,
    // When encountering a class template specialization, specialize it immediately rather than waiting for the later
    // monomorphize() call
    specialize_immediately: bool,
    stop_on_error: bool,
) -> Result<USR> {
    let namespaces = if let Some(ns) = namespace_override {
        ns.to_vec()
    } else {
        get_namespaces_for_decl(class_decl.into(), tu, ast, already_visited)?
    };

    let class_decl_qualified_name =
        get_qualified_name(&class_decl.display_name(), &namespaces, ast)?;

    // Check for std:: types we're going to extract manually here
    if class_decl.display_name().starts_with("basic_string<") {
        debug!("Manually extracting basic_string {class_decl:?}");
        return create_std_string(class_decl.canonical()?, ast, tu, already_visited);
    } else if class_decl.display_name().starts_with("vector<") {
        debug!("Manually extracting vector {class_decl:?}");
        return create_std_vector(
            class_decl,
            ast,
            already_visited,
            tu,
            allow_list,
            overrides,
            specialize_immediately,
            stop_on_error,
        );
    } else if class_decl.display_name().starts_with("unique_ptr<") {
        debug!("manually extracting std::unique_ptr {class_decl:?}");
        return create_std_unique_ptr(
            class_decl,
            ast,
            already_visited,
            tu,
            allow_list,
            overrides,
            specialize_immediately,
            stop_on_error,
        );
    } else if class_decl.display_name().starts_with("unordered_map<") {
        if class_decl.kind() == CursorKind::ClassDecl {
            debug!("manually extracting std::unordered_map {class_decl:?}");
            return create_std_unordered_map(
                class_decl,
                ast,
                already_visited,
                tu,
                allow_list,
                overrides,
                specialize_immediately,
                stop_on_error,
            );
        }
    } else if class_decl.display_name().starts_with("map<") {
        if class_decl.kind() == CursorKind::ClassDecl {
            debug!("manually extracting std::map {class_decl:?}");
            return create_std_map(
                class_decl,
                ast,
                already_visited,
                tu,
                allow_list,
                overrides,
                specialize_immediately,
                stop_on_error,
            );
        } else {
            debug!("Found map {class_decl:?}");
        }
    } else if class_decl.display_name().starts_with("function<") {
        unreachable!("std::function should have been extracted in type extraction");
        // return create_std_function(class_decl, ast, already_visited, tu, allow_list, overrides);
    }

    for over in &overrides.overs {
        if over.0.is_match(&class_decl_qualified_name) {
            return over.1(
                class_decl.into(),
                ast,
                tu,
                already_visited,
                allow_list,
                overrides,
            );
        }
    }

    if class_decl.specialized_template().is_ok() {
        // this is a class template specialization, handle it separately
        debug!("extract_class_decl: {} is a CTS", class_decl.usr());
        return extract_class_template_specialization(
            class_decl,
            already_visited,
            ast,
            tu,
            allow_list,
            overrides,
            specialize_immediately,
            stop_on_error,
        )
        .map_err(|e| Error::FailedToExtractClassTemplateSpecialization {
            name: class_decl.usr().to_string(),
            source: Box::new(e),
        });
    }

    if already_visited.contains(&class_decl.usr()) {
        debug!("extract_class_decl: already visited {}", class_decl.usr());
        return Ok(class_decl.usr());
    } else {
        already_visited.push(class_decl.usr());
    }

    let cd = extract_class_decl_inner(
        class_decl,
        tu,
        ast,
        already_visited,
        allow_list,
        overrides,
        None,
        namespace_override,
        stop_on_error,
    )
    .map_err(|e| Error::FailedToExtractClass {
        usr: class_decl.usr(),
        source: Box::new(e),
    })?;

    debug!("extract_class_decl: inserting {}", class_decl.usr());
    if class_decl.kind() == CursorKind::ClassTemplate && cd.template_parameters.is_empty() {
        error!(
            "class {:?} is a ClassTemplate but has no template parameters. is def: {}",
            class_decl,
            class_decl.is_definition()
        );
    }

    ast.insert_class(cd);

    Ok(class_decl.usr())
}

fn extract_class_decl_inner(
    class_decl: CurClassDecl,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    qname_override: Option<&str>,
    namespace_override: Option<&[USR]>,
    stop_on_error: bool,
) -> Result<ClassDecl> {
    let namespaces = if let Some(ns) = namespace_override {
        ns.to_vec()
    } else {
        get_namespaces_for_decl(class_decl.into(), tu, ast, already_visited)?
    };

    let mut class_name = class_decl.display_name();
    let mut class_spelling = class_decl.spelling();
    if class_name.is_empty() {
        class_name = "anon".into();
    }
    let class_name = class_name;

    if class_spelling.is_empty() {
        debug!("spelling is empty, class_name is {class_name}");
        class_spelling = "anon".into();
    }
    let class_spelling = class_spelling;

    let class_decl_qualified_name = get_qualified_name(&class_name, &namespaces, ast)?;

    debug!(
        "extract_class_decl({}) {} {}",
        class_decl.usr(),
        class_decl_qualified_name,
        class_decl.num_template_arguments(),
    );

    // First, trawl the bases for all their methods we'll want to inherit
    let mut base_methods = Vec::new();
    let mut base_private_methods = Vec::new();
    let mut base_constructors = Vec::new();
    for c_base in class_decl.children_of_kind(CursorKind::CXXBaseSpecifier, false) {
        if let Ok(c_base_decl) = c_base.referenced() {
            let c_base_decl = c_base_decl.canonical()?;
            let base_decl_namespaces =
                get_namespaces_for_decl(c_base_decl, tu, ast, already_visited)?;
            let base_decl_qualified_name =
                get_qualified_name(&c_base_decl.display_name(), &base_decl_namespaces, ast)?;
            match c_base_decl.kind() {
                CursorKind::ClassDecl | CursorKind::StructDecl | CursorKind::ClassTemplate => {
                    debug!("Extracting base {c_base_decl:?}");
                    let u_base = match extract_class_decl(
                        c_base_decl.try_into()?,
                        tu,
                        ast,
                        already_visited,
                        allow_list,
                        class_overrides,
                        None,
                        true,
                        stop_on_error,
                    )
                    .map_err(|e| {
                        // dump_cursor(c_base_decl, tu);
                        Error::FailedToExtractBaseClass {
                            base: c_base_decl.usr(),
                            usr: class_decl.usr(),
                            source: Box::new(e),
                        }
                    }) {
                        Ok(u) => u,
                        Err(e) if stop_on_error => return Err(e),
                        Err(e) => {
                            warn!("Failed to extract base class {c_base_decl:?} of class {class_decl:?}");
                            for e in source_iter(&e) {
                                warn!("    {e}");
                            }
                            continue;
                        }
                    };

                    let access = c_base.cxx_access_specifier()?;

                    let base = ast.get_class(u_base).unwrap_or_else(|| {
                        panic!(
                            "Failed to get class that should have just been inserted: {u_base:?}"
                        )
                    });

                    for method in &base.methods {
                        if method.is_any_constructor() {
                            // we store constructors regardless of their access since we want to know about private constructors
                            // in order to not generate implicit versions for them
                            base_constructors.push(method.clone());
                        } else if !method.is_destructor() {
                            if access == AccessSpecifier::Public {
                                base_methods.push(method.clone());
                            } else {
                                base_private_methods.push(method.clone())
                            }
                        }
                    }
                }
                CursorKind::TypeAliasTemplateDecl => {
                    if base_decl_qualified_name.contains("allocator<") {
                        return Err(Error::Unsupported {
                            description: "std::allocator".to_string(),
                            source: Trace::new(),
                        });
                    } else {
                        dump_parents(class_decl.try_into().unwrap());
                        unimplemented!(
                            "cannot handle base of TypeAliasTemplateDecl {c_base_decl:?}"
                        );
                    }
                }
                _ => {
                    // dump_cursor(class_decl.into(), tu);
                    unimplemented!("cannot handle base {c_base_decl:?}");
                }
            }
        }
    }

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();

    let class_name = class_decl.spelling();
    let members = class_decl.children();
    let mut index = 0;
    let mut has_private_fields = false;

    let needs_implicit = NeedsImplicit {
        ctor: class_decl.cxxrecord_needs_implicit_default_constructor(),
        copy_ctor: class_decl.cxxrecord_needs_implicit_copy_constructor(),
        move_ctor: class_decl.cxxrecord_needs_implicit_move_constructor(),
        copy_assign: class_decl.cxxrecord_needs_implicit_copy_assignment(),
        move_assign: class_decl.cxxrecord_needs_implicit_move_assignment(),
        dtor: class_decl.cxxrecord_needs_implicit_destructor(),
    };

    for member in members {
        let member_qualified_name = if let Some(qname) = qname_override {
            // allow the subclass to override the qualified name when extracting so we can pull stuff up in the allow list
            format!("{}::{}", qname, member.display_name())
        } else {
            format!("{}::{}", class_decl_qualified_name, member.display_name())
        };

        debug!("member {}", member_qualified_name);

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

                            template_parameters.push(TemplateParameterDecl::Integer {
                                name,
                                default: Some(tu.token(child.location()).spelling()),
                                index,
                            });
                            index += 1;
                        }
                        _ => {
                            return Err(Error::Unsupported {
                                description: format!(
                                "Unsupported template parameter kind {} in {member_qualified_name}",
                                child.kind()
                            ),
                                source: Trace::new(),
                            })
                        }
                    }
                }
            }
            CursorKind::TemplateTemplateParameter => unimplemented!(),
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public
                        && allow_list.allows(&member_qualified_name)
                    {
                        match extract_method(
                            member,
                            &template_parameters,
                            already_visited,
                            tu,
                            ast,
                            allow_list,
                            class_overrides,
                            &class_name,
                            stop_on_error,
                        ) {
                            Ok(method) => methods.push(method),
                            Err(e) if e.is_unsupported() || !stop_on_error => {
                                warn!("Could not extract method {member_qualified_name} which will be ignored");
                                for e in source_iter(&e) {
                                    warn!("    {e}");
                                }
                                continue;
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor {
                        name: member.display_name(),
                        source: Trace::new(),
                    });
                }
            }
            CursorKind::FieldDecl => {
                if let Ok(access) = member.cxx_access_specifier() {
                    if access == AccessSpecifier::Public {
                    } else {
                        has_private_fields = true;
                        // don't extract private fields because we don't want them for the API and they probably contain
                        // all sorts of nastiness.
                        continue;
                    }
                    debug!("    field {}", member.display_name());
                    let field = extract_field(
                        member,
                        &template_parameters,
                        already_visited,
                        ast,
                        tu,
                        allow_list,
                        class_overrides,
                        stop_on_error,
                    )
                    .map_err(|e| Error::FailedToExtractField {
                        class: class_name.clone(),
                        name: member.display_name(),
                        source: Box::new(e),
                    })?;
                    fields.push(field);
                } else {
                    return Err(Error::FailedToGetAccessSpecifierFor {
                        name: member.display_name(),
                        source: Trace::new(),
                    });
                }
            }
            CursorKind::ClassDecl | CursorKind::ClassTemplate => {
                let class = extract_class_decl_inner(
                    member.try_into()?,
                    tu,
                    ast,
                    already_visited,
                    allow_list,
                    class_overrides,
                    qname_override,
                    namespace_override,
                    stop_on_error,
                )?;

                ast.insert_class(class);
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

    // add the inherited methods if they're not overriden in this class
    'outer: for base_method in base_methods {
        for method in &methods {
            if method.signature_matches(&base_method) {
                continue 'outer;
            }
        }

        methods.push(base_method);
    }

    // do the same thing for constructors and update the ROF
    'outer: for base_constructor in base_constructors {
        for method in &methods {
            if method.signature_matches(&base_constructor) {
                continue 'outer;
            }
        }

        methods.push(base_constructor);
    }

    let _name = class_decl.spelling();

    // If this is a template decl we won't be able to get a type from it so just set POD to false
    // Note that we have a slightly different definition of POD from cpp: we do not consider records with private fields
    // to be POD as they're not correctly representable in C.
    // TODO(AL): well, actually... they're not in C but they are in Rust. Though if you have a private field you probably
    // have a non-trivial constructor, but we should probably not force that here
    let is_pod = if template_parameters.is_empty() {
        match class_decl.ty() {
            Ok(ty) => ty.is_pod() && !has_private_fields,
            Err(_) => {
                error!("Could not get type from {class_decl:?}");
                false
            }
        }
    } else {
        false
    };

    let cd = ClassDecl::new(
        class_decl.usr(),
        class_spelling,
        fields,
        methods,
        namespaces,
        template_parameters,
        is_pod,
        needs_implicit,
    );

    Ok(cd)
}

fn dump_parents(c: Cursor) {
    println!("{c:?}");
    if let Ok(p) = c.semantic_parent() {
        dump_parents(p);
    }
}

#[derive(Default)]
pub struct OverrideList {
    overs: Vec<(regex::Regex, Box<ClassExtractionFn>)>,
}

impl OverrideList {
    pub fn new(overs: Vec<(String, Box<ClassExtractionFn>)>) -> Self {
        let overs = overs
            .into_iter()
            .map(|(s, c)| (Regex::new(&s).unwrap(), c))
            .collect();

        OverrideList { overs }
    }
}

impl Debug for OverrideList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: Vec<String> = self
            .overs
            .iter()
            .map(|o| o.0.as_str().to_string())
            .collect();
        write!(f, "[{}]", s.join(", "))
    }
}

pub type ClassExtractionFn =
    fn(Cursor, &mut AST, &TranslationUnit, &mut Vec<USR>, &AllowList, &OverrideList) -> Result<USR>;

// TODO(AL): fix these once we've patched libclang
/*
fn method_is_copy_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}

fn method_is_move_assignment_operator(method: Cursor, class: Cursor) -> bool {
    false
}
*/

pub fn extract_field(
    c_field: Cursor,
    class_template_parameters: &[TemplateParameterDecl],
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<Field> {
    let ty = c_field.ty()?;
    let qual_type = extract_type(
        ty,
        class_template_parameters,
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;

    Ok(Field {
        name: c_field.display_name(),
        qual_type,
    })
}

#[derive(Clone)]
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
                _ => format!("{arg:?}"),
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

    use crate::{
        class::{ClassBindKind, OverrideList},
        error::Error,
        parse_string_and_extract_ast, AllowList,
    };

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
            &AllowList::default(),
            &OverrideList::default(),
            true,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
        ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
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
    fn extract_non_pod() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
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
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
    
        "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }

    #[test]
    fn extract_non_pod2() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            // test that adding a constructor forces non-pod
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                public:
                    Class();
                    float b;
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
                    Method DefaultConstructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@Class# Class rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

        "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }

    #[test]
    fn extract_inherited1() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    namespace detail {
                        class Base {
                            int a;
                        public:
                            float b;
                            Base() = delete;
                            Base(int a, float b);
                            void base_do_thing();
                            virtual void do_thing();
                        };
                    }

                    class Class : public detail::Base {
                    public:
                        float c;
                        void derived_do_thing();
                        void do_thing() override;
                    };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![
                    "^Class.*$".to_string(),
                    "^detail::Base.*$".to_string(),
                ]),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@N@detail detail None
                    Namespace c:@N@detail@S@Base Base None
                    Namespace c:@S@Class Class None
                    ClassDecl c:@N@detail@S@Base Base rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[] specializations=[] namespaces=[c:@N@detail]
                    Field b: float
                    Method DefaultConstructor deleted=true const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base# Base rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base#I#f# Base rename=Some("ctor") ignore=false return=void args=[a: int, b: float] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method Method deleted=false const=false virtual=true pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@do_thing# do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]

                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor ] template_parameters=[] specializations=[] namespaces=[]
                    Field c: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@derived_do_thing# derived_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=true pure_virtual=false specializations=[] Function c:@S@Class@F@do_thing# do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method DefaultConstructor deleted=true const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base# Base rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@detail@S@Base@F@Base#I#f# Base rename=Some("ctor") ignore=false return=void args=[a: int, b: float] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@detail, c:@N@detail@S@Base]

        "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));
            Ok(())
        })
    }

    #[test]
    fn extract_inherited2() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Base {
                    int a;
                    Base();
                    Base(const Base&);
                    Base(Base&&);
                public:
                    float b;
                    void base_do_thing();
                };

                class Class : public Base {
                public:
                    float c;
                    void derived_do_thing();
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Base Base None
                    Namespace c:@S@Class Class None
                    ClassDecl c:@S@Base Base rename=None OpaquePtr is_pod=false ignore=false needs=[dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field b: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Base]

                    ClassDecl c:@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field c: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@derived_do_thing# derived_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Base@F@base_do_thing# base_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Base]

        "#
                ),
            )?;

            let class_id = ast.find_class("Class")?;
            let class = &ast.classes()[class_id];
            assert!(matches!(class.bind_kind(), ClassBindKind::OpaquePtr));

            Ok(())
        })
    }

    #[test]
    fn extract_nested_class() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class {
                public:
                    float class_float;
                    void class_do_thing();

                    class Inner {
                    public:
                        int inner_int();
                        void inner_do_thing();
                    };
                };
            "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    Namespace c:@S@Class@S@Inner Inner None
                    ClassDecl c:@S@Class@S@Inner Inner rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@S@Class]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@S@Inner@F@inner_int# inner_int rename=None ignore=false return=int args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class, c:@S@Class@S@Inner]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@S@Inner@F@inner_do_thing# inner_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class, c:@S@Class@S@Inner]

                    ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Field class_float: float
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@F@class_do_thing# class_do_thing rename=None ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@S@Class]

        "#
                ),
            )?;

            Ok(())
        })
    }
}
