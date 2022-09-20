use std::convert::TryInto;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use bbl_clang::cursor::{CurClassTemplate, CurStructDecl, CurTypedef, Cursor};
use bbl_clang::ty::Type;
use bbl_clang::{cursor::USR, cursor_kind::CursorKind, translation_unit::TranslationUnit};
use tracing::{debug, error, info, instrument, trace, warn};
use ustr::{Ustr, UstrMap};

use crate::class::{ClassBindKind, ClassDecl, MethodSpecializationId};
use crate::function::{extract_function, Function, Method};
use crate::index_map::{IndexMapKey, UstrIndexMap};
use crate::namespace::{self, extract_namespace, Namespace};
use crate::template_argument::{TemplateArgument, TemplateType};
use crate::type_alias;
use crate::type_alias::{ClassTemplateSpecialization, FunctionTemplateSpecialization, TypeAlias};
use crate::{class::extract_class_decl, type_alias::extract_class_template_specialization};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

/// Stores all the extracted AST information that we care about in a flat structure.
///
/// The flat structure makes it easier to translate to C by storing only the information that we care about.
pub struct AST {
    pub(crate) classes: UstrIndexMap<ClassDecl, ClassId>,
    pub(crate) class_template_specializations:
        UstrIndexMap<ClassTemplateSpecialization, ClassTemplateSpecializationId>,
    pub(crate) functions: UstrIndexMap<Function, FunctionId>,
    pub(crate) function_template_specializations:
        UstrIndexMap<FunctionTemplateSpecialization, FunctionTemplateSpecializationId>,
    pub(crate) namespaces: UstrIndexMap<Namespace, NamespaceId>,
    pub(crate) type_aliases: UstrIndexMap<TypeAlias, TypeAliasId>,
    pub(crate) includes: Vec<Include>,
}

impl Debug for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for include in self.includes.iter() {
            writeln!(f, "{include:?}")?;
        }

        for namespace in self.namespaces.iter() {
            writeln!(f, "{namespace:?}")?;
        }

        for class in self.classes.iter() {
            writeln!(f, "{class:?}")?;
        }

        for function in self.functions.iter() {
            writeln!(f, "{function:?}")?;
        }

        for type_alias in self.type_aliases.iter() {
            writeln!(f, "{type_alias:?}")?;
        }

        Ok(())
    }
}

/// A `#include` directive pulled from the parsed translation unit
#[derive(Debug, Clone)]
pub struct Include {
    name: String,    //< The filename from the include directive
    bracket: String, //< What kind of "bracket" was around the filename, i.e. '<' or '"'
}

impl Include {
    /// Construct a new [`Include`]
    pub fn new(name: String, bracket: String) -> Include {
        Include { name, bracket }
    }

    /// Get the name of the included file.
    ///
    /// i.e. for `#include <dir/file.h>` this would return "dir/file.h"
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the type of "bracket" around the file.
    ///
    /// i.e. for `#include <dir/file.h>` this would return "<"
    pub fn bracket(&self) -> &str {
        &self.bracket
    }

    /// Get the formatted filename, including brackets.
    ///
    /// i.e. for `#include <dir/file.h>` this would return "<dir/file.h>""
    pub fn get_bracketed_name(&self) -> String {
        format!("{0}{1}{0}", self.bracket(), self.name())
    }

    /// Get the full include statement, including `#include`
    pub fn get_statement(&self) -> String {
        format!(
            "#include {}{}{}",
            self.bracket(),
            self.name(),
            if self.bracket() == "<" { ">" } else { "\"" }
        )
    }
}

impl Default for AST {
    fn default() -> Self {
        AST::new()
    }
}

impl AST {
    /// Construct a new, empty [`AST`]
    pub fn new() -> Self {
        AST {
            classes: UstrIndexMap::new(),
            class_template_specializations: UstrIndexMap::new(),
            functions: UstrIndexMap::new(),
            function_template_specializations: UstrIndexMap::new(),
            namespaces: UstrIndexMap::new(),
            type_aliases: UstrIndexMap::new(),
            includes: Vec::new(),
        }
    }

    /// Get the list of includes extracted from the translation unit as a slice
    pub fn includes(&self) -> &[Include] {
        &self.includes
    }

    /// Get a reference to the map holding all the [`ClassDecl`]s extracted from the translation unit
    pub fn classes(&self) -> &UstrIndexMap<ClassDecl, ClassId> {
        &self.classes
    }

    pub fn classe_template_specializations(
        &self,
    ) -> &UstrIndexMap<ClassTemplateSpecialization, ClassTemplateSpecializationId> {
        &self.class_template_specializations
    }

    /// Get a reference to the map holding all the [`Function`]s extracted from the translation unit
    pub fn functions(&self) -> &UstrIndexMap<Function, FunctionId> {
        &self.functions
    }

    pub fn function_template_specializations(
        &self,
    ) -> &UstrIndexMap<FunctionTemplateSpecialization, FunctionTemplateSpecializationId> {
        &self.function_template_specializations
    }

    /// Get a reference to the map holding all the [`TypeAlias`]es extracted from the translation unit
    pub fn type_aliases(&self) -> &UstrIndexMap<TypeAlias, TypeAliasId> {
        &self.type_aliases
    }

    /// Get a reference to the map holding all the [`Namespace`]s extracted from the translation unit
    pub fn namespaces(&self) -> &UstrIndexMap<Namespace, NamespaceId> {
        &self.namespaces
    }

    /// Find a namespace by exact match on the name and return its id, which can then be used to get a reference to the
    /// actual [`Namespace`]
    ///
    /// # Returns
    /// * [`NamespaceId`] if a [`Namespace`] with name `name` exists
    /// * [`Error::NamespaceNotFound`] if no [`Namespace`] with name `name` exists
    pub fn find_namespace(&self, name: &str) -> Result<NamespaceId> {
        for namespace in self.namespaces.iter() {
            if namespace.name == name {
                return self
                    .namespaces
                    .get_id(&namespace.usr().into())
                    .map(|i| NamespaceId(*i))
                    .ok_or_else(|| Error::NamespaceNotFound(name.to_string()));
            }
        }

        Err(Error::NamespaceNotFound(name.to_string()))
    }

    /// Find a class by exact match on the name and return its id, which can then be used to get a reference to the
    /// actual [`ClassDecl`]
    ///
    /// # Returns
    /// * [`ClassDeclId`] if a [`ClassDecl`] with name `name` exists
    /// * [`Error::ClassDeclNotFound`] if no [`ClassDecl`] with name `name` exists
    pub fn find_class(&self, name: &str) -> Result<ClassId> {
        // Iterate over all classes and check whether their qualified name fully or partially matches the provided name
        let mut matches = Vec::new();
        let mut qnames = Vec::new();
        for (class_id, class) in self.classes.iter().enumerate() {
            let qname =
                class
                    .get_qualified_name(self)
                    .map_err(|e| Error::FailedToGetQualifiedNameFor {
                        name: class.name().to_string(),
                        source: Box::new(e),
                    })?;

            // Should only ever have one match for the qualified name or it would be a compile error on the cpp side
            if qname == name {
                return Ok(ClassId::new(class_id));
            } else if qname.contains(name) {
                matches.push((class_id, class, qname.clone()));
            }

            qnames.push(qname);
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.classes.len());
                for qname in &qnames {
                    let dist = levenshtein::levenshtein(qname, name);
                    distances.push((dist, qname));
                }

                distances.sort_by(|a, b| a.0.cmp(&b.0));

                error!("Could not find class matching qualified name: \"{}\"", name,);
                error!("Did you mean one of:");
                for (_, sug) in distances.iter().take(3) {
                    error!("  {sug}");
                }

                Err(Error::ClassNotFound(name.into()))
            }
            1 => Ok(ClassId::new(matches[0].0)),
            _ => {
                error!("Multiple matches found for class \"{name}\":");

                for (_, _, qname) in matches {
                    error!("  {}", qname);
                }

                Err(Error::MultipleMatches)
            }
        }
    }

    /// Make a new [`ClassTemplateSpecialization`] from the [`ClassDecl`] with id `class_id` with template arguments `args`.
    pub fn specialize_class(
        &mut self,
        class_id: ClassId,
        name: &str,
        args: Vec<Option<TemplateType>>,
    ) -> Result<ClassTemplateSpecializationId> {
        let class_decl = self.classes.index_mut(class_id);

        let usr = USR::new(&format!("{}_{name}", class_decl.usr().as_str()));

        let cts = ClassTemplateSpecialization {
            specialized_decl: class_decl.usr(),
            usr,
            name: name.into(),
            template_arguments: args,
            namespaces: Vec::new(),
        };

        let id = self
            .class_template_specializations
            .insert(usr.into(), cts);

        let id = ClassTemplateSpecializationId(id);

        class_decl.specializations.push(id);

        Ok(id)
    }

    /// Set the bind kind of the [`ClassDecl`] with id `class_id`
    pub fn class_set_bind_kind(
        &mut self,
        class_id: ClassId,
        bind_kind: ClassBindKind,
    ) -> Result<()> {
        let could_be = self.classes.index(class_id).could_be_valuetype(self)?;

        let class_decl = self.classes.index_mut(class_id);
        if could_be {
            match bind_kind {
                ClassBindKind::OpaquePtr => {
                    class_decl.set_bind_kind(bind_kind);
                    Ok(())
                }
                ClassBindKind::ValueType => {
                    if could_be {
                        class_decl.set_bind_kind(bind_kind);
                    }

                    Ok(())
                }
                ClassBindKind::OpaqueBytes => todo!("Handle opaquebytes"),
            }
        } else {
            Err(Error::ClassCannotBeValueType(class_decl.name().to_string()))
        }
    }

    /// Find a function by exact match on the name and return its id, which can then be used to get a reference to the
    /// actual [`Function`]
    ///
    /// # Returns
    /// * [`FunctionId`] if a [`Function`] with name `name` exists
    /// * [`Error::FunctionNotFound`] if no [`Function`] with name `name` exists
    pub fn find_function(&self, signature: &str) -> Result<FunctionId> {
        let mut matches = Vec::new();

        for (function_id, function) in self.functions.iter().enumerate() {
            if function.signature(self, &[], None).contains(signature) {
                matches.push((function_id, function));
            }
        }

        match matches.len() {
            0 => {
                let mut distances = Vec::with_capacity(self.functions.len());
                for function in self.functions.iter() {
                    let sig = function.signature(self, &[], None);
                    let dist = levenshtein::levenshtein(&sig, signature);
                    distances.push((dist, sig));
                }

                distances.sort_by(|a, b| a.0.cmp(&b.0));

                error!(
                    "Could not find function matching signature: \"{}\"",
                    signature
                );
                error!("Did you mean one of:");
                for (_, sug) in distances.iter().take(3) {
                    error!("  {sug}");
                }

                Err(Error::FunctionNotFound(signature.into()))
            }
            1 => Ok(FunctionId(matches[0].0)),
            _ => {
                error!("Multiple matches found for signature \"{signature}\":");

                for (_, function) in matches {
                    error!("  {}", function.signature(self, &[], None));
                }

                Err(Error::MultipleMatches)
            }
        }
    }

    /// Create a new [`FunctionTemplateSpecialization`] from this template function with the given template arguments
    pub fn specialize_function(
        &mut self,
        function_id: FunctionId,
        name: &str,
        template_arguments: Vec<Option<TemplateType>>,
    ) -> Result<FunctionTemplateSpecializationId> {
        let function_decl = self.functions.index_mut(function_id);

        let usr = USR::new(&format!("{}_{name}", function_decl.usr().as_str()));

        let fts = FunctionTemplateSpecialization {
            specialized_decl: function_decl.usr(),
            usr,
            name: name.into(),
            template_arguments,
            namespaces: Vec::new(),
        };

        let id = self
            .function_template_specializations
            .insert(usr.into(), fts);

        let id = FunctionTemplateSpecializationId(id);

        function_decl.specializations.push(id);

        Ok(id)
    }

    /// Sets the external name of the namespace.
    ///
    /// When classes and methods are converted to their C representations, their names are prepended with the full
    /// namespace scope. For instance:
    /// ```c++
    /// Imath_3_1::V3f::cross()
    /// ```
    /// becomes:
    /// ```c
    /// Imath_3_1_V3f_cross()
    /// ```
    /// Renaming the namespace allows an external name that matches the external namespace from C++, rather than the
    /// extracted one, which may be versioned:
    /// ```c
    /// Imath_V3f_cross()
    /// ```
    pub fn rename_namespace(&mut self, namespace_id: NamespaceId, new_name: &str) {
        self.namespaces.index_mut(namespace_id).rename(new_name);
    }

    /// Find a method on the class with id `class_id` by exact match on the name and return its id, which can then be
    /// used to get a reference to the actual [`Method`]
    ///
    /// # Returns
    /// * [`MethodId`] if a [`Method`] with name `name` exists
    /// * [`Error::MethodNotFound`] if no [`Method`] with name `name` exists
    pub fn find_method(&self, class_id: ClassId, signature: &str) -> Result<MethodId> {
        let class = self.classes.index(class_id);
        class.find_method(self, signature).map(|t| t.0)
    }

    /// Add a specialization for the template [`Method`] with id `method_id` on the [`ClassDecl`] with id `class_id`.
    ///
    /// The arguments in `args` are applied to the method template in the order in which they are given, which is assumed
    /// to match the order in which they are declared.
    pub fn specialize_method(
        &mut self,
        class_id: ClassId,
        method_id: MethodId,
        name: &str,
        args: Vec<Option<TemplateType>>,
    ) -> Result<MethodSpecializationId> {
        self.classes
            .index_mut(class_id)
            .specialize_method(method_id, name, args)
    }

    /// Rename a method when it is translated
    ///
    /// This can be used to disambiguate overrides
    pub fn rename_method(&mut self, class_id: ClassId, method_id: MethodId, new_name: &str) {
        self.classes
            .index_mut(class_id)
            .rename_method(method_id, new_name);
    }

    /// Don't translate this method
    pub fn ignore_method(&mut self, class_id: ClassId, method_id: MethodId) {
        self.classes.index_mut(class_id).ignore_method(method_id);
    }

    pub fn insert_class(&mut self, class: ClassDecl) {
        self.classes.insert(class.usr().into(), class);
    }

    pub fn insert_class_template_specialization(&mut self, class: ClassTemplateSpecialization) {
        self.class_template_specializations.insert(class.usr().into(), class);
    }

    pub fn get_type_alias(&self, usr: USR) -> Option<&TypeAlias> {
        self.type_aliases.get(&usr.into())
    }

    pub fn get_class(&self, usr: USR) -> Option<&ClassDecl> {
        self.classes.get(&usr.into())
    }

    pub fn get_class_template_specialization(&self, usr: USR) -> Option<&ClassTemplateSpecialization> {
        self.class_template_specializations.get(&usr.into())
    }

    pub fn insert_function(&mut self, function: Function) {
        self.functions.insert(function.usr().into(), function);
    }

    pub fn insert_function_template_specialization(&mut self, function: FunctionTemplateSpecialization) {
        self.function_template_specializations.insert(function.usr().into(), function);
    }

    pub fn insert_type_alias(&mut self, type_alias: TypeAlias) -> usize {
        self.type_aliases
            .insert(type_alias.usr().into(), type_alias)
    }

    pub fn get_function(&self, usr: USR) -> Option<&Function> {
        self.functions.get(&usr.into())
    }

    pub fn function(&self, id: FunctionId) -> &Function {
        self.functions.index(id)
    }

    pub fn insert_namespace(&mut self, namespace: Namespace) {
        self.namespaces.insert(namespace.usr().into(), namespace);
    }

    pub fn get_namespace(&self, usr: USR) -> Option<&Namespace> {
        self.namespaces.get(&usr.into())
    }

    pub fn get_class_or_namespace_names(&self, usr: USR) -> Result<(&str, Option<&String>)> {
        if let Some(ns) = self.namespaces.get(&usr.into()) {
            Ok((&ns.name, ns.rename.as_ref()))
        } else if let Some(class) = self.classes.get(&usr.into()) {
            Ok((&class.name, class.rename.as_ref()))
        } else {
            Err(Error::ClassOrNamespaceNotFound(usr))
        }
    }

    pub fn pretty_print(&self, depth: usize) {
        for inc in self.includes.iter() {
            println!("{}", inc.get_statement());
        }

        for namespace in self.namespaces.iter() {
            namespace.pretty_print(depth + 1, self);
            println!();
        }

        println!();
        for class in self.classes.iter() {
            class.pretty_print(depth + 1, self, None);
            println!();
        }

        println!();
        for function in self.functions.iter() {
            function.pretty_print(depth + 1, self, &[], None);
            println!();
        }

        for type_alias in self.type_aliases.iter() {
            type_alias.pretty_print(depth + 1, self, &[]);
            println!();
        }
    }
}

pub fn get_qualified_name(decl: &str, namespaces: &[USR], ast: &AST) -> Result<String> {
    let mut result = String::new();
    for uns in namespaces {
        if let Some(ns) = ast.get_namespace(*uns) {
            result = format!("{result}{}::", ns.name());
        } else if let Some(class) = ast.get_class(*uns) {
            result = format!("{result}{}::", class.name());
        } else {
            return Err(Error::ClassOrNamespaceNotFound(*uns));
        }
    }

    result = format!("{result}{decl}");

    Ok(result)
}

/// Main recursive function to walk the AST and extract the pieces we're interested in
#[instrument(
    skip(depth, max_depth, tu, namespaces, already_visited),
    level = "trace"
)]
pub fn extract_ast(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    namespaces: Vec<USR>,
) -> Result<()> {
    let mut namespaces = namespaces;

    if depth > max_depth {
        // println!("");
        return Ok(());
    }
    let indent = format!("{:width$}", "", width = depth * 2);

    match c.kind() {
        CursorKind::ClassDecl => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            // Also make sure that we're dealing with a definition rather than a forward declaration
            // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
            // (for opaque types in the API)
            if c.is_definition() {
                extract_class_decl(
                    c.try_into()?,
                    depth + 1,
                    tu,
                    &namespaces,
                    ast,
                    already_visited,
                )?;
            }

            return Ok(());
        }
        CursorKind::ClassTemplate => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            // Also make sure that we're dealing with a definition rather than a forward declaration
            // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
            // (for opaque types in the API)
            if c.is_definition() {
                let c_class_template: CurClassTemplate = c.try_into()?;
                extract_class_decl(
                    c_class_template.as_class_decl(),
                    depth + 1,
                    tu,
                    &namespaces,
                    ast,
                    already_visited,
                )?;
            }

            return Ok(());
        }
        CursorKind::StructDecl => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            // Also make sure that we're dealing with a definition rather than a forward declaration
            // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
            // (for opaque types in the API)
            if c.is_definition() {
                let c_struct: CurStructDecl = c.try_into()?;
                extract_class_decl(
                    c_struct.as_class_decl(),
                    depth + 1,
                    tu,
                    &namespaces,
                    ast,
                    already_visited,
                )?;
            }

            return Ok(());
        }
        CursorKind::TypeAliasDecl | CursorKind::TypedefDecl => {
            // check if this type alias has a TemplateRef child, in which case it's a class template specialization
            if c.has_child_of_kind(CursorKind::TemplateRef) {
                let cts = extract_class_template_specialization(
                    c.try_into()?,
                    depth + 1,
                    already_visited,
                    ast,
                    tu,
                    &namespaces,
                )
                .map_err(|e| {
                    Error::FailedToExtractClassTemplateSpecialization {
                        name: c.display_name(),
                        source: Box::new(e),
                    }
                })?;
                ast.insert_class_template_specialization(cts);
            } else {
                debug!(
                    "TypeAliasDecl {} not handled as it is not a CTS",
                    c.display_name()
                );
            }

            return Ok(());
        }
        CursorKind::Namespace => {
            let usr = extract_namespace(c, depth, tu, ast);
            let name = ast
                .get_namespace(usr)
                .ok_or_else(|| Error::NamespaceNotFound(usr.to_string()))?
                .name();

            // We bail out on std and will insert manual AST for the types we support because fuck dealing with that
            // horror show
            if name == "std" {
                let children = c.children_of_kind(CursorKind::Namespace, true);

                for child in children {
                    extract_namespace(child, depth, tu, ast);
                }

                debug!("Found std namespace, bailing early");
                return Ok(());
            }
        }
        CursorKind::FunctionDecl | CursorKind::FunctionTemplate => {
            let fun =
                extract_function(c, depth + 1, &[], already_visited, tu, ast).map_err(|e| {
                    Error::FailedToExtractFunction {
                        name: c.display_name(),
                        source: Box::new(e),
                    }
                })?;
            ast.insert_function(fun);
            already_visited.push(c.usr());

            return Ok(());
        }
        // CursorKind::NamespaceRef => {

        // }
        _ => (),
    }

    trace!("{indent}{}: {} {}", c.kind(), c.display_name(), c.usr());

    if let Ok(cr) = c.referenced() {
        if cr != c && !already_visited.contains(&cr.usr()) {
            // print!("{}-> ", indent);
            if !cr.usr().is_empty() {
                already_visited.push(cr.usr());
            }
            extract_ast(
                cr,
                depth + 1,
                max_depth,
                already_visited,
                ast,
                tu,
                namespaces.clone(),
            )?;
        } else {
            debug!("{indent} already visited {cr:?}, skipping...");
        }
    }

    let children = c.children();

    for child in children {
        extract_ast(
            child,
            depth + 1,
            max_depth,
            already_visited,
            ast,
            tu,
            namespaces.clone(),
        )?;
    }

    Ok(())
}

pub fn dump_cursor(c: Cursor, tu: &TranslationUnit) {
    let mut av = Vec::new();
    dump(c, 0, 20, &mut av, tu, &[], None);
}

use colored::*;
pub fn dump(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    skip_kinds: &[CursorKind],
    color_s: Option<Color>,
) {
    if depth > max_depth {
        println!("⋱");
        return;
    }

    let indent = format!("{:width$}", "", width = depth * 4);

    let template_args = if c.num_template_arguments() != -1 {
        format!("[{}]", c.num_template_arguments())
    } else {
        "".to_string()
    };

    let color_s = if let Some(color_s) = color_s {
        color_s
    } else {
        Color::White
    };

    match c.kind() {
        CursorKind::IntegerLiteral => {
            println!("{}: {}", c.kind(), tu.token(c.location()).spelling());
        }
        _ => println!(
            "{}",
            format!(
                "{}: {} {} {} {}",
                c.kind(),
                c.display_name(),
                c.usr(),
                template_args,
                if c.is_definition() { "[def]" } else { "" },
            )
            .color(color_s)
        ),
    }

    if let Ok(ty) = c.ty() {
        print!("{}", format!("{indent}  ").color(color_s));
        dump_type(
            ty,
            depth,
            max_depth,
            already_visited,
            tu,
            skip_kinds,
            Some(color_s),
        );
    }

    if let Ok(cr) = c.referenced() {
        if cr != c {
            if already_visited.contains(&cr.usr()) {
                let template_args = if c.num_template_arguments() != -1 {
                    format!("[{}]", c.num_template_arguments())
                } else {
                    "".to_string()
                };

                println!(
                    "{}",
                    format!(
                        "{indent}↪ {}: {} {} {} 🗸",
                        cr.kind(),
                        cr.display_name(),
                        cr.usr(),
                        template_args
                    )
                    .color(Color::BrightBlack)
                );
            } else {
                if !cr.usr().is_empty() {
                    already_visited.push(cr.usr());
                }
                if !skip_kinds.contains(&cr.kind()) {
                    print!("{indent}↪ ");
                    dump(
                        cr,
                        depth + 1,
                        max_depth,
                        already_visited,
                        tu,
                        skip_kinds,
                        Some(Color::Cyan),
                    );
                }
            }
        }
    }

    if let Ok(cr) = c.specialized_template() {
        if cr != c {
            if already_visited.contains(&cr.usr()) {
                let template_args = if c.num_template_arguments() != -1 {
                    format!("[{}]", c.num_template_arguments())
                } else {
                    "".to_string()
                };

                println!(
                    "{}",
                    format!(
                        "{indent}↪ {}: {} {} {} 🗸",
                        cr.kind(),
                        cr.display_name(),
                        cr.usr(),
                        template_args
                    )
                    .color(Color::BrightBlack)
                );
            } else {
                if !cr.usr().is_empty() {
                    already_visited.push(cr.usr());
                }
                if !skip_kinds.contains(&cr.kind()) {
                    print!("{indent}↪ ");
                    dump(
                        cr,
                        depth + 1,
                        max_depth,
                        already_visited,
                        tu,
                        skip_kinds,
                        Some(Color::Cyan),
                    );
                }
            }
        }
    }

    let children = c.children();

    for child in children {
        if !child.usr().is_empty() {
            already_visited.push(child.usr());
        }

        if skip_kinds.contains(&child.kind()) {
            continue;
        }

        let (icon, color_s) = match child.kind() {
            CursorKind::ClassDecl => ("●", Color::BrightGreen),
            CursorKind::ClassTemplate => ("○", Color::BrightGreen),
            CursorKind::FieldDecl => ("▸", Color::Green),
            CursorKind::CXXAccessSpecifier => ("▸", Color::Green),
            CursorKind::FunctionDecl => ("ƒ", Color::BrightCyan),
            CursorKind::FunctionTemplate => ("ⓕ", Color::BrightCyan),
            CursorKind::CXXMethod => ("ɱ", Color::BrightBlue),
            CursorKind::TemplateTypeParameter
            | CursorKind::TemplateTemplateParameter
            | CursorKind::NonTypeTemplateParameter => ("▸", Color::Yellow),
            CursorKind::TypeAliasDecl | CursorKind::TypedefDecl => ("Τ", Color::BrightMagenta),
            _ => ("▸", Color::White),
        };

        print!("{}", format!("{indent}{icon} ").color(color_s));

        dump(
            child,
            depth + 1,
            max_depth,
            already_visited,
            tu,
            skip_kinds,
            Some(color_s),
        );
    }
}

pub fn dump_type(
    ty: Type,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    skip_kinds: &[CursorKind],
    color_s: Option<Color>,
) {
    let color_s = if let Some(color_s) = color_s {
        color_s
    } else {
        Color::White
    };

    let args = ty.template_argument_types().map(|v| {
        v.iter()
            .map(|t| {
                if let Some(t) = t {
                    format!("{}", t)
                } else {
                    "NonType".to_string()
                }
            })
            .collect::<Vec<String>>()
    });

    let template_args = if let Some(args) = args {
        format!("<{}>", args.join(", "))
    } else {
        "".to_string()
    };

    let pod = if ty.is_pod() { "[pod]" } else { "" };
    let builtin = if ty.is_builtin() { "[builtin]" } else { "" };

    let decl = if let Ok(c) = ty.type_declaration() {
        format!("({c:?})")
    } else {
        "()".to_string()
    };

    println!(
        "{}",
        format!(
            "𝜏 {}: {} {} {builtin} {pod} {decl}",
            ty.spelling(),
            ty.kind(),
            template_args
        )
        .italic()
        .color(color_s)
    );

    let indent = format!("{:width$}", "", width = depth * 4);
    if let Ok(c_decl) = ty.type_declaration() {
        if !already_visited.contains(&c_decl.usr()) {
            already_visited.push(c_decl.usr());
            print!("{}", format!("{indent}  = ").color(Color::BrightBlack));
            dump(
                c_decl,
                depth + 1,
                max_depth,
                already_visited,
                tu,
                skip_kinds,
                Some(Color::BrightBlack),
            );
        } else {
            println!(
                "{}",
                format!(
                    "{indent}  = {}: {} {} {} 🗸",
                    c_decl.kind(),
                    c_decl.display_name(),
                    c_decl.usr(),
                    template_args
                )
                .color(Color::BrightBlack)
            );
        }
        if matches!(
            c_decl.kind(),
            CursorKind::TypedefDecl | CursorKind::TypeAliasDecl
        ) {
            let c_td: CurTypedef = c_decl.try_into().unwrap();
            let ty = c_td.underlying_type().unwrap();
            print!("{}", format!("{indent}  ↪u→ ").color(color_s));
            dump_type(
                ty,
                depth + 1,
                max_depth,
                already_visited,
                tu,
                skip_kinds,
                Some(color_s),
            );
        }
    }

    // if let Ok(ty) = ty.named_type() {
    //     print!("{}", format!("{indent}  ↪n→ ").color(color_s));
    //     dump_type(ty, depth+1, max_depth, already_visited, tu, skip_kinds, Some(color_s));
    // }
}

#[instrument(level = "trace", skip(tu))]
pub fn extract_ast_from_namespace(
    name: Option<&str>,
    c_tu: Cursor,
    tu: &TranslationUnit,
) -> Result<AST> {
    let ns = if let Some(name) = name {
        if name.is_empty() {
            c_tu.children()
        } else {
            c_tu.children_of_kind_with_name(CursorKind::Namespace, name, true)
        }
    } else {
        c_tu.children()
    };

    let mut ast = AST::new();

    tu.get_inclusions(|file, locations| {
        if locations.len() == 1 {
            for location in locations {
                let name = tu.get_cursor_at_location(location).unwrap().display_name();
                ast.includes.push(Include::new(
                    name,
                    tu.token(*location)
                        .spelling()
                        .get(0..1)
                        .unwrap()
                        .to_string(),
                ));
            }
        }
    });

    let namespaces = Vec::new();
    let mut already_visited = Vec::new();
    for cur in ns {
        extract_ast(
            cur,
            0,
            100,
            &mut already_visited,
            &mut ast,
            tu,
            namespaces.clone(),
        )?;
    }

    Ok(ast)
}

// walk back up through a cursor's semantic parents and add them as namespaces
pub fn walk_namespaces(
    c: Result<Cursor, bbl_clang::error::Error>,
    namespaces: &mut Vec<USR>,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
) -> Result<()> {
    if let Ok(c) = c {
        if c.kind() != CursorKind::TranslationUnit {
            if ast.get_namespace(c.usr()).is_none() {
                extract_namespace(c, 0, tu, ast);
            }

            namespaces.push(c.usr());
            walk_namespaces(c.semantic_parent(), namespaces, tu, ast, already_visited);
        }
    }

    Ok(())
}

pub fn get_namespaces_for_decl(
    c: Cursor,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
) -> Result<Vec<USR>> {
    let mut namespaces = Vec::new();
    walk_namespaces(
        c.semantic_parent(),
        &mut namespaces,
        tu,
        ast,
        already_visited,
    );
    namespaces.reverse();
    Ok(namespaces)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClassId(usize);

impl ClassId {
    pub fn new(id: usize) -> ClassId {
        ClassId(id)
    }
}

impl IndexMapKey for ClassId {
    fn get(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClassTemplateSpecializationId(usize);

impl ClassTemplateSpecializationId {
    pub fn new(id: usize) -> ClassTemplateSpecializationId {
        ClassTemplateSpecializationId(id)
    }
}

impl IndexMapKey for ClassTemplateSpecializationId {
    fn get(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct MethodId(usize);

impl IndexMapKey for MethodId {
    fn get(&self) -> usize {
        self.0
    }
}

impl MethodId {
    pub fn new(id: usize) -> MethodId {
        MethodId(id)
    }
}

impl From<MethodId> for usize {
    fn from(id: MethodId) -> Self {
        id.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NamespaceId(usize);

impl IndexMapKey for NamespaceId {
    fn get(&self) -> usize {
        self.0
    }
}

impl NamespaceId {
    pub fn new(id: usize) -> NamespaceId {
        NamespaceId(id)
    }
}

impl From<NamespaceId> for usize {
    fn from(id: NamespaceId) -> Self {
        id.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FunctionId(usize);

impl IndexMapKey for FunctionId {
    fn get(&self) -> usize {
        self.0
    }
}

impl FunctionId {
    pub fn new(id: usize) -> FunctionId {
        FunctionId(id)
    }
}

impl From<FunctionId> for usize {
    fn from(id: FunctionId) -> Self {
        id.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FunctionTemplateSpecializationId(usize);

impl IndexMapKey for FunctionTemplateSpecializationId {
    fn get(&self) -> usize {
        self.0
    }
}

impl FunctionTemplateSpecializationId {
    pub fn new(id: usize) -> FunctionTemplateSpecializationId {
        FunctionTemplateSpecializationId(id)
    }
}

impl From<FunctionTemplateSpecializationId> for usize {
    fn from(id: FunctionTemplateSpecializationId) -> Self {
        id.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeAliasId(usize);

impl IndexMapKey for TypeAliasId {
    fn get(&self) -> usize {
        self.0
    }
}

impl TypeAliasId {
    pub fn new(id: usize) -> TypeAliasId {
        TypeAliasId(id)
    }
}

impl From<TypeAliasId> for usize {
    fn from(id: TypeAliasId) -> Self {
        id.0
    }
}
