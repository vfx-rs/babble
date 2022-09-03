use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use bbl_clang::cursor::Cursor;
use bbl_clang::{cursor::USR, cursor_kind::CursorKind, translation_unit::TranslationUnit};
use tracing::instrument;
use ustr::{Ustr, UstrMap};

use log::*;

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

pub struct AST {
    pub(crate) classes: UstrIndexMap<ClassDecl, ClassId>,
    pub(crate) functions: UstrIndexMap<Function, FunctionId>,
    pub(crate) namespaces: UstrIndexMap<Namespace, NamespaceId>,
    pub(crate) type_aliases: UstrIndexMap<TypeAlias, TypeAliasId>,
    pub(crate) includes: Vec<Include>,
}

impl Debug for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AST({} classes, {} functions, {} namespaces, {} type aliases, {} includes)",
            self.classes().len(),
            self.functions().len(),
            self.namespaces().len(),
            self.type_aliases().len(),
            self.includes().len()
        )
    }
}

#[derive(Debug, Clone)]
pub struct Include {
    name: String,
    bracket: String,
}

impl Include {
    pub fn new(name: String, bracket: String) -> Include {
        Include { name, bracket }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn bracket(&self) -> &str {
        &self.bracket
    }

    pub fn get_bracketed_name(&self) -> String {
        format!("{0}{1}{0}", self.bracket(), self.name())
    }

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
    pub fn new() -> Self {
        AST {
            classes: UstrIndexMap::new(),
            functions: UstrIndexMap::new(),
            namespaces: UstrIndexMap::new(),
            type_aliases: UstrIndexMap::new(),
            includes: Vec::new(),
        }
    }

    pub fn includes(&self) -> &[Include] {
        &self.includes
    }

    pub fn classes(&self) -> &UstrIndexMap<ClassDecl, ClassId> {
        &self.classes
    }
    pub fn functions(&self) -> &UstrIndexMap<Function, FunctionId> {
        &self.functions
    }
    pub fn type_aliases(&self) -> &UstrIndexMap<TypeAlias, TypeAliasId> {
        &self.type_aliases
    }
    pub fn namespaces(&self) -> &UstrIndexMap<Namespace, NamespaceId> {
        &self.namespaces
    }

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

    /// Make a new [`ClassTemplateSpecialization`] from this class with the given template arguments
    pub fn specialize_class(
        &mut self,
        class_id: ClassId,
        name: &str,
        args: Vec<Option<TemplateType>>,
    ) -> Result<TypeAliasId> {
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
            .type_aliases
            .insert(usr.into(), TypeAlias::ClassTemplateSpecialization(cts));

        let id = TypeAliasId(id);

        class_decl.specializations.push(id);

        Ok(id)
    }

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
                ClassBindKind::OpaqueBytes => todo!(),
                ClassBindKind::ValueType => {
                    if could_be {
                        class_decl.set_bind_kind(bind_kind);
                    }

                    Ok(())
                }
            }
        } else {
            Err(Error::ClassCannotBeValueType(class_decl.name().to_string()))
        }
    }

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
    ) -> Result<TypeAliasId> {
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
            .type_aliases
            .insert(usr.into(), TypeAlias::FunctionTemplateSpecialization(fts));

        let id = TypeAliasId(id);

        function_decl.specializations.push(id);

        Ok(id)
    }

    pub fn rename_namespace(&mut self, namespace_id: NamespaceId, new_name: &str) {
        self.namespaces.index_mut(namespace_id).rename(new_name);
    }

    pub fn find_method(&self, class_id: ClassId, signature: &str) -> Result<MethodId> {
        let class = self.classes.index(class_id);
        class.find_method(self, signature).map(|t| t.0)
    }

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

    pub fn rename_method(&mut self, class_id: ClassId, method_id: MethodId, new_name: &str) {
        self.classes
            .index_mut(class_id)
            .rename_method(method_id, new_name);
    }

    pub fn ignore_method(&mut self, class_id: ClassId, method_id: MethodId) {
        self.classes.index_mut(class_id).ignore_method(method_id);
    }

    pub fn insert_class(&mut self, class: ClassDecl) {
        self.classes.insert(class.usr().into(), class);
    }

    pub fn get_type_alias(&self, usr: USR) -> Option<&TypeAlias> {
        self.type_aliases.get(&usr.into())
    }

    pub fn get_class(&self, usr: USR) -> Option<&ClassDecl> {
        self.classes.get(&usr.into())
    }

    pub fn insert_function(&mut self, function: Function) {
        self.functions.insert(function.usr().into(), function);
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
#[instrument(skip(depth, max_depth, tu, namespaces, already_visited))]
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
        CursorKind::ClassTemplate | CursorKind::ClassDecl | CursorKind::StructDecl => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            if !already_visited.contains(&c.usr()) {
                // Also make sure that we're dealing with a definition rather than a forward declaration
                // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
                // (for opaque types in the API)
                if c.is_definition() {
                    let cd =
                        extract_class_decl(c, depth + 1, tu, &namespaces, ast, already_visited)?;
                    ast.insert_class(cd);
                    already_visited.push(c.usr());
                }
            }
        }
        CursorKind::TypeAliasDecl | CursorKind::TypedefDecl => {
            // check if this type alias has a TemplateRef child, in which case it's a class template specialization
            if c.has_child_of_kind(CursorKind::TemplateRef) {
                let cts = extract_class_template_specialization(
                    c,
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
                ast.insert_type_alias(TypeAlias::ClassTemplateSpecialization(cts));
            } else {
                debug!(
                    "TypeAliasDecl {} not handled as it is not a CTS",
                    c.display_name()
                );
            }
        }
        CursorKind::Namespace => {
            let ns = extract_namespace(c, depth, tu);
            let name = ns.name().to_string();
            let usr = ns.usr;
            ast.insert_namespace(ns);
            already_visited.push(usr);

            // We bail out on std and will insert manual AST for the types we support because fuck dealing with that
            // horror show
            if name == "std" {
                let children = c.children_of_kind(CursorKind::Namespace, true);

                for child in children {
                    let ns = extract_namespace(c, depth, tu);
                    let usr = ns.usr;
                    ast.insert_namespace(ns);
                    already_visited.push(usr);
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
        }
        // CursorKind::NamespaceRef => {

        // }
        _ => (),
    }

    debug!("{indent}{}: {} {}", c.kind(), c.display_name(), c.usr());

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

pub fn dump(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
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

    match c.kind() {
        CursorKind::IntegerLiteral => {
            println!("{}: {}", c.kind(), tu.token(c.location()).spelling());
        }
        _ => println!(
            "{}: {} {} {}",
            c.kind(),
            c.display_name(),
            c.usr(),
            template_args
        ),
    }

    if let Ok(ty) = c.ty() {
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
        let indent = format!("{:width$}", "", width = depth.saturating_sub(1) * 4 + 2);

        let pod = if ty.is_pod() { "[POD]" } else { "" };

        println!(
            "{indent}𝜏 {}: {} {} {pod}",
            ty.spelling(),
            ty.kind(),
            template_args
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
                    "{indent}↪ {}: {} {} {} 🗸",
                    cr.kind(),
                    cr.display_name(),
                    cr.usr(),
                    template_args
                );
            } else {
                if !cr.usr().is_empty() {
                    already_visited.push(cr.usr());
                }
                print!("{indent}↪ ");
                dump(cr, depth + 1, max_depth, already_visited, tu);
            }
        }
    }

    let children = c.children();

    for child in children {
        if !child.usr().is_empty() {
            already_visited.push(child.usr());
        }

        let icon = match child.kind() {
            CursorKind::ClassDecl => "●",
            CursorKind::ClassTemplate => "○",
            CursorKind::FunctionDecl => "ƒ",
            CursorKind::FunctionTemplate => "ⓕ",
            CursorKind::CXXMethod => "ɱ",
            _ => "▸",
        };

        print!("{indent}{icon} ");

        dump(child, depth + 1, max_depth, already_visited, tu);
    }
}

#[instrument]
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
) {
    if let Ok(c) = c {
        if c.kind() != CursorKind::TranslationUnit {
            if ast.get_namespace(c.usr()).is_none() {
                let ns = extract_namespace(c, 0, tu);
                let name = ns.name().to_string();
                let usr = ns.usr;
                ast.insert_namespace(ns);
            }

            namespaces.push(c.usr());
            walk_namespaces(c.semantic_parent(), namespaces, tu, ast);
        }
    }
}

pub fn get_namespaces_for_decl(c: Cursor, tu: &TranslationUnit, ast: &mut AST) -> Vec<USR> {
    let mut namespaces = Vec::new();
    walk_namespaces(c.semantic_parent(), &mut namespaces, tu, ast);
    namespaces.reverse();
    namespaces
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
