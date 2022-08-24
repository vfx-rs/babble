use ustr::{Ustr, UstrMap};

use log::*;

use crate::class::ClassDecl;
use crate::function::{Function, Method};
use crate::index::Index;
use crate::namespace::{self, extract_namespace, Namespace};
use crate::type_alias::TypeAlias;
use crate::{
    class::extract_class_decl, cursor::USR, cursor_kind::CursorKind,
    type_alias::extract_class_template_specialization, Cursor, TranslationUnit,
};
use crate::{cursor, type_alias};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct UstrIndexMap<T> {
    storage: Vec<T>,
    map: UstrMap<usize>,
}

impl<T> UstrIndexMap<T> {
    pub fn new() -> UstrIndexMap<T> {
        UstrIndexMap {
            storage: Vec::new(),
            map: Default::default(),
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.storage.iter()
    }

    pub fn get(&self, key: &Ustr) -> Option<&T> {
        self.map.get(key).map(|id| &self.storage[*id])
    }

    pub fn get_mut(&mut self, key: &Ustr) -> Option<&mut T> {
        self.map.get(key).map(|id| &mut self.storage[*id])
    }

    pub fn get_id(&self, key: &Ustr) -> Option<&usize> {
        self.map.get(key)
    }

    pub fn index(&self, id: usize) -> &T {
        &self.storage[id]
    }

    pub fn index_mut(&mut self, id: usize) -> &mut T {
        &mut self.storage[id]
    }

    pub fn len(&self) -> usize {
        self.storage.len()
    }

    pub fn insert(&mut self, key: Ustr, value: T) -> usize {
        let id = self.storage.len();
        self.storage.push(value);
        self.map.insert(key, id);
        id
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClassId(pub(crate) usize);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct MethodId(pub(crate) usize);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct NamespaceId(pub(crate) usize);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FunctionId(pub(crate) usize);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeAliasId(pub(crate) usize);

pub struct AST {
    pub(crate) classes: UstrIndexMap<ClassDecl>,
    pub(crate) functions: UstrIndexMap<Function>,
    pub(crate) namespaces: UstrIndexMap<Namespace>,
    pub(crate) type_aliases: UstrIndexMap<TypeAlias>,
}

impl AST {
    pub fn new() -> Self {
        AST {
            classes: UstrIndexMap::new(),
            functions: UstrIndexMap::new(),
            namespaces: UstrIndexMap::new(),
            type_aliases: UstrIndexMap::new(),
        }
    }

    pub fn pretty_print(&self, depth: usize) {
        for namespace in self.namespaces.iter() {
            namespace.pretty_print(depth + 1, self);
            println!("");
        }

        println!("");
        for class in self.classes.iter() {
            class.pretty_print(depth + 1, self, None);
            println!("");
        }

        println!("");
        // for function in self.functions.iter() {
        //     function.pretty_print(depth + 1, self, &[], None);
        //     println!("");
        // }

        for type_alias in self.type_aliases.iter() {
            type_alias.pretty_print(depth + 1, self);
            println!("");
        }
    }

    pub fn find_namespace(&self, name: &str) -> Result<NamespaceId> {
        for namespace in self.namespaces.iter() {
            if namespace.name == name {
                return self
                    .namespaces
                    .get_id(&namespace.usr().0)
                    .map(|i| NamespaceId(*i))
                    .ok_or(Error::NamespaceNotFound);
            }
        }

        Err(Error::RecordNotFound)
    }

    pub fn find_class(&self, name: &str) -> Result<ClassId> {
        for class in self.classes.iter() {
            if class.name() == name {
                return self
                    .classes
                    .get_id(&class.usr().0)
                    .map(|i| ClassId(*i))
                    .ok_or(Error::RecordNotFound);
            }
        }

        Err(Error::RecordNotFound)
    }

    pub fn rename_namespace(&mut self, namespace_id: NamespaceId, new_name: &str) {
        self.namespaces.index_mut(namespace_id.0).rename(new_name);
    }

    pub fn find_method(&self, class_id: ClassId, signature: &str) -> Result<MethodId> {
        let class = self.classes.index(class_id.0);
        class.find_method(self, signature).map(|t| t.0)
    }

    pub fn rename_method(&mut self, class_id: ClassId, method_id: MethodId, new_name: &str) {
        self.classes.index_mut(class_id.0).rename_method(method_id, new_name);
    }

    pub fn ignore_method(&mut self, class_id: ClassId, method_id: MethodId) {
        self.classes.index_mut(class_id.0).ignore_method(method_id);
    }

    pub fn insert_class(&mut self, class: ClassDecl) {
        self.classes.insert(class.usr().0, class);
    }

    pub fn get_class(&self, usr: USR) -> Option<&ClassDecl> {
        self.classes.get(&usr.0)
    }

    pub fn insert_function(&mut self, function: Function) {
        self.functions.insert(function.usr().0, function);
    }

    pub fn insert_type_alias(&mut self, type_alias: TypeAlias) {
        self.type_aliases.insert(type_alias.usr().0, type_alias);
    }

    pub fn get_function(&self, usr: USR) -> Option<&Function> {
        self.functions.get(&usr.0)
    }

    pub fn insert_namespace(&mut self, namespace: Namespace) {
        self.namespaces.insert(namespace.usr().0, namespace);
    }

    pub fn get_namespace(&self, usr: USR) -> Option<&Namespace> {
        self.namespaces.get(&usr.0)
    }
}

/// Main recursive function to walk the AST and extract the pieces we're interested in
pub fn extract_ast(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    namespaces: Vec<USR>,
) {
    let mut namespaces = namespaces;

    if depth > max_depth {
        // println!("");
        return;
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
                    let cd = extract_class_decl(c, depth + 1, tu, &namespaces);
                    ast.insert_class(cd);
                    already_visited.push(c.usr());
                }
            }
        }
        /*
        CursorKind::ClassDecl | CursorKind::StructDecl => {
            // Make sure that we're dealing with a definition rather than a forward declaration
            // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
            // (for opaque types in the API)
            if c.is_definition() {
                let cd = extract_class_decl(c, depth + 1, tu, &namespaces);
                ast.insert_record(Record::ClassDecl(cd));
            }
        }
        */
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
                .expect(&format!("Failed to extract TypeAliasDecl {c:?}"));
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
            let usr = ns.usr;
            ast.insert_namespace(ns);
            already_visited.push(usr);
        }
        // CursorKind::NamespaceRef => {

        // }
        _ => (),
    }

    debug!("{indent}{}: {} {}", c.kind(), c.display_name(), c.usr());

    if let Ok(cr) = c.referenced() {
        if cr != c && !already_visited.contains(&cr.usr()) {
            // print!("{}-> ", indent);
            if !cr.usr().0.is_empty() {
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
            );
        } else {
            debug!("{indent} already visited {cr:?}, skipping...");
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        extract_ast(
            child,
            depth + 1,
            max_depth,
            already_visited,
            ast,
            tu,
            namespaces.clone(),
        );
    }
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
        println!(
            "{indent}𝜏 {}: {} {}",
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
    if children.len() > 0 {}

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

pub fn extract_ast_from_namespace(name: &str, c_tu: Cursor, tu: &TranslationUnit) -> AST {
    let ns = if name.is_empty() {
        c_tu.children()
    } else {
        c_tu.children_of_kind_with_name(CursorKind::Namespace, name, true)
    };

    let mut ast = AST::new();
    let namespaces = Vec::new();
    let mut already_visited = Vec::new();
    for cur in ns {
        extract_ast(
            cur.clone(),
            0,
            100,
            &mut already_visited,
            &mut ast,
            &tu,
            namespaces.clone(),
        );
    }

    ast
}
