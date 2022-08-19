use ustr::{UstrMap, Ustr};

use log::*;

use crate::cursor;
use crate::function::{Method, Function};
use crate::index::Index;
use crate::namespace::{self, extract_namespace, Namespace};
use crate::{
    class::extract_class_decl, class_template::extract_class_template, cursor::USR,
    cursor_kind::CursorKind, record::Record, type_alias::extract_class_template_specialization, Cursor,
    TranslationUnit,
};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct AST {
    records: Vec<Record>,
    record_map: UstrMap<usize>,
    methods: Vec<Method>,
    method_map: UstrMap<usize>,
    functions: Vec<Function>,
    function_map: UstrMap<usize>,
    namespaces: Vec<Namespace>,
    namespace_map: UstrMap<usize>,
}

impl AST {
    pub fn new() -> Self {
        AST {
            records: Vec::new(),
            record_map: Default::default(),
            methods: Vec::new(),
            method_map: Default::default(),
            functions: Vec::new(),
            function_map: Default::default(),
            namespaces: Vec::new(),
            namespace_map: Default::default(),
        }
    }

    pub fn pretty_print(&self, depth: usize) {
        for namespace in &self.namespaces {
            namespace.pretty_print(depth + 1, self);
            println!("");
        }

        for record in &self.records {
            record.pretty_print(depth + 1, self);
            println!("");
        }
    }

    pub fn class(&self, name: &str) -> Result<USR> {
        for rec in &self.records {
            if rec.name() == name {
                return Ok(rec.usr())
            }
        }

        Err(Error::RecordNotFound)
    }

    pub fn insert_record(&mut self, record: Record) {
        let idx = self.records.len();
        self.record_map.insert(record.usr().0, idx);
        self.records.push(record);
    }

    pub fn get_record(&self, usr: USR) -> Option<&Record> {
        self.record_map.get(&usr.0).map(|idx| &self.records[*idx])
    }

    pub fn insert_method(&mut self, method: Method) {
        let idx = self.methods.len();
        self.method_map.insert(method.usr().0, idx);
        self.methods.push(method);
    }

    pub fn get_method(&self, usr: USR) -> Option<&Method> {
        self.method_map.get(&usr.0).map(|idx| &self.methods[*idx])
    }

    pub fn insert_function(&mut self, function: Function) {
        let idx = self.functions.len();
        self.function_map.insert(function.usr().0, idx);
        self.functions.push(function);
    }

    pub fn get_function(&self, usr: USR) -> Option<&Function> {
        self.function_map.get(&usr.0).map(|idx| &self.functions[*idx])
    }

    pub fn insert_namespace(&mut self, namespace: Namespace) {
        let idx = self.namespaces.len();
        self.namespace_map.insert(namespace.usr().0, idx);
        self.namespaces.push(namespace);
    }

    pub fn get_namespace(&self, usr: USR) -> Option<&Namespace> {
        self.namespace_map.get(&usr.0).map(|idx| &self.namespaces[*idx])
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
        CursorKind::ClassTemplate => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            if !already_visited.contains(&c.usr()) {
                // Also make sure that we're dealing with a definition rather than a forward declaration
                // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
                // (for opaque types in the API)
                if c.is_definition() {
                    let ct = extract_class_template(c, depth + 1, tu, &namespaces);
                    ast.insert_record(Record::ClassTemplate(ct));
                    already_visited.push(c.usr());
                }
            }
        }
        CursorKind::ClassDecl | CursorKind::StructDecl => {
            // Make sure that we're dealing with a definition rather than a forward declaration
            // TODO: We're probably going to need to handle forward declarations for which we never find a definition too
            // (for opaque types in the API)
            if c.is_definition() {
                let cd = extract_class_decl(c, depth + 1, &namespaces);
                ast.insert_record(Record::ClassDecl(cd));
            }
        }
        CursorKind::TypeAliasDecl | CursorKind::TypedefDecl => {
            // check if this type alias has a TemplateRef child, in which case it's a class template specialization
            if c.has_child_of_kind(CursorKind::TemplateRef) {
                let cts = extract_class_template_specialization(c, depth + 1, already_visited, ast, tu, &namespaces)
                    .expect(&format!("Failed to extract TypeAliasDecl {c:?}"));
                ast.insert_record(Record::ClassTemplateSpecialization(cts));
            } else {
                debug!("TypeAliasDecl {} not handled as it is not a CTS", c.display_name());
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
