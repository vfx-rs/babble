use indexmap::IndexMap;

use log::*;

use crate::index::Index;
use crate::{
    class::extract_class_decl, class_template::extract_class_template, cursor::USR,
    cursor_kind::CursorKind, record::Record, type_alias::extract_type_alias_decl, Cursor,
    TranslationUnit,
};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct AST {
    pub(crate) records: IndexMap<USR, Record>,
}

impl AST {
    pub fn new() -> Self {
        AST {
            records: IndexMap::<USR, Record>::new(),
        }
    }

    pub fn pretty_print(&self, depth: usize) {
        for (usr, record) in &self.records {
            record.pretty_print(depth + 1, self);
            println!("");
        }
    }
}

pub fn extract_ast(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: &mut Vec<String>,
    ast: &mut AST,
    tu: &TranslationUnit,
) {
    if depth > max_depth {
        // println!("");
        return;
    }
    let indent = format!("{:width$}", "", width = depth * 2);

    match c.kind() {
        CursorKind::ClassTemplate => {
            // We might extract a class template when visiting a type alias so check that we haven't already done so
            if !already_visited.contains(&c.usr().0) {
                let ct = extract_class_template(c, depth + 1, tu);
                ast.records
                    .insert(ct.class_decl.usr.clone(), Record::ClassTemplate(ct));
                already_visited.push(c.usr().0);
            }
        }
        CursorKind::ClassDecl => {
            let cd = extract_class_decl(c, depth + 1);
            ast.records.insert(cd.usr.clone(), Record::ClassDecl(cd));
        }
        CursorKind::TypeAliasDecl | CursorKind::TypedefDecl => {
            let cts = extract_type_alias_decl(c, depth + 1, already_visited, ast, tu)
                .expect(&format!("Failed to extract TypeAliasDecl {c:?}"));
            ast.records.insert(cts.usr.clone(), Record::ClassTemplateSpecialization(cts));
        }

        _ => (),
    }

    debug!("{indent}{}: {} {}", c.kind(), c.display_name(), c.usr());

    if let Ok(cr) = c.referenced() {
        if cr != c && !already_visited.contains(&cr.usr().0) {
            // print!("{}-> ", indent);
            if !cr.usr().0.is_empty() {
                already_visited.push(cr.usr().0);
            }
            extract_ast(cr, depth + 1, max_depth, already_visited, ast, tu);
        } else {
            debug!("{indent} already visited {cr:?}, skipping...");
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        extract_ast(child, depth + 1, max_depth, already_visited, ast, tu);
    }
}
