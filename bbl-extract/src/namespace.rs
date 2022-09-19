use crate::{ast::AST, index_map::UstrIndexMap};
use bbl_clang::{
    cursor::Cursor,
    cursor::{CurNamespace, USR},
    translation_unit::TranslationUnit,
};
use std::fmt::Debug;
use tracing::instrument;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Namespace {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) rename: Option<String>,
}

impl Debug for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Namespace {} {} {:?}", self.usr, self.name, self.rename)
    }
}

impl Namespace {
    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);
        println!("+ Namespace {} {}", self.usr, self.name);
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn rename(&mut self, new_name: &str) {
        self.rename = Some(new_name.into());
    }
}

#[instrument(skip(depth, tu), level = "trace")]
pub fn extract_namespace(
    c_namespace: Cursor,
    depth: usize,
    tu: &TranslationUnit,
    ast: &mut AST,
) -> USR {
    let usr = c_namespace.usr();
    // we don't use alread_visited for namespaces because we're considering classes etc as namespaces too for the
    // purpose of building fully qualified names
    // TODO(AL): do we want to separate this into scopes?
    if ast.get_namespace(usr).is_some() {
        return usr;
    }

    ast.insert_namespace(Namespace {
        usr,
        name: c_namespace.display_name(),
        rename: None,
    });

    usr
}
