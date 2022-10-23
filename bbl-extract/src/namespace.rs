use crate::{
    ast::{get_namespaces_for_decl, AST},
    error::Error,
    index_map::UstrIndexMap,
};
use bbl_clang::{
    cursor::Cursor,
    cursor::{CurNamespace, USR},
    translation_unit::TranslationUnit,
};
use std::fmt::Debug;
use tracing::instrument;

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Namespace {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) rename: Option<String>,
    // parent namespaces
    namespaces: Vec<USR>,
}

impl Debug for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Namespace {} {} {:?}", self.usr, self.name, self.rename)
    }
}

impl Namespace {
    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn rename(&mut self, new_name: &str) {
        self.rename = Some(new_name.into());
    }

    pub fn namespaces(&self) -> &[USR] {
        self.namespaces.as_ref()
    }
}

#[instrument(skip(depth, tu), level = "trace")]
pub fn extract_namespace(
    c_namespace: Cursor,
    depth: usize,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    let usr = c_namespace.usr();
    // we don't use alread_visited for namespaces because we're considering classes etc as namespaces too for the
    // purpose of building fully qualified names
    // TODO(AL): do we want to separate this into scopes?
    if ast.get_namespace(usr).is_some() {
        return Ok(usr);
    }

    let namespaces = get_namespaces_for_decl(c_namespace, tu, ast, already_visited)?;

    ast.insert_namespace(Namespace {
        usr,
        name: c_namespace.display_name(),
        rename: None,
        namespaces,
    });

    Ok(usr)
}
