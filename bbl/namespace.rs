use crate::{ast::AST, cursor::USR, Cursor, TranslationUnit};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Namespace {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) rename: Option<String>,
}

impl Namespace {
    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);
        println!("+ Namespace {} {}", self.usr, self.name);
    }

    pub fn usr(&self) -> USR {
        self.usr
    }
}

pub fn extract_namespace(c_namespace: Cursor, depth: usize, tu: &TranslationUnit) -> Namespace {
    let indent = format!("{:width$}", "", width = depth * 2);
    Namespace {
        usr: c_namespace.usr(),
        name: c_namespace.display_name(),
        rename: None,
    }
}
