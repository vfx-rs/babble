use log::*;
use std::fmt::Display;

use crate::{
    ast::AST,
    class::ClassDecl,
    class_template::{ClassTemplate, ClassTemplateSpecialization},
    cursor::USR,
    template_argument::TemplateType,
};

pub enum Record {
    ClassDecl(ClassDecl),
    ClassTemplate(ClassTemplate),
    ClassTemplateSpecialization(ClassTemplateSpecialization),
}

impl Record {
    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        match self {
            Record::ClassDecl(decl) => {
                decl.pretty_print(depth, ast, &[], None);
            }
            Record::ClassTemplate(ct) => {
                ct.pretty_print(depth, ast, None);
            }
            Record::ClassTemplateSpecialization(cts) => cts.pretty_print(depth, ast),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Record::ClassDecl(cd) => &cd.name,
            Record::ClassTemplate(ct) => &ct.class_decl.name,
            Record::ClassTemplateSpecialization(cts) => &cts.name,
        }
    }
}
