use log::*;
use std::fmt::Display;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use crate::{
    ast::AST,
    class::ClassDecl,
    class_template::{ClassTemplate, ClassTemplateSpecialization},
    cursor::USR,
    template_argument::{TemplateType, TemplateParameterDecl}, function::Method,
};

pub enum Record {
    ClassDecl(ClassDecl),
    ClassTemplate(ClassTemplate),
    ClassTemplateSpecialization(ClassTemplateSpecialization),
}

impl Record {
    pub fn set_ignore(&mut self, ignore: bool) {
        match self {
            Record::ClassDecl(cd) => cd.set_ignore(ignore),
            Record::ClassTemplate(ct) => ct.set_ignore(ignore),
            _ => warn!("Tried to set ignore on a specialization"),
        }
    }

    pub fn rename(&mut self, name: &str) {
        match self {
            Record::ClassDecl(cd) => cd.set_rename(name),
            Record::ClassTemplate(ct) => ct.set_rename(name),
            _ => warn!("Tried to set rename a specialization"),
        }
    }

    pub fn method(&self, signature: &str, ast: &AST) -> Result<USR> {
        match self {
            Record::ClassDecl(cd) => cd.method(signature, ast, &[], None),
            _ => todo!()
        }
    }

    pub fn pretty_print(&self, depth: usize, ast: &AST) {
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


    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        match self {
            Record::ClassDecl(decl) => {
                decl.format(ast)
            }
            Record::ClassTemplate(ct) => {
                ct.format(ast, class_template_args)
            }
            // TODO: do we need to pass teh template args here?
            Record::ClassTemplateSpecialization(cts) => cts.format(ast),
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
