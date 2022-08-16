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
    pub fn pretty_print(&self, depth: usize, binding: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        match self {
            Record::ClassDecl(decl) => {
                print!("ClassDecl ");
                decl.pretty_print(depth, binding);
            }
            Record::ClassTemplate(ct) => {
                print!("ClassTemplate ");
                ct.pretty_print(depth, binding);
            }
            Record::ClassTemplateSpecialization(ClassTemplateSpecialization {
                specialized_decl,
                usr,
                name,
                args,
            }) => {
                let args = args.iter().map(|a| format!("{:?}", a)).collect::<Vec<_>>();
                println!(
                    "ClassTemplateSpecialization {name} of ({specialized_decl}) with <{}>",
                    args.join(", ")
                );
            }
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
