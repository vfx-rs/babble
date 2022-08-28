use bbl_clang::cursor_kind::CursorKind;
use bbl_clang::translation_unit::TranslationUnit;
use log::*;
use std::fmt::Display;

use crate::ast::AST;
use crate::class::extract_class_decl;
use crate::namespace::extract_namespace;
use crate::qualtype::extract_type;
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use bbl_clang::cursor::{Cursor, USR};
use bbl_clang::ty::Type;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub enum TypeAlias {
    TypeAliasType { name: String, usr: USR },
    ClassTemplateSpecialization(ClassTemplateSpecialization),
    FunctionTemplateSpecialization(FunctionTemplateSpecialization),
}

impl TypeAlias {
    pub fn usr(&self) -> USR {
        match self {
            TypeAlias::TypeAliasType { usr, .. } => *usr,
            TypeAlias::ClassTemplateSpecialization(cts) => cts.usr,
            TypeAlias::FunctionTemplateSpecialization(fts) => fts.usr,
        }
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        match self {
            TypeAlias::TypeAliasType { name, usr } => todo!(),
            TypeAlias::ClassTemplateSpecialization(cts) => cts.pretty_print(depth, ast),
            TypeAlias::FunctionTemplateSpecialization(fts) => {
                fts.pretty_print(depth, ast, outer_template_parameters)
            }
        }
    }
}

pub fn extract_class_template_specialization(
    c_type_alias_decl: Cursor,
    depth: usize,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    namespaces: &Vec<USR>,
) -> Result<ClassTemplateSpecialization> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let name = c_type_alias_decl.display_name();

    debug!(
        "{}TypeAliasDecl {} {} {}",
        indent,
        c_type_alias_decl.usr(),
        c_type_alias_decl.pretty_printed(),
        c_type_alias_decl.location().spelling_location()
    );

    if let Ok(ty) = c_type_alias_decl.ty() {
        let template_args = extract_template_args(&c_type_alias_decl, &ty, tu);

        debug!("Template args:");
        for template_arg in &template_args {
            if let Some(a) = template_arg {
                debug!("    {a}");
            }
        }

        // TODO: merge this loop with the one above
        // First child will be the namespace of the target, next will be the template ref which will point to the class template
        let mut specialized_decl = None;
        let mut local_namespaces = Vec::new();
        for child in c_type_alias_decl.children() {
            // println!("{indent}    {} {} {}", child.usr(), child.display_name(), child.kind());
            if child.kind() == CursorKind::NamespaceRef {
                let c_namespace = child.referenced().unwrap();
                if !already_visited.contains(&c_namespace.usr()) {
                    // extract the namespace here
                    let ns = extract_namespace(c_namespace, depth + 1, tu);
                    let usr = ns.usr.clone();
                    ast.insert_namespace(ns);
                    already_visited.push(usr);
                }
                local_namespaces.push(c_namespace.usr());
            } else if child.kind() == CursorKind::TemplateRef {
                if let Ok(cref) = child.referenced() {
                    if cref.kind() == CursorKind::ClassTemplate {
                        if !already_visited.contains(&cref.usr()) {
                            // If we haven't already extracted the class which this alias refers to, do it now
                            // if we've got namespaces defined on this ref then we /probably/ want to use them,
                            // otherwise use the ones passed in
                            let ct_namespaes = if local_namespaces.is_empty() {
                                &namespaces
                            } else {
                                &local_namespaces
                            };
                            debug!("extracting class template {cref:?}");
                            let cd = extract_class_decl(cref, depth + 1, tu, ct_namespaes);
                            ast.insert_class(cd);
                            already_visited.push(cref.usr());
                        }
                        specialized_decl = Some(cref.usr());
                        debug!("{indent}    -> {}", cref.usr());
                    } else {
                        unimplemented!();
                    }
                } else {
                    error!("{indent}ERROR could not get referenced template from {child:?}");
                    return Err(Error::FailedToGetTemplateRefFrom(
                        c_type_alias_decl.display_name(),
                    ));
                }
            }
        }

        Ok(ClassTemplateSpecialization {
            specialized_decl: specialized_decl.ok_or(Error::FailedToGetTemplateRefFrom(
                c_type_alias_decl.display_name(),
            ))?,
            usr: c_type_alias_decl.usr(),
            name,
            template_arguments: template_args,
            namespaces: namespaces.clone(),
        })
    } else {
        error!("Could not get type from TypeAliasDecl {c_type_alias_decl:?}");
        Err(Error::FailedToGetTypeFrom(c_type_alias_decl.display_name()))
    }
}

fn extract_template_args(
    c_type_alias_decl: &Cursor,
    ty: &Type,
    tu: &TranslationUnit,
) -> Vec<Option<TemplateType>> {
    let num_args = ty.num_template_arguments();

    // Get any type template args on this type alias.
    let mut template_args = Vec::new();
    if num_args > 0 {
        template_args.reserve(num_args as usize);
        for i in 0..num_args {
            if let Ok(tty) = ty.template_argument_as_type(i as u32) {
                let qt = extract_type(tty.clone(), &[])
                    .expect(&format!("Could not extract QualType from {}", tty));
                template_args.push(Some(TemplateType::Type(qt)));
            } else {
                // If it's not a type, we have to handle it separately, below.
                template_args.push(None);
            }
        }
    }

    let mut non_type_indices = Vec::new();
    for (i, arg) in template_args.iter().enumerate() {
        if arg.is_none() {
            non_type_indices.push(i)
        };
    }

    if !non_type_indices.is_empty() {
        // there are non-type template arguments. we'll have to walk the children of the TypeAliasDecl and try and
        // guess which ones they are...
        // The ordering should be:
        // 1. Namespaces
        // 2. TemplateRef -> ClassTemplate (presumably there could be other types referenced here too)
        // 3. Template arguments of *some kind*
        // 4. Everything else...
        // This will be buggy...
        let mut current = 0;
        for child in c_type_alias_decl.children() {
            match child.kind() {
                CursorKind::NamespaceRef | CursorKind::TemplateRef => (),
                CursorKind::IntegerLiteral => {
                    let value = tu.token(child.location()).spelling();
                    let index = non_type_indices[current];
                    template_args[index] = Some(TemplateType::Integer(value));
                    current += 1;
                }
                _ => unimplemented!(
                    "Unimplemented CursorKind {} when getting non-type template args",
                    child.kind()
                ),
            }

            if current == non_type_indices.len() {
                break;
            }
        }

        if current != non_type_indices.len() {
            panic!(
                "Could not reconcile all non-type template arguments for {:?}",
                c_type_alias_decl
            );
        }
    }

    template_args
}

pub struct ClassTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones.
    ///
    /// Revisit and maybe we want to make that a hard error
    pub(crate) template_arguments: Vec<Option<TemplateType>>,
    /// The typedef itself is namespaced
    pub(crate) namespaces: Vec<USR>,
}

impl ClassTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[Option<TemplateType>] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ ClassTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        // this will be complicated...
        let class = ast.get_class(self.specialized_decl).unwrap();
        class.pretty_print(depth, ast, Some(self.template_arguments()));
    }

    pub fn format(&self, ast: &AST) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let class = ast.get_class(self.specialized_decl).unwrap();
        class.format(ast, Some(self.template_arguments()))
    }
}

pub struct FunctionTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones.
    ///
    /// Revisit and maybe we want to make that a hard error
    pub(crate) template_arguments: Vec<Option<TemplateType>>,
    /// The typedef itself is namespaced
    pub(crate) namespaces: Vec<USR>,
}

impl FunctionTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[Option<TemplateType>] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ FunctionTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.pretty_print(
            depth,
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        );
    }

    pub fn format(&self, ast: &AST, outer_template_parameters: &[TemplateParameterDecl]) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.format(
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        )
    }
}
