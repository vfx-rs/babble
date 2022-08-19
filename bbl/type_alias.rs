use log::*;
use std::fmt::Display;

use crate::class_template::ClassTemplateSpecialization;
use crate::cursor::USR;
use crate::namespace::extract_namespace;
use crate::qualtype::extract_type;
use crate::template_argument::TemplateType;
use crate::ty::Type;
use crate::{Cursor, ast::AST, TranslationUnit, record::Record, cursor_kind::CursorKind, class_template::extract_class_template};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

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
                    let ns = extract_namespace(c_namespace, depth+1, tu);
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
                            let ct_namespaes = if local_namespaces.is_empty() { &namespaces } else { &local_namespaces };
                            debug!("extracting class template {cref:?}");
                            let ct = extract_class_template(cref, depth + 1, tu, ct_namespaes);
                            ast.insert_record(Record::ClassTemplate(ct));
                            already_visited.push(cref.usr());
                        }
                        specialized_decl = Some(cref.usr());
                        debug!("{indent}    -> {}", cref.usr());
                    } else {
                        unimplemented!();
                    }
                } else {
                    error!("{indent}ERROR could not get referenced template from {child:?}");
                    return Err(Error::InvalidType);
                }
            }
        }

        Ok(ClassTemplateSpecialization {
            specialized_decl: specialized_decl.ok_or(Error::InvalidType)?,
            usr: c_type_alias_decl.usr(),
            name,
            args: template_args,
            namespaces: namespaces.clone(),
        })
    } else {
        error!("Could not get type from TypeAliasDecl {c_type_alias_decl:?}");
        Err(Error::InvalidType)
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
