pub(crate) use clang_sys::clang_createIndex;
pub(crate) use clang_sys::{self, clang_disposeIndex, clang_parseTranslationUnit, CXIndex};
use std::ffi::CString;

pub mod cursor;
pub mod template_argument;
pub use cursor::{ChildVisitResult, Cursor};
pub mod cursor_kind;
pub mod error;
use error::Error;
pub mod translation_unit;
pub use translation_unit::TranslationUnit;
pub mod string;
pub use string::CXStringEx;

use crate::cursor_kind::CursorKind;
pub mod token;

pub mod cxtype;
pub mod diagnostic;
pub mod file;

type Result<T, E = Error> = std::result::Result<T, E>;

use cxtype::{Type, TypeKind};

struct QualType {
    name: String,
    kind: TypeKind,
    is_const: bool,
    pointee: Option<Box<QualType>>,
    type_ref: String,
}

enum ParameterType {
    TemplateTypeParameter(String),
    QualType(QualType),
}

struct Parameter {
    name: String,
    ty: ParameterType,
}

struct Method {
    name: String,
    result: Option<ParameterType>,
    parameters: Vec<Parameter>,
    is_const: bool,
}

struct Record {
    fields: Vec<QualType>,
    methods: Vec<Method>,
}

fn target_method(c_method: Cursor, depth: usize) -> Result<Method> {
    let indent = format!("{:width$}", "", width = depth * 2);

    print!(
        "{indent}+ CXXMethod {} {}",
        c_method.display_name(),
        if c_method.cxx_method_is_const() {
            "const"
        } else {
            ""
        }
    );

    if let Ok(ty_result) = c_method.result_ty() {
        println!(" -> {ty_result:?}");
    } else {
        println!("");
    }

    let parameters: Vec<Parameter> = Vec::new();
    if let Ok(num_args) = c_method.num_arguments() {
        for i in 0..num_args {
            let c_arg = c_method.argument(i).unwrap();
            println!("{indent}    arg: {c_arg:?}");

            // should always be ParmDecl
            if c_arg.kind() != CursorKind::ParmDecl {
                unimplemented!();
            }

            if let Ok(ty_arg) = c_arg.ty() {
                println!("{indent}    {ty_arg:?}")
            }

            for child in c_arg.children() {
                println!("{indent}      {child:?}");
            }
        }
    }

    Err(Error::InvalidType)
}

fn target_class_template(class_template: Cursor, depth: usize) -> Record {
    let indent = format!("{:width$}", "", width = depth * 2);

    println!("{indent}target_class_template({})", class_template.usr());

    let mut methods = Vec::new();
    let mut fields = Vec::new();

    let members = class_template.children();
    for member in members {
        match member.kind() {
            CursorKind::CXXMethod => {
                if let Ok(method) = target_method(member, depth + 1) {
                    methods.push(method);
                }
            }
            _ => {
                println!("{indent}  {member:?}");
                for child in member.children() {
                    println!("{indent}    {child:?}");
                    match child.kind() {
                        CursorKind::TypeRef => {
                            if let Ok(c) = child.referenced() {
                                println!("{indent}    -> {c:?}");
                            }
                        }
                        CursorKind::ParmDecl => {
                            if let Ok(ty) = child.ty() {
                                println!("{indent}      type {ty:?}")
                            }

                            for c in child.children() {
                                println!("{indent}      {c:?}");
                            }
                        }
                        CursorKind::CompoundStmt => {
                            for stmt in child.children() {
                                println!("{indent}      {stmt:?}");
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    Record { fields, methods }
}

/// Handle a ClassTemplate in the binding namespace
fn binding_class_template(c_class_template: Cursor, depth: usize, namespaces: &Vec<String>) {
    let indent = format!("{:width$}", "", width = depth * 2);

    println!("{indent}binding_class_template({})", c_class_template.usr());

    let children = c_class_template.children();

    let bound_type = children
        .iter()
        .filter(|c| c.kind() == CursorKind::TypeAliasDecl && c.display_name() == "BoundType")
        .next();

    if let Some(bound_type_tad) = bound_type {
        let template_type_parameters: Vec<_> = children
            .iter()
            .filter(|c| c.kind() == CursorKind::TemplateTypeParameter)
            .map(|c| c.display_name())
            .collect();

        let attrs: Vec<_> = children
            .iter()
            .filter(|c| c.kind() == CursorKind::AnnotateAttr)
            .map(|c| c.display_name())
            .collect();

        let tad_children = bound_type_tad.children();

        // TODO: get namespace here
        // let namespaces = ...

        let template_ref_pointee = tad_children
            .iter()
            .filter(|c| c.kind() == CursorKind::TemplateRef)
            .map(|c| c.referenced())
            .next();

        if let Some(Ok(template_ref_pointee)) = template_ref_pointee {
            target_class_template(template_ref_pointee, depth + 1);
        } else {
            println!(
                "{indent}ERROR could not get TemplateRef from {}",
                bound_type_tad.pretty_printed()
            );
        }
    } else {
        println!(
            "{indent}ERROR ClassTemplate {} has no BoundType decl",
            c_class_template.display_name()
        );
    }
}

/// Handle a TypeAliasDecl in the binding namespace
///
/// We use a TypeAliasDecl to monomorphize template classes in the target.
fn binding_type_alias_decl(c_type_alias_decl: Cursor, depth: usize, namespaces: &Vec<String>) {
    let indent = format!("{:width$}", "", width = depth * 2);

    println!(
        "{}TypeAliasDecl {} {} {}",
        indent,
        c_type_alias_decl.usr(),
        c_type_alias_decl.pretty_printed(),
        c_type_alias_decl.location().spelling_location()
    );
    // println!("{indent}Namespaces: {namespaces:?}");
    // println!("{indent}* Children:");

    if let Ok(ty) = c_type_alias_decl.ty() {
        let num_args = ty.num_template_arguments();
        // println!("{indent}Num template args: {}", num_args);

        // Get any template args on this type alias
        let mut template_args = Vec::new();
        if num_args > 0 {
            template_args.reserve(num_args as usize);
            for i in 0..num_args {
                // TODO: (AL) There doesn't appear to be a way to get non-type template arguments from a Type...
                if let Ok(tty) = ty.template_argument_as_type(i as u32) {
                    // println!("{indent}    template arg {i} is {}", tty.spelling());
                    template_args.push(tty);
                }
            }
        }
        let template_args = template_args;
        println!("{indent}    template args: {template_args:?}");

        // First child will be the namespace of the target, next will be the template ref which will point to the class template
        for child in c_type_alias_decl.children() {
            // println!("{indent}    {} {} {}", child.usr(), child.display_name(), child.kind());
            if child.kind() == CursorKind::Namespace {
                // push namespace here?
            } else if child.kind() == CursorKind::TemplateRef {
                if let Ok(cref) = child.referenced() {
                    if cref.kind() == CursorKind::ClassTemplate {
                        // do_class_template(cref, template_args);
                        println!("{indent}    -> {}", cref.usr());
                        return;
                    } else {
                        unimplemented!();
                    }
                } else {
                    println!("{indent}ERROR could not get referenced template from TemplateRef");
                }
            }
        }
    } else {
        println!("{}ERROR could not get type from TypeAliasDecl", indent)
    }
}

fn print(c: Cursor, depth: usize, already_visited: Vec<String>, mut namespaces: Vec<String>) {
    if depth > 13 {
        // println!("");
        return;
    }
    let indent = format!("{:width$}", "", width = depth * 2);

    if c.kind() == CursorKind::Namespace {
        namespaces.push(c.display_name());
    }

    if c.kind() == CursorKind::ClassTemplate {
        binding_class_template(c, depth, &namespaces);
        return;
    }

    if c.kind() == CursorKind::TypeAliasDecl {
        binding_type_alias_decl(c, depth, &namespaces);
        return;
    }

    // println!("{}: {} {}", c.kind(), c.display_name(), c.usr());

    if let Ok(cr) = c.referenced() {
        if cr != c && !already_visited.contains(&cr.usr()) {
            // print!("{}-> ", indent);
            let mut v = already_visited.clone();
            if !cr.usr().is_empty() {
                v.push(cr.usr());
            }
            print(cr, depth + 1, v, Vec::new());
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        let mut v = already_visited.clone();
        if !child.usr().is_empty() {
            v.push(child.usr());
        }
        // print!("{}C ", indent);
        print(child, depth + 1, v, namespaces.clone());
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let index = Index::new();
    let tu = index.parse_translation_unit(
        &std::env::args().into_iter().collect::<Vec<_>>()[1],
        &[
            "-std=c++14",
            "-x",
            "c++",
            "-isystem/home/anders/packages/llvm/14.0.0/include/c++/v1/",
            "-isystem/home/anders/packages/llvm/14.0.0/include/x86_64-unknown-linux-gnu/c++/v1/",
            "-isystem/usr/include/c++/9",
            "-isystem/usr/lib/gcc/x86_64-linux-gnu/9/include/", 
            "-isystem/usr/include/x86_64-linux-gnu",
            "-isystem/usr/include/linux",
            "-isystem/usr/local/include",
            "-isystem/usr/include",
            "-I/home/anders/packages/openexr/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include",
            "-I/home/anders/packages/imath/3.1.5/platform-linux/arch-x86_64/cxx11abi-0/cfg-release/include/Imath",
        ],
    );
    println!("{} diagnostics", tu.num_diagnostics());
    for d in tu.diagnostics() {
        println!("{}", d);
    }
    let cur = tu.get_cursor()?;

    let cur = cur
        .children_of_kind_with_name(CursorKind::Namespace, "cppmm_bind", true)
        .get(0)
        .unwrap()
        .clone();

    print(cur.clone(), 0, Vec::new(), Vec::new());

    /*
    let ns_imath = cur
        .children_of_kind(CursorKind::Namespace, false)
        .get(0)
        .unwrap()
        .clone();

    let type_alias_decls = ns_imath.children_of_kind(CursorKind::TypeAliasDecl, false);
    */

    Ok(())
}

pub struct Index {
    inner: CXIndex,
}

impl Index {
    pub fn new() -> Index {
        let inner = unsafe { clang_createIndex(0, 0) };
        Index { inner }
    }

    pub fn parse_translation_unit(&self, filename: &str, args: &[&str]) -> TranslationUnit {
        let cfilename = CString::new(filename).unwrap();

        let cargs: Vec<_> = args.iter().map(|a| CString::new(*a).unwrap()).collect();
        let cstrargs: Vec<_> = cargs.iter().map(|a| a.as_ptr()).collect();

        let tu = unsafe {
            clang_parseTranslationUnit(
                self.inner,
                cfilename.as_ptr(),
                cstrargs.as_ptr(),
                cstrargs.len() as i32,
                std::ptr::null_mut(),
                0,
                0,
            )
        };

        if tu.is_null() {
            panic!("translation unit {filename} failed to parse")
        }

        TranslationUnit { inner: tu }
    }
}

impl Drop for Index {
    fn drop(&mut self) {
        unsafe {
            clang_disposeIndex(self.inner);
        }
    }
}

#[cfg(test)]
pub(crate) fn get_test_filename(base: &str) -> String {
    std::path::PathBuf::from(std::env!("CARGO_MANIFEST_DIR"))
        .join("testdata")
        .join(base)
        .as_os_str()
        .to_string_lossy()
        .to_string()
}
