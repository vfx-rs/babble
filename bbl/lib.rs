use cursor::USR;
use hashbrown::HashMap;

use std::fmt::Display;
use std::path::Path;

use log::*;

pub mod cursor;
pub mod index;
pub mod template_argument;
pub mod virtual_file;
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
pub use crate::diagnostic::Severity;
pub mod file;

type Result<T, E = Error> = std::result::Result<T, E>;

/// Convenience function to parse a file with the given compiler arguments and optionally log diagnostics
pub fn parse_file<P: AsRef<Path>, S: AsRef<str>>(
    filename: P,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let index = index::Index::new();
    let tu = index.parse_translation_unit(filename, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    Ok(tu)
}

/// Convenience function to parse a C++ string with the given compiler arguments and optionally log diagnostics
///
/// This creates a temporary file in `std::env::temp_dir()` with the file contents passed
pub fn parse_string<S1: AsRef<str>, S: AsRef<str>>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
) -> Result<TranslationUnit> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    let index = index::Index::new();
    let tu = index.parse_translation_unit(path, cli_args)?;

    if log_diagnostics {
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => error!("{}", d),
            }
        }
    }

    Ok(tu)
}

use cxtype::{Type, TypeKind};

pub enum TypeRef {
    Builtin(TypeKind),
    Ref(USR),
    Pointer(Box<QualType>),
    LValueReference(Box<QualType>),
    RValueReference(Box<QualType>),
    TemplateTypeParameter(String),
    TemplateNonTypeParameter(String),
    Unknown(TypeKind),
}

pub struct QualType {
    name: String,
    is_const: bool,
    type_ref: TypeRef,
}

impl QualType {
    pub fn unknown(tk: TypeKind) -> Self {
        QualType {
            name: "UNKNOWN".to_string(),
            is_const: false,
            type_ref: TypeRef::Unknown(tk),
        }
    }

    pub fn format(&self, ast: &Ast) -> String {
        let result = String::new();

        let result = if self.is_const {
            format!("{result}const ")
        } else {
            result
        };

        let result = match &self.type_ref {
            TypeRef::Builtin(tk) => {
                format!("{result}{}", tk.spelling())
            }
            TypeRef::Pointer(pointee) => format!("{result}{}*", pointee.format(ast)),
            TypeRef::LValueReference(pointee) => format!("{result}{}&", pointee.format(ast)),
            TypeRef::RValueReference(pointee) => {
                format!("{result}{}&&", pointee.format(ast))
            }
            TypeRef::Ref(usr) => {
                let name = ast.records.get(usr).map(|r| format!("{r}")).unwrap_or(usr.0.clone());
                format!("{result}{}", name)
            }
            TypeRef::TemplateTypeParameter(t) => {
                format!("{result}{}", t)
            }
            TypeRef::TemplateNonTypeParameter(t) => {
                format!("{result}{}", t)
            }
            TypeRef::Unknown(tk) => {
                format!("{result}UNKNOWN({})", tk.spelling())
            }
        };

        result
    }
}

impl Display for QualType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }

        match &self.type_ref {
            TypeRef::Builtin(tk) => {
                write!(f, "{}", tk.spelling())
            }
            TypeRef::Pointer(pointee) => write!(f, "{}*", *pointee),
            TypeRef::LValueReference(pointee) => write!(f, "{}&", *pointee),
            TypeRef::RValueReference(pointee) => {
                write!(f, "{}&&", *pointee)
            }
            TypeRef::Ref(usr) => {
                write!(f, "{}", usr)
            }
            TypeRef::TemplateTypeParameter(t) => {
                write!(f, "{}", t)
            }
            TypeRef::TemplateNonTypeParameter(t) => {
                write!(f, "{}", t)
            }
            TypeRef::Unknown(tk) => {
                write!(f, "UNKNOWN({})", tk.spelling())
            }
        }
    }
}

pub struct Argument {
    name: String,
    qual_type: QualType,
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Argument {
    fn format(&self, binding: &Ast) -> String {
        format!("{}: {}", self.name, self.qual_type.format(binding))
    }
}

pub struct Function {
    usr: USR,
    name: String,
    result: QualType,
    arguments: Vec<Argument>,
}

impl Function {
    pub fn new(usr: USR, name: String, result: QualType, arguments: Vec<Argument>) -> Self {
        Function {
            usr,
            name,
            result,
            arguments,
        }
    }
}

struct Method {
    function: Function,
    is_const: bool,
    is_static: bool,
    is_virtual: bool,
    is_pure_virtual: bool,
}

impl Method {
    pub fn new(
        usr: USR,
        name: String,
        result: QualType,
        arguments: Vec<Argument>,
        is_const: bool,
        is_static: bool,
        is_virtual: bool,
        is_pure_virtual: bool,
    ) -> Self {
        Method {
            function: Function::new(usr, name, result, arguments),
            is_const,
            is_static,
            is_virtual,
            is_pure_virtual,
        }
    }

    pub fn pretty_print(&self, depth: usize, binding: &Ast) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let mut s = String::new();
        if self.is_static {
            s += " static"
        };
        if self.is_virtual {
            s += " virtual"
        };

        s += " ";
        s += &self.function.name;

        let args = self
            .function
            .arguments
            .iter()
            .map(|p| p.format(binding))
            .collect::<Vec<String>>()
            .join(", ");
        s += &format!("({})", args);

        if self.is_const {
            s += " const"
        };

        s += &format!(" -> {}", self.function.result.format(binding));

        if self.is_pure_virtual {
            s += " = 0"
        };

        s += ";";

        println!("{indent}{s}");
    }
}

struct Record {
    usr: USR,
    name: String,
    fields: Vec<QualType>,
    methods: Vec<Method>,
    template_parameters: Vec<String>,
}

impl Record {
    pub fn pretty_print(&self, depth: usize, binding: &Ast) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let template_decl = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!(
                "template <typename {}>\n{indent}",
                self.template_parameters.join(", typename ")
            )
        };

        let template = if self.template_parameters.is_empty() {
            String::new()
        } else {
            format!("<{}>", self.template_parameters.join(", "))
        };
        println!("{indent}{template_decl}class {}{template} {{", self.name);

        for method in &self.methods {
            method.pretty_print(depth + 1, binding);
        }

        println!("{indent}}}");
    }
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.template_parameters.is_empty() {
            write!(f, "<{}>", self.template_parameters.join(", "))?;
        }

        Ok(())
    }
}

pub struct Ast {
    records: HashMap<USR, Record>,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            records: HashMap::new(),
        }
    }

    pub fn pretty_print(&self, depth: usize) {
        for (usr, record) in &self.records {
            print!("{usr}: ");
            record.pretty_print(depth + 1, self);
        }
    }
}

fn qualtype_from_typeref(c_tr: Cursor) -> Result<QualType> {
    if let Ok(c_ref) = c_tr.referenced() {
        let c_ref =
            if c_ref.kind() == CursorKind::ClassDecl || c_ref.kind() == CursorKind::ClassTemplate {
                c_ref.canonical().unwrap()
            } else {
                c_ref
            };

        let is_const = if let Ok(ty) = c_tr.ty() {
            ty.is_const_qualified()
        } else {
            false
        };

        match c_ref.kind() {
            CursorKind::ClassDecl | CursorKind::ClassTemplate => {
                let c_ref = c_ref.canonical().unwrap();
                Ok(QualType {
                    name: c_ref.spelling(),
                    is_const,
                    type_ref: TypeRef::Ref(c_ref.usr()),
                })
            }
            CursorKind::TemplateTypeParameter => Ok(QualType {
                name: c_ref.spelling(),
                is_const,
                type_ref: TypeRef::TemplateTypeParameter(c_ref.spelling()),
            }),
            _ => {
                error!("unhandled type {}", c_tr.display_name());
                Ok(QualType::unknown(c_ref.ty().unwrap().kind()))
            }
        }
    } else {
        error!(
            "could not get referenced type from TypeRef {}",
            c_tr.display_name()
        );
        Ok(QualType::unknown(c_tr.ty().unwrap().kind()))
    }
}

fn bind_type(ty: Type, template_parameters: &[String]) -> Result<QualType> {
    let is_const = ty.is_const_qualified();
    let name = ty.spelling();

    if ty.is_builtin() {
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Builtin(ty.kind()),
        })
    } else if let Ok(c_ref) = ty.type_declaration() {
        println!("type {name} has decl {} {}", c_ref.spelling(), c_ref.usr());
        Ok(QualType {
            name,
            is_const,
            type_ref: TypeRef::Ref(c_ref.usr()),
        })
    } else {
        match ty.kind() {
            TypeKind::Pointer => {
                let pointee = ty.pointee_type()?;
                let ty_ref = bind_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::Pointer(Box::new(ty_ref)),
                })
            }
            TypeKind::LValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = bind_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::LValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::RValueReference => {
                let pointee = ty.pointee_type()?;
                let ty_ref = bind_type(pointee, template_parameters)?;
                Ok(QualType {
                    name,
                    is_const,
                    type_ref: TypeRef::RValueReference(Box::new(ty_ref)),
                })
            }
            TypeKind::Unexposed => {
                let name = if is_const {
                    name.strip_prefix("const ").unwrap().to_string()
                } else {
                    name
                };
                if template_parameters.contains(&name) {
                    Ok(QualType {
                        name: name.clone(),
                        is_const,
                        type_ref: TypeRef::TemplateTypeParameter(name.clone()),
                    })
                } else {
                    error!(
                        "Got unexposed for {name} with no matching template parmaeter in {:?}",
                        template_parameters
                    );
                    Err(Error::TypeUnexposed)
                }
            }
            _ => {
                error!("Unhandled {:?}", ty);
                Ok(QualType::unknown(ty.kind()))
            }
        }
    }
}

fn bind_argument(c_arg: Cursor, template_parameters: &[String]) -> Result<Argument> {
    let children = c_arg.children();

    let ty = c_arg.ty()?;

    let qual_type = if ty.is_builtin() || ty.is_pointer() {
        bind_type(ty, template_parameters)?
    } else if !children.is_empty() {
        match children[0].kind() {
            CursorKind::TypeRef | CursorKind::TemplateRef => qualtype_from_typeref(children[0])?,
            _ => {
                debug!("other kind {:?}", children[0].kind(),);
                QualType::unknown(children[0].ty().unwrap().kind())
            }
        }
    } else {
        error!("coulnd't do argument type");
        QualType::unknown(children[0].ty().unwrap().kind())
    };

    Ok(Argument {
        name: c_arg.spelling(),
        qual_type,
    })
}

fn bind_method(
    c_method: Cursor,
    depth: usize,
    class_template_parameters: &Vec<String>,
) -> Result<Method> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let c_method = c_method.canonical().unwrap();

    debug!(
        "{indent}+ CXXMethod {} {}",
        c_method.display_name(),
        if c_method.cxx_method_is_const() {
            "const"
        } else {
            ""
        }
    );

    // NOTE (AL) The only reliable way to get at return type and arguments appears to be to inspect the children directly.
    // This is unfortunately a little vague, but appears to be:
    // 1. Any TemplateType/NonTypeTemplateParameters (etc. one would assume)
    // 2. The return type (if it's not a builtin)
    // 3. The arguments
    // So we need to traverse the children, pluck off any template parameters,
    // then the first thing that's *not* a template parameter will be the return type, and everything after that will be an argument
    // let c_method_template_parameters = c_method.children().iter().filter(|c| c.kind() == CursorKind::TemplateTypeParameter || c.kind() == CursorKind::NonTypeTemplateParameter || c.kind() || CursorKind::TemplateTemplateParameter)
    let children = c_method.children();
    debug!("{} children", children.len());
    debug!("{:?}", children);

    let c_template_parameters: Vec<Cursor> = children
        .iter()
        .take_while(|c| {
            c.kind() == CursorKind::TemplateTypeParameter
                || c.kind() == CursorKind::NonTypeTemplateParameter
                || c.kind() == CursorKind::TemplateTemplateParameter
        })
        .map(|c| {
            debug!("Taking {} as template type", c.display_name());
            c.clone()
        })
        .collect();

    let template_parameters = class_template_parameters
        .iter()
        .map(|t| t.clone())
        .chain(c_template_parameters.iter().map(|c| c.display_name()))
        .collect::<Vec<_>>();

    let mut skip = c_template_parameters.len();

    let ty_result = c_method.result_ty().unwrap();
    let result = if ty_result.is_builtin() || ty_result.is_pointer() {
        bind_type(ty_result, &template_parameters)?
    } else {
        let c_result = children.iter().skip(skip).next().expect(&format!(
            "Could not get result cursor from {}",
            c_method.display_name()
        ));

        skip += 1;

        if c_result.kind() == CursorKind::TypeRef || c_result.kind() == CursorKind::TemplateRef {
            qualtype_from_typeref(c_result.clone())?
        } else {
            QualType::unknown(ty_result.kind())
        }
    };

    let num_arguments = match c_method.num_arguments() {
        Ok(n) => n as usize,
        Err(_) => children.len(),
    };
    let mut arguments: Vec<Argument> = Vec::with_capacity(num_arguments);
    for c_arg in children.iter().skip(skip).take(num_arguments) {
        debug!("{indent}    arg: {}", c_arg.display_name());

        if c_arg.kind() != CursorKind::ParmDecl {
            break;
        }

        arguments.push(bind_argument(c_arg.clone(), &template_parameters)?);

        // if let Ok(ty_arg) = c_arg.ty() {
        //     println!("{indent}      {ty_arg:?}")
        // }

        // for child in c_arg.children() {
        //     println!("{indent}        {child:?}");
        // }
    }

    /*
    //     bind_type(ty_result)?
    let qt_result = if let Ok(ty_result) = c_method.result_ty() {
        match bind_type(ty_result.clone()) {
            Ok(qt_result) => {
                println!(" -> {}", qt_result);
                qt_result
            }
            // Err(Error::TypeUnexposed) => {
            //     // inspect children to get type
            // }
            Err(e) => {
                println!("\nERROR unexpcted error binding result type for {}: {e}", c_method.display_name());
                // return Err(Error::InvalidType);
                QualType::unknown(ty_result.kind())
            }
        }
    } else {
        println!("\nERROR could not get result type for {}", c_method.display_name());
        // return Err(Error::InvalidType);
        QualType::unknown(TypeKind::Unexposed)
    };

    if let Ok(num_args) = c_method.num_arguments() {
        for i in 0..num_args {
            let c_arg = c_method.argument(i).unwrap();
            // println!("{indent}    arg: {c_arg:?}");

            parameters.push(Parameter {
                name: c_arg.spelling(),
            });

            // should always be ParmDecl
            if c_arg.kind() != CursorKind::ParmDecl {
                unimplemented!();
            }

            // if let Ok(ty_arg) = c_arg.ty() {
            //     println!("{indent}      {ty_arg:?}")
            // }

            // for child in c_arg.children() {
            //     println!("{indent}        {child:?}");
            // }
        }
    }
    */

    for child in c_method.children() {
        debug!(
            "{indent}    - {}: {} {}",
            child.display_name(),
            child.kind(),
            child.usr()
        );
        if child.kind() == CursorKind::TypeRef || child.kind() == CursorKind::TemplateRef {
            if let Ok(c_ref) = child.referenced() {
                let c_ref = if c_ref.kind() == CursorKind::ClassDecl
                    || c_ref.kind() == CursorKind::ClassTemplate
                {
                    c_ref.canonical().unwrap()
                } else {
                    c_ref
                };

                debug!(
                    "{indent}      - {}: {} {}",
                    c_ref.display_name(),
                    c_ref.kind(),
                    c_ref.usr()
                );
            }
        }
    }

    Ok(Method::new(
        c_method.usr(),
        c_method.spelling(),
        result,
        arguments,
        c_method.cxx_method_is_const(),
        c_method.cxx_method_is_static(),
        c_method.cxx_method_is_virtual(),
        c_method.cxx_method_is_pure_virtual(),
    ))
}

fn bind_class_template(class_template: Cursor, depth: usize) -> Record {
    let indent = format!("{:width$}", "", width = depth * 2);

    debug!("{indent}bind_class_template({})", class_template.usr());

    let mut methods = Vec::new();
    let mut fields = Vec::new();
    let mut template_parameters = Vec::new();

    let members = class_template.children();
    for member in members {
        match member.kind() {
            CursorKind::TemplateTypeParameter => {
                let t = member.display_name();
                template_parameters.push(t);
            }
            CursorKind::CXXMethod
            | CursorKind::Constructor
            | CursorKind::Destructor
            | CursorKind::FunctionTemplate => {
                if let Ok(method) = bind_method(member, depth + 1, &template_parameters) {
                    methods.push(method);
                }
            }
            _ => {
                debug!("{indent}  {member:?}");
                for child in member.children() {
                    debug!("{indent}    {child:?}");
                    match child.kind() {
                        CursorKind::TypeRef => {
                            if let Ok(c) = child.referenced() {
                                debug!("{indent}    -> {c:?}");
                            }
                        }
                        CursorKind::ParmDecl => {
                            if let Ok(ty) = child.ty() {
                                debug!("{indent}      type {ty:?}")
                            }

                            for c in child.children() {
                                debug!("{indent}      {c:?}");
                            }
                        }
                        CursorKind::CompoundStmt => {
                            for stmt in child.children() {
                                debug!("{indent}      {stmt:?}");
                            }
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    Record {
        usr: class_template.usr(),
        name: class_template.spelling(),
        fields,
        methods,
        template_parameters,
    }
}

pub fn ast_from_namespace(name: &str, c_tu: Cursor) -> Ast {
    let ns = c_tu.children_of_kind_with_name(CursorKind::Namespace, name, true);

    let mut binding = Ast::new();
    for cur in ns {
        print(cur.clone(), 0, 2, Vec::new(), Vec::new(), &mut binding);
    }

    binding
}

fn print(
    c: Cursor,
    depth: usize,
    max_depth: usize,
    already_visited: Vec<String>,
    mut namespaces: Vec<String>,
    binding: &mut Ast,
) {
    if depth > max_depth {
        // println!("");
        return;
    }
    let indent = format!("{:width$}", "", width = depth * 2);

    /*
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
    */

    if c.kind() == CursorKind::ClassTemplate {
        let record = bind_class_template(c, depth + 1);
        binding.records.insert(record.usr.clone(), record);
    }

    debug!("{indent}{}: {} {}", c.kind(), c.display_name(), c.usr());

    if let Ok(cr) = c.referenced() {
        if cr != c && !already_visited.contains(&cr.usr().0) {
            // print!("{}-> ", indent);
            let mut v = already_visited.clone();
            if !cr.usr().0.is_empty() {
                v.push(cr.usr().0);
            }
            print(cr, depth + 1, max_depth, v, Vec::new(), binding);
        }
    }

    let children = c.children();
    if children.len() > 0 {}

    for child in children {
        let mut v = already_visited.clone();
        if !child.usr().0.is_empty() {
            v.push(child.usr().0);
        }
        // print!("{}C ", indent);
        print(child, depth + 1, max_depth, v, namespaces.clone(), binding);
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

/*
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
*/
