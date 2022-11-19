use bbl_clang::{
    cli_args_with,
    cursor::{ChildVisitResult, CurTypedef, Cursor, USR},
    cursor_kind::CursorKind,
    diagnostic::Severity,
    exception::ExceptionSpecificationKind,
    index::Index,
    template_argument::TemplateArgumentKind,
    translation_unit::TranslationUnit,
    ty::{Type, TypeKind},
    virtual_file::FileCommands,
};
use log::{debug, error, info, warn};

use bbl_extract::{
    ast::{get_namespaces_for_decl, AST},
    class::{ClassBindKind, ClassDecl, NeedsImplicit, OverrideList},
    function::{
        extract_function, extract_method, Argument, Const, Deleted, Method, MethodKind,
        PureVirtual, Virtual,
    },
    namespace::extract_namespace,
    qualtype::{extract_type, QualType},
    AllowList,
};
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Extraction error")]
    Extraction(#[from] bbl_extract::error::Error),
    #[error("Clang error")]
    Clang(#[from] bbl_clang::error::Error),
    #[error("I/O Error")]
    Io(#[from] std::io::Error),
    #[error("Error parsing C++")]
    CompilationError,
}

#[derive(Debug, Default, Clone)]
struct Binding {
    classes: Vec<ClassBinding>,
    namespace_aliases: Vec<(Cursor, String)>,
    functions: Vec<FunctionBinding>,
}

impl Binding {
    fn functions(&self) -> &[FunctionBinding] {
        self.functions.as_ref()
    }
}

#[derive(Debug, Clone)]
struct ClassBinding {
    class_decl: Cursor,
    methods: Vec<FunctionBinding>,
    ignore: bool,
    rename: Option<String>,
    bind_kind: ClassBindKind,
    needs_implicit: NeedsImplicit,
    ctors: Vec<(Vec<Type>, Option<String>)>,
    tu: TranslationUnit,
}

impl ClassBinding {
    pub fn new(class_decl: Cursor, tu: TranslationUnit, rename: Option<String>) -> ClassBinding {
        ClassBinding {
            class_decl,
            methods: Vec::new(),
            ignore: false,
            rename,
            bind_kind: ClassBindKind::OpaquePtr,
            needs_implicit: NeedsImplicit::default(),
            ctors: Vec::new(),
            tu,
        }
    }

    fn ctors(&self) -> &[(Vec<Type>, Option<String>)] {
        self.ctors.as_ref()
    }
}

impl ClassBinding {
    fn class_decl(&self) -> Cursor {
        self.class_decl
    }

    fn methods(&self) -> &[FunctionBinding] {
        self.methods.as_ref()
    }

    fn ignore(&self) -> bool {
        self.ignore
    }

    fn rename(&self) -> Option<&String> {
        self.rename.as_ref()
    }

    fn bind_kind(&self) -> ClassBindKind {
        self.bind_kind
    }
}

#[derive(Debug, Clone)]
struct FunctionBinding {
    decl: Cursor,
    rename: Option<String>,
}

fn extract_class_binding(
    c_expr_with_cleanups: Cursor,
    class_binding: &mut ClassBinding,
) -> Result<()> {
    c_expr_with_cleanups.visit_children(|c, _| {
        match c.kind() {
            CursorKind::CallExpr if c.display_name() == "m" => {
                let mut rename = None;
                for child in c.children(){
                    if child.kind() == CursorKind::ImplicitCastExpr {
                        if let Some(c_lit) = child.first_child_of_kind(CursorKind::StringLiteral) {
                            rename = Some(strip_quotes(c_lit.display_name()));
                        }
                    }
                }
                // Then we get UnaryOperator > DeclRefExpr > CXXMethod
                if let Some(c_uo) = c.first_child_of_kind(CursorKind::UnaryOperator) {
                    if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr) {
                        if let Ok(c_method) = c_ref_expr.referenced() {
                            if c_method.kind() == CursorKind::CXXMethod {
                                class_binding.methods.push(FunctionBinding{decl: c_method, rename});
                            } else {
                                error!(
                                "got what should have been the CXXMethod but it was {c_method:?}"
                            );
                            }
                        }
                    }
                // Or a static_cast<>() if the user is disambiguating overloads
                } else if let Some(c_sc) = c.first_child_of_kind(CursorKind::CXXStaticCastExpr) {
                    if let Some(c_uo) = c_sc.first_child_of_kind(CursorKind::UnaryOperator) {
                        if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr)
                        {
                            if let Ok(c_method) = c_ref_expr.referenced() {
                                if c_method.kind() == CursorKind::CXXMethod {
                                    class_binding.methods.push(FunctionBinding{decl: c_method, rename});
                                } else {
                                    error!(
                                "got what should have been the CXXMethod but it was {c_method:?}"
                            );
                                }
                            }
                        }
                    }
                // Or a C-style cast
                } else if let Some(c_sc) = c.first_child_of_kind(CursorKind::CStyleCastExpr) {
                    if let Some(c_ic) = c_sc.first_child_of_kind(CursorKind::ImplicitCastExpr) {
                        if let Some(c_pe) = c_ic.first_child_of_kind(CursorKind::ParenExpr) {
                            if let Some(c_uo) = c_pe.first_child_of_kind(CursorKind::UnaryOperator) {
                                if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr)
                                {
                                    if let Ok(c_method) = c_ref_expr.referenced() {
                                        if c_method.kind() == CursorKind::CXXMethod {
                                            class_binding.methods.push(FunctionBinding{decl: c_method, rename});
                                        } else {
                                            error!( "got what should have been the CXXMethod but it was {c_method:?}");
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if let Some(c_call) = c.first_child_of_kind(CursorKind::CallExpr) {
                    match c_call.display_name().as_str() {
                        // We can't use member function pointers for constructors, so we differentiate the ctors using
                        // a special type, Ctor, whose template parameter pack matches the arguments for the given
                        // constructor (and the signature is checked with SFINAE on the C++ side).
                        // The other RoF methods are checked with similar types, below (but they don't have arguments)
                        "Ctor" => {
                            let ctor_ty = c_call.ty().unwrap();
                            let ctor_decl = ctor_ty.type_declaration().unwrap();
                            if ctor_decl.num_template_arguments() == 1
                                && ctor_decl.template_argument_kind(0).unwrap()
                                    == TemplateArgumentKind::Pack
                            {
                                let pack_size = ctor_decl.template_argument_pack_size(0);
                                if pack_size >= 0 {
                                    let mut ctor_args = Vec::new();
                                    for i in 0..pack_size {
                                        ctor_args.push(
                                            ctor_decl
                                                .template_argument_pack_type(0, i as u32)
                                                .unwrap(),
                                        );
                                    }

                                    class_binding
                                        .ctors
                                        .push((ctor_args, get_first_string_arg(c)));
                                } else {
                                    error!("Ctor decl pak should have 0 or more args");
                                }
                            } else {
                                error!("Ctor decl should have 1 template pack arg");
                            }
                        }
                        "CopyCtor" => {
                            class_binding.needs_implicit.copy_ctor = true;
                        }
                        "MoveCtor" => {
                            class_binding.needs_implicit.move_ctor = true;
                        }
                        "Dtor" => {
                            class_binding.needs_implicit.dtor = true;
                        }
                        _ => (),
                    }
                }

                ChildVisitResult::Recurse
            }
            CursorKind::CallExpr if c.display_name() == "Class" => {
                let children = c.children();
                for child in children {
                    if child.kind() == CursorKind::ImplicitCastExpr {
                        if let Some(c_lit) = child.first_child_of_kind(CursorKind::StringLiteral) {
                            class_binding.rename = Some(strip_quotes(c_lit.display_name()));
                        }
                    }
                }

                // Class call should always be last
                ChildVisitResult::Break
            }
            _ => ChildVisitResult::Recurse,
        }
    });

    Ok(())
}

fn get_class_from_typedef(c_td: Cursor) -> Option<Cursor> {
    let td: CurTypedef = c_td.try_into().unwrap();
    let u_decl = td.underlying_type().unwrap().type_declaration().unwrap();

    match u_decl.kind() {
        CursorKind::ClassDecl | CursorKind::StructDecl => Some(u_decl),
        CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => get_class_from_typedef(u_decl),
        _ => None,
    }
}

fn get_underlying_type(ty: Type) -> Result<Type> {
    match ty.kind() {
        TypeKind::Elaborated => get_underlying_type(ty.named_type()?),
        // TypeKind::Using => get_underlying_type(ty.underlying_type()?),
        TypeKind::Using => panic!("GOT USING on {ty:?}"),
        _ => Ok(ty),
    }
}

fn create_binding(c_compound: Cursor, tu: &TranslationUnit, binding: &mut Binding) {
    c_compound.visit_children(|c, _| {
        // If the user binds without any method calls on the Class object, then the entry will be CallExpr
        // This might be nested under a VarDecl if the user has done `auto v = bbl::Class<Foo>()`
        // Once we've hit this point, the full expression appears in reverse order, so given:
        //
        // Class<Foo>("Foo")
        //   .m(&Foo::bar)
        //   .m(&Foo::baz)
        //
        // we get m(&Foo::baz) first, then m(&Foo::bar), then the constructor call.
        match c.kind() {
            CursorKind::CallExpr if c.display_name() == "fn" || c.display_name() == "bbl::fn" => {
                extract_function_binding(c, binding);
            }
            CursorKind::ExprWithCleanups | CursorKind::CallExpr => {
                if let Ok(ty) = c.ty() {
                    if ty.spelling().starts_with("Class<")
                        || ty.spelling().starts_with("bbl::Class<")
                    {
                        debug!("Found Class< {ty:?}");
                        let num_template_args = ty.num_template_arguments();
                        if num_template_args == 1 {
                            let tty = ty.template_argument_as_type(0).unwrap();
                            let mut rename = None;
                            let tty = get_underlying_type(tty).unwrap();
                            if let Ok(decl) = tty.type_declaration() {
                                if matches!(
                                    decl.kind(),
                                    CursorKind::ClassDecl
                                        | CursorKind::StructDecl
                                        | CursorKind::TypedefDecl
                                        | CursorKind::TypeAliasDecl
                                ) {
                                    let class_decl = match decl.kind() {
                                        CursorKind::ClassDecl | CursorKind::StructDecl => decl,
                                        CursorKind::TypedefDecl | CursorKind::TypeAliasDecl => {
                                            debug!("Got typedef {}", decl.display_name());
                                            rename = Some(decl.display_name());
                                            get_class_from_typedef(decl).unwrap()
                                        }
                                        _ => unreachable!(),
                                    };

                                    let mut class_binding =
                                        ClassBinding::new(class_decl, tu.clone(), rename);
                                    extract_class_binding(c, &mut class_binding).unwrap();
                                    binding.classes.push(class_binding);

                                    return ChildVisitResult::Continue;
                                } else {
                                    error!("Invalid type passed to Class constructor: {decl:?}");
                                }
                            } else {
                                error!("Could not get decl from {tty:?}");
                            }
                        } else {
                            error!("Got Class< with {num_template_args} template args");
                        }
                    }
                }
                return ChildVisitResult::Continue;
            }
            CursorKind::DeclStmt => {
                // use namespace aliases to capture namespace renaming
                if let Some(c_ns_alias) = c.first_child_of_kind(CursorKind::NamespaceAlias) {
                    if let Ok(c_ns) = c_ns_alias.definition() {
                        binding
                            .namespace_aliases
                            .push((c_ns, c_ns_alias.display_name()));
                    }
                }
            }
            // allow inner blocks
            CursorKind::CompoundStmt => {
                create_binding(c, tu, binding);
                return ChildVisitResult::Continue;
            }
            _ => (),
        }
        ChildVisitResult::Recurse
    });
}

fn extract_function_binding(c: Cursor, binding: &mut Binding) {
    let mut rename = None;
    let children = c.children();
    for child in children {
        if child.kind() == CursorKind::ImplicitCastExpr {
            if let Some(c_lit) = child.first_child_of_kind(CursorKind::StringLiteral) {
                rename = Some(strip_quotes(c_lit.display_name()));
            }
        }
    }
    // Then we get UnaryOperator > DeclRefExpr > CXXMethod
    if let Some(c_uo) = c.first_child_of_kind(CursorKind::UnaryOperator) {
        if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr) {
            if let Ok(c_function) = c_ref_expr.referenced() {
                if c_function.kind() == CursorKind::FunctionDecl {
                    binding.functions.push(FunctionBinding {
                        decl: c_function,
                        rename,
                    });
                } else {
                    error!("got what should have been the FunctionDecl but it was {c_function:?}");
                }
            }
        }
    // Or a static_cast<>() if the user is disambiguating overloads
    } else if let Some(c_sc) = c.first_child_of_kind(CursorKind::CXXStaticCastExpr) {
        if let Some(c_uo) = c_sc.first_child_of_kind(CursorKind::UnaryOperator) {
            if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr) {
                if let Ok(c_function) = c_ref_expr.referenced() {
                    if c_function.kind() == CursorKind::FunctionDecl {
                        binding.functions.push(FunctionBinding {
                            decl: c_function,
                            rename,
                        });
                    } else {
                        error!(
                            "got what should have been the FunctionDecl but it was {c_function:?}"
                        );
                    }
                }
            }
        }
    // Or a C-style cast
    } else if let Some(c_sc) = c.first_child_of_kind(CursorKind::CStyleCastExpr) {
        if let Some(c_uo) = c_sc.first_child_of_kind(CursorKind::UnaryOperator) {
            if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr) {
                if let Ok(c_function) = c_ref_expr.referenced() {
                    if c_function.kind() == CursorKind::FunctionDecl {
                        binding.functions.push(FunctionBinding {
                            decl: c_function,
                            rename,
                        });
                    } else {
                        error!(
                            "got what should have been the FunctionDecl but it was {c_function:?}"
                        );
                    }
                }
            }
        }
    } else {
        error!("Expected UnaryOperator or CXXStaticCastExpr extracting function. Got {c:?}");
    }
}

fn strip_quotes(s: String) -> String {
    let mut s = s;
    s.pop();
    s.remove(0);
    s
}

fn get_first_string_arg(c: Cursor) -> Option<String> {
    c.first_child_of_kind(CursorKind::ImplicitCastExpr)
        .and_then(|ice| {
            ice.first_child_of_kind(CursorKind::StringLiteral)
                .map(|sl| strip_quotes(sl.display_name()))
        })
}

fn bind_class(
    cb: &ClassBinding,
    tu: &TranslationUnit,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
) -> Result<ClassDecl> {
    let c_class_decl = cb.class_decl();
    let name = c_class_decl.display_name();
    let namespaces = get_namespaces_for_decl(c_class_decl, tu, ast, already_visited)?;

    let mut class = ClassDecl::new(
        cb.class_decl().usr(),
        name,
        Vec::new(),
        Vec::new(),
        namespaces,
        Vec::new(),
        false,
        cb.needs_implicit.clone(),
    );

    if let Some(rename) = cb.rename() {
        class.set_rename(rename);
    }

    class.set_ignore(cb.ignore());
    class.set_bind_kind(cb.bind_kind());

    Ok(class)
}

fn create_root_bindings_from_binding_tu(
    c: Cursor,
    filename: &str,
    tu: &TranslationUnit,
    ast: &mut AST,
    binding: &mut Binding,
) -> Result<()> {
    let bind_fn = c
        .first_child_of_kind_with_name(CursorKind::FunctionDecl, "bbl_bind")
        .expect("Could not find bbl_bind");

    for inc in c.children_of_kind(CursorKind::InclusionDirective, false) {
        if let Some(s) = inc.location().spelling_location().file.file_name() {
            if s == filename && inc.spelling() != "bbl.hpp" {
                ast.append_include(&format!("#include <{}>", inc.spelling()));
            }
        }
    }

    let body = bind_fn
        .first_child_of_kind(CursorKind::CompoundStmt)
        .expect("Could not find body for bbl_bind");

    create_binding(body, tu, binding);

    Ok(())
}

pub fn extract_ast_from_binding(file_commands: &[FileCommands]) -> Result<AST> {
    let mut ast = AST::new();
    let mut already_visited = Vec::new();
    let mut binding = Binding::default();

    let mut tus = Vec::new();

    let allow_list = AllowList::default();
    let overrides = OverrideList::default();

    let mut has_error = false;
    for fc in file_commands {
        let index = Index::new();
        let tu = index.create_translation_unit(fc.filename(), &cli_args_with(fc.args())?)?;
        for d in tu.diagnostics() {
            match d.severity() {
                Severity::Ignored => debug!("{}", d),
                Severity::Note => info!("{}", d),
                Severity::Warning => warn!("{}", d),
                Severity::Error | Severity::Fatal => {
                    has_error = true;
                    error!("{}", d)
                }
            }
        }

        let cur = tu.get_cursor()?;
        create_root_bindings_from_binding_tu(cur, fc.filename(), &tu, &mut ast, &mut binding)?;

        tus.push((fc.filename().to_string(), tu));
    }

    if has_error {
        return Err(Error::CompilationError);
    }

    // first extract the classes to fill the AST with types so we favour the users extractions
    for cb in &binding.classes {
        let class = bind_class(cb, &cb.tu, &mut ast, &mut already_visited)?;
        already_visited.push(class.usr());
        ast.insert_class(class);
    }

    // now extract their methods
    for cb in &binding.classes {
        let u_class = cb.class_decl().usr();
        let mut namespaces = ast.get_class(u_class).unwrap().namespaces().to_vec();
        namespaces.push(u_class);

        for FunctionBinding {
            decl: c_method,
            rename,
        } in cb.methods()
        {
            let mut method = match extract_method(
                *c_method,
                &[],
                &mut already_visited,
                &cb.tu,
                &mut ast,
                &allow_list,
                &overrides,
                &cb.class_decl().display_name(),
                false,
            ) {
                Ok(m) => m,
                Err(e) => {
                    warn!(
                        "Failed to extract method {} from class {}: {e}",
                        c_method.display_name(),
                        cb.class_decl().display_name()
                    );
                    continue;
                }
            };

            method.set_replacement_name(rename.clone());

            let class = ast.get_class_mut(cb.class_decl().usr()).unwrap();
            class.push_method(method);
        }

        // do the constructors
        for (index, (ctor_args, rename)) in cb.ctors().iter().enumerate() {
            let args = ctor_args
                .iter()
                .enumerate()
                .map(|(pi, t)| {
                    extract_type(
                        *t,
                        &[],
                        &mut already_visited,
                        &mut ast,
                        &cb.tu,
                        &AllowList::default(),
                        &OverrideList::default(),
                        true,
                    )
                    .map(|ty| Argument::new(&format!("parm{pi}"), ty))
                })
                .collect::<Result<Vec<Argument>, bbl_extract::error::Error>>()?;

            let ctor = Method::new(
                USR::new(&format!("{u_class}#ctor:{index}")),
                "ctor".to_string(),
                MethodKind::Constructor,
                QualType::void(),
                args,
                rename.clone(),
                namespaces.clone(),
                Vec::new(),
                ExceptionSpecificationKind::None,
                Const(false),
                Virtual(false),
                PureVirtual(false),
                Deleted(false),
            );

            let class = ast.get_class_mut(u_class).unwrap();
            class.push_method(ctor);
        }
    }

    // process any namespace renames
    for (c_namespace, name) in &binding.namespace_aliases {
        let u_ns = extract_namespace(
            *c_namespace,
            &c_namespace.translation_unit(),
            &mut ast,
            &mut already_visited,
        )
        .unwrap();
        let ns = ast.get_namespace(u_ns).unwrap();
        let parent_namespaces = ns.namespaces().to_vec();

        if parent_namespaces.is_empty() {
            // is a top-level namespace, rename directly
            ast.set_namespace_rename(u_ns, name).unwrap();
        } else {
            // is an inner namespace. rename the highest-level parent and set all others, including this one, to empty
            ast.set_namespace_rename(parent_namespaces[0], name)
                .unwrap();

            for u_p_ns in parent_namespaces.iter().skip(1) {
                ast.set_namespace_rename(*u_p_ns, "").unwrap();
            }
            ast.set_namespace_rename(u_ns, "").unwrap();
        }
    }

    // extract functions
    for fn_binding in binding.functions() {
        let mut function = extract_function(
            fn_binding.decl,
            &[],
            &mut already_visited,
            &fn_binding.decl.translation_unit(),
            &mut ast,
            &allow_list,
            &overrides,
            true,
        )?;
        function.set_replacement_name(fn_binding.rename.clone());

        ast.insert_function(function);
    }

    Ok(ast)
}

/*
/// Main recursive function to walk the AST and extract the pieces we're interested in
pub fn extract_ast_from_binding_tu(
    c: Cursor,
    filename: &str,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    stop_on_error: bool,
) -> Result<()> {
    // first, get the bbl_bind() function
    let bind_fn = c
        .first_child_of_kind_with_name(CursorKind::FunctionDecl, "bbl_bind")
        .expect("Could not find bbl_bind");

    // dump_cursor_until(bind_fn, tu, 20, false);

    for inc in c.children_of_kind(CursorKind::InclusionDirective, false) {
        if let Some(s) = inc.location().spelling_location().file.file_name() {
            if s == filename && inc.spelling() != "bbl.hpp" {
                ast.append_include(&format!("#include <{}>", inc.spelling()));
            }
        }
    }

    let body = bind_fn
        .first_child_of_kind(CursorKind::CompoundStmt)
        .expect("Could not find body for bbl_bind");

    let mut binding = Binding::default();
    create_binding(body, tu, &mut binding);

    // println!("Got binding: {binding:?}");

    let allow_list = AllowList::default();
    let overrides = OverrideList::default();

    // first extract the classes to fill the AST with types so we favour the users extractions
    for cb in &binding.classes {
        let class = bind_class(cb, tu, ast, already_visited)?;
        already_visited.push(class.usr());
        ast.insert_class(class);
    }

    // now extract their methods
    for cb in &binding.classes {
        let u_class = cb.class_decl().usr();
        let mut namespaces = ast.get_class(u_class).unwrap().namespaces().to_vec();
        namespaces.push(u_class);

        for FunctionBinding {
            decl: c_method,
            rename,
        } in cb.methods()
        {
            let mut method = extract_method(
                *c_method,
                &[],
                already_visited,
                tu,
                ast,
                &allow_list,
                &overrides,
                &cb.class_decl().display_name(),
                stop_on_error,
            )
            .unwrap_or_else(|_| {
                panic!(
                    "Failed to extract method {} from class {}",
                    c_method.display_name(),
                    cb.class_decl().display_name()
                )
            });

            method.set_replacement_name(rename.clone());

            let class = ast.get_class_mut(cb.class_decl().usr()).unwrap();
            class.push_method(method);
        }

        // do the constructors
        for (index, (ctor_args, rename)) in cb.ctors().iter().enumerate() {
            let args = ctor_args
                .iter()
                .enumerate()
                .map(|(pi, t)| {
                    extract_type(
                        *t,
                        &[],
                        already_visited,
                        ast,
                        tu,
                        &AllowList::default(),
                        &OverrideList::default(),
                        true,
                    )
                    .map(|ty| Argument::new(&format!("parm{pi}"), ty))
                })
                .collect::<Result<Vec<Argument>, bbl_extract::error::Error>>()?;

            let ctor = Method::new(
                USR::new(&format!("{u_class}#ctor:{index}")),
                "ctor".to_string(),
                MethodKind::Constructor,
                QualType::void(),
                args,
                rename.clone(),
                namespaces.clone(),
                Vec::new(),
                ExceptionSpecificationKind::None,
                Const(false),
                Virtual(false),
                PureVirtual(false),
                Deleted(false),
            );

            let class = ast.get_class_mut(u_class).unwrap();
            class.push_method(ctor);
        }
    }

    // process any namespace renames
    for (c_namespace, name) in &binding.namespace_aliases {
        let u_ns = extract_namespace(*c_namespace, tu, ast, already_visited).unwrap();
        let ns = ast.get_namespace(u_ns).unwrap();
        let parent_namespaces = ns.namespaces().to_vec();

        if parent_namespaces.is_empty() {
            // is a top-level namespace, rename directly
            ast.set_namespace_rename(u_ns, name).unwrap();
        } else {
            // is an inner namespace. rename the highest-level parent and set all others, including this one, to empty
            ast.set_namespace_rename(parent_namespaces[0], name)
                .unwrap();

            for u_p_ns in parent_namespaces.iter().skip(1) {
                ast.set_namespace_rename(*u_p_ns, "").unwrap();
            }
            ast.set_namespace_rename(u_ns, "").unwrap();
        }
    }

    // extract functions
    for fn_binding in binding.functions() {
        let mut function = extract_function(
            fn_binding.decl,
            &[],
            already_visited,
            tu,
            ast,
            &allow_list,
            &overrides,
            stop_on_error,
        )?;
        function.set_replacement_name(fn_binding.rename.clone());

        ast.insert_function(function);
    }

    // println!("{ast:?}");

    Ok(())
}

pub fn bind_file<P: AsRef<Path> + std::fmt::Debug, S: AsRef<str> + std::fmt::Debug>(
    path: P,
    cli_args: &[S],
    log_diagnostics: bool,
    stop_on_error: bool,
) -> Result<AST> {
    let index = Index::new();
    let tu = index.create_translation_unit(&path, cli_args)?;

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

    let cur = tu.get_cursor()?;

    let mut already_visited = Vec::new();
    let mut ast = AST::new();

    extract_ast_from_binding_tu(
        cur,
        path.as_ref().as_os_str().to_str().unwrap(),
        &mut already_visited,
        &mut ast,
        &tu,
        stop_on_error,
    )?;

    Ok(ast)
}

pub fn bind_string<S1: AsRef<str> + std::fmt::Debug, S: AsRef<str> + std::fmt::Debug>(
    contents: S1,
    cli_args: &[S],
    log_diagnostics: bool,
    stop_on_error: bool,
) -> Result<AST> {
    let path = virtual_file::write_temp_file(contents.as_ref())?;
    bind_file(&path, cli_args, log_diagnostics, stop_on_error)
}
*/
