use bbl_clang::{
    cursor::{ChildVisitResult, CurTypedef, Cursor, USR},
    cursor_kind::CursorKind,
    exception::ExceptionSpecificationKind,
    template_argument::TemplateArgumentKind,
    translation_unit::TranslationUnit,
    ty::{Type, TypeKind},
};
use log::debug;
use tracing::error;

use crate::{
    ast::{dump_cursor, dump_cursor_until, get_namespaces_for_decl, AST},
    class::{ClassBindKind, ClassDecl, NeedsImplicit, OverrideList},
    error::Error,
    function::{
        extract_function, extract_method, Argument, Const, Deleted, Method, MethodKind,
        PureVirtual, Virtual,
    },
    namespace::extract_namespace,
    qualtype::{extract_type, QualType},
    AllowList,
};
type Result<T, E = Error> = std::result::Result<T, E>;

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
}

impl ClassBinding {
    pub fn new(class_decl: Cursor, rename: Option<String>) -> ClassBinding {
        ClassBinding {
            class_decl,
            methods: Vec::new(),
            ignore: false,
            rename,
            bind_kind: ClassBindKind::OpaquePtr,
            needs_implicit: NeedsImplicit::default(),
            ctors: Vec::new(),
        }
    }

    fn needs_implicit(&self) -> &NeedsImplicit {
        &self.needs_implicit
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

impl FunctionBinding {
    fn decl(&self) -> Cursor {
        self.decl
    }

    fn rename(&self) -> Option<&String> {
        self.rename.as_ref()
    }
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
                    println!("GOT C STYLE CAST");
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
        TypeKind::Using => get_underlying_type(ty.underlying_type()?),
        _ => Ok(ty),
    }
}

fn create_binding(c_compound: Cursor, binding: &mut Binding) {
    c_compound.visit_children(|c, _| {
        // The main "entry point" is UnexposedExpr, which is actually ExprWithCleanups, but this isn't exposed by the C API.
        // TODO(AL): expose this (or watch for exposition later).
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

                                    let mut class_binding = ClassBinding::new(class_decl, rename);
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
                create_binding(c, binding);
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

    Ok(ClassDecl {
        usr: cb.class_decl().usr(),
        name,
        fields: Vec::new(),
        methods: Vec::new(),
        namespaces,
        template_parameters: Vec::new(),
        specializations: Vec::new(),
        ignore: cb.ignore(),
        rename: cb.rename().cloned(),
        bind_kind: cb.bind_kind(),
        specialized_methods: Vec::new(),
        is_pod: false,
        needs_implicit: cb.needs_implicit.clone(),
    })
}

#[allow(clippy::too_many_arguments)]
/// Main recursive function to walk the AST and extract the pieces we're interested in
pub fn extract_ast_from_binding_tu(
    c: Cursor,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    stop_on_error: bool,
) -> Result<()> {
    // first, get the bbl_bind() function
    let bind_fn = c
        .first_child_of_kind_with_name(CursorKind::FunctionDecl, "bbl_bind")
        .expect("Could not find bbl_bind");

    dump_cursor_until(bind_fn, tu, 20, false);

    let body = bind_fn
        .first_child_of_kind(CursorKind::CompoundStmt)
        .expect("Could not find body for bbl_bind");

    let mut binding = Binding::default();
    create_binding(body, &mut binding);

    println!("Got binding: {binding:?}");

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
            .expect("Failed to extract method");

            method.set_replacement_name(rename.clone());

            let class = ast.get_class_mut(cb.class_decl().usr()).unwrap();
            class.methods.push(method);
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
                .collect::<Result<Vec<Argument>>>()?;

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
            class.methods.push(ctor);
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
        function.replacement_name = fn_binding.rename.clone();

        ast.insert_function(function);
    }

    println!("{ast:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use indoc::indoc;

    use crate::{
        bind_string,
        class::{ClassBindKind, OverrideList},
        error::Error,
        parse_string_and_extract_ast, AllowList,
    };

    #[test]
    fn bind_pod() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            // test that a POD extracts as a valuetype
            let ast = bind_string(
                indoc!(
                    r#"
#ifndef BBL_HPP
#define BBL_HPP

#define BBL_MODULE(name) \
    static void bbl_bind(const char *modname = #name)

namespace bbl
{
    template <typename C>
    class Class
    {
    public:
        Class()
        {
        }

        Class(const char* name)
        {
        }

        template <typename Func>
        Class m(Func fn)
        {
            return *this;
        }

        template <typename Func>
        Class m(Func fn, const char *rename)
        {
            return *this;
        }
    };
}

#endif

namespace NS {
struct Foo
{
    int a;
};

template <typename T>
struct Bar
{
    T t;
};

typedef Bar<int> IntBar;
typedef Bar<Foo> FooBar;

class Base
{
public:
    void do_inherited_thing();
};

class Test : public Base
{
    int _a;

public:
    float b;
    void do_thing();

    template <typename T>
    T do_templated_thing(T *);
};
}

BBL_MODULE(Test::Module)
{
    // bbl::Class<NS::Foo>();
    bbl::Class<NS::Bar<NS::Foo>>("FooBar").rename("FooBar");

    // bbl::Class<NS::Test>()
    //     .m(&Test::do_thing)
    //     .m(&Test::do_inherited_thing)
    //     .m(&Test::do_templated_thing<Foo>, "do_templated_thing_foo")
    //     .m(&Test::do_templated_thing<FooBar>, "do_templated_thing_foobar")
    //     .m(&Test::do_templated_thing<int>, "do_templated_thing_int");
}
        "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::default(),
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            assert_eq!(
                format!("{ast:?}"),
                indoc!(
                    r#"

    "#
                )
            );

            Ok(())
        })
    }
}
