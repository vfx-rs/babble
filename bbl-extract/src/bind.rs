use bbl_clang::{
    cursor::{ChildVisitResult, CurTypedef, Cursor, USR},
    cursor_kind::CursorKind,
    translation_unit::TranslationUnit,
};
use log::debug;
use tracing::error;

use crate::{
    ast::{dump_cursor, dump_cursor_until, get_namespaces_for_decl, AST},
    class::{ClassBindKind, ClassDecl, NeedsImplicit, OverrideList},
    error::Error,
    function::extract_method,
    AllowList,
};
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
struct ClassBinding {
    class_decl: Cursor,
    methods: Vec<Cursor>,
    ignore: bool,
    rename: Option<String>,
    bind_kind: ClassBindKind,
}

impl ClassBinding {
    pub fn new(class_decl: Cursor, rename: Option<String>) -> ClassBinding {
        ClassBinding {
            class_decl,
            methods: Vec::new(),
            ignore: false,
            rename,
            bind_kind: ClassBindKind::OpaquePtr,
        }
    }
}

impl ClassBinding {
    fn class_decl(&self) -> Cursor {
        self.class_decl
    }

    fn methods(&self) -> &[Cursor] {
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

fn extract_class_binding(
    c_expr_with_cleanups: Cursor,
    class_binding: &mut ClassBinding,
) -> Result<()> {
    c_expr_with_cleanups.visit_children(|c, _| {
        match c.kind() {
            CursorKind::CallExpr if c.display_name() == "m" => {
                // Then we get UnaryOperator > DeclRefExpr > CXXMethod
                if let Some(c_uo) = c.first_child_of_kind(CursorKind::UnaryOperator) {
                    if let Some(c_ref_expr) = c_uo.first_child_of_kind(CursorKind::DeclRefExpr) {
                        if let Ok(c_method) = c_ref_expr.referenced() {
                            if c_method.kind() == CursorKind::CXXMethod {
                                class_binding.methods.push(c_method);
                            } else {
                                error!(
                                "got what should have been the CXXMethod but it was {c_method:?}"
                            );
                            }
                        }
                    }
                }

                ChildVisitResult::Recurse
            }
            CursorKind::CallExpr if c.display_name() == "Class" => {
                let children = c.children();
                for child in children {
                    if child.kind() == CursorKind::ImplicitCastExpr {
                        if let Some(c_lit) = child.first_child_of_kind(CursorKind::StringLiteral) {
                            let mut name = c_lit.display_name();
                            // remove first and last chars as they're quotes
                            name.pop();
                            name.remove(0);
                            class_binding.rename = Some(name);
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

fn find_class_bindings(c_compound: Cursor) -> Vec<ClassBinding> {
    let mut result = Vec::new();
    c_compound.visit_children(|c, _| {
        // The "entry point" is UnexposedExpr, which is actually ExprWithCleanups, but this isn't exposed by the C API.
        // TODO(AL): expose this (or watch for exposition later).
        // This might be nested under a VarDecl if the user has done `auto v = bbl::Class<Foo>()`
        // Once we've hit this point, the full expression appears in reverse order, so given:
        //
        // Class<Foo>("Foo")
        //   .m(&Foo::bar)
        //   .m(&Foo::baz)
        //
        // we get m(&Foo::baz) first, then m(&Foo::bar), then the constructor call.
        if matches!(c.kind(), CursorKind::ExprWithCleanups) {
            if let Ok(ty) = c.ty() {
                if ty.spelling().starts_with("Class<") || ty.spelling().starts_with("bbl::Class<") {
                    debug!("Found Class< {ty:?}");
                    let num_template_args = ty.num_template_arguments();
                    if num_template_args == 1 {
                        let tty = ty.template_argument_as_type(0).unwrap();
                        let mut rename = None;
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
                                result.push(class_binding);

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
        }
        ChildVisitResult::Recurse
    });

    result
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
        needs_implicit: NeedsImplicit::default(),
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

    let class_bindings = find_class_bindings(body);

    println!("Got classes: {class_bindings:?}");

    let allow_list = AllowList::default();
    let overrides = OverrideList::default();

    // first extract the classes to fill the AST with types so we favour the users extractions
    for cb in &class_bindings {
        let class = bind_class(cb, tu, ast, already_visited)?;
        ast.insert_class(class);
    }

    // now extract their methods
    for cb in &class_bindings {
        for c_method in cb.methods() {
            let method = extract_method(
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

            let class = ast.get_class_mut(cb.class_decl().usr()).unwrap();
            class.methods.push(method);
        }
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
