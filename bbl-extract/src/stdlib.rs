use bbl_clang::{
    cursor::{CurClassDecl, Cursor, USR},
    exception::ExceptionSpecificationKind,
    translation_unit::TranslationUnit,
    ty::TypeKind,
};

use crate::{
    ast::{get_namespaces_for_decl, AST},
    class::{ClassDecl, NeedsImplicit, OverrideList},
    function::{Argument, Const, PureVirtual, Virtual},
    function::{Deleted, FunctionProto, Method, MethodKind},
    qualtype::{extract_type, QualType, TypeRef},
    templates::{
        specialize_class_template, ClassTemplateSpecialization, TemplateArgument,
        TemplateParameterDecl,
    },
    AllowList,
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub fn create_std_string(
    class: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    if already_visited.contains(&class.usr()) {
        return Ok(class.usr());
    } else {
        already_visited.push(class.usr());
    }

    let _ = get_namespaces_for_decl(class, tu, ast, already_visited)?; // just to make sure they're in the AST
    let u_std = ast
        .find_namespace("std")
        .map(|id| ast.namespaces()[id].usr())
        .unwrap();

    let method_namespaces = vec![u_std, class.usr()];

    let methods = vec![
        Method::new(
            USR::new("BBL:basic_string_ctor"),
            "string".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            Vec::new(),
            Some("ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:basic_string_ctor_char_ptr"),
            "string".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            vec![Argument::new(
                "char_ptr",
                QualType::pointer("const char*", QualType::char(true)),
            )],
            Some("from_char_ptr".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:basic_string_dtor"),
            "~string".to_string(),
            MethodKind::Destructor,
            QualType::void(),
            Vec::new(),
            Some("dtor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:basic_string_copy_ctor"),
            "string".to_string(),
            MethodKind::CopyConstructor,
            QualType::void(),
            vec![Argument::new(
                "other",
                QualType::lvalue_reference(
                    "std::string&",
                    QualType::type_ref("const std::string", true, class.usr()),
                ),
            )],
            Some("copy_ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:basic_string_move_ctor"),
            "string".to_string(),
            MethodKind::MoveConstructor,
            QualType::void(),
            vec![Argument::new(
                "other",
                QualType::rvalue_reference(
                    "std::string&&",
                    QualType::type_ref("std::string", false, class.usr()),
                ),
            )],
            Some("move_ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:basic_string_c_str"),
            "c_str".to_string(),
            MethodKind::Method,
            QualType {
                name: "char const*".to_string(),
                is_const: false,
                type_ref: TypeRef::Pointer(Box::new(QualType {
                    name: "char const".to_string(),
                    is_const: true,
                    type_ref: TypeRef::Builtin(TypeKind::Char_S),
                })),
            },
            vec![],
            Some("c_str".to_string()),
            method_namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(true),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
    ];

    let cd = ClassDecl::new(
        class.usr(),
        "string".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        // vec![TemplateParameterDecl::typ("_CharT", 0)],
        vec![],
        false,
        NeedsImplicit::default(),
    );

    ast.insert_class(cd);
    Ok(class.usr())
}

pub fn create_std_vector(
    c: CurClassDecl,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    specialize_immediately: bool,
    stop_on_error: bool,
) -> Result<USR> {
    if already_visited.contains(&c.usr()) {
        return Ok(c.usr());
    } else {
        already_visited.push(c.usr());
    }

    let c_tmpl = c.specialized_template().unwrap();
    let usr_tmpl = create_std_vector_tmpl(c_tmpl, ast, tu, already_visited)?;

    let name = c.display_name();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty = c.template_argument_type(0)?;
    let template_arguments = vec![TemplateArgument::Type(extract_type(
        ty,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?)];

    let cts = ClassTemplateSpecialization::new(
        usr_tmpl,
        c.usr(),
        &name,
        template_arguments.clone(),
        namespaces,
        NeedsImplicit {
            copy_ctor: true,
            move_ctor: true,
            dtor: true,
            ..Default::default()
        },
        false,
    );

    let cd = ast.get_class_mut(usr_tmpl).unwrap();
    cd.add_specialization(template_arguments, c.usr());

    if specialize_immediately {
        let cd = ast.get_class(usr_tmpl).unwrap();
        let sd = specialize_class_template(cd, &cts, ast)?;
        ast.insert_class(sd);
    }

    let _ = ast.insert_class_template_specialization(cts);

    Ok(c.usr())
}

fn create_std_vector_tmpl(
    c_tmpl: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    if already_visited.contains(&c_tmpl.usr()) {
        return Ok(c_tmpl.usr());
    } else {
        already_visited.push(c_tmpl.usr());
    }

    // get the namespaces for std::vector<> as we might not have found them already
    let _ = get_namespaces_for_decl(c_tmpl, tu, ast, already_visited)?;

    let u_std = ast
        .find_namespace("std")
        .map(|id| ast.namespaces()[id].usr())
        .unwrap();

    let method_namespaces = vec![u_std, c_tmpl.usr()];

    let methods = vec![
        Method::new(
            USR::new("BBL:vector_ctor_default"),
            "vector".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            Vec::new(),
            Some("ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:vector_ctor_pointers"),
            "vector".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            vec![
                Argument::new(
                    "begin",
                    QualType::pointer("const T *", QualType::template_parameter("T", "T", true)),
                ),
                Argument::new(
                    "end",
                    QualType::pointer("const T *", QualType::template_parameter("T", "T", true)),
                ),
            ],
            Some("from_begin_and_end".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:vector_data_const"),
            "data".to_string(),
            MethodKind::Method,
            QualType::pointer("const T *", QualType::template_parameter("T", "T", true)),
            vec![],
            Some("data".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(true),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:vector_data_mut"),
            "data".to_string(),
            MethodKind::Method,
            QualType::pointer("T *", QualType::template_parameter("T", "T", false)),
            vec![],
            Some("data_mut".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:vector_size"),
            "size".to_string(),
            MethodKind::Method,
            QualType::builtin(TypeKind::ULongLong, false),
            vec![],
            None,
            method_namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(true),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
    ];

    let cd = ClassDecl::new(
        c_tmpl.usr(),
        "vector".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        vec![TemplateParameterDecl::typ("T", 0)],
        false,
        NeedsImplicit {
            copy_ctor: true,
            move_ctor: true,
            dtor: true,
            ..Default::default()
        },
    );

    ast.insert_class(cd);

    Ok(c_tmpl.usr())
}

pub fn create_std_unique_ptr(
    c: CurClassDecl,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    specialize_immediately: bool,
    stop_on_error: bool,
) -> Result<USR> {
    if already_visited.contains(&c.usr()) {
        return Ok(c.usr());
    } else {
        already_visited.push(c.usr());
    }

    let c_tmpl = c.specialized_template().unwrap();
    let usr_tmpl = create_std_unique_ptr_tmpl(c_tmpl, ast, tu, already_visited)?;

    let name = c.display_name();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty = c.template_argument_type(0)?;
    let template_arguments = vec![TemplateArgument::Type(extract_type(
        ty,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?)];

    let cts = ClassTemplateSpecialization::new(
        usr_tmpl,
        c.usr(),
        &name,
        template_arguments.clone(),
        namespaces,
        NeedsImplicit {
            move_ctor: true,
            move_assign: true,
            dtor: true,
            ..Default::default()
        },
        false,
    );

    let cd = ast.get_class_mut(usr_tmpl).unwrap();
    cd.add_specialization(template_arguments, c.usr());

    if specialize_immediately {
        let cd = ast.get_class(usr_tmpl).unwrap();
        let sd = specialize_class_template(cd, &cts, ast)?;
        ast.insert_class(sd);
    }

    let _ = ast.insert_class_template_specialization(cts);

    Ok(c.usr())
}

fn create_std_unique_ptr_tmpl(
    c_tmpl: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    if already_visited.contains(&c_tmpl.usr()) {
        return Ok(c_tmpl.usr());
    } else {
        already_visited.push(c_tmpl.usr());
    }

    // get the namespaces for std::vector<> as we might not have found them already
    let _ = get_namespaces_for_decl(c_tmpl, tu, ast, already_visited)?;

    let u_std = ast
        .find_namespace("std")
        .map(|id| ast.namespaces()[id].usr())
        .unwrap();

    let method_namespaces = vec![u_std, c_tmpl.usr()];

    let methods = vec![
        Method::new(
            USR::new("BBL:unique_ptr_ctor_default"),
            "unique_ptr".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            Vec::new(),
            Some("ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:unique_ptr_get_const"),
            "get".to_string(),
            MethodKind::Method,
            QualType::pointer("const T *", QualType::template_parameter("T", "T", true)),
            vec![],
            Some("get".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(true),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:unique_ptr_get_mut"),
            "get".to_string(),
            MethodKind::Method,
            QualType::pointer("T *", QualType::template_parameter("T", "T", false)),
            vec![],
            Some("get_mut".to_string()),
            method_namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
    ];

    let cd = ClassDecl::new(
        c_tmpl.usr(),
        "unique_ptr".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        vec![TemplateParameterDecl::typ("T", 0)],
        false,
        NeedsImplicit {
            dtor: true,
            ..Default::default()
        },
    );

    ast.insert_class(cd);

    Ok(c_tmpl.usr())
}

pub fn create_std_function(
    c: CurClassDecl,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<USR> {
    if already_visited.contains(&c.usr()) {
        return Ok(c.usr());
    } else {
        already_visited.push(c.usr());
    }

    // we'll just explicitly convert this to a function prototype here and not try and extract the underlying template
    // as that requires handling parameter packs etc.
    let name = regex::Regex::new("(?:[^a-zA-Z0-9])+")
        .unwrap()
        .replace_all(&c.display_name(), "_")
        .to_string();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty = c.template_argument_type(0)?;

    if ty.kind() != TypeKind::FunctionProto {
        panic!(
            "Got type kind {:?} instead of FunctionProto for {c:?}",
            ty.kind()
        );
    }

    let result = extract_type(
        ty.result_type()?,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;
    let num_args = ty.num_arg_types()?;
    let mut args = Vec::new();
    for i in 0..num_args {
        args.push(extract_type(
            ty.arg_type(i)?,
            &[],
            already_visited,
            ast,
            tu,
            allow_list,
            class_overrides,
            stop_on_error,
        )?);
    }

    ast.insert_function_proto(FunctionProto::new(name, c.usr(), result, args, namespaces));

    Ok(c.usr())
}

pub fn create_std_map(
    c: CurClassDecl,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    specialize_immediately: bool,
    stop_on_error: bool,
) -> Result<USR> {
    if already_visited.contains(&c.usr()) {
        return Ok(c.usr());
    } else {
        already_visited.push(c.usr());
    }

    let c_tmpl = c.specialized_template().unwrap();
    let usr_tmpl = create_std_map_tmpl(c_tmpl, ast, tu, already_visited)?;

    let name = c.display_name();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty_key = c.template_argument_type(0)?;
    let ty_t = c.template_argument_type(1)?;
    let template_arguments = vec![
        TemplateArgument::Type(extract_type(
            ty_key,
            &[],
            already_visited,
            ast,
            tu,
            allow_list,
            class_overrides,
            stop_on_error,
        )?),
        TemplateArgument::Type(extract_type(
            ty_t,
            &[],
            already_visited,
            ast,
            tu,
            allow_list,
            class_overrides,
            stop_on_error,
        )?),
    ];

    let cts = ClassTemplateSpecialization::new(
        usr_tmpl,
        c.usr(),
        &name,
        template_arguments.clone(),
        namespaces,
        NeedsImplicit::all(),
        false,
    );

    let cd = ast.get_class_mut(usr_tmpl).unwrap();
    cd.add_specialization(template_arguments, c.usr());

    if specialize_immediately {
        let cd = ast.get_class(usr_tmpl).unwrap();
        let sd = specialize_class_template(cd, &cts, ast)?;
        ast.insert_class(sd);
    }

    let _ = ast.insert_class_template_specialization(cts);

    Ok(c.usr())
}

fn create_std_map_tmpl(
    c_tmpl: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    if already_visited.contains(&c_tmpl.usr()) {
        return Ok(c_tmpl.usr());
    } else {
        already_visited.push(c_tmpl.usr());
    }

    // get the namespaces for std::vector<> as we might not have found them already
    let _ = get_namespaces_for_decl(c_tmpl, tu, ast, already_visited)?;

    let u_std = ast
        .find_namespace("std")
        .map(|id| ast.namespaces()[id].usr())
        .unwrap();

    let method_namespaces = vec![u_std, c_tmpl.usr()];

    let methods = vec![
        Method::new(
            USR::new("BBL:map_ctor_default"),
            "map".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            Vec::new(),
            Some("ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:map_at_const"),
            "at".to_string(),
            MethodKind::Method,
            QualType::lvalue_reference("const T &", QualType::template_parameter("T", "T", true)),
            vec![Argument::new(
                "key",
                QualType::lvalue_reference(
                    "const Key &",
                    QualType::template_parameter("Key", "Key", true),
                ),
            )],
            Some("at".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(true),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
        Method::new(
            USR::new("BBL:map_at_mut"),
            "at".to_string(),
            MethodKind::Method,
            QualType::lvalue_reference("T &", QualType::template_parameter("T", "T", false)),
            vec![Argument::new(
                "key",
                QualType::lvalue_reference(
                    "const Key &",
                    QualType::template_parameter("Key", "Key", true),
                ),
            )],
            Some("at_mut".to_string()),
            method_namespaces,
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
    ];

    let cd = ClassDecl::new(
        c_tmpl.usr(),
        "map".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        vec![
            TemplateParameterDecl::typ("Key", 0),
            TemplateParameterDecl::typ("T", 1),
        ],
        false,
        NeedsImplicit {
            dtor: true,
            ..Default::default()
        },
    );

    ast.insert_class(cd);

    Ok(c_tmpl.usr())
}

#[cfg(test)]
mod tests {
    use crate::class::OverrideList;
    use crate::parse_string_and_extract_ast;
    use crate::AllowList;
    use bbl_clang::cli_args;
    use indoc::indoc;

    #[test]
    fn extract_vector() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let mut ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                #include <vector>

                namespace Test_1_0 {
                class Class {
                    float c;
                public:
                };

                typedef std::vector<Class> ClassVector;
                }
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec!["^Test_1_0".to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ns = ast.find_namespace("Test_1_0")?;
            ast.rename_namespace(ns, "Test");

            let ast = ast.monomorphize()?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Include { name: "vector", bracket: "<" }
                    Namespace c:@N@Test_1_0 Test_1_0 Some("Test")
                    Namespace c:@N@std std None
                    ClassDecl c:@N@Test_1_0@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@Test_1_0]

                    ClassDecl c:@N@std@ST>2#T#T@vector vector rename=None OpaquePtr is_pod=false ignore=false needs=[cctor mctor dtor ] template_parameters=[Type(T)] specializations=[([Test_1_0::Class], c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_)] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_ctor_default vector rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_ctor_pointers vector rename=Some("from_begin_and_end") ignore=false return=void args=[begin: const T *, end: const T *] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:vector_data_const data rename=Some("data") ignore=false return=const T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_data_mut data rename=Some("data_mut") ignore=false return=T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:vector_size size rename=None ignore=false return=ULongLong args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]

                    ClassDecl c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ vector<Test_1_0::Class> rename=None OpaquePtr is_pod=false ignore=false needs=[cctor mctor dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_ctor_default vector rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_ctor_pointers vector rename=Some("from_begin_and_end") ignore=false return=void args=[begin: Test_1_0::Class*, end: Test_1_0::Class*] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:vector_data_const data rename=Some("data") ignore=false return=Test_1_0::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:vector_data_mut data rename=Some("data_mut") ignore=false return=Test_1_0::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:vector_size size rename=None ignore=false return=ULongLong args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@vector]

                    TypeAlias ClassVector = std::vector<Class>
                    ClassTemplateSpecialization c:@N@std@S@vector>#$@N@Test_1_0@S@Class#$@N@std@S@allocator>#S0_ vector<Test_1_0::Class> specialized_decl=c:@N@std@ST>2#T#T@vector template_arguments=[Test_1_0::Class] namespaces=[c:@N@std]
                "#
                ),
            )
        })
    }

    #[test]
    fn extract_unique_ptr() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let mut ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    #include <memory>

                    namespace Test_1_0 {
                    class Class {
                        float c;
                    public:
                    };

                    typedef std::unique_ptr<Class> ClassPtr;
                    }
                    "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec!["^Test_1_0".to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ns = ast.find_namespace("Test_1_0")?;
            ast.rename_namespace(ns, "Test");

            let ast = ast.monomorphize()?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Include { name: "memory", bracket: "<" }
                    Namespace c:@N@Test_1_0 Test_1_0 Some("Test")
                    Namespace c:@N@std std None
                    ClassDecl c:@N@Test_1_0@S@Class Class rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@Test_1_0]

                    ClassDecl c:@N@std@ST>2#T#T@unique_ptr unique_ptr rename=None OpaquePtr is_pod=false ignore=false needs=[dtor ] template_parameters=[Type(T)] specializations=[([Test_1_0::Class], c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_)] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_ctor_default unique_ptr rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_const get rename=Some("get") ignore=false return=const T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_mut get rename=Some("get_mut") ignore=false return=T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]

                    ClassDecl c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_ unique_ptr<Test_1_0::Class> rename=None OpaquePtr is_pod=false ignore=false needs=[mctor mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_ctor_default unique_ptr rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_const get rename=Some("get") ignore=false return=Test_1_0::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_mut get rename=Some("get_mut") ignore=false return=Test_1_0::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]

                    TypeAlias ClassPtr = std::unique_ptr<Class>
                    ClassTemplateSpecialization c:@N@std@S@unique_ptr>#$@N@Test_1_0@S@Class#$@N@std@S@default_delete>#S0_ unique_ptr<Test_1_0::Class> specialized_decl=c:@N@std@ST>2#T#T@unique_ptr template_arguments=[Test_1_0::Class] namespaces=[c:@N@std]
                    "#
                ),
            )
        })
    }

    #[test]
    fn extract_std_function() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let mut ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                #include <functional>

                namespace Test_1_0 {
                using PropertyPredicateFunc = std::function<bool(const char* name)>;

                void take_function(const PropertyPredicateFunc& predicate = {});
                }
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec!["^Test_1_0".to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ns = ast.find_namespace("Test_1_0")?;
            ast.rename_namespace(ns, "Test");

            let ast = ast.monomorphize()?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Include { name: "functional", bracket: "<" }
                    Namespace c:@N@Test_1_0 Test_1_0 Some("Test")
                    Function c:@N@Test_1_0@F@take_function#&1$@N@std@S@function>#Fb(#*1C)# take_function rename=None ignore=false return=void args=[predicate: PropertyPredicateFunc const] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@Test_1_0]
                    TypeAlias PropertyPredicateFunc = bool (const char *)
                "#
                ),
            )
        })
    }
}
