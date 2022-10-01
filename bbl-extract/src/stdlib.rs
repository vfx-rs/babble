use bbl_clang::{
    access_specifier::AccessSpecifier,
    cursor::{CurClassDecl, Cursor, USR},
    exception::ExceptionSpecificationKind,
    translation_unit::TranslationUnit,
    ty::TypeKind,
};

use crate::{
    ast::{get_namespaces_for_decl, AST},
    class::{ClassDecl, MethodState, RuleOfFive},
    function::{Argument, Const, PureVirtual, Static, Virtual},
    function::{Deleted, Method, MethodKind},
    namespace,
    qualtype::{extract_type, QualType, TypeRef},
    templates::{
        extract_class_template_specialization, ClassTemplateSpecialization, TemplateArgument,
        TemplateParameterDecl,
    },
    AllowList,
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub fn create_std_string(class: Cursor, ast: &AST) -> ClassDecl {
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
            Static(false),
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
            Static(false),
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
            Static(false),
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
            Static(false),
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
            Static(false),
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
            Static(false),
            Virtual(false),
            PureVirtual(false),
            Deleted(false),
        ),
    ];

    ClassDecl::new(
        class.usr(),
        "string".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        // vec![TemplateParameterDecl::typ("_CharT", 0)],
        vec![],
        false,
        RuleOfFive {
            ctor: MethodState::Defined(AccessSpecifier::Public),
            copy_ctor: MethodState::Defined(AccessSpecifier::Public),
            move_ctor: MethodState::Defined(AccessSpecifier::Public),
            copy_assign: MethodState::Undefined,
            move_assign: MethodState::Undefined,
            dtor: MethodState::Defined(AccessSpecifier::Public),
        },
    )
}

pub fn create_std_vector(
    c: CurClassDecl,
    ast: &mut AST,
    already_visited: &mut Vec<USR>,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<USR> {
    let c_tmpl = c.specialized_template().unwrap();
    let usr_tmpl = create_std_vector_tmpl(c_tmpl, ast, tu, already_visited)?;

    let name = regex::Regex::new("(?:[^a-zA-Z0-9])+")
        .unwrap()
        .replace_all(&c.display_name(), "_")
        .to_string();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty = c.template_argument_type(0)?;
    let template_arguments = vec![TemplateArgument::Type(extract_type(
        ty,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
    )?)];

    let cts = ClassTemplateSpecialization {
        specialized_decl: usr_tmpl,
        name,
        usr: c.usr(),
        namespaces,
        template_arguments,
    };

    let id = ast.insert_class_template_specialization(cts);
    let cd = ast.get_class_mut(usr_tmpl).unwrap();
    cd.specializations.push(id);

    Ok(c.usr())
}

fn create_std_vector_tmpl(
    c_tmpl: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    // get the namespaces for std::vector<> as we might not have found them already
    let namespaces = get_namespaces_for_decl(c_tmpl, tu, ast, already_visited)?;

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
            Static(false),
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
            Static(false),
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
            Static(false),
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
            Static(false),
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
            Static(false),
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
        RuleOfFive {
            ctor: MethodState::Defined(AccessSpecifier::Public),
            copy_ctor: MethodState::Undefined,
            move_ctor: MethodState::Undefined,
            copy_assign: MethodState::Undefined,
            move_assign: MethodState::Undefined,
            dtor: MethodState::Undefined,
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
) -> Result<USR> {
    let c_tmpl = c.specialized_template().unwrap();
    let usr_tmpl = create_std_unique_ptr_tmpl(c_tmpl, ast, tu, already_visited)?;

    let name = regex::Regex::new("(?:[^a-zA-Z0-9])+")
        .unwrap()
        .replace_all(&c.display_name(), "_")
        .to_string();

    let namespaces = get_namespaces_for_decl(c.into(), tu, ast, already_visited)?;
    let ty = c.template_argument_type(0)?;
    let template_arguments = vec![TemplateArgument::Type(extract_type(
        ty,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
    )?)];

    let cts = ClassTemplateSpecialization {
        specialized_decl: usr_tmpl,
        name,
        usr: c.usr(),
        namespaces,
        template_arguments,
    };

    let id = ast.insert_class_template_specialization(cts);
    let cd = ast.get_class_mut(usr_tmpl).unwrap();
    cd.specializations.push(id);

    Ok(c.usr())
}

fn create_std_unique_ptr_tmpl(
    c_tmpl: Cursor,
    ast: &mut AST,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
) -> Result<USR> {
    // get the namespaces for std::vector<> as we might not have found them already
    let namespaces = get_namespaces_for_decl(c_tmpl, tu, ast, already_visited)?;

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
            Static(false),
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
            Static(false),
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
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            Const(false),
            Static(false),
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
        RuleOfFive {
            ctor: MethodState::Defined(AccessSpecifier::Public),
            copy_ctor: MethodState::Undefined,
            move_ctor: MethodState::Undefined,
            copy_assign: MethodState::Undefined,
            move_assign: MethodState::Undefined,
            dtor: MethodState::Undefined,
        },
    );

    ast.insert_class(cd);

    Ok(c_tmpl.usr())
}
