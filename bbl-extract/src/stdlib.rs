use bbl_clang::{
    cursor::{Cursor, USR},
    exception::ExceptionSpecificationKind,
    ty::TypeKind,
};

use crate::{
    ast::AST,
    class::{ClassDecl, MethodState, RuleOfFive},
    function::{Argument, Const, PureVirtual, Static, Virtual},
    function::{Method, MethodKind},
    namespace,
    qualtype::{QualType, TypeRef},
    templates::TemplateParameterDecl,
};

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
        ),
    ];

    let mut cd = ClassDecl::new(
        class.usr(),
        "string".to_string(),
        Vec::new(),
        methods,
        vec![u_std],
        // vec![TemplateParameterDecl::typ("_CharT", 0)],
        vec![],
        false,
        RuleOfFive {
            ctor: MethodState::Defined,
            copy_ctor: MethodState::Defined,
            move_ctor: MethodState::Defined,
            copy_assign: MethodState::Undefined,
            move_assign: MethodState::Undefined,
            dtor: MethodState::Defined,
        },
    );

    cd.rename = Some("string".to_string());

    cd
}
