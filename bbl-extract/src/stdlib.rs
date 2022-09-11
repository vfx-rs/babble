use bbl_clang::{
    cursor::{Cursor, USR},
    exception::ExceptionSpecificationKind,
};

use crate::{
    class::{ClassDecl, RuleOfFive, MethodState},
    function::Argument,
    function::{Method, MethodKind},
    namespace,
    qualtype::{QualType, TypeRef},
    template_argument::TemplateParameterDecl,
};

pub fn create_std_string(class: Cursor, namespaces: Vec<USR>) -> ClassDecl {
    let mut method_namespaces = namespaces.clone();
    method_namespaces.push(class.usr());

    let methods = vec![
        Method::new(
            USR::new("basic_string_ctor"),
            "string".to_string(),
            MethodKind::Constructor,
            QualType::void(),
            Vec::new(),
            Some("ctor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            false,
            false,
            false,
            false,
        ),
        Method::new(
            USR::new("basic_string_dtor"),
            "~string".to_string(),
            MethodKind::Destructor,
            QualType::void(),
            Vec::new(),
            Some("dtor".to_string()),
            method_namespaces.clone(),
            Vec::new(),
            ExceptionSpecificationKind::None,
            false,
            false,
            false,
            false,
        ),
        Method::new(
            USR::new("basic_string_copy_ctor"),
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
            false,
            false,
            false,
            false,
        ),
        Method::new(
            USR::new("basic_string_move_ctor"),
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
            false,
            false,
            false,
            false,
        ),
    ];

    ClassDecl::new(
        class.usr(),
        class.spelling(),
        Vec::new(),
        methods,
        namespaces,
        vec![TemplateParameterDecl::typ("_CharT", 0)],
        false,
        RuleOfFive {
            ctor: MethodState::Defined,
            copy_ctor: MethodState::Defined,
            move_ctor: MethodState::Defined,
            copy_assign: MethodState::Undefined,
            move_assign: MethodState::Undefined,
            dtor: MethodState::Defined,
        }
    )
}
