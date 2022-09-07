use bbl_clang::{cursor::{USR, Cursor}, exception::ExceptionSpecificationKind};

use crate::{class::ClassDecl, function::{Method, MethodKind}, namespace, qualtype::{QualType, TypeRef}, template_argument::TemplateParameterDecl};

pub fn create_std_string(class: Cursor, namespaces: Vec<USR>) -> ClassDecl {

    let mut method_namespaces = namespaces.clone();
    method_namespaces.push(class.usr());

    let methods = vec![Method::new(
        USR::new("basic_string_ctor"),
        "ctor".to_string(),
        MethodKind::Constructor,
        QualType {
            name: "[result]".to_string(),
            is_const: false,
            type_ref: TypeRef::Ref(class.usr())
        },
        Vec::new(),
        Some("ctor".to_string()),
        method_namespaces,
        Vec::new(),
        ExceptionSpecificationKind::None,
        false,
        false,
        false,
        false,
    )];

    ClassDecl::new(
        class.usr(),
        class.spelling(),
        Vec::new(),
        methods,
        namespaces,
        vec![TemplateParameterDecl::typ("_CharT", 0)],
        false,
    )
}
