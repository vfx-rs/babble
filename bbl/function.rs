use log::*;
use std::fmt::Display;

use crate::cursor_kind::CursorKind;
use crate::qualtype::{extract_type, extract_type_from_typeref};
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use crate::{ast::AST, cursor::USR, qualtype::QualType, Cursor};

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Argument {
    pub(crate) name: String,
    pub(crate) qual_type: QualType,
}

impl Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.qual_type)
    }
}

impl Argument {
    fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        format!(
            "{}: {}",
            self.name,
            self.qual_type
                .format(ast, class_template_parameters, class_template_args)
        )
    }
}

pub struct Function {
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) result: QualType,
    pub(crate) arguments: Vec<Argument>,
    pub(crate) replacement_name: Option<String>,
    pub(crate) ignored: bool,
}

impl Function {
    pub fn new(usr: USR, name: String, result: QualType, arguments: Vec<Argument>, replacement_name: Option<String>) -> Self {
        Function {
            usr,
            name,
            result,
            arguments,
            replacement_name,
            ignored: false,
        }
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn rename(&mut self, new_name: &str) {
        self.replacement_name = Some(new_name.to_string());
    }

    pub fn ignore(&mut self) {
        self.ignored = true;
    }

    pub fn format(
        &self,
        ast: &AST,
        template_parameters: &[TemplateParameterDecl],
        template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        let mut s = String::new();
        s += &self.name;

        let args = self
            .arguments
            .iter()
            .map(|p| p.format(ast, template_parameters, template_args))
            .collect::<Vec<String>>()
            .join(", ");
        s += &format!("({})", args);

        s += &format!(
            " -> {}",
            self.result.format(ast, template_parameters, template_args)
        );

        s
    }
}

pub struct Method {
    pub(crate) function: Function,
    pub(crate) is_const: bool,
    pub(crate) is_static: bool,
    pub(crate) is_virtual: bool,
    pub(crate) is_pure_virtual: bool,
}

impl Method {
    pub fn new(
        usr: USR,
        name: String,
        result: QualType,
        arguments: Vec<Argument>,
        rename: Option<String>,
        is_const: bool,
        is_static: bool,
        is_virtual: bool,
        is_pure_virtual: bool,
    ) -> Self {
        Method {
            function: Function::new(usr, name, result, arguments, rename),
            is_const,
            is_static,
            is_virtual,
            is_pure_virtual,
        }
    }

    pub fn usr(&self) -> USR {
        self.function.usr
    }

    pub fn name(&self) -> &str {
        &self.function.name
    }

    pub fn rename(&mut self, new_name: &str) {
        self.function.rename(new_name);
    }

    pub fn ignore(&mut self) {
        self.function.ignore();
    }

    pub fn signature(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        self.format(ast, class_template_parameters, class_template_args)
    }

    pub fn format(
        &self,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) -> String {
        let mut s = self
            .function
            .format(ast, class_template_parameters, class_template_args);

        if self.is_static {
            s += " static"
        };

        if self.is_virtual {
            s += " virtual"
        };

        if self.is_const {
            s += " const"
        };

        if self.is_pure_virtual {
            s += " = 0"
        };

        s
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        class_template_parameters: &[TemplateParameterDecl],
        class_template_args: Option<&[Option<TemplateType>]>,
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let s = self.format(ast, class_template_parameters, class_template_args);

        let mut modify_attrs = Vec::new();
        if self.function.ignored {
            modify_attrs.push("ignored".to_string());
        }

        if let Some(new_name) = &self.function.replacement_name {
            modify_attrs.push(format!("rename(\"{new_name}\")"));
        }

        let attr_string = if modify_attrs.is_empty() {
            String::new()
        } else {
            format!("    [[{}]]", modify_attrs.join(", "))
        };

        println!("{indent}{s}{attr_string}");

    }
}

pub fn extract_argument(c_arg: Cursor, template_parameters: &[String]) -> Result<Argument> {
    let children = c_arg.children();

    let ty = c_arg.ty()?;

    let qual_type = if ty.is_builtin() || ty.is_pointer() {
        extract_type(ty, template_parameters)?
    } else if !children.is_empty() {
        match children[0].kind() {
            CursorKind::TypeRef | CursorKind::TemplateRef => {
                extract_type_from_typeref(children[0])?
            }
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

pub fn extract_method(
    c_method: Cursor,
    depth: usize,
    class_template_parameters: &[TemplateParameterDecl],
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
        .map(|t| t.name().to_string())
        .chain(c_template_parameters.iter().map(|c| c.display_name()))
        .collect::<Vec<_>>();

    let mut skip = c_template_parameters.len();

    let ty_result = c_method.result_ty().unwrap();
    let result = if ty_result.is_builtin() || ty_result.is_pointer() {
        extract_type(ty_result, &template_parameters)?
    } else {
        let c_result = children.iter().skip(skip).next().expect(&format!(
            "Could not get result cursor from {}",
            c_method.display_name()
        ));

        skip += 1;

        if c_result.kind() == CursorKind::TypeRef || c_result.kind() == CursorKind::TemplateRef {
            extract_type_from_typeref(c_result.clone())?
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

        arguments.push(extract_argument(c_arg.clone(), &template_parameters)?);
    }

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

    let replacement_name = get_default_replacement_name(c_method);

    Ok(Method::new(
        c_method.usr(),
        c_method.spelling(),
        result,
        arguments,
        replacement_name,
        c_method.cxx_method_is_const(),
        c_method.cxx_method_is_static(),
        c_method.cxx_method_is_virtual(),
        c_method.cxx_method_is_pure_virtual(),
    ))
}

fn get_default_replacement_name(c_method: Cursor) -> Option<String> {
    match c_method.kind() {
        CursorKind::Constructor => {
            if c_method.cxx_constructor_is_copy_constructor() {
                Some("copy_ctor".into())
            } else if c_method.cxx_constructor_is_move_constructor() {
                Some("move_ctor".into())
            } else if c_method.cxx_constructor_is_move_constructor() {
                Some("default_ctor".into())
            } else {
                Some("ctor".into())
            }
        }
        CursorKind::Destructor => Some("dtor".into()),
        _ => if c_method.display_name().contains("operator+=") {
            Some("op_add_assign".into())
        } else if c_method.display_name().contains("operator+") {
            Some("op_add".into())
        } else if c_method.display_name().contains("operator-=") {
            Some("op_sub_assign".into())
        } else if c_method.display_name().contains("operator-") && c_method.num_arguments().unwrap() == 1 {
            Some("op_sub".into())
        } else if c_method.display_name().contains("operator-") && c_method.num_arguments().unwrap() == 0 {
            Some("op_neg".into())
        } else if c_method.display_name().contains("operator*=") {
            Some("op_mul_assign".into())
        } else if c_method.display_name().contains("operator*") {
            Some("op_mul".into())
        } else if c_method.display_name().contains("operator/=") {
            Some("op_div_assign".into())
        } else if c_method.display_name().contains("operator/") {
            Some("op_div".into())
        } else if c_method.display_name().contains("operator&=") {
            Some("op_bitand_assign".into())
        } else if c_method.display_name().contains("operator&") {
            Some("op_bitand".into())
        } else if c_method.display_name().contains("operator|=") {
            Some("op_bitor_assign".into())
        } else if c_method.display_name().contains("operator|") {
            Some("op_bitor".into())
        } else if c_method.display_name().contains("operator^=") {
            Some("op_bitxor_assign".into())
        } else if c_method.display_name().contains("operator^") {
            Some("op_bitxor".into())
        } else if c_method.display_name().contains("operator%=") {
            Some("op_rem_assign".into())
        } else if c_method.display_name().contains("operator%") {
            Some("op_rem".into())
        } else if c_method.display_name().contains("operator<<=") {
            Some("op_shl_assign".into())
        } else if c_method.display_name().contains("operator<<") {
            Some("op_shl".into())
        } else if c_method.display_name().contains("operator>>=") {
            Some("op_shr_assign".into())
        } else if c_method.display_name().contains("operator>>") {
            Some("op_shr".into())
        } else if c_method.display_name().contains("operator==") {
            Some("op_eq".into())
        } else if c_method.display_name().contains("operator=") {
            Some("op_assign".into())
        } else if c_method.display_name().contains("operator!") {
            Some("op_not".into())
        } else if c_method.display_name().contains("operator[]") {
            Some("op_index".into())
        } else {
            None
        }
    }
}
