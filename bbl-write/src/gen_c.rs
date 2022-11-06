use bbl_clang::{cursor::USR, ty::TypeKind};
use bbl_extract::{ast::AST, class::ClassBindKind};
use bbl_translate::{
    cenum::CEnum,
    cfunction::{CFunction, CFunctionProto, Expr},
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    ctypedef::CTypedef,
    Entity, CAST,
};
use bbl_util::Trace;

use crate::error::Error;
use petgraph::{
    algo::{min_spanning_tree, tarjan_scc, toposort},
    data::Element,
    data::FromElements,
    dot::Dot,
    graphmap::DiGraphMap,
    prelude::DiGraph,
    visit::{depth_first_search, Control, DfsEvent},
};
use tracing::instrument;

use std::{fmt::Write, ops::Deref};

type Result<T, E = Error> = std::result::Result<T, E>;

/// Generate the c library code (itself c++ but with a c header)
///
/// # Returns
/// A tuple containing two [`String`]s, the first of which is the header source, the second is the implementation source
#[instrument(level = "trace", skip(c_ast))]
pub fn gen_c(module_name: &str, c_ast: &CAST) -> Result<(String, String)> {
    let module_name_upper = module_name.to_uppercase().replace('-', "_");
    let mut header = format!("#ifndef __{}_H__\n", module_name_upper);
    header = format!("{header}#define __{}_H__\n\n", module_name_upper);
    header = format!("{header}#ifdef __cplusplus\nextern \"C\" {{\n#endif\n\n");

    let mut source = c_ast
        .includes
        .iter()
        .map(|i| i.get_statement())
        .collect::<Vec<_>>()
        .join("\n");
    // let mut source = String::new();

    write!(
        &mut source,
        r#"{}/* Inserted from binding includes */
{}
/* End insert from binding includes */
"#,
        "\n\n", c_ast.header_string
    )?;
    writeln!(&mut source, "\n\n#include <utility>")?;
    writeln!(&mut source, "#include <exception>\n")?;

    let ast_graph = create_ast_graph(c_ast);

    // for node in min_spanning_tree(&ast_graph) {
    //     if let Element::Node { weight } = node {
    //         if weight == USR::new("__ROOT__") {
    //             continue;
    //         }
    // let entity = c_ast.get_entity(weight).unwrap();

    // for node in tarjan_scc(&ast_graph).into_iter().flatten() {
    // for node in toposort(&ast_graph, None).unwrap() {
    //     if node == USR::new("__ROOT__") {
    //         continue;
    //     }

    let u_root = USR::new("__ROOT__");

    // do a first pass to emit fwd decls
    header = format!("{header}/* Forward declarations */\n");
    depth_first_search(&ast_graph, Some(USR::new("__ROOT__")), |event| {
        if let DfsEvent::Finish(node, _) = event {
            if node == u_root {
                return Control::Continue;
            }

            let entity = c_ast.get_entity(node).unwrap();

            if let Entity::Struct(st) = entity {
                let decl = generate_struct_forward_declaration(st).unwrap();
                header = format!("{header}{decl}\n");
            }
        }

        Control::<USR>::Continue
    });

    header = format!("{header}\n\n/* Declarations */\n");
    depth_first_search(&ast_graph, Some(USR::new("__ROOT__")), |event| {
        if let DfsEvent::Finish(node, _) = event {
            if node == u_root {
                return Control::Continue;
            }

            let entity = c_ast.get_entity(node).unwrap();

            match entity {
                Entity::Struct(st) => {
                    let decl = generate_struct_declaration(st, c_ast).unwrap();
                    header = format!("{header}{decl}\n");
                }
                Entity::Typedef(td) => {
                    if td.is_template(c_ast) {
                        return Control::<USR>::Continue;
                    }

                    let decl = generate_typedef(td, c_ast)
                        .map_err(|e| Error::FailedToGenerateTypedef {
                            name: td.name_internal.clone(),
                            source: Box::new(e),
                        })
                        .unwrap();
                    header = format!("{header}{decl}\n")
                }
                Entity::Function(fun) => {
                    let decl = gen_function_declaration(fun, c_ast)
                        .map_err(|source| Error::FailedToGenerateFunction {
                            name: fun.name_internal.clone(),
                            source: Box::new(source),
                        })
                        .unwrap();
                    header = format!("{header}{decl}\n");
                }
                Entity::Enum(enm) => {
                    let decl = generate_enum_declaration(enm).unwrap();
                    header = format!("{header}{decl}\n\n")
                }
                Entity::FunctionPointer(_) => (),
            }
        }

        Control::Continue
    });

    /*
    // Generate forward declarations for each struct first
    header = format!("{header}/* Forward declarations */
\n");
    for st in c_ast.structs.iter() {
        let decl = generate_struct_forward_declaration(st)?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;

    // Generate the declaration for each enum
    header = format!("{header} /* Enum declarations */
\n");
    for enm in c_ast.enums.iter() {
        let decl = generate_enum_declaration(enm)?;
        header = format!("{header}{decl}\n\n")
    }

    writeln!(&mut header)?;

    // Generate the declaration for each struct
    header = format!("{header} /* Struct declarations */
\n");
    for st in c_ast.structs.iter() {
        let decl = generate_struct_declaration(st, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;

    // Generate typedefs
    header = format!("{header} /* Typedefs */
\n");
    for td in c_ast.typedefs.iter() {
        if td.is_template(c_ast) {
            continue;
        }

        let decl = generate_typedef(td, c_ast).map_err(|e| Error::FailedToGenerateTypedef {
            name: td.name_internal.clone(),
            source: Box::new(e),
        })?;
        header = format!("{header}{decl}\n")
    }

    writeln!(&mut header)?;
    */

    // Generate the declaration and definition for each function
    for fun in c_ast.functions.iter() {
        /*
        let decl = gen_function_declaration(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_internal.clone(),
                source: Box::new(source),
            }
        })?;
        header = format!("{header}{decl}\n");
        */

        let defn = gen_function_definition(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_internal.clone(),
                source: Box::new(source),
            }
        })?;

        source = format!("{source}{defn}\n");
    }

    header = format!("{header}\n#ifdef __cplusplus\n}}\n#endif\n\n");

    header = format!("{header}\n#endif /* ifdef __{}_H__ */\n", module_name_upper);

    Ok((header, source))
}

fn create_ast_graph(c_ast: &CAST) -> DiGraphMap<USR, ()> {
    let mut graph = DiGraphMap::new();
    let root = graph.add_node(USR::new("__ROOT__"));

    // if we only output types etc that are referenced directly by functions, then we cut out a lot of cruft
    // TODO(AL): make this configurable
    let all_types = true;

    if all_types {
        for st in c_ast.structs.iter() {
            create_ast_graph_usr(st.usr, c_ast, &mut graph);
            graph.add_edge(root, st.usr, ());
        }

        for enm in c_ast.enums.iter() {
            create_ast_graph_usr(enm.usr, c_ast, &mut graph);
            graph.add_edge(root, enm.usr, ());
        }

        // for td in c_ast.typedefs.iter() {
        //     create_ast_graph_usr(td.usr, c_ast, &mut graph);
        //     graph.add_edge(root, td.usr, ());
        // }
    }

    for fun in c_ast.functions.iter() {
        create_ast_graph_usr(fun.usr(), c_ast, &mut graph);
        graph.add_edge(root, fun.usr(), ());
    }

    // for proto in c_ast.function_protos.iter() {
    //     create_ast_graph_usr(proto.usr, c_ast, &mut graph);
    //     graph.add_edge(root, proto.usr, ());
    // }

    // println!("***----\n{:?}\n***----", Dot::new(&graph));

    graph
}

fn create_ast_graph_usr(usr: USR, c_ast: &CAST, graph: &mut DiGraphMap<USR, ()>) {
    if graph.contains_node(usr) {
        return;
    }

    let en = match c_ast.get_entity(usr) {
        Some(en) => en,
        None => {
            panic!("Could not find AST entity for {usr}");
        }
    };

    match en {
        Entity::Struct(st) => {
            if matches!(st.bind_kind, ClassBindKind::ValueType) {
                for fld in &st.fields {
                    create_ast_graph_qt(usr, fld.qual_type(), c_ast, graph);
                }
            }
        }
        Entity::Typedef(td) => {
            create_ast_graph_qt(td.usr, &td.underlying_type, c_ast, graph);
            graph.add_edge(usr, td.usr, ());
        }
        Entity::Function(fun) => {
            create_ast_graph_qt(usr, &fun.result, c_ast, graph);
            for arg in &fun.arguments {
                create_ast_graph_qt(usr, &arg.qual_type, c_ast, graph);
            }
        }
        Entity::Enum(_) => (),
        Entity::FunctionPointer(proto) => {
            create_ast_graph_qt(usr, &proto.result, c_ast, graph);
            for arg in &proto.args {
                create_ast_graph_qt(usr, arg, c_ast, graph);
            }
        }
    }
}

fn create_ast_graph_qt(
    from_usr: USR,
    qual_type: &CQualType,
    c_ast: &CAST,
    graph: &mut DiGraphMap<USR, ()>,
) {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) => (),
        CTypeRef::Ref(usr) => {
            create_ast_graph_usr(*usr, c_ast, graph);
            graph.add_edge(from_usr, *usr, ());
        }
        CTypeRef::Pointer(pointee) => create_ast_graph_qt(from_usr, pointee, c_ast, graph),
        CTypeRef::FunctionProto { result, args } => {
            create_ast_graph_qt(from_usr, result, c_ast, graph);
            for arg in args {
                create_ast_graph_qt(from_usr, arg, c_ast, graph);
            }
        }
        CTypeRef::Template(_) => (),
    }
}

/// Generate the signature for this function
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_signature(
    fun: &CFunction,
    c_ast: &CAST,
    use_public_names: bool,
) -> Result<String, Error> {
    let result = format!(
        "{} {}",
        gen_c_type(&fun.result, c_ast, use_public_names).map_err(|e| {
            Error::FailedToGenerateResultType {
                source: Box::new(e),
            }
        })?,
        fun.name_internal
    );

    let str_arg_types = fun
        .arguments
        .iter()
        .map(|a| {
            gen_c_type(&a.qual_type, c_ast, use_public_names).map_err(|e| {
                Error::FailedToGenerateArgument {
                    name: a.name.clone(),
                    source: Box::new(e),
                }
            })
        })
        .collect::<Result<Vec<String>>>()?;

    let arg_str = fun
        .arguments
        .iter()
        .zip(str_arg_types.iter())
        .map(|(a, ty)| format!("{} {}", ty, a.name,))
        .collect::<Vec<String>>();

    Ok(format!("{result}({})", arg_str.join(", ")))
}

/// Generate the declaration of this function
///
/// This is both the function signature and the #define to give it a public name
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_declaration(fun: &CFunction, c_ast: &CAST) -> Result<String, Error> {
    let mut result = format!(
        "{};\n",
        gen_function_signature(fun, c_ast, true).map_err(|e| Error::FailedToGenerateSignature {
            name: fun.name_internal.clone(),
            source: Box::new(e)
        })?
    );

    if fun.name_internal != fun.name_external {
        result = format!(
            "{result}#define {} {};\n",
            fun.name_external, fun.name_internal
        );
    }

    Ok(result)
}

/// Generate the cast expression for converting the c type to its cpp counterpart
#[instrument(level = "trace", skip(ast, c_ast))]
fn gen_cast(qual_type: &CQualType, ast: &AST, c_ast: &CAST) -> Result<Option<String>> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) | CTypeRef::FunctionProto { .. } => Ok(None),
        CTypeRef::Pointer(pointee_qt) => {
            if let Some(s) = gen_cast(pointee_qt, ast, c_ast)? {
                Ok(Some(format!("{s}*")))
            } else {
                Ok(None)
            }
        }
        CTypeRef::Ref(usr) => {
            // ref might be to a class directly, or to a typedef
            if let Some(class) = ast.get_class(*usr) {
                Ok(Some(class.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        name: class.name().to_string(),
                        source,
                    }
                })?))
            } else if let Some(cts) = ast.get_type_alias(*usr) {
                Ok(Some(cts.get_qualified_name(ast).map_err(|source| {
                    Error::FailedToGetQualifiedName {
                        name: cts.name().to_string(),
                        source,
                    }
                })?))
            } else {
                Err(Error::FailedToFindTyperef {
                    usr: *usr,
                    source: Trace::new(),
                })
            }
        }
        CTypeRef::Template(parm) => {
            panic!("Unexpanded template {parm}")
        }
    }
}

fn write_expr(body: &mut String, expr: &Expr, depth: usize) -> Result<()> {
    match expr {
        Expr::Compound(stmts) => {
            writeln!(body, " {{")?;
            for expr in stmts {
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, expr, depth + 1)?;
                writeln!(body, ";")?;
            }
            writeln!(body, "{:width$}}}", "", width = depth * 4)?;
        }
        Expr::CppMethodCall {
            receiver,
            function,
            template_arguments,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            write!(body, "->{function}")?;

            if !template_arguments.is_empty() {
                write!(body, "<")?;
                write_argument_list(body, template_arguments, depth)?;
                write!(body, ">")?;
            }

            write!(body, "(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            write_argument_list(body, arguments, depth)?;
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppStaticMethodCall {
            receiver,
            function,
            template_arguments,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            write!(body, "::{function}")?;

            if !template_arguments.is_empty() {
                write!(body, "<")?;
                write_argument_list(body, template_arguments, depth)?;
                write!(body, ">")?;
            }

            write!(body, "(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            write_argument_list(body, arguments, depth)?;
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppFunctionCall {
            function,
            template_arguments,
            arguments,
        } => {
            write!(body, "{function}")?;
            if !template_arguments.is_empty() {
                write!(body, "<")?;
                write_argument_list(body, template_arguments, depth)?;
                write!(body, ">")?;
            }

            write!(body, "(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            write_argument_list(body, arguments, depth)?;
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::CppConstructor {
            receiver,
            template_arguments,
            arguments,
        } => {
            write_expr(body, receiver, depth)?;
            if !template_arguments.is_empty() {
                write!(body, "<")?;
                write_argument_list(body, template_arguments, depth)?;
                write!(body, ">")?;
            }

            write!(body, "(")?;
            if !arguments.is_empty() {
                writeln!(body)?;
            }
            write_argument_list(body, arguments, depth)?;
            if arguments.is_empty() {
                write!(body, ")")?;
            } else {
                write!(body, "{:width$})", "", width = depth * 4)?;
            }
        }
        Expr::Cast { to_type, value } => {
            write!(body, "(({to_type})")?;
            write_expr(body, value, depth)?;
            write!(body, ")")?;
        }
        Expr::Deref { value } => {
            write!(body, "*")?;
            write_expr(body, value, depth)?;
        }
        Expr::AddrOf { value } => {
            write!(body, "&")?;
            write_expr(body, value, depth)?;
        }
        Expr::Move(expr) => {
            write!(body, "std::move(")?;
            write_expr(body, expr, depth)?;
            write!(body, ")")?;
        }
        Expr::New(expr) => {
            write!(body, "new ")?;
            write_expr(body, expr, depth)?;
        }
        Expr::Delete(expr) => {
            write!(body, "delete ")?;
            write_expr(body, expr, depth)?;
        }
        Expr::Assignment { left, right } => {
            write_expr(body, left, depth)?;
            write!(body, " = ")?;
            write_expr(body, right, depth)?;
        }
        Expr::Token(v) => {
            write!(body, "{v}")?;
        }
        Expr::TryCatch {
            try_block,
            catch_blocks,
        } => {
            writeln!(body, "try {{")?;
            write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
            write_expr(body, try_block, depth + 1)?;
            writeln!(body, ";")?;
            write!(body, "{:width$}}}", "", width = depth * 4)?;

            for catch in catch_blocks {
                write!(body, " catch (")?;
                write_expr(body, &catch.exception, depth)?;
                writeln!(body, ") {{")?;
                write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
                write_expr(body, &catch.stmt, depth + 1)?;
                writeln!(body, ";")?;
                write!(body, "{:width$}}}", "", width = depth * 4)?;
            }
        }
        Expr::Return(expr) => {
            write!(body, "return ")?;
            write_expr(body, expr, depth)?;
        }
        _ => (),
    }

    Ok(())
}

fn write_argument_list(body: &mut String, arguments: &[Expr], depth: usize) -> Result<()> {
    if !arguments.is_empty() {
        for arg in arguments.iter().take(arguments.len() - 1) {
            write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
            write_expr(body, arg, depth + 1)?;
            writeln!(body, ",")?;
        }
        if let Some(arg) = arguments.last() {
            write!(body, "{:width$}", "", width = (depth + 1) * 4)?;
            write_expr(body, arg, depth + 1)?;
            writeln!(body)?;
        }
    }
    Ok(())
}

/// Generate the definition of this function
///
/// This means calling its cpp counterpart and generating all necessary casting
#[instrument(level = "trace", skip(c_ast))]
fn gen_function_definition(fun: &CFunction, c_ast: &CAST) -> Result<String, Error> {
    let mut body = gen_function_signature(fun, c_ast, false).map_err(|e| {
        Error::FailedToGenerateSignature {
            name: fun.name_internal.clone(),
            source: Box::new(e),
        }
    })?;

    write_expr(&mut body, &fun.body, 0)?;

    Ok(body)
}

/// Generate the spelling for this C type
#[instrument(level = "trace", skip(c_ast))]
fn gen_c_type(qt: &CQualType, c_ast: &CAST, use_public_names: bool) -> Result<String> {
    let const_ = if qt.is_const() { " const" } else { "" };
    Ok(match qt.type_ref() {
        CTypeRef::Builtin(tk) => match tk {
            TypeKind::Bool => "bool".to_string(),
            TypeKind::Char_S => "char".to_string(),
            TypeKind::Char_U => "unsigned char".to_string(),
            TypeKind::Double => "double".to_string(),
            TypeKind::Float => "float".to_string(),
            TypeKind::Int => "int".to_string(),
            TypeKind::Long => "long".to_string(),
            TypeKind::LongDouble => "long double".to_string(),
            TypeKind::LongLong => "long long".to_string(),
            TypeKind::Short => "short".to_string(),
            TypeKind::UChar => "unsigned char".to_string(),
            TypeKind::UInt => "unsigned int".to_string(),
            TypeKind::ULong => "unsigned long".to_string(),
            TypeKind::ULongLong => "unsigned long long".to_string(),
            TypeKind::UShort => "unsigned short".to_string(),
            TypeKind::Void => "void".to_string(),
            _ => unimplemented!("need to implement builtin {tk}"),
        },
        CTypeRef::Ref(usr) => {
            // first check to see if there's a direct class reference
            if let Some(st) = c_ast.get_struct(*usr) {
                format!(
                    "{}{const_}",
                    if use_public_names {
                        &st.name_external
                    } else {
                        &st.name_internal
                    }
                )
            } else if let Some(td) = c_ast.get_typedef(*usr) {
                // no struct with this USR, see if there's a typedef instead
                format!("{}{const_}", td.name_external)
            } else if let Some(enm) = c_ast.get_enum(*usr) {
                format!("{}{const_}", enm.name_external)
            } else if let Some(_proto) = c_ast.get_function_proto(*usr) {
                // we should never get here as we need to handle function pointers explicitly at the typedef level
                unreachable!()
            } else {
                return Err(Error::FailedToFindTyperef {
                    usr: *usr,
                    source: Trace::new(),
                });
            }
        }
        CTypeRef::Pointer(pointee) => {
            format!("{}*{const_}", gen_c_type(pointee, c_ast, use_public_names)?)
        }
        CTypeRef::FunctionProto { result, args } => {
            format!(
                "{}(*)({})",
                gen_c_type(result.deref(), c_ast, use_public_names)?,
                args.iter()
                    .map(|a| gen_c_type(a, c_ast, use_public_names))
                    .collect::<Result<Vec<String>>>()?
                    .join(", ")
            )
        }
        _ => unimplemented!("Need to implement {qt:?}"),
    })
}

#[instrument(level = "trace")]
fn generate_function_proto(
    proto: &CFunctionProto,
    c_ast: &CAST,
    name: Option<&str>,
) -> Result<String> {
    Ok(format!(
        "{}(*{})({})",
        gen_c_type(&proto.result, c_ast, true).map_err(|e| Error::FailedToGenerateResultType {
            source: Box::new(e)
        })?,
        if let Some(name) = name { name } else { "" },
        proto
            .args
            .iter()
            .map(
                |a| gen_c_type(a, c_ast, true).map_err(|e| Error::FailedToGenerateArgument {
                    name: a.name().to_string(),
                    source: Box::new(e)
                })
            )
            .collect::<Result<Vec<String>>>()?
            .join(", ")
    ))
}

/// Generate a forwad declaration for this struct
#[instrument(level = "trace")]
fn generate_struct_forward_declaration(st: &CStruct) -> Result<String> {
    Ok(format!("typedef struct {0} {0};", st.name_internal))
}

/// Generate the declaration of this struct
#[instrument(level = "trace", skip(c_ast))]
fn generate_struct_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_declaration(st),
        ClassBindKind::ValueType => generate_valuetype_declaration(st, c_ast),
        _ => todo!("Handle opaquebytes"),
    }
}

#[instrument(level = "trace", skip(c_ast))]
fn generate_typedef(td: &CTypedef, c_ast: &CAST) -> Result<String> {
    if let CTypeRef::Ref(usr) = td.underlying_type.type_ref() {
        if let Some(proto) = c_ast.get_function_proto(*usr) {
            return Ok(format!(
                "typedef {};",
                generate_function_proto(proto, c_ast, Some(&td.name_external)).map_err(|e| {
                    Error::FailedToGenerateFunctionProto {
                        source: Box::new(e),
                    }
                })?
            ));
        }
    } else if let CTypeRef::Pointer(pointee) = td.underlying_type.type_ref() {
        if let CTypeRef::FunctionProto { result, args } = pointee.type_ref() {
            return Ok(format!(
                "typedef {}(*{})({});",
                gen_c_type(result.deref(), c_ast, true).map_err(|e| {
                    Error::FailedToGenerateResultType {
                        source: Box::new(e),
                    }
                })?,
                td.name_external,
                args.iter()
                    .map(|a| gen_c_type(a, c_ast, true).map_err(|e| {
                        Error::FailedToGenerateArgument {
                            name: a.name().to_string(),
                            source: Box::new(e),
                        }
                    }))
                    .collect::<Result<Vec<String>>>()?
                    .join(", ")
            ));
        }
    }

    Ok(format!(
        "typedef {} {};",
        gen_c_type(&td.underlying_type, c_ast, true)?,
        td.name_external
    ))
}

#[instrument(level = "trace")]
fn generate_enum_declaration(enm: &CEnum) -> Result<String> {
    let mut result = format!("enum {} {{\n", enm.name_internal);

    if !enm.variants.is_empty() {
        for var in enm.variants.iter().take(enm.variants.len() - 1) {
            result = format!("{result}    {}_{} = {},\n", enm.name_external, var.0, var.1);
        }
        let var = enm.variants.last().unwrap();
        result = format!("{result}    {}_{} = {}\n", enm.name_external, var.0, var.1);
    }

    result = format!("{result}}};\n");
    result = format!(
        "{result}typedef {} {};",
        enm.name_internal, enm.name_external
    );

    Ok(result)
}

/// Generate the declaration for a valuetype struct
///
/// This means generating all fields
#[instrument(level = "trace", skip(c_ast))]
fn generate_valuetype_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    let ind = "    ";
    let mut result = format!("struct {} {{\n", st.name_internal);

    for field in &st.fields {
        result = format!(
            "{result}{ind}{} {};\n",
            gen_c_type(field.qual_type(), c_ast, true)?,
            field.name()
        );
    }

    result = format!("{result}}};\n");

    if st.name_external != st.name_internal {
        result = format!(
            "{result}typedef {} {};\n",
            st.name_internal, st.name_external
        );
    }

    Ok(result)
}

/// Generate the declaration for an opaqueptr struct
///
/// This is basically just a forward declaration since the type is only ever represented by a pointer to it
#[instrument(level = "trace")]
fn generate_opaqueptr_declaration(st: &CStruct) -> Result<String> {
    if st.name_external != st.name_internal {
        Ok(format!(
            "typedef {0} {1};\n",
            st.name_internal, st.name_external
        ))
    } else {
        Ok("".to_string())
    }
}
