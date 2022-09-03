use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind, function::Method, index_map::IndexMapKey};
use bbl_translate::{CFunction, CFunctionSource, CQualType, CStruct, CTypeRef, CAST};

pub mod error;
use error::{ArgumentError, Error, FunctionGenerationError, TypeError};

type Result<T, E = Error> = std::result::Result<T, E>;

/// Generate the c library code (itself c++ but with a c header)
///
/// # Returns
/// A tuple containing two [`String`]s, the first of which is the header source, the second is the implementation source
pub fn gen_c(module_name: &str, ast: &AST, c_ast: &CAST) -> Result<(String, String)> {
    let mut header = format!("#ifndef __{}_H__\n", module_name.to_uppercase());
    header = format!("{header}#define __{}_H__\n\n", module_name.to_uppercase());
    header = format!("{header}#ifdef __cplusplus\nextern \"C\" {{\n#endif\n\n");

    let mut source = c_ast
        .includes
        .iter()
        .map(|i| i.get_statement())
        .collect::<Vec<_>>()
        .join("\n");
    if !source.is_empty() {
        source = format!("{source}\n\n");
    }

    // Generate the declaration for each struct
    for st in c_ast.structs.iter() {
        let decl = generate_struct_declaration(st, c_ast)?;
        header = format!("{header}{decl}\n")
    }

    // Generate the declaration and definition for each function
    for fun in c_ast.functions.iter() {
        let decl = gen_function_declaration(fun, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source,
            }
        })?;

        let defn = gen_function_definition(fun, ast, c_ast).map_err(|source| {
            Error::FailedToGenerateFunction {
                name: fun.name_private.clone(),
                source,
            }
        })?;

        header = format!("{header}{decl}\n");
        source = format!("{source}{defn}\n");
    }

    header = format!("{header}\n#ifdef __cplusplus\n}}\n#endif\n\n");

    header = format!(
        "{header}\n#endif /* ifdef __{}_H__ */\n",
        module_name.to_uppercase()
    );

    Ok((header, source))
}

/// Generate the signature for this function
fn gen_function_signature(
    fun: &CFunction,
    c_ast: &CAST,
    use_public_names: bool,
) -> Result<String, TypeError> {
    let result = format!(
        "{} {}",
        gen_c_type(&fun.result, c_ast, use_public_names),
        fun.name_private
    );

    let arg_str = fun
        .arguments
        .iter()
        .map(|a| {
            format!(
                "{} {}",
                gen_c_type(&a.qual_type, c_ast, use_public_names),
                a.name
            )
        })
        .collect::<Vec<String>>();

    Ok(format!("{result}({})", arg_str.join(", ")))
}

/// Generate the declaration of this function
///
/// This is both the function signature and the #define to give it a public name
fn gen_function_declaration(
    fun: &CFunction,
    c_ast: &CAST,
) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{};\n",
        gen_function_signature(fun, c_ast, true)
            .map_err(FunctionGenerationError::FailedToGenerateSignature)?
    );

    if fun.name_private != fun.name_public {
        result = format!(
            "{result}#define {} {};\n",
            fun.name_public, fun.name_private
        );
    }

    Ok(result)
}

/// Generate the cast expression for converting the c type to its cpp counterpart
fn gen_cast(qual_type: &CQualType, ast: &AST, c_ast: &CAST) -> Result<Option<String>, TypeError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_) => Ok(None),
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
                    TypeError::FailedToGetQualifiedName {
                        name: class.name().to_string(),
                        source,
                    }
                })?))
            } else if let Some(cts) = ast.get_type_alias(*usr) {
                Ok(Some(cts.get_qualified_name(ast).map_err(|source| {
                    TypeError::FailedToGetQualifiedName {
                        name: cts.name().to_string(),
                        source,
                    }
                })?))
            } else {
                Err(TypeError::TypeRefNotFound(*usr))
            }
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk)),
    }
}

/// Generate the c++ required to cast and pass an argument from c to the cpp fuction call
///
/// For example:
/// NS::Class    -> *(NS::Class*)&arg
/// NS::Class*   -> (NS::Class*)arg
/// NS::Class&   -> *(NS::Class*)arg
/// NS::Class*&  -> *(NS::Class**)arg
fn generate_arg_pass(
    arg_name: &str,
    qual_type: &CQualType,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, ArgumentError> {
    match qual_type.type_ref() {
        CTypeRef::Builtin(_tk) => Ok(arg_name.to_string()),
        CTypeRef::Pointer(_) => {
            // needs_deref() will be true if this pointer was a reference in the cpp ast, or if it's a conversion of a
            // non-value-type passed by value
            let deref = if qual_type.needs_deref() { "*" } else { "" };
            if let Some(cast) = gen_cast(qual_type, ast, c_ast)? {
                Ok(format!("{deref}({cast}){arg_name}"))
            } else {
                Ok(format!("{deref}{arg_name}"))
            }
        }
        CTypeRef::Ref(_) => {
            if let Some(cast) = gen_cast(qual_type, ast, c_ast)? {
                Ok(format!("*({cast}*)&{arg_name}"))
            } else {
                Ok(arg_name.to_string())
            }
        }
        CTypeRef::Unknown(tk) => Err(TypeError::UnknownType(*tk))?,
    }
}

/// Generate the cpp function call expression, including casting all arguments
fn gen_cpp_call(fun: &CFunction, ast: &AST) -> Result<String, TypeError> {
    match fun.source {
        CFunctionSource::Function(cpp_fun_id) => {
            let cpp_fun = &ast.functions()[cpp_fun_id];
            Ok(cpp_fun.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: cpp_fun.name().to_string(),
                    source: e,
                }
            })?)
        }
        CFunctionSource::Method((class_id, method_id)) => {
            let class = &ast.classes()[class_id];
            let method: &Method = &class.methods()[method_id.get()];

            let qname = method.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: class.name().to_string(),
                    source: e,
                }
            })?;

            if method.is_static() {
                Ok(qname)
            } else {
                Ok(format!("(({}*)self)->{}", qname, method.name()))
            }
        }
        CFunctionSource::SpecializedMethod((class_id, method_id)) => {
            let class = &ast.classes()[class_id];
            let spec_method = &class.specialized_methods()[method_id.0];
            let temp_method = &class.methods()[spec_method.specialized_decl().get()];

            let qname = temp_method.get_qualified_name(ast).map_err(|e| {
                TypeError::FailedToGetQualifiedName {
                    name: class.name().to_string(),
                    source: e,
                }
            })?;

            if temp_method.is_static() {
                Ok(qname)
            } else {
                Ok(format!("(({}*)self)->{}", qname, temp_method.name()))
            }
        }
    }
}

/// Generate the definition of this function
///
/// This means calling its cpp counterpart and generating all necessary casting
fn gen_function_definition(
    fun: &CFunction,
    ast: &AST,
    c_ast: &CAST,
) -> Result<String, FunctionGenerationError> {
    let mut result = format!(
        "{} {{\n",
        gen_function_signature(fun, c_ast, false)
            .map_err(FunctionGenerationError::FailedToGenerateSignature)?
    );
    let indent = "    ";

    // Get the cast expression to cast the cpp return type to c
    let cast = gen_cast(&fun.result, ast, c_ast)
        .map_err(|e| FunctionGenerationError::FailedToGenerateArg {
            name: "[result]".to_string(),
            source: ArgumentError::TypeError(e),
        })?
        .map(|s| format!("({s})"))
        .unwrap_or_else(|| "".to_string());

    // Append the call to the cpp function
    result = format!(
        "{result}{indent}return {cast}{}",
        gen_cpp_call(fun, ast).map_err(FunctionGenerationError::FailedToGenerateCall)?
    );

    // Get all the arguments for the call
    let arg_str =
        if fun.arguments.is_empty() || fun.arguments.len() == 1 && fun.arguments[0].is_self {
            "();".to_string()
        } else {
            let mut args = Vec::new();
            for arg in &fun.arguments {
                if arg.is_self {
                    continue;
                }

                match generate_arg_pass(&arg.name, &arg.qual_type, ast, c_ast) {
                    Ok(s) => args.push(format!("{indent}{indent}{s}")),
                    Err(source) => {
                        return Err(FunctionGenerationError::FailedToGenerateArg {
                            name: arg.name.to_string(),
                            source,
                        })
                    }
                }
            }

            format!("(\n{}\n{indent});", args.join(",\n"))
        };

    result = format!("{result}{arg_str}\n");

    Ok(format!("{result}}}\n"))
}

/// Generate the spelling for this C type
fn gen_c_type(qt: &CQualType, c_ast: &CAST, use_public_names: bool) -> String {
    match qt.type_ref() {
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
            _ => qt.format(c_ast, use_public_names),
        },
        _ => qt.format(c_ast, use_public_names),
    }
}

/// Generate the declaration of this struct
fn generate_struct_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    match &st.bind_kind {
        ClassBindKind::OpaquePtr => generate_opaqueptr_declaration(st),
        ClassBindKind::ValueType => generate_valuetype_declaration(st, c_ast),
        _ => todo!(),
    }
}

/// Generate the declaration for a valuetype struct
///
/// This means generating all fields
fn generate_valuetype_declaration(st: &CStruct, c_ast: &CAST) -> Result<String> {
    let ind = "    ";
    let mut result = format!("struct {} {{\n", st.name_private);

    for field in &st.fields {
        result = format!(
            "{result}{ind}{} {};\n",
            gen_c_type(field.qual_type(), c_ast, true),
            field.name()
        );
    }

    result = format!("{result}}};\n");
    result = format!("{result}typedef {} {};\n", st.name_private, st.name_public);

    Ok(result)
}

/// Generate the declaration for an opaqueptr struct
///
/// This is basically just a forward declaration since the type is only ever represented by a pointer to it
fn generate_opaqueptr_declaration(st: &CStruct) -> Result<String> {
    Ok(format!(
        "struct {0};\ntypedef {0} {1};\n",
        st.name_private, st.name_public
    ))
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use bbl_extract::{class::ClassBindKind, parse_string_and_extract_ast};
    use bbl_translate::translate_cpp_ast_to_c;
    use env_logger::fmt::Color;
    use log::{error, Level};
    use opentelemetry::sdk::export::trace::stdout;
    use tracing::span;
    use tracing_subscriber::Registry;

    use crate::{gen_c, Error};

    #[test]
    fn write_simple_class() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {
class Class {
public:
    int method1();
    void method2(const Class& c);
};
}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 2);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    }

    #[test]
    fn write_simple_valuetype() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {

class Class {
public:
    int a;
    float b;
};

}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        // let class = ast.find_class("Test_1_0::Class")?;
        // ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 0);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    }

    #[test]
    fn write_valuetype_pass_by_value() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {

class Class {
public:
    int a;
    float b;
};

void fun(Class c);

}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n\n{c_header}\n\nSOURCE:\n\n{c_source}");

        Ok(())
    }

    #[test]
    fn write_opaqueptr_pass_by_value() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {
class Class {
    int _c; // will force opaqueptr as no longer POD
public:
    int a;
    float b;
};
void fun(Class c);
}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 1);
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    }

    #[test]
    fn write_nested_valuetype() -> Result<(), Error> {
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {
class A {
public:
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 0);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    }

    #[test]
    fn write_nested_valuetype_with_forced_member() -> Result<(), Error> {
        init_log();
        let mut ast = parse_string_and_extract_ast(
            r#"
namespace Test_1_0 {
class A {
public:
    A(const A&); // presence of copy ctor means no POD any more, but we *know* it's a trivial constructor
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
            &cli_args()?,
            true,
            None,
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let class = ast.find_class("Test_1_0::A")?;
        ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    }

    #[test]
    fn write_includes() -> Result<(), Error> {
        init_log();
        let mut ast = parse_string_and_extract_ast(
            r#"
#include <stddef.h>
#include <string>

namespace Test_1_0 {
class A {
public:
    A(const A&); // presence of copy ctor means no POD any more, but we *know* it's a trivial constructor
    int a;
    float b;
};

class B {
public:
    A a;
};
}
            "#,
            &cli_args()?,
            true,
            Some("Test_1_0"),
        )?;

        ast.pretty_print(0);

        let ns = ast.find_namespace("Test_1_0")?;
        ast.rename_namespace(ns, "Test");

        let class = ast.find_class("Test_1_0::A")?;
        ast.class_set_bind_kind(class, ClassBindKind::ValueType)?;

        let c_ast = translate_cpp_ast_to_c(&ast)?;
        println!("ast has {} includes", ast.includes().len());
        println!("cast has {} includes", c_ast.includes.len());
        c_ast.pretty_print(0);

        assert_eq!(c_ast.structs.len(), 2);
        assert_eq!(c_ast.functions.len(), 1);

        let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
        println!("HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------");

        Ok(())
    }

    #[test]
    fn write_take_std_string() -> Result<(), Error> {
        run_with_telemetry(|| {
            let mut ast = parse_string_and_extract_ast(
                r#"
    #include <string>

    namespace Test {
    class Class {
    public:
        std::string take_string(const std::string& s);
    };
    }
            "#,
                &cli_args()?,
                true,
                Some("Test"),
            )?;

            ast.pretty_print(0);

            let c_ast = translate_cpp_ast_to_c(&ast)?;
            c_ast.pretty_print(0);

            assert_eq!(c_ast.structs.len(), 2);
            assert_eq!(c_ast.functions.len(), 1);

            let (c_header, c_source) = gen_c("test", &ast, &c_ast)?;
            println!(
                "HEADER:\n--------\n{c_header}--------\n\nSOURCE:\n--------\n{c_source}--------"
            );

            Ok(())
        })
    }

    fn run_with_telemetry<F>(closure: F) -> Result<(), Error>
    where
        F: FnOnce() -> Result<(), Error>,
    {
        use opentelemetry::global;
        use tracing_subscriber::layer::SubscriberExt;

        global::set_text_map_propagator(opentelemetry_jaeger::Propagator::new());
        let tracer = opentelemetry_jaeger::new_pipeline()
            .install_simple()
            .unwrap();

        // Create a tracing layer with the configured tracer
        let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

        // Use the tracing subscriber `Registry`, or any other subscriber
        // that impls `LookupSpan`
        let subscriber = Registry::default().with(telemetry);

        // Trace executed code
        let res = tracing::subscriber::with_default(subscriber, || {
            // Spans will be sent to the configured OpenTelemetry exporter
            let root = span!(tracing::Level::TRACE, "app_start", work_units = 2);
            let _enter = root.enter();

            closure()
        });
        global::shutdown_tracer_provider(); // sending remaining spans

        res.map_err(|err| {
            error!("{err}");
            for e in source_iter(&err) {
                error!("  because: {e}")
            }

            err
        })
    }

    #[tracing::instrument]
    fn fun_b(arg: &str) {}

    #[tracing::instrument]
    fn fun_a(arg: &str) {
        fun_b("your mum's a slag");
    }

    #[test]
    fn test_tracing() {
        use opentelemetry::global;
        use tracing_subscriber::layer::SubscriberExt;

        global::set_text_map_propagator(opentelemetry_jaeger::Propagator::new());
        let tracer = opentelemetry_jaeger::new_pipeline()
            .install_simple()
            .unwrap();

        // Create a tracing layer with the configured tracer
        let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

        // Use the tracing subscriber `Registry`, or any other subscriber
        // that impls `LookupSpan`
        let subscriber = Registry::default().with(telemetry);

        // Trace executed code
        tracing::subscriber::with_default(subscriber, || {
            // Spans will be sent to the configured OpenTelemetry exporter
            let root = span!(tracing::Level::TRACE, "app_start", work_units = 2);
            let _enter = root.enter();

            tracing::error!("This event will be logged in the root span.");

            fun_a("fuck you");
        });
        global::shutdown_tracer_provider(); // sending remaining spans
    }

    pub fn init_log() {
        use std::io::Write;

        let _ = env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("warn"))
            .format_timestamp(None)
            .format(|buf, record| -> Result<(), std::io::Error> {
                let mut level_style = buf.style();
                match record.level() {
                    Level::Trace => level_style.set_color(Color::Blue),
                    Level::Debug => level_style.set_color(Color::White),
                    Level::Info => level_style.set_color(Color::Cyan),
                    Level::Warn => level_style.set_color(Color::Yellow),
                    Level::Error => level_style.set_color(Color::Red),
                };

                writeln!(
                    buf,
                    "{} [{}:{}] {}",
                    level_style.value(record.level()),
                    record.file().unwrap_or(""),
                    record.line().unwrap_or(0),
                    record.args()
                )
            })
            // .is_test(true)
            .try_init();
    }

    fn source_iter(
        error: &impl std::error::Error,
    ) -> impl Iterator<Item = &(dyn std::error::Error + 'static)> {
        SourceIter {
            current: error.source(),
        }
    }

    struct SourceIter<'a> {
        current: Option<&'a (dyn std::error::Error + 'static)>,
    }

    impl<'a> Iterator for SourceIter<'a> {
        type Item = &'a (dyn std::error::Error + 'static);

        fn next(&mut self) -> Option<Self::Item> {
            let current = self.current;
            self.current = self.current.and_then(std::error::Error::source);
            current
        }
    }
}
