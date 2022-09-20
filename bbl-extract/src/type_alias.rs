use bbl_clang::cursor_kind::CursorKind;
use bbl_clang::translation_unit::TranslationUnit;
use std::convert::TryInto;
use std::fmt::Display;
use tracing::{debug, error, info, instrument, trace, warn};

use crate::ast::{dump_cursor, get_namespaces_for_decl, get_qualified_name, TypeAliasId, AST};
use crate::class::extract_class_decl;
use crate::namespace::extract_namespace;
use crate::qualtype::{extract_type, QualType};
use crate::stdlib::create_std_string;
use crate::template_argument::{TemplateParameterDecl, TemplateType};
use bbl_clang::cursor::{CurClassTemplate, CurTemplateRef, CurTypedef, Cursor, USR};
use bbl_clang::ty::Type;
use std::fmt::Debug;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct TypeAlias {
    name: String,
    usr: USR,
    namespaces: Vec<USR>,
    underlying_type: QualType,
}

impl Debug for TypeAlias {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeAlias {} = {:?}", self.name, self.underlying_type)
    }
}

impl TypeAlias {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn underlying_type(&self) -> &QualType {
        &self.underlying_type
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        println!("TypeAlias {} = {}", self.name, self.underlying_type.format(ast, &[], None))
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }
}

/*
pub enum TypeAlias {
    TypeAliasType { name: String, usr: USR },
    ClassTemplateSpecialization(ClassTemplateSpecialization),
    FunctionTemplateSpecialization(FunctionTemplateSpecialization),
}

impl Debug for TypeAlias {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAlias::TypeAliasType { name, usr } => {
                write!(f, "TypeAlias {usr} {name}")
            }
            TypeAlias::ClassTemplateSpecialization(ClassTemplateSpecialization {
                specialized_decl,
                usr,
                name,
                template_arguments,
                namespaces,
            }) => {
                write!(f, "ClassTemplateSpecialization {usr} {name} specializes={specialized_decl} template_arguments={template_arguments:?} namespaces={namespaces:?}")
            }
            TypeAlias::FunctionTemplateSpecialization(FunctionTemplateSpecialization {
                specialized_decl,
                usr,
                name,
                template_arguments,
                namespaces,
            }) => {
                write!(f, "FunctionTemplateSpecialization {usr} {name} specializes={specialized_decl} template_arguments={template_arguments:?} namespaces={namespaces:?}")
            }
        }
    }
}

impl TypeAlias {
    pub fn usr(&self) -> USR {
        match self {
            TypeAlias::TypeAliasType { usr, .. } => *usr,
            TypeAlias::ClassTemplateSpecialization(cts) => cts.usr,
            TypeAlias::FunctionTemplateSpecialization(fts) => fts.usr,
        }
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        match self {
            TypeAlias::TypeAliasType { name, usr } => todo!(),
            TypeAlias::ClassTemplateSpecialization(cts) => cts.pretty_print(depth, ast),
            TypeAlias::FunctionTemplateSpecialization(fts) => {
                fts.pretty_print(depth, ast, outer_template_parameters)
            }
        }
    }

    pub fn name(&self) -> &str {
        match self {
            TypeAlias::ClassTemplateSpecialization(cts) => cts.name(),
            _ => unimplemented!(),
        }
    }

    pub fn namespaces(&self) -> &[USR] {
        match self {
            TypeAlias::ClassTemplateSpecialization(cts) => cts.namespaces(),
            _ => unimplemented!(),
        }
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), self.namespaces(), ast)
    }
}
*/

#[instrument(skip(depth, already_visited, ast, tu), level = "trace")]
pub fn extract_typedef_decl<'a>(
    c_typedef: CurTypedef,
    depth: usize,
    already_visited: &mut Vec<USR>,
    ast: &'a mut AST,
    tu: &TranslationUnit,
) -> Result<&'a TypeAlias> {
    let usr = c_typedef.usr();
    if already_visited.contains(&usr) {
        trace!("already visiting. skipping.");
        return ast
            .get_type_alias(c_typedef.usr())
            .ok_or_else(|| Error::TypeAliasNotFound(c_typedef.usr()));
    } else {
        already_visited.push(usr);
    }

    let name = c_typedef.display_name();
    let namespaces = get_namespaces_for_decl(c_typedef.into(), tu, ast, already_visited)?;

    let underlying_type = extract_type(
        c_typedef.underlying_type()?,
        depth + 1,
        &[],
        already_visited,
        ast,
        tu,
    )?;

    let id = ast.insert_type_alias(TypeAlias {
        name,
        usr,
        namespaces,
        underlying_type,        
    });

    // literally just inserted it
    let ta = ast.get_type_alias(usr).unwrap();

    Ok(ta)

    /*
    // if it has a TemplateRef child it's a class template specialization
    if c_typedef.has_child_of_kind(CursorKind::TemplateRef) {
        trace!("Got ClassTemplateSpecialization {:?}", c_typedef);
        let type_alias =
            TypeAlias::ClassTemplateSpecialization(extract_class_template_specialization(
                c_typedef,
                depth + 1,
                already_visited,
                ast,
                tu,
                &Vec::new(),
            )?);

        already_visited.push(c_typedef.usr());

        let id = ast.insert_type_alias(type_alias);
        return Ok(&ast.type_aliases()[TypeAliasId::new(id)]);
    } else if let Some(type_ref) = c_typedef.first_child_of_kind(CursorKind::TypeRef) {
        let c_ref = type_ref.referenced().unwrap_or_else(|e| {
            dump_cursor(c_typedef.into(), tu);
            panic!("Could not get referenced decl from TypeRef: {e}");
        });
        trace!("Got type ref. referenced is {:?}", c_ref);

        match c_ref.kind() {
            CursorKind::ClassDecl => {
                println!("Got ClassDecl {:?}", c_ref);
                already_visited.push(c_typedef.usr());
                extract_class_decl(
                    c_ref.try_into()?,
                    depth + 1,
                    tu,
                    &Vec::new(),
                    ast,
                    already_visited,
                )?;

                let id = ast.insert_type_alias(TypeAlias::TypeAliasType {
                    name: c_typedef.display_name(),
                    usr: c_ref.usr(),
                });
                return Ok(&ast.type_aliases()[TypeAliasId::new(id)]);
            }
            _ => {
                panic!("Got unhandled cursor ref kind in extract_typedef_decl: {c_ref:?}");
            }
        }
    } else {
        // dunno
        dump_cursor(c_typedef.into(), tu);
        unimplemented!();
    }
    */
}

#[instrument(skip(depth, tu, namespaces, ast, already_visited), level = "trace")]
pub fn extract_class_template_specialization(
    c_typedef: CurTypedef,
    depth: usize,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    namespaces: &Vec<USR>,
) -> Result<ClassTemplateSpecialization> {
    let indent = format!("{:width$}", "", width = depth * 2);

    let name = c_typedef.display_name();

    debug!(
        "{}TypeAliasDecl {} {} {}",
        indent,
        c_typedef.usr(),
        c_typedef.pretty_printed(c_typedef.printing_policy()),
        c_typedef.location().spelling_location()
    );

    if let Ok(ty) = c_typedef.ty() {
        let template_args =
            extract_template_args(&c_typedef, depth + 1, &ty, tu, already_visited, ast).map_err(
                |e| Error::FailedToExtractTemplateArgs {
                    source: Box::new(e),
                },
            )?;

        debug!("Template args:");
        template_args.iter().flatten().map(|a| debug!("    {a}"));

        // TODO: merge this loop with the one above
        // First child will be the namespace of the target, next will be the template ref which will point to the class template
        let mut specialized_decl = None;
        let mut local_namespaces = Vec::new();
        for child in c_typedef.children() {
            // println!("{indent}    {} {} {}", child.usr(), child.display_name(), child.kind());
            if child.kind() == CursorKind::NamespaceRef {
                let c_namespace = child
                    .referenced()
                    .map_err(|_| Error::FailedToGetNamespaceRefFrom(child.display_name()))?;
                // extract the namespace here
                extract_namespace(c_namespace, depth + 1, tu, ast);
                local_namespaces.push(c_namespace.usr());
            } else if child.kind() == CursorKind::TemplateRef {
                if let Ok(cref) = child.referenced() {
                    if cref.kind() == CursorKind::ClassTemplate {
                        let c_class_template: CurClassTemplate = cref.try_into()?;
                        if !already_visited.contains(&cref.usr()) {
                            // If we haven't already extracted the class which this alias refers to, do it now
                            // if we've got namespaces defined on this ref then we /probably/ want to use them,
                            // otherwise use the ones passed in
                            let ct_namespaces = if local_namespaces.is_empty() {
                                namespaces
                            } else {
                                &local_namespaces
                            };

                            if cref.display_name().starts_with("basic_string<") {
                                // do string here and break out of the loop so we don't try extracting char traits and
                                // allocator on windows which are both template types
                                // TODO(AL): We probably want to break on first TemplateRef for any type alias don't we?
                                debug!("Extracting basic_string {}", cref.usr());
                                let cd = create_std_string(cref, ct_namespaces.to_vec());
                                ast.insert_class(cd);
                                already_visited.push(cref.usr());
                                specialized_decl = Some(cref.usr());
                                break;
                            } else {
                                debug!("extracting class template {cref:?}");
                                extract_class_decl(
                                    c_class_template.as_class_decl(),
                                    depth + 1,
                                    tu,
                                    ct_namespaces,
                                    ast,
                                    already_visited,
                                )?;
                            }
                        }
                        specialized_decl = Some(cref.usr());
                        debug!("{indent}    -> {}", cref.usr());
                    } else {
                        unimplemented!();
                    }
                } else {
                    return Err(Error::FailedToGetTemplateRefFrom(c_typedef.display_name()));
                }
            }
        }

        let namespaces = get_namespaces_for_decl(*c_typedef, tu, ast, already_visited)?;

        let specialized_decl = specialized_decl
            .ok_or_else(|| Error::FailedToGetTemplateRefFrom(c_typedef.display_name()))?;

        trace!("Storing CTS with name: {name}, usr: {}, specializing: {specialized_decl}, template_args: {:?}", c_typedef.usr(), template_args);
        Ok(ClassTemplateSpecialization {
            specialized_decl,
            usr: c_typedef.usr(),
            name,
            template_arguments: template_args,
            namespaces,
        })
    } else {
        Err(Error::FailedToGetTypeFrom(c_typedef.display_name()))
    }
}

fn extract_template_args(
    c_type_alias_decl: &Cursor,
    depth: usize,
    ty: &Type,
    tu: &TranslationUnit,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
) -> Result<Vec<Option<TemplateType>>> {
    let num_args = ty.num_template_arguments();

    // Get any type template args on this type alias.
    let mut template_args = Vec::new();
    if num_args > 0 {
        template_args.reserve(num_args as usize);
        for i in 0..num_args {
            if let Ok(tty) = ty.template_argument_as_type(i as u32) {
                let qt = extract_type(tty.clone(), depth + 1, &[], already_visited, ast, tu)?;
                template_args.push(Some(TemplateType::Type(qt)));
            } else {
                // If it's not a type, we have to handle it separately, below.
                template_args.push(None);
            }
        }
    }

    let mut non_type_indices = Vec::new();
    for (i, arg) in template_args.iter().enumerate() {
        if arg.is_none() {
            non_type_indices.push(i)
        };
    }

    if !non_type_indices.is_empty() {
        // there are non-type template arguments. we'll have to walk the children of the TypeAliasDecl and try and
        // guess which ones they are...
        // The ordering should be:
        // 1. Namespaces
        // 2. TemplateRef -> ClassTemplate (presumably there could be other types referenced here too)
        // 3. Template arguments of *some kind*
        // 4. Everything else...
        // This will be buggy...
        let mut current = 0;
        for child in c_type_alias_decl.children() {
            match child.kind() {
                CursorKind::NamespaceRef | CursorKind::TemplateRef => (),
                CursorKind::IntegerLiteral => {
                    let value = tu.token(child.location()).spelling();
                    let index = non_type_indices[current];
                    template_args[index] = Some(TemplateType::Integer(value));
                    current += 1;
                }
                _ => unimplemented!(
                    "Unimplemented CursorKind {} when getting non-type template args",
                    child.kind()
                ),
            }

            if current == non_type_indices.len() {
                break;
            }
        }

        if current != non_type_indices.len() {
            return Err(Error::TemplateArgExtraction(
                c_type_alias_decl.display_name(),
            ));
        }
    }

    Ok(template_args)
}

#[derive(Debug)]
pub struct ClassTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones.
    ///
    /// Revisit and maybe we want to make that a hard error
    pub(crate) template_arguments: Vec<Option<TemplateType>>,
    /// The typedef itself is namespaced
    pub(crate) namespaces: Vec<USR>,
}

impl ClassTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[Option<TemplateType>] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn pretty_print(&self, depth: usize, ast: &AST) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ ClassTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        let class = ast.get_class(self.specialized_decl).unwrap();
        class.pretty_print(depth, ast, Some(self.template_arguments()));
    }

    pub fn format(&self, ast: &AST) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let class = ast.get_class(self.specialized_decl).unwrap();
        class.format(ast, Some(self.template_arguments()))
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }
}

#[derive(Debug)]
pub struct FunctionTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    /// Vec of options here because we know how many template arguments there are, but can't directly get any non-type
    /// ones.
    ///
    /// Revisit and maybe we want to make that a hard error
    pub(crate) template_arguments: Vec<Option<TemplateType>>,
    /// The typedef itself is namespaced
    pub(crate) namespaces: Vec<USR>,
}

impl FunctionTemplateSpecialization {
    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[Option<TemplateType>] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn pretty_print(
        &self,
        depth: usize,
        ast: &AST,
        outer_template_parameters: &[TemplateParameterDecl],
    ) {
        let indent = format!("{:width$}", "", width = depth * 2);

        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        println!(
            "+ FunctionTemplateSpecialization {}::{} of ({}) with <{}>",
            ns_string,
            self.name,
            self.usr,
            args.join(", ")
        );

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.pretty_print(
            depth,
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        );
    }

    pub fn format(&self, ast: &AST, outer_template_parameters: &[TemplateParameterDecl]) -> String {
        let args = self
            .template_arguments()
            .iter()
            .map(|a| format!("{:?}", a))
            .collect::<Vec<_>>();

        let ns_string = self
            .namespaces
            .iter()
            .map(|u| ast.get_namespace(*u).unwrap().name.clone())
            .collect::<Vec<String>>()
            .join("::");

        // this will be complicated...
        let function = ast.get_function(self.specialized_decl).unwrap();
        function.format(
            ast,
            outer_template_parameters,
            Some(self.template_arguments()),
        )
    }
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use env_logger::fmt::Color;
    use indoc::indoc;
    use log::Level;

    use crate::{class::ClassBindKind, error::Error, parse_string_and_extract_ast};

    #[test]
    fn extract_typealias_typedef() -> Result<(), Error> {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
            template <typename T, int N=4>
            class shared_ptr {
                T* t;

            public:
                const T* get() const;
                T* get();
            };

            class A {int a;};
            class B {int b;};

            using APtr = shared_ptr<A>;
            typedef shared_ptr<B> BPtr;

            using APtr2 = APtr;
            typedef BPtr BPtr2;
        "#
            ),
            &cli_args()?,
            true,
            None,
        )?;

        println!("{ast:?}");
        assert_eq!(
            format!("{ast:?}"),
            indoc!(
                r#"
                Namespace c:@ST>2#T#NI@shared_ptr shared_ptr<T, N> None
                ClassDecl c:@ST>2#T#NI@shared_ptr shared_ptr rename=None OpaquePtr is_pod=false ignore=false rof=[] template_parameters=[Type(T), Int(N=4)] specializations=[] namespaces=[]
                Method Method const=true virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get#1 get rename=None ignore=false return=const T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]
                Method Method const=false virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get# get rename=None ignore=false return=T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]

                ClassDecl c:@S@A A rename=None ValueType is_pod=true ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]

                ClassDecl c:@S@B B rename=None ValueType is_pod=true ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]

                ClassTemplateSpecialization c:@APtr APtr specializes=c:@ST>2#T#NI@shared_ptr template_arguments=[Some(Type(c:@S@A))] namespaces=[]
                ClassTemplateSpecialization c:ec50d40f103284de.cpp@T@BPtr BPtr specializes=c:@ST>2#T#NI@shared_ptr template_arguments=[Some(Type(c:@S@B))] namespaces=[]
        "#
            )
        );

        Ok(())
    }

    #[test]
    fn extract_pod_typedef() -> Result<(), Error> {
        run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                class Class_ {
                    int a;
                };

                typedef const Class_ Class;

                void take_class(Class& c);
            "#
                ),
                &cli_args()?,
                true,
                None,
            )?;

            println!("{ast:?}");
            assert_eq!(
                format!("{ast:?}"),
                indoc!(
                    r#"
                    ClassDecl c:@S@Class_ Class_ rename=None ValueType is_pod=true ignore=false rof=[] template_parameters=[] specializations=[] namespaces=[]

                    Function c:@F@take_class#&1$@S@Class_# take_class rename=None ignore=false return=void args=[c: Class &] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                    TypeAlias Class = Class_ const
                    "#
                )
            );

            Ok(())
        })
    }

    pub(crate) fn run_test<F>(closure: F) -> Result<(), Error>
    where
        F: FnOnce() -> Result<(), Error>,
    {
        use tracing::error;
        use tracing::level_filters::LevelFilter;
        /*
        use tracing_subscriber::fmt::format::FmtSpan;
        tracing_subscriber::fmt()
            .with_span_events(FmtSpan::NEW | FmtSpan::CLOSE)
            .with_max_level(LevelFilter::TRACE)
            .init();
        */

        init_log();

        let res = closure();

        res.map_err(|err| {
            error!("{err}");
            for e in source_iter(&err) {
                error!("  because: {e}")
            }

            err
        })
    }

    pub(crate) fn init_log() {
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

    pub(crate) fn source_iter(
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
