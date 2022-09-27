use bbl_clang::cursor_kind::CursorKind;
use bbl_clang::template_argument::TemplateArgumentKind;
use bbl_clang::translation_unit::TranslationUnit;
use std::convert::TryInto;
use std::fmt::Display;
use tracing::{debug, error, info, instrument, trace, warn};

use crate::AllowList;
use crate::ast::{dump_cursor, get_namespaces_for_decl, get_qualified_name, TypeAliasId, AST};
use crate::class::extract_class_decl;
use crate::namespace::extract_namespace;
use crate::qualtype::{extract_type, QualType};
use crate::stdlib::create_std_string;
use crate::templates::{TemplateArgument, TemplateParameterDecl};
use bbl_clang::cursor::{CurClassDecl, CurClassTemplate, CurTemplateRef, CurTypedef, Cursor, USR};
use bbl_clang::ty::Type;
use std::fmt::Debug;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

pub struct Typedef {
    name: String,
    usr: USR,
    namespaces: Vec<USR>,
    underlying_type: QualType,
}

impl Debug for Typedef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeAlias {} = {:?}", self.name, self.underlying_type)
    }
}

impl Typedef {
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
        println!(
            "+ Typedef {} {} = {}",
            self.usr,
            get_qualified_name(&self.name, &self.namespaces, ast).unwrap(),
            self.underlying_type.format(ast, &[], None)
        )
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }
}

#[instrument(skip(already_visited, ast, tu), level = "trace")]
pub fn extract_typedef_decl<'a>(
    c_typedef: CurTypedef,
    already_visited: &mut Vec<USR>,
    ast: &'a mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
) -> Result<USR> {
    let usr = c_typedef.usr();
    if already_visited.contains(&usr) {
        trace!("already visiting. skipping.");
        return Ok(usr);
    } else {
        already_visited.push(usr);
    }

    let name = c_typedef.display_name();
    let namespaces = get_namespaces_for_decl(c_typedef.into(), tu, ast, already_visited)?;

    let underlying_type = extract_type(
        c_typedef.underlying_type()?,
        &[],
        already_visited,
        ast,
        tu,
        allow_list,
    )?;

    let id = ast.insert_type_alias(Typedef {
        name,
        usr,
        namespaces,
        underlying_type,
    });

    Ok(usr)
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use env_logger::fmt::Color;
    use indoc::indoc;
    use log::Level;

    use crate::{class::ClassBindKind, error::Error, parse_string_and_extract_ast, AllowList};

    #[test]
    fn extract_typealias_typedef() -> Result<(), Error> {
        run_test(|| {
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
                &AllowList::default(),
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

                    TypeAlias APtr = shared_ptr<A>
                    TypeAlias BPtr = shared_ptr<B>
                    TypeAlias APtr2 = APtr
                    TypeAlias BPtr2 = BPtr
                    ClassTemplateSpecialization c:@S@shared_ptr>#$@S@A#VI4 shared_ptr_A_4_ specialized_decl=c:@ST>2#T#NI@shared_ptr template_arguments=[A, 4] namespaces=[]
                    ClassTemplateSpecialization c:@S@shared_ptr>#$@S@B#VI4 shared_ptr_B_4_ specialized_decl=c:@ST>2#T#NI@shared_ptr template_arguments=[B, 4] namespaces=[]
            "#
                )
            );

            Ok(())
        })
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
                &AllowList::default(),
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
