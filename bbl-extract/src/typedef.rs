use bbl_clang::translation_unit::TranslationUnit;
use tracing::{instrument, trace, warn};

use crate::ast::{get_namespaces_for_decl, get_qualified_name, AST};
use crate::class::OverrideList;
use crate::qualtype::{extract_type, QualType};
use crate::AllowList;
use bbl_clang::cursor::{CurTypedef, USR};
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
    class_overrides: &OverrideList,
    extra_template_parameters: &[String],
    stop_on_error: bool,
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
        extra_template_parameters,
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;

    let _ = ast.insert_type_alias(Typedef {
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
    use indoc::indoc;

    use crate::{class::OverrideList, parse_string_and_extract_ast, AllowList};

    #[test]
    fn extract_typealias_typedef() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
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
                &OverrideList::default(),
                true,
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@ST>2#T#NI@shared_ptr shared_ptr<T, N> None
                    ClassDecl c:@ST>2#T#NI@shared_ptr shared_ptr rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[Type(T), Int(N=4)] specializations=[([A, 4], c:@S@shared_ptr>#$@S@A#VI4), ([B, 4], c:@S@shared_ptr>#$@S@B#VI4)] namespaces=[]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get#1 get rename=None ignore=false return=const T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get# get rename=None ignore=false return=T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]

                    ClassDecl c:@S@A A rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]

                    ClassDecl c:@S@B B rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]

                    ClassDecl c:@S@shared_ptr>#$@S@A#VI4 shared_ptr<A, 4> rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[] specializations=[] namespaces=[]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get#1 get rename=None ignore=false return=A* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get# get rename=None ignore=false return=A* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]

                    ClassDecl c:@S@shared_ptr>#$@S@B#VI4 shared_ptr<B, 4> rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[] specializations=[] namespaces=[]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get#1 get rename=None ignore=false return=B* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@ST>2#T#NI@shared_ptr@F@get# get rename=None ignore=false return=B* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@ST>2#T#NI@shared_ptr]

                    TypeAlias APtr = shared_ptr<A>
                    TypeAlias BPtr = shared_ptr<B>
                    TypeAlias APtr2 = APtr
                    TypeAlias BPtr2 = BPtr
                    ClassTemplateSpecialization c:@S@shared_ptr>#$@S@A#VI4 shared_ptr<A, 4> specialized_decl=c:@ST>2#T#NI@shared_ptr template_arguments=[A, 4] namespaces=[]
                    ClassTemplateSpecialization c:@S@shared_ptr>#$@S@B#VI4 shared_ptr<B, 4> specialized_decl=c:@ST>2#T#NI@shared_ptr template_arguments=[B, 4] namespaces=[]
                "#
                ),
            )
        })
    }

    #[test]
    fn extract_pod_typedef() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
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
                &OverrideList::default(),
                true,
            )?;

            println!("{ast:?}");
            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    ClassDecl c:@S@Class_ Class_ rename=None OpaquePtr is_pod=false ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]

                    Function c:@F@take_class#&1$@S@Class_# take_class rename=None ignore=false return=void args=[c: Class &] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                    TypeAlias Class = Class_ const
                    "#
                ),
            )
        })
    }
}
