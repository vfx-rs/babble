#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use bbl_clang::{
    cursor::{CurClassDecl, CurClassTemplate, USR},
    template_argument::TemplateArgumentKind,
    translation_unit::TranslationUnit,
};
use bbl_util::Trace;
use hashbrown::HashSet;
use tracing::log::debug;

use crate::{
    ast::{dump_cursor_until, get_namespaces_for_decl, get_qualified_name, AST},
    class::{extract_class_decl, ClassBindKind, ClassDecl, NeedsImplicit, OverrideList},
    qualtype::{extract_type, QualType},
    AllowList,
};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use std::fmt::{Debug, Display};

pub fn extract_class_template_specialization(
    c_class_decl: CurClassDecl,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    specialize_immediately: bool,
    stop_on_error: bool,
) -> Result<USR> {
    debug!(
        "extract_class_template_specialization: {c_class_decl:?} {}",
        c_class_decl.is_definition()
    );

    if already_visited.contains(&c_class_decl.usr()) {
        return Ok(c_class_decl.usr());
    } else {
        already_visited.push(c_class_decl.usr());
    }

    let template_arguments = extract_template_args(
        c_class_decl,
        already_visited,
        ast,
        tu,
        allow_list,
        class_overrides,
        stop_on_error,
    )?;
    let namespaces = get_namespaces_for_decl(c_class_decl.into(), tu, ast, already_visited)?;

    debug!("template arguments {:?}", template_arguments);

    let needs_implicit = NeedsImplicit {
        ctor: c_class_decl.cxxrecord_needs_implicit_default_constructor(),
        copy_ctor: c_class_decl.cxxrecord_needs_implicit_copy_constructor(),
        move_ctor: c_class_decl.cxxrecord_needs_implicit_move_constructor(),
        copy_assign: c_class_decl.cxxrecord_needs_implicit_copy_assignment(),
        move_assign: c_class_decl.cxxrecord_needs_implicit_move_assignment(),
        dtor: c_class_decl.cxxrecord_needs_implicit_destructor(),
    };

    let is_pod = c_class_decl.ty()?.is_pod();

    let specialized_decl: CurClassTemplate = c_class_decl
        .specialized_template()
        .map_err(|_| Error::ClassDeclIsNotSpecialization {
            usr: c_class_decl.usr(),
            source: Trace::new(),
        })?
        .try_into()
        .map_err(|e| {
            dump_cursor_until(*c_class_decl, tu, 4);
            e
        })?;
    debug!("extract_class_template_specialization: got specialized decl {specialized_decl:?}");

    let name = c_class_decl.display_name();

    let u_specialized = extract_class_decl(
        specialized_decl.as_class_decl(),
        tu,
        ast,
        already_visited,
        allow_list,
        class_overrides,
        None,
        specialize_immediately,
        stop_on_error,
    )
    .map_err(|e| Error::FailedToExtractClassTemplate {
        usr: specialized_decl.usr(),
        source: Box::new(e),
    })?;

    let cts = ClassTemplateSpecialization {
        specialized_decl: specialized_decl.usr(),
        usr: c_class_decl.usr(),
        name: c_class_decl.display_name(),
        template_arguments: template_arguments.clone(),
        namespaces,
        needs_implicit,
        is_pod,
    };

    // let class_template = ast.get_class_mut(specialized_decl.usr()).expect(&format!(
    //     "Could not extract just inserted class template {:?}",
    //     specialized_decl.usr()
    // ));

    if ast.get_class(specialized_decl.usr()).is_none() {
        println!("{ast:?}");
        println!("--- {u_specialized}");
        println!("--- {}", specialized_decl.usr());
        println!(" in {c_class_decl:?}");
        panic!();
    }

    let class_template =
        ast.get_class_mut(specialized_decl.usr())
            .ok_or_else(|| Error::ClassTemplateNotFound {
                usr: specialized_decl.usr(),
                source: Trace::new(),
            })?;

    debug!("Adding specialization of {c_class_decl:?} with {template_arguments:?}");
    class_template.add_specialization(template_arguments, c_class_decl.usr());

    if specialize_immediately {
        let class_template = ast
            .get_class(specialized_decl.usr())
            .expect("Could not extract just inserted class template");

        let sd = specialize_class_template(class_template, &cts, ast)?;
        println!("Inserting {}", sd.usr());
        ast.insert_class(sd);
    }

    // Delay the actual specialization until after the user's had a chance to muck with the AST
    ast.insert_class_template_specialization(cts);

    Ok(c_class_decl.usr())
}

/// Monomorphize a class template into a ClassDecl with all the template parameters replaced by concrete types
pub fn specialize_class_template(
    class_template: &ClassDecl,
    cts: &ClassTemplateSpecialization,
    ast: &AST,
) -> Result<ClassDecl> {
    let usr = cts.usr();
    let name = cts.name().to_string();

    let mut fields = Vec::new();
    let mut methods = Vec::new();
    let namespaces = cts.namespaces().to_vec();
    let template_parameters = class_template.template_parameters().to_vec();
    let template_arguments = cts.template_arguments().to_vec();
    let is_pod = cts.is_pod(); // TODO(AL): need to re-evaluate this from the specialization
    let needs_implicit = cts.needs_implicit.clone();

    for tmpl_field in class_template.fields() {
        let mut spec_field = tmpl_field.clone();

        let mut matched_parameters = HashSet::new();
        if spec_field.qual_type.is_template(ast) {
            spec_field.qual_type.replace_templates(
                &template_parameters,
                &template_arguments,
                &mut matched_parameters,
                ast,
            )?;
        }

        fields.push(spec_field);
    }

    for tmpl_method in class_template.methods() {
        let mut spec_method = tmpl_method.clone();

        spec_method.replace_templates(&template_parameters, &template_arguments, ast)?;

        methods.push(spec_method);
    }

    let cd = ClassDecl::new(
        usr,
        name,
        fields,
        methods,
        namespaces,
        vec![],
        is_pod,
        needs_implicit,
    );

    Ok(cd)
}

pub fn extract_template_args(
    c_class_decl: CurClassDecl,
    already_visited: &mut Vec<USR>,
    ast: &mut AST,
    tu: &TranslationUnit,
    allow_list: &AllowList,
    class_overrides: &OverrideList,
    stop_on_error: bool,
) -> Result<Vec<TemplateArgument>> {
    let mut result = Vec::new();
    let num_template_args = c_class_decl.num_template_arguments();
    if num_template_args < 1 {
        return Err(Error::TooFewTemplateArguments {
            usr: c_class_decl.usr(),
            num: num_template_args,
            source: Trace::new(),
        });
    }

    for i in 0..num_template_args {
        match c_class_decl.template_argument_kind(i as u32)? {
            TemplateArgumentKind::Type => {
                let ty = c_class_decl.template_argument_type(i as u32)?;
                result.push(TemplateArgument::Type(extract_type(
                    ty,
                    &[],
                    already_visited,
                    ast,
                    tu,
                    allow_list,
                    class_overrides,
                    stop_on_error,
                )?))
            }
            TemplateArgumentKind::Integral => result.push(TemplateArgument::Integral(
                c_class_decl.template_argument_value(i as u32),
            )),
            _ => unimplemented!("Not handling template arguments other than int or type"),
        }
    }

    Ok(result)
}

pub struct ClassTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) template_arguments: Vec<TemplateArgument>,
    pub(crate) namespaces: Vec<USR>,
    needs_implicit: NeedsImplicit,
    is_pod: bool,
}

impl Debug for ClassTemplateSpecialization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ClassTemplateSpecialization {usr} {name} specialized_decl={specialized_decl} template_arguments={template_arguments:?} namespaces={namespaces:?}",
            usr=self.usr,
            name=self.name(),
            specialized_decl = self.specialized_decl,
            template_arguments = self.template_arguments(),
            namespaces = self.namespaces(),
        )
    }
}

impl ClassTemplateSpecialization {
    pub fn new(
        specialized_decl: USR,
        usr: USR,
        name: &str,
        template_arguments: Vec<TemplateArgument>,
        namespaces: Vec<USR>,
        needs_implicit: NeedsImplicit,
        is_pod: bool,
    ) -> Self {
        ClassTemplateSpecialization {
            specialized_decl,
            usr,
            name: name.to_string(),
            template_arguments,
            namespaces,
            needs_implicit,
            is_pod,
        }
    }

    pub fn specialized_decl(&self) -> USR {
        self.specialized_decl
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn template_arguments(&self) -> &[TemplateArgument] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }

    pub fn bind_kind(&self, ast: &AST) -> Result<ClassBindKind> {
        ast.get_class(self.specialized_decl)
            .map(|class| *class.bind_kind())
            .ok_or_else(|| Error::ClassNotFound {
                name: self.specialized_decl.to_string(),
                source: Trace::new(),
            })
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }

    pub fn needs_implicit(&self) -> &NeedsImplicit {
        &self.needs_implicit
    }

    pub fn is_pod(&self) -> bool {
        self.is_pod
    }
}

#[derive(Debug)]
pub struct FunctionTemplateSpecialization {
    pub(crate) specialized_decl: USR,
    pub(crate) usr: USR,
    pub(crate) name: String,
    pub(crate) template_arguments: Vec<TemplateArgument>,
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

    pub fn template_arguments(&self) -> &[TemplateArgument] {
        &self.template_arguments
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TemplateArgument {
    Null,
    Type(QualType),
    Declaration,
    NullPtr,
    Integral(i64),
    Template,
    TemplateExpansion,
    Expression,
    Pack,
}

impl Debug for TemplateArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TemplateArgument::*;
        match self {
            Null => write!(f, "Null"),
            Type(qt) => write!(f, "{qt:?}"),
            Declaration => write!(f, "Declaration"),
            NullPtr => write!(f, "NullPtr"),
            Integral(i) => write!(f, "{i}"),
            Template => write!(f, "Template"),
            TemplateExpansion => write!(f, "TemplateExpansion"),
            Expression => write!(f, "Expression"),
            Pack => write!(f, "Pack"),
        }
    }
}

/// A template parameter as defined in a class- or function-template declaration
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TemplateParameterDecl {
    Type {
        name: String,
        index: usize,
    },
    Integer {
        name: String,
        index: usize,
        default: Option<String>,
    },
}

impl TemplateParameterDecl {
    pub fn typ(name: &str, index: usize) -> TemplateParameterDecl {
        TemplateParameterDecl::Type {
            name: name.into(),
            index,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            TemplateParameterDecl::Type { name, .. } => name,
            TemplateParameterDecl::Integer { name, .. } => name,
        }
    }

    pub fn default_name(&self) -> String {
        match self {
            TemplateParameterDecl::Type { name, .. } => format!("typename {name}"),
            TemplateParameterDecl::Integer { name, .. } => format!("integer {name}"),
        }
    }

    pub fn index(&self) -> usize {
        match self {
            TemplateParameterDecl::Type { index, .. } => *index,
            TemplateParameterDecl::Integer { index, .. } => *index,
        }
    }
}

impl Display for TemplateParameterDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateParameterDecl::Type { name, .. } => write!(f, "{name}"),
            TemplateParameterDecl::Integer { name, default, .. } => {
                write!(f, "{name}")?;
                if let Some(default) = default {
                    write!(f, "={default}")?;
                }
                Ok(())
            }
        }
    }
}

impl Debug for TemplateParameterDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateParameterDecl::Type { name, .. } => write!(f, "Type({name})"),
            TemplateParameterDecl::Integer { name, default, .. } => {
                write!(f, "Int({name}")?;
                if let Some(default) = default {
                    write!(f, "={default}")?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use bbl_clang::{cli_args, cli_args_with};
    use indoc::indoc;

    use crate::{class::OverrideList, parse_string_and_extract_ast, AllowList};

    #[test]
    fn extract_template_template() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    namespace detail {
                    template <typename T>
                    struct Vec {
                        T x;
                        T y;
                    };

                    typedef Vec<float> V2f;

                    template <class V>
                    struct Box {
                        V _min;
                        V _max;

                        V min() const;
                        V max() const;
                    };

                    typedef Box<V2f> Box2f;
                    }

                    void take_vec(const detail::V2f&);
                    void take_box(const detail::Box2f&);
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![r"^take_".to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");

            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@N@detail detail None
                    ClassDecl c:@N@detail@ST>1#T@Vec Vec rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[Type(T)] specializations=[([float], c:@N@detail@S@Vec>#f)] namespaces=[c:@N@detail]
                    Field x: T
                    Field y: T

                    ClassDecl c:@N@detail@ST>1#T@Box Box rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[Type(V)] specializations=[([detail::Vec<float>], c:@N@detail@S@Box>#$@N@detail@S@Vec>#f)] namespaces=[c:@N@detail]
                    Field _min: V
                    Field _max: V

                    ClassDecl c:@N@detail@S@Vec>#f Vec<float> rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[] specializations=[] namespaces=[c:@N@detail]
                    Field x: float
                    Field y: float

                    ClassDecl c:@N@detail@S@Box>#$@N@detail@S@Vec>#f Box<detail::Vec<float>> rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[] specializations=[] namespaces=[c:@N@detail]
                    Field _min: detail::Vec<float>
                    Field _max: detail::Vec<float>

                    Function c:@F@take_vec#&1$@N@detail@S@Vec>#f# take_vec rename=None ignore=false return=void args=[: const detail::V2f &] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                    Function c:@F@take_box#&1$@N@detail@S@Box>#$@N@detail@S@Vec>#f# take_box rename=None ignore=false return=void args=[: const detail::Box2f &] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                    TypeAlias V2f = Vec<float>
                    TypeAlias Box2f = Box<V2f>
                    ClassTemplateSpecialization c:@N@detail@S@Vec>#f Vec<float> specialized_decl=c:@N@detail@ST>1#T@Vec template_arguments=[float] namespaces=[c:@N@detail]
                    ClassTemplateSpecialization c:@N@detail@S@Box>#$@N@detail@S@Vec>#f Box<detail::Vec<float>> specialized_decl=c:@N@detail@ST>1#T@Box template_arguments=[detail::Vec<float>] namespaces=[c:@N@detail]
                    "#
                ),
            )
        })
    }

    #[test]
    fn extract_class_template_parameter() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    namespace detail {
                    template <typename T>
                    struct Vec {
                        T x;
                        T y;
                    };
                    }

                    class Class {
                    public:
                        template <typename T>
                        float take_vec(const detail::Vec<T>&);
                    };
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![r"^Class".to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");

            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Namespace c:@S@Class Class None
                    Namespace c:@N@detail detail None
                    ClassDecl c:@N@detail@ST>1#T@Vec Vec rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[Type(T)] specializations=[] namespaces=[c:@N@detail]
                    Field x: T
                    Field y: T

                    ClassDecl c:@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@S@Class@FT@>1#Ttake_vec#&1>@N@detail@ST>1#T@Vec1t0.0#f# take_vec rename=None ignore=false return=float args=[: const detail::Vec<T> &] noexcept=None template_parameters=[Type(T)] specializations=[] namespaces=[c:@S@Class]

                    "#
                ),
            )
        })
    }

    #[test]
    fn extract_nested_template() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    #include <memory>

                    namespace Test {
                        template <class T>
                        struct HandleTo {
                            typedef std::unique_ptr<T> Handle;
                        };

                        class Class;
                        typedef HandleTo<Class>::Handle ClassHandle;
                        class Class {
                        public:
                            ClassHandle create();
                        };

                    }
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![r#"^Test::.*$"#.to_string()]),
                &OverrideList::default(),
                true,
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");

            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Include { name: "memory", bracket: "<" }
                    Namespace c:@N@Test Test None
                    Namespace c:@N@Test@S@HandleTo>#$@N@Test@S@Class HandleTo<Test::Class> None
                    Namespace c:@N@std std None
                    Namespace c:@N@Test@S@Class Class None
                    ClassDecl c:@N@Test@ST>1#T@HandleTo HandleTo rename=None OpaquePtr is_pod=false ignore=false needs=[] template_parameters=[Type(T)] specializations=[] namespaces=[c:@N@Test]

                    ClassDecl c:@N@std@ST>2#T#T@unique_ptr unique_ptr rename=None OpaquePtr is_pod=false ignore=false needs=[dtor ] template_parameters=[Type(T)] specializations=[([Test::Class], c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_)] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_ctor_default unique_ptr rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_const get rename=Some("get") ignore=false return=const T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_mut get rename=Some("get_mut") ignore=false return=T * args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]

                    ClassDecl c:@N@Test@S@Class Class rename=None ValueType is_pod=true ignore=false needs=[ctor cctor mctor cass mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@Test]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function c:@N@Test@S@Class@F@create# create rename=None ignore=false return=ClassHandle args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@Test, c:@N@Test@S@Class]

                    ClassDecl c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_ unique_ptr<Test::Class> rename=None OpaquePtr is_pod=false ignore=false needs=[mctor mass dtor ] template_parameters=[] specializations=[] namespaces=[c:@N@std]
                    Method Constructor deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_ctor_default unique_ptr rename=Some("ctor") ignore=false return=void args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=true virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_const get rename=Some("get") ignore=false return=Test::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]
                    Method Method deleted=false const=false virtual=false pure_virtual=false specializations=[] Function BBL:unique_ptr_get_mut get rename=Some("get_mut") ignore=false return=Test::Class* args=[] noexcept=None template_parameters=[] specializations=[] namespaces=[c:@N@std, c:@N@std@ST>2#T#T@unique_ptr]

                    TypeAlias Handle = std::unique_ptr<Class>
                    TypeAlias ClassHandle = HandleTo<Class>::Handle
                    ClassTemplateSpecialization c:@N@std@S@unique_ptr>#$@N@Test@S@Class#$@N@std@S@default_delete>#S0_ unique_ptr<Test::Class> specialized_decl=c:@N@std@ST>2#T#T@unique_ptr template_arguments=[Test::Class] namespaces=[c:@N@std]
                    "#
                ),
            )
        })
    }

    /*
    #[test]
    fn extract_inherited_template() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    #include <string>

                    namespace detail {
                        template <typename Key, typename Value>
                        class map {
                        public:
                            typedef Key key_type;
                            typedef Value value_type;
                            typedef int size_type;

                            void insert(key_type k, value_type v);
                            const value_type& at(const key_type& k);
                            size_type erase(const key_type& k);
                        };
                    }

                    namespace Test {
                        class Class : private detail::map<int, int> {
                        public:
                            using map::key_type;
                            using map::value_type;
                            using map::size_type;

                            using map::map;
                            using map::insert;
                            using map::at;

                            size_type erase(const key_type& key) {
                                return map::erase(key);
                            }
                        };
                    }
                "#
                ),
                &cli_args_with(&["-std=c++14"])?,
                true,
                None,
                &AllowList::new(vec![r#"^Test::.*$"#.to_string()]),
                &OverrideList::default(),
            )?;

            let ast = ast.monomorphize()?;

            println!("{ast:?}");

            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    "#
                ),
            )
        })
    }
    */

    /*
    #[test]
    fn extract_enable_if() -> bbl_util::Result<()> {
        bbl_util::run_test(|| {
            let ast = parse_string_and_extract_ast(
                indoc!(
                    r#"
                    #include <type_traits>

                    enum X { value1 = true, value2 = true };

                    template<class T>
                    typename std::enable_if_t<T::value1, int>::type
                    func(){return 0;}
                "#
                ),
                &cli_args()?,
                true,
                None,
                &AllowList::new(vec![r#"^func\(\)$"#.to_string()])
            )?;

            println!("{ast:?}");

            bbl_util::compare(
                &format!("{ast:?}"),
                indoc!(
                    r#"
                    Function c:@F@take_enum#$@E@Numbered#$@E@Unnumbered# take_enum rename=None ignore=false return=void args=[n: Numbered, u: Unnumbered] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                    Enum Numbered c:@E@Numbered [First=1 Second=2 Third=3 ] namespaces=[]
                    Enum Unnumbered c:@E@Unnumbered [First=0 Second=1 Third=2 ] namespaces=[]
                    "#
                ),
            )
        })
    }
    */
}
