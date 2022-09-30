use bbl_clang::cursor::{Cursor, USR, CurEnumConstant};
use bbl_clang::cursor_kind::CursorKind;
use bbl_clang::translation_unit::TranslationUnit;
use bbl_clang::ty::Type;
use tracing::log::debug;

use crate::ast::{AST, get_namespaces_for_decl, get_qualified_name};
use crate::error::Error;

type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone)]
pub struct Enum {
    name: String,
    usr: USR,
    ty: Type,
    variants: Vec<(String, i64)>,
    namespaces: Vec<USR>,
}

impl std::fmt::Debug for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Enum {} {} [", self.name, self.usr)?;
        for var in &self.variants {
            write!(f, "{}={} ", var.0, var.1)?;
        }
        write!(f, "]");
        write!(f, " namespaces={:?}", self.namespaces);
        Ok(())
    }
}

impl Enum {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn usr(&self) -> USR {
        self.usr
    }

    pub fn variants(&self) -> &[(String, i64)] {
        &self.variants
    }

    pub fn get_qualified_name(&self, ast: &AST) -> Result<String> {
        get_qualified_name(self.name(), &self.namespaces, ast)
    }

    pub fn namespaces(&self) -> &[USR] {
        &self.namespaces
    }
}

pub fn extract_enum(c_enum: Cursor, ast: &mut AST, already_visited: &mut Vec<USR>, tu: &TranslationUnit) -> Result<USR> {
    let usr = c_enum.usr();
    if already_visited.contains(&usr) {
        debug!("extract_enum_decl: already visited {}", usr);
        return Ok(usr);
    } else {
        already_visited.push(usr);
    }

    let name = c_enum.spelling();
    let ty = c_enum.enum_decl_integer_type()?;
    let namespaces = get_namespaces_for_decl(c_enum, tu, ast, already_visited)?;
    let enum_decl_qualified_name = get_qualified_name(&name, &namespaces, ast)?;

    let mut variants = Vec::new();
    for child in c_enum.children_of_kind(CursorKind::EnumConstantDecl, false) {
        let c_constant: CurEnumConstant = child.try_into()?;
        variants.push((c_constant.spelling(), c_constant.value()))
    }

    ast.insert_enum(Enum {
        name,
        usr,
        ty,
        variants,
        namespaces,
    });

    Ok(usr)
}

#[cfg(test)]
mod tests {
    use bbl_clang::cli_args;
    use indoc::indoc;

    use crate::{class::ClassBindKind, error::Error, parse_string_and_extract_ast, AllowList};

    #[test]
    fn extract_enum() -> Result<(), Error> {
        let ast = parse_string_and_extract_ast(
            indoc!(
                r#"
                enum Numbered {
                    First = 1,
                    Second,
                    Third = 3,
                };

                enum Unnumbered {
                    First,
                    Second,
                    Third,
                };

                void take_enum(Numbered n, Unnumbered u);
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
                Namespace c:@E@Numbered Numbered None
                Namespace c:@E@Numbered@First First None
                Namespace c:@E@Numbered@Third Third None
                Function c:@F@take_enum#$@E@Numbered#$@E@Unnumbered# take_enum rename=None ignore=false return=void args=[n: Numbered, u: Unnumbered] noexcept=None template_parameters=[] specializations=[] namespaces=[]
                Enum Numbered c:@E@Numbered [First=1 Second=2 Third=3 ]
                Enum Unnumbered c:@E@Unnumbered []
    "#
            )
        );


        Ok(())
    }


}