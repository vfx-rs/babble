use bbl_clang::cursor::USR;
use bbl_extract::{
    ast::AST,
    enm::Enum,
    index_map::{IndexMapKey, UstrIndexMap},
};

use crate::{build_namespace_prefix, error::Error};

type Result<T, E = Error> = std::result::Result<T, E>;

pub struct CEnum {
    name_external: String,
    name_internal: String,
    usr: USR,
    variants: Vec<(String, i64)>,
}

impl std::fmt::Debug for CEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CEnum {} {} [", self.name_internal, self.name_external)?;
        for var in &self.variants {
            write!(f, "{}={} ", var.0, var.1)?;
        }
        write!(f, "]")?;

        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct CEnumId(usize);

impl CEnumId {
    pub fn new(id: usize) -> CEnumId {
        CEnumId(id)
    }
}

impl IndexMapKey for CEnumId {
    fn get(&self) -> usize {
        self.0
    }
}

pub fn translate_enum(
    ast: &AST,
    enm: &Enum,
    enums: &mut UstrIndexMap<CEnum, CEnumId>,
) -> Result<(), Error> {
    let (ns_prefix_external, ns_prefix_internal) = build_namespace_prefix(ast, enm.namespaces())?;

    // we don't sgenerate unique names for typedefs, because it's perfectly fine to typedef one name to itself, and this
    // allows us to seamlessly handle manual implementations for typedef'd templated std types like std::string
    let enm_c_name_external = format!("{ns_prefix_external}{}", enm.name());
    let enm_c_name_internal = format!("{ns_prefix_internal}{}", enm.name());

    enums.insert(
        enm.usr().into(),
        CEnum {
            name_external: enm_c_name_external,
            name_internal: enm_c_name_internal,
            usr: enm.usr(),
            variants: enm.variants().to_vec(),
        },
    );

    Ok(())
}
