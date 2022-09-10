use bbl_clang::ty::TypeKind;
use bbl_extract::{ast::AST, class::ClassBindKind, function::Method, index_map::IndexMapKey};
use bbl_translate::{
    cfunction::{CArgument, CFunction, CFunctionSource},
    cstruct::CStruct,
    ctype::{CQualType, CTypeRef},
    ctypedef::CTypedef,
    CAST,
};

use crate::error::{ArgumentError, Error, FunctionGenerationError, TypeError};
use tracing::{debug, instrument, trace};

use std::fmt::Write;

type Result<T, E = Error> = std::result::Result<T, E>;

