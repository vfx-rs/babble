use crate::{
    ast::AST,
    class::{self, ClassDecl, AccessSpecifier, extract_field},
    cursor::USR,
    cursor_kind::CursorKind,
    function::extract_method,
    record::Record,
    template_argument::{TemplateParameterDecl, TemplateType},
    Cursor, TranslationUnit, class_template,
};
use log::{debug, error};
use std::fmt::Display;

use crate::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;
