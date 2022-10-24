use crate::{
    access_specifier::AccessSpecifier, exception::ExceptionSpecificationKind,
    printing_policy::PrintingPolicy, template_argument::TemplateArgumentKind,
    token::SourceLocation,
};

use super::cursor_kind::CursorKind;
use super::string::CXStringEx;
use bbl_util::Trace;
use clang_sys::{
    clang_CXXConstructor_isConvertingConstructor, clang_CXXConstructor_isCopyConstructor,
    clang_CXXConstructor_isDefaultConstructor, clang_CXXConstructor_isMoveConstructor,
    clang_CXXMethod_isConst, clang_CXXMethod_isDefaulted, clang_CXXMethod_isDeleted,
    clang_CXXMethod_isPureVirtual, clang_CXXMethod_isStatic, clang_CXXMethod_isVirtual,
    clang_CXXRecord_isAbstract, clang_CXXRecord_needsImplicitCopyAssignment,
    clang_CXXRecord_needsImplicitCopyConstructor, clang_CXXRecord_needsImplicitDefaultConstructor,
    clang_CXXRecord_needsImplicitDestructor, clang_CXXRecord_needsImplicitMoveAssignment,
    clang_CXXRecord_needsImplicitMoveConstructor, clang_Cursor_getArgument,
    clang_Cursor_getNumArguments, clang_Cursor_getNumTemplateArguments,
    clang_Cursor_getTemplateArgumentKind, clang_Cursor_getTemplateArgumentType,
    clang_Cursor_getTemplateArgumentUnsignedValue, clang_Cursor_getTemplateArgumentValue,
    clang_Cursor_getVarDeclInitializer, clang_Cursor_hasVarDeclExternalStorage,
    clang_Cursor_hasVarDeclGlobalStorage, clang_equalCursors, clang_getCXXAccessSpecifier,
    clang_getCanonicalCursor, clang_getCursorDefinition, clang_getCursorDisplayName,
    clang_getCursorExceptionSpecificationType, clang_getCursorKind, clang_getCursorLocation,
    clang_getCursorPrettyPrinted, clang_getCursorPrintingPolicy, clang_getCursorReferenced,
    clang_getCursorResultType, clang_getCursorSemanticParent, clang_getCursorSpelling,
    clang_getCursorType, clang_getCursorUSR, clang_getEnumConstantDeclUnsignedValue,
    clang_getEnumConstantDeclValue, clang_getEnumDeclIntegerType, clang_getNullCursor,
    clang_getNumOverloadedDecls, clang_getOverloadedDecl, clang_getSpecializedCursorTemplate,
    clang_getTypedefDeclUnderlyingType, clang_isCursorDefinition, clang_isInvalid,
    clang_visitChildren, CXChildVisitResult, CXChildVisit_Break, CXChildVisit_Continue,
    CXChildVisit_Recurse, CXClientData, CXCursor,
};
use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
    ops::Deref,
    os::raw::{c_longlong, c_ulonglong, c_void},
};

use crate::ty::{to_type, Type};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

use ustr::Ustr;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct USR(Ustr);

impl USR {
    pub fn new(s: &str) -> USR {
        USR(Ustr::from(s))
    }

    pub fn is_empty(&self) -> bool {
        self.0.as_str().is_empty()
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for USR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for USR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<USR> for Ustr {
    fn from(u: USR) -> Self {
        u.0
    }
}

impl AsRef<Ustr> for USR {
    fn as_ref(&self) -> &Ustr {
        &self.0
    }
}

#[derive(Copy, Clone)]
pub struct Cursor {
    pub(crate) inner: CXCursor,
}

impl Debug for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} {}", self.display_name(), self.kind(), self.usr())
    }
}

impl PartialEq for Cursor {
    fn eq(&self, other: &Self) -> bool {
        unsafe { clang_equalCursors(self.inner, other.inner) != 0 }
    }
}

impl Eq for Cursor {}

impl Cursor {
    pub fn visit_children<F>(&self, callback: F)
    where
        F: FnMut(Cursor, Cursor) -> ChildVisitResult,
    {
        unsafe {
            let (closure, trampoline) = unpack_cursor_visitor_closure(callback);
            clang_visitChildren(self.inner, trampoline, closure);
        }
    }

    pub fn children(&self) -> Vec<Cursor> {
        let mut children = Vec::new();
        self.visit_children(|c, _| {
            children.push(c);
            ChildVisitResult::Continue
        });

        children
    }

    /// Check whether this Cursor has a child of the given kind
    pub fn has_child_of_kind(&self, kind: CursorKind) -> bool {
        let mut result = false;
        self.visit_children(|c, _| {
            if c.kind() == kind {
                result = true;
                ChildVisitResult::Break
            } else {
                ChildVisitResult::Continue
            }
        });

        result
    }

    pub fn kind(&self) -> CursorKind {
        unsafe { clang_getCursorKind(self.inner).into() }
    }

    pub fn display_name(&self) -> String {
        unsafe { clang_getCursorDisplayName(self.inner).to_string() }
    }

    pub fn spelling(&self) -> String {
        unsafe { clang_getCursorSpelling(self.inner).to_string() }
    }

    pub fn usr(&self) -> USR {
        unsafe { USR(clang_getCursorUSR(self.inner).to_ustr()) }
    }

    pub fn is_definition(&self) -> bool {
        unsafe { clang_isCursorDefinition(self.inner) != 0 }
    }

    pub fn definition(&self) -> Result<Cursor> {
        unsafe {
            let cx = clang_getCursorDefinition(self.inner);
            cursor(cx)
        }
    }

    pub fn has_var_decl_global_storage(&self) -> bool {
        unsafe { clang_Cursor_hasVarDeclGlobalStorage(self.inner) != 0 }
    }

    pub fn has_var_decl_external_storage(&self) -> bool {
        unsafe { clang_Cursor_hasVarDeclExternalStorage(self.inner) != 0 }
    }

    pub fn var_decl_initializer(&self) -> Result<Cursor> {
        unsafe {
            let inner = clang_Cursor_getVarDeclInitializer(self.inner);
            cursor(inner)
        }
    }

    pub fn location(&self) -> SourceLocation {
        unsafe {
            let s = clang_getCursorLocation(self.inner);
            SourceLocation { inner: s }
        }
    }

    pub fn referenced(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getCursorReferenced(self.inner)) }
    }

    pub fn specialized_template(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getSpecializedCursorTemplate(self.inner)) }
    }

    pub fn children_of_kind(&self, kind: CursorKind, recursive: bool) -> Vec<Cursor> {
        let mut result = Vec::new();
        self.visit_children(|c, _| {
            if c.kind() == kind {
                result.push(c);
            }

            if recursive {
                ChildVisitResult::Recurse
            } else {
                ChildVisitResult::Continue
            }
        });

        result
    }

    pub fn first_child_of_kind(&self, kind: CursorKind) -> Option<Cursor> {
        let mut result = None;
        self.visit_children(|c, _| {
            if c.kind() == kind {
                result = Some(c);
                ChildVisitResult::Break
            } else {
                ChildVisitResult::Continue
            }
        });

        result
    }

    pub fn children_of_kind_with_name(
        &self,
        kind: CursorKind,
        name: &str,
        recursive: bool,
    ) -> Vec<Cursor> {
        let mut result = Vec::new();
        self.visit_children(|c, _| {
            if c.kind() == kind && c.display_name() == name {
                result.push(c);
            }

            if recursive {
                ChildVisitResult::Recurse
            } else {
                ChildVisitResult::Continue
            }
        });

        result
    }

    pub fn num_template_arguments(&self) -> i32 {
        unsafe { clang_Cursor_getNumTemplateArguments(self.inner) }
    }

    pub fn template_argument_kind(&self, i: u32) -> Result<TemplateArgumentKind> {
        unsafe { clang_Cursor_getTemplateArgumentKind(self.inner, i).try_into() }
    }

    pub fn template_argument_type(&self, i: u32) -> Result<Type> {
        unsafe { to_type(clang_Cursor_getTemplateArgumentType(self.inner, i)) }
    }

    pub fn template_argument_value(&self, i: u32) -> c_longlong {
        unsafe { clang_Cursor_getTemplateArgumentValue(self.inner, i) }
    }

    pub fn template_argument_unsigned_value(&self, i: u32) -> c_ulonglong {
        unsafe { clang_Cursor_getTemplateArgumentUnsignedValue(self.inner, i) }
    }

    pub fn ty(&self) -> Result<Type> {
        unsafe { to_type(clang_getCursorType(self.inner)) }
    }

    pub fn result_ty(&self) -> Result<Type> {
        unsafe { to_type(clang_getCursorResultType(self.inner)) }
    }

    pub fn num_arguments(&self) -> Result<u32> {
        unsafe {
            let n = clang_Cursor_getNumArguments(self.inner);
            if n < 0 {
                Err(Error::InvalidCursor)
            } else {
                Ok(n as u32)
            }
        }
    }

    pub fn argument(&self, i: u32) -> Result<Cursor> {
        unsafe { cursor(clang_Cursor_getArgument(self.inner, i)) }
    }

    pub fn cxx_method_is_const(&self) -> bool {
        unsafe { clang_CXXMethod_isConst(self.inner) != 0 }
    }

    pub fn cxx_method_is_static(&self) -> bool {
        unsafe { clang_CXXMethod_isStatic(self.inner) != 0 }
    }

    pub fn cxx_method_is_defaulted(&self) -> bool {
        unsafe { clang_CXXMethod_isDefaulted(self.inner) != 0 }
    }

    pub fn cxx_method_is_deleted(&self) -> bool {
        unsafe { clang_CXXMethod_isDeleted(self.inner) != 0 }
    }

    pub fn cxx_method_is_virtual(&self) -> bool {
        unsafe { clang_CXXMethod_isVirtual(self.inner) != 0 }
    }

    pub fn cxx_method_is_pure_virtual(&self) -> bool {
        unsafe { clang_CXXMethod_isPureVirtual(self.inner) != 0 }
    }

    pub fn cxx_method_is_copy_assignment_operator(&self) -> bool {
        // TODO(AL): implement this properly once we've patched libclang to implement this function
        // unsafe { clang_CXXMethod_isCopyAssignmentOperator(self.inner) != 0 }
        false
    }

    pub fn cxx_method_is_move_assignment_operator(&self) -> bool {
        // TODO(AL): implement this properly once we've patched libclang to implement this function
        // unsafe { clang_CXXMethod_isMoveAssignmentOperator(self.inner) != 0 }
        false
    }

    pub fn cxx_record_is_abstract(&self) -> bool {
        unsafe { clang_CXXRecord_isAbstract(self.inner) != 0 }
    }

    pub fn cxx_access_specifier(&self) -> Result<AccessSpecifier> {
        unsafe { clang_getCXXAccessSpecifier(self.inner).try_into() }
    }

    pub fn cxx_constructor_is_converting_constructor(&self) -> bool {
        unsafe { clang_CXXConstructor_isConvertingConstructor(self.inner) != 0 }
    }

    pub fn cxx_constructor_is_copy_constructor(&self) -> bool {
        unsafe { clang_CXXConstructor_isCopyConstructor(self.inner) != 0 }
    }

    pub fn cxx_constructor_is_move_constructor(&self) -> bool {
        unsafe { clang_CXXConstructor_isMoveConstructor(self.inner) != 0 }
    }

    pub fn cxx_constructor_is_default_constructor(&self) -> bool {
        unsafe { clang_CXXConstructor_isDefaultConstructor(self.inner) != 0 }
    }

    pub fn canonical(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getCanonicalCursor(self.inner)) }
    }

    pub fn semantic_parent(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getCursorSemanticParent(self.inner)) }
    }

    pub fn exception_specification_kind(&self) -> Result<ExceptionSpecificationKind> {
        unsafe { clang_getCursorExceptionSpecificationType(self.inner).try_into() }
    }

    pub fn printing_policy(&self) -> PrintingPolicy {
        unsafe {
            PrintingPolicy {
                inner: clang_getCursorPrintingPolicy(self.inner),
            }
        }
    }

    pub fn pretty_printed(&self, policy: PrintingPolicy) -> String {
        unsafe { clang_getCursorPrettyPrinted(self.inner, policy.inner).to_string() }
    }

    pub fn enum_decl_integer_type(&self) -> Result<Type> {
        unsafe { to_type(clang_getEnumDeclIntegerType(self.inner)) }
    }

    pub fn cxxrecord_needs_implicit_default_constructor(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitDefaultConstructor(self.inner) != 0 }
    }

    pub fn cxxrecord_needs_implicit_copy_constructor(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitCopyConstructor(self.inner) != 0 }
    }

    pub fn cxxrecord_needs_implicit_move_constructor(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitMoveConstructor(self.inner) != 0 }
    }

    pub fn cxxrecord_needs_implicit_copy_assignment(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitCopyAssignment(self.inner) != 0 }
    }

    pub fn cxxrecord_needs_implicit_move_assignment(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitMoveAssignment(self.inner) != 0 }
    }

    pub fn cxxrecord_needs_implicit_destructor(&self) -> bool {
        unsafe { clang_CXXRecord_needsImplicitDestructor(self.inner) != 0 }
    }

    pub fn num_overloaded_decls(&self) -> u32 {
        unsafe { clang_getNumOverloadedDecls(self.inner) }
    }

    pub fn get_overloaded_decl(&self, i: u32) -> Result<Cursor> {
        unsafe { cursor(clang_getOverloadedDecl(self.inner, i)) }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurEnumConstant(Cursor);

impl Deref for CurEnumConstant {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurEnumConstant {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::EnumConstantDecl {
            Ok(CurEnumConstant(c))
        } else {
            Err(Error::InvalidCursor)
        }
    }
}

impl CurEnumConstant {
    pub fn value(&self) -> i64 {
        unsafe { clang_getEnumConstantDeclValue(self.inner) }
    }

    pub fn unsigned_value(&self) -> u64 {
        unsafe { clang_getEnumConstantDeclUnsignedValue(self.inner) }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurTemplateRef(Cursor);

impl Deref for CurTemplateRef {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurTemplateRef {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::TemplateRef {
            Ok(CurTemplateRef(c))
        } else {
            Err(Error::InvalidCursor)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurClassDecl(Cursor);

impl Deref for CurClassDecl {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurClassDecl {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::ClassDecl
            || c.kind() == CursorKind::StructDecl
            || c.kind() == CursorKind::ClassTemplate
        {
            Ok(CurClassDecl(c))
        } else {
            Err(Error::FailedToConvertCursorKind {
                from: c.kind(),
                to: CursorKind::ClassDecl,
                source: Trace::new(),
            })
        }
    }
}

impl From<CurClassDecl> for Cursor {
    fn from(c: CurClassDecl) -> Self {
        c.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurStructDecl(Cursor);

impl CurStructDecl {
    pub fn as_class_decl(&self) -> CurClassDecl {
        CurClassDecl(self.0)
    }
}

impl Deref for CurStructDecl {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurStructDecl {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::StructDecl {
            Ok(CurStructDecl(c))
        } else {
            Err(Error::FailedToConvertCursorKind {
                from: c.kind(),
                to: CursorKind::StructDecl,
                source: Trace::new(),
            })
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurClassTemplate(Cursor);

impl CurClassTemplate {
    pub fn as_class_decl(&self) -> CurClassDecl {
        CurClassDecl(self.0)
    }
}

impl Deref for CurClassTemplate {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurClassTemplate {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::ClassTemplate
            || c.kind() == CursorKind::ClassTemplatePartialSpecialization
        {
            Ok(CurClassTemplate(c))
        } else {
            Err(Error::FailedToConvertCursorKind {
                from: c.kind(),
                to: CursorKind::ClassTemplate,
                source: Trace::new(),
            })
        }
    }
}

impl From<CurClassTemplate> for Cursor {
    fn from(c: CurClassTemplate) -> Self {
        c.0
    }
}

/// This is either a typedef or a type alias. Split later if we need the distinction but their sturcture appears to be
/// the same in the AST
#[derive(Debug, Copy, Clone)]
pub struct CurTypedef(Cursor);

impl CurTypedef {
    pub fn underlying_type(&self) -> Result<Type> {
        unsafe { to_type(clang_getTypedefDeclUnderlyingType(self.inner)) }
    }
}

impl Deref for CurTypedef {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurTypedef {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if matches!(
            c.kind(),
            CursorKind::TypedefDecl | CursorKind::TypeAliasDecl
        ) {
            Ok(CurTypedef(c))
        } else {
            Err(Error::FailedToConvertCursorKind {
                from: c.kind(),
                to: CursorKind::TypedefDecl,
                source: Trace::new(),
            })
        }
    }
}

impl From<CurTypedef> for Cursor {
    fn from(ct: CurTypedef) -> Self {
        ct.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CurNamespace(Cursor);

impl Deref for CurNamespace {
    type Target = Cursor;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<Cursor> for CurNamespace {
    type Error = Error;
    fn try_from(c: Cursor) -> Result<Self, Self::Error> {
        if c.kind() == CursorKind::Namespace {
            Ok(CurNamespace(c))
        } else {
            Err(Error::FailedToConvertCursorKind {
                from: c.kind(),
                to: CursorKind::Namespace,
                source: Trace::new(),
            })
        }
    }
}

impl From<CurNamespace> for Cursor {
    fn from(c: CurNamespace) -> Self {
        c.0
    }
}

pub(crate) fn cursor(cx: CXCursor) -> Result<Cursor> {
    unsafe {
        let null = clang_getNullCursor();

        if clang_isInvalid(cx.kind) != 0 || clang_equalCursors(null, cx) != 0 {
            Err(Error::InvalidCursor)
        } else {
            Ok(Cursor { inner: cx })
        }
    }
}

pub type CursorVisitor = extern "C" fn(CXCursor, CXCursor, CXClientData) -> CXChildVisitResult;

/// Unpack a Rust closure, extracting a `void*` pointer to the data and a
/// trampoline function which can be used to invoke it.
///
/// # Safety
///
/// It is the user's responsibility to ensure the closure outlives the returned
/// `void*` pointer.
///
/// Calling the trampoline function with anything except the `void*` pointer
/// will result in *Undefined Behaviour*.
///
/// The closure should guarantee that it never panics, seeing as panicking
/// across the FFI barrier is *Undefined Behaviour*. You may find
/// `std::panic::catch_unwind()` useful.
unsafe fn unpack_cursor_visitor_closure<F>(closure: F) -> (*mut c_void, CursorVisitor)
where
    F: FnMut(Cursor, Cursor) -> ChildVisitResult,
{
    extern "C" fn trampoline<F>(
        cursor: CXCursor,
        parent: CXCursor,
        client_data: CXClientData,
    ) -> CXChildVisitResult
    where
        F: FnMut(Cursor, Cursor) -> ChildVisitResult,
    {
        let closure: &mut F = unsafe { &mut *(client_data as *mut F) };
        (*closure)(Cursor { inner: cursor }, Cursor { inner: parent }).into()
    }

    let cb = Box::new(closure);
    let cb = Box::leak(cb);

    (cb as *mut F as *mut c_void, trampoline::<F>)
}

pub enum ChildVisitResult {
    Recurse,
    Continue,
    Break,
}

impl From<ChildVisitResult> for CXChildVisitResult {
    fn from(c: ChildVisitResult) -> Self {
        match c {
            ChildVisitResult::Break => CXChildVisit_Break,
            ChildVisitResult::Continue => CXChildVisit_Continue,
            ChildVisitResult::Recurse => CXChildVisit_Recurse,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{cursor_kind::CursorKind, error::Error, parse_string_to_tu};

    #[test]
    fn test_children() -> Result<(), Error> {
        let contents = r#"
template <class T>
class shared_ptr {
    T* t;
};


namespace A {

/// Some awesome class that we've written.
///
/// How awesome? SO awesome.
class Test {
public:
    int do_something(float a);
};

void free_function(int a, const Test& b);

}

namespace cppmm_bind {

/// A comment here
class Test  {
    using BoundType = ::A::Test;

    int do_something(float a);

} __attribute__((annotate("cppmm|blah")));

using TestPtr = shared_ptr<::A::Test>;

auto& free_function = ::A::free_function;

}
        "#;

        let tu = parse_string_to_tu(contents, &["-std=c++11"], true)?;

        let c = tu.get_cursor().unwrap();
        let children = c.children();

        assert_eq!(
            format!("{:?}", children),
            "[shared_ptr<T>: ClassTemplate c:@ST>1#T@shared_ptr, A: Namespace c:@N@A, cppmm_bind: Namespace c:@N@cppmm_bind]"
        );

        Ok(())
    }

    #[test]
    fn test_vardecl() -> Result<(), Error> {
        let contents = r#"
int a = 1;
extern int b;
        "#;
        let tu = parse_string_to_tu(contents, &["-std=c++11"], true)?;
        let c = tu.get_cursor().unwrap();
        let children = c.children();

        assert!(children[0].has_var_decl_global_storage());
        assert!(!children[0].has_var_decl_external_storage());
        assert!(children[1].has_var_decl_global_storage());
        assert!(children[1].has_var_decl_external_storage());

        let init = children[0].var_decl_initializer().unwrap();
        assert_eq!(init.kind(), CursorKind::IntegerLiteral);

        assert_eq!("1", tu.token(init.location()).spelling());

        Ok(())
    }

    #[test]
    fn test_pretty_print() -> Result<(), Error> {
        let contents = r#"
namespace Foo {
namespace Bar {
class Baz {};
}
}
        "#;
        let tu = parse_string_to_tu(contents, &["-std=c++11"], true)?;
        let c = tu.get_cursor().unwrap();

        c.visit_children(|c, _| {
            if c.kind() == CursorKind::ClassDecl {
                println!("{}", c.display_name());
                let pp = c.printing_policy();
                println!("{}", c.pretty_printed(pp));
                pp.set_suppress_scope(false);
                println!("{}", c.pretty_printed(pp));
                pp.set_fully_qualified_name(true);
                println!("{}", c.pretty_printed(pp));
            }

            super::ChildVisitResult::Recurse
        });

        Ok(())
    }
}
