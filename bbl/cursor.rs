use crate::{template_argument::TemplateArgumentKind, token::SourceLocation, class::AccessSpecifier};

use super::cursor_kind::CursorKind;
use super::string::CXStringEx;
use clang_sys::{
    clang_CXXMethod_isConst, clang_CXXMethod_isDefaulted, clang_CXXMethod_isPureVirtual,
    clang_CXXMethod_isStatic, clang_CXXMethod_isVirtual, clang_CXXRecord_isAbstract,
    clang_Cursor_getArgument, clang_Cursor_getNumArguments, clang_Cursor_getNumTemplateArguments,
    clang_Cursor_getTemplateArgumentKind, clang_Cursor_getTemplateArgumentType,
    clang_Cursor_getTemplateArgumentUnsignedValue, clang_Cursor_getTemplateArgumentValue,
    clang_Cursor_getVarDeclInitializer, clang_Cursor_hasVarDeclExternalStorage,
    clang_Cursor_hasVarDeclGlobalStorage, clang_equalCursors, clang_getCanonicalCursor,
    clang_getCursorDefinition, clang_getCursorDisplayName, clang_getCursorKind,
    clang_getCursorLocation, clang_getCursorPrettyPrinted, clang_getCursorPrintingPolicy,
    clang_getCursorReferenced, clang_getCursorResultType, clang_getCursorSpelling,
    clang_getCursorType, clang_getCursorUSR, clang_getNullCursor, clang_isCursorDefinition,
    clang_isInvalid, clang_visitChildren, CXChildVisitResult, CXChildVisit_Break,
    CXChildVisit_Continue, CXChildVisit_Recurse, CXClientData, CXCursor, clang_getCXXAccessSpecifier,
};
use std::{
    fmt::{Debug, Display},
    os::raw::{c_longlong, c_ulonglong, c_void},
};

use crate::ty::{to_type, Type};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct USR(pub String);

impl Display for USR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
            let (closure, trampoline) = unpack_closure(callback);
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
        unsafe { USR(clang_getCursorUSR(self.inner).to_string()) }
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

    pub fn pretty_printed(&self) -> String {
        unsafe {
            let policy = clang_getCursorPrintingPolicy(self.inner);
            clang_getCursorPrettyPrinted(self.inner, policy).to_string()
        }
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

    pub fn cxx_method_is_virtual(&self) -> bool {
        unsafe { clang_CXXMethod_isVirtual(self.inner) != 0 }
    }

    pub fn cxx_method_is_pure_virtual(&self) -> bool {
        unsafe { clang_CXXMethod_isPureVirtual(self.inner) != 0 }
    }

    pub fn cxx_record_is_abstract(&self) -> bool {
        unsafe { clang_CXXRecord_isAbstract(self.inner) != 0 }
    }

    pub fn cxx_access_specifier(&self) -> Result<AccessSpecifier> {
        unsafe { clang_getCXXAccessSpecifier(self.inner).try_into() }
    }

    pub fn canonical(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getCanonicalCursor(self.inner)) }
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
unsafe fn unpack_closure<F>(closure: F) -> (*mut c_void, CursorVisitor)
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
    use crate::{cursor_kind::CursorKind, error::Error, parse_string};

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

        let tu = parse_string(contents, &["-std=c++11"], true)?;

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
        let tu = parse_string(contents, &["-std=c++11"], true)?;
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
}
