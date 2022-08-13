#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::fmt::{Debug, Display};

use clang_sys::*;

use crate::{cursor::cursor, CXStringEx, Cursor};

use super::error::Error;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone)]
pub struct Type {
    inner: CXType,
}

impl Type {
    pub fn kind(&self) -> TypeKind {
        self.inner.kind.into()
    }

    pub fn num_template_arguments(&self) -> i32 {
        unsafe { clang_Type_getNumTemplateArguments(self.inner) }
    }

    pub fn template_argument_as_type(&self, i: u32) -> Result<Type> {
        unsafe { to_type(clang_Type_getTemplateArgumentAsType(self.inner, i)) }
    }

    pub fn spelling(&self) -> String {
        unsafe { clang_getTypeSpelling(self.inner).to_string() }
    }

    pub fn is_const_qualified(&self) -> bool {
        unsafe { clang_isConstQualifiedType(self.inner) != 0 }
    }

    pub fn type_declaration(&self) -> Result<Cursor> {
        unsafe { cursor(clang_getTypeDeclaration(self.inner)) }
    }

    pub fn is_builtin(&self) -> bool {
        (self.kind() as u32) > 1 && (self.kind() as u32) < 25
    }

    pub fn is_pointer(&self) -> bool {
        self.kind() == TypeKind::Pointer
            || self.kind() == TypeKind::LValueReference
            || self.kind() == TypeKind::RValueReference
    }

    pub fn pointee_type(&self) -> Result<Type> {
        unsafe { to_type(clang_getPointeeType(self.inner)) }
    }

    /// Retrieve the type named by the qualified-id.
    ///
    /// *Errors
    /// If a non-elaborated type is passed in
    pub fn named_type(&self) -> Result<Type> {
        unsafe { to_type(clang_Type_getNamedType(self.inner)) }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.spelling())
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.spelling(), self.kind())
    }
}

pub fn to_type(cx: CXType) -> Result<Type> {
    if cx.kind == CXType_Invalid {
        Err(Error::InvalidType)
    } else {
        Ok(Type { inner: cx })
    }
}

impl TypeKind {
    pub fn spelling(&self) -> String {
        unsafe {
            let cx: CXTypeKind = (*self).into();
            clang_getTypeKindSpelling(cx).to_string()
        }
    }
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeKind {
    Unexposed = 1,
    Void = 2,
    Bool = 3,
    Char_U = 4,
    UChar = 5,
    Char16 = 6,
    Char32 = 7,
    UShort = 8,
    UInt = 9,
    ULong = 10,
    ULongLong = 11,
    UInt128 = 12,
    Char_S = 13,
    SChar = 14,
    WChar = 15,
    Short = 16,
    Int = 17,
    Long = 18,
    LongLong = 19,
    Int128 = 20,
    Float = 21,
    Double = 22,
    LongDouble = 23,
    NullPtr = 24,
    Overload = 25,
    Dependent = 26,
    ObjCId = 27,
    ObjCClass = 28,
    ObjCSel = 29,
    Float128 = 30,
    Half = 31,
    Float16 = 32,
    ShortAccum = 33,
    Accum = 34,
    LongAccum = 35,
    UShortAccum = 36,
    UAccum = 37,
    ULongAccum = 38,
    BFloat16 = 39,
    Complex = 100,
    Pointer = 101,
    BlockPointer = 102,
    LValueReference = 103,
    RValueReference = 104,
    Record = 105,
    Enum = 106,
    Typedef = 107,
    ObjCInterface = 108,
    ObjCObjectPointer = 109,
    FunctionNoProto = 110,
    FunctionProto = 111,
    ConstantArray = 112,
    Vector = 113,
    IncompleteArray = 114,
    VariableArray = 115,
    DependentSizedArray = 116,
    MemberPointer = 117,
    Auto = 118,
    Elaborated = 119,
    Pipe = 120,
    OCLImage1dRO = 121,
    OCLImage1dArrayRO = 122,
    OCLImage1dBufferRO = 123,
    OCLImage2dRO = 124,
    OCLImage2dArrayRO = 125,
    OCLImage2dDepthRO = 126,
    OCLImage2dArrayDepthRO = 127,
    OCLImage2dMSAARO = 128,
    OCLImage2dArrayMSAARO = 129,
    OCLImage2dMSAADepthRO = 130,
    OCLImage2dArrayMSAADepthRO = 131,
    OCLImage3dRO = 132,
    OCLImage1dWO = 133,
    OCLImage1dArrayWO = 134,
    OCLImage1dBufferWO = 135,
    OCLImage2dWO = 136,
    OCLImage2dArrayWO = 137,
    OCLImage2dDepthWO = 138,
    OCLImage2dArrayDepthWO = 139,
    OCLImage2dMSAAWO = 140,
    OCLImage2dArrayMSAAWO = 141,
    OCLImage2dMSAADepthWO = 142,
    OCLImage2dArrayMSAADepthWO = 143,
    OCLImage3dWO = 144,
    OCLImage1dRW = 145,
    OCLImage1dArrayRW = 146,
    OCLImage1dBufferRW = 147,
    OCLImage2dRW = 148,
    OCLImage2dArrayRW = 149,
    OCLImage2dDepthRW = 150,
    OCLImage2dArrayDepthRW = 151,
    OCLImage2dMSAARW = 152,
    OCLImage2dArrayMSAARW = 153,
    OCLImage2dMSAADepthRW = 154,
    OCLImage2dArrayMSAADepthRW = 155,
    OCLImage3dRW = 156,
    OCLSampler = 157,
    OCLEvent = 158,
    OCLQueue = 159,
    OCLReserveID = 160,
    ObjCObject = 161,
    ObjCTypeParam = 162,
    Attributed = 163,
    OCLIntelSubgroupAVCMcePayload = 164,
    OCLIntelSubgroupAVCImePayload = 165,
    OCLIntelSubgroupAVCRefPayload = 166,
    OCLIntelSubgroupAVCSicPayload = 167,
    OCLIntelSubgroupAVCMceResult = 168,
    OCLIntelSubgroupAVCImeResult = 169,
    OCLIntelSubgroupAVCRefResult = 170,
    OCLIntelSubgroupAVCSicResult = 171,
    OCLIntelSubgroupAVCImeResultSingleRefStreamout = 172,
    OCLIntelSubgroupAVCImeResultDualRefStreamout = 173,
    OCLIntelSubgroupAVCImeSingleRefStreamin = 174,
    OCLIntelSubgroupAVCImeDualRefStreamin = 175,
    ExtVector = 176,
    Atomic = 177,
}

impl From<TypeKind> for clang_sys::CXTypeKind {
    fn from(k: TypeKind) -> Self {
        match k {
            TypeKind::Unexposed => CXType_Unexposed,
            TypeKind::Void => CXType_Void,
            TypeKind::Bool => CXType_Bool,
            TypeKind::Char_U => CXType_Char_U,
            TypeKind::UChar => CXType_UChar,
            TypeKind::Char16 => CXType_Char16,
            TypeKind::Char32 => CXType_Char32,
            TypeKind::UShort => CXType_UShort,
            TypeKind::UInt => CXType_UInt,
            TypeKind::ULong => CXType_ULong,
            TypeKind::ULongLong => CXType_ULongLong,
            TypeKind::UInt128 => CXType_UInt128,
            TypeKind::Char_S => CXType_Char_S,
            TypeKind::SChar => CXType_SChar,
            TypeKind::WChar => CXType_WChar,
            TypeKind::Short => CXType_Short,
            TypeKind::Int => CXType_Int,
            TypeKind::Long => CXType_Long,
            TypeKind::LongLong => CXType_LongLong,
            TypeKind::Int128 => CXType_Int128,
            TypeKind::Float => CXType_Float,
            TypeKind::Double => CXType_Double,
            TypeKind::LongDouble => CXType_LongDouble,
            TypeKind::NullPtr => CXType_NullPtr,
            TypeKind::Overload => CXType_Overload,
            TypeKind::Dependent => CXType_Dependent,
            TypeKind::ObjCId => CXType_ObjCId,
            TypeKind::ObjCClass => CXType_ObjCClass,
            TypeKind::ObjCSel => CXType_ObjCSel,
            TypeKind::Float128 => CXType_Float128,
            TypeKind::Half => CXType_Half,
            TypeKind::Float16 => CXType_Float16,
            TypeKind::ShortAccum => CXType_ShortAccum,
            TypeKind::Accum => CXType_Accum,
            TypeKind::LongAccum => CXType_LongAccum,
            TypeKind::UShortAccum => CXType_UShortAccum,
            TypeKind::UAccum => CXType_UAccum,
            TypeKind::ULongAccum => CXType_ULongAccum,
            TypeKind::BFloat16 => CXType_BFloat16,
            TypeKind::Complex => CXType_Complex,
            TypeKind::Pointer => CXType_Pointer,
            TypeKind::BlockPointer => CXType_BlockPointer,
            TypeKind::LValueReference => CXType_LValueReference,
            TypeKind::RValueReference => CXType_RValueReference,
            TypeKind::Record => CXType_Record,
            TypeKind::Enum => CXType_Enum,
            TypeKind::Typedef => CXType_Typedef,
            TypeKind::ObjCInterface => CXType_ObjCInterface,
            TypeKind::ObjCObjectPointer => CXType_ObjCObjectPointer,
            TypeKind::FunctionNoProto => CXType_FunctionNoProto,
            TypeKind::FunctionProto => CXType_FunctionProto,
            TypeKind::ConstantArray => CXType_ConstantArray,
            TypeKind::Vector => CXType_Vector,
            TypeKind::IncompleteArray => CXType_IncompleteArray,
            TypeKind::VariableArray => CXType_VariableArray,
            TypeKind::DependentSizedArray => CXType_DependentSizedArray,
            TypeKind::MemberPointer => CXType_MemberPointer,
            TypeKind::Auto => CXType_Auto,
            TypeKind::Elaborated => CXType_Elaborated,
            TypeKind::Pipe => CXType_Pipe,
            TypeKind::OCLImage1dRO => CXType_OCLImage1dRO,
            TypeKind::OCLImage1dArrayRO => CXType_OCLImage1dArrayRO,
            TypeKind::OCLImage1dBufferRO => CXType_OCLImage1dBufferRO,
            TypeKind::OCLImage2dRO => CXType_OCLImage2dRO,
            TypeKind::OCLImage2dArrayRO => CXType_OCLImage2dArrayRO,
            TypeKind::OCLImage2dDepthRO => CXType_OCLImage2dDepthRO,
            TypeKind::OCLImage2dArrayDepthRO => CXType_OCLImage2dArrayDepthRO,
            TypeKind::OCLImage2dMSAARO => CXType_OCLImage2dMSAARO,
            TypeKind::OCLImage2dArrayMSAARO => CXType_OCLImage2dArrayMSAARO,
            TypeKind::OCLImage2dMSAADepthRO => CXType_OCLImage2dMSAADepthRO,
            TypeKind::OCLImage2dArrayMSAADepthRO => CXType_OCLImage2dArrayMSAADepthRO,
            TypeKind::OCLImage3dRO => CXType_OCLImage3dRO,
            TypeKind::OCLImage1dWO => CXType_OCLImage1dWO,
            TypeKind::OCLImage1dArrayWO => CXType_OCLImage1dArrayWO,
            TypeKind::OCLImage1dBufferWO => CXType_OCLImage1dBufferWO,
            TypeKind::OCLImage2dWO => CXType_OCLImage2dWO,
            TypeKind::OCLImage2dArrayWO => CXType_OCLImage2dArrayWO,
            TypeKind::OCLImage2dDepthWO => CXType_OCLImage2dDepthWO,
            TypeKind::OCLImage2dArrayDepthWO => CXType_OCLImage2dArrayDepthWO,
            TypeKind::OCLImage2dMSAAWO => CXType_OCLImage2dMSAAWO,
            TypeKind::OCLImage2dArrayMSAAWO => CXType_OCLImage2dArrayMSAAWO,
            TypeKind::OCLImage2dMSAADepthWO => CXType_OCLImage2dMSAADepthWO,
            TypeKind::OCLImage2dArrayMSAADepthWO => CXType_OCLImage2dArrayMSAADepthWO,
            TypeKind::OCLImage3dWO => CXType_OCLImage3dWO,
            TypeKind::OCLImage1dRW => CXType_OCLImage1dRW,
            TypeKind::OCLImage1dArrayRW => CXType_OCLImage1dArrayRW,
            TypeKind::OCLImage1dBufferRW => CXType_OCLImage1dBufferRW,
            TypeKind::OCLImage2dRW => CXType_OCLImage2dRW,
            TypeKind::OCLImage2dArrayRW => CXType_OCLImage2dArrayRW,
            TypeKind::OCLImage2dDepthRW => CXType_OCLImage2dDepthRW,
            TypeKind::OCLImage2dArrayDepthRW => CXType_OCLImage2dArrayDepthRW,
            TypeKind::OCLImage2dMSAARW => CXType_OCLImage2dMSAARW,
            TypeKind::OCLImage2dArrayMSAARW => CXType_OCLImage2dArrayMSAARW,
            TypeKind::OCLImage2dMSAADepthRW => CXType_OCLImage2dMSAADepthRW,
            TypeKind::OCLImage2dArrayMSAADepthRW => CXType_OCLImage2dArrayMSAADepthRW,
            TypeKind::OCLImage3dRW => CXType_OCLImage3dRW,
            TypeKind::OCLSampler => CXType_OCLSampler,
            TypeKind::OCLEvent => CXType_OCLEvent,
            TypeKind::OCLQueue => CXType_OCLQueue,
            TypeKind::OCLReserveID => CXType_OCLReserveID,
            TypeKind::ObjCObject => CXType_ObjCObject,
            TypeKind::ObjCTypeParam => CXType_ObjCTypeParam,
            TypeKind::Attributed => CXType_Attributed,
            TypeKind::OCLIntelSubgroupAVCMcePayload => CXType_OCLIntelSubgroupAVCMcePayload,
            TypeKind::OCLIntelSubgroupAVCImePayload => CXType_OCLIntelSubgroupAVCImePayload,
            TypeKind::OCLIntelSubgroupAVCRefPayload => CXType_OCLIntelSubgroupAVCRefPayload,
            TypeKind::OCLIntelSubgroupAVCSicPayload => CXType_OCLIntelSubgroupAVCSicPayload,
            TypeKind::OCLIntelSubgroupAVCMceResult => CXType_OCLIntelSubgroupAVCMceResult,
            TypeKind::OCLIntelSubgroupAVCImeResult => CXType_OCLIntelSubgroupAVCImeResult,
            TypeKind::OCLIntelSubgroupAVCRefResult => CXType_OCLIntelSubgroupAVCRefResult,
            TypeKind::OCLIntelSubgroupAVCSicResult => CXType_OCLIntelSubgroupAVCSicResult,
            TypeKind::OCLIntelSubgroupAVCImeResultSingleRefStreamout => {
                CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout
            }
            TypeKind::OCLIntelSubgroupAVCImeResultDualRefStreamout => {
                CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout
            }
            TypeKind::OCLIntelSubgroupAVCImeSingleRefStreamin => {
                CXType_OCLIntelSubgroupAVCImeSingleRefStreamin
            }
            TypeKind::OCLIntelSubgroupAVCImeDualRefStreamin => {
                CXType_OCLIntelSubgroupAVCImeDualRefStreamin
            }
            TypeKind::ExtVector => CXType_ExtVector,
            TypeKind::Atomic => CXType_Atomic,
        }
    }
}

impl From<clang_sys::CXTypeKind> for TypeKind {
    fn from(k: clang_sys::CXTypeKind) -> Self {
        match k {
            CXType_Unexposed => TypeKind::Unexposed,
            CXType_Void => TypeKind::Void,
            CXType_Bool => TypeKind::Bool,
            CXType_Char_U => TypeKind::Char_U,
            CXType_UChar => TypeKind::UChar,
            CXType_Char16 => TypeKind::Char16,
            CXType_Char32 => TypeKind::Char32,
            CXType_UShort => TypeKind::UShort,
            CXType_UInt => TypeKind::UInt,
            CXType_ULong => TypeKind::ULong,
            CXType_ULongLong => TypeKind::ULongLong,
            CXType_UInt128 => TypeKind::UInt128,
            CXType_Char_S => TypeKind::Char_S,
            CXType_SChar => TypeKind::SChar,
            CXType_WChar => TypeKind::WChar,
            CXType_Short => TypeKind::Short,
            CXType_Int => TypeKind::Int,
            CXType_Long => TypeKind::Long,
            CXType_LongLong => TypeKind::LongLong,
            CXType_Int128 => TypeKind::Int128,
            CXType_Float => TypeKind::Float,
            CXType_Double => TypeKind::Double,
            CXType_LongDouble => TypeKind::LongDouble,
            CXType_NullPtr => TypeKind::NullPtr,
            CXType_Overload => TypeKind::Overload,
            CXType_Dependent => TypeKind::Dependent,
            CXType_ObjCId => TypeKind::ObjCId,
            CXType_ObjCClass => TypeKind::ObjCClass,
            CXType_ObjCSel => TypeKind::ObjCSel,
            CXType_Float128 => TypeKind::Float128,
            CXType_Half => TypeKind::Half,
            CXType_Float16 => TypeKind::Float16,
            CXType_ShortAccum => TypeKind::ShortAccum,
            CXType_Accum => TypeKind::Accum,
            CXType_LongAccum => TypeKind::LongAccum,
            CXType_UShortAccum => TypeKind::UShortAccum,
            CXType_UAccum => TypeKind::UAccum,
            CXType_ULongAccum => TypeKind::ULongAccum,
            CXType_BFloat16 => TypeKind::BFloat16,
            CXType_Complex => TypeKind::Complex,
            CXType_Pointer => TypeKind::Pointer,
            CXType_BlockPointer => TypeKind::BlockPointer,
            CXType_LValueReference => TypeKind::LValueReference,
            CXType_RValueReference => TypeKind::RValueReference,
            CXType_Record => TypeKind::Record,
            CXType_Enum => TypeKind::Enum,
            CXType_Typedef => TypeKind::Typedef,
            CXType_ObjCInterface => TypeKind::ObjCInterface,
            CXType_ObjCObjectPointer => TypeKind::ObjCObjectPointer,
            CXType_FunctionNoProto => TypeKind::FunctionNoProto,
            CXType_FunctionProto => TypeKind::FunctionProto,
            CXType_ConstantArray => TypeKind::ConstantArray,
            CXType_Vector => TypeKind::Vector,
            CXType_IncompleteArray => TypeKind::IncompleteArray,
            CXType_VariableArray => TypeKind::VariableArray,
            CXType_DependentSizedArray => TypeKind::DependentSizedArray,
            CXType_MemberPointer => TypeKind::MemberPointer,
            CXType_Auto => TypeKind::Auto,
            CXType_Elaborated => TypeKind::Elaborated,
            CXType_Pipe => TypeKind::Pipe,
            CXType_OCLImage1dRO => TypeKind::OCLImage1dRO,
            CXType_OCLImage1dArrayRO => TypeKind::OCLImage1dArrayRO,
            CXType_OCLImage1dBufferRO => TypeKind::OCLImage1dBufferRO,
            CXType_OCLImage2dRO => TypeKind::OCLImage2dRO,
            CXType_OCLImage2dArrayRO => TypeKind::OCLImage2dArrayRO,
            CXType_OCLImage2dDepthRO => TypeKind::OCLImage2dDepthRO,
            CXType_OCLImage2dArrayDepthRO => TypeKind::OCLImage2dArrayDepthRO,
            CXType_OCLImage2dMSAARO => TypeKind::OCLImage2dMSAARO,
            CXType_OCLImage2dArrayMSAARO => TypeKind::OCLImage2dArrayMSAARO,
            CXType_OCLImage2dMSAADepthRO => TypeKind::OCLImage2dMSAADepthRO,
            CXType_OCLImage2dArrayMSAADepthRO => TypeKind::OCLImage2dArrayMSAADepthRO,
            CXType_OCLImage3dRO => TypeKind::OCLImage3dRO,
            CXType_OCLImage1dWO => TypeKind::OCLImage1dWO,
            CXType_OCLImage1dArrayWO => TypeKind::OCLImage1dArrayWO,
            CXType_OCLImage1dBufferWO => TypeKind::OCLImage1dBufferWO,
            CXType_OCLImage2dWO => TypeKind::OCLImage2dWO,
            CXType_OCLImage2dArrayWO => TypeKind::OCLImage2dArrayWO,
            CXType_OCLImage2dDepthWO => TypeKind::OCLImage2dDepthWO,
            CXType_OCLImage2dArrayDepthWO => TypeKind::OCLImage2dArrayDepthWO,
            CXType_OCLImage2dMSAAWO => TypeKind::OCLImage2dMSAAWO,
            CXType_OCLImage2dArrayMSAAWO => TypeKind::OCLImage2dArrayMSAAWO,
            CXType_OCLImage2dMSAADepthWO => TypeKind::OCLImage2dMSAADepthWO,
            CXType_OCLImage2dArrayMSAADepthWO => TypeKind::OCLImage2dArrayMSAADepthWO,
            CXType_OCLImage3dWO => TypeKind::OCLImage3dWO,
            CXType_OCLImage1dRW => TypeKind::OCLImage1dRW,
            CXType_OCLImage1dArrayRW => TypeKind::OCLImage1dArrayRW,
            CXType_OCLImage1dBufferRW => TypeKind::OCLImage1dBufferRW,
            CXType_OCLImage2dRW => TypeKind::OCLImage2dRW,
            CXType_OCLImage2dArrayRW => TypeKind::OCLImage2dArrayRW,
            CXType_OCLImage2dDepthRW => TypeKind::OCLImage2dDepthRW,
            CXType_OCLImage2dArrayDepthRW => TypeKind::OCLImage2dArrayDepthRW,
            CXType_OCLImage2dMSAARW => TypeKind::OCLImage2dMSAARW,
            CXType_OCLImage2dArrayMSAARW => TypeKind::OCLImage2dArrayMSAARW,
            CXType_OCLImage2dMSAADepthRW => TypeKind::OCLImage2dMSAADepthRW,
            CXType_OCLImage2dArrayMSAADepthRW => TypeKind::OCLImage2dArrayMSAADepthRW,
            CXType_OCLImage3dRW => TypeKind::OCLImage3dRW,
            CXType_OCLSampler => TypeKind::OCLSampler,
            CXType_OCLEvent => TypeKind::OCLEvent,
            CXType_OCLQueue => TypeKind::OCLQueue,
            CXType_OCLReserveID => TypeKind::OCLReserveID,
            CXType_ObjCObject => TypeKind::ObjCObject,
            CXType_ObjCTypeParam => TypeKind::ObjCTypeParam,
            CXType_Attributed => TypeKind::Attributed,
            CXType_OCLIntelSubgroupAVCMcePayload => TypeKind::OCLIntelSubgroupAVCMcePayload,
            CXType_OCLIntelSubgroupAVCImePayload => TypeKind::OCLIntelSubgroupAVCImePayload,
            CXType_OCLIntelSubgroupAVCRefPayload => TypeKind::OCLIntelSubgroupAVCRefPayload,
            CXType_OCLIntelSubgroupAVCSicPayload => TypeKind::OCLIntelSubgroupAVCSicPayload,
            CXType_OCLIntelSubgroupAVCMceResult => TypeKind::OCLIntelSubgroupAVCMceResult,
            CXType_OCLIntelSubgroupAVCImeResult => TypeKind::OCLIntelSubgroupAVCImeResult,
            CXType_OCLIntelSubgroupAVCRefResult => TypeKind::OCLIntelSubgroupAVCRefResult,
            CXType_OCLIntelSubgroupAVCSicResult => TypeKind::OCLIntelSubgroupAVCSicResult,
            CXType_OCLIntelSubgroupAVCImeResultSingleRefStreamout => {
                TypeKind::OCLIntelSubgroupAVCImeResultSingleRefStreamout
            }
            CXType_OCLIntelSubgroupAVCImeResultDualRefStreamout => {
                TypeKind::OCLIntelSubgroupAVCImeResultDualRefStreamout
            }
            CXType_OCLIntelSubgroupAVCImeSingleRefStreamin => {
                TypeKind::OCLIntelSubgroupAVCImeSingleRefStreamin
            }
            CXType_OCLIntelSubgroupAVCImeDualRefStreamin => {
                TypeKind::OCLIntelSubgroupAVCImeDualRefStreamin
            }
            CXType_ExtVector => TypeKind::ExtVector,
            CXType_Atomic => TypeKind::Atomic,
            _ => unimplemented!(),
        }
    }
}
