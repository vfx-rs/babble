#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use clang_sys::*;

#[derive(Copy, Clone)]
pub struct PrintingPolicy {
    pub(crate) inner: CXPrintingPolicy,
}

impl PrintingPolicy {
    pub fn set_suppress_scope(&self, value: bool) {
        unsafe {
            clang_PrintingPolicy_setProperty(self.inner, PrintingPolicyProperty::SuppressScope.into(), value as u32);
        }
    }   

    pub fn set_fully_qualified_name(&self, value: bool) {
        unsafe {
            clang_PrintingPolicy_setProperty(self.inner, PrintingPolicyProperty::FullyQualifiedName.into(), value as u32);
        }
    }
}

pub enum PrintingPolicyProperty {
    Indentation,
    SuppressSpecifiers,
    SuppressTagKeyword,
    IncludeTagDefinition,
    SuppressScope,
    SuppressUnwrittenScope,
    SuppressInitializers,
    ConstantArraySizeAsWritten,
    AnonymousTagLocations,
    SuppressStrongLifetime,
    SuppressLifetimeQualifiers,
    SuppressTemplateArgsInCXXConstructors,
    Bool,
    Restrict,
    Alignof,
    UnderscoreAlignof,
    UseVoidForZeroParams,
    TerseOutput,
    PolishForDeclaration,
    Half,
    MSWChar,
    IncludeNewlines,
    MSVCFormatting,
    ConstantsAsWritten,
    SuppressImplicitBase,
    FullyQualifiedName,
}

impl From<CXPrintingPolicyProperty> for PrintingPolicyProperty {
    fn from(p: CXPrintingPolicyProperty) -> Self {
        use PrintingPolicyProperty::*;
        match p {
            CXPrintingPolicy_Indentation => Indentation,
            CXPrintingPolicy_SuppressSpecifiers => SuppressSpecifiers,
            CXPrintingPolicy_SuppressTagKeyword => SuppressTagKeyword,
            CXPrintingPolicy_IncludeTagDefinition => IncludeTagDefinition,
            CXPrintingPolicy_SuppressScope => SuppressScope,
            CXPrintingPolicy_SuppressUnwrittenScope => SuppressUnwrittenScope,
            CXPrintingPolicy_SuppressInitializers => SuppressInitializers,
            CXPrintingPolicy_ConstantArraySizeAsWritten => ConstantArraySizeAsWritten,
            CXPrintingPolicy_AnonymousTagLocations => AnonymousTagLocations,
            CXPrintingPolicy_SuppressStrongLifetime => SuppressStrongLifetime,
            CXPrintingPolicy_SuppressLifetimeQualifiers => SuppressLifetimeQualifiers,
            CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors => SuppressTemplateArgsInCXXConstructors,
            CXPrintingPolicy_Bool => Bool,
            CXPrintingPolicy_Restrict => Restrict,
            CXPrintingPolicy_Alignof => Alignof,
            CXPrintingPolicy_UnderscoreAlignof => UnderscoreAlignof,
            CXPrintingPolicy_UseVoidForZeroParams => UseVoidForZeroParams,
            CXPrintingPolicy_TerseOutput => TerseOutput,
            CXPrintingPolicy_PolishForDeclaration => PolishForDeclaration,
            CXPrintingPolicy_Half => Half,
            CXPrintingPolicy_MSWChar => MSWChar,
            CXPrintingPolicy_IncludeNewlines => IncludeNewlines,
            CXPrintingPolicy_MSVCFormatting => MSVCFormatting,
            CXPrintingPolicy_ConstantsAsWritten => ConstantsAsWritten,
            CXPrintingPolicy_SuppressImplicitBase => SuppressImplicitBase,
            CXPrintingPolicy_FullyQualifiedName => FullyQualifiedName,
            _ => unreachable!()
        }
    }
} 

impl From<PrintingPolicyProperty> for CXPrintingPolicyProperty {
    fn from(p: PrintingPolicyProperty) -> Self {
        use PrintingPolicyProperty::*;
        match p {
            Indentation => CXPrintingPolicy_Indentation,
            SuppressSpecifiers => CXPrintingPolicy_SuppressSpecifiers,
            SuppressTagKeyword => CXPrintingPolicy_SuppressTagKeyword,
            IncludeTagDefinition => CXPrintingPolicy_IncludeTagDefinition,
            SuppressScope => CXPrintingPolicy_SuppressScope,
            SuppressUnwrittenScope => CXPrintingPolicy_SuppressUnwrittenScope,
            SuppressInitializers => CXPrintingPolicy_SuppressInitializers,
            ConstantArraySizeAsWritten => CXPrintingPolicy_ConstantArraySizeAsWritten,
            AnonymousTagLocations => CXPrintingPolicy_AnonymousTagLocations,
            SuppressStrongLifetime => CXPrintingPolicy_SuppressStrongLifetime,
            SuppressLifetimeQualifiers => CXPrintingPolicy_SuppressLifetimeQualifiers,
            SuppressTemplateArgsInCXXConstructors => CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors,
            Bool => CXPrintingPolicy_Bool,
            Restrict => CXPrintingPolicy_Restrict,
            Alignof => CXPrintingPolicy_Alignof,
            UnderscoreAlignof => CXPrintingPolicy_UnderscoreAlignof,
            UseVoidForZeroParams => CXPrintingPolicy_UseVoidForZeroParams,
            TerseOutput => CXPrintingPolicy_TerseOutput,
            PolishForDeclaration => CXPrintingPolicy_PolishForDeclaration,
            Half => CXPrintingPolicy_Half,
            MSWChar => CXPrintingPolicy_MSWChar,
            IncludeNewlines => CXPrintingPolicy_IncludeNewlines,
            MSVCFormatting => CXPrintingPolicy_MSVCFormatting,
            ConstantsAsWritten => CXPrintingPolicy_ConstantsAsWritten,
            SuppressImplicitBase => CXPrintingPolicy_SuppressImplicitBase,
            FullyQualifiedName => CXPrintingPolicy_FullyQualifiedName,
        }       
    }
}