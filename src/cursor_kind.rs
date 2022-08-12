#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]

use std::fmt::Display;

use clang_sys::*;

impl From<CursorKind> for CXCursorKind {
    fn from(c: CursorKind) -> Self {
        match c {
            CursorKind::UnexposedDecl => CXCursor_UnexposedDecl,
            CursorKind::StructDecl => CXCursor_StructDecl,
            CursorKind::UnionDecl => CXCursor_UnionDecl,
            CursorKind::ClassDecl => CXCursor_ClassDecl,
            CursorKind::EnumDecl => CXCursor_EnumDecl,
            CursorKind::FieldDecl => CXCursor_FieldDecl,
            CursorKind::EnumConstantDecl => CXCursor_EnumConstantDecl,
            CursorKind::FunctionDecl => CXCursor_FunctionDecl,
            CursorKind::VarDecl => CXCursor_VarDecl,
            CursorKind::ParmDecl => CXCursor_ParmDecl,
            CursorKind::ObjCInterfaceDecl => CXCursor_ObjCInterfaceDecl,
            CursorKind::ObjCCategoryDecl => CXCursor_ObjCCategoryDecl,
            CursorKind::ObjCProtocolDecl => CXCursor_ObjCProtocolDecl,
            CursorKind::ObjCPropertyDecl => CXCursor_ObjCPropertyDecl,
            CursorKind::ObjCIvarDecl => CXCursor_ObjCIvarDecl,
            CursorKind::ObjCInstanceMethodDecl => CXCursor_ObjCInstanceMethodDecl,
            CursorKind::ObjCClassMethodDecl => CXCursor_ObjCClassMethodDecl,
            CursorKind::ObjCImplementationDecl => CXCursor_ObjCImplementationDecl,
            CursorKind::ObjCCategoryImplDecl => CXCursor_ObjCCategoryImplDecl,
            CursorKind::TypedefDecl => CXCursor_TypedefDecl,
            CursorKind::CXXMethod => CXCursor_CXXMethod,
            CursorKind::Namespace => CXCursor_Namespace,
            CursorKind::LinkageSpec => CXCursor_LinkageSpec,
            CursorKind::Constructor => CXCursor_Constructor,
            CursorKind::Destructor => CXCursor_Destructor,
            CursorKind::ConversionFunction => CXCursor_ConversionFunction,
            CursorKind::TemplateTypeParameter => CXCursor_TemplateTypeParameter,
            CursorKind::NonTypeTemplateParameter => CXCursor_NonTypeTemplateParameter,
            CursorKind::TemplateTemplateParameter => CXCursor_TemplateTemplateParameter,
            CursorKind::FunctionTemplate => CXCursor_FunctionTemplate,
            CursorKind::ClassTemplate => CXCursor_ClassTemplate,
            CursorKind::ClassTemplatePartialSpecialization => CXCursor_ClassTemplatePartialSpecialization,
            CursorKind::NamespaceAlias => CXCursor_NamespaceAlias,
            CursorKind::UsingDirective => CXCursor_UsingDirective,
            CursorKind::UsingDeclaration => CXCursor_UsingDeclaration,
            CursorKind::TypeAliasDecl => CXCursor_TypeAliasDecl,
            CursorKind::ObjCSynthesizeDecl => CXCursor_ObjCSynthesizeDecl,
            CursorKind::ObjCDynamicDecl => CXCursor_ObjCDynamicDecl,
            CursorKind::CXXAccessSpecifier => CXCursor_CXXAccessSpecifier,

            CursorKind::ObjCSuperClassRef => CXCursor_ObjCSuperClassRef,
            CursorKind::ObjCProtocolRef => CXCursor_ObjCProtocolRef,
            CursorKind::ObjCClassRef => CXCursor_ObjCClassRef,
            CursorKind::TypeRef => CXCursor_TypeRef,
            CursorKind::CXXBaseSpecifier => CXCursor_CXXBaseSpecifier,
            CursorKind::TemplateRef => CXCursor_TemplateRef,
            CursorKind::NamespaceRef => CXCursor_NamespaceRef,
            CursorKind::MemberRef => CXCursor_MemberRef,
            CursorKind::LabelRef => CXCursor_LabelRef,
            CursorKind::OverloadedDeclRef => CXCursor_OverloadedDeclRef,
            CursorKind::VariableRef => CXCursor_VariableRef,
  
            CursorKind::InvalidFile => CXCursor_InvalidFile,
            CursorKind::NoDeclFound => CXCursor_NoDeclFound,
            CursorKind::NotImplemented => CXCursor_NotImplemented,
            CursorKind::InvalidCode => CXCursor_InvalidCode,
  
            CursorKind::UnexposedExpr => CXCursor_UnexposedExpr,
            CursorKind::DeclRefExpr => CXCursor_DeclRefExpr,
            CursorKind::MemberRefExpr => CXCursor_MemberRefExpr,
            CursorKind::CallExpr => CXCursor_CallExpr,
            CursorKind::ObjCMessageExpr => CXCursor_ObjCMessageExpr,
            CursorKind::BlockExpr => CXCursor_BlockExpr,
            CursorKind::IntegerLiteral => CXCursor_IntegerLiteral,
            CursorKind::FloatingLiteral => CXCursor_FloatingLiteral,
            CursorKind::ImaginaryLiteral => CXCursor_ImaginaryLiteral,
            CursorKind::StringLiteral => CXCursor_StringLiteral,
            CursorKind::CharacterLiteral => CXCursor_CharacterLiteral,
            CursorKind::ParenExpr => CXCursor_ParenExpr,
            CursorKind::UnaryOperator => CXCursor_UnaryOperator,
            CursorKind::ArraySubscriptExpr => CXCursor_ArraySubscriptExpr,
            CursorKind::BinaryOperator => CXCursor_BinaryOperator,
            CursorKind::CompoundAssignOperator => CXCursor_CompoundAssignOperator,
            CursorKind::ConditionalOperator => CXCursor_ConditionalOperator,
            CursorKind::CStyleCastExpr => CXCursor_CStyleCastExpr,
            CursorKind::CompoundLiteralExpr => CXCursor_CompoundLiteralExpr,
            CursorKind::InitListExpr => CXCursor_InitListExpr,
            CursorKind::AddrLabelExpr => CXCursor_AddrLabelExpr,
            CursorKind::StmtExpr => CXCursor_StmtExpr,
            CursorKind::GenericSelectionExpr => CXCursor_GenericSelectionExpr,
            CursorKind::GNUNullExpr => CXCursor_GNUNullExpr,
            CursorKind::CXXStaticCastExpr => CXCursor_CXXStaticCastExpr,
            CursorKind::CXXDynamicCastExpr => CXCursor_CXXDynamicCastExpr,
            CursorKind::CXXReinterpretCastExpr => CXCursor_CXXReinterpretCastExpr,
            CursorKind::CXXConstCastExpr => CXCursor_CXXConstCastExpr,
            CursorKind::CXXFunctionalCastExpr => CXCursor_CXXFunctionalCastExpr,
            CursorKind::CXXTypeidExpr => CXCursor_CXXTypeidExpr,
            CursorKind::CXXBoolLiteralExpr => CXCursor_CXXBoolLiteralExpr,
            CursorKind::CXXNullPtrLiteralExpr => CXCursor_CXXNullPtrLiteralExpr,
            CursorKind::CXXThisExpr => CXCursor_CXXThisExpr,
            CursorKind::CXXThrowExpr => CXCursor_CXXThrowExpr,
            CursorKind::CXXNewExpr => CXCursor_CXXNewExpr,
            CursorKind::CXXDeleteExpr => CXCursor_CXXDeleteExpr,
            CursorKind::UnaryExpr => CXCursor_UnaryExpr,
            CursorKind::ObjCStringLiteral => CXCursor_ObjCStringLiteral,
            CursorKind::ObjCEncodeExpr => CXCursor_ObjCEncodeExpr,
            CursorKind::ObjCSelectorExpr => CXCursor_ObjCSelectorExpr,
            CursorKind::ObjCProtocolExpr => CXCursor_ObjCProtocolExpr,
            CursorKind::ObjCBridgedCastExpr => CXCursor_ObjCBridgedCastExpr,
            CursorKind::PackExpansionExpr => CXCursor_PackExpansionExpr,
            CursorKind::SizeOfPackExpr => CXCursor_SizeOfPackExpr,
            CursorKind::LambdaExpr => CXCursor_LambdaExpr,
            CursorKind::ObjCBoolLiteralExpr => CXCursor_ObjCBoolLiteralExpr,
            CursorKind::ObjCSelfExpr => CXCursor_ObjCSelfExpr,
            CursorKind::OMPArraySectionExpr => CXCursor_OMPArraySectionExpr,
            CursorKind::ObjCAvailabilityCheckExpr => CXCursor_ObjCAvailabilityCheckExpr,
            CursorKind::FixedPointLiteral => CXCursor_FixedPointLiteral,
            CursorKind::OMPArrayShapingExpr => CXCursor_OMPArrayShapingExpr,
            CursorKind::OMPIteratorExpr => CXCursor_OMPIteratorExpr,
            CursorKind::CXXAddrspaceCastExpr => CXCursor_CXXAddrspaceCastExpr,
            CursorKind::UnexposedStmt => CXCursor_UnexposedStmt,
            CursorKind::LabelStmt => CXCursor_LabelStmt,
            CursorKind::CompoundStmt => CXCursor_CompoundStmt,
            CursorKind::CaseStmt => CXCursor_CaseStmt,
            CursorKind::DefaultStmt => CXCursor_DefaultStmt,
            CursorKind::IfStmt => CXCursor_IfStmt,
            CursorKind::SwitchStmt => CXCursor_SwitchStmt,
            CursorKind::WhileStmt => CXCursor_WhileStmt,
            CursorKind::DoStmt => CXCursor_DoStmt,
            CursorKind::ForStmt => CXCursor_ForStmt,
            CursorKind::GotoStmt => CXCursor_GotoStmt,
            CursorKind::IndirectGotoStmt => CXCursor_IndirectGotoStmt,
            CursorKind::ContinueStmt => CXCursor_ContinueStmt,
            CursorKind::BreakStmt => CXCursor_BreakStmt,
            CursorKind::ReturnStmt => CXCursor_ReturnStmt,
            CursorKind::ObjCAtTryStmt => CXCursor_ObjCAtTryStmt,
            CursorKind::ObjCAtCatchStmt => CXCursor_ObjCAtCatchStmt,
            CursorKind::ObjCAtFinallyStmt => CXCursor_ObjCAtFinallyStmt,
            CursorKind::ObjCAtThrowStmt => CXCursor_ObjCAtThrowStmt,
            CursorKind::ObjCAtSynchronizedStmt => CXCursor_ObjCAtSynchronizedStmt,
            CursorKind::ObjCAutoreleasePoolStmt => CXCursor_ObjCAutoreleasePoolStmt,
            CursorKind::ObjCForCollectionStmt => CXCursor_ObjCForCollectionStmt,
            CursorKind::CXXCatchStmt => CXCursor_CXXCatchStmt,
            CursorKind::CXXTryStmt => CXCursor_CXXTryStmt,
            CursorKind::CXXForRangeStmt => CXCursor_CXXForRangeStmt,
            CursorKind::SEHTryStmt => CXCursor_SEHTryStmt,
            CursorKind::SEHExceptStmt => CXCursor_SEHExceptStmt,
            CursorKind::SEHFinallyStmt => CXCursor_SEHFinallyStmt,
            CursorKind::MSAsmStmt => CXCursor_MSAsmStmt,
            CursorKind::NullStmt => CXCursor_NullStmt,
            CursorKind::DeclStmt => CXCursor_DeclStmt,


            CursorKind::OMPParallelDirective => CXCursor_OMPParallelDirective,
            CursorKind::OMPSimdDirective => CXCursor_OMPSimdDirective,
            CursorKind::OMPForDirective => CXCursor_OMPForDirective,
            CursorKind::OMPSectionsDirective => CXCursor_OMPSectionsDirective,
            CursorKind::OMPSectionDirective => CXCursor_OMPSectionDirective,
            CursorKind::OMPSingleDirective => CXCursor_OMPSingleDirective,
            CursorKind::OMPParallelForDirective => CXCursor_OMPParallelForDirective,
            CursorKind::OMPParallelSectionsDirective => CXCursor_OMPParallelSectionsDirective,
            CursorKind::OMPTaskDirective => CXCursor_OMPTaskDirective,
            CursorKind::OMPMasterDirective => CXCursor_OMPMasterDirective,
            CursorKind::OMPCriticalDirective => CXCursor_OMPCriticalDirective,
            CursorKind::OMPTaskyieldDirective => CXCursor_OMPTaskyieldDirective,
            CursorKind::OMPBarrierDirective => CXCursor_OMPBarrierDirective,
            CursorKind::OMPTaskwaitDirective => CXCursor_OMPTaskwaitDirective,
            CursorKind::OMPFlushDirective => CXCursor_OMPFlushDirective,
            CursorKind::SEHLeaveStmt => CXCursor_SEHLeaveStmt,
            CursorKind::OMPOrderedDirective => CXCursor_OMPOrderedDirective,
            CursorKind::OMPAtomicDirective => CXCursor_OMPAtomicDirective,
            CursorKind::OMPForSimdDirective => CXCursor_OMPForSimdDirective,
            CursorKind::OMPParallelForSimdDirective => CXCursor_OMPParallelForSimdDirective,
            CursorKind::OMPTargetDirective => CXCursor_OMPTargetDirective,
            CursorKind::OMPTeamsDirective => CXCursor_OMPTeamsDirective,
            CursorKind::OMPTaskgroupDirective => CXCursor_OMPTaskgroupDirective,
            CursorKind::OMPCancellationPointDirective => CXCursor_OMPCancellationPointDirective,
            CursorKind::OMPCancelDirective => CXCursor_OMPCancelDirective,
            CursorKind::OMPTargetDataDirective => CXCursor_OMPTargetDataDirective,
            CursorKind::OMPTaskLoopDirective => CXCursor_OMPTaskLoopDirective,
            CursorKind::OMPTaskLoopSimdDirective => CXCursor_OMPTaskLoopSimdDirective,
            CursorKind::OMPDistributeDirective => CXCursor_OMPDistributeDirective,
            CursorKind::OMPTargetEnterDataDirective => CXCursor_OMPTargetEnterDataDirective,
            CursorKind::OMPTargetExitDataDirective => CXCursor_OMPTargetExitDataDirective,
            CursorKind::OMPTargetParallelDirective => CXCursor_OMPTargetParallelDirective,
            CursorKind::OMPTargetParallelForDirective => CXCursor_OMPTargetParallelForDirective,
            CursorKind::OMPTargetUpdateDirective => CXCursor_OMPTargetUpdateDirective,
            CursorKind::OMPDistributeParallelForDirective => CXCursor_OMPDistributeParallelForDirective,
            CursorKind::OMPDistributeParallelForSimdDirective => CXCursor_OMPDistributeParallelForSimdDirective,
            CursorKind::OMPDistributeSimdDirective => CXCursor_OMPDistributeSimdDirective,
            CursorKind::OMPTargetParallelForSimdDirective => CXCursor_OMPTargetParallelForSimdDirective,
            CursorKind::OMPTargetSimdDirective => CXCursor_OMPTargetSimdDirective,
            CursorKind::OMPTeamsDistributeDirective => CXCursor_OMPTeamsDistributeDirective,
            CursorKind::OMPTeamsDistributeSimdDirective => CXCursor_OMPTeamsDistributeSimdDirective,
            CursorKind::OMPTeamsDistributeParallelForSimdDirective => CXCursor_OMPTeamsDistributeParallelForSimdDirective,
            CursorKind::OMPTeamsDistributeParallelForDirective => CXCursor_OMPTeamsDistributeParallelForDirective,
            CursorKind::OMPTargetTeamsDirective => CXCursor_OMPTargetTeamsDirective,
            CursorKind::OMPTargetTeamsDistributeDirective => CXCursor_OMPTargetTeamsDistributeDirective,
            CursorKind::OMPTargetTeamsDistributeParallelForDirective => CXCursor_OMPTargetTeamsDistributeParallelForDirective,
            CursorKind::OMPTargetTeamsDistributeParallelForSimdDirective => CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective,
            CursorKind::OMPTargetTeamsDistributeSimdDirective => CXCursor_OMPTargetTeamsDistributeSimdDirective,
            CursorKind::BuiltinBitCastExpr => CXCursor_BuiltinBitCastExpr,
            CursorKind::OMPMasterTaskLoopDirective => CXCursor_OMPMasterTaskLoopDirective,
            CursorKind::OMPParallelMasterTaskLoopDirective => CXCursor_OMPParallelMasterTaskLoopDirective,
            CursorKind::OMPMasterTaskLoopSimdDirective => CXCursor_OMPMasterTaskLoopSimdDirective,
            CursorKind::OMPParallelMasterTaskLoopSimdDirective => CXCursor_OMPParallelMasterTaskLoopSimdDirective,
            CursorKind::OMPParallelMasterDirective => CXCursor_OMPParallelMasterDirective,
            CursorKind::OMPDepobjDirective => CXCursor_OMPDepobjDirective,
            CursorKind::OMPScanDirective => CXCursor_OMPScanDirective,
            CursorKind::OMPTileDirective => CXCursor_OMPTileDirective,
            CursorKind::OMPCanonicalLoop => CXCursor_OMPCanonicalLoop,
            CursorKind::OMPInteropDirective => CXCursor_OMPInteropDirective,
            CursorKind::OMPDispatchDirective => CXCursor_OMPDispatchDirective,
            CursorKind::OMPMaskedDirective => CXCursor_OMPMaskedDirective,
            CursorKind::OMPUnrollDirective => CXCursor_OMPUnrollDirective,
  
            CursorKind::TranslationUnit => CXCursor_TranslationUnit,

            CursorKind::UnexposedAttr => CXCursor_UnexposedAttr,
            
            CursorKind::IBActionAttr => CXCursor_IBActionAttr,
            CursorKind::IBOutletAttr => CXCursor_IBOutletAttr,
            CursorKind::IBOutletCollectionAttr => CXCursor_IBOutletCollectionAttr,
            CursorKind::CXXFinalAttr => CXCursor_CXXFinalAttr,
            CursorKind::CXXOverrideAttr => CXCursor_CXXOverrideAttr,
            CursorKind::AnnotateAttr => CXCursor_AnnotateAttr,
            CursorKind::AsmLabelAttr => CXCursor_AsmLabelAttr,
            CursorKind::PackedAttr => CXCursor_PackedAttr,
            CursorKind::PureAttr => CXCursor_PureAttr,
            CursorKind::ConstAttr => CXCursor_ConstAttr,
            CursorKind::NoDuplicateAttr => CXCursor_NoDuplicateAttr,
            CursorKind::CUDAConstantAttr => CXCursor_CUDAConstantAttr,
            CursorKind::CUDADeviceAttr => CXCursor_CUDADeviceAttr,
            CursorKind::CUDAGlobalAttr => CXCursor_CUDAGlobalAttr,
            CursorKind::CUDAHostAttr => CXCursor_CUDAHostAttr,
            CursorKind::CUDASharedAttr => CXCursor_CUDASharedAttr,
            CursorKind::VisibilityAttr => CXCursor_VisibilityAttr,
            CursorKind::DLLExport => CXCursor_DLLExport,
            CursorKind::DLLImport => CXCursor_DLLImport,
            CursorKind::NSReturnsRetained => CXCursor_NSReturnsRetained,
            CursorKind::NSReturnsNotRetained => CXCursor_NSReturnsNotRetained,
            CursorKind::NSReturnsAutoreleased => CXCursor_NSReturnsAutoreleased,
            CursorKind::NSConsumesSelf => CXCursor_NSConsumesSelf,
            CursorKind::NSConsumed => CXCursor_NSConsumed,
            CursorKind::ObjCException => CXCursor_ObjCException,
            CursorKind::ObjCNSObject => CXCursor_ObjCNSObject,
            CursorKind::ObjCIndependentClass => CXCursor_ObjCIndependentClass,
            CursorKind::ObjCPreciseLifetime => CXCursor_ObjCPreciseLifetime,
            CursorKind::ObjCReturnsInnerPointer => CXCursor_ObjCReturnsInnerPointer,
            CursorKind::ObjCRequiresSuper => CXCursor_ObjCRequiresSuper,
            CursorKind::ObjCRootClass => CXCursor_ObjCRootClass,
            CursorKind::ObjCSubclassingRestricted => CXCursor_ObjCSubclassingRestricted,
            CursorKind::ObjCExplicitProtocolImpl => CXCursor_ObjCExplicitProtocolImpl,
            CursorKind::ObjCDesignatedInitializer => CXCursor_ObjCDesignatedInitializer,
            CursorKind::ObjCRuntimeVisible => CXCursor_ObjCRuntimeVisible,
            CursorKind::ObjCBoxable => CXCursor_ObjCBoxable,
            CursorKind::FlagEnum => CXCursor_FlagEnum,
            CursorKind::ConvergentAttr => CXCursor_ConvergentAttr,
            CursorKind::WarnUnusedAttr => CXCursor_WarnUnusedAttr,
            CursorKind::WarnUnusedResultAttr => CXCursor_WarnUnusedResultAttr,
            CursorKind::AlignedAttr => CXCursor_AlignedAttr,
            CursorKind::PreprocessingDirective => CXCursor_PreprocessingDirective,
            CursorKind::MacroDefinition => CXCursor_MacroDefinition,
            CursorKind::MacroExpansion => CXCursor_MacroExpansion,

            CursorKind::InclusionDirective => CXCursor_InclusionDirective,

            CursorKind::ModuleImportDecl => CXCursor_ModuleImportDecl,
            CursorKind::TypeAliasTemplateDecl => CXCursor_TypeAliasTemplateDecl,
            CursorKind::StaticAssert => CXCursor_StaticAssert,
            CursorKind::FriendDecl => CXCursor_FriendDecl,
            CursorKind::OverloadCandidate => CXCursor_OverloadCandidate,
            _ => unimplemented!(),
        }
    }
}

impl From<CXCursorKind> for CursorKind {
    fn from(cx: CXCursorKind) -> Self {
        match cx {
            CXCursor_UnexposedDecl => CursorKind::UnexposedDecl,
            CXCursor_StructDecl => CursorKind::StructDecl,
            CXCursor_UnionDecl => CursorKind::UnionDecl,
            CXCursor_ClassDecl => CursorKind::ClassDecl,
            CXCursor_EnumDecl => CursorKind::EnumDecl,
            CXCursor_FieldDecl => CursorKind::FieldDecl,
            CXCursor_EnumConstantDecl => CursorKind::EnumConstantDecl,
            CXCursor_FunctionDecl => CursorKind::FunctionDecl,
            CXCursor_VarDecl => CursorKind::VarDecl,
            CXCursor_ParmDecl => CursorKind::ParmDecl,
            CXCursor_ObjCInterfaceDecl => CursorKind::ObjCInterfaceDecl,
            CXCursor_ObjCCategoryDecl => CursorKind::ObjCCategoryDecl,
            CXCursor_ObjCProtocolDecl => CursorKind::ObjCProtocolDecl,
            CXCursor_ObjCPropertyDecl => CursorKind::ObjCPropertyDecl,
            CXCursor_ObjCIvarDecl => CursorKind::ObjCIvarDecl,
            CXCursor_ObjCInstanceMethodDecl => CursorKind::ObjCInstanceMethodDecl,
            CXCursor_ObjCClassMethodDecl => CursorKind::ObjCClassMethodDecl,
            CXCursor_ObjCImplementationDecl => CursorKind::ObjCImplementationDecl,
            CXCursor_ObjCCategoryImplDecl => CursorKind::ObjCCategoryImplDecl,
            CXCursor_TypedefDecl => CursorKind::TypedefDecl,
            CXCursor_CXXMethod => CursorKind::CXXMethod,
            CXCursor_Namespace => CursorKind::Namespace,
            CXCursor_LinkageSpec => CursorKind::LinkageSpec,
            CXCursor_Constructor => CursorKind::Constructor,
            CXCursor_Destructor => CursorKind::Destructor,
            CXCursor_ConversionFunction => CursorKind::ConversionFunction,
            CXCursor_TemplateTypeParameter => CursorKind::TemplateTypeParameter,
            CXCursor_NonTypeTemplateParameter => CursorKind::NonTypeTemplateParameter,
            CXCursor_TemplateTemplateParameter => CursorKind::TemplateTemplateParameter,
            CXCursor_FunctionTemplate => CursorKind::FunctionTemplate,
            CXCursor_ClassTemplate => CursorKind::ClassTemplate,
            CXCursor_ClassTemplatePartialSpecialization => CursorKind::ClassTemplatePartialSpecialization,
            CXCursor_NamespaceAlias => CursorKind::NamespaceAlias,
            CXCursor_UsingDirective => CursorKind::UsingDirective,
            CXCursor_UsingDeclaration => CursorKind::UsingDeclaration,
            CXCursor_TypeAliasDecl => CursorKind::TypeAliasDecl,
            CXCursor_ObjCSynthesizeDecl => CursorKind::ObjCSynthesizeDecl,
            CXCursor_ObjCDynamicDecl => CursorKind::ObjCDynamicDecl,
            CXCursor_CXXAccessSpecifier => CursorKind::CXXAccessSpecifier,

            CXCursor_ObjCSuperClassRef => CursorKind::ObjCSuperClassRef,
            CXCursor_ObjCProtocolRef => CursorKind::ObjCProtocolRef,
            CXCursor_ObjCClassRef => CursorKind::ObjCClassRef,
            CXCursor_TypeRef => CursorKind::TypeRef,
            CXCursor_CXXBaseSpecifier => CursorKind::CXXBaseSpecifier,
            CXCursor_TemplateRef => CursorKind::TemplateRef,
            CXCursor_NamespaceRef => CursorKind::NamespaceRef,
            CXCursor_MemberRef => CursorKind::MemberRef,
            CXCursor_LabelRef => CursorKind::LabelRef,
            CXCursor_OverloadedDeclRef => CursorKind::OverloadedDeclRef,
            CXCursor_VariableRef => CursorKind::VariableRef,
  
            CXCursor_InvalidFile => CursorKind::InvalidFile,
            CXCursor_NoDeclFound => CursorKind::NoDeclFound,
            CXCursor_NotImplemented => CursorKind::NotImplemented,
            CXCursor_InvalidCode => CursorKind::InvalidCode,
  
            CXCursor_UnexposedExpr => CursorKind::UnexposedExpr,
            CXCursor_DeclRefExpr => CursorKind::DeclRefExpr,
            CXCursor_MemberRefExpr => CursorKind::MemberRefExpr,
            CXCursor_CallExpr => CursorKind::CallExpr,
            CXCursor_ObjCMessageExpr => CursorKind::ObjCMessageExpr,
            CXCursor_BlockExpr => CursorKind::BlockExpr,
            CXCursor_IntegerLiteral => CursorKind::IntegerLiteral,
            CXCursor_FloatingLiteral => CursorKind::FloatingLiteral,
            CXCursor_ImaginaryLiteral => CursorKind::ImaginaryLiteral,
            CXCursor_StringLiteral => CursorKind::StringLiteral,
            CXCursor_CharacterLiteral => CursorKind::CharacterLiteral,
            CXCursor_ParenExpr => CursorKind::ParenExpr,
            CXCursor_UnaryOperator => CursorKind::UnaryOperator,
            CXCursor_ArraySubscriptExpr => CursorKind::ArraySubscriptExpr,
            CXCursor_BinaryOperator => CursorKind::BinaryOperator,
            CXCursor_CompoundAssignOperator => CursorKind::CompoundAssignOperator,
            CXCursor_ConditionalOperator => CursorKind::ConditionalOperator,
            CXCursor_CStyleCastExpr => CursorKind::CStyleCastExpr,
            CXCursor_CompoundLiteralExpr => CursorKind::CompoundLiteralExpr,
            CXCursor_InitListExpr => CursorKind::InitListExpr,
            CXCursor_AddrLabelExpr => CursorKind::AddrLabelExpr,
            CXCursor_StmtExpr => CursorKind::StmtExpr,
            CXCursor_GenericSelectionExpr => CursorKind::GenericSelectionExpr,
            CXCursor_GNUNullExpr => CursorKind::GNUNullExpr,
            CXCursor_CXXStaticCastExpr => CursorKind::CXXStaticCastExpr,
            CXCursor_CXXDynamicCastExpr => CursorKind::CXXDynamicCastExpr,
            CXCursor_CXXReinterpretCastExpr => CursorKind::CXXReinterpretCastExpr,
            CXCursor_CXXConstCastExpr => CursorKind::CXXConstCastExpr,
            CXCursor_CXXFunctionalCastExpr => CursorKind::CXXFunctionalCastExpr,
            CXCursor_CXXTypeidExpr => CursorKind::CXXTypeidExpr,
            CXCursor_CXXBoolLiteralExpr => CursorKind::CXXBoolLiteralExpr,
            CXCursor_CXXNullPtrLiteralExpr => CursorKind::CXXNullPtrLiteralExpr,
            CXCursor_CXXThisExpr => CursorKind::CXXThisExpr,
            CXCursor_CXXThrowExpr => CursorKind::CXXThrowExpr,
            CXCursor_CXXNewExpr => CursorKind::CXXNewExpr,
            CXCursor_CXXDeleteExpr => CursorKind::CXXDeleteExpr,
            CXCursor_UnaryExpr => CursorKind::UnaryExpr,
            CXCursor_ObjCStringLiteral => CursorKind::ObjCStringLiteral,
            CXCursor_ObjCEncodeExpr => CursorKind::ObjCEncodeExpr,
            CXCursor_ObjCSelectorExpr => CursorKind::ObjCSelectorExpr,
            CXCursor_ObjCProtocolExpr => CursorKind::ObjCProtocolExpr,
            CXCursor_ObjCBridgedCastExpr => CursorKind::ObjCBridgedCastExpr,
            CXCursor_PackExpansionExpr => CursorKind::PackExpansionExpr,
            CXCursor_SizeOfPackExpr => CursorKind::SizeOfPackExpr,
            CXCursor_LambdaExpr => CursorKind::LambdaExpr,
            CXCursor_ObjCBoolLiteralExpr => CursorKind::ObjCBoolLiteralExpr,
            CXCursor_ObjCSelfExpr => CursorKind::ObjCSelfExpr,
            CXCursor_OMPArraySectionExpr => CursorKind::OMPArraySectionExpr,
            CXCursor_ObjCAvailabilityCheckExpr => CursorKind::ObjCAvailabilityCheckExpr,
            CXCursor_FixedPointLiteral => CursorKind::FixedPointLiteral,
            CXCursor_OMPArrayShapingExpr => CursorKind::OMPArrayShapingExpr,
            CXCursor_OMPIteratorExpr => CursorKind::OMPIteratorExpr,
            CXCursor_CXXAddrspaceCastExpr => CursorKind::CXXAddrspaceCastExpr,
            
            CXCursor_UnexposedStmt => CursorKind::UnexposedStmt,
            CXCursor_LabelStmt => CursorKind::LabelStmt,
            CXCursor_CompoundStmt => CursorKind::CompoundStmt,
            CXCursor_CaseStmt => CursorKind::CaseStmt,
            CXCursor_DefaultStmt => CursorKind::DefaultStmt,
            CXCursor_IfStmt => CursorKind::IfStmt,
            CXCursor_SwitchStmt => CursorKind::SwitchStmt,
            CXCursor_WhileStmt => CursorKind::WhileStmt,
            CXCursor_DoStmt => CursorKind::DoStmt,
            CXCursor_ForStmt => CursorKind::ForStmt,
            CXCursor_GotoStmt => CursorKind::GotoStmt,
            CXCursor_IndirectGotoStmt => CursorKind::IndirectGotoStmt,
            CXCursor_ContinueStmt => CursorKind::ContinueStmt,
            CXCursor_BreakStmt => CursorKind::BreakStmt,
            CXCursor_ReturnStmt => CursorKind::ReturnStmt,
            CXCursor_AsmStmt => CursorKind::GCCAsmStmt,
            CXCursor_ObjCAtTryStmt => CursorKind::ObjCAtTryStmt,
            CXCursor_ObjCAtCatchStmt => CursorKind::ObjCAtCatchStmt,
            CXCursor_ObjCAtFinallyStmt => CursorKind::ObjCAtFinallyStmt,
            CXCursor_ObjCAtThrowStmt => CursorKind::ObjCAtThrowStmt,
            CXCursor_ObjCAtSynchronizedStmt => CursorKind::ObjCAtSynchronizedStmt,
            CXCursor_ObjCAutoreleasePoolStmt => CursorKind::ObjCAutoreleasePoolStmt,
            CXCursor_ObjCForCollectionStmt => CursorKind::ObjCForCollectionStmt,
            CXCursor_CXXCatchStmt => CursorKind::CXXCatchStmt,
            CXCursor_CXXTryStmt => CursorKind::CXXTryStmt,
            CXCursor_CXXForRangeStmt => CursorKind::CXXForRangeStmt,
            CXCursor_SEHTryStmt => CursorKind::SEHTryStmt,
            CXCursor_SEHExceptStmt => CursorKind::SEHExceptStmt,
            CXCursor_SEHFinallyStmt => CursorKind::SEHFinallyStmt,
            CXCursor_MSAsmStmt => CursorKind::MSAsmStmt,
            CXCursor_NullStmt => CursorKind::NullStmt,
            CXCursor_DeclStmt => CursorKind::DeclStmt,


            CXCursor_OMPParallelDirective => CursorKind::OMPParallelDirective,
            CXCursor_OMPSimdDirective => CursorKind::OMPSimdDirective,
            CXCursor_OMPForDirective => CursorKind::OMPForDirective,
            CXCursor_OMPSectionsDirective => CursorKind::OMPSectionsDirective,
            CXCursor_OMPSectionDirective => CursorKind::OMPSectionDirective,
            CXCursor_OMPSingleDirective => CursorKind::OMPSingleDirective,
            CXCursor_OMPParallelForDirective => CursorKind::OMPParallelForDirective,
            CXCursor_OMPParallelSectionsDirective => CursorKind::OMPParallelSectionsDirective,
            CXCursor_OMPTaskDirective => CursorKind::OMPTaskDirective,
            CXCursor_OMPMasterDirective => CursorKind::OMPMasterDirective,
            CXCursor_OMPCriticalDirective => CursorKind::OMPCriticalDirective,
            CXCursor_OMPTaskyieldDirective => CursorKind::OMPTaskyieldDirective,
            CXCursor_OMPBarrierDirective => CursorKind::OMPBarrierDirective,
            CXCursor_OMPTaskwaitDirective => CursorKind::OMPTaskwaitDirective,
            CXCursor_OMPFlushDirective => CursorKind::OMPFlushDirective,
            CXCursor_SEHLeaveStmt => CursorKind::SEHLeaveStmt,
            CXCursor_OMPOrderedDirective => CursorKind::OMPOrderedDirective,
            CXCursor_OMPAtomicDirective => CursorKind::OMPAtomicDirective,
            CXCursor_OMPForSimdDirective => CursorKind::OMPForSimdDirective,
            CXCursor_OMPParallelForSimdDirective => CursorKind::OMPParallelForSimdDirective,
            CXCursor_OMPTargetDirective => CursorKind::OMPTargetDirective,
            CXCursor_OMPTeamsDirective => CursorKind::OMPTeamsDirective,
            CXCursor_OMPTaskgroupDirective => CursorKind::OMPTaskgroupDirective,
            CXCursor_OMPCancellationPointDirective => CursorKind::OMPCancellationPointDirective,
            CXCursor_OMPCancelDirective => CursorKind::OMPCancelDirective,
            CXCursor_OMPTargetDataDirective => CursorKind::OMPTargetDataDirective,
            CXCursor_OMPTaskLoopDirective => CursorKind::OMPTaskLoopDirective,
            CXCursor_OMPTaskLoopSimdDirective => CursorKind::OMPTaskLoopSimdDirective,
            CXCursor_OMPDistributeDirective => CursorKind::OMPDistributeDirective,
            CXCursor_OMPTargetEnterDataDirective => CursorKind::OMPTargetEnterDataDirective,
            CXCursor_OMPTargetExitDataDirective => CursorKind::OMPTargetExitDataDirective,
            CXCursor_OMPTargetParallelDirective => CursorKind::OMPTargetParallelDirective,
            CXCursor_OMPTargetParallelForDirective => CursorKind::OMPTargetParallelForDirective,
            CXCursor_OMPTargetUpdateDirective => CursorKind::OMPTargetUpdateDirective,
            CXCursor_OMPDistributeParallelForDirective => CursorKind::OMPDistributeParallelForDirective,
            CXCursor_OMPDistributeParallelForSimdDirective => CursorKind::OMPDistributeParallelForSimdDirective,
            CXCursor_OMPDistributeSimdDirective => CursorKind::OMPDistributeSimdDirective,
            CXCursor_OMPTargetParallelForSimdDirective => CursorKind::OMPTargetParallelForSimdDirective,
            CXCursor_OMPTargetSimdDirective => CursorKind::OMPTargetSimdDirective,
            CXCursor_OMPTeamsDistributeDirective => CursorKind::OMPTeamsDistributeDirective,
            CXCursor_OMPTeamsDistributeSimdDirective => CursorKind::OMPTeamsDistributeSimdDirective,
            CXCursor_OMPTeamsDistributeParallelForSimdDirective => CursorKind::OMPTeamsDistributeParallelForSimdDirective,
            CXCursor_OMPTeamsDistributeParallelForDirective => CursorKind::OMPTeamsDistributeParallelForDirective,
            CXCursor_OMPTargetTeamsDirective => CursorKind::OMPTargetTeamsDirective,
            CXCursor_OMPTargetTeamsDistributeDirective => CursorKind::OMPTargetTeamsDistributeDirective,
            CXCursor_OMPTargetTeamsDistributeParallelForDirective => CursorKind::OMPTargetTeamsDistributeParallelForDirective,
            CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective => CursorKind::OMPTargetTeamsDistributeParallelForSimdDirective,
            CXCursor_OMPTargetTeamsDistributeSimdDirective => CursorKind::OMPTargetTeamsDistributeSimdDirective,
            CXCursor_BuiltinBitCastExpr => CursorKind::BuiltinBitCastExpr,
            CXCursor_OMPMasterTaskLoopDirective => CursorKind::OMPMasterTaskLoopDirective,
            CXCursor_OMPParallelMasterTaskLoopDirective => CursorKind::OMPParallelMasterTaskLoopDirective,
            CXCursor_OMPMasterTaskLoopSimdDirective => CursorKind::OMPMasterTaskLoopSimdDirective,
            CXCursor_OMPParallelMasterTaskLoopSimdDirective => CursorKind::OMPParallelMasterTaskLoopSimdDirective,
            CXCursor_OMPParallelMasterDirective => CursorKind::OMPParallelMasterDirective,
            CXCursor_OMPDepobjDirective => CursorKind::OMPDepobjDirective,
            CXCursor_OMPScanDirective => CursorKind::OMPScanDirective,
            CXCursor_OMPTileDirective => CursorKind::OMPTileDirective,
            CXCursor_OMPCanonicalLoop => CursorKind::OMPCanonicalLoop,
            CXCursor_OMPInteropDirective => CursorKind::OMPInteropDirective,
            CXCursor_OMPDispatchDirective => CursorKind::OMPDispatchDirective,
            CXCursor_OMPMaskedDirective => CursorKind::OMPMaskedDirective,
            CXCursor_OMPUnrollDirective => CursorKind::OMPUnrollDirective,
  
            CXCursor_TranslationUnit => CursorKind::TranslationUnit,
            CXCursor_UnexposedAttr => CursorKind::UnexposedAttr,
            
            CXCursor_IBActionAttr => CursorKind::IBActionAttr,
            CXCursor_IBOutletAttr => CursorKind::IBOutletAttr,
            CXCursor_IBOutletCollectionAttr => CursorKind::IBOutletCollectionAttr,
            CXCursor_CXXFinalAttr => CursorKind::CXXFinalAttr,
            CXCursor_CXXOverrideAttr => CursorKind::CXXOverrideAttr,
            CXCursor_AnnotateAttr => CursorKind::AnnotateAttr,
            CXCursor_AsmLabelAttr => CursorKind::AsmLabelAttr,
            CXCursor_PackedAttr => CursorKind::PackedAttr,
            CXCursor_PureAttr => CursorKind::PureAttr,
            CXCursor_ConstAttr => CursorKind::ConstAttr,
            CXCursor_NoDuplicateAttr => CursorKind::NoDuplicateAttr,
            CXCursor_CUDAConstantAttr => CursorKind::CUDAConstantAttr,
            CXCursor_CUDADeviceAttr => CursorKind::CUDADeviceAttr,
            CXCursor_CUDAGlobalAttr => CursorKind::CUDAGlobalAttr,
            CXCursor_CUDAHostAttr => CursorKind::CUDAHostAttr,
            CXCursor_CUDASharedAttr => CursorKind::CUDASharedAttr,
            CXCursor_VisibilityAttr => CursorKind::VisibilityAttr,
            CXCursor_DLLExport => CursorKind::DLLExport,
            CXCursor_DLLImport => CursorKind::DLLImport,
            CXCursor_NSReturnsRetained => CursorKind::NSReturnsRetained,
            CXCursor_NSReturnsNotRetained => CursorKind::NSReturnsNotRetained,
            CXCursor_NSReturnsAutoreleased => CursorKind::NSReturnsAutoreleased,
            CXCursor_NSConsumesSelf => CursorKind::NSConsumesSelf,
            CXCursor_NSConsumed => CursorKind::NSConsumed,
            CXCursor_ObjCException => CursorKind::ObjCException,
            CXCursor_ObjCNSObject => CursorKind::ObjCNSObject,
            CXCursor_ObjCIndependentClass => CursorKind::ObjCIndependentClass,
            CXCursor_ObjCPreciseLifetime => CursorKind::ObjCPreciseLifetime,
            CXCursor_ObjCReturnsInnerPointer => CursorKind::ObjCReturnsInnerPointer,
            CXCursor_ObjCRequiresSuper => CursorKind::ObjCRequiresSuper,
            CXCursor_ObjCRootClass => CursorKind::ObjCRootClass,
            CXCursor_ObjCSubclassingRestricted => CursorKind::ObjCSubclassingRestricted,
            CXCursor_ObjCExplicitProtocolImpl => CursorKind::ObjCExplicitProtocolImpl,
            CXCursor_ObjCDesignatedInitializer => CursorKind::ObjCDesignatedInitializer,
            CXCursor_ObjCRuntimeVisible => CursorKind::ObjCRuntimeVisible,
            CXCursor_ObjCBoxable => CursorKind::ObjCBoxable,
            CXCursor_FlagEnum => CursorKind::FlagEnum,
            CXCursor_ConvergentAttr => CursorKind::ConvergentAttr,
            CXCursor_WarnUnusedAttr => CursorKind::WarnUnusedAttr,
            CXCursor_WarnUnusedResultAttr => CursorKind::WarnUnusedResultAttr,
            CXCursor_AlignedAttr => CursorKind::AlignedAttr,
            CXCursor_PreprocessingDirective => CursorKind::PreprocessingDirective,
            CXCursor_MacroDefinition => CursorKind::MacroDefinition,
            CXCursor_MacroExpansion => CursorKind::MacroExpansion,


            CXCursor_InclusionDirective => CursorKind::InclusionDirective,


            CXCursor_ModuleImportDecl => CursorKind::ModuleImportDecl,
            CXCursor_TypeAliasTemplateDecl => CursorKind::TypeAliasTemplateDecl,
            CXCursor_StaticAssert => CursorKind::StaticAssert,
            CXCursor_FriendDecl => CursorKind::FriendDecl,
            CXCursor_OverloadCandidate => CursorKind::OverloadCandidate,
            _ => unimplemented!()
        }
    }
}

impl Display for CursorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CursorKind {
   /* Declarations */
   /**
    * A declaration whose specific kind is not exposed via this
    * interface.
    *
    * Unexposed declarations have the same operations as any other kind
    * of declaration; one can extract their location information,
    * spelling, find their definitions, etc. However, the specific kind
    * of the declaration is not reported.
    */
   UnexposedDecl = 1,
   /** A C or C++ struct. */
   StructDecl = 2,
   /** A C or C++ union. */
   UnionDecl = 3,
   /** A C++ class. */
   ClassDecl = 4,
   /** An enumeration. */
   EnumDecl = 5,
   /**
    * A field (in C) or non-static data member (in C++) in a
    * struct, union, or C++ class.
    */
   FieldDecl = 6,
   /** An enumerator constant. */
   EnumConstantDecl = 7,
   /** A function. */
   FunctionDecl = 8,
   /** A variable. */
   VarDecl = 9,
   /** A function or method parameter. */
   ParmDecl = 10,
   /** An Objective-C \@interface. */
   ObjCInterfaceDecl = 11,
   /** An Objective-C \@interface for a category. */
   ObjCCategoryDecl = 12,
   /** An Objective-C \@protocol declaration. */
   ObjCProtocolDecl = 13,
   /** An Objective-C \@property declaration. */
   ObjCPropertyDecl = 14,
   /** An Objective-C instance variable. */
   ObjCIvarDecl = 15,
   /** An Objective-C instance method. */
   ObjCInstanceMethodDecl = 16,
   /** An Objective-C class method. */
   ObjCClassMethodDecl = 17,
   /** An Objective-C \@implementation. */
   ObjCImplementationDecl = 18,
   /** An Objective-C \@implementation for a category. */
   ObjCCategoryImplDecl = 19,
   /** A typedef. */
   TypedefDecl = 20,
   /** A C++ class method. */
   CXXMethod = 21,
   /** A C++ namespace. */
   Namespace = 22,
   /** A linkage specification, e.g. 'extern "C"'. */
   LinkageSpec = 23,
   /** A C++ constructor. */
   Constructor = 24,
   /** A C++ destructor. */
   Destructor = 25,
   /** A C++ conversion function. */
   ConversionFunction = 26,
   /** A C++ template type parameter. */
   TemplateTypeParameter = 27,
   /** A C++ non-type template parameter. */
   NonTypeTemplateParameter = 28,
   /** A C++ template template parameter. */
   TemplateTemplateParameter = 29,
   /** A C++ function template. */
   FunctionTemplate = 30,
   /** A C++ class template. */
   ClassTemplate = 31,
   /** A C++ class template partial specialization. */
   ClassTemplatePartialSpecialization = 32,
   /** A C++ namespace alias declaration. */
   NamespaceAlias = 33,
   /** A C++ using directive. */
   UsingDirective = 34,
   /** A C++ using declaration. */
   UsingDeclaration = 35,
   /** A C++ alias declaration */
   TypeAliasDecl = 36,
   /** An Objective-C \@synthesize definition. */
   ObjCSynthesizeDecl = 37,
   /** An Objective-C \@dynamic definition. */
   ObjCDynamicDecl = 38,
   /** An access specifier. */
   CXXAccessSpecifier = 39,
  
   /* References */
   ObjCSuperClassRef = 40,
   ObjCProtocolRef = 41,
   ObjCClassRef = 42,
   /**
    * A reference to a type declaration.
    *
    * A type reference occurs anywhere where a type is named but not
    * declared. For example, given:
    *
    * \code
    * typedef unsigned size_type;
    * size_type size;
    * \endcode
    *
    * The typedef is a declaration of size_type (TypedefDecl),
    * while the type of the variable "size" is referenced. The cursor
    * referenced by the type of size is the typedef for size_type.
    */
   TypeRef = 43,
   CXXBaseSpecifier = 44,
   /**
    * A reference to a class template, function template, template
    * template parameter, or class template partial specialization.
    */
   TemplateRef = 45,
   /**
    * A reference to a namespace or namespace alias.
    */
   NamespaceRef = 46,
   /**
    * A reference to a member of a struct, union, or class that occurs in
    * some non-expression context, e.g., a designated initializer.
    */
   MemberRef = 47,
   /**
    * A reference to a labeled statement.
    *
    * This cursor kind is used to describe the jump to "start_over" in the
    * goto statement in the following example:
    *
    * \code
    *   start_over:
    *     ++counter;
    *
    *     goto start_over;
    * \endcode
    *
    * A label reference cursor refers to a label statement.
    */
   LabelRef = 48,
  
   /**
    * A reference to a set of overloaded functions or function templates
    * that has not yet been resolved to a specific function or function template.
    *
    * An overloaded declaration reference cursor occurs in C++ templates where
    * a dependent name refers to a function. For example:
    *
    * \code
    * template<typename T> void swap(T&, T&);
    *
    * struct X { ... };
    * void swap(X&, X&);
    *
    * template<typename T>
    * void reverse(T* first, T* last) {
    *   while (first < last - 1) {
    *     swap(*first, *--last);
    *     ++first;
    *   }
    * }
    *
    * struct Y { };
    * void swap(Y&, Y&);
    * \endcode
    *
    * Here, the identifier "swap" is associated with an overloaded declaration
    * reference. In the template definition, "swap" refers to either of the two
    * "swap" functions declared above, so both results will be available. At
    * instantiation time, "swap" may also refer to other functions found via
    * argument-dependent lookup (e.g., the "swap" function at the end of the
    * example).
    *
    * The functions \c clang_getNumOverloadedDecls() and
    * \c clang_getOverloadedDecl() can be used to retrieve the definitions
    * referenced by this cursor.
    */
   OverloadedDeclRef = 49,
  
   /**
    * A reference to a variable that occurs in some non-expression
    * context, e.g., a C++ lambda capture list.
    */
   VariableRef = 50,
  
   /* Error conditions */
   InvalidFile = 70,
   NoDeclFound = 71,
   NotImplemented = 72,
   InvalidCode = 73,
  
   /**
    * An expression whose specific kind is not exposed via this
    * interface.
    *
    * Unexposed expressions have the same operations as any other kind
    * of expression; one can extract their location information,
    * spelling, children, etc. However, the specific kind of the
    * expression is not reported.
    */
   UnexposedExpr = 100,
  
   /**
    * An expression that refers to some value declaration, such
    * as a function, variable, or enumerator.
    */
   DeclRefExpr = 101,
  
   /**
    * An expression that refers to a member of a struct, union,
    * class, Objective-C class, etc.
    */
   MemberRefExpr = 102,
  
   /** An expression that calls a function. */
   CallExpr = 103,
  
   /** An expression that sends a message to an Objective-C
    object or class. */
   ObjCMessageExpr = 104,
  
   /** An expression that represents a block literal. */
   BlockExpr = 105,
  
   /** An integer literal.
    */
   IntegerLiteral = 106,
  
   /** A floating point number literal.
    */
   FloatingLiteral = 107,
  
   /** An imaginary number literal.
    */
   ImaginaryLiteral = 108,
  
   /** A string literal.
    */
   StringLiteral = 109,
  
   /** A character literal.
    */
   CharacterLiteral = 110,
  
   /** A parenthesized expression, e.g. "(1)".
    *
    * This AST node is only formed if full location information is requested.
    */
   ParenExpr = 111,
  
   /** This represents the unary-expression's (except sizeof and
    * alignof).
    */
   UnaryOperator = 112,
  
   /** [C99 6.5.2.1] Array Subscripting.
    */
   ArraySubscriptExpr = 113,
  
   /** A builtin binary operation expression such as "x + y" or
    * "x <= y".
    */
   BinaryOperator = 114,
  
   /** Compound assignment such as "+=".
    */
   CompoundAssignOperator = 115,
  
   /** The ?: ternary operator.
    */
   ConditionalOperator = 116,
  
   /** An explicit cast in C (C99 6.5.4) or a C-style cast in C++
    * (C++ [expr.cast]), which uses the syntax (Type)expr.
    *
    * For example: (int)f.
    */
   CStyleCastExpr = 117,
  
   /** [C99 6.5.2.5]
    */
   CompoundLiteralExpr = 118,
  
   /** Describes an C or C++ initializer list.
    */
   InitListExpr = 119,
  
   /** The GNU address of label extension, representing &&label.
    */
   AddrLabelExpr = 120,
  
   /** This is the GNU Statement Expression extension: ({int X=4; X;})
    */
   StmtExpr = 121,
  
   /** Represents a C11 generic selection.
    */
   GenericSelectionExpr = 122,
  
   /** Implements the GNU __null extension, which is a name for a null
    * pointer constant that has integral type (e.g., int or long) and is the same
    * size and alignment as a pointer.
    *
    * The __null extension is typically only used by system headers, which define
    * NULL as __null in C++ rather than using 0 (which is an integer that may not
    * match the size of a pointer).
    */
   GNUNullExpr = 123,
  
   /** C++'s static_cast<> expression.
    */
   CXXStaticCastExpr = 124,
  
   /** C++'s dynamic_cast<> expression.
    */
   CXXDynamicCastExpr = 125,
  
   /** C++'s reinterpret_cast<> expression.
    */
   CXXReinterpretCastExpr = 126,
  
   /** C++'s const_cast<> expression.
    */
   CXXConstCastExpr = 127,
  
   /** Represents an explicit C++ type conversion that uses "functional"
    * notion (C++ [expr.type.conv]).
    *
    * Example:
    * \code
    *   x = int(0.5);
    * \endcode
    */
   CXXFunctionalCastExpr = 128,
  
   /** A C++ typeid expression (C++ [expr.typeid]).
    */
   CXXTypeidExpr = 129,
  
   /** [C++ 2.13.5] C++ Boolean Literal.
    */
   CXXBoolLiteralExpr = 130,
  
   /** [C++0x 2.14.7] C++ Pointer Literal.
    */
   CXXNullPtrLiteralExpr = 131,
  
   /** Represents the "this" expression in C++
    */
   CXXThisExpr = 132,
  
   /** [C++ 15] C++ Throw Expression.
    *
    * This handles 'throw' and 'throw' assignment-expression. When
    * assignment-expression isn't present, Op will be null.
    */
   CXXThrowExpr = 133,
  
   /** A new expression for memory allocation and constructor calls, e.g:
    * "new CXXNewExpr(foo)".
    */
   CXXNewExpr = 134,
  
   /** A delete expression for memory deallocation and destructor calls,
    * e.g. "delete[] pArray".
    */
   CXXDeleteExpr = 135,
  
   /** A unary expression. (noexcept, sizeof, or other traits)
    */
   UnaryExpr = 136,
  
   /** An Objective-C string literal i.e. @"foo".
    */
   ObjCStringLiteral = 137,
  
   /** An Objective-C \@encode expression.
    */
   ObjCEncodeExpr = 138,
  
   /** An Objective-C \@selector expression.
    */
   ObjCSelectorExpr = 139,
  
   /** An Objective-C \@protocol expression.
    */
   ObjCProtocolExpr = 140,
  
   /** An Objective-C "bridged" cast expression, which casts between
    * Objective-C pointers and C pointers, transferring ownership in the process.
    *
    * \code
    *   NSString *str = (__bridge_transfer NSString *)CFCreateString();
    * \endcode
    */
   ObjCBridgedCastExpr = 141,
  
   /** Represents a C++0x pack expansion that produces a sequence of
    * expressions.
    *
    * A pack expansion expression contains a pattern (which itself is an
    * expression) followed by an ellipsis. For example:
    *
    * \code
    * template<typename F, typename ...Types>
    * void forward(F f, Types &&...args) {
    *  f(static_cast<Types&&>(args)...);
    * }
    * \endcode
    */
   PackExpansionExpr = 142,
  
   /** Represents an expression that computes the length of a parameter
    * pack.
    *
    * \code
    * template<typename ...Types>
    * struct count {
    *   static const unsigned value = sizeof...(Types);
    * };
    * \endcode
    */
   SizeOfPackExpr = 143,
  
   /* Represents a C++ lambda expression that produces a local function
    * object.
    *
    * \code
    * void abssort(float *x, unsigned N) {
    *   std::sort(x, x + N,
    *             [](float a, float b) {
    *               return std::abs(a) < std::abs(b);
    *             });
    * }
    * \endcode
    */
   LambdaExpr = 144,
  
   /** Objective-c Boolean Literal.
    */
   ObjCBoolLiteralExpr = 145,
  
   /** Represents the "self" expression in an Objective-C method.
    */
   ObjCSelfExpr = 146,
  
   /** OpenMP 5.0 [2.1.5, Array Section].
    */
   OMPArraySectionExpr = 147,
  
   /** Represents an @available(...) check.
    */
   ObjCAvailabilityCheckExpr = 148,
  
   /**
    * Fixed point literal
    */
   FixedPointLiteral = 149,
  
   /** OpenMP 5.0 [2.1.4, Array Shaping].
    */
   OMPArrayShapingExpr = 150,
  
   /**
    * OpenMP 5.0 [2.1.6 Iterators]
    */
   OMPIteratorExpr = 151,
  
   /** OpenCL's addrspace_cast<> expression.
    */
   CXXAddrspaceCastExpr = 152,
  
   /**
    * Expression that references a C++20 concept.
    */
   ConceptSpecializationExpr = 153,
  
   /**
    * Expression that references a C++20 concept.
    */
   RequiresExpr = 154,
  
   /* Statements */
   /**
    * A statement whose specific kind is not exposed via this
    * interface.
    *
    * Unexposed statements have the same operations as any other kind of
    * statement; one can extract their location information, spelling,
    * children, etc. However, the specific kind of the statement is not
    * reported.
    */
   UnexposedStmt = 200,
  
   /** A labelled statement in a function.
    *
    * This cursor kind is used to describe the "start_over:" label statement in
    * the following example:
    *
    * \code
    *   start_over:
    *     ++counter;
    * \endcode
    *
    */
   LabelStmt = 201,
  
   /** A group of statements like { stmt stmt }.
    *
    * This cursor kind is used to describe compound statements, e.g. function
    * bodies.
    */
   CompoundStmt = 202,
  
   /** A case statement.
    */
   CaseStmt = 203,
  
   /** A default statement.
    */
   DefaultStmt = 204,
  
   /** An if statement
    */
   IfStmt = 205,
  
   /** A switch statement.
    */
   SwitchStmt = 206,
  
   /** A while statement.
    */
   WhileStmt = 207,
  
   /** A do statement.
    */
   DoStmt = 208,
  
   /** A for statement.
    */
   ForStmt = 209,
  
   /** A goto statement.
    */
   GotoStmt = 210,
  
   /** An indirect goto statement.
    */
   IndirectGotoStmt = 211,
  
   /** A continue statement.
    */
   ContinueStmt = 212,
  
   /** A break statement.
    */
   BreakStmt = 213,
  
   /** A return statement.
    */
   ReturnStmt = 214,
  
   /** A GCC inline assembly statement extension.
    */
   GCCAsmStmt = 215,
  
   /** Objective-C's overall \@try-\@catch-\@finally statement.
    */
   ObjCAtTryStmt = 216,
  
   /** Objective-C's \@catch statement.
    */
   ObjCAtCatchStmt = 217,
  
   /** Objective-C's \@finally statement.
    */
   ObjCAtFinallyStmt = 218,
  
   /** Objective-C's \@throw statement.
    */
   ObjCAtThrowStmt = 219,
  
   /** Objective-C's \@synchronized statement.
    */
   ObjCAtSynchronizedStmt = 220,
  
   /** Objective-C's autorelease pool statement.
    */
   ObjCAutoreleasePoolStmt = 221,
  
   /** Objective-C's collection statement.
    */
   ObjCForCollectionStmt = 222,
  
   /** C++'s catch statement.
    */
   CXXCatchStmt = 223,
  
   /** C++'s try statement.
    */
   CXXTryStmt = 224,
  
   /** C++'s for (* : *) statement.
    */
   CXXForRangeStmt = 225,
  
   /** Windows Structured Exception Handling's try statement.
    */
   SEHTryStmt = 226,
  
   /** Windows Structured Exception Handling's except statement.
    */
   SEHExceptStmt = 227,
  
   /** Windows Structured Exception Handling's finally statement.
    */
   SEHFinallyStmt = 228,
  
   /** A MS inline assembly statement extension.
    */
   MSAsmStmt = 229,
  
   /** The null statement ";": C99 6.8.3p3.
    *
    * This cursor kind is used to describe the null statement.
    */
   NullStmt = 230,
  
   /** Adaptor class for mixing declarations with statements and
    * expressions.
    */
   DeclStmt = 231,
  
   /** OpenMP parallel directive.
    */
   OMPParallelDirective = 232,
  
   /** OpenMP SIMD directive.
    */
   OMPSimdDirective = 233,
  
   /** OpenMP for directive.
    */
   OMPForDirective = 234,
  
   /** OpenMP sections directive.
    */
   OMPSectionsDirective = 235,
  
   /** OpenMP section directive.
    */
   OMPSectionDirective = 236,
  
   /** OpenMP single directive.
    */
   OMPSingleDirective = 237,
  
   /** OpenMP parallel for directive.
    */
   OMPParallelForDirective = 238,
  
   /** OpenMP parallel sections directive.
    */
   OMPParallelSectionsDirective = 239,
  
   /** OpenMP task directive.
    */
   OMPTaskDirective = 240,
  
   /** OpenMP master directive.
    */
   OMPMasterDirective = 241,
  
   /** OpenMP critical directive.
    */
   OMPCriticalDirective = 242,
  
   /** OpenMP taskyield directive.
    */
   OMPTaskyieldDirective = 243,
  
   /** OpenMP barrier directive.
    */
   OMPBarrierDirective = 244,
  
   /** OpenMP taskwait directive.
    */
   OMPTaskwaitDirective = 245,
  
   /** OpenMP flush directive.
    */
   OMPFlushDirective = 246,
  
   /** Windows Structured Exception Handling's leave statement.
    */
   SEHLeaveStmt = 247,
  
   /** OpenMP ordered directive.
    */
   OMPOrderedDirective = 248,
  
   /** OpenMP atomic directive.
    */
   OMPAtomicDirective = 249,
  
   /** OpenMP for SIMD directive.
    */
   OMPForSimdDirective = 250,
  
   /** OpenMP parallel for SIMD directive.
    */
   OMPParallelForSimdDirective = 251,
  
   /** OpenMP target directive.
    */
   OMPTargetDirective = 252,
  
   /** OpenMP teams directive.
    */
   OMPTeamsDirective = 253,
  
   /** OpenMP taskgroup directive.
    */
   OMPTaskgroupDirective = 254,
  
   /** OpenMP cancellation point directive.
    */
   OMPCancellationPointDirective = 255,
  
   /** OpenMP cancel directive.
    */
   OMPCancelDirective = 256,
  
   /** OpenMP target data directive.
    */
   OMPTargetDataDirective = 257,
  
   /** OpenMP taskloop directive.
    */
   OMPTaskLoopDirective = 258,
  
   /** OpenMP taskloop simd directive.
    */
   OMPTaskLoopSimdDirective = 259,
  
   /** OpenMP distribute directive.
    */
   OMPDistributeDirective = 260,
  
   /** OpenMP target enter data directive.
    */
   OMPTargetEnterDataDirective = 261,
  
   /** OpenMP target exit data directive.
    */
   OMPTargetExitDataDirective = 262,
  
   /** OpenMP target parallel directive.
    */
   OMPTargetParallelDirective = 263,
  
   /** OpenMP target parallel for directive.
    */
   OMPTargetParallelForDirective = 264,
  
   /** OpenMP target update directive.
    */
   OMPTargetUpdateDirective = 265,
  
   /** OpenMP distribute parallel for directive.
    */
   OMPDistributeParallelForDirective = 266,
  
   /** OpenMP distribute parallel for simd directive.
    */
   OMPDistributeParallelForSimdDirective = 267,
  
   /** OpenMP distribute simd directive.
    */
   OMPDistributeSimdDirective = 268,
  
   /** OpenMP target parallel for simd directive.
    */
   OMPTargetParallelForSimdDirective = 269,
  
   /** OpenMP target simd directive.
    */
   OMPTargetSimdDirective = 270,
  
   /** OpenMP teams distribute directive.
    */
   OMPTeamsDistributeDirective = 271,
  
   /** OpenMP teams distribute simd directive.
    */
   OMPTeamsDistributeSimdDirective = 272,
  
   /** OpenMP teams distribute parallel for simd directive.
    */
   OMPTeamsDistributeParallelForSimdDirective = 273,
  
   /** OpenMP teams distribute parallel for directive.
    */
   OMPTeamsDistributeParallelForDirective = 274,
  
   /** OpenMP target teams directive.
    */
   OMPTargetTeamsDirective = 275,
  
   /** OpenMP target teams distribute directive.
    */
   OMPTargetTeamsDistributeDirective = 276,
  
   /** OpenMP target teams distribute parallel for directive.
    */
   OMPTargetTeamsDistributeParallelForDirective = 277,
  
   /** OpenMP target teams distribute parallel for simd directive.
    */
   OMPTargetTeamsDistributeParallelForSimdDirective = 278,
  
   /** OpenMP target teams distribute simd directive.
    */
   OMPTargetTeamsDistributeSimdDirective = 279,
  
   /** C++2a std::bit_cast expression.
    */
   BuiltinBitCastExpr = 280,
  
   /** OpenMP master taskloop directive.
    */
   OMPMasterTaskLoopDirective = 281,
  
   /** OpenMP parallel master taskloop directive.
    */
   OMPParallelMasterTaskLoopDirective = 282,
  
   /** OpenMP master taskloop simd directive.
    */
   OMPMasterTaskLoopSimdDirective = 283,
  
   /** OpenMP parallel master taskloop simd directive.
    */
   OMPParallelMasterTaskLoopSimdDirective = 284,
  
   /** OpenMP parallel master directive.
    */
   OMPParallelMasterDirective = 285,
  
   /** OpenMP depobj directive.
    */
   OMPDepobjDirective = 286,
  
   /** OpenMP scan directive.
    */
   OMPScanDirective = 287,
  
   /** OpenMP tile directive.
    */
   OMPTileDirective = 288,
  
   /** OpenMP canonical loop.
    */
   OMPCanonicalLoop = 289,
  
   /** OpenMP interop directive.
    */
   OMPInteropDirective = 290,
  
   /** OpenMP dispatch directive.
    */
   OMPDispatchDirective = 291,
  
   /** OpenMP masked directive.
    */
   OMPMaskedDirective = 292,
  
   /** OpenMP unroll directive.
    */
   OMPUnrollDirective = 293,
  
   /** OpenMP metadirective directive.
    */
   OMPMetaDirective = 294,
  
   /** OpenMP loop directive.
    */
   OMPGenericLoopDirective = 295,
  
   /** OpenMP teams loop directive.
    */
   OMPTeamsGenericLoopDirective = 296,
  
   /** OpenMP target teams loop directive.
    */
   OMPTargetTeamsGenericLoopDirective = 297,
  
   /** OpenMP parallel loop directive.
    */
   OMPParallelGenericLoopDirective = 298,
  
   /** OpenMP target parallel loop directive.
    */
   OMPTargetParallelGenericLoopDirective = 299,
  
   /** OpenMP parallel masked directive.
    */
   OMPParallelMaskedDirective = 300,
  
   /** OpenMP masked taskloop directive.
    */
   OMPMaskedTaskLoopDirective = 301,
  
   /** OpenMP masked taskloop simd directive.
    */
   OMPMaskedTaskLoopSimdDirective = 302,
  
   /** OpenMP parallel masked taskloop directive.
    */
   OMPParallelMaskedTaskLoopDirective = 303,
  
   /** OpenMP parallel masked taskloop simd directive.
    */
   OMPParallelMaskedTaskLoopSimdDirective = 304,
  
   /**
    * Cursor that represents the translation unit itself.
    *
    * The translation unit cursor exists primarily to act as the root
    * cursor for traversing the contents of a translation unit.
    */
   TranslationUnit = 350,
  
   /* Attributes */
   /**
    * An attribute whose specific kind is not exposed via this
    * interface.
    */
   UnexposedAttr = 400,
  
   IBActionAttr = 401,
   IBOutletAttr = 402,
   IBOutletCollectionAttr = 403,
   CXXFinalAttr = 404,
   CXXOverrideAttr = 405,
   AnnotateAttr = 406,
   AsmLabelAttr = 407,
   PackedAttr = 408,
   PureAttr = 409,
   ConstAttr = 410,
   NoDuplicateAttr = 411,
   CUDAConstantAttr = 412,
   CUDADeviceAttr = 413,
   CUDAGlobalAttr = 414,
   CUDAHostAttr = 415,
   CUDASharedAttr = 416,
   VisibilityAttr = 417,
   DLLExport = 418,
   DLLImport = 419,
   NSReturnsRetained = 420,
   NSReturnsNotRetained = 421,
   NSReturnsAutoreleased = 422,
   NSConsumesSelf = 423,
   NSConsumed = 424,
   ObjCException = 425,
   ObjCNSObject = 426,
   ObjCIndependentClass = 427,
   ObjCPreciseLifetime = 428,
   ObjCReturnsInnerPointer = 429,
   ObjCRequiresSuper = 430,
   ObjCRootClass = 431,
   ObjCSubclassingRestricted = 432,
   ObjCExplicitProtocolImpl = 433,
   ObjCDesignatedInitializer = 434,
   ObjCRuntimeVisible = 435,
   ObjCBoxable = 436,
   FlagEnum = 437,
   ConvergentAttr = 438,
   WarnUnusedAttr = 439,
   WarnUnusedResultAttr = 440,
   AlignedAttr = 441,
  
   /* Preprocessing */
   PreprocessingDirective = 500,
   MacroDefinition = 501,
   MacroExpansion = 502,
   InclusionDirective = 503,
  
   /* Extra Declarations */
   /**
    * A module import declaration.
    */
   ModuleImportDecl = 600,
   TypeAliasTemplateDecl = 601,
   /**
    * A static_assert or _Static_assert node
    */
   StaticAssert = 602,
   /**
    * a friend declaration.
    */
   FriendDecl = 603,
   /**
    * a concept declaration.
    */
   ConceptDecl = 604,
  
   /**
    * A code completion overload candidate.
    */
   OverloadCandidate = 700
}

