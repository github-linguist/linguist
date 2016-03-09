// start
// define cpp macros: SET_SYMBOL, CREATE_CLASS, SET_CLASS, DEFINE_BASE_CLASSES, DEFINE_CLASS_NAMES, EXPOSE_TO_CANDO 
// define cpp macro: ALL_STAGES to get the effect of defining all of the macros above
// define cpp macro: EXPOSE_PYTHON to expose python
// Associating namespace(llvmo) with package(LlvmoPkg)
 // class DebugLoc_O : public core::T_O
 // class InsertPoint_O : public core::T_O
 // class LLVMContext_O : public core::ExternalObject_O
 // class Pass_O : public core::ExternalObject_O
 // class FunctionPass_O : public Pass_O
 // class ModulePass_O : public Pass_O
 // class ImmutablePass_O : public ModulePass_O
 // class PassManagerBase_O : public core::ExternalObject_O
 // class Value_O : public core::ExternalObject_O
 // class User_O : public Value_O
 // class LLVMContext_O : public core::ExternalObject_O
 // class Pass_O : public core::ExternalObject_O
 // class FunctionPass_O : public Pass_O
 // class ModulePass_O : public Pass_O
 // class ImmutablePass_O : public ModulePass_O
 // class PassManagerBase_O : public core::ExternalObject_O
 // class Value_O : public core::ExternalObject_O
 // class User_O : public Value_O
 // class Attribute_O : public core::T_O
 // class DataLayout_O : public ImmutablePass_O
 // class Constant_O : public User_O
 // class ConstantArray_O : public Constant_O
 // class ConstantDataSequential_O : public Constant_O
 // class ConstantDataArray_O : public ConstantDataSequential_O
 // class ConstantExpr_O : public Constant_O
 // class GlobalValue_O : public Constant_O
 // class GlobalVariable_O : public GlobalValue_O
 // class ExecutionEngine_O : public core::ExternalObject_O
 // class Module_O : public core::ExternalObject_O
 // class FunctionPassManager_O : public PassManagerBase_O
 // class EngineBuilder_O : public core::ExternalObject_O
 // class APFloat_O : public core::ExternalObject_O
 // class APInt_O : public core::ExternalObject_O
 // class IRBuilderBase_O : public core::ExternalObject_O
 // class IRBuilder_O : public IRBuilderBase_O
 // class Instruction_O : public User_O
 // class StoreInst_O : public Instruction_O
 // class FenceInst_O : public Instruction_O
 // class AtomicCmpXchgInst_O : public Instruction_O
 // class AtomicRMWInst_O : public Instruction_O
 // class PHINode_O : public Instruction_O
 // class CallInst_O : public Instruction_O
 // class LandingPadInst_O : public Instruction_O
 // class UnaryInstruction_O : public Instruction_O
 // class AllocaInst_O : public UnaryInstruction_O
 // class VAArgInst_O : public UnaryInstruction_O
 // class LoadInst_O : public UnaryInstruction_O
 // class TerminatorInst_O : public Instruction_O
 // class BranchInst_O : public TerminatorInst_O
 // class SwitchInst_O : public TerminatorInst_O
 // class IndirectBrInst_O : public TerminatorInst_O
 // class InvokeInst_O : public TerminatorInst_O
 // class ResumeInst_O : public TerminatorInst_O
 // class UnreachableInst_O : public TerminatorInst_O
 // class ReturnInst_O : public TerminatorInst_O
 // class ConstantFP_O : public Constant_O
 // class ConstantInt_O : public Constant_O
 // class UndefValue_O : public Constant_O
 // class ConstantPointerNull_O : public Constant_O
 // class MDNode_O : public Value_O
 // class MDString_O : public Value_O
 // class Function_O : public GlobalValue_O
 // class BasicBlock_O : public Value_O
 // class Argument_O : public Value_O
 // class Type_O : public core::ExternalObject_O
 // class FunctionType_O : public Type_O
 // class IntegerType_O : public Type_O
 // class CompositeType_O : public Type_O
 // class StructType_O : public CompositeType_O
 // class SequentialType_O : public CompositeType_O
 // class PointerType_O : public SequentialType_O
 // class ArrayType_O : public SequentialType_O
 // class VectorType_O : public SequentialType_O
 // class LLVMContext_O : public core::ExternalObject_O
 // class Module_O : public core::ExternalObject_O
 // class Builder_O : public core::ExternalObject_O
 // class APFloat_O : public core::ExternalObject_O
 // class Value_O : public core::ExternalObject_O
 // class User_O : public Value_O
 // class Constant_O : public User_O
 // class ConstantFP_O : public Constant_O
// Associating namespace(llvmo) with package(LlvmoPkg)
#ifdef HEADER_INCLUDES
#include "llvmoExpose.h"
#include "llvmoExpose.scrape_inc.h"
#include "debugLoc.h"
#include "insertPoint.h"
#include "llvmoExpose.generated.h"
#endif // HEADER_INCLUDES
#undef HEADER_INCLUDES
#if defined(SET_SYMBOL) || defined(ALL_STAGES)
// requires LOOKUP_SYMBOL(pkg,symbolName) be defined
llvmo::APFloat_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APFloat_O::static_packageName(),llvmo::APFloat_O::static_className()));
llvmo::APInt_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::APInt_O::static_packageName(),llvmo::APInt_O::static_className()));
llvmo::Attribute_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Attribute_O::static_packageName(),llvmo::Attribute_O::static_className()));
llvmo::Builder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Builder_O::static_packageName(),llvmo::Builder_O::static_className()));
llvmo::DebugLoc_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DebugLoc_O::static_packageName(),llvmo::DebugLoc_O::static_className()));
llvmo::EngineBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::EngineBuilder_O::static_packageName(),llvmo::EngineBuilder_O::static_className()));
llvmo::ExecutionEngine_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ExecutionEngine_O::static_packageName(),llvmo::ExecutionEngine_O::static_className()));
llvmo::IRBuilderBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IRBuilderBase_O::static_packageName(),llvmo::IRBuilderBase_O::static_className()));
llvmo::InsertPoint_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::InsertPoint_O::static_packageName(),llvmo::InsertPoint_O::static_className()));
llvmo::LLVMContext_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LLVMContext_O::static_packageName(),llvmo::LLVMContext_O::static_className()));
llvmo::Module_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Module_O::static_packageName(),llvmo::Module_O::static_className()));
llvmo::PassManagerBase_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PassManagerBase_O::static_packageName(),llvmo::PassManagerBase_O::static_className()));
llvmo::Pass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Pass_O::static_packageName(),llvmo::Pass_O::static_className()));
llvmo::Type_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Type_O::static_packageName(),llvmo::Type_O::static_className()));
llvmo::Value_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Value_O::static_packageName(),llvmo::Value_O::static_className()));
llvmo::Argument_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Argument_O::static_packageName(),llvmo::Argument_O::static_className()));
llvmo::BasicBlock_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::BasicBlock_O::static_packageName(),llvmo::BasicBlock_O::static_className()));
llvmo::CompositeType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::CompositeType_O::static_packageName(),llvmo::CompositeType_O::static_className()));
llvmo::FunctionPassManager_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPassManager_O::static_packageName(),llvmo::FunctionPassManager_O::static_className()));
llvmo::FunctionPass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionPass_O::static_packageName(),llvmo::FunctionPass_O::static_className()));
llvmo::FunctionType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FunctionType_O::static_packageName(),llvmo::FunctionType_O::static_className()));
llvmo::IRBuilder_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IRBuilder_O::static_packageName(),llvmo::IRBuilder_O::static_className()));
llvmo::IntegerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IntegerType_O::static_packageName(),llvmo::IntegerType_O::static_className()));
llvmo::MDNode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDNode_O::static_packageName(),llvmo::MDNode_O::static_className()));
llvmo::MDString_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::MDString_O::static_packageName(),llvmo::MDString_O::static_className()));
llvmo::ModulePass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ModulePass_O::static_packageName(),llvmo::ModulePass_O::static_className()));
llvmo::User_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::User_O::static_packageName(),llvmo::User_O::static_className()));
llvmo::Constant_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Constant_O::static_packageName(),llvmo::Constant_O::static_className()));
llvmo::ImmutablePass_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ImmutablePass_O::static_packageName(),llvmo::ImmutablePass_O::static_className()));
llvmo::Instruction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Instruction_O::static_packageName(),llvmo::Instruction_O::static_className()));
llvmo::SequentialType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::SequentialType_O::static_packageName(),llvmo::SequentialType_O::static_className()));
llvmo::StructType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::StructType_O::static_packageName(),llvmo::StructType_O::static_className()));
llvmo::ArrayType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ArrayType_O::static_packageName(),llvmo::ArrayType_O::static_className()));
llvmo::AtomicCmpXchgInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AtomicCmpXchgInst_O::static_packageName(),llvmo::AtomicCmpXchgInst_O::static_className()));
llvmo::AtomicRMWInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AtomicRMWInst_O::static_packageName(),llvmo::AtomicRMWInst_O::static_className()));
llvmo::CallInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::CallInst_O::static_packageName(),llvmo::CallInst_O::static_className()));
llvmo::ConstantArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantArray_O::static_packageName(),llvmo::ConstantArray_O::static_className()));
llvmo::ConstantDataSequential_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantDataSequential_O::static_packageName(),llvmo::ConstantDataSequential_O::static_className()));
llvmo::ConstantExpr_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantExpr_O::static_packageName(),llvmo::ConstantExpr_O::static_className()));
llvmo::ConstantFP_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantFP_O::static_packageName(),llvmo::ConstantFP_O::static_className()));
llvmo::ConstantInt_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantInt_O::static_packageName(),llvmo::ConstantInt_O::static_className()));
llvmo::ConstantPointerNull_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantPointerNull_O::static_packageName(),llvmo::ConstantPointerNull_O::static_className()));
llvmo::DataLayout_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::DataLayout_O::static_packageName(),llvmo::DataLayout_O::static_className()));
llvmo::FenceInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::FenceInst_O::static_packageName(),llvmo::FenceInst_O::static_className()));
llvmo::GlobalValue_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::GlobalValue_O::static_packageName(),llvmo::GlobalValue_O::static_className()));
llvmo::LandingPadInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LandingPadInst_O::static_packageName(),llvmo::LandingPadInst_O::static_className()));
llvmo::PHINode_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PHINode_O::static_packageName(),llvmo::PHINode_O::static_className()));
llvmo::PointerType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::PointerType_O::static_packageName(),llvmo::PointerType_O::static_className()));
llvmo::StoreInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::StoreInst_O::static_packageName(),llvmo::StoreInst_O::static_className()));
llvmo::TerminatorInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::TerminatorInst_O::static_packageName(),llvmo::TerminatorInst_O::static_className()));
llvmo::UnaryInstruction_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UnaryInstruction_O::static_packageName(),llvmo::UnaryInstruction_O::static_className()));
llvmo::UndefValue_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UndefValue_O::static_packageName(),llvmo::UndefValue_O::static_className()));
llvmo::VectorType_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::VectorType_O::static_packageName(),llvmo::VectorType_O::static_className()));
llvmo::AllocaInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::AllocaInst_O::static_packageName(),llvmo::AllocaInst_O::static_className()));
llvmo::BranchInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::BranchInst_O::static_packageName(),llvmo::BranchInst_O::static_className()));
llvmo::ConstantDataArray_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ConstantDataArray_O::static_packageName(),llvmo::ConstantDataArray_O::static_className()));
llvmo::Function_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::Function_O::static_packageName(),llvmo::Function_O::static_className()));
llvmo::GlobalVariable_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::GlobalVariable_O::static_packageName(),llvmo::GlobalVariable_O::static_className()));
llvmo::IndirectBrInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::IndirectBrInst_O::static_packageName(),llvmo::IndirectBrInst_O::static_className()));
llvmo::InvokeInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::InvokeInst_O::static_packageName(),llvmo::InvokeInst_O::static_className()));
llvmo::LoadInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::LoadInst_O::static_packageName(),llvmo::LoadInst_O::static_className()));
llvmo::ResumeInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ResumeInst_O::static_packageName(),llvmo::ResumeInst_O::static_className()));
llvmo::ReturnInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::ReturnInst_O::static_packageName(),llvmo::ReturnInst_O::static_className()));
llvmo::SwitchInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::SwitchInst_O::static_packageName(),llvmo::SwitchInst_O::static_className()));
llvmo::UnreachableInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::UnreachableInst_O::static_packageName(),llvmo::UnreachableInst_O::static_className()));
llvmo::VAArgInst_O::___set_static_ClassSymbol(LOOKUP_SYMBOL(llvmo::VAArgInst_O::static_packageName(),llvmo::VAArgInst_O::static_className()));
#endif // SET_SYMBOL
#undef SET_SYMBOL
#if defined(CREATE_CLASS) || defined(ALL_STAGES)
// Depends on undefinedMetaClass
    core::MetaClass_sp undefinedMetaClass; undefinedMetaClass.reset();

    LOG(BF("Creating class[classllvmo__APFloat_Oval]"));
    core::BuiltInClass_sp classllvmo__APFloat_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__APFloat_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APFloat_Oval,_lisp,llvmo::APFloat_O::static_classSymbol());
    llvmo::APFloat_O::___staticMetaClass = classllvmo__APFloat_Oval;
    _lisp->setf_findClass(llvmo::APFloat_O::static_classSymbol(),classllvmo__APFloat_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::APFloat_O>;
        llvmo::APFloat_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::APFloat_O::static_className() % (void*)(llvmo::APFloat_O::static_newNil_callback()) );
    classllvmo__APFloat_Oval->setInstance_newNil_callback(llvmo::APFloat_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::APFloat_O> nil_for_class(new llvmo::APFloat_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::APFloat_O::static_className() );
        llvmo::APFloat_O::_nil = nil_for_class;
        classllvmo__APFloat_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__APFloat_Oval->setSupportsSlots(llvmo::APFloat_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__APInt_Oval]"));
    core::BuiltInClass_sp classllvmo__APInt_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__APInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__APInt_Oval,_lisp,llvmo::APInt_O::static_classSymbol());
    llvmo::APInt_O::___staticMetaClass = classllvmo__APInt_Oval;
    _lisp->setf_findClass(llvmo::APInt_O::static_classSymbol(),classllvmo__APInt_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::APInt_O>;
        llvmo::APInt_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::APInt_O::static_className() % (void*)(llvmo::APInt_O::static_newNil_callback()) );
    classllvmo__APInt_Oval->setInstance_newNil_callback(llvmo::APInt_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::APInt_O> nil_for_class(new llvmo::APInt_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::APInt_O::static_className() );
        llvmo::APInt_O::_nil = nil_for_class;
        classllvmo__APInt_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__APInt_Oval->setSupportsSlots(llvmo::APInt_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Attribute_Oval]"));
    core::BuiltInClass_sp classllvmo__Attribute_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Attribute_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Attribute_Oval,_lisp,llvmo::Attribute_O::static_classSymbol());
    llvmo::Attribute_O::___staticMetaClass = classllvmo__Attribute_Oval;
    _lisp->setf_findClass(llvmo::Attribute_O::static_classSymbol(),classllvmo__Attribute_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Attribute_O>;
        llvmo::Attribute_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Attribute_O::static_className() % (void*)(llvmo::Attribute_O::static_newNil_callback()) );
    classllvmo__Attribute_Oval->setInstance_newNil_callback(llvmo::Attribute_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Attribute_O> nil_for_class(new llvmo::Attribute_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Attribute_O::static_className() );
        llvmo::Attribute_O::_nil = nil_for_class;
        classllvmo__Attribute_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Attribute_Oval->setSupportsSlots(llvmo::Attribute_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Builder_Oval]"));
    core::BuiltInClass_sp classllvmo__Builder_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Builder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Builder_Oval,_lisp,llvmo::Builder_O::static_classSymbol());
    llvmo::Builder_O::___staticMetaClass = classllvmo__Builder_Oval;
    _lisp->setf_findClass(llvmo::Builder_O::static_classSymbol(),classllvmo__Builder_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Builder_O>;
        llvmo::Builder_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Builder_O::static_className() % (void*)(llvmo::Builder_O::static_newNil_callback()) );
    classllvmo__Builder_Oval->setInstance_newNil_callback(llvmo::Builder_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Builder_O> nil_for_class(new llvmo::Builder_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Builder_O::static_className() );
        llvmo::Builder_O::_nil = nil_for_class;
        classllvmo__Builder_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Builder_Oval->setSupportsSlots(llvmo::Builder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__DebugLoc_Oval]"));
    core::BuiltInClass_sp classllvmo__DebugLoc_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__DebugLoc_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DebugLoc_Oval,_lisp,llvmo::DebugLoc_O::static_classSymbol());
    llvmo::DebugLoc_O::___staticMetaClass = classllvmo__DebugLoc_Oval;
    _lisp->setf_findClass(llvmo::DebugLoc_O::static_classSymbol(),classllvmo__DebugLoc_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::DebugLoc_O>;
        llvmo::DebugLoc_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::DebugLoc_O::static_className() % (void*)(llvmo::DebugLoc_O::static_newNil_callback()) );
    classllvmo__DebugLoc_Oval->setInstance_newNil_callback(llvmo::DebugLoc_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::DebugLoc_O> nil_for_class(new llvmo::DebugLoc_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::DebugLoc_O::static_className() );
        llvmo::DebugLoc_O::_nil = nil_for_class;
        classllvmo__DebugLoc_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__DebugLoc_Oval->setSupportsSlots(llvmo::DebugLoc_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__EngineBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__EngineBuilder_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__EngineBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__EngineBuilder_Oval,_lisp,llvmo::EngineBuilder_O::static_classSymbol());
    llvmo::EngineBuilder_O::___staticMetaClass = classllvmo__EngineBuilder_Oval;
    _lisp->setf_findClass(llvmo::EngineBuilder_O::static_classSymbol(),classllvmo__EngineBuilder_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::EngineBuilder_O>;
        llvmo::EngineBuilder_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::EngineBuilder_O::static_className() % (void*)(llvmo::EngineBuilder_O::static_newNil_callback()) );
    classllvmo__EngineBuilder_Oval->setInstance_newNil_callback(llvmo::EngineBuilder_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::EngineBuilder_O> nil_for_class(new llvmo::EngineBuilder_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::EngineBuilder_O::static_className() );
        llvmo::EngineBuilder_O::_nil = nil_for_class;
        classllvmo__EngineBuilder_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__EngineBuilder_Oval->setSupportsSlots(llvmo::EngineBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ExecutionEngine_Oval]"));
    core::BuiltInClass_sp classllvmo__ExecutionEngine_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ExecutionEngine_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ExecutionEngine_Oval,_lisp,llvmo::ExecutionEngine_O::static_classSymbol());
    llvmo::ExecutionEngine_O::___staticMetaClass = classllvmo__ExecutionEngine_Oval;
    _lisp->setf_findClass(llvmo::ExecutionEngine_O::static_classSymbol(),classllvmo__ExecutionEngine_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ExecutionEngine_O>;
        llvmo::ExecutionEngine_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ExecutionEngine_O::static_className() % (void*)(llvmo::ExecutionEngine_O::static_newNil_callback()) );
    classllvmo__ExecutionEngine_Oval->setInstance_newNil_callback(llvmo::ExecutionEngine_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ExecutionEngine_O> nil_for_class(new llvmo::ExecutionEngine_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ExecutionEngine_O::static_className() );
        llvmo::ExecutionEngine_O::_nil = nil_for_class;
        classllvmo__ExecutionEngine_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ExecutionEngine_Oval->setSupportsSlots(llvmo::ExecutionEngine_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__IRBuilderBase_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilderBase_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__IRBuilderBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilderBase_Oval,_lisp,llvmo::IRBuilderBase_O::static_classSymbol());
    llvmo::IRBuilderBase_O::___staticMetaClass = classllvmo__IRBuilderBase_Oval;
    _lisp->setf_findClass(llvmo::IRBuilderBase_O::static_classSymbol(),classllvmo__IRBuilderBase_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::IRBuilderBase_O>;
        llvmo::IRBuilderBase_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::IRBuilderBase_O::static_className() % (void*)(llvmo::IRBuilderBase_O::static_newNil_callback()) );
    classllvmo__IRBuilderBase_Oval->setInstance_newNil_callback(llvmo::IRBuilderBase_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::IRBuilderBase_O> nil_for_class(new llvmo::IRBuilderBase_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilderBase_O::static_className() );
        llvmo::IRBuilderBase_O::_nil = nil_for_class;
        classllvmo__IRBuilderBase_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__IRBuilderBase_Oval->setSupportsSlots(llvmo::IRBuilderBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__InsertPoint_Oval]"));
    core::BuiltInClass_sp classllvmo__InsertPoint_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__InsertPoint_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InsertPoint_Oval,_lisp,llvmo::InsertPoint_O::static_classSymbol());
    llvmo::InsertPoint_O::___staticMetaClass = classllvmo__InsertPoint_Oval;
    _lisp->setf_findClass(llvmo::InsertPoint_O::static_classSymbol(),classllvmo__InsertPoint_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::InsertPoint_O>;
        llvmo::InsertPoint_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::InsertPoint_O::static_className() % (void*)(llvmo::InsertPoint_O::static_newNil_callback()) );
    classllvmo__InsertPoint_Oval->setInstance_newNil_callback(llvmo::InsertPoint_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::InsertPoint_O> nil_for_class(new llvmo::InsertPoint_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::InsertPoint_O::static_className() );
        llvmo::InsertPoint_O::_nil = nil_for_class;
        classllvmo__InsertPoint_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__InsertPoint_Oval->setSupportsSlots(llvmo::InsertPoint_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__LLVMContext_Oval]"));
    core::BuiltInClass_sp classllvmo__LLVMContext_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__LLVMContext_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LLVMContext_Oval,_lisp,llvmo::LLVMContext_O::static_classSymbol());
    llvmo::LLVMContext_O::___staticMetaClass = classllvmo__LLVMContext_Oval;
    _lisp->setf_findClass(llvmo::LLVMContext_O::static_classSymbol(),classllvmo__LLVMContext_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::LLVMContext_O>;
        llvmo::LLVMContext_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::LLVMContext_O::static_className() % (void*)(llvmo::LLVMContext_O::static_newNil_callback()) );
    classllvmo__LLVMContext_Oval->setInstance_newNil_callback(llvmo::LLVMContext_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::LLVMContext_O> nil_for_class(new llvmo::LLVMContext_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::LLVMContext_O::static_className() );
        llvmo::LLVMContext_O::_nil = nil_for_class;
        classllvmo__LLVMContext_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__LLVMContext_Oval->setSupportsSlots(llvmo::LLVMContext_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Module_Oval]"));
    core::BuiltInClass_sp classllvmo__Module_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Module_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Module_Oval,_lisp,llvmo::Module_O::static_classSymbol());
    llvmo::Module_O::___staticMetaClass = classllvmo__Module_Oval;
    _lisp->setf_findClass(llvmo::Module_O::static_classSymbol(),classllvmo__Module_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Module_O>;
        llvmo::Module_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Module_O::static_className() % (void*)(llvmo::Module_O::static_newNil_callback()) );
    classllvmo__Module_Oval->setInstance_newNil_callback(llvmo::Module_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Module_O> nil_for_class(new llvmo::Module_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Module_O::static_className() );
        llvmo::Module_O::_nil = nil_for_class;
        classllvmo__Module_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Module_Oval->setSupportsSlots(llvmo::Module_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__PassManagerBase_Oval]"));
    core::BuiltInClass_sp classllvmo__PassManagerBase_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__PassManagerBase_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PassManagerBase_Oval,_lisp,llvmo::PassManagerBase_O::static_classSymbol());
    llvmo::PassManagerBase_O::___staticMetaClass = classllvmo__PassManagerBase_Oval;
    _lisp->setf_findClass(llvmo::PassManagerBase_O::static_classSymbol(),classllvmo__PassManagerBase_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::PassManagerBase_O>;
        llvmo::PassManagerBase_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::PassManagerBase_O::static_className() % (void*)(llvmo::PassManagerBase_O::static_newNil_callback()) );
    classllvmo__PassManagerBase_Oval->setInstance_newNil_callback(llvmo::PassManagerBase_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::PassManagerBase_O> nil_for_class(new llvmo::PassManagerBase_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::PassManagerBase_O::static_className() );
        llvmo::PassManagerBase_O::_nil = nil_for_class;
        classllvmo__PassManagerBase_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__PassManagerBase_Oval->setSupportsSlots(llvmo::PassManagerBase_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Pass_Oval]"));
    core::BuiltInClass_sp classllvmo__Pass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Pass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Pass_Oval,_lisp,llvmo::Pass_O::static_classSymbol());
    llvmo::Pass_O::___staticMetaClass = classllvmo__Pass_Oval;
    _lisp->setf_findClass(llvmo::Pass_O::static_classSymbol(),classllvmo__Pass_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Pass_O>;
        llvmo::Pass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Pass_O::static_className() % (void*)(llvmo::Pass_O::static_newNil_callback()) );
    classllvmo__Pass_Oval->setInstance_newNil_callback(llvmo::Pass_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Pass_O> nil_for_class(new llvmo::Pass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Pass_O::static_className() );
        llvmo::Pass_O::_nil = nil_for_class;
        classllvmo__Pass_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Pass_Oval->setSupportsSlots(llvmo::Pass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Type_Oval]"));
    core::BuiltInClass_sp classllvmo__Type_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Type_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Type_Oval,_lisp,llvmo::Type_O::static_classSymbol());
    llvmo::Type_O::___staticMetaClass = classllvmo__Type_Oval;
    _lisp->setf_findClass(llvmo::Type_O::static_classSymbol(),classllvmo__Type_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Type_O>;
        llvmo::Type_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Type_O::static_className() % (void*)(llvmo::Type_O::static_newNil_callback()) );
    classllvmo__Type_Oval->setInstance_newNil_callback(llvmo::Type_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Type_O> nil_for_class(new llvmo::Type_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Type_O::static_className() );
        llvmo::Type_O::_nil = nil_for_class;
        classllvmo__Type_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Type_Oval->setSupportsSlots(llvmo::Type_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Value_Oval]"));
    core::BuiltInClass_sp classllvmo__Value_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Value_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Value_Oval,_lisp,llvmo::Value_O::static_classSymbol());
    llvmo::Value_O::___staticMetaClass = classllvmo__Value_Oval;
    _lisp->setf_findClass(llvmo::Value_O::static_classSymbol(),classllvmo__Value_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Value_O>;
        llvmo::Value_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Value_O::static_className() % (void*)(llvmo::Value_O::static_newNil_callback()) );
    classllvmo__Value_Oval->setInstance_newNil_callback(llvmo::Value_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Value_O> nil_for_class(new llvmo::Value_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Value_O::static_className() );
        llvmo::Value_O::_nil = nil_for_class;
        classllvmo__Value_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Value_Oval->setSupportsSlots(llvmo::Value_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Argument_Oval]"));
    core::BuiltInClass_sp classllvmo__Argument_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Argument_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Argument_Oval,_lisp,llvmo::Argument_O::static_classSymbol());
    llvmo::Argument_O::___staticMetaClass = classllvmo__Argument_Oval;
    _lisp->setf_findClass(llvmo::Argument_O::static_classSymbol(),classllvmo__Argument_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Argument_O>;
        llvmo::Argument_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Argument_O::static_className() % (void*)(llvmo::Argument_O::static_newNil_callback()) );
    classllvmo__Argument_Oval->setInstance_newNil_callback(llvmo::Argument_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Argument_O> nil_for_class(new llvmo::Argument_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Argument_O::static_className() );
        llvmo::Argument_O::_nil = nil_for_class;
        classllvmo__Argument_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Argument_Oval->setSupportsSlots(llvmo::Argument_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__BasicBlock_Oval]"));
    core::BuiltInClass_sp classllvmo__BasicBlock_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__BasicBlock_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BasicBlock_Oval,_lisp,llvmo::BasicBlock_O::static_classSymbol());
    llvmo::BasicBlock_O::___staticMetaClass = classllvmo__BasicBlock_Oval;
    _lisp->setf_findClass(llvmo::BasicBlock_O::static_classSymbol(),classllvmo__BasicBlock_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::BasicBlock_O>;
        llvmo::BasicBlock_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::BasicBlock_O::static_className() % (void*)(llvmo::BasicBlock_O::static_newNil_callback()) );
    classllvmo__BasicBlock_Oval->setInstance_newNil_callback(llvmo::BasicBlock_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::BasicBlock_O> nil_for_class(new llvmo::BasicBlock_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::BasicBlock_O::static_className() );
        llvmo::BasicBlock_O::_nil = nil_for_class;
        classllvmo__BasicBlock_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__BasicBlock_Oval->setSupportsSlots(llvmo::BasicBlock_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__CompositeType_Oval]"));
    core::BuiltInClass_sp classllvmo__CompositeType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__CompositeType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CompositeType_Oval,_lisp,llvmo::CompositeType_O::static_classSymbol());
    llvmo::CompositeType_O::___staticMetaClass = classllvmo__CompositeType_Oval;
    _lisp->setf_findClass(llvmo::CompositeType_O::static_classSymbol(),classllvmo__CompositeType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::CompositeType_O>;
        llvmo::CompositeType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::CompositeType_O::static_className() % (void*)(llvmo::CompositeType_O::static_newNil_callback()) );
    classllvmo__CompositeType_Oval->setInstance_newNil_callback(llvmo::CompositeType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::CompositeType_O> nil_for_class(new llvmo::CompositeType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::CompositeType_O::static_className() );
        llvmo::CompositeType_O::_nil = nil_for_class;
        classllvmo__CompositeType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__CompositeType_Oval->setSupportsSlots(llvmo::CompositeType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__FunctionPassManager_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPassManager_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__FunctionPassManager_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPassManager_Oval,_lisp,llvmo::FunctionPassManager_O::static_classSymbol());
    llvmo::FunctionPassManager_O::___staticMetaClass = classllvmo__FunctionPassManager_Oval;
    _lisp->setf_findClass(llvmo::FunctionPassManager_O::static_classSymbol(),classllvmo__FunctionPassManager_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::FunctionPassManager_O>;
        llvmo::FunctionPassManager_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::FunctionPassManager_O::static_className() % (void*)(llvmo::FunctionPassManager_O::static_newNil_callback()) );
    classllvmo__FunctionPassManager_Oval->setInstance_newNil_callback(llvmo::FunctionPassManager_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::FunctionPassManager_O> nil_for_class(new llvmo::FunctionPassManager_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPassManager_O::static_className() );
        llvmo::FunctionPassManager_O::_nil = nil_for_class;
        classllvmo__FunctionPassManager_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__FunctionPassManager_Oval->setSupportsSlots(llvmo::FunctionPassManager_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__FunctionPass_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionPass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__FunctionPass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionPass_Oval,_lisp,llvmo::FunctionPass_O::static_classSymbol());
    llvmo::FunctionPass_O::___staticMetaClass = classllvmo__FunctionPass_Oval;
    _lisp->setf_findClass(llvmo::FunctionPass_O::static_classSymbol(),classllvmo__FunctionPass_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::FunctionPass_O>;
        llvmo::FunctionPass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::FunctionPass_O::static_className() % (void*)(llvmo::FunctionPass_O::static_newNil_callback()) );
    classllvmo__FunctionPass_Oval->setInstance_newNil_callback(llvmo::FunctionPass_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::FunctionPass_O> nil_for_class(new llvmo::FunctionPass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionPass_O::static_className() );
        llvmo::FunctionPass_O::_nil = nil_for_class;
        classllvmo__FunctionPass_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__FunctionPass_Oval->setSupportsSlots(llvmo::FunctionPass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__FunctionType_Oval]"));
    core::BuiltInClass_sp classllvmo__FunctionType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__FunctionType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FunctionType_Oval,_lisp,llvmo::FunctionType_O::static_classSymbol());
    llvmo::FunctionType_O::___staticMetaClass = classllvmo__FunctionType_Oval;
    _lisp->setf_findClass(llvmo::FunctionType_O::static_classSymbol(),classllvmo__FunctionType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::FunctionType_O>;
        llvmo::FunctionType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::FunctionType_O::static_className() % (void*)(llvmo::FunctionType_O::static_newNil_callback()) );
    classllvmo__FunctionType_Oval->setInstance_newNil_callback(llvmo::FunctionType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::FunctionType_O> nil_for_class(new llvmo::FunctionType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::FunctionType_O::static_className() );
        llvmo::FunctionType_O::_nil = nil_for_class;
        classllvmo__FunctionType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__FunctionType_Oval->setSupportsSlots(llvmo::FunctionType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__IRBuilder_Oval]"));
    core::BuiltInClass_sp classllvmo__IRBuilder_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__IRBuilder_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IRBuilder_Oval,_lisp,llvmo::IRBuilder_O::static_classSymbol());
    llvmo::IRBuilder_O::___staticMetaClass = classllvmo__IRBuilder_Oval;
    _lisp->setf_findClass(llvmo::IRBuilder_O::static_classSymbol(),classllvmo__IRBuilder_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::IRBuilder_O>;
        llvmo::IRBuilder_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::IRBuilder_O::static_className() % (void*)(llvmo::IRBuilder_O::static_newNil_callback()) );
    classllvmo__IRBuilder_Oval->setInstance_newNil_callback(llvmo::IRBuilder_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::IRBuilder_O> nil_for_class(new llvmo::IRBuilder_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::IRBuilder_O::static_className() );
        llvmo::IRBuilder_O::_nil = nil_for_class;
        classllvmo__IRBuilder_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__IRBuilder_Oval->setSupportsSlots(llvmo::IRBuilder_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__IntegerType_Oval]"));
    core::BuiltInClass_sp classllvmo__IntegerType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__IntegerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IntegerType_Oval,_lisp,llvmo::IntegerType_O::static_classSymbol());
    llvmo::IntegerType_O::___staticMetaClass = classllvmo__IntegerType_Oval;
    _lisp->setf_findClass(llvmo::IntegerType_O::static_classSymbol(),classllvmo__IntegerType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::IntegerType_O>;
        llvmo::IntegerType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::IntegerType_O::static_className() % (void*)(llvmo::IntegerType_O::static_newNil_callback()) );
    classllvmo__IntegerType_Oval->setInstance_newNil_callback(llvmo::IntegerType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::IntegerType_O> nil_for_class(new llvmo::IntegerType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::IntegerType_O::static_className() );
        llvmo::IntegerType_O::_nil = nil_for_class;
        classllvmo__IntegerType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__IntegerType_Oval->setSupportsSlots(llvmo::IntegerType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__MDNode_Oval]"));
    core::BuiltInClass_sp classllvmo__MDNode_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__MDNode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDNode_Oval,_lisp,llvmo::MDNode_O::static_classSymbol());
    llvmo::MDNode_O::___staticMetaClass = classllvmo__MDNode_Oval;
    _lisp->setf_findClass(llvmo::MDNode_O::static_classSymbol(),classllvmo__MDNode_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::MDNode_O>;
        llvmo::MDNode_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::MDNode_O::static_className() % (void*)(llvmo::MDNode_O::static_newNil_callback()) );
    classllvmo__MDNode_Oval->setInstance_newNil_callback(llvmo::MDNode_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::MDNode_O> nil_for_class(new llvmo::MDNode_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::MDNode_O::static_className() );
        llvmo::MDNode_O::_nil = nil_for_class;
        classllvmo__MDNode_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__MDNode_Oval->setSupportsSlots(llvmo::MDNode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__MDString_Oval]"));
    core::BuiltInClass_sp classllvmo__MDString_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__MDString_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__MDString_Oval,_lisp,llvmo::MDString_O::static_classSymbol());
    llvmo::MDString_O::___staticMetaClass = classllvmo__MDString_Oval;
    _lisp->setf_findClass(llvmo::MDString_O::static_classSymbol(),classllvmo__MDString_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::MDString_O>;
        llvmo::MDString_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::MDString_O::static_className() % (void*)(llvmo::MDString_O::static_newNil_callback()) );
    classllvmo__MDString_Oval->setInstance_newNil_callback(llvmo::MDString_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::MDString_O> nil_for_class(new llvmo::MDString_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::MDString_O::static_className() );
        llvmo::MDString_O::_nil = nil_for_class;
        classllvmo__MDString_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__MDString_Oval->setSupportsSlots(llvmo::MDString_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ModulePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ModulePass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ModulePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ModulePass_Oval,_lisp,llvmo::ModulePass_O::static_classSymbol());
    llvmo::ModulePass_O::___staticMetaClass = classllvmo__ModulePass_Oval;
    _lisp->setf_findClass(llvmo::ModulePass_O::static_classSymbol(),classllvmo__ModulePass_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ModulePass_O>;
        llvmo::ModulePass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ModulePass_O::static_className() % (void*)(llvmo::ModulePass_O::static_newNil_callback()) );
    classllvmo__ModulePass_Oval->setInstance_newNil_callback(llvmo::ModulePass_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ModulePass_O> nil_for_class(new llvmo::ModulePass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ModulePass_O::static_className() );
        llvmo::ModulePass_O::_nil = nil_for_class;
        classllvmo__ModulePass_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ModulePass_Oval->setSupportsSlots(llvmo::ModulePass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__User_Oval]"));
    core::BuiltInClass_sp classllvmo__User_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__User_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__User_Oval,_lisp,llvmo::User_O::static_classSymbol());
    llvmo::User_O::___staticMetaClass = classllvmo__User_Oval;
    _lisp->setf_findClass(llvmo::User_O::static_classSymbol(),classllvmo__User_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::User_O>;
        llvmo::User_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::User_O::static_className() % (void*)(llvmo::User_O::static_newNil_callback()) );
    classllvmo__User_Oval->setInstance_newNil_callback(llvmo::User_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::User_O> nil_for_class(new llvmo::User_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::User_O::static_className() );
        llvmo::User_O::_nil = nil_for_class;
        classllvmo__User_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__User_Oval->setSupportsSlots(llvmo::User_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Constant_Oval]"));
    core::BuiltInClass_sp classllvmo__Constant_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Constant_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Constant_Oval,_lisp,llvmo::Constant_O::static_classSymbol());
    llvmo::Constant_O::___staticMetaClass = classllvmo__Constant_Oval;
    _lisp->setf_findClass(llvmo::Constant_O::static_classSymbol(),classllvmo__Constant_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Constant_O>;
        llvmo::Constant_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Constant_O::static_className() % (void*)(llvmo::Constant_O::static_newNil_callback()) );
    classllvmo__Constant_Oval->setInstance_newNil_callback(llvmo::Constant_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Constant_O> nil_for_class(new llvmo::Constant_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Constant_O::static_className() );
        llvmo::Constant_O::_nil = nil_for_class;
        classllvmo__Constant_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Constant_Oval->setSupportsSlots(llvmo::Constant_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ImmutablePass_Oval]"));
    core::BuiltInClass_sp classllvmo__ImmutablePass_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ImmutablePass_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ImmutablePass_Oval,_lisp,llvmo::ImmutablePass_O::static_classSymbol());
    llvmo::ImmutablePass_O::___staticMetaClass = classllvmo__ImmutablePass_Oval;
    _lisp->setf_findClass(llvmo::ImmutablePass_O::static_classSymbol(),classllvmo__ImmutablePass_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ImmutablePass_O>;
        llvmo::ImmutablePass_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ImmutablePass_O::static_className() % (void*)(llvmo::ImmutablePass_O::static_newNil_callback()) );
    classllvmo__ImmutablePass_Oval->setInstance_newNil_callback(llvmo::ImmutablePass_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ImmutablePass_O> nil_for_class(new llvmo::ImmutablePass_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ImmutablePass_O::static_className() );
        llvmo::ImmutablePass_O::_nil = nil_for_class;
        classllvmo__ImmutablePass_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ImmutablePass_Oval->setSupportsSlots(llvmo::ImmutablePass_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Instruction_Oval]"));
    core::BuiltInClass_sp classllvmo__Instruction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Instruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Instruction_Oval,_lisp,llvmo::Instruction_O::static_classSymbol());
    llvmo::Instruction_O::___staticMetaClass = classllvmo__Instruction_Oval;
    _lisp->setf_findClass(llvmo::Instruction_O::static_classSymbol(),classllvmo__Instruction_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Instruction_O>;
        llvmo::Instruction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Instruction_O::static_className() % (void*)(llvmo::Instruction_O::static_newNil_callback()) );
    classllvmo__Instruction_Oval->setInstance_newNil_callback(llvmo::Instruction_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Instruction_O> nil_for_class(new llvmo::Instruction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Instruction_O::static_className() );
        llvmo::Instruction_O::_nil = nil_for_class;
        classllvmo__Instruction_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Instruction_Oval->setSupportsSlots(llvmo::Instruction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__SequentialType_Oval]"));
    core::BuiltInClass_sp classllvmo__SequentialType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__SequentialType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SequentialType_Oval,_lisp,llvmo::SequentialType_O::static_classSymbol());
    llvmo::SequentialType_O::___staticMetaClass = classllvmo__SequentialType_Oval;
    _lisp->setf_findClass(llvmo::SequentialType_O::static_classSymbol(),classllvmo__SequentialType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::SequentialType_O>;
        llvmo::SequentialType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::SequentialType_O::static_className() % (void*)(llvmo::SequentialType_O::static_newNil_callback()) );
    classllvmo__SequentialType_Oval->setInstance_newNil_callback(llvmo::SequentialType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::SequentialType_O> nil_for_class(new llvmo::SequentialType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::SequentialType_O::static_className() );
        llvmo::SequentialType_O::_nil = nil_for_class;
        classllvmo__SequentialType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__SequentialType_Oval->setSupportsSlots(llvmo::SequentialType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__StructType_Oval]"));
    core::BuiltInClass_sp classllvmo__StructType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__StructType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StructType_Oval,_lisp,llvmo::StructType_O::static_classSymbol());
    llvmo::StructType_O::___staticMetaClass = classllvmo__StructType_Oval;
    _lisp->setf_findClass(llvmo::StructType_O::static_classSymbol(),classllvmo__StructType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::StructType_O>;
        llvmo::StructType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::StructType_O::static_className() % (void*)(llvmo::StructType_O::static_newNil_callback()) );
    classllvmo__StructType_Oval->setInstance_newNil_callback(llvmo::StructType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::StructType_O> nil_for_class(new llvmo::StructType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::StructType_O::static_className() );
        llvmo::StructType_O::_nil = nil_for_class;
        classllvmo__StructType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__StructType_Oval->setSupportsSlots(llvmo::StructType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ArrayType_Oval]"));
    core::BuiltInClass_sp classllvmo__ArrayType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ArrayType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ArrayType_Oval,_lisp,llvmo::ArrayType_O::static_classSymbol());
    llvmo::ArrayType_O::___staticMetaClass = classllvmo__ArrayType_Oval;
    _lisp->setf_findClass(llvmo::ArrayType_O::static_classSymbol(),classllvmo__ArrayType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ArrayType_O>;
        llvmo::ArrayType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ArrayType_O::static_className() % (void*)(llvmo::ArrayType_O::static_newNil_callback()) );
    classllvmo__ArrayType_Oval->setInstance_newNil_callback(llvmo::ArrayType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ArrayType_O> nil_for_class(new llvmo::ArrayType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ArrayType_O::static_className() );
        llvmo::ArrayType_O::_nil = nil_for_class;
        classllvmo__ArrayType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ArrayType_Oval->setSupportsSlots(llvmo::ArrayType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__AtomicCmpXchgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicCmpXchgInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__AtomicCmpXchgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicCmpXchgInst_Oval,_lisp,llvmo::AtomicCmpXchgInst_O::static_classSymbol());
    llvmo::AtomicCmpXchgInst_O::___staticMetaClass = classllvmo__AtomicCmpXchgInst_Oval;
    _lisp->setf_findClass(llvmo::AtomicCmpXchgInst_O::static_classSymbol(),classllvmo__AtomicCmpXchgInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::AtomicCmpXchgInst_O>;
        llvmo::AtomicCmpXchgInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::AtomicCmpXchgInst_O::static_className() % (void*)(llvmo::AtomicCmpXchgInst_O::static_newNil_callback()) );
    classllvmo__AtomicCmpXchgInst_Oval->setInstance_newNil_callback(llvmo::AtomicCmpXchgInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::AtomicCmpXchgInst_O> nil_for_class(new llvmo::AtomicCmpXchgInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicCmpXchgInst_O::static_className() );
        llvmo::AtomicCmpXchgInst_O::_nil = nil_for_class;
        classllvmo__AtomicCmpXchgInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__AtomicCmpXchgInst_Oval->setSupportsSlots(llvmo::AtomicCmpXchgInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__AtomicRMWInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AtomicRMWInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__AtomicRMWInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AtomicRMWInst_Oval,_lisp,llvmo::AtomicRMWInst_O::static_classSymbol());
    llvmo::AtomicRMWInst_O::___staticMetaClass = classllvmo__AtomicRMWInst_Oval;
    _lisp->setf_findClass(llvmo::AtomicRMWInst_O::static_classSymbol(),classllvmo__AtomicRMWInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::AtomicRMWInst_O>;
        llvmo::AtomicRMWInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::AtomicRMWInst_O::static_className() % (void*)(llvmo::AtomicRMWInst_O::static_newNil_callback()) );
    classllvmo__AtomicRMWInst_Oval->setInstance_newNil_callback(llvmo::AtomicRMWInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::AtomicRMWInst_O> nil_for_class(new llvmo::AtomicRMWInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::AtomicRMWInst_O::static_className() );
        llvmo::AtomicRMWInst_O::_nil = nil_for_class;
        classllvmo__AtomicRMWInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__AtomicRMWInst_Oval->setSupportsSlots(llvmo::AtomicRMWInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__CallInst_Oval]"));
    core::BuiltInClass_sp classllvmo__CallInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__CallInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__CallInst_Oval,_lisp,llvmo::CallInst_O::static_classSymbol());
    llvmo::CallInst_O::___staticMetaClass = classllvmo__CallInst_Oval;
    _lisp->setf_findClass(llvmo::CallInst_O::static_classSymbol(),classllvmo__CallInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::CallInst_O>;
        llvmo::CallInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::CallInst_O::static_className() % (void*)(llvmo::CallInst_O::static_newNil_callback()) );
    classllvmo__CallInst_Oval->setInstance_newNil_callback(llvmo::CallInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::CallInst_O> nil_for_class(new llvmo::CallInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::CallInst_O::static_className() );
        llvmo::CallInst_O::_nil = nil_for_class;
        classllvmo__CallInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__CallInst_Oval->setSupportsSlots(llvmo::CallInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantArray_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantArray_Oval,_lisp,llvmo::ConstantArray_O::static_classSymbol());
    llvmo::ConstantArray_O::___staticMetaClass = classllvmo__ConstantArray_Oval;
    _lisp->setf_findClass(llvmo::ConstantArray_O::static_classSymbol(),classllvmo__ConstantArray_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantArray_O>;
        llvmo::ConstantArray_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantArray_O::static_className() % (void*)(llvmo::ConstantArray_O::static_newNil_callback()) );
    classllvmo__ConstantArray_Oval->setInstance_newNil_callback(llvmo::ConstantArray_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantArray_O> nil_for_class(new llvmo::ConstantArray_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantArray_O::static_className() );
        llvmo::ConstantArray_O::_nil = nil_for_class;
        classllvmo__ConstantArray_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantArray_Oval->setSupportsSlots(llvmo::ConstantArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataSequential_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataSequential_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantDataSequential_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataSequential_Oval,_lisp,llvmo::ConstantDataSequential_O::static_classSymbol());
    llvmo::ConstantDataSequential_O::___staticMetaClass = classllvmo__ConstantDataSequential_Oval;
    _lisp->setf_findClass(llvmo::ConstantDataSequential_O::static_classSymbol(),classllvmo__ConstantDataSequential_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantDataSequential_O>;
        llvmo::ConstantDataSequential_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantDataSequential_O::static_className() % (void*)(llvmo::ConstantDataSequential_O::static_newNil_callback()) );
    classllvmo__ConstantDataSequential_Oval->setInstance_newNil_callback(llvmo::ConstantDataSequential_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantDataSequential_O> nil_for_class(new llvmo::ConstantDataSequential_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataSequential_O::static_className() );
        llvmo::ConstantDataSequential_O::_nil = nil_for_class;
        classllvmo__ConstantDataSequential_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantDataSequential_Oval->setSupportsSlots(llvmo::ConstantDataSequential_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantExpr_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantExpr_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantExpr_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantExpr_Oval,_lisp,llvmo::ConstantExpr_O::static_classSymbol());
    llvmo::ConstantExpr_O::___staticMetaClass = classllvmo__ConstantExpr_Oval;
    _lisp->setf_findClass(llvmo::ConstantExpr_O::static_classSymbol(),classllvmo__ConstantExpr_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantExpr_O>;
        llvmo::ConstantExpr_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantExpr_O::static_className() % (void*)(llvmo::ConstantExpr_O::static_newNil_callback()) );
    classllvmo__ConstantExpr_Oval->setInstance_newNil_callback(llvmo::ConstantExpr_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantExpr_O> nil_for_class(new llvmo::ConstantExpr_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantExpr_O::static_className() );
        llvmo::ConstantExpr_O::_nil = nil_for_class;
        classllvmo__ConstantExpr_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantExpr_Oval->setSupportsSlots(llvmo::ConstantExpr_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantFP_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantFP_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantFP_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantFP_Oval,_lisp,llvmo::ConstantFP_O::static_classSymbol());
    llvmo::ConstantFP_O::___staticMetaClass = classllvmo__ConstantFP_Oval;
    _lisp->setf_findClass(llvmo::ConstantFP_O::static_classSymbol(),classllvmo__ConstantFP_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantFP_O>;
        llvmo::ConstantFP_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantFP_O::static_className() % (void*)(llvmo::ConstantFP_O::static_newNil_callback()) );
    classllvmo__ConstantFP_Oval->setInstance_newNil_callback(llvmo::ConstantFP_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantFP_O> nil_for_class(new llvmo::ConstantFP_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantFP_O::static_className() );
        llvmo::ConstantFP_O::_nil = nil_for_class;
        classllvmo__ConstantFP_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantFP_Oval->setSupportsSlots(llvmo::ConstantFP_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantInt_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantInt_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantInt_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantInt_Oval,_lisp,llvmo::ConstantInt_O::static_classSymbol());
    llvmo::ConstantInt_O::___staticMetaClass = classllvmo__ConstantInt_Oval;
    _lisp->setf_findClass(llvmo::ConstantInt_O::static_classSymbol(),classllvmo__ConstantInt_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantInt_O>;
        llvmo::ConstantInt_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantInt_O::static_className() % (void*)(llvmo::ConstantInt_O::static_newNil_callback()) );
    classllvmo__ConstantInt_Oval->setInstance_newNil_callback(llvmo::ConstantInt_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantInt_O> nil_for_class(new llvmo::ConstantInt_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantInt_O::static_className() );
        llvmo::ConstantInt_O::_nil = nil_for_class;
        classllvmo__ConstantInt_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantInt_Oval->setSupportsSlots(llvmo::ConstantInt_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantPointerNull_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantPointerNull_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantPointerNull_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantPointerNull_Oval,_lisp,llvmo::ConstantPointerNull_O::static_classSymbol());
    llvmo::ConstantPointerNull_O::___staticMetaClass = classllvmo__ConstantPointerNull_Oval;
    _lisp->setf_findClass(llvmo::ConstantPointerNull_O::static_classSymbol(),classllvmo__ConstantPointerNull_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantPointerNull_O>;
        llvmo::ConstantPointerNull_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantPointerNull_O::static_className() % (void*)(llvmo::ConstantPointerNull_O::static_newNil_callback()) );
    classllvmo__ConstantPointerNull_Oval->setInstance_newNil_callback(llvmo::ConstantPointerNull_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantPointerNull_O> nil_for_class(new llvmo::ConstantPointerNull_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantPointerNull_O::static_className() );
        llvmo::ConstantPointerNull_O::_nil = nil_for_class;
        classllvmo__ConstantPointerNull_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantPointerNull_Oval->setSupportsSlots(llvmo::ConstantPointerNull_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__DataLayout_Oval]"));
    core::BuiltInClass_sp classllvmo__DataLayout_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__DataLayout_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__DataLayout_Oval,_lisp,llvmo::DataLayout_O::static_classSymbol());
    llvmo::DataLayout_O::___staticMetaClass = classllvmo__DataLayout_Oval;
    _lisp->setf_findClass(llvmo::DataLayout_O::static_classSymbol(),classllvmo__DataLayout_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::DataLayout_O>;
        llvmo::DataLayout_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::DataLayout_O::static_className() % (void*)(llvmo::DataLayout_O::static_newNil_callback()) );
    classllvmo__DataLayout_Oval->setInstance_newNil_callback(llvmo::DataLayout_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::DataLayout_O> nil_for_class(new llvmo::DataLayout_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::DataLayout_O::static_className() );
        llvmo::DataLayout_O::_nil = nil_for_class;
        classllvmo__DataLayout_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__DataLayout_Oval->setSupportsSlots(llvmo::DataLayout_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__FenceInst_Oval]"));
    core::BuiltInClass_sp classllvmo__FenceInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__FenceInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__FenceInst_Oval,_lisp,llvmo::FenceInst_O::static_classSymbol());
    llvmo::FenceInst_O::___staticMetaClass = classllvmo__FenceInst_Oval;
    _lisp->setf_findClass(llvmo::FenceInst_O::static_classSymbol(),classllvmo__FenceInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::FenceInst_O>;
        llvmo::FenceInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::FenceInst_O::static_className() % (void*)(llvmo::FenceInst_O::static_newNil_callback()) );
    classllvmo__FenceInst_Oval->setInstance_newNil_callback(llvmo::FenceInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::FenceInst_O> nil_for_class(new llvmo::FenceInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::FenceInst_O::static_className() );
        llvmo::FenceInst_O::_nil = nil_for_class;
        classllvmo__FenceInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__FenceInst_Oval->setSupportsSlots(llvmo::FenceInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__GlobalValue_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalValue_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__GlobalValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalValue_Oval,_lisp,llvmo::GlobalValue_O::static_classSymbol());
    llvmo::GlobalValue_O::___staticMetaClass = classllvmo__GlobalValue_Oval;
    _lisp->setf_findClass(llvmo::GlobalValue_O::static_classSymbol(),classllvmo__GlobalValue_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::GlobalValue_O>;
        llvmo::GlobalValue_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::GlobalValue_O::static_className() % (void*)(llvmo::GlobalValue_O::static_newNil_callback()) );
    classllvmo__GlobalValue_Oval->setInstance_newNil_callback(llvmo::GlobalValue_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::GlobalValue_O> nil_for_class(new llvmo::GlobalValue_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalValue_O::static_className() );
        llvmo::GlobalValue_O::_nil = nil_for_class;
        classllvmo__GlobalValue_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__GlobalValue_Oval->setSupportsSlots(llvmo::GlobalValue_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__LandingPadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LandingPadInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__LandingPadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LandingPadInst_Oval,_lisp,llvmo::LandingPadInst_O::static_classSymbol());
    llvmo::LandingPadInst_O::___staticMetaClass = classllvmo__LandingPadInst_Oval;
    _lisp->setf_findClass(llvmo::LandingPadInst_O::static_classSymbol(),classllvmo__LandingPadInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::LandingPadInst_O>;
        llvmo::LandingPadInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::LandingPadInst_O::static_className() % (void*)(llvmo::LandingPadInst_O::static_newNil_callback()) );
    classllvmo__LandingPadInst_Oval->setInstance_newNil_callback(llvmo::LandingPadInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::LandingPadInst_O> nil_for_class(new llvmo::LandingPadInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::LandingPadInst_O::static_className() );
        llvmo::LandingPadInst_O::_nil = nil_for_class;
        classllvmo__LandingPadInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__LandingPadInst_Oval->setSupportsSlots(llvmo::LandingPadInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__PHINode_Oval]"));
    core::BuiltInClass_sp classllvmo__PHINode_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__PHINode_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PHINode_Oval,_lisp,llvmo::PHINode_O::static_classSymbol());
    llvmo::PHINode_O::___staticMetaClass = classllvmo__PHINode_Oval;
    _lisp->setf_findClass(llvmo::PHINode_O::static_classSymbol(),classllvmo__PHINode_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::PHINode_O>;
        llvmo::PHINode_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::PHINode_O::static_className() % (void*)(llvmo::PHINode_O::static_newNil_callback()) );
    classllvmo__PHINode_Oval->setInstance_newNil_callback(llvmo::PHINode_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::PHINode_O> nil_for_class(new llvmo::PHINode_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::PHINode_O::static_className() );
        llvmo::PHINode_O::_nil = nil_for_class;
        classllvmo__PHINode_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__PHINode_Oval->setSupportsSlots(llvmo::PHINode_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__PointerType_Oval]"));
    core::BuiltInClass_sp classllvmo__PointerType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__PointerType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__PointerType_Oval,_lisp,llvmo::PointerType_O::static_classSymbol());
    llvmo::PointerType_O::___staticMetaClass = classllvmo__PointerType_Oval;
    _lisp->setf_findClass(llvmo::PointerType_O::static_classSymbol(),classllvmo__PointerType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::PointerType_O>;
        llvmo::PointerType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::PointerType_O::static_className() % (void*)(llvmo::PointerType_O::static_newNil_callback()) );
    classllvmo__PointerType_Oval->setInstance_newNil_callback(llvmo::PointerType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::PointerType_O> nil_for_class(new llvmo::PointerType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::PointerType_O::static_className() );
        llvmo::PointerType_O::_nil = nil_for_class;
        classllvmo__PointerType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__PointerType_Oval->setSupportsSlots(llvmo::PointerType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__StoreInst_Oval]"));
    core::BuiltInClass_sp classllvmo__StoreInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__StoreInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__StoreInst_Oval,_lisp,llvmo::StoreInst_O::static_classSymbol());
    llvmo::StoreInst_O::___staticMetaClass = classllvmo__StoreInst_Oval;
    _lisp->setf_findClass(llvmo::StoreInst_O::static_classSymbol(),classllvmo__StoreInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::StoreInst_O>;
        llvmo::StoreInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::StoreInst_O::static_className() % (void*)(llvmo::StoreInst_O::static_newNil_callback()) );
    classllvmo__StoreInst_Oval->setInstance_newNil_callback(llvmo::StoreInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::StoreInst_O> nil_for_class(new llvmo::StoreInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::StoreInst_O::static_className() );
        llvmo::StoreInst_O::_nil = nil_for_class;
        classllvmo__StoreInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__StoreInst_Oval->setSupportsSlots(llvmo::StoreInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__TerminatorInst_Oval]"));
    core::BuiltInClass_sp classllvmo__TerminatorInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__TerminatorInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__TerminatorInst_Oval,_lisp,llvmo::TerminatorInst_O::static_classSymbol());
    llvmo::TerminatorInst_O::___staticMetaClass = classllvmo__TerminatorInst_Oval;
    _lisp->setf_findClass(llvmo::TerminatorInst_O::static_classSymbol(),classllvmo__TerminatorInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::TerminatorInst_O>;
        llvmo::TerminatorInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::TerminatorInst_O::static_className() % (void*)(llvmo::TerminatorInst_O::static_newNil_callback()) );
    classllvmo__TerminatorInst_Oval->setInstance_newNil_callback(llvmo::TerminatorInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::TerminatorInst_O> nil_for_class(new llvmo::TerminatorInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::TerminatorInst_O::static_className() );
        llvmo::TerminatorInst_O::_nil = nil_for_class;
        classllvmo__TerminatorInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__TerminatorInst_Oval->setSupportsSlots(llvmo::TerminatorInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__UnaryInstruction_Oval]"));
    core::BuiltInClass_sp classllvmo__UnaryInstruction_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__UnaryInstruction_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnaryInstruction_Oval,_lisp,llvmo::UnaryInstruction_O::static_classSymbol());
    llvmo::UnaryInstruction_O::___staticMetaClass = classllvmo__UnaryInstruction_Oval;
    _lisp->setf_findClass(llvmo::UnaryInstruction_O::static_classSymbol(),classllvmo__UnaryInstruction_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::UnaryInstruction_O>;
        llvmo::UnaryInstruction_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::UnaryInstruction_O::static_className() % (void*)(llvmo::UnaryInstruction_O::static_newNil_callback()) );
    classllvmo__UnaryInstruction_Oval->setInstance_newNil_callback(llvmo::UnaryInstruction_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::UnaryInstruction_O> nil_for_class(new llvmo::UnaryInstruction_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::UnaryInstruction_O::static_className() );
        llvmo::UnaryInstruction_O::_nil = nil_for_class;
        classllvmo__UnaryInstruction_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__UnaryInstruction_Oval->setSupportsSlots(llvmo::UnaryInstruction_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__UndefValue_Oval]"));
    core::BuiltInClass_sp classllvmo__UndefValue_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__UndefValue_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UndefValue_Oval,_lisp,llvmo::UndefValue_O::static_classSymbol());
    llvmo::UndefValue_O::___staticMetaClass = classllvmo__UndefValue_Oval;
    _lisp->setf_findClass(llvmo::UndefValue_O::static_classSymbol(),classllvmo__UndefValue_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::UndefValue_O>;
        llvmo::UndefValue_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::UndefValue_O::static_className() % (void*)(llvmo::UndefValue_O::static_newNil_callback()) );
    classllvmo__UndefValue_Oval->setInstance_newNil_callback(llvmo::UndefValue_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::UndefValue_O> nil_for_class(new llvmo::UndefValue_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::UndefValue_O::static_className() );
        llvmo::UndefValue_O::_nil = nil_for_class;
        classllvmo__UndefValue_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__UndefValue_Oval->setSupportsSlots(llvmo::UndefValue_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__VectorType_Oval]"));
    core::BuiltInClass_sp classllvmo__VectorType_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__VectorType_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VectorType_Oval,_lisp,llvmo::VectorType_O::static_classSymbol());
    llvmo::VectorType_O::___staticMetaClass = classllvmo__VectorType_Oval;
    _lisp->setf_findClass(llvmo::VectorType_O::static_classSymbol(),classllvmo__VectorType_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::VectorType_O>;
        llvmo::VectorType_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::VectorType_O::static_className() % (void*)(llvmo::VectorType_O::static_newNil_callback()) );
    classllvmo__VectorType_Oval->setInstance_newNil_callback(llvmo::VectorType_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::VectorType_O> nil_for_class(new llvmo::VectorType_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::VectorType_O::static_className() );
        llvmo::VectorType_O::_nil = nil_for_class;
        classllvmo__VectorType_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__VectorType_Oval->setSupportsSlots(llvmo::VectorType_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__AllocaInst_Oval]"));
    core::BuiltInClass_sp classllvmo__AllocaInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__AllocaInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__AllocaInst_Oval,_lisp,llvmo::AllocaInst_O::static_classSymbol());
    llvmo::AllocaInst_O::___staticMetaClass = classllvmo__AllocaInst_Oval;
    _lisp->setf_findClass(llvmo::AllocaInst_O::static_classSymbol(),classllvmo__AllocaInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::AllocaInst_O>;
        llvmo::AllocaInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::AllocaInst_O::static_className() % (void*)(llvmo::AllocaInst_O::static_newNil_callback()) );
    classllvmo__AllocaInst_Oval->setInstance_newNil_callback(llvmo::AllocaInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::AllocaInst_O> nil_for_class(new llvmo::AllocaInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::AllocaInst_O::static_className() );
        llvmo::AllocaInst_O::_nil = nil_for_class;
        classllvmo__AllocaInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__AllocaInst_Oval->setSupportsSlots(llvmo::AllocaInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__BranchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__BranchInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__BranchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__BranchInst_Oval,_lisp,llvmo::BranchInst_O::static_classSymbol());
    llvmo::BranchInst_O::___staticMetaClass = classllvmo__BranchInst_Oval;
    _lisp->setf_findClass(llvmo::BranchInst_O::static_classSymbol(),classllvmo__BranchInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::BranchInst_O>;
        llvmo::BranchInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::BranchInst_O::static_className() % (void*)(llvmo::BranchInst_O::static_newNil_callback()) );
    classllvmo__BranchInst_Oval->setInstance_newNil_callback(llvmo::BranchInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::BranchInst_O> nil_for_class(new llvmo::BranchInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::BranchInst_O::static_className() );
        llvmo::BranchInst_O::_nil = nil_for_class;
        classllvmo__BranchInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__BranchInst_Oval->setSupportsSlots(llvmo::BranchInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ConstantDataArray_Oval]"));
    core::BuiltInClass_sp classllvmo__ConstantDataArray_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ConstantDataArray_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ConstantDataArray_Oval,_lisp,llvmo::ConstantDataArray_O::static_classSymbol());
    llvmo::ConstantDataArray_O::___staticMetaClass = classllvmo__ConstantDataArray_Oval;
    _lisp->setf_findClass(llvmo::ConstantDataArray_O::static_classSymbol(),classllvmo__ConstantDataArray_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ConstantDataArray_O>;
        llvmo::ConstantDataArray_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ConstantDataArray_O::static_className() % (void*)(llvmo::ConstantDataArray_O::static_newNil_callback()) );
    classllvmo__ConstantDataArray_Oval->setInstance_newNil_callback(llvmo::ConstantDataArray_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ConstantDataArray_O> nil_for_class(new llvmo::ConstantDataArray_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ConstantDataArray_O::static_className() );
        llvmo::ConstantDataArray_O::_nil = nil_for_class;
        classllvmo__ConstantDataArray_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ConstantDataArray_Oval->setSupportsSlots(llvmo::ConstantDataArray_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__Function_Oval]"));
    core::BuiltInClass_sp classllvmo__Function_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__Function_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__Function_Oval,_lisp,llvmo::Function_O::static_classSymbol());
    llvmo::Function_O::___staticMetaClass = classllvmo__Function_Oval;
    _lisp->setf_findClass(llvmo::Function_O::static_classSymbol(),classllvmo__Function_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::Function_O>;
        llvmo::Function_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::Function_O::static_className() % (void*)(llvmo::Function_O::static_newNil_callback()) );
    classllvmo__Function_Oval->setInstance_newNil_callback(llvmo::Function_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::Function_O> nil_for_class(new llvmo::Function_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::Function_O::static_className() );
        llvmo::Function_O::_nil = nil_for_class;
        classllvmo__Function_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__Function_Oval->setSupportsSlots(llvmo::Function_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__GlobalVariable_Oval]"));
    core::BuiltInClass_sp classllvmo__GlobalVariable_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__GlobalVariable_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__GlobalVariable_Oval,_lisp,llvmo::GlobalVariable_O::static_classSymbol());
    llvmo::GlobalVariable_O::___staticMetaClass = classllvmo__GlobalVariable_Oval;
    _lisp->setf_findClass(llvmo::GlobalVariable_O::static_classSymbol(),classllvmo__GlobalVariable_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::GlobalVariable_O>;
        llvmo::GlobalVariable_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::GlobalVariable_O::static_className() % (void*)(llvmo::GlobalVariable_O::static_newNil_callback()) );
    classllvmo__GlobalVariable_Oval->setInstance_newNil_callback(llvmo::GlobalVariable_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::GlobalVariable_O> nil_for_class(new llvmo::GlobalVariable_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::GlobalVariable_O::static_className() );
        llvmo::GlobalVariable_O::_nil = nil_for_class;
        classllvmo__GlobalVariable_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__GlobalVariable_Oval->setSupportsSlots(llvmo::GlobalVariable_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__IndirectBrInst_Oval]"));
    core::BuiltInClass_sp classllvmo__IndirectBrInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__IndirectBrInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__IndirectBrInst_Oval,_lisp,llvmo::IndirectBrInst_O::static_classSymbol());
    llvmo::IndirectBrInst_O::___staticMetaClass = classllvmo__IndirectBrInst_Oval;
    _lisp->setf_findClass(llvmo::IndirectBrInst_O::static_classSymbol(),classllvmo__IndirectBrInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::IndirectBrInst_O>;
        llvmo::IndirectBrInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::IndirectBrInst_O::static_className() % (void*)(llvmo::IndirectBrInst_O::static_newNil_callback()) );
    classllvmo__IndirectBrInst_Oval->setInstance_newNil_callback(llvmo::IndirectBrInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::IndirectBrInst_O> nil_for_class(new llvmo::IndirectBrInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::IndirectBrInst_O::static_className() );
        llvmo::IndirectBrInst_O::_nil = nil_for_class;
        classllvmo__IndirectBrInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__IndirectBrInst_Oval->setSupportsSlots(llvmo::IndirectBrInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__InvokeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__InvokeInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__InvokeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__InvokeInst_Oval,_lisp,llvmo::InvokeInst_O::static_classSymbol());
    llvmo::InvokeInst_O::___staticMetaClass = classllvmo__InvokeInst_Oval;
    _lisp->setf_findClass(llvmo::InvokeInst_O::static_classSymbol(),classllvmo__InvokeInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::InvokeInst_O>;
        llvmo::InvokeInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::InvokeInst_O::static_className() % (void*)(llvmo::InvokeInst_O::static_newNil_callback()) );
    classllvmo__InvokeInst_Oval->setInstance_newNil_callback(llvmo::InvokeInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::InvokeInst_O> nil_for_class(new llvmo::InvokeInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::InvokeInst_O::static_className() );
        llvmo::InvokeInst_O::_nil = nil_for_class;
        classllvmo__InvokeInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__InvokeInst_Oval->setSupportsSlots(llvmo::InvokeInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__LoadInst_Oval]"));
    core::BuiltInClass_sp classllvmo__LoadInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__LoadInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__LoadInst_Oval,_lisp,llvmo::LoadInst_O::static_classSymbol());
    llvmo::LoadInst_O::___staticMetaClass = classllvmo__LoadInst_Oval;
    _lisp->setf_findClass(llvmo::LoadInst_O::static_classSymbol(),classllvmo__LoadInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::LoadInst_O>;
        llvmo::LoadInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::LoadInst_O::static_className() % (void*)(llvmo::LoadInst_O::static_newNil_callback()) );
    classllvmo__LoadInst_Oval->setInstance_newNil_callback(llvmo::LoadInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::LoadInst_O> nil_for_class(new llvmo::LoadInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::LoadInst_O::static_className() );
        llvmo::LoadInst_O::_nil = nil_for_class;
        classllvmo__LoadInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__LoadInst_Oval->setSupportsSlots(llvmo::LoadInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ResumeInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ResumeInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ResumeInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ResumeInst_Oval,_lisp,llvmo::ResumeInst_O::static_classSymbol());
    llvmo::ResumeInst_O::___staticMetaClass = classllvmo__ResumeInst_Oval;
    _lisp->setf_findClass(llvmo::ResumeInst_O::static_classSymbol(),classllvmo__ResumeInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ResumeInst_O>;
        llvmo::ResumeInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ResumeInst_O::static_className() % (void*)(llvmo::ResumeInst_O::static_newNil_callback()) );
    classllvmo__ResumeInst_Oval->setInstance_newNil_callback(llvmo::ResumeInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ResumeInst_O> nil_for_class(new llvmo::ResumeInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ResumeInst_O::static_className() );
        llvmo::ResumeInst_O::_nil = nil_for_class;
        classllvmo__ResumeInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ResumeInst_Oval->setSupportsSlots(llvmo::ResumeInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__ReturnInst_Oval]"));
    core::BuiltInClass_sp classllvmo__ReturnInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__ReturnInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__ReturnInst_Oval,_lisp,llvmo::ReturnInst_O::static_classSymbol());
    llvmo::ReturnInst_O::___staticMetaClass = classllvmo__ReturnInst_Oval;
    _lisp->setf_findClass(llvmo::ReturnInst_O::static_classSymbol(),classllvmo__ReturnInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::ReturnInst_O>;
        llvmo::ReturnInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::ReturnInst_O::static_className() % (void*)(llvmo::ReturnInst_O::static_newNil_callback()) );
    classllvmo__ReturnInst_Oval->setInstance_newNil_callback(llvmo::ReturnInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::ReturnInst_O> nil_for_class(new llvmo::ReturnInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::ReturnInst_O::static_className() );
        llvmo::ReturnInst_O::_nil = nil_for_class;
        classllvmo__ReturnInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__ReturnInst_Oval->setSupportsSlots(llvmo::ReturnInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__SwitchInst_Oval]"));
    core::BuiltInClass_sp classllvmo__SwitchInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__SwitchInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__SwitchInst_Oval,_lisp,llvmo::SwitchInst_O::static_classSymbol());
    llvmo::SwitchInst_O::___staticMetaClass = classllvmo__SwitchInst_Oval;
    _lisp->setf_findClass(llvmo::SwitchInst_O::static_classSymbol(),classllvmo__SwitchInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::SwitchInst_O>;
        llvmo::SwitchInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::SwitchInst_O::static_className() % (void*)(llvmo::SwitchInst_O::static_newNil_callback()) );
    classllvmo__SwitchInst_Oval->setInstance_newNil_callback(llvmo::SwitchInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::SwitchInst_O> nil_for_class(new llvmo::SwitchInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::SwitchInst_O::static_className() );
        llvmo::SwitchInst_O::_nil = nil_for_class;
        classllvmo__SwitchInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__SwitchInst_Oval->setSupportsSlots(llvmo::SwitchInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__UnreachableInst_Oval]"));
    core::BuiltInClass_sp classllvmo__UnreachableInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__UnreachableInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__UnreachableInst_Oval,_lisp,llvmo::UnreachableInst_O::static_classSymbol());
    llvmo::UnreachableInst_O::___staticMetaClass = classllvmo__UnreachableInst_Oval;
    _lisp->setf_findClass(llvmo::UnreachableInst_O::static_classSymbol(),classllvmo__UnreachableInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::UnreachableInst_O>;
        llvmo::UnreachableInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::UnreachableInst_O::static_className() % (void*)(llvmo::UnreachableInst_O::static_newNil_callback()) );
    classllvmo__UnreachableInst_Oval->setInstance_newNil_callback(llvmo::UnreachableInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::UnreachableInst_O> nil_for_class(new llvmo::UnreachableInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::UnreachableInst_O::static_className() );
        llvmo::UnreachableInst_O::_nil = nil_for_class;
        classllvmo__UnreachableInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__UnreachableInst_Oval->setSupportsSlots(llvmo::UnreachableInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */

    LOG(BF("Creating class[classllvmo__VAArgInst_Oval]"));
    core::BuiltInClass_sp classllvmo__VAArgInst_Oval = core::BuiltInClass_O::create(undefinedMetaClass);
    classllvmo__VAArgInst_Oval->__setup_stage1_with_sharedPtr_lisp_sid(classllvmo__VAArgInst_Oval,_lisp,llvmo::VAArgInst_O::static_classSymbol());
    llvmo::VAArgInst_O::___staticMetaClass = classllvmo__VAArgInst_Oval;
    _lisp->setf_findClass(llvmo::VAArgInst_O::static_classSymbol(),classllvmo__VAArgInst_Oval);
    {
        AllocatorCallback cb = &new_Nil<llvmo::VAArgInst_O>;
        llvmo::VAArgInst_O::___set_static_newNil_callback(cb);
    }
    LOG(BF("Set _newNil_callback for class(%s) to %X")% llvmo::VAArgInst_O::static_className() % (void*)(llvmo::VAArgInst_O::static_newNil_callback()) );
    classllvmo__VAArgInst_Oval->setInstance_newNil_callback(llvmo::VAArgInst_O::static_newNil_callback());
    {
        boost::shared_ptr<llvmo::VAArgInst_O> nil_for_class(new llvmo::VAArgInst_O(undefinedMetaClass));
        nil_for_class->__setWeakThis(nil_for_class);
        LOG(BF("Created nil for class[%s]") % llvmo::VAArgInst_O::static_className() );
        llvmo::VAArgInst_O::_nil = nil_for_class;
        classllvmo__VAArgInst_Oval->setInstanceNil(nil_for_class);
    }
    classllvmo__VAArgInst_Oval->setSupportsSlots(llvmo::VAArgInst_O::static_supportsSlots());
    /* ----- the class and its nil are now defined but the _class of these objects is undefined - set in next stage  ----- */
#endif // CREATE_CLASS
#undef CREATE_CLASS
#ifdef DUMP_INFO_CLASS // {
// Depends on nothing

    LOG(BF("---    dump_info   --- className: llvmo::APFloat_O @ %X") % classllvmo__APFloat_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APFloat_O::static_className() % llvmo::APFloat_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::APInt_O @ %X") % classllvmo__APInt_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::APInt_O::static_className() % llvmo::APInt_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Attribute_O @ %X") % classllvmo__Attribute_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Attribute_O::static_className() % llvmo::Attribute_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Builder_O @ %X") % classllvmo__Builder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Builder_O::static_className() % llvmo::Builder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DebugLoc_O @ %X") % classllvmo__DebugLoc_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DebugLoc_O::static_className() % llvmo::DebugLoc_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::EngineBuilder_O @ %X") % classllvmo__EngineBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::EngineBuilder_O::static_className() % llvmo::EngineBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ExecutionEngine_O @ %X") % classllvmo__ExecutionEngine_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ExecutionEngine_O::static_className() % llvmo::ExecutionEngine_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IRBuilderBase_O @ %X") % classllvmo__IRBuilderBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IRBuilderBase_O::static_className() % llvmo::IRBuilderBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::InsertPoint_O @ %X") % classllvmo__InsertPoint_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::InsertPoint_O::static_className() % llvmo::InsertPoint_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LLVMContext_O @ %X") % classllvmo__LLVMContext_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LLVMContext_O::static_className() % llvmo::LLVMContext_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Module_O @ %X") % classllvmo__Module_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Module_O::static_className() % llvmo::Module_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PassManagerBase_O @ %X") % classllvmo__PassManagerBase_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PassManagerBase_O::static_className() % llvmo::PassManagerBase_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Pass_O @ %X") % classllvmo__Pass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Pass_O::static_className() % llvmo::Pass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Type_O @ %X") % classllvmo__Type_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Type_O::static_className() % llvmo::Type_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Value_O @ %X") % classllvmo__Value_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Value_O::static_className() % llvmo::Value_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Argument_O @ %X") % classllvmo__Argument_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Argument_O::static_className() % llvmo::Argument_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::BasicBlock_O @ %X") % classllvmo__BasicBlock_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::BasicBlock_O::static_className() % llvmo::BasicBlock_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::CompositeType_O @ %X") % classllvmo__CompositeType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::CompositeType_O::static_className() % llvmo::CompositeType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionPassManager_O @ %X") % classllvmo__FunctionPassManager_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionPassManager_O::static_className() % llvmo::FunctionPassManager_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionPass_O @ %X") % classllvmo__FunctionPass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionPass_O::static_className() % llvmo::FunctionPass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FunctionType_O @ %X") % classllvmo__FunctionType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FunctionType_O::static_className() % llvmo::FunctionType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IRBuilder_O @ %X") % classllvmo__IRBuilder_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IRBuilder_O::static_className() % llvmo::IRBuilder_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IntegerType_O @ %X") % classllvmo__IntegerType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IntegerType_O::static_className() % llvmo::IntegerType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDNode_O @ %X") % classllvmo__MDNode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDNode_O::static_className() % llvmo::MDNode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::MDString_O @ %X") % classllvmo__MDString_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::MDString_O::static_className() % llvmo::MDString_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ModulePass_O @ %X") % classllvmo__ModulePass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ModulePass_O::static_className() % llvmo::ModulePass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::User_O @ %X") % classllvmo__User_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::User_O::static_className() % llvmo::User_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Constant_O @ %X") % classllvmo__Constant_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Constant_O::static_className() % llvmo::Constant_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ImmutablePass_O @ %X") % classllvmo__ImmutablePass_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ImmutablePass_O::static_className() % llvmo::ImmutablePass_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Instruction_O @ %X") % classllvmo__Instruction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Instruction_O::static_className() % llvmo::Instruction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::SequentialType_O @ %X") % classllvmo__SequentialType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::SequentialType_O::static_className() % llvmo::SequentialType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::StructType_O @ %X") % classllvmo__StructType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::StructType_O::static_className() % llvmo::StructType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ArrayType_O @ %X") % classllvmo__ArrayType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ArrayType_O::static_className() % llvmo::ArrayType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AtomicCmpXchgInst_O @ %X") % classllvmo__AtomicCmpXchgInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AtomicCmpXchgInst_O::static_className() % llvmo::AtomicCmpXchgInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AtomicRMWInst_O @ %X") % classllvmo__AtomicRMWInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AtomicRMWInst_O::static_className() % llvmo::AtomicRMWInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::CallInst_O @ %X") % classllvmo__CallInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::CallInst_O::static_className() % llvmo::CallInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantArray_O @ %X") % classllvmo__ConstantArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantArray_O::static_className() % llvmo::ConstantArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantDataSequential_O @ %X") % classllvmo__ConstantDataSequential_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantDataSequential_O::static_className() % llvmo::ConstantDataSequential_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantExpr_O @ %X") % classllvmo__ConstantExpr_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantExpr_O::static_className() % llvmo::ConstantExpr_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantFP_O @ %X") % classllvmo__ConstantFP_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantFP_O::static_className() % llvmo::ConstantFP_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantInt_O @ %X") % classllvmo__ConstantInt_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantInt_O::static_className() % llvmo::ConstantInt_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantPointerNull_O @ %X") % classllvmo__ConstantPointerNull_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantPointerNull_O::static_className() % llvmo::ConstantPointerNull_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::DataLayout_O @ %X") % classllvmo__DataLayout_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::DataLayout_O::static_className() % llvmo::DataLayout_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::FenceInst_O @ %X") % classllvmo__FenceInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::FenceInst_O::static_className() % llvmo::FenceInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::GlobalValue_O @ %X") % classllvmo__GlobalValue_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::GlobalValue_O::static_className() % llvmo::GlobalValue_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LandingPadInst_O @ %X") % classllvmo__LandingPadInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LandingPadInst_O::static_className() % llvmo::LandingPadInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PHINode_O @ %X") % classllvmo__PHINode_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PHINode_O::static_className() % llvmo::PHINode_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::PointerType_O @ %X") % classllvmo__PointerType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::PointerType_O::static_className() % llvmo::PointerType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::StoreInst_O @ %X") % classllvmo__StoreInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::StoreInst_O::static_className() % llvmo::StoreInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::TerminatorInst_O @ %X") % classllvmo__TerminatorInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::TerminatorInst_O::static_className() % llvmo::TerminatorInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UnaryInstruction_O @ %X") % classllvmo__UnaryInstruction_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UnaryInstruction_O::static_className() % llvmo::UnaryInstruction_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UndefValue_O @ %X") % classllvmo__UndefValue_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UndefValue_O::static_className() % llvmo::UndefValue_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::VectorType_O @ %X") % classllvmo__VectorType_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::VectorType_O::static_className() % llvmo::VectorType_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::AllocaInst_O @ %X") % classllvmo__AllocaInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::AllocaInst_O::static_className() % llvmo::AllocaInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::BranchInst_O @ %X") % classllvmo__BranchInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::BranchInst_O::static_className() % llvmo::BranchInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ConstantDataArray_O @ %X") % classllvmo__ConstantDataArray_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ConstantDataArray_O::static_className() % llvmo::ConstantDataArray_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::Function_O @ %X") % classllvmo__Function_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::Function_O::static_className() % llvmo::Function_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::GlobalVariable_O @ %X") % classllvmo__GlobalVariable_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::GlobalVariable_O::static_className() % llvmo::GlobalVariable_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::IndirectBrInst_O @ %X") % classllvmo__IndirectBrInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::IndirectBrInst_O::static_className() % llvmo::IndirectBrInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::InvokeInst_O @ %X") % classllvmo__InvokeInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::InvokeInst_O::static_className() % llvmo::InvokeInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::LoadInst_O @ %X") % classllvmo__LoadInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::LoadInst_O::static_className() % llvmo::LoadInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ResumeInst_O @ %X") % classllvmo__ResumeInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ResumeInst_O::static_className() % llvmo::ResumeInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::ReturnInst_O @ %X") % classllvmo__ReturnInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::ReturnInst_O::static_className() % llvmo::ReturnInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::SwitchInst_O @ %X") % classllvmo__SwitchInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::SwitchInst_O::static_className() % llvmo::SwitchInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::UnreachableInst_O @ %X") % classllvmo__UnreachableInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::UnreachableInst_O::static_className() % llvmo::UnreachableInst_O::static_classSymbol() );

    LOG(BF("---    dump_info   --- className: llvmo::VAArgInst_O @ %X") % classllvmo__VAArgInst_Oval.get());
    LOG(BF("%s::static_classSymbol() = %d") % llvmo::VAArgInst_O::static_className() % llvmo::VAArgInst_O::static_classSymbol() );
#endif // } DUMP_INFO_CLASS
#undef DUMP_INFO_CLASS
#if defined(DEFINE_BASE_CLASSES) || defined(ALL_STAGES) // {
// Depends on nothing
classllvmo__APFloat_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__APInt_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Attribute_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classllvmo__Builder_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__DebugLoc_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classllvmo__EngineBuilder_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__ExecutionEngine_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__IRBuilderBase_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__InsertPoint_Oval->addInstanceBaseClass(core::T_O::static_classSymbol());
classllvmo__LLVMContext_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Module_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__PassManagerBase_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Pass_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Type_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Value_Oval->addInstanceBaseClass(core::ExternalObject_O::static_classSymbol());
classllvmo__Argument_Oval->addInstanceBaseClass(llvmo::Value_O::static_classSymbol());
classllvmo__BasicBlock_Oval->addInstanceBaseClass(llvmo::Value_O::static_classSymbol());
classllvmo__CompositeType_Oval->addInstanceBaseClass(llvmo::Type_O::static_classSymbol());
classllvmo__FunctionPassManager_Oval->addInstanceBaseClass(llvmo::PassManagerBase_O::static_classSymbol());
classllvmo__FunctionPass_Oval->addInstanceBaseClass(llvmo::Pass_O::static_classSymbol());
classllvmo__FunctionType_Oval->addInstanceBaseClass(llvmo::Type_O::static_classSymbol());
classllvmo__IRBuilder_Oval->addInstanceBaseClass(llvmo::IRBuilderBase_O::static_classSymbol());
classllvmo__IntegerType_Oval->addInstanceBaseClass(llvmo::Type_O::static_classSymbol());
classllvmo__MDNode_Oval->addInstanceBaseClass(llvmo::Value_O::static_classSymbol());
classllvmo__MDString_Oval->addInstanceBaseClass(llvmo::Value_O::static_classSymbol());
classllvmo__ModulePass_Oval->addInstanceBaseClass(llvmo::Pass_O::static_classSymbol());
classllvmo__User_Oval->addInstanceBaseClass(llvmo::Value_O::static_classSymbol());
classllvmo__Constant_Oval->addInstanceBaseClass(llvmo::User_O::static_classSymbol());
classllvmo__ImmutablePass_Oval->addInstanceBaseClass(llvmo::ModulePass_O::static_classSymbol());
classllvmo__Instruction_Oval->addInstanceBaseClass(llvmo::User_O::static_classSymbol());
classllvmo__SequentialType_Oval->addInstanceBaseClass(llvmo::CompositeType_O::static_classSymbol());
classllvmo__StructType_Oval->addInstanceBaseClass(llvmo::CompositeType_O::static_classSymbol());
classllvmo__ArrayType_Oval->addInstanceBaseClass(llvmo::SequentialType_O::static_classSymbol());
classllvmo__AtomicCmpXchgInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__AtomicRMWInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__CallInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__ConstantArray_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantDataSequential_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantExpr_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantFP_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantInt_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__ConstantPointerNull_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__DataLayout_Oval->addInstanceBaseClass(llvmo::ImmutablePass_O::static_classSymbol());
classllvmo__FenceInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__GlobalValue_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__LandingPadInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__PHINode_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__PointerType_Oval->addInstanceBaseClass(llvmo::SequentialType_O::static_classSymbol());
classllvmo__StoreInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__TerminatorInst_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__UnaryInstruction_Oval->addInstanceBaseClass(llvmo::Instruction_O::static_classSymbol());
classllvmo__UndefValue_Oval->addInstanceBaseClass(llvmo::Constant_O::static_classSymbol());
classllvmo__VectorType_Oval->addInstanceBaseClass(llvmo::SequentialType_O::static_classSymbol());
classllvmo__AllocaInst_Oval->addInstanceBaseClass(llvmo::UnaryInstruction_O::static_classSymbol());
classllvmo__BranchInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__ConstantDataArray_Oval->addInstanceBaseClass(llvmo::ConstantDataSequential_O::static_classSymbol());
classllvmo__Function_Oval->addInstanceBaseClass(llvmo::GlobalValue_O::static_classSymbol());
classllvmo__GlobalVariable_Oval->addInstanceBaseClass(llvmo::GlobalValue_O::static_classSymbol());
classllvmo__IndirectBrInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__InvokeInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__LoadInst_Oval->addInstanceBaseClass(llvmo::UnaryInstruction_O::static_classSymbol());
classllvmo__ResumeInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__ReturnInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__SwitchInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__UnreachableInst_Oval->addInstanceBaseClass(llvmo::TerminatorInst_O::static_classSymbol());
classllvmo__VAArgInst_Oval->addInstanceBaseClass(llvmo::UnaryInstruction_O::static_classSymbol());
#endif // } DEFINE_BASE_CLASSES
#undef DEFINE_BASE_CLASSES
#if defined(DEFINE_CLASS_NAMES) || defined(ALL_STAGES) // {
 core::Package_sp _curPkg = _lisp->findPackage(CurrentPkg);
// Depends on nothing

    classllvmo__APFloat_Oval->__setup_stage3_name(llvmo::APFloat_O::static_classSymbol());

    classllvmo__APInt_Oval->__setup_stage3_name(llvmo::APInt_O::static_classSymbol());

    classllvmo__Attribute_Oval->__setup_stage3_name(llvmo::Attribute_O::static_classSymbol());

    classllvmo__Builder_Oval->__setup_stage3_name(llvmo::Builder_O::static_classSymbol());

    classllvmo__DebugLoc_Oval->__setup_stage3_name(llvmo::DebugLoc_O::static_classSymbol());

    classllvmo__EngineBuilder_Oval->__setup_stage3_name(llvmo::EngineBuilder_O::static_classSymbol());

    classllvmo__ExecutionEngine_Oval->__setup_stage3_name(llvmo::ExecutionEngine_O::static_classSymbol());

    classllvmo__IRBuilderBase_Oval->__setup_stage3_name(llvmo::IRBuilderBase_O::static_classSymbol());

    classllvmo__InsertPoint_Oval->__setup_stage3_name(llvmo::InsertPoint_O::static_classSymbol());

    classllvmo__LLVMContext_Oval->__setup_stage3_name(llvmo::LLVMContext_O::static_classSymbol());

    classllvmo__Module_Oval->__setup_stage3_name(llvmo::Module_O::static_classSymbol());

    classllvmo__PassManagerBase_Oval->__setup_stage3_name(llvmo::PassManagerBase_O::static_classSymbol());

    classllvmo__Pass_Oval->__setup_stage3_name(llvmo::Pass_O::static_classSymbol());

    classllvmo__Type_Oval->__setup_stage3_name(llvmo::Type_O::static_classSymbol());

    classllvmo__Value_Oval->__setup_stage3_name(llvmo::Value_O::static_classSymbol());

    classllvmo__Argument_Oval->__setup_stage3_name(llvmo::Argument_O::static_classSymbol());

    classllvmo__BasicBlock_Oval->__setup_stage3_name(llvmo::BasicBlock_O::static_classSymbol());

    classllvmo__CompositeType_Oval->__setup_stage3_name(llvmo::CompositeType_O::static_classSymbol());

    classllvmo__FunctionPassManager_Oval->__setup_stage3_name(llvmo::FunctionPassManager_O::static_classSymbol());

    classllvmo__FunctionPass_Oval->__setup_stage3_name(llvmo::FunctionPass_O::static_classSymbol());

    classllvmo__FunctionType_Oval->__setup_stage3_name(llvmo::FunctionType_O::static_classSymbol());

    classllvmo__IRBuilder_Oval->__setup_stage3_name(llvmo::IRBuilder_O::static_classSymbol());

    classllvmo__IntegerType_Oval->__setup_stage3_name(llvmo::IntegerType_O::static_classSymbol());

    classllvmo__MDNode_Oval->__setup_stage3_name(llvmo::MDNode_O::static_classSymbol());

    classllvmo__MDString_Oval->__setup_stage3_name(llvmo::MDString_O::static_classSymbol());

    classllvmo__ModulePass_Oval->__setup_stage3_name(llvmo::ModulePass_O::static_classSymbol());

    classllvmo__User_Oval->__setup_stage3_name(llvmo::User_O::static_classSymbol());

    classllvmo__Constant_Oval->__setup_stage3_name(llvmo::Constant_O::static_classSymbol());

    classllvmo__ImmutablePass_Oval->__setup_stage3_name(llvmo::ImmutablePass_O::static_classSymbol());

    classllvmo__Instruction_Oval->__setup_stage3_name(llvmo::Instruction_O::static_classSymbol());

    classllvmo__SequentialType_Oval->__setup_stage3_name(llvmo::SequentialType_O::static_classSymbol());

    classllvmo__StructType_Oval->__setup_stage3_name(llvmo::StructType_O::static_classSymbol());

    classllvmo__ArrayType_Oval->__setup_stage3_name(llvmo::ArrayType_O::static_classSymbol());

    classllvmo__AtomicCmpXchgInst_Oval->__setup_stage3_name(llvmo::AtomicCmpXchgInst_O::static_classSymbol());

    classllvmo__AtomicRMWInst_Oval->__setup_stage3_name(llvmo::AtomicRMWInst_O::static_classSymbol());

    classllvmo__CallInst_Oval->__setup_stage3_name(llvmo::CallInst_O::static_classSymbol());

    classllvmo__ConstantArray_Oval->__setup_stage3_name(llvmo::ConstantArray_O::static_classSymbol());

    classllvmo__ConstantDataSequential_Oval->__setup_stage3_name(llvmo::ConstantDataSequential_O::static_classSymbol());

    classllvmo__ConstantExpr_Oval->__setup_stage3_name(llvmo::ConstantExpr_O::static_classSymbol());

    classllvmo__ConstantFP_Oval->__setup_stage3_name(llvmo::ConstantFP_O::static_classSymbol());

    classllvmo__ConstantInt_Oval->__setup_stage3_name(llvmo::ConstantInt_O::static_classSymbol());

    classllvmo__ConstantPointerNull_Oval->__setup_stage3_name(llvmo::ConstantPointerNull_O::static_classSymbol());

    classllvmo__DataLayout_Oval->__setup_stage3_name(llvmo::DataLayout_O::static_classSymbol());

    classllvmo__FenceInst_Oval->__setup_stage3_name(llvmo::FenceInst_O::static_classSymbol());

    classllvmo__GlobalValue_Oval->__setup_stage3_name(llvmo::GlobalValue_O::static_classSymbol());

    classllvmo__LandingPadInst_Oval->__setup_stage3_name(llvmo::LandingPadInst_O::static_classSymbol());

    classllvmo__PHINode_Oval->__setup_stage3_name(llvmo::PHINode_O::static_classSymbol());

    classllvmo__PointerType_Oval->__setup_stage3_name(llvmo::PointerType_O::static_classSymbol());

    classllvmo__StoreInst_Oval->__setup_stage3_name(llvmo::StoreInst_O::static_classSymbol());

    classllvmo__TerminatorInst_Oval->__setup_stage3_name(llvmo::TerminatorInst_O::static_classSymbol());

    classllvmo__UnaryInstruction_Oval->__setup_stage3_name(llvmo::UnaryInstruction_O::static_classSymbol());

    classllvmo__UndefValue_Oval->__setup_stage3_name(llvmo::UndefValue_O::static_classSymbol());

    classllvmo__VectorType_Oval->__setup_stage3_name(llvmo::VectorType_O::static_classSymbol());

    classllvmo__AllocaInst_Oval->__setup_stage3_name(llvmo::AllocaInst_O::static_classSymbol());

    classllvmo__BranchInst_Oval->__setup_stage3_name(llvmo::BranchInst_O::static_classSymbol());

    classllvmo__ConstantDataArray_Oval->__setup_stage3_name(llvmo::ConstantDataArray_O::static_classSymbol());

    classllvmo__Function_Oval->__setup_stage3_name(llvmo::Function_O::static_classSymbol());

    classllvmo__GlobalVariable_Oval->__setup_stage3_name(llvmo::GlobalVariable_O::static_classSymbol());

    classllvmo__IndirectBrInst_Oval->__setup_stage3_name(llvmo::IndirectBrInst_O::static_classSymbol());

    classllvmo__InvokeInst_Oval->__setup_stage3_name(llvmo::InvokeInst_O::static_classSymbol());

    classllvmo__LoadInst_Oval->__setup_stage3_name(llvmo::LoadInst_O::static_classSymbol());

    classllvmo__ResumeInst_Oval->__setup_stage3_name(llvmo::ResumeInst_O::static_classSymbol());

    classllvmo__ReturnInst_Oval->__setup_stage3_name(llvmo::ReturnInst_O::static_classSymbol());

    classllvmo__SwitchInst_Oval->__setup_stage3_name(llvmo::SwitchInst_O::static_classSymbol());

    classllvmo__UnreachableInst_Oval->__setup_stage3_name(llvmo::UnreachableInst_O::static_classSymbol());

    classllvmo__VAArgInst_Oval->__setup_stage3_name(llvmo::VAArgInst_O::static_classSymbol());
#endif // } DEFINE_CLASS_NAMES
#undef DEFINE_CLASS_NAMES
#if defined(EXPOSE_TO_CANDO) || defined(ALL_STAGES)
#ifdef Use_LlvmoPkg
extern void Register_llvmo__APFloat_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__APFloat_O");
    Register_llvmo__APFloat_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__APInt_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__APInt_O");
    Register_llvmo__APInt_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Attribute_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Attribute_O");
    Register_llvmo__Attribute_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Builder_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Builder_O");
    Register_llvmo__Builder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__DebugLoc_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_llvmo__DebugLoc_O");
    Register_llvmo__DebugLoc_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__EngineBuilder_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__EngineBuilder_O");
    Register_llvmo__EngineBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ExecutionEngine_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ExecutionEngine_O");
    Register_llvmo__ExecutionEngine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__IRBuilderBase_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__IRBuilderBase_O");
    Register_llvmo__IRBuilderBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__InsertPoint_O(core::Lisp_sp); // base(s): set(['core::T_O'])
{_BLOCK_TRACE("initializing Register_llvmo__InsertPoint_O");
    Register_llvmo__InsertPoint_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__LLVMContext_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__LLVMContext_O");
    Register_llvmo__LLVMContext_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Module_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Module_O");
    Register_llvmo__Module_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__PassManagerBase_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__PassManagerBase_O");
    Register_llvmo__PassManagerBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Pass_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Pass_O");
    Register_llvmo__Pass_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Type_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Type_O");
    Register_llvmo__Type_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Value_O(core::Lisp_sp); // base(s): set(['core::ExternalObject_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Value_O");
    Register_llvmo__Value_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Argument_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Argument_O");
    Register_llvmo__Argument_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__BasicBlock_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
{_BLOCK_TRACE("initializing Register_llvmo__BasicBlock_O");
    Register_llvmo__BasicBlock_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__CompositeType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
{_BLOCK_TRACE("initializing Register_llvmo__CompositeType_O");
    Register_llvmo__CompositeType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__FunctionPassManager_O(core::Lisp_sp); // base(s): set(['llvmo::PassManagerBase_O'])
{_BLOCK_TRACE("initializing Register_llvmo__FunctionPassManager_O");
    Register_llvmo__FunctionPassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__FunctionPass_O(core::Lisp_sp); // base(s): set(['llvmo::Pass_O'])
{_BLOCK_TRACE("initializing Register_llvmo__FunctionPass_O");
    Register_llvmo__FunctionPass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__FunctionType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
{_BLOCK_TRACE("initializing Register_llvmo__FunctionType_O");
    Register_llvmo__FunctionType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__IRBuilder_O(core::Lisp_sp); // base(s): set(['llvmo::IRBuilderBase_O'])
{_BLOCK_TRACE("initializing Register_llvmo__IRBuilder_O");
    Register_llvmo__IRBuilder_O(_lisp); // base(s): set(['llvmo::IRBuilderBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__IntegerType_O(core::Lisp_sp); // base(s): set(['llvmo::Type_O'])
{_BLOCK_TRACE("initializing Register_llvmo__IntegerType_O");
    Register_llvmo__IntegerType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__MDNode_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
{_BLOCK_TRACE("initializing Register_llvmo__MDNode_O");
    Register_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__MDString_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
{_BLOCK_TRACE("initializing Register_llvmo__MDString_O");
    Register_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ModulePass_O(core::Lisp_sp); // base(s): set(['llvmo::Pass_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ModulePass_O");
    Register_llvmo__ModulePass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__User_O(core::Lisp_sp); // base(s): set(['llvmo::Value_O'])
{_BLOCK_TRACE("initializing Register_llvmo__User_O");
    Register_llvmo__User_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Constant_O(core::Lisp_sp); // base(s): set(['llvmo::User_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Constant_O");
    Register_llvmo__Constant_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ImmutablePass_O(core::Lisp_sp); // base(s): set(['llvmo::ModulePass_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ImmutablePass_O");
    Register_llvmo__ImmutablePass_O(_lisp); // base(s): set(['llvmo::ModulePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Instruction_O(core::Lisp_sp); // base(s): set(['llvmo::User_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Instruction_O");
    Register_llvmo__Instruction_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__SequentialType_O(core::Lisp_sp); // base(s): set(['llvmo::CompositeType_O'])
{_BLOCK_TRACE("initializing Register_llvmo__SequentialType_O");
    Register_llvmo__SequentialType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__StructType_O(core::Lisp_sp); // base(s): set(['llvmo::CompositeType_O'])
{_BLOCK_TRACE("initializing Register_llvmo__StructType_O");
    Register_llvmo__StructType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ArrayType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ArrayType_O");
    Register_llvmo__ArrayType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__AtomicCmpXchgInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__AtomicCmpXchgInst_O");
    Register_llvmo__AtomicCmpXchgInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__AtomicRMWInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__AtomicRMWInst_O");
    Register_llvmo__AtomicRMWInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__CallInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__CallInst_O");
    Register_llvmo__CallInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantArray_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantArray_O");
    Register_llvmo__ConstantArray_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantDataSequential_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantDataSequential_O");
    Register_llvmo__ConstantDataSequential_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantExpr_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantExpr_O");
    Register_llvmo__ConstantExpr_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantFP_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantFP_O");
    Register_llvmo__ConstantFP_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantInt_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantInt_O");
    Register_llvmo__ConstantInt_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantPointerNull_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantPointerNull_O");
    Register_llvmo__ConstantPointerNull_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__DataLayout_O(core::Lisp_sp); // base(s): set(['llvmo::ImmutablePass_O'])
{_BLOCK_TRACE("initializing Register_llvmo__DataLayout_O");
    Register_llvmo__DataLayout_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__FenceInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__FenceInst_O");
    Register_llvmo__FenceInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__GlobalValue_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__GlobalValue_O");
    Register_llvmo__GlobalValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__LandingPadInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__LandingPadInst_O");
    Register_llvmo__LandingPadInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__PHINode_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__PHINode_O");
    Register_llvmo__PHINode_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__PointerType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
{_BLOCK_TRACE("initializing Register_llvmo__PointerType_O");
    Register_llvmo__PointerType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__StoreInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__StoreInst_O");
    Register_llvmo__StoreInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__TerminatorInst_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__TerminatorInst_O");
    Register_llvmo__TerminatorInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__UnaryInstruction_O(core::Lisp_sp); // base(s): set(['llvmo::Instruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__UnaryInstruction_O");
    Register_llvmo__UnaryInstruction_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__UndefValue_O(core::Lisp_sp); // base(s): set(['llvmo::Constant_O'])
{_BLOCK_TRACE("initializing Register_llvmo__UndefValue_O");
    Register_llvmo__UndefValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__VectorType_O(core::Lisp_sp); // base(s): set(['llvmo::SequentialType_O'])
{_BLOCK_TRACE("initializing Register_llvmo__VectorType_O");
    Register_llvmo__VectorType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__AllocaInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__AllocaInst_O");
    Register_llvmo__AllocaInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__BranchInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__BranchInst_O");
    Register_llvmo__BranchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ConstantDataArray_O(core::Lisp_sp); // base(s): set(['llvmo::ConstantDataSequential_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ConstantDataArray_O");
    Register_llvmo__ConstantDataArray_O(_lisp); // base(s): set(['llvmo::ConstantDataSequential_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__Function_O(core::Lisp_sp); // base(s): set(['llvmo::GlobalValue_O'])
{_BLOCK_TRACE("initializing Register_llvmo__Function_O");
    Register_llvmo__Function_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__GlobalVariable_O(core::Lisp_sp); // base(s): set(['llvmo::GlobalValue_O'])
{_BLOCK_TRACE("initializing Register_llvmo__GlobalVariable_O");
    Register_llvmo__GlobalVariable_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__IndirectBrInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__IndirectBrInst_O");
    Register_llvmo__IndirectBrInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__InvokeInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__InvokeInst_O");
    Register_llvmo__InvokeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__LoadInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__LoadInst_O");
    Register_llvmo__LoadInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ResumeInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ResumeInst_O");
    Register_llvmo__ResumeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__ReturnInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__ReturnInst_O");
    Register_llvmo__ReturnInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__SwitchInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__SwitchInst_O");
    Register_llvmo__SwitchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__UnreachableInst_O(core::Lisp_sp); // base(s): set(['llvmo::TerminatorInst_O'])
{_BLOCK_TRACE("initializing Register_llvmo__UnreachableInst_O");
    Register_llvmo__UnreachableInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Register_llvmo__VAArgInst_O(core::Lisp_sp); // base(s): set(['llvmo::UnaryInstruction_O'])
{_BLOCK_TRACE("initializing Register_llvmo__VAArgInst_O");
    Register_llvmo__VAArgInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#endif // EXPOSE_TO_CANDO
#undef EXPOSE_TO_CANDO
#ifdef EXPOSE_TO_PYTHON
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__APFloat_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__APFloat_O");
	Call_exposePython_llvmo__APFloat_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__APInt_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__APInt_O");
	Call_exposePython_llvmo__APInt_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Attribute_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Attribute_O");
	Call_exposePython_llvmo__Attribute_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Builder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Builder_O");
	Call_exposePython_llvmo__Builder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DebugLoc_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DebugLoc_O");
	Call_exposePython_llvmo__DebugLoc_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__EngineBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__EngineBuilder_O");
	Call_exposePython_llvmo__EngineBuilder_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ExecutionEngine_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ExecutionEngine_O");
	Call_exposePython_llvmo__ExecutionEngine_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IRBuilderBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IRBuilderBase_O");
	Call_exposePython_llvmo__IRBuilderBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__InsertPoint_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__InsertPoint_O");
	Call_exposePython_llvmo__InsertPoint_O(_lisp); // base(s): set(['core::T_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LLVMContext_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LLVMContext_O");
	Call_exposePython_llvmo__LLVMContext_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Module_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Module_O");
	Call_exposePython_llvmo__Module_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PassManagerBase_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PassManagerBase_O");
	Call_exposePython_llvmo__PassManagerBase_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Pass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Pass_O");
	Call_exposePython_llvmo__Pass_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Type_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Type_O");
	Call_exposePython_llvmo__Type_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Value_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Value_O");
	Call_exposePython_llvmo__Value_O(_lisp); // base(s): set(['core::ExternalObject_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Argument_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Argument_O");
	Call_exposePython_llvmo__Argument_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__BasicBlock_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__BasicBlock_O");
	Call_exposePython_llvmo__BasicBlock_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__CompositeType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__CompositeType_O");
	Call_exposePython_llvmo__CompositeType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionPassManager_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionPassManager_O");
	Call_exposePython_llvmo__FunctionPassManager_O(_lisp); // base(s): set(['llvmo::PassManagerBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionPass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionPass_O");
	Call_exposePython_llvmo__FunctionPass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FunctionType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FunctionType_O");
	Call_exposePython_llvmo__FunctionType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IRBuilder_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IRBuilder_O");
	Call_exposePython_llvmo__IRBuilder_O(_lisp); // base(s): set(['llvmo::IRBuilderBase_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IntegerType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IntegerType_O");
	Call_exposePython_llvmo__IntegerType_O(_lisp); // base(s): set(['llvmo::Type_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDNode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDNode_O");
	Call_exposePython_llvmo__MDNode_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__MDString_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__MDString_O");
	Call_exposePython_llvmo__MDString_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ModulePass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ModulePass_O");
	Call_exposePython_llvmo__ModulePass_O(_lisp); // base(s): set(['llvmo::Pass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__User_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__User_O");
	Call_exposePython_llvmo__User_O(_lisp); // base(s): set(['llvmo::Value_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Constant_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Constant_O");
	Call_exposePython_llvmo__Constant_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ImmutablePass_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ImmutablePass_O");
	Call_exposePython_llvmo__ImmutablePass_O(_lisp); // base(s): set(['llvmo::ModulePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Instruction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Instruction_O");
	Call_exposePython_llvmo__Instruction_O(_lisp); // base(s): set(['llvmo::User_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__SequentialType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__SequentialType_O");
	Call_exposePython_llvmo__SequentialType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__StructType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__StructType_O");
	Call_exposePython_llvmo__StructType_O(_lisp); // base(s): set(['llvmo::CompositeType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ArrayType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ArrayType_O");
	Call_exposePython_llvmo__ArrayType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AtomicCmpXchgInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AtomicCmpXchgInst_O");
	Call_exposePython_llvmo__AtomicCmpXchgInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AtomicRMWInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AtomicRMWInst_O");
	Call_exposePython_llvmo__AtomicRMWInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__CallInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__CallInst_O");
	Call_exposePython_llvmo__CallInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantArray_O");
	Call_exposePython_llvmo__ConstantArray_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantDataSequential_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantDataSequential_O");
	Call_exposePython_llvmo__ConstantDataSequential_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantExpr_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantExpr_O");
	Call_exposePython_llvmo__ConstantExpr_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantFP_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantFP_O");
	Call_exposePython_llvmo__ConstantFP_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantInt_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantInt_O");
	Call_exposePython_llvmo__ConstantInt_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantPointerNull_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantPointerNull_O");
	Call_exposePython_llvmo__ConstantPointerNull_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__DataLayout_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__DataLayout_O");
	Call_exposePython_llvmo__DataLayout_O(_lisp); // base(s): set(['llvmo::ImmutablePass_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__FenceInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__FenceInst_O");
	Call_exposePython_llvmo__FenceInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__GlobalValue_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__GlobalValue_O");
	Call_exposePython_llvmo__GlobalValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LandingPadInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LandingPadInst_O");
	Call_exposePython_llvmo__LandingPadInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PHINode_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PHINode_O");
	Call_exposePython_llvmo__PHINode_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__PointerType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__PointerType_O");
	Call_exposePython_llvmo__PointerType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__StoreInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__StoreInst_O");
	Call_exposePython_llvmo__StoreInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__TerminatorInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__TerminatorInst_O");
	Call_exposePython_llvmo__TerminatorInst_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UnaryInstruction_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UnaryInstruction_O");
	Call_exposePython_llvmo__UnaryInstruction_O(_lisp); // base(s): set(['llvmo::Instruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UndefValue_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UndefValue_O");
	Call_exposePython_llvmo__UndefValue_O(_lisp); // base(s): set(['llvmo::Constant_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__VectorType_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__VectorType_O");
	Call_exposePython_llvmo__VectorType_O(_lisp); // base(s): set(['llvmo::SequentialType_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__AllocaInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__AllocaInst_O");
	Call_exposePython_llvmo__AllocaInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__BranchInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__BranchInst_O");
	Call_exposePython_llvmo__BranchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ConstantDataArray_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ConstantDataArray_O");
	Call_exposePython_llvmo__ConstantDataArray_O(_lisp); // base(s): set(['llvmo::ConstantDataSequential_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__Function_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__Function_O");
	Call_exposePython_llvmo__Function_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__GlobalVariable_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__GlobalVariable_O");
	Call_exposePython_llvmo__GlobalVariable_O(_lisp); // base(s): set(['llvmo::GlobalValue_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__IndirectBrInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__IndirectBrInst_O");
	Call_exposePython_llvmo__IndirectBrInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__InvokeInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__InvokeInst_O");
	Call_exposePython_llvmo__InvokeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__LoadInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__LoadInst_O");
	Call_exposePython_llvmo__LoadInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ResumeInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ResumeInst_O");
	Call_exposePython_llvmo__ResumeInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__ReturnInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__ReturnInst_O");
	Call_exposePython_llvmo__ReturnInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__SwitchInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__SwitchInst_O");
	Call_exposePython_llvmo__SwitchInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__UnreachableInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__UnreachableInst_O");
	Call_exposePython_llvmo__UnreachableInst_O(_lisp); // base(s): set(['llvmo::TerminatorInst_O'])
}
#endif // ifdef Use_LlvmoPkg
#ifdef Use_LlvmoPkg
extern void Call_exposePython_llvmo__VAArgInst_O(::core::Lisp_sp lisp);
{_DBG("exposing to python: llvmo__VAArgInst_O");
	Call_exposePython_llvmo__VAArgInst_O(_lisp); // base(s): set(['llvmo::UnaryInstruction_O'])
}
#endif // ifdef Use_LlvmoPkg
#endif // EXPOSE_TO_PYTHON
#undef EXPOSE_TO_PYTHON
#undef ALL_STAGES
