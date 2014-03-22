/* Missing stuff from llvm */

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

/* Just use all includes from Core.cpp */
#include "llvm-c/Core.h"
#include "llvm/IR/Attributes.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/PassManager.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/system_error.h"
#include <cassert>
#include <cstdlib>
#include <cstring>

using namespace llvm;

#define LLVM_DEFINE_VALUE_CAST(name)                                       \
  LLVMValueRef LLVMIsA##name(LLVMValueRef Val) {                           \
    return wrap(static_cast<Value*>(dyn_cast_or_null<name>(unwrap(Val)))); \
  }

extern "C" {
LLVM_DEFINE_VALUE_CAST(ConstantDataArray)
LLVM_DEFINE_VALUE_CAST(ConstantDataVector)
LLVM_DEFINE_VALUE_CAST(FenceInst)
LLVM_DEFINE_VALUE_CAST(AtomicCmpXchgInst)
LLVM_DEFINE_VALUE_CAST(AtomicRMWInst)
}

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"

extern "C" {
CAMLprim value get_alignment(LLVMValueRef Value) {
  /* There are several getAlignment() methods, need to be sure to use the right one */
  if (LLVMIsAGlobalVariable(Value)) {
    GlobalVariable *I = unwrap<GlobalVariable>(Value);
    if (I) return Val_int(I->getAlignment());
  } else if (LLVMIsAFunction(Value)) {
    Function *I = unwrap<Function>(Value);
    if (I) return Val_int(I->getAlignment());
  } else if (LLVMIsAAllocaInst(Value)) {
    AllocaInst *I = unwrap<AllocaInst>(Value);
    if (I) return Val_int(I->getAlignment());
  } else if (LLVMIsALoadInst(Value)) {
    LoadInst *I = unwrap<LoadInst>(Value);
    if (I) return Val_int(I->getAlignment());
  } else if (LLVMIsAStoreInst(Value)) {
    StoreInst *I = unwrap<StoreInst>(Value);
    if (I) return Val_int(I->getAlignment());
  }
  return Val_int(-1);
}
CAMLprim LLVMValueRef getElementAsConstant(LLVMValueRef Value, value index) {
  ConstantDataSequential *I = unwrap<ConstantDataSequential>(Value);
  if (I) return wrap(I->getElementAsConstant(Long_val(index)));
  return 0;
}
CAMLprim value getNumElements(LLVMValueRef Value) {
  if (LLVMIsAConstantDataSequential(Value)) {
    ConstantDataSequential *I = unwrap<ConstantDataSequential>(Value);
    if (I) return Val_int(I->getNumElements());
  }
  return Val_int(-1);
}
}
