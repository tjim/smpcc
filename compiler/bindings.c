#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include "llvm-c/Core.h"

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/callback.h"

#define DEFINE_ISA(Kind) \
CAMLprim value isa_##Kind(LLVMValueRef Val) {return Val_bool(LLVMIsA##Kind(Val));}

LLVM_FOR_EACH_VALUE_SUBCLASS(DEFINE_ISA)

/* LLVM_FOR_EACH_VALUE_SUBCLASS does not iterate over
   ConstantDataArray, ConstantDataVector
   FenceInst, AtomicCmpXchgInst, AtomicRMWInst
   Looks like a bug in Core.h, we fix things up here and in llvmfixup.cpp
   Also note that Core.h indicates that UnaryInstruction is NOT a subclass of Instruction,
   when in fact it is a subclass
 */

#include "llvm/Support/DataTypes.h"

#ifdef __cplusplus

/* Need these includes to support the LLVM 'cast' template for the C++ 'wrap' 
   and 'unwrap' conversion functions. */
#include "llvm/IRBuilder.h"
#include "llvm/Module.h"
#include "llvm/PassRegistry.h"

extern "C" {
#endif

//LLVM_DECLARE_VALUE_CAST(ConstantDataArray)
//LLVM_DECLARE_VALUE_CAST(ConstantDataVector)
LLVM_DECLARE_VALUE_CAST(FenceInst)
LLVM_DECLARE_VALUE_CAST(AtomicCmpXchgInst)
LLVM_DECLARE_VALUE_CAST(AtomicRMWInst)

//DEFINE_ISA(ConstantDataArray)
//DEFINE_ISA(ConstantDataVector)
DEFINE_ISA(FenceInst)
DEFINE_ISA(AtomicCmpXchgInst)
DEFINE_ISA(AtomicRMWInst)

#ifdef __cplusplus
}
#endif

