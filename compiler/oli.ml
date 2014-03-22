(* OLI: Ocaml LLVM Interface *)

(* Types *)
type oType =
  | Void
  | Half
  | Float
  | Double
  | X86fp80
  | X86_mmx
  | Fp128
  | Ppc_fp128
  | Label
  | Metadata
  (* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
  | Integer    of int                               (* bitwidth *)
  | FunctionTy of oType * oType list * bool         (* return type, param types, is_var_arg *)
  | Struct     of string option                     (* name *) (* TODO: missing isLiteral *)
  | Array      of int * oType                       (* array length, element type *)
  | Pointer    of int * oType                       (* address space, element type *)
  | Vector     of int * oType                       (* array length, element type *)

let rec bitwidth = function
  | Void                                         -> 0
  | Half                                         -> 16 (* ??? *)
  | Float                                        -> 32
  | Double                                       -> 64
  | X86fp80                                      -> 80
  | X86_mmx                                      -> 128 (* ??? *)
  | Fp128                                        -> 128
  | Ppc_fp128                                    -> 128
  | Label                                        -> 32 (* ??? *)
  | Metadata                                     -> -1 (* ??? *)
(* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
  | Integer x                                    -> x
  | FunctionTy(return_ty, param_tys, is_var_arg) -> -1 (* ??? *)
  | Struct nopt                                  -> 64 (* TEMPORARY TO HACK DIJKSTRA *)
  | Array(len,element_ty)                        -> len*(bitwidth element_ty)
  | Pointer(address_space,element_ty)            -> 64
  | Vector(len,element_ty)                       -> len*(bitwidth element_ty)

let bytewidth ty =
  let bits = bitwidth ty in
  if (bits mod 8) <> 0 then failwith "bytewidth: bits are not a multiple of 8";
  bits/8

type alignment = int

type variable = (bool * string * oType) (* (isaglobal, name, type) *)
type typed_value = oType * oValue
and oValue =
| Variable           of variable (* not an LLVM value, added here to break Instruction -> Instruction cycles in the value graph *)
| Argument           of variable
| BasicBlock         of variable (* name or slot *)
| InlineAsm
| MDNode
| MDString           of string
| Instruction        of oInstruction (* This is in the class hierarchy but we actually never use it, due to cycle-breaking *)
| NullValue
| BlockAddress       of oValue
| ConstantAggregateZero
| ConstantArray      of oValue list
| ConstantDataArray  of oValue list
| ConstantDataVector of oValue list
| ConstantExpr       of Llvm.Opcode.t * typed_value list
| ConstantFP
| ConstantInt        of int * int64 option (* bitwidth, value*)
| ConstantPointerNull of int (* bitwidth *)
| ConstantStruct     of bool * oValue list
| ConstantVector     of oValue list
| UndefValue
| Function           of variable
| GlobalAlias        of variable
| GlobalVariable     of variable
and oInstruction =
| AssignInst         of oType * (oType * oValue) list (* Not in LLVM, added for my convenience *)
| BinaryOperator     of oType * (oType * oValue) list * Llvm.Opcode.t
| CallInst           of bool * int * oType * variable * (oType * oValue) list (* is_tail_call, Llvm.CallConv, callee type, callee name, operands *) (*of oCallInst*) (* Don't bother with intrinsic hierarchy for now *)
| FCmpInst           of oType * (oType * oValue) list
| ICmpInst           of Llvm.Icmp.t option * (oType * oValue) list
| ExtractElementInst of oType * (oType * oValue) list
| GetElementPtrInst  of oType * (oType * oValue) list
| InsertElementInst  of oType * (oType * oValue) list
| InsertValueInst    of oType * (oType * oValue) list
| LandingPadInst     of oType * (oType * oValue) list
| PHINode            of oType * (oValue * variable) list (* type, (operand x block name) list *)
| SelectInst         of oType * (oType * oValue) list
| ShuffleVectorInst  of oType * (oType * oValue) list
| StoreInst          of alignment * oType * (oType * oValue) list
| BranchInst         of oType * (oType * oValue) list
| IndirectBrInst     of oType * (oType * oValue) list
| InvokeInst         of oType * (oType * oValue) list
| ReturnInst         of oType * (oType * oValue) list
| SwitchInst         of oType * (oType * oValue) list
| UnreachableInst    of oType * (oType * oValue) list
| ResumeInst         of oType * (oType * oValue) list
| AllocaInst         of alignment * oType * (oType * oValue) list
| BitCastInst        of oType * (oType * oValue) list
| FPExtInst          of oType * (oType * oValue) list
| FPToSIInst         of oType * (oType * oValue) list
| FPToUIInst         of oType * (oType * oValue) list
| FPTruncInst        of oType * (oType * oValue) list
| IntToPtrInst       of oType * (oType * oValue) list
| PtrToIntInst       of oType * (oType * oValue) list
| SExtInst           of oType * (oType * oValue) list
| SIToFPInst         of oType * (oType * oValue) list
| TruncInst          of oType * (oType * oValue) list
| UIToFPInst         of oType * (oType * oValue) list
| ZExtInst           of oType * (oType * oValue) list
| ExtractValueInst   of oType * (oType * oValue) list
| LoadInst           of alignment * oType * (oType * oValue) list
| VAArgInst          of oType * (oType * oValue) list
| FenceInst          of oType * (oType * oValue) list
| AtomicCmpXchgInst  of oType * (oType * oValue) list
| AtomicRMWInst      of oType * (oType * oValue) list
and oCallInst = (* currently unused *)
| IntrinsicInst      of oIntrinsicInst
and oIntrinsicInst = (* currently unused *)
| DbgInfoIntrinsic   of oDbgInfoIntrinsic
| MemIntrinsic       of oMemIntrinsic
and oDbgInfoIntrinsic = (* currently unused *)
| DbgDeclareInst
and oMemIntrinsic = (* currently unused *)
| MemCpyInst
| MemMoveInst
| MemSetInst

let ty_of_instruction = function
| BinaryOperator     (ty,ops,opcode) -> ty
| CallInst           (_,_,ty,_,ops)  -> ty
| AssignInst         (ty,ops)        -> ty
| ExtractElementInst (ty,ops)        -> ty
| GetElementPtrInst  (ty,ops)        -> ty
| InsertElementInst  (ty,ops)        -> ty
| InsertValueInst    (ty,ops)        -> ty
| LandingPadInst     (ty,ops)        -> ty
| PHINode            (ty,ops)        -> ty
| SelectInst         (ty,ops)        -> ty
| ShuffleVectorInst  (ty,ops)        -> ty
| StoreInst          (_,ty,ops)      -> ty
| FenceInst          (ty,ops)        -> ty
| AtomicCmpXchgInst  (ty,ops)        -> ty
| AtomicRMWInst      (ty,ops)        -> ty
| FCmpInst           (ty,ops)        -> ty
| ICmpInst           (cm,ops)        -> Integer 32 (* TODO: this is a guess *)
| BranchInst         (ty,ops)        -> ty
| IndirectBrInst     (ty,ops)        -> ty
| InvokeInst         (ty,ops)        -> ty
| ReturnInst         (ty,ops)        -> ty
| SwitchInst         (ty,ops)        -> ty
| UnreachableInst    (ty,ops)        -> ty
| ResumeInst         (ty,ops)        -> ty
| AllocaInst         (_,ty,ops)      -> ty
| ExtractValueInst   (ty,ops)        -> ty
| LoadInst           (_,ty,ops)      -> ty
| VAArgInst          (ty,ops)        -> ty
| BitCastInst        (ty,ops)        -> ty
| FPExtInst          (ty,ops)        -> ty
| FPToSIInst         (ty,ops)        -> ty
| FPToUIInst         (ty,ops)        -> ty
| FPTruncInst        (ty,ops)        -> ty
| IntToPtrInst       (ty,ops)        -> ty
| PtrToIntInst       (ty,ops)        -> ty
| SExtInst           (ty,ops)        -> ty
| SIToFPInst         (ty,ops)        -> ty
| TruncInst          (ty,ops)        -> ty
| UIToFPInst         (ty,ops)        -> ty
| ZExtInst           (ty,ops)        -> ty
let ops_of_instruction = function
| BinaryOperator     (ty,ops,opcode) -> ops
| CallInst           (_,_,ty,_,ops)  -> ops
| AssignInst         (ty,ops)        -> ops
| ExtractElementInst (ty,ops)        -> ops
| GetElementPtrInst  (ty,ops)        -> ops
| InsertElementInst  (ty,ops)        -> ops
| InsertValueInst    (ty,ops)        -> ops
| LandingPadInst     (ty,ops)        -> ops
| PHINode            (ty,ops)        -> List.map (fun (v,bl) -> (ty,v)) ops
| SelectInst         (ty,ops)        -> ops
| ShuffleVectorInst  (ty,ops)        -> ops
| StoreInst          (_,ty,ops)      -> ops
| FenceInst          (ty,ops)        -> ops
| AtomicCmpXchgInst  (ty,ops)        -> ops
| AtomicRMWInst      (ty,ops)        -> ops
| FCmpInst           (ty,ops)        -> ops
| ICmpInst           (cm,ops)        -> ops
| BranchInst         (ty,ops)        -> ops
| IndirectBrInst     (ty,ops)        -> ops
| InvokeInst         (ty,ops)        -> ops
| ReturnInst         (ty,ops)        -> ops
| SwitchInst         (ty,ops)        -> ops
| UnreachableInst    (ty,ops)        -> ops
| ResumeInst         (ty,ops)        -> ops
| AllocaInst         (_,ty,ops)      -> ops
| ExtractValueInst   (ty,ops)        -> ops
| LoadInst           (_,ty,ops)      -> ops
| VAArgInst          (ty,ops)        -> ops
| BitCastInst        (ty,ops)        -> ops
| FPExtInst          (ty,ops)        -> ops
| FPToSIInst         (ty,ops)        -> ops
| FPToUIInst         (ty,ops)        -> ops
| FPTruncInst        (ty,ops)        -> ops
| IntToPtrInst       (ty,ops)        -> ops
| PtrToIntInst       (ty,ops)        -> ops
| SExtInst           (ty,ops)        -> ops
| SIToFPInst         (ty,ops)        -> ops
| TruncInst          (ty,ops)        -> ops
| UIToFPInst         (ty,ops)        -> ops
| ZExtInst           (ty,ops)        -> ops
let opcode_of_instruction = function
| BinaryOperator     (ty,ops,opcode) -> opcode
| CallInst           (_,_,ty,_,ops)  -> Llvm.Opcode.Call
| AssignInst         (ty,ops)        -> Llvm.Opcode.ExtractElement (* TODO: FIX !!! *)
| ExtractElementInst (ty,ops)        -> Llvm.Opcode.ExtractElement
| GetElementPtrInst  (ty,ops)        -> Llvm.Opcode.GetElementPtr
| InsertElementInst  (ty,ops)        -> Llvm.Opcode.InsertElement
| InsertValueInst    (ty,ops)        -> Llvm.Opcode.InsertValue
| LandingPadInst     (ty,ops)        -> Llvm.Opcode.LandingPad
| PHINode            (ty,ops)        -> Llvm.Opcode.PHI
| SelectInst         (ty,ops)        -> Llvm.Opcode.Select
| ShuffleVectorInst  (ty,ops)        -> Llvm.Opcode.ShuffleVector
| StoreInst          (_,ty,ops)      -> Llvm.Opcode.Store
| FenceInst          (ty,ops)        -> Llvm.Opcode.Fence
| AtomicCmpXchgInst  (ty,ops)        -> Llvm.Opcode.AtomicCmpXchg
| AtomicRMWInst      (ty,ops)        -> Llvm.Opcode.AtomicRMW
| FCmpInst           (ty,ops)        -> Llvm.Opcode.FCmp
| ICmpInst           (cm,ops)        -> Llvm.Opcode.ICmp
| BranchInst         (ty,ops)        -> Llvm.Opcode.Br
| IndirectBrInst     (ty,ops)        -> Llvm.Opcode.IndirectBr
| InvokeInst         (ty,ops)        -> Llvm.Opcode.Invoke
| ReturnInst         (ty,ops)        -> Llvm.Opcode.Ret
| SwitchInst         (ty,ops)        -> Llvm.Opcode.Switch
| UnreachableInst    (ty,ops)        -> Llvm.Opcode.Unreachable
| ResumeInst         (ty,ops)        -> Llvm.Opcode.Resume
| AllocaInst         (_,ty,ops)      -> Llvm.Opcode.Alloca
| ExtractValueInst   (ty,ops)        -> Llvm.Opcode.ExtractValue
| LoadInst           (_,ty,ops)      -> Llvm.Opcode.Load
| VAArgInst          (ty,ops)        -> Llvm.Opcode.VAArg
| BitCastInst        (ty,ops)        -> Llvm.Opcode.BitCast
| FPExtInst          (ty,ops)        -> Llvm.Opcode.FPExt
| FPToSIInst         (ty,ops)        -> Llvm.Opcode.FPToSI
| FPToUIInst         (ty,ops)        -> Llvm.Opcode.FPToUI
| FPTruncInst        (ty,ops)        -> Llvm.Opcode.FPTrunc
| IntToPtrInst       (ty,ops)        -> Llvm.Opcode.IntToPtr
| PtrToIntInst       (ty,ops)        -> Llvm.Opcode.PtrToInt
| SExtInst           (ty,ops)        -> Llvm.Opcode.SExt
| SIToFPInst         (ty,ops)        -> Llvm.Opcode.SIToFP
| TruncInst          (ty,ops)        -> Llvm.Opcode.Trunc
| UIToFPInst         (ty,ops)        -> Llvm.Opcode.UIToFP
| ZExtInst           (ty,ops)        -> Llvm.Opcode.ZExt

type g_info = {
    g_name: variable;
    g_ty: oType;
    g_val: oValue;
    g_linkage: Llvm.Linkage.t;
    g_visibility: Llvm.Visibility.t;
    g_is_constant: bool;
  }

type attr = Llvm.Attribute.t

type b_info = {
    b_name: variable;
    mutable b_instrs: (variable option * oInstruction) list;
  }

type p_info = (variable * attr list) (* variable, attributes *)

type f_info = {
    f_name: variable;
    f_is_declaration: bool;
    f_linkage: Llvm.Linkage.t;
    f_visibility: Llvm.Visibility.t;
    f_call_conv: int; (* See Llvm.CallConv *)
    f_type: oType;
    f_params: p_info list;
    f_attrs: attr list;
    mutable f_blocks: b_info list;
  }

type mod_info = {
    globals: g_info list;
    functions: f_info list;
  }

let getElementType = function
  | Array(_,ty) -> ty
  | Pointer(_,ty) -> ty
  | Vector(_,ty) -> ty
  | _ -> failwith "Oil.getElementType: not an Array, Pointer, or Vector"

let rec cvt_oType ty =
  match Llvm.classify_type ty with
  | Llvm.TypeKind.Void      -> Void
  | Llvm.TypeKind.Half      -> Half
  | Llvm.TypeKind.Float     -> Float
  | Llvm.TypeKind.Double    -> Double
  | Llvm.TypeKind.X86fp80   -> X86fp80
  | Llvm.TypeKind.X86_mmx   -> X86_mmx
  | Llvm.TypeKind.Fp128     -> Fp128
  | Llvm.TypeKind.Ppc_fp128 -> Ppc_fp128
  | Llvm.TypeKind.Label     -> Label
  | Llvm.TypeKind.Metadata  -> Metadata
(* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
  | Llvm.TypeKind.Integer   -> Integer (Llvm.integer_bitwidth ty)
  | Llvm.TypeKind.Array     -> Array (Llvm.array_length ty, cvt_oType (Llvm.element_type ty))
  | Llvm.TypeKind.Pointer   -> Pointer (Llvm.address_space ty, cvt_oType (Llvm.element_type ty));
  | Llvm.TypeKind.Vector    -> Vector (Llvm.vector_size ty, cvt_oType (Llvm.element_type ty));
  | Llvm.TypeKind.Struct    -> Struct (Llvm.struct_name ty) (* TODO: missing isLiteral *)
  | Llvm.TypeKind.Function  -> FunctionTy (cvt_oType (Llvm.return_type ty),
                                           List.map cvt_oType (Array.to_list (Llvm.param_types ty)),
                                           Llvm.is_var_arg ty)

external get_alignment             : Llvm.llvalue -> int = "get_alignment"
external getElementAsConstant      : Llvm.llvalue -> int -> Llvm.llvalue = "getElementAsConstant"
external getNumElements            : Llvm.llvalue -> int = "getNumElements"

(* Value classification, see llvm-c/Core.h *)
external isAArgument               : Llvm.llvalue -> bool = "isa_Argument"
external isABasicBlock             : Llvm.llvalue -> bool = "isa_BasicBlock"
external isAInlineAsm              : Llvm.llvalue -> bool = "isa_InlineAsm"
external isAMDNode                 : Llvm.llvalue -> bool = "isa_MDNode"
external isAMDString               : Llvm.llvalue -> bool = "isa_MDString"
external isAUser                   : Llvm.llvalue -> bool = "isa_User"
external isAConstant               : Llvm.llvalue -> bool = "isa_Constant"
external isABlockAddress           : Llvm.llvalue -> bool = "isa_BlockAddress"
external isAConstantAggregateZero  : Llvm.llvalue -> bool = "isa_ConstantAggregateZero"
external isAConstantArray          : Llvm.llvalue -> bool = "isa_ConstantArray"
external isAConstantDataArray      : Llvm.llvalue -> bool = "isa_ConstantDataArray"
external isAConstantDataVector     : Llvm.llvalue -> bool = "isa_ConstantDataVector"
external isAConstantExpr           : Llvm.llvalue -> bool = "isa_ConstantExpr"
external isAConstantFP             : Llvm.llvalue -> bool = "isa_ConstantFP"
external isAConstantInt            : Llvm.llvalue -> bool = "isa_ConstantInt"
external isAConstantPointerNull    : Llvm.llvalue -> bool = "isa_ConstantPointerNull"
external isAConstantStruct         : Llvm.llvalue -> bool = "isa_ConstantStruct"
external isAConstantVector         : Llvm.llvalue -> bool = "isa_ConstantVector"
external isAGlobalValue            : Llvm.llvalue -> bool = "isa_GlobalValue"
external isAFunction               : Llvm.llvalue -> bool = "isa_Function"
external isAGlobalAlias            : Llvm.llvalue -> bool = "isa_GlobalAlias"
external isAGlobalVariable         : Llvm.llvalue -> bool = "isa_GlobalVariable"
external isAUndefValue             : Llvm.llvalue -> bool = "isa_UndefValue"
external isAInstruction            : Llvm.llvalue -> bool = "isa_Instruction"
external isABinaryOperator         : Llvm.llvalue -> bool = "isa_BinaryOperator"
external isACallInst               : Llvm.llvalue -> bool = "isa_CallInst"
external isAIntrinsicInst          : Llvm.llvalue -> bool = "isa_IntrinsicInst"
external isADbgInfoIntrinsic       : Llvm.llvalue -> bool = "isa_DbgInfoIntrinsic"
external isADbgDeclareInst         : Llvm.llvalue -> bool = "isa_DbgDeclareInst"
external isAMemIntrinsic           : Llvm.llvalue -> bool = "isa_MemIntrinsic"
external isAMemCpyInst             : Llvm.llvalue -> bool = "isa_MemCpyInst"
external isAMemMoveInst            : Llvm.llvalue -> bool = "isa_MemMoveInst"
external isAMemSetInst             : Llvm.llvalue -> bool = "isa_MemSetInst"
external isACmpInst                : Llvm.llvalue -> bool = "isa_CmpInst"
external isAFCmpInst               : Llvm.llvalue -> bool = "isa_FCmpInst"
external isAICmpInst               : Llvm.llvalue -> bool = "isa_ICmpInst"
external isAExtractElementInst     : Llvm.llvalue -> bool = "isa_ExtractElementInst"
external isAGetElementPtrInst      : Llvm.llvalue -> bool = "isa_GetElementPtrInst"
external isAInsertElementInst      : Llvm.llvalue -> bool = "isa_InsertElementInst"
external isAInsertValueInst        : Llvm.llvalue -> bool = "isa_InsertValueInst"
external isALandingPadInst         : Llvm.llvalue -> bool = "isa_LandingPadInst"
external isAPHINode                : Llvm.llvalue -> bool = "isa_PHINode"
external isASelectInst             : Llvm.llvalue -> bool = "isa_SelectInst"
external isAShuffleVectorInst      : Llvm.llvalue -> bool = "isa_ShuffleVectorInst"
external isAStoreInst              : Llvm.llvalue -> bool = "isa_StoreInst"
external isATerminatorInst         : Llvm.llvalue -> bool = "isa_TerminatorInst"
external isABranchInst             : Llvm.llvalue -> bool = "isa_BranchInst"
external isAIndirectBrInst         : Llvm.llvalue -> bool = "isa_IndirectBrInst"
external isAInvokeInst             : Llvm.llvalue -> bool = "isa_InvokeInst"
external isAReturnInst             : Llvm.llvalue -> bool = "isa_ReturnInst"
external isASwitchInst             : Llvm.llvalue -> bool = "isa_SwitchInst"
external isAUnreachableInst        : Llvm.llvalue -> bool = "isa_UnreachableInst"
external isAResumeInst             : Llvm.llvalue -> bool = "isa_ResumeInst"
external isAUnaryInstruction       : Llvm.llvalue -> bool = "isa_UnaryInstruction"
external isAFenceInst              : Llvm.llvalue -> bool = "isa_FenceInst"
external isAAtomicCmpXchgInst      : Llvm.llvalue -> bool = "isa_AtomicCmpXchgInst"
external isAAtomicRMWInst          : Llvm.llvalue -> bool = "isa_AtomicRMWInst"
external isAAllocaInst             : Llvm.llvalue -> bool = "isa_AllocaInst"
external isACastInst               : Llvm.llvalue -> bool = "isa_CastInst"
external isABitCastInst            : Llvm.llvalue -> bool = "isa_BitCastInst"
external isAFPExtInst              : Llvm.llvalue -> bool = "isa_FPExtInst"
external isAFPToSIInst             : Llvm.llvalue -> bool = "isa_FPToSIInst"
external isAFPToUIInst             : Llvm.llvalue -> bool = "isa_FPToUIInst"
external isAFPTruncInst            : Llvm.llvalue -> bool = "isa_FPTruncInst"
external isAIntToPtrInst           : Llvm.llvalue -> bool = "isa_IntToPtrInst"
external isAPtrToIntInst           : Llvm.llvalue -> bool = "isa_PtrToIntInst"
external isASExtInst               : Llvm.llvalue -> bool = "isa_SExtInst"
external isASIToFPInst             : Llvm.llvalue -> bool = "isa_SIToFPInst"
external isATruncInst              : Llvm.llvalue -> bool = "isa_TruncInst"
external isAUIToFPInst             : Llvm.llvalue -> bool = "isa_UIToFPInst"
external isAZExtInst               : Llvm.llvalue -> bool = "isa_ZExtInst"
external isAExtractValueInst       : Llvm.llvalue -> bool = "isa_ExtractValueInst"
external isALoadInst               : Llvm.llvalue -> bool = "isa_LoadInst"
external isAVAArgInst              : Llvm.llvalue -> bool = "isa_VAArgInst"
let hasName v =
  not(isAMDString v) && ("" <> Llvm.value_name v)
let variable slots v =
  let is_global = isAGlobalValue v in
  let value_name = Llvm.value_name v in
  let ty = cvt_oType (Llvm.type_of v) in
  if value_name <> "" then (is_global, value_name, ty) else
  if is_global then (is_global, slots#get_module_slot v, ty) else
  (is_global, slots#get_local_slot v, ty)
let operand_list v =
  let result = ref [] in
  for j = (Llvm.num_operands v)-1 downto 0 do
    result := (Llvm.operand v j)::!result
  done;
  !result

let rec cvt_typed slots         v = (cvt_oType (Llvm.type_of v), cvt_oValue slots v)
and cvt_oValue slots            v =
match Llvm.classify_value v with
  | Llvm.ValueKind.NullValue             -> NullValue
  | Llvm.ValueKind.Argument              -> Argument(variable slots v)
  | Llvm.ValueKind.BasicBlock            -> BasicBlock(variable slots v)
  | Llvm.ValueKind.InlineAsm             -> InlineAsm
  | Llvm.ValueKind.MDNode                -> MDNode
  | Llvm.ValueKind.MDString              -> MDString(match Llvm.get_mdstring v with None -> "" | Some x -> x)
  | Llvm.ValueKind.BlockAddress          -> BlockAddress(cvt_oValue slots (Llvm.block_parent (Llvm.block_of_value v)))
  | Llvm.ValueKind.ConstantAggregateZero -> ConstantAggregateZero
  | Llvm.ValueKind.ConstantArray         -> ConstantArray(List.map (cvt_oValue slots) (operand_list v))
  | Llvm.ValueKind.ConstantDataArray     -> 
      let len = getNumElements v in
      let ops = ref [] in
      for i = len - 1 downto 0 do
        ops := (getElementAsConstant v i) :: !ops
      done;
      ConstantDataArray(List.map (cvt_oValue slots) !ops)
  | Llvm.ValueKind.ConstantDataVector    -> ConstantDataVector(List.map (cvt_oValue slots) (operand_list v))
  | Llvm.ValueKind.ConstantExpr          -> ConstantExpr(Llvm.constexpr_opcode v,
                                                         List.map (cvt_typed slots) (operand_list v))
  | Llvm.ValueKind.ConstantFP            -> ConstantFP
  | Llvm.ValueKind.ConstantInt           -> ConstantInt(Llvm.integer_bitwidth (Llvm.type_of v),Llvm.int64_of_const v)
  | Llvm.ValueKind.ConstantPointerNull   -> ConstantPointerNull(bitwidth(cvt_oType (Llvm.type_of v)))
  | Llvm.ValueKind.ConstantStruct        -> ConstantStruct(Llvm.is_packed (Llvm.type_of v),
                                                           List.map (cvt_oValue slots) (operand_list v))
  | Llvm.ValueKind.ConstantVector        -> ConstantVector(List.map (cvt_oValue slots) (operand_list v))
  | Llvm.ValueKind.Function              -> Function(variable slots v)
  | Llvm.ValueKind.GlobalAlias           -> GlobalAlias(variable slots v)
  | Llvm.ValueKind.GlobalVariable        -> GlobalVariable(variable slots v)
  | Llvm.ValueKind.UndefValue            -> UndefValue
  | Llvm.ValueKind.Instruction _         -> Variable(variable slots v) (* don't cvt_oInstruction, that will be called separately *)
and cvt_oInstruction slots      v =
let ty = cvt_oType (Llvm.type_of v) in
let ops = List.map (cvt_typed slots) (operand_list v) in
if isABinaryOperator            v then BinaryOperator(ty,ops,Llvm.instr_opcode v) else
if isACallInst                  v then
  let operands = operand_list v in
  (match List.rev operands with
  | [] -> raise (Invalid_argument "call instruction without callee")
  | callee::rev_operands ->
      let callee_ty = cvt_oType (Llvm.type_of callee) in
      (match callee_ty with
        Pointer(_,FunctionTy _) -> ()
      | _ -> raise(Invalid_argument "impossible callee type"));
      (* let attrs = Llvm.function_attr callee in (* TODO *) *)
      (* TODO: return attributes *)
      CallInst(Llvm.is_tail_call v,
               Llvm.function_call_conv v,
               callee_ty,
               variable slots callee,
               (List.rev (List.map (cvt_typed slots) rev_operands)))) else
if isACmpInst                   v then (cvt_oCmpInst slots v) else
if isAExtractElementInst        v then ExtractElementInst(ty,ops) else
if isAGetElementPtrInst         v then GetElementPtrInst(ty,ops) else
if isAInsertElementInst         v then InsertElementInst(ty,ops) else
if isAInsertValueInst           v then InsertValueInst(ty,ops) else
if isALandingPadInst            v then LandingPadInst(ty,ops) else
if isAPHINode                   v then PHINode(cvt_oType (Llvm.type_of v),
                                               List.map
                                                 (fun (v',block) ->
                                                   (cvt_oValue slots v', variable slots (Llvm.value_of_block block)))
                                                 (Llvm.incoming v)) else
if isASelectInst                v then SelectInst(ty,ops) else
if isAShuffleVectorInst         v then ShuffleVectorInst(ty,ops) else
if isAStoreInst                 v then StoreInst(get_alignment v,ty,ops) else
if isATerminatorInst            v then (cvt_oTerminatorInst slots v) else
if isAUnaryInstruction          v then (cvt_oUnaryInstruction slots v) else
if isAFenceInst v then FenceInst(ty,ops) else
if isAAtomicCmpXchgInst v then AtomicCmpXchgInst(ty,ops) else
if isAAtomicRMWInst v then AtomicRMWInst(ty,ops) else
failwith "cvt_oInstruction"
and cvt_oCallInst slots         v =
if isAIntrinsicInst             v then IntrinsicInst(cvt_oIntrinsicInst slots v) else
failwith "cvt_oCallInst"
and cvt_oIntrinsicInst slots    v =
if isADbgInfoIntrinsic          v then DbgInfoIntrinsic(cvt_oDbgInfoIntrinsic slots v) else
if isAMemIntrinsic              v then MemIntrinsic(cvt_oMemIntrinsic slots v) else
failwith "cvt_oIntrinsicInst"
and cvt_oDbgInfoIntrinsic slots v =
if isADbgDeclareInst            v then DbgDeclareInst else
failwith "cvt_oDbgInfoIntrinsic"
and cvt_oMemIntrinsic slots     v =
if isAMemCpyInst                v then MemCpyInst else
if isAMemMoveInst               v then MemMoveInst else
if isAMemSetInst                v then MemSetInst else
failwith "cvt_oMemIntrinsic"
and cvt_oCmpInst slots          v =
if isAFCmpInst                  v then FCmpInst(cvt_oType (Llvm.type_of v),
                                                List.map (cvt_typed slots) (operand_list v)) else
if isAICmpInst                  v then ICmpInst(Llvm.icmp_predicate v,
                                                List.map (cvt_typed slots) (operand_list v)) else
failwith "cvt_oCmpInst"
and cvt_oTerminatorInst slots   v =
let ty = cvt_oType (Llvm.type_of v) in
let ops = List.map (cvt_typed slots) (operand_list v) in
if isABranchInst                v then BranchInst(ty,ops) else
if isAIndirectBrInst            v then IndirectBrInst(ty,ops) else
if isAInvokeInst                v then InvokeInst(ty,ops) else
if isAReturnInst                v then ReturnInst(ty,ops) else
if isASwitchInst                v then SwitchInst(ty,ops) else
if isAUnreachableInst           v then UnreachableInst(ty,ops) else
if isAResumeInst                v then ResumeInst(ty,ops) else
failwith "cvt_oTerminatorInst"
and cvt_oUnaryInstruction slots v =
let ty = cvt_oType (Llvm.type_of v) in
let ops = List.map (cvt_typed slots) (operand_list v) in
if isAAllocaInst                v then AllocaInst(get_alignment v,ty,ops) else
if isACastInst                  v then (cvt_oCastInst slots v) else
if isAExtractValueInst          v then ExtractValueInst(ty,ops) else
if isALoadInst                  v then LoadInst(get_alignment v,ty,ops) else
if isAVAArgInst                 v then VAArgInst(ty,ops) else
failwith "cvt_oUnaryInstruction"
and cvt_oCastInst slots         v =
let ty = cvt_oType (Llvm.type_of v) in
let ops = List.map (cvt_typed slots) (operand_list v) in
if isABitCastInst               v then BitCastInst(ty,ops) else
if isAFPExtInst                 v then FPExtInst(ty,ops) else
if isAFPToSIInst                v then FPToSIInst(ty,ops) else
if isAFPToUIInst                v then FPToUIInst(ty,ops) else
if isAFPTruncInst               v then FPTruncInst(ty,ops) else
if isAIntToPtrInst              v then IntToPtrInst(ty,ops) else
if isAPtrToIntInst              v then PtrToIntInst(ty,ops) else
if isASExtInst                  v then SExtInst(ty,ops) else
if isASIToFPInst                v then SIToFPInst(ty,ops) else
if isATruncInst                 v then TruncInst(ty,ops) else
if isAUIToFPInst                v then UIToFPInst(ty,ops) else
if isAZExtInst                  v then ZExtInst(ty,ops) else
failwith "cvt_oCastInst"

let slots md =
  (* NB: omits slots for metadata, must add if we use metadata, see AsmWriter.cpp *)
  let post_incr x = let n = !x in x := n+1; string_of_int n in
  let mMap = Hashtbl.create 11 in
  let mNext = ref 0 in
  let create_module_slot x =
    if ("" = Llvm.value_name x)
    then Hashtbl.add mMap x (post_incr mNext) in
  Llvm.iter_globals create_module_slot md;
  Llvm.iter_functions create_module_slot md;
  let lMap = Hashtbl.create 11 in
  let lNext = ref 0 in
  let create_function_slot x =
    if ("" = Llvm.value_name x)
    then Hashtbl.add lMap x (post_incr lNext) in
  Llvm.iter_functions
    (fun f ->
      lNext := 0;
      Llvm.iter_params create_function_slot f;
      Llvm.iter_blocks
        (fun b ->
          create_function_slot (Llvm.value_of_block b);
          Llvm.iter_instrs
            (fun i ->
              match Llvm.classify_type (Llvm.type_of i) with
                Llvm.TypeKind.Void -> ()
              | _ -> create_function_slot i)
            b)
        f;
      ())
    md;
  object
      method get_md = md
      method get_module_slot v = Hashtbl.find mMap v
      method get_local_slot v = Hashtbl.find lMap v
  end

let read_object_file f =
  let ctx = Llvm.create_context() in
  let mbuf = Llvm.MemoryBuffer.of_file f in
  let md = Llvm_bitreader.parse_bitcode ctx mbuf in
  slots md

let get_block slots v =
  let b_name = variable slots (Llvm.value_of_block v) in
  let b_instrs =
    let x = ref [] in
    Llvm.iter_instrs
      (fun i ->
        let nopt =
          let n = Llvm.value_name i in
          let ty = cvt_oType (Llvm.type_of i) in
          if ""<>n then Some(false,n,ty) else
          (try Some(false,slots#get_local_slot i,ty)
          with _ -> None) in
        x := (nopt,cvt_oInstruction slots i)::!x)
      v;
    List.rev !x in
  { b_name; b_instrs }

let get_function slots v =
  (* TODO: missing AnnotationWriter *)
  (* TODO: missing Materializable *)
  let f_name = variable slots v in
  let f_is_declaration = Llvm.is_declaration v in
  let f_linkage = Llvm.linkage v in
  let f_visibility = Llvm.visibility v in
  let f_call_conv = Llvm.function_call_conv v in
  let f_type = cvt_oType (Llvm.type_of v) in
  (* TODO: missing return attributes *)
  let f_params =
    let x = ref [] in
    Llvm.iter_params
      (fun p ->
        let p_name =
          match Llvm.value_name p with
          | "" -> "" (* NEED SLOT LOOKUP?? *)
          | n -> n in
        let p_type = cvt_oType (Llvm.type_of p) in
        let p_attrs = Llvm.param_attr p in
        x := ((false,p_name,p_type),p_attrs) :: !x)
      v;
    List.rev !x in
  let f_attrs = List.rev(Llvm.function_attr v) in
  let f_blocks =
    let x = ref [] in
    Llvm.iter_blocks
      (fun b -> x := (get_block slots b)::!x)
      v;
    List.rev !x in
  { f_name; f_is_declaration; f_linkage; f_visibility; f_call_conv; f_type; f_params; f_attrs; f_blocks; }

let get_module file =
  let slots = read_object_file file in
  let globals = ref [] in
  let get_global v =
    let g_name = variable slots v in
    let g_ty = cvt_oType (Llvm.type_of v) in
    let g_val =
      (* TODO: handle no initializer case *)
      cvt_oValue slots (Llvm.global_initializer v) in
    let g_linkage = Llvm.linkage v in
    let g_visibility = Llvm.visibility v in
    let g_is_constant = Llvm.is_global_constant v in
    globals := { g_name; g_ty; g_val; g_linkage; g_visibility; g_is_constant }::!globals in
  let funcs = ref [] in
  let get_function v =
    funcs := (get_function slots v)::!funcs in
  Llvm.iter_globals get_global slots#get_md;
  Llvm.iter_functions get_function slots#get_md;
  { globals=List.rev !globals; functions=List.rev !funcs }
;;

let bprintf = Printf.bprintf;;
let bpr_EscapedString b s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    let isprint c = c >= '\032' && c < '\128' in
    if (isprint c && c <> '\\' && c <> '\"') then
      bprintf b "%c" c
    else
      bprintf b "\\%x" (Char.code c)
  done
let pad_to_column b n =
  let rec column i =
    if i<=0 then 0 else
    if Buffer.nth b (i-1) = '\n' then 0 else
    1 + column (i-1) in
  let column = column (Buffer.length b) in
  for x = column to n-1 do
    Buffer.add_char b ' '
  done
let bpr_concat sep bpr b =
  let rec loop = function
  | [] -> ()
  | [hd] -> bpr b hd
  | hd::tl ->
      bprintf b "%a%s" bpr hd sep;
      loop tl in
  loop

let bpr_call_conv b x =
  if x = Llvm.CallConv.c            then bprintf b "" else
  if x = Llvm.CallConv.fast         then bprintf b "fastcc " else
  if x = Llvm.CallConv.cold         then bprintf b "coldcc " else
  if x = Llvm.CallConv.x86_stdcall  then bprintf b "x86_stdcallcc " else
  if x = Llvm.CallConv.x86_fastcall then bprintf b "x86_fastcallcc " else
  (* TODO: missing a bunch, see AsmWriter.cpp *)
  bprintf b "cc%d " x

let bpr_attr b = function
  | Llvm.Attribute.Zext             -> bprintf b "zeroext"
  | Llvm.Attribute.Sext             -> bprintf b "signext"
  | Llvm.Attribute.Noreturn         -> bprintf b "noreturn"
  | Llvm.Attribute.Inreg            -> bprintf b "inreg"
  | Llvm.Attribute.Structret        -> bprintf b "sret"
  | Llvm.Attribute.Nounwind         -> bprintf b "nounwind"
  | Llvm.Attribute.Noalias          -> bprintf b "noalias"
  | Llvm.Attribute.Byval            -> bprintf b "byval"
  | Llvm.Attribute.Nest             -> bprintf b "nest"
  | Llvm.Attribute.Readnone         -> bprintf b "readnone"
  | Llvm.Attribute.Readonly         -> bprintf b "readonly"
  | Llvm.Attribute.Noinline         -> bprintf b "noinline"
  | Llvm.Attribute.Alwaysinline     -> bprintf b "alwaysinline"
  | Llvm.Attribute.Optsize          -> bprintf b "optsize"
  | Llvm.Attribute.Ssp              -> bprintf b "ssp"
  | Llvm.Attribute.Sspreq           -> bprintf b "sspreq"
  | Llvm.Attribute.Nocapture        -> bprintf b "nocapture"
  | Llvm.Attribute.Noredzone        -> bprintf b "noredzone"
  | Llvm.Attribute.Noimplicitfloat  -> bprintf b "noimplicitfloat"
  | Llvm.Attribute.Naked            -> bprintf b "naked"
  | Llvm.Attribute.Inlinehint       -> bprintf b "inlinehint"
  | Llvm.Attribute.ReturnsTwice     -> bprintf b "returns_twice"
  | Llvm.Attribute.UWTable          -> bprintf b "uwtable"
  | Llvm.Attribute.NonLazyBind      -> bprintf b "nonlazybind"
  | Llvm.Attribute.Alignment x      -> bprintf b "align %d" x
  | Llvm.Attribute.Stackalignment x -> bprintf b "alignstack(%d)" x

let bpr_typekind b = function
| Llvm.TypeKind.Void      -> bprintf b "Void"
| Llvm.TypeKind.Half      -> bprintf b "Half"
| Llvm.TypeKind.Float     -> bprintf b "Float"
| Llvm.TypeKind.Double    -> bprintf b "Double"
| Llvm.TypeKind.X86fp80   -> bprintf b "X86fp80"
| Llvm.TypeKind.X86_mmx   -> bprintf b "X86_mmx"
| Llvm.TypeKind.Fp128     -> bprintf b "Fp128"
| Llvm.TypeKind.Ppc_fp128 -> bprintf b "Ppc_fp128"
| Llvm.TypeKind.Label     -> bprintf b "Label"
| Llvm.TypeKind.Integer   -> bprintf b "Integer"
| Llvm.TypeKind.Function  -> bprintf b "Function"
| Llvm.TypeKind.Struct    -> bprintf b "Struct"
| Llvm.TypeKind.Array     -> bprintf b "Array"
| Llvm.TypeKind.Pointer   -> bprintf b "Pointer"
| Llvm.TypeKind.Vector    -> bprintf b "Vector"
| Llvm.TypeKind.Metadata  -> bprintf b "Metadata"

let bpr_icmp b = function
| Llvm.Icmp.Eq  -> bprintf b "eq"
| Llvm.Icmp.Ne  -> bprintf b "ne"
| Llvm.Icmp.Ugt -> bprintf b "ugt"
| Llvm.Icmp.Uge -> bprintf b "uge"
| Llvm.Icmp.Ult -> bprintf b "ult"
| Llvm.Icmp.Ule -> bprintf b "ule"
| Llvm.Icmp.Sgt -> bprintf b "sgt"
| Llvm.Icmp.Sge -> bprintf b "sge"
| Llvm.Icmp.Slt -> bprintf b "slt"
| Llvm.Icmp.Sle -> bprintf b "sle"

let bpr_opcode b = function
| Llvm.Opcode.Invalid        -> bprintf b "invalid"
| Llvm.Opcode.Ret            -> bprintf b "ret"
| Llvm.Opcode.Br             -> bprintf b "br"
| Llvm.Opcode.Switch         -> bprintf b "switch"
| Llvm.Opcode.IndirectBr     -> bprintf b "indirectbr"
| Llvm.Opcode.Invoke         -> bprintf b "invoke"
| Llvm.Opcode.Invalid2       -> bprintf b "invalid2"
| Llvm.Opcode.Unreachable    -> bprintf b "unreachable"
| Llvm.Opcode.Add            -> bprintf b "add"
| Llvm.Opcode.FAdd           -> bprintf b "fadd"
| Llvm.Opcode.Sub            -> bprintf b "sub"
| Llvm.Opcode.FSub           -> bprintf b "fsub"
| Llvm.Opcode.Mul            -> bprintf b "mul"
| Llvm.Opcode.FMul           -> bprintf b "fmul"
| Llvm.Opcode.UDiv           -> bprintf b "udiv"
| Llvm.Opcode.SDiv           -> bprintf b "sdiv"
| Llvm.Opcode.FDiv           -> bprintf b "fdiv"
| Llvm.Opcode.URem           -> bprintf b "urem"
| Llvm.Opcode.SRem           -> bprintf b "srem"
| Llvm.Opcode.FRem           -> bprintf b "frem"
| Llvm.Opcode.Shl            -> bprintf b "shl"
| Llvm.Opcode.LShr           -> bprintf b "lshr"
| Llvm.Opcode.AShr           -> bprintf b "ashr"
| Llvm.Opcode.And            -> bprintf b "and"
| Llvm.Opcode.Or             -> bprintf b "or"
| Llvm.Opcode.Xor            -> bprintf b "xor"
| Llvm.Opcode.Alloca         -> bprintf b "alloca"
| Llvm.Opcode.Load           -> bprintf b "load"
| Llvm.Opcode.Store          -> bprintf b "store"
| Llvm.Opcode.GetElementPtr  -> bprintf b "getelementptr"
| Llvm.Opcode.Trunc          -> bprintf b "trunc"
| Llvm.Opcode.ZExt           -> bprintf b "zext"
| Llvm.Opcode.SExt           -> bprintf b "sext"
| Llvm.Opcode.FPToUI         -> bprintf b "fptoui"
| Llvm.Opcode.FPToSI         -> bprintf b "fptosi"
| Llvm.Opcode.UIToFP         -> bprintf b "uitofp"
| Llvm.Opcode.SIToFP         -> bprintf b "sitofp"
| Llvm.Opcode.FPTrunc        -> bprintf b "fptrunc"
| Llvm.Opcode.FPExt          -> bprintf b "fpext"
| Llvm.Opcode.PtrToInt       -> bprintf b "ptrtoint"
| Llvm.Opcode.IntToPtr       -> bprintf b "inttoptr"
| Llvm.Opcode.BitCast        -> bprintf b "bitcast"
| Llvm.Opcode.ICmp           -> bprintf b "icmp"
| Llvm.Opcode.FCmp           -> bprintf b "fcmp"
| Llvm.Opcode.PHI            -> bprintf b "phi"
| Llvm.Opcode.Call           -> bprintf b "call"
| Llvm.Opcode.Select         -> bprintf b "select"
| Llvm.Opcode.UserOp1        -> bprintf b "userop1"
| Llvm.Opcode.UserOp2        -> bprintf b "userop2"
| Llvm.Opcode.VAArg          -> bprintf b "vaarg"
| Llvm.Opcode.ExtractElement -> bprintf b "extractelement"
| Llvm.Opcode.InsertElement  -> bprintf b "insertelement"
| Llvm.Opcode.ShuffleVector  -> bprintf b "shufflevector"
| Llvm.Opcode.ExtractValue   -> bprintf b "extractvalue"
| Llvm.Opcode.InsertValue    -> bprintf b "insertvalue"
| Llvm.Opcode.Fence          -> bprintf b "fence"
| Llvm.Opcode.AtomicCmpXchg  -> bprintf b "atomiccmpxchg"
| Llvm.Opcode.AtomicRMW      -> bprintf b "atomicrmw"
| Llvm.Opcode.Resume         -> bprintf b "resume"
| Llvm.Opcode.LandingPad     -> bprintf b "landingpad"
(*| Llvm.Opcode.Unwind         -> bprintf b "unwind"*)

(* TODO: use quotes and escaping if necessary, see PrintLLVMName *)
let bpr_name b n prefix =
  bprintf b "%s%s" prefix n
let bpr_local_name b n =
  bprintf b "%%%s" n
let bpr_global_name b n =
  bprintf b "@%s" n

let bpr_oType b ty =
  let rec pr = function
    | Void      -> bprintf b "void"
    | Half      -> bprintf b "half"
    | Float     -> bprintf b "float"
    | Double    -> bprintf b "double"
    | X86fp80   -> bprintf b "x86_fp80"
    | X86_mmx   -> bprintf b "x86_mmx"
    | Fp128     -> bprintf b "fp128"
    | Ppc_fp128 -> bprintf b "ppc_fp128"
    | Label     -> bprintf b "label"
    | Metadata  -> bprintf b "metadata"
(* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
    | Integer x ->
        bprintf b "i%d" x
    | FunctionTy(return_ty, param_tys, is_var_arg) ->
        pr return_ty;
        bprintf b " (";
        (match param_tys with
        | [] ->
            if is_var_arg then bprintf b "..."
        | hd::tl ->
            pr hd;
            List.iter (fun ty2 -> bprintf b ", "; pr ty2) tl;
            if is_var_arg then bprintf b ", ...");
        bprintf b ")"
    | Struct nopt ->
        (* TODO: missing isLiteral *)
        (match nopt with
        | Some n -> bpr_local_name b n
        | None -> bprintf b "<some struct type>"
        )
    | Array(len,element_ty) ->
        bprintf b "[%d x " len;
        pr element_ty;
        bprintf b "]"
    | Pointer(address_space,element_ty) ->
        pr element_ty;
        if 0<>address_space then bprintf b " addrspace(%d)" address_space;
        bprintf b "*"
    | Vector(len,element_ty) ->
        bprintf b "<%d x " len;
        pr element_ty;
        bprintf b ">"
  in pr ty

let pr_ty = false (*true for debugging, won't work for circuit output*)
let bpr_variable b (is_global,name,ty) =
  if pr_ty then
    if is_global then bprintf b "(@%s:%a)" name bpr_oType ty else bprintf b "(%%%s:%a)" name bpr_oType ty
  else
    if is_global then bprintf b "@%s" name else bprintf b "%%%s" name
let rec bpr_oValue b op = match op with
  | Variable x          -> bpr_variable b x
(*  if Llvm.is_constant v && not(Llvm.is_global_constant v) then bpr_oConstant b v else *)
  | NullValue -> bprintf b "NULL"
  | ConstantInt(bitwidth,value) ->
      (match value with
        None -> bprintf b "%%-1"
      | Some x ->
          if 1=bitwidth
          then if x<>Int64.zero then bprintf b "true" else bprintf b "false"
          else bprintf b "%s" (Int64.to_string x))
  | ConstantFP
    -> bprintf b "SOME FP CONSTANT"
  | ConstantAggregateZero
    -> bprintf b "zeroinitializer"
  | BlockAddress op
    ->
      bprintf b "blockaddress(";
      bpr_oValue b op;
      bprintf b ",????";
      (*(writeAsOperandInternal (Llvm.block_parent (Llvm.block_of_value v)))*)
      bprintf b ")"
  | ConstantArray ops ->
      bprintf b "[%a]" (bpr_concat ", " bpr_oValue) ops
  | ConstantVector ops ->
      bprintf b "<%a>" (bpr_concat ", " bpr_oValue) ops
  | ConstantStruct(is_packed,ops) ->
      if is_packed then bprintf b "<";
      bprintf b "{%a}" (bpr_concat ", " bpr_oValue) ops;
      if is_packed then bprintf b ">"
  | ConstantExpr(opcode,ops) ->
      bpr_opcode b opcode;
      (* TODO: WriteOptimizationInfo(Out, CE); *)
      (* TODO:     if (CE->isCompare()) *)
      (* TODO: if (CE->hasIndices()) { *)
      (* TODO: if (CE->isCast()) { *)
      bprintf b " ( %a )" bpr_typed_oValue_list ops
  | ConstantPointerNull _ ->
      bprintf b "null"
  | UndefValue ->
      bprintf b "undef"
  | GlobalAlias var
  | GlobalVariable var
  | Function var ->
      bpr_variable b var
  | ConstantDataArray ops ->
      bprintf b "ConstantDataArray(%a)" (bpr_concat ", " bpr_oValue) ops
  | ConstantDataVector [] ->
      bprintf b "ConstantDataVector(EMPTY)"
  | ConstantDataVector ops ->
      bprintf b "ConstantDataVector(%a)" (bpr_concat ", " bpr_oValue) ops
  | InlineAsm           -> bprintf b "InlineAsm"
  | MDNode              -> bprintf b "MDNode"
  | MDString x          -> bprintf b "!\"%a\"" bpr_EscapedString x
  | Argument x          -> bpr_variable b x
  | BasicBlock x        -> bpr_variable b x
  | Instruction _ ->
      failwith "bpr_oValue" (* our cvt functions never produce this *)

and bpr_typed_oValue b (ty,op) =
  bprintf b "%a %a" bpr_oType ty bpr_oValue op

and bpr_typed_oValue_list b l =
  (match l with [] -> ()
  | (ty,op)::tl ->
      let all_same_type = List.for_all (fun (ty',_) -> ty=ty') tl in
      if all_same_type then begin
        bprintf b "%a %a" bpr_oType ty bpr_oValue op;
        List.iter (fun (_,op) -> bprintf b ", %a" bpr_oValue op) tl
      end else
        bpr_concat ", " (fun b (ty,op) -> bprintf b "%a %a" bpr_oType ty bpr_oValue op) b l)

let is_cast = function
  | Llvm.Opcode.PtrToInt
  | Llvm.Opcode.IntToPtr
  | Llvm.Opcode.FPTrunc
  | Llvm.Opcode.FPExt
  | Llvm.Opcode.FPToUI
  | Llvm.Opcode.FPToSI
  | Llvm.Opcode.UIToFP
  | Llvm.Opcode.SIToFP
  | Llvm.Opcode.Trunc
  | Llvm.Opcode.BitCast
  | Llvm.Opcode.ZExt
  | Llvm.Opcode.SExt -> true
  | _ -> false

let bpr_instr b (nopt,i) =
  (* TODO: handle atomic/volatile *)
  (* TODO: WriteOptimizationInfo *)
  (* TODO: writeAtomicRMWOperation *)
  bprintf b "  ";
  (match nopt with None -> ()
  | Some n -> bprintf b "%a = " bpr_variable n);
  (match i with
  | CallInst(is_tail_call, callconv, callee_ty, callee_name, operands) ->
      (* TODO: is_var_arg *)
      if is_tail_call then bprintf b "tail ";
      bprintf b "call ";
      bpr_call_conv b callconv;
      (match callee_ty with
      | Pointer(_,FunctionTy(rt,_,false)) -> (* false=>not is_var_arg *)
          bprintf b "%a %a(%a)\n" bpr_oType rt bpr_variable callee_name bpr_typed_oValue_list operands
      | _ ->
          bprintf b "%a %a(%a)\n" bpr_oType callee_ty bpr_variable callee_name bpr_typed_oValue_list operands)
  | ICmpInst(pred, ops) ->
      bprintf b "icmp ";
      (match pred with None -> ()
      | Some icmp -> bprintf b "%a " bpr_icmp icmp);
      bprintf b "%a\n" bpr_typed_oValue_list ops
  | PHINode(ty, incoming) ->
      bprintf b "phi %a " bpr_oType ty;
      let first = ref true in
      List.iter
        (fun (op,block_name) ->
          if not !first then bprintf b ", " else first := false;
          bprintf b "[ %a, %a ]" bpr_oValue op bpr_variable block_name
        )
        incoming;
      bprintf b "\n"
  | BranchInst(ty, ops)
  | IndirectBrInst(ty, ops) ->
      (match ops with
      | [x;y;z] ->
          bprintf b "br %a\n" bpr_typed_oValue_list [x;z;y] (* NB swap order of y,z *)
            (* LLVM prints the true target first and the false target second.
               But in the abstract syntax the false target comes first, and the true target comes second. *)
      | _ ->
          bprintf b "br %a\n" bpr_typed_oValue_list ops)
  | AssignInst(result_ty,[(ty,op)]) ->
      bprintf b "%a (%a:%a)\n" bpr_oType result_ty bpr_oValue op bpr_oType ty
  | (AllocaInst(a,result_ty,[(ty,op)])) ->
      bprintf b "alloca %a (%a:%a)" bpr_oType (getElementType result_ty) bpr_oValue op bpr_oType ty;
      if (a>0) then bprintf b ", align %d" a;
      bprintf b "\n"
  | (LoadInst(a,result_ty,[(ty,op)])) ->
      bprintf b "load %a (%a:%a)" bpr_oType result_ty bpr_oValue op bpr_oType ty;
      if (a>0) then bprintf b ", align %d" a;
      bprintf b "\n"
  | x ->
      let ty = ty_of_instruction x in
      let ops = ops_of_instruction x in
      let opcode = opcode_of_instruction x in
      if is_cast opcode then
        bprintf b "%a %a to %a\n" bpr_opcode opcode bpr_typed_oValue_list ops bpr_oType ty
      else
        bprintf b "%a %a\n" bpr_opcode opcode bpr_typed_oValue_list ops)

let bpr_block b block =
(*  if block.b_name <> "0" then*)
    (* TODO: predecessor blocks *)
  bprintf b "; <label>:%s%a\n" (let (_,x,_) = block.b_name in x) pad_to_column 50;
  List.iter (bpr_instr b) block.b_instrs;
  bprintf b "\n"

let bpr_linkage b = function
  | Llvm.Linkage.Private                 -> bprintf b "private "
  | Llvm.Linkage.Linker_private          -> bprintf b "linker_private "
(* TODO: missing linker_private-weak, linker_private_weak_def_auto *)
  | Llvm.Linkage.Internal                -> bprintf b "internal "
  | Llvm.Linkage.Link_once               -> bprintf b "link_once "
  | Llvm.Linkage.Link_once_odr           -> bprintf b "link_once_odr "
  | Llvm.Linkage.Link_once_odr_auto_hide -> bprintf b "link_once_odr_auto_hide "
  | Llvm.Linkage.Linker_private_weak     -> bprintf b "linker_private_weak "
  | Llvm.Linkage.Weak                    -> bprintf b "weak "
  | Llvm.Linkage.Weak_odr                -> bprintf b "weak_odr "
  | Llvm.Linkage.Common                  -> bprintf b "common "
  | Llvm.Linkage.Appending               -> bprintf b "appending "
  | Llvm.Linkage.Dllimport               -> bprintf b "dllimport "
  | Llvm.Linkage.Dllexport               -> bprintf b "dllexport "
  | Llvm.Linkage.External_weak           -> bprintf b "extern_weak "
  | Llvm.Linkage.Available_externally    -> bprintf b "available_externally "
  | Llvm.Linkage.External                -> bprintf b "" (* !! see PrintLinkage !! *)
  | Llvm.Linkage.Ghost                   -> bprintf b "" (* !! see PrintLinkage !! *)

let bpr_visibility b = function
  | Llvm.Visibility.Default   -> bprintf b "" (* !! see PrintVisibility !! *)
  | Llvm.Visibility.Hidden    -> bprintf b "hidden "
  | Llvm.Visibility.Protected -> bprintf b "protected "

let bpr_argument b (var,attrs) =
  let (_,_,ty) = var in
  bprintf b "%a " bpr_oType ty;
  List.iter (fun a -> bprintf b "%a " bpr_attr a) attrs;
  bpr_variable b var

let bpr_function b {f_name; f_is_declaration; f_linkage; f_visibility; f_call_conv; f_type; f_params; f_attrs; f_blocks} =
  bprintf b "\n";
  (* TODO: missing AnnotationWriter *)
  (* TODO: missing Materializable *)
  if f_is_declaration then bprintf b "declare " else bprintf b "define ";
  bpr_linkage b f_linkage;
  bpr_visibility b f_visibility;
  bpr_call_conv b f_call_conv;
  (match f_type with
    Pointer(_,FunctionTy(rt,_,_)) ->
      (* TODO: missing return attributes *)
      bpr_oType b rt
  | _ -> raise(Invalid_argument "impossible function type"));
  bprintf b " %a("  bpr_variable f_name;
  bpr_concat ", " bpr_argument b f_params;
  (match f_type with
    Pointer(_,FunctionTy(_,_,is_var_arg)) ->
      if is_var_arg
      then if f_params<>[] then bprintf b ", ..." else bprintf b "..."
  | _ -> raise(Invalid_argument "impossible function type"));
  bprintf b ")";
  List.iter (fun a -> bprintf b " %a" bpr_attr a) f_attrs;
  (* TODO: missing stuff *)
  if f_is_declaration then bprintf b "\n" else begin
    bprintf b " {\n";
    List.iter (bpr_block b) f_blocks;
    bprintf b "}\n";
  end

let bpr_global b {g_name; g_ty; g_val; g_linkage; g_visibility; g_is_constant } =
  bprintf b "%a = %a %a %s %a\n" bpr_variable g_name
    bpr_linkage g_linkage
    bpr_visibility g_visibility
    (if g_is_constant then "constant" else "global")
    bpr_typed_oValue (getElementType g_ty,g_val)

let bpr_module b {globals; functions} =
  List.iter (bpr_global b) globals;
  List.iter (bpr_function b) functions

let string_of_variable var =
  let b = Buffer.create 11 in
  bpr_variable b var;
  Buffer.contents b

module VSet = Set.Make(
  struct
    type t = variable
    let compare = compare
  end)

let rec free_of_variable = function
| (true,_,_) -> VSet.empty (* globals are not free *)
| x -> VSet.singleton x
and free_of_value = function
| Variable x -> VSet.singleton x
| Argument x -> VSet.singleton x
| ConstantStruct(_,ops)
| ConstantVector ops
| ConstantDataArray ops
| ConstantDataVector ops
| ConstantArray ops ->
    List.fold_left VSet.union VSet.empty
      (List.map free_of_value ops)
| ConstantExpr(_,tops) ->
    List.fold_left VSet.union VSet.empty
      (List.map free_of_value (List.map snd tops))
| GlobalAlias slot
| GlobalVariable slot
| Function slot ->
    VSet.empty
    (* VSet.singleton(slot) *)
| BlockAddress _
| NullValue
| ConstantAggregateZero
| ConstantFP
| ConstantInt _
| ConstantPointerNull _
| UndefValue -> VSet.empty
| Instruction _
| BasicBlock _
| InlineAsm
| MDNode
| MDString _     -> VSet.empty

let free_of_instruction i =
  List.fold_left VSet.union VSet.empty
    (List.map free_of_value (List.map snd (ops_of_instruction i)))

let free_of_block bl =
  let rec loop = function
    | [] -> VSet.empty
    | (None,i)::tl ->
        VSet.union
          (free_of_instruction i)
          (loop tl)
    | (Some x,i)::tl ->
        VSet.union
          (free_of_instruction i)
          (VSet.remove x (loop tl)) in
  loop bl.b_instrs

let assigned_of_block bl =
  let rec loop = function
    | [] -> VSet.empty
    | (None,i)::tl -> loop tl
    | (Some x,i)::tl -> VSet.union (VSet.singleton x) (loop tl) in
  loop bl.b_instrs

let ovalue_map g f =
  let h ops con = con (List.map (fun (ty,op) -> (ty, g op)) ops) in
  let imap = function
    | PHINode(ty, x) (*            of oType * (oValue * variable) list *) (* type, (operand x block name) list *)
        (*                      *) -> let ops,vars = List.split x in
        (*                      *)    PHINode(ty, List.combine (List.map g ops) vars)
    | ICmpInst(cmp, ops)           -> h ops (fun ops -> ICmpInst(cmp, ops))
    | BinaryOperator(ty, ops, opc) -> h ops (fun ops -> BinaryOperator     (ty, ops, opc))
    | CallInst  (a, b, ty, v, ops) -> h ops (fun ops -> CallInst           (a, b, ty, v, ops))
    | StoreInst       (a, ty, ops) -> h ops (fun ops -> StoreInst          (a, ty, ops))
    | AllocaInst      (a, ty, ops) -> h ops (fun ops -> AllocaInst         (a, ty, ops))
    | LoadInst        (a, ty, ops) -> h ops (fun ops -> LoadInst           (a, ty, ops))
    | AssignInst         (ty, ops) -> h ops (fun ops -> AssignInst         (ty, ops))
    | FCmpInst           (ty, ops) -> h ops (fun ops -> FCmpInst           (ty, ops))
    | ExtractElementInst (ty, ops) -> h ops (fun ops -> ExtractElementInst (ty, ops))
    | GetElementPtrInst  (ty, ops) -> h ops (fun ops -> GetElementPtrInst  (ty, ops))
    | InsertElementInst  (ty, ops) -> h ops (fun ops -> InsertElementInst  (ty, ops))
    | InsertValueInst    (ty, ops) -> h ops (fun ops -> InsertValueInst    (ty, ops))
    | LandingPadInst     (ty, ops) -> h ops (fun ops -> LandingPadInst     (ty, ops))
    | SelectInst         (ty, ops) -> h ops (fun ops -> SelectInst         (ty, ops))
    | ShuffleVectorInst  (ty, ops) -> h ops (fun ops -> ShuffleVectorInst  (ty, ops))
    | BranchInst         (ty, ops) -> h ops (fun ops -> BranchInst         (ty, ops))
    | IndirectBrInst     (ty, ops) -> h ops (fun ops -> IndirectBrInst     (ty, ops))
    | InvokeInst         (ty, ops) -> h ops (fun ops -> InvokeInst         (ty, ops))
    | ReturnInst         (ty, ops) -> h ops (fun ops -> ReturnInst         (ty, ops))
    | SwitchInst         (ty, ops) -> h ops (fun ops -> SwitchInst         (ty, ops))
    | UnreachableInst    (ty, ops) -> h ops (fun ops -> UnreachableInst    (ty, ops))
    | ResumeInst         (ty, ops) -> h ops (fun ops -> ResumeInst         (ty, ops))
    | BitCastInst        (ty, ops) -> h ops (fun ops -> BitCastInst        (ty, ops))
    | FPExtInst          (ty, ops) -> h ops (fun ops -> FPExtInst          (ty, ops))
    | FPToSIInst         (ty, ops) -> h ops (fun ops -> FPToSIInst         (ty, ops))
    | FPToUIInst         (ty, ops) -> h ops (fun ops -> FPToUIInst         (ty, ops))
    | FPTruncInst        (ty, ops) -> h ops (fun ops -> FPTruncInst        (ty, ops))
    | IntToPtrInst       (ty, ops) -> h ops (fun ops -> IntToPtrInst       (ty, ops))
    | PtrToIntInst       (ty, ops) -> h ops (fun ops -> PtrToIntInst       (ty, ops))
    | SExtInst           (ty, ops) -> h ops (fun ops -> SExtInst           (ty, ops))
    | SIToFPInst         (ty, ops) -> h ops (fun ops -> SIToFPInst         (ty, ops))
    | TruncInst          (ty, ops) -> h ops (fun ops -> TruncInst          (ty, ops))
    | UIToFPInst         (ty, ops) -> h ops (fun ops -> UIToFPInst         (ty, ops))
    | ZExtInst           (ty, ops) -> h ops (fun ops -> ZExtInst           (ty, ops))
    | ExtractValueInst   (ty, ops) -> h ops (fun ops -> ExtractValueInst   (ty, ops))
    | VAArgInst          (ty, ops) -> h ops (fun ops -> VAArgInst          (ty, ops))
    | FenceInst          (ty, ops) -> h ops (fun ops -> FenceInst          (ty, ops))
    | AtomicCmpXchgInst  (ty, ops) -> h ops (fun ops -> AtomicCmpXchgInst  (ty, ops))
    | AtomicRMWInst      (ty, ops) -> h ops (fun ops -> AtomicRMWInst      (ty, ops)) in
  (f.f_blocks <-
    (List.map
      (fun bl ->
        { b_name = bl.b_name;
          b_instrs = List.map (fun (nopt,i) -> (nopt, imap i)) bl.b_instrs;
        })
      f.f_blocks));
  ()

(* currently unused, for future use *)
let ovalue_imap f =
  let rec g = function
    | [] -> ([], [])
    | hd::tl ->
        let (is1, tl') = g tl in
        let is2, hd' =
          (match hd with
            _ -> [], hd
          ) in
        is2@is1, hd'::tl' in
  let h ops con =
    let (is, ops') = g ops in is @ [con ops'] in
  let elim (nopt,i) =
    (match i with
    | PHINode(ty, x) -> (*            of oType * (oValue * variable) list *) (* type, (operand x block name) list *)
        let ops,vars = List.split x in
        h ops (fun ops -> (nopt, PHINode(ty, List.combine ops vars)))
    | ICmpInst(cmp, ops) ->
        let (is, ops') = g ops in is @ [ (nopt, ICmpInst(cmp, ops')) ]
    | BinaryOperator(ty, ops, opc) -> h ops (fun ops -> (nopt, BinaryOperator     (ty, ops, opc)))
    | CallInst  (a, b, ty, v, ops) -> h ops (fun ops -> (nopt, CallInst           (a, b, ty, v, ops)))
    | StoreInst       (a, ty, ops) -> h ops (fun ops -> (nopt, StoreInst          (a, ty, ops)))
    | AllocaInst      (a, ty, ops) -> h ops (fun ops -> (nopt, AllocaInst         (a, ty, ops)))
    | LoadInst        (a, ty, ops) -> h ops (fun ops -> (nopt, LoadInst           (a, ty, ops)))
    | AssignInst         (ty, ops) -> h ops (fun ops -> (nopt, AssignInst         (ty, ops)))
    | FCmpInst           (ty, ops) -> h ops (fun ops -> (nopt, FCmpInst           (ty, ops)))
    | ExtractElementInst (ty, ops) -> h ops (fun ops -> (nopt, ExtractElementInst (ty, ops)))
    | GetElementPtrInst  (ty, ops) -> h ops (fun ops -> (nopt, GetElementPtrInst  (ty, ops)))
    | InsertElementInst  (ty, ops) -> h ops (fun ops -> (nopt, InsertElementInst  (ty, ops)))
    | InsertValueInst    (ty, ops) -> h ops (fun ops -> (nopt, InsertValueInst    (ty, ops)))
    | LandingPadInst     (ty, ops) -> h ops (fun ops -> (nopt, LandingPadInst     (ty, ops)))
    | SelectInst         (ty, ops) -> h ops (fun ops -> (nopt, SelectInst         (ty, ops)))
    | ShuffleVectorInst  (ty, ops) -> h ops (fun ops -> (nopt, ShuffleVectorInst  (ty, ops)))
    | BranchInst         (ty, ops) -> h ops (fun ops -> (nopt, BranchInst         (ty, ops)))
    | IndirectBrInst     (ty, ops) -> h ops (fun ops -> (nopt, IndirectBrInst     (ty, ops)))
    | InvokeInst         (ty, ops) -> h ops (fun ops -> (nopt, InvokeInst         (ty, ops)))
    | ReturnInst         (ty, ops) -> h ops (fun ops -> (nopt, ReturnInst         (ty, ops)))
    | SwitchInst         (ty, ops) -> h ops (fun ops -> (nopt, SwitchInst         (ty, ops)))
    | UnreachableInst    (ty, ops) -> h ops (fun ops -> (nopt, UnreachableInst    (ty, ops)))
    | ResumeInst         (ty, ops) -> h ops (fun ops -> (nopt, ResumeInst         (ty, ops)))
    | BitCastInst        (ty, ops) -> h ops (fun ops -> (nopt, BitCastInst        (ty, ops)))
    | FPExtInst          (ty, ops) -> h ops (fun ops -> (nopt, FPExtInst          (ty, ops)))
    | FPToSIInst         (ty, ops) -> h ops (fun ops -> (nopt, FPToSIInst         (ty, ops)))
    | FPToUIInst         (ty, ops) -> h ops (fun ops -> (nopt, FPToUIInst         (ty, ops)))
    | FPTruncInst        (ty, ops) -> h ops (fun ops -> (nopt, FPTruncInst        (ty, ops)))
    | IntToPtrInst       (ty, ops) -> h ops (fun ops -> (nopt, IntToPtrInst       (ty, ops)))
    | PtrToIntInst       (ty, ops) -> h ops (fun ops -> (nopt, PtrToIntInst       (ty, ops)))
    | SExtInst           (ty, ops) -> h ops (fun ops -> (nopt, SExtInst           (ty, ops)))
    | SIToFPInst         (ty, ops) -> h ops (fun ops -> (nopt, SIToFPInst         (ty, ops)))
    | TruncInst          (ty, ops) -> h ops (fun ops -> (nopt, TruncInst          (ty, ops)))
    | UIToFPInst         (ty, ops) -> h ops (fun ops -> (nopt, UIToFPInst         (ty, ops)))
    | ZExtInst           (ty, ops) -> h ops (fun ops -> (nopt, ZExtInst           (ty, ops)))
    | ExtractValueInst   (ty, ops) -> h ops (fun ops -> (nopt, ExtractValueInst   (ty, ops)))
    | VAArgInst          (ty, ops) -> h ops (fun ops -> (nopt, VAArgInst          (ty, ops)))
    | FenceInst          (ty, ops) -> h ops (fun ops -> (nopt, FenceInst          (ty, ops)))
    | AtomicCmpXchgInst  (ty, ops) -> h ops (fun ops -> (nopt, AtomicCmpXchgInst  (ty, ops)))
    | AtomicRMWInst      (ty, ops) -> h ops (fun ops -> (nopt, AtomicRMWInst      (ty, ops)))) in
  (f.f_blocks <-
    (List.map
      (fun bl ->
        { b_name = bl.b_name;
          b_instrs = List.concat (List.map elim bl.b_instrs);
        })
      f.f_blocks));
  ()

