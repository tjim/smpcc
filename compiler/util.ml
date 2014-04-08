type variable =
  | ID of bool * int (* global?, index *)
  | VAR of bool * string (* global?, name *)

type oType =
  | Void
  | Half
  | Float
  | Double
  | X86_fp80
  | X86_mmx
  | Fp128
  | Ppc_fp128
  | Label
  | Metadata
  | Varty of variable
  (* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
  | Integer    of int                               (* bitwidth *)
  | FunctionTy of oType * oType list * bool         (* return type, param types, var_arg? *)
  | Struct     of bool * oType list                 (* packed?, fields *) (* TODO: name *) (* TODO: missing isLiteral *)
  | Array      of int * oType                       (* array length, element type *)
  | Pointer    of oType * int option                (* element type, address space *)
  | Vector     of int * oType                       (* array length, element type *)

module I = struct
  type t =
    | Eq
    | Ne
    | Slt
    | Sgt
    | Sle
    | Sge
    | Ult
    | Ugt
    | Ule
    | Uge
end

module F = struct
  type t =
    | Oeq
    | One
    | Olt
    | Ogt
    | Ole
    | Oge
    | Ord
    | Uno
    | Ueq
    | Une
    | Ult
    | Ugt
    | Ule
    | Uge
    | True
    | False
end

type oValue =
  | Variable       of variable
  | Argument       of variable
  | BasicBlock     of variable (* name or slot *)
  | InlineAsm
  | MDNode         of int
  | MDNodeVector   of (oType * oValue) option list
  | MDString       of string
  | UndefValue
  | ConstantStruct of bool * (oType * oValue) list (* packed?, fields *)
  | ConstantVector of (oType * oValue) list
  | ConstantArray  of (oType * oValue) list
  | Zero
  | Null
  | True
  | False
  | Int            of Big_int.big_int
  | Float          of string
  | Trunc          of (oType * oValue) * oType
  | Zext           of (oType * oValue) * oType
  | Sext           of (oType * oValue) * oType
  | Fptrunc        of (oType * oValue) * oType
  | Fpext          of (oType * oValue) * oType
  | Bitcast        of (oType * oValue) * oType
  | Addrspacecast  of (oType * oValue) * oType
  | Uitofp         of (oType * oValue) * oType
  | Sitofp         of (oType * oValue) * oType
  | Fptoui         of (oType * oValue) * oType
  | Fptosi         of (oType * oValue) * oType
  | Inttoptr       of (oType * oValue) * oType
  | Ptrtoint       of (oType * oValue) * oType
  | ExtractValue   of (oType * oValue) * int list
  | InsertValue    of (oType * oValue) * (oType * oValue) * int list
  | Icmp           of I.t * (oType * oValue) * (oType * oValue)
  | Fcmp           of F.t * (oType * oValue) * (oType * oValue)
  | Sdiv           of bool * (oType * oValue) * (oType * oValue) (* exact?, arg, arg *)
  | Udiv           of bool * (oType * oValue) * (oType * oValue) (* exact?, arg, arg *)
  | Lshr           of bool * (oType * oValue) * (oType * oValue) (* exact?, arg, arg *)
  | Ashr           of bool * (oType * oValue) * (oType * oValue) (* exact?, arg, arg *)
  | Fadd           of (oType * oValue) * (oType * oValue)
  | Fsub           of (oType * oValue) * (oType * oValue)
  | Fmul           of (oType * oValue) * (oType * oValue)
  | Fdiv           of (oType * oValue) * (oType * oValue)
  | Urem           of (oType * oValue) * (oType * oValue)
  | Srem           of (oType * oValue) * (oType * oValue)
  | Frem           of (oType * oValue) * (oType * oValue)
  | And            of (oType * oValue) * (oType * oValue)
  | Or             of (oType * oValue) * (oType * oValue)
  | Xor            of (oType * oValue) * (oType * oValue)
  | Getelementptr  of bool * (oType * oValue) list (* inbounds?, args *)
  | Shufflevector  of (oType * oValue) list
  | Insertelement  of (oType * oValue) list
  | Extractelement of (oType * oValue) list
  | Select         of (oType * oValue) list
  | Add            of bool * bool * (oType * oValue) * (oType * oValue) (* nuw?, nsw?, arg, arg *)
  | Sub            of bool * bool * (oType * oValue) * (oType * oValue) (* nuw?, nsw?, arg, arg *)
  | Mul            of bool * bool * (oType * oValue) * (oType * oValue) (* nuw?, nsw?, arg, arg *)
  | Shl            of bool * bool * (oType * oValue) * (oType * oValue) (* nuw?, nsw?, arg, arg *)
  | Asm            of bool * bool * bool * string * string (* sideeffect?, alignstack?, inteldialect?, arg, arg *)
  | Blockaddress   of oValue * oValue

type linkage =
  | Extern_weak
  | External
  | Private
  | Internal
  | Weak
  | Weak_odr
  | Linkonce
  | Linkonce_odr
  | Available_externally
  | Appending
  | Common

type visibility =
  | Default
  | Hidden
  | Protected

type dll_storageclass =
  | Dllimport
  | Dllexport

type tattr =
  | Section of string
  | Align of int

type aliasee =
  | A_bitcast of (oType * oValue) * oType
  | A_getelementptr of bool * (oType * oValue) list
  | A_type_value of (oType * oValue)

type param_attribute =
  | Align of int (* dup *)
  | Byval
  | Inalloca
  | Inreg        (* dup *)
  | Nest
  | Noalias      (* dup *)
  | Nocapture
  | Readnone
  | Readonly
  | Returned
  | Signext      (* dup *)
  | Sret
  | Zeroext      (* dup *)

type function_attribute =
| AttrGrpID of int
| Attr of string * string
| Alignstack of int
| Alwaysinline
| Builtin
| Cold
| Inlinehint
| Minsize
| Naked
| Nobuiltin
| Noduplicate
| Noimplicitfloat
| Noinline
| Nonlazybind
| Noredzone
| Noreturn
| Nounwind
| Optnone
| Optsize
| Readnone
| Readonly
| Returns_twice
| Ssp
| Sspreq
| Sspstrong
| Sanitize_address
| Sanitize_thread
| Sanitize_memory
| Uwtable

type callingconv =
  | Ccc
  | Fastcc
  | Intel_ocl_bicc
  | Coldcc
  | X86_stdcallcc
  | X86_fastcallcc
  | X86_thiscallcc
  | X86_cdeclmethodcc
  | Arm_apcscc
  | Arm_aapcscc
  | Arm_aapcs_vfpcc
  | Msp430_intrcc
  | Ptx_kernel
  | Ptx_device
  | Spir_func
  | Spir_kernel
  | X86_64_sysvcc
  | X86_64_win64cc
  | Webkit_jscc
  | Anyregcc
  | Preserve_mostcc
  | Preserve_allcc
  | Cc

type return_attribute =
  | Inreg
  | Noalias
  | Signext
  | Zeroext

type fast_math_flag =
| Fast
| Nnan
| Ninf
| Nsz
| Arcp

type ordering =
| Unordered
| Monotonic
| Acquire
| Release
| Acq_rel
| Seq_cst

type landingpad =
  | Catch of oType * oValue
  | Filter of oType * oValue

type call_attribute =
| Noreturn
| Nounwind
| Readnone
| Readonly

type binop =
| Xchg
| Add
| Sub
| And
| Nand
| Or
| Xor
| Max
| Min
| Umax
| Umin

type iflags = bool * bool

type instr =
  | Add            of iflags * (oType * oValue) * oValue
  | Sub            of iflags * (oType * oValue) * oValue
  | Mul            of iflags * (oType * oValue) * oValue
  | Shl            of iflags * (oType * oValue) * oValue
  | Fadd           of fast_math_flag list * (oType * oValue) * oValue
  | Fsub           of fast_math_flag list * (oType * oValue) * oValue
  | Fmul           of fast_math_flag list * (oType * oValue) * oValue
  | Fdiv           of fast_math_flag list * (oType * oValue) * oValue
  | Frem           of fast_math_flag list * (oType * oValue) * oValue
  | Sdiv           of bool * (oType * oValue) * oValue
  | Udiv           of bool * (oType * oValue) * oValue
  | Lshr           of bool * (oType * oValue) * oValue
  | Ashr           of bool * (oType * oValue) * oValue
  | Urem           of (oType * oValue) * oValue
  | Srem           of (oType * oValue) * oValue
  | And            of (oType * oValue) * oValue
  | Or             of (oType * oValue) * oValue
  | Xor            of (oType * oValue) * oValue
  | Icmp           of I.t * (oType * oValue) * oValue
  | Fcmp           of F.t * (oType * oValue) * oValue
  | Trunc          of (oType * oValue) * oType
  | Zext           of (oType * oValue) * oType
  | Sext           of (oType * oValue) * oType
  | Fptrunc        of (oType * oValue) * oType
  | Fpext          of (oType * oValue) * oType
  | Bitcast        of (oType * oValue) * oType
  | Addrspacecast  of (oType * oValue) * oType
  | Uitofp         of (oType * oValue) * oType
  | Sitofp         of (oType * oValue) * oType
  | Fptoui         of (oType * oValue) * oType
  | Fptosi         of (oType * oValue) * oType
  | Inttoptr       of (oType * oValue) * oType
  | Ptrtoint       of (oType * oValue) * oType
  | Va_arg         of (oType * oValue) * oType
  | Getelementptr  of bool * (oType * oValue) list
  | Extractelement of (oType * oValue) list
  | Insertelement  of (oType * oValue) list
  | Shufflevector  of (oType * oValue) list
  | Select         of (oType * oValue) list
  | Phi            of oType * (oValue * oValue) list
  | Landingpad of oType * (oType * oValue) * bool * landingpad list
  | Call of bool * callingconv option * return_attribute list * oType * oValue * (oType * oValue * param_attribute option) list * call_attribute list
  | Alloca of bool * oType * (oType * oValue) option * int option
  | Load of bool * bool * (oType * oValue) * (bool * ordering) option * int option
  | Store of bool * bool * (oType * oValue) * (oType * oValue) * (bool * ordering) option * int option
  | Cmpxchg of bool * (oType * oValue) * (oType * oValue) * (oType * oValue) * bool * ordering * ordering
  | Atomicrmw of bool * binop * (oType * oValue) * (oType * oValue) * bool * ordering
  | Fence of bool * ordering
  | Extractvalue   of (oType * oValue) * int list
  | Insertvalue    of (oType * oValue) * (oType * oValue) * int list
(* terminator instructions *)
  | Unreachable
  | Return of (oType * oValue) option
  | Br of (oType * oValue) *  ((oType * oValue) * (oType * oValue)) option
  | Indirectbr of (oType * oValue) * (oType * oValue) list
  | Resume of (oType * oValue)
  | Switch of (oType * oValue) * (oType * oValue) * ((oType * oValue) * (oType * oValue)) list
  | Invoke of callingconv option * return_attribute list * oType * oValue * ((oType * oValue * param_attribute option) list) * function_attribute list * (oType * oValue) * (oType * oValue)

type toplevel =
  | FunDecl
  | FunDefn
  | AsmDefn of string
  | Target of string
  | Datalayout of string
  | Deplibs of string list
  | Type of variable * oType option
  | Global of variable * linkage option * visibility option * dll_storageclass option * int option * bool * bool * bool * oType * oValue option * tattr list
  | GlobalAlias of variable * linkage option * visibility option * linkage option * aliasee
  | MDNodeDefn of int * oType * (oType * oValue) option list
  | MDVarDefn of string * int list
  | AttrDefn of string * unit list
