type var =
  | Id   of bool * int    (* global?, index *)
  | Name of bool * string (* global?, name *)

type typ =
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
  | Vartyp  of var
  | Integer of int                   (* bitwidth *)
  | FunTy   of typ * typ list * bool (* return type, param types, var_arg? *)
  | Struct  of bool * typ list       (* packed?, fields *) (* TODO: name *) (* TODO: missing isLiteral *)
  | Array   of int * typ             (* array length, element type *)
  | Pointer of typ * int option      (* element type, address space *)
  | Vector  of int * typ             (* array length, element type *)

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

type value =
  | Var            of var
  | Argument       of var
  | BasicBlock     of var (* name or slot *)
  | InlineAsm
  | MDNode         of int
  | MDNodeVector   of (typ * value) option list
  | MDString       of string
  | UndefValue
  | ConstantStruct of bool * (typ * value) list (* packed?, fields *)
  | ConstantVector of (typ * value) list
  | ConstantArray  of (typ * value) list
  | Zero
  | Null
  | True
  | False
  | Int            of Big_int.big_int
  | Float          of string
  | Trunc          of (typ * value) * typ
  | Zext           of (typ * value) * typ
  | Sext           of (typ * value) * typ
  | Fptrunc        of (typ * value) * typ
  | Fpext          of (typ * value) * typ
  | Bitcast        of (typ * value) * typ
  | Addrspacecast  of (typ * value) * typ
  | Uitofp         of (typ * value) * typ
  | Sitofp         of (typ * value) * typ
  | Fptoui         of (typ * value) * typ
  | Fptosi         of (typ * value) * typ
  | Inttoptr       of (typ * value) * typ
  | Ptrtoint       of (typ * value) * typ
  | ExtractValue   of (typ * value) * int list
  | InsertValue    of (typ * value) * (typ * value) * int list
  | Icmp           of I.t * (typ * value) * (typ * value)
  | Fcmp           of F.t * (typ * value) * (typ * value)
  | Sdiv           of bool * (typ * value) * (typ * value) (* exact?, arg, arg *)
  | Udiv           of bool * (typ * value) * (typ * value) (* exact?, arg, arg *)
  | Lshr           of bool * (typ * value) * (typ * value) (* exact?, arg, arg *)
  | Ashr           of bool * (typ * value) * (typ * value) (* exact?, arg, arg *)
  | Fadd           of (typ * value) * (typ * value)
  | Fsub           of (typ * value) * (typ * value)
  | Fmul           of (typ * value) * (typ * value)
  | Fdiv           of (typ * value) * (typ * value)
  | Urem           of (typ * value) * (typ * value)
  | Srem           of (typ * value) * (typ * value)
  | Frem           of (typ * value) * (typ * value)
  | And            of (typ * value) * (typ * value)
  | Or             of (typ * value) * (typ * value)
  | Xor            of (typ * value) * (typ * value)
  | Getelementptr  of bool * (typ * value) list (* inbounds?, args *)
  | Shufflevector  of (typ * value) list
  | Insertelement  of (typ * value) list
  | Extractelement of (typ * value) list
  | Select         of (typ * value) list
  | Add            of bool * bool * (typ * value) * (typ * value) (* nuw?, nsw?, arg, arg *)
  | Sub            of bool * bool * (typ * value) * (typ * value) (* nuw?, nsw?, arg, arg *)
  | Mul            of bool * bool * (typ * value) * (typ * value) (* nuw?, nsw?, arg, arg *)
  | Shl            of bool * bool * (typ * value) * (typ * value) (* nuw?, nsw?, arg, arg *)
  | Asm            of bool * bool * bool * string * string (* sideeffect?, alignstack?, inteldialect?, arg, arg *)
  | Blockaddress   of value * value

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

type aliasee =
  | A_bitcast of (typ * value) * typ
  | A_getelementptr of bool * (typ * value) list
  | A_type_value of (typ * value)

type attribute =
  | Align of int                   (* param_attribute *)
  | Byval                          (* param_attribute *)
  | Inalloca                       (* param_attribute *)
  | Inreg                          (* param_attribute *) (* return_attribute *)
  | Nest                           (* param_attribute *)
  | Noalias                        (* param_attribute *) (* return_attribute *)
  | Nocapture                      (* param_attribute *)
  | Readnone                       (* param_attribute *) (* function_attribute *) (* call_attribute *)
  | Readonly                       (* param_attribute *) (* function_attribute *) (* call_attribute *)
  | Returned                       (* param_attribute *)
  | Signext                        (* param_attribute *) (* return_attribute *)
  | Sret                           (* param_attribute *)
  | Zeroext                        (* param_attribute *) (* return_attribute *)
  | AttrGrpID of int               (* function_attribute *)
  | Attr of string * string option (* function_attribute *)
  | Alignstack of int              (* function_attribute *)
  | Alwaysinline                   (* function_attribute *)
  | Builtin                        (* function_attribute *)
  | Cold                           (* function_attribute *)
  | Inlinehint                     (* function_attribute *)
  | Minsize                        (* function_attribute *)
  | Naked                          (* function_attribute *)
  | Nobuiltin                      (* function_attribute *)
  | Noduplicate                    (* function_attribute *)
  | Noimplicitfloat                (* function_attribute *)
  | Noinline                       (* function_attribute *)
  | Nonlazybind                    (* function_attribute *)
  | Noredzone                      (* function_attribute *)
  | Noreturn                       (* function_attribute *) (* call_attribute *)
  | Nounwind                       (* function_attribute *) (* call_attribute *)
  | Optnone                        (* function_attribute *)
  | Optsize                        (* function_attribute *)
  | Returns_twice                  (* function_attribute *)
  | Ssp                            (* function_attribute *)
  | Sspreq                         (* function_attribute *)
  | Sspstrong                      (* function_attribute *)
  | Sanitize_address               (* function_attribute *)
  | Sanitize_thread                (* function_attribute *)
  | Sanitize_memory                (* function_attribute *)
  | Uwtable                        (* function_attribute *)
type call_attribute = attribute
type return_attribute = attribute
type param_attribute = attribute
type function_attribute = attribute

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
  | Catch of typ * value
  | Filter of typ * value

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
  | Add            of iflags * (typ * value) * value
  | Sub            of iflags * (typ * value) * value
  | Mul            of iflags * (typ * value) * value
  | Shl            of iflags * (typ * value) * value
  | Fadd           of fast_math_flag list * (typ * value) * value
  | Fsub           of fast_math_flag list * (typ * value) * value
  | Fmul           of fast_math_flag list * (typ * value) * value
  | Fdiv           of fast_math_flag list * (typ * value) * value
  | Frem           of fast_math_flag list * (typ * value) * value
  | Sdiv           of bool * (typ * value) * value
  | Udiv           of bool * (typ * value) * value
  | Lshr           of bool * (typ * value) * value
  | Ashr           of bool * (typ * value) * value
  | Urem           of (typ * value) * value
  | Srem           of (typ * value) * value
  | And            of (typ * value) * value
  | Or             of (typ * value) * value
  | Xor            of (typ * value) * value
  | Icmp           of I.t * (typ * value) * value
  | Fcmp           of F.t * (typ * value) * value
  | Trunc          of (typ * value) * typ
  | Zext           of (typ * value) * typ
  | Sext           of (typ * value) * typ
  | Fptrunc        of (typ * value) * typ
  | Fpext          of (typ * value) * typ
  | Bitcast        of (typ * value) * typ
  | Addrspacecast  of (typ * value) * typ
  | Uitofp         of (typ * value) * typ
  | Sitofp         of (typ * value) * typ
  | Fptoui         of (typ * value) * typ
  | Fptosi         of (typ * value) * typ
  | Inttoptr       of (typ * value) * typ
  | Ptrtoint       of (typ * value) * typ
  | Va_arg         of (typ * value) * typ
  | Getelementptr  of bool * (typ * value) list
  | Extractelement of (typ * value) list
  | Insertelement  of (typ * value) list
  | Shufflevector  of (typ * value) list
  | Select         of (typ * value) list
  | Phi            of typ * (value * value) list
  | Landingpad of typ * (typ * value) * bool * landingpad list
  | Call of bool * callingconv option * return_attribute list * typ * value * (typ * value * param_attribute option) list * call_attribute list
  | Alloca of bool * typ * (typ * value) option * int option
  | Load of bool * bool * (typ * value) * (bool * ordering) option * int option
  | Store of bool * bool * (typ * value) * (typ * value) * (bool * ordering) option * int option
  | Cmpxchg of bool * (typ * value) * (typ * value) * (typ * value) * bool * ordering * ordering
  | Atomicrmw of bool * binop * (typ * value) * (typ * value) * bool * ordering
  | Fence of bool * ordering
  | Extractvalue   of (typ * value) * int list
  | Insertvalue    of (typ * value) * (typ * value) * int list
(* terminator instructions *)
  | Unreachable
  | Return of (typ * value) option
  | Br of (typ * value) *  ((typ * value) * (typ * value)) option
  | Indirectbr of (typ * value) * (typ * value) list
  | Resume of (typ * value)
  | Switch of (typ * value) * (typ * value) * ((typ * value) * (typ * value)) list
  | Invoke of callingconv option * return_attribute list * typ * value * ((typ * value * param_attribute option) list) * function_attribute list * (typ * value) * (typ * value)

type finfo = {
    mutable flinkage: linkage option;
    mutable fvisibility: visibility option;
    mutable fstorageclass: dll_storageclass option;
    mutable fcallingconv: callingconv option;
    mutable freturnattrs: return_attribute list;
    mutable freturntyp: typ;
    mutable fname: var;
    mutable fparams: (typ list * bool);
    mutable funnamed_addr: bool;
    mutable fattrs: function_attribute list;
    mutable fsection: string option;
    mutable falign: int option;
    mutable fgc: string option;
    mutable fprefix: (typ * value) option;
    mutable fblocks: (string option * instr list) list;
  }

type thread_local =
  | Localdynamic
  | Initialexec
  | Localexec

type ginfo = {
    mutable gname: var;
    mutable glinkage: linkage option;
    mutable gvisibility: visibility option;
    mutable gstorageclass: dll_storageclass option;
    mutable gthread_local: thread_local option option;
    mutable gaddrspace: int option;
    mutable gunnamed_addr: bool;
    mutable gexternally_initialized: bool;
    mutable gconstant: bool;
    mutable gtyp: typ;
    mutable gvalue: value option;
    mutable gsection: string option;
    mutable galign: int option;
  }

type ainfo = {
    aname: var;
    avisibility: visibility option;
    alinkage: linkage option;
    aaliasee: aliasee
  }

type mdinfo = {
    mdid: int;
    mdtyp: typ;
    mdcontents: (typ * value) option list;
  }

(* compilation unit *)
type cunit = {
    mutable ctarget: string option;
    mutable cdatalayout: string option;
    mutable casms: string list;
    mutable cfuns: finfo list;
    mutable ctyps: (var * typ option) list;
    mutable cglobals: ginfo list;
    mutable caliases: ainfo list;
    mutable cmdnodes: mdinfo list;
    mutable cmdvars: (string * int list) list;
    mutable cattrgrps: (string * attribute list) list;
  }
