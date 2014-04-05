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
