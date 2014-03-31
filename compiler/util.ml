type variable =
  | ID of bool * int
  | VAR of bool * string

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
  | FunctionTy of oType * oType list * bool         (* return type, param types, is_var_arg *)
  | Struct     of bool * oType list                 (* packed, fields *) (* TODO: name *) (* TODO: missing isLiteral *)
  | Array      of int * oType                       (* array length, element type *)
  | Pointer    of oType * int option                (* element type, address space *)
  | Vector     of int * oType                       (* array length, element type *)

