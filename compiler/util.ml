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
  (* Missing X86_MMXTyID:   OS << "x86_mmx" ????? *)
  | Integer    of int                               (* bitwidth *)
  | FunctionTy of oType * oType list * bool         (* return type, param types, is_var_arg *)
  | Struct     of string option                     (* name *) (* TODO: missing isLiteral *)
  | Array      of int * oType                       (* array length, element type *)
  | Pointer    of int * oType                       (* address space, element type *)
  | Vector     of int * oType                       (* array length, element type *)
