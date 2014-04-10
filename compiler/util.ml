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
  | Funtyp  of typ * typ list * bool (* return type, param types, var_arg? *)
  | Structtyp of bool * typ list       (* packed?, fields *) (* TODO: name *) (* TODO: missing isLiteral *)
  | Arraytyp of int * typ             (* array length, element type *)
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
  | Mdnode         of int
  | Mdstring       of string
  | Mdnodevector   of (typ * value) option list
  | Undef
  | Zero
  | Null
  | True
  | False
  | Int            of Big_int.big_int
  | Float          of string
  | Blockaddress   of value * value
  | Struct of bool * (typ * value) list (* packed?, fields *)
  | Vector of (typ * value) list
  | Array  of (typ * value) list
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
  | Extractvalue   of (typ * value) * int list
  | Insertvalue    of (typ * value) * (typ * value) * int list
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
  | A_typ_value of (typ * value)

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
  | Attrgrp of int                 (* function_attribute *)
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

type instr =
  | Add            of bool * bool * (typ * value) * value
  | Sub            of bool * bool * (typ * value) * value
  | Mul            of bool * bool * (typ * value) * value
  | Shl            of bool * bool * (typ * value) * value
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
    mutable fblocks: (string option * (var option * instr) list) list;
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
    mutable cdatalayout: string option;
    mutable ctarget: string option;
    mutable casms: string list;
    mutable ctyps: (var * typ option) list;
    mutable cglobals: ginfo list;
    mutable caliases: ainfo list;
    mutable cfuns: finfo list;
    mutable cattrgrps: (string * attribute list) list;
    mutable cmdvars: (string * int list) list;
    mutable cmdnodes: mdinfo list;
  }

open Printf

let opt pr b = function
  | None -> ()
  | Some x -> pr b x

let bpr_concat sep bpr b =
  let rec loop = function
  | [] -> ()
  | [hd] -> bpr b hd
  | hd::tl ->
      bprintf b "%a%s" bpr hd sep;
      loop tl in
  loop

let bpr_binop b = function
  | Xchg -> bprintf b "xchg"
  | Add -> bprintf b "add"
  | Sub -> bprintf b "sub"
  | And -> bprintf b "and"
  | Nand -> bprintf b "nand"
  | Or -> bprintf b "or"
  | Xor -> bprintf b "xor"
  | Max -> bprintf b "max"
  | Min -> bprintf b "min"
  | Umax -> bprintf b "umax"
  | Umin -> bprintf b "umin"

let bpr_fast_math_flag b = function
  | Fast -> bprintf b "fast"
  | Nnan -> bprintf b "nnan"
  | Ninf -> bprintf b "ninf"
  | Nsz  -> bprintf b "nsz"
  | Arcp -> bprintf b "arcp"

let bpr_fast_math_flags = bpr_concat " " bpr_fast_math_flag

let bpr_attribute b = function
  | Align x -> bprintf b "align %d" x
  | Byval -> bprintf b "byval"
  | Inalloca -> bprintf b "inalloca"
  | Inreg -> bprintf b "inreg"
  | Nest -> bprintf b "nest"
  | Noalias -> bprintf b "noalias"
  | Nocapture -> bprintf b "nocapture"
  | Readnone -> bprintf b "readnone"
  | Readonly -> bprintf b "readonly"
  | Returned -> bprintf b "returned"
  | Signext -> bprintf b "signext"
  | Sret -> bprintf b "sret"
  | Zeroext -> bprintf b "zeroext"
  | Attrgrp x -> bprintf b "#%d" x
  | Attr(x,None) -> bprintf b "%s" x
  | Attr(x,Some y) -> bprintf b "%s = %s" x y
  | Alignstack x -> bprintf b "alignstack = (%d)" x
  | Alwaysinline -> bprintf b "alwaysinline"
  | Builtin -> bprintf b "builtin"
  | Cold -> bprintf b "cold"
  | Inlinehint -> bprintf b "inlinehint"
  | Minsize -> bprintf b "minsize"
  | Naked -> bprintf b "naked"
  | Nobuiltin -> bprintf b "nobuiltin"
  | Noduplicate -> bprintf b "noduplicate"
  | Noimplicitfloat -> bprintf b "noimplicitfloat"
  | Noinline -> bprintf b "noinline"
  | Nonlazybind -> bprintf b "nonlazybind"
  | Noredzone -> bprintf b "noredzone"
  | Noreturn -> bprintf b "noreturn"
  | Nounwind -> bprintf b "nounwind"
  | Optnone -> bprintf b "optnone"
  | Optsize -> bprintf b "optsize"
  | Returns_twice -> bprintf b "returns_twice"
  | Ssp -> bprintf b "ssp"
  | Sspreq -> bprintf b "sspreq"
  | Sspstrong -> bprintf b "sspstrong"
  | Sanitize_address -> bprintf b "sanitize_address"
  | Sanitize_thread -> bprintf b "sanitize_thread"
  | Sanitize_memory -> bprintf b "sanitize_memory"
  | Uwtable -> bprintf b "uwtable"

let bpr_attributes =
  bpr_concat " " bpr_attribute

let bpr_attrgrp b (x,y) =
  bprintf b "attributes !%s = { %a }\n" x bpr_attributes y

let bpr_callingconv b = function
  | Ccc -> bprintf b "ccc"
  | Fastcc -> bprintf b "fastcc"
  | Intel_ocl_bicc -> bprintf b "intel_ocl_bicc"
  | Coldcc -> bprintf b "coldcc"
  | X86_stdcallcc -> bprintf b "x86_stdcallcc"
  | X86_fastcallcc -> bprintf b "x86_fastcallcc"
  | X86_thiscallcc -> bprintf b "x86_thiscallcc"
  | X86_cdeclmethodcc -> bprintf b "x86_cdeclmethodcc"
  | Arm_apcscc -> bprintf b "arm_apcscc"
  | Arm_aapcscc -> bprintf b "arm_aapcscc"
  | Arm_aapcs_vfpcc -> bprintf b "arm_aapcs_vfpcc"
  | Msp430_intrcc -> bprintf b "msp430_intrcc"
  | Ptx_kernel -> bprintf b "ptx_kernel"
  | Ptx_device -> bprintf b "ptx_device"
  | Spir_func -> bprintf b "spir_func"
  | Spir_kernel -> bprintf b "spir_kernel"
  | X86_64_sysvcc -> bprintf b "x86_64_sysvcc"
  | X86_64_win64cc -> bprintf b "x86_64_win64cc"
  | Webkit_jscc -> bprintf b "webkit_jscc"
  | Anyregcc -> bprintf b "anyregcc"
  | Preserve_mostcc -> bprintf b "preserve_mostcc"
  | Preserve_allcc -> bprintf b "preserve_allcc"
  | Cc -> bprintf b "cc"

let bpr_storageclass b = function
  | Dllimport -> bprintf b "dllimport"
  | Dllexport -> bprintf b "dllexport"

let bpr_thread_local b = function
  | None -> bprintf b "thread_local"
  | Some Localdynamic -> bprintf b "thread_local(localdynamic)"
  | Some Initialexec -> bprintf b "thread_local(initialexec)"
  | Some Localexec -> bprintf b "thread_local(localexec)"

let bpr_icmp b = function
  | I.Eq  -> bprintf b "eq"
  | I.Ne  -> bprintf b "ne"
  | I.Slt -> bprintf b "slt"
  | I.Sgt -> bprintf b "sgt"
  | I.Sle -> bprintf b "sle"
  | I.Sge -> bprintf b "sge"
  | I.Ult -> bprintf b "ult"
  | I.Ugt -> bprintf b "ugt"
  | I.Ule -> bprintf b "ule"
  | I.Uge -> bprintf b "uge"

let bpr_fcmp b = function
  | F.Oeq     -> bprintf b "oeq"
  | F.One     -> bprintf b "one"
  | F.Olt     -> bprintf b "olt"
  | F.Ogt     -> bprintf b "ogt"
  | F.Ole     -> bprintf b "ole"
  | F.Oge     -> bprintf b "oge"
  | F.Ord     -> bprintf b "ord"
  | F.Uno     -> bprintf b "uno"
  | F.Ueq     -> bprintf b "ueq"
  | F.Une     -> bprintf b "une"
  | F.Ult     -> bprintf b "ult"
  | F.Ugt     -> bprintf b "ugt"
  | F.Ule     -> bprintf b "ule"
  | F.Uge     -> bprintf b "uge"
  | F.True    -> bprintf b "true"
  | F.False   -> bprintf b "false"

let bpr_var b = function
  | Id(true, i) -> bprintf b "@%d" i
  | Id(false, i) -> bprintf b "%%%d" i
  | Name(true, name) -> bprintf b "@%s" name
  | Name(false, name) -> bprintf b "%%%s" name

let rec bpr_typ b typ =
  let rec pr = function
    | Void      -> bprintf b "void"
    | Half      -> bprintf b "half"
    | Float     -> bprintf b "float"
    | Double    -> bprintf b "double"
    | X86_fp80  -> bprintf b "x86_fp80"
    | X86_mmx   -> bprintf b "x86_mmx"
    | Fp128     -> bprintf b "fp128"
    | Ppc_fp128 -> bprintf b "ppc_fp128"
    | Label     -> bprintf b "label"
    | Metadata  -> bprintf b "metadata"
    | Integer x ->
        bprintf b "i%d" x
    | Vartyp x -> bpr_var b x
    | Funtyp(return_ty, param_tys, is_var_arg) ->
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
    | Structtyp(packed, typs) ->
        if packed then bprintf b "<";
        (match typs with
        | [] -> bprintf b "{}"
        | hd::tl ->
            bprintf b "{ %a" bpr_typ hd;
            List.iter (bprintf b ", %a " bpr_typ) tl;
            bprintf b " }");
        if packed then bprintf b ">"
    | Arraytyp(len,element_ty) ->
        bprintf b "[%d x " len;
        pr element_ty;
        bprintf b "]"
    | Pointer(element_ty,address_space) ->
        pr element_ty;
        (match address_space with
        | None -> ()
        | Some 0 -> ()
        | Some x -> bprintf b " addrspace(%d)" x);
        bprintf b "*"
    | Vector(len,element_ty) ->
        bprintf b "<%d x " len;
        pr element_ty;
        bprintf b ">"
  in pr typ

let bpr_linkage b = function
  | External             -> ()
  | Private              -> bprintf b "private"
  | Extern_weak          -> bprintf b "extern_weak"
  | Internal             -> bprintf b "internal"
  | Weak                 -> bprintf b "weak"
  | Weak_odr             -> bprintf b "weak_odr"
  | Linkonce             -> bprintf b "linkonce"
  | Linkonce_odr         -> bprintf b "linkonce_odr"
  | Common               -> bprintf b "common"
  | Appending            -> bprintf b "appending"
  | Available_externally -> bprintf b "available_externally"

let bpr_visibility b = function
  | Default -> ()
  | Hidden -> bprintf b "hidden"
  | Protected -> bprintf b "protected"

let bpr_index_list b l =
  List.iter (bprintf b ", %d") l

let bpr_yesno s b x =
  if x then bprintf b "%s" s

let rec bpr_value b op = match op with
  | Var x          -> bpr_var b x
  | Mdnode x       -> bprintf b "!%d" x
  | Mdnodevector x -> bpr_mdnodevector b x
  | Mdstring x     -> bprintf b "!%s" x
  | Null           -> bprintf b "null"
  | Undef          -> bprintf b "undef"
  | Zero           -> bprintf b "zeroinitializer"
  | True           -> bprintf b "true"
  | False          -> bprintf b "false"
  | Int x          -> bprintf b "%s" (Big_int.string_of_big_int x)
  | Float x        -> bprintf b "%s" x
  | Blockaddress(x,y) ->
      bprintf b "blockaddress(%a, %a)" bpr_value x bpr_value y
  | Array ops ->
      bprintf b "[%a]" (bpr_concat ", " bpr_typ_value) ops
  | Vector ops ->
      bprintf b "<%a>" (bpr_concat ", " bpr_typ_value) ops
  | Struct(is_packed,ops) ->
      if is_packed then bprintf b "<";
      bprintf b "{%a}" (bpr_concat ", " bpr_typ_value) ops;
      if is_packed then bprintf b ">"
  | Trunc(x,y)     -> bprintf b "trunc(%a, %a)" bpr_typ_value x              bpr_typ y
  | Zext(x,y)           -> bprintf b "zext(%a, %a)" bpr_typ_value x          bpr_typ y
  | Sext(x,y)           -> bprintf b "sext(%a, %a)" bpr_typ_value x          bpr_typ y
  | Fptrunc(x,y)        -> bprintf b "fptrunc(%a, %a)" bpr_typ_value x       bpr_typ y
  | Fpext(x,y)          -> bprintf b "fpext(%a, %a)" bpr_typ_value x         bpr_typ y
  | Bitcast(x,y)        -> bprintf b "bitcast(%a, %a)" bpr_typ_value x       bpr_typ y
  | Addrspacecast(x,y)  -> bprintf b "addrspacecast(%a, %a)" bpr_typ_value x bpr_typ y
  | Uitofp(x,y)         -> bprintf b "uitofp(%a, %a)" bpr_typ_value x        bpr_typ y
  | Sitofp(x,y)         -> bprintf b "sitofp(%a, %a)" bpr_typ_value x        bpr_typ y
  | Fptoui(x,y)         -> bprintf b "fptoui(%a, %a)" bpr_typ_value x        bpr_typ y
  | Fptosi(x,y)         -> bprintf b "fptosi(%a, %a)" bpr_typ_value x        bpr_typ y
  | Inttoptr(x,y)       -> bprintf b "inttoptr(%a, %a)" bpr_typ_value x      bpr_typ y
  | Ptrtoint(x,y)       -> bprintf b "ptrtoint(%a, %a)" bpr_typ_value x      bpr_typ y
  | Extractvalue(x,y) -> bprintf b "extractvalue(%a%a)" bpr_typ_value x bpr_index_list y
  | Insertvalue(x,y, z) -> bprintf b "insertvalue(%a, %a%a)" bpr_typ_value x bpr_typ_value y bpr_index_list z
  | Icmp(cmp, x, y) -> bprintf b "icmp %a (%a, %a)" bpr_icmp cmp bpr_typ_value x bpr_typ_value y
  | Fcmp(cmp, x, y)  -> bprintf b "fcmp %a (%a, %a)" bpr_fcmp cmp bpr_typ_value x bpr_typ_value y
  | Sdiv(e, x, y)           -> bprintf b "sdiv%a(%a, %a)" (bpr_yesno " exact ") e bpr_typ_value x bpr_typ_value y
  | Udiv(e, x, y)           -> bprintf b "udiv%a(%a, %a)" (bpr_yesno " exact ") e bpr_typ_value x bpr_typ_value y
  | Lshr(e, x, y)           -> bprintf b "lshr%a(%a, %a)" (bpr_yesno " exact ") e bpr_typ_value x bpr_typ_value y
  | Ashr(e, x, y)           -> bprintf b "ashr%a(%a, %a)" (bpr_yesno " exact ") e bpr_typ_value x bpr_typ_value y
  | Fadd(x, y)           -> bprintf b "fadd(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Fsub(x, y)           -> bprintf b "fsub(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Fmul(x, y)           -> bprintf b "fmul(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Fdiv(x, y)           -> bprintf b "fdiv(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Frem(x, y)           -> bprintf b "frem(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Urem(x, y)           -> bprintf b "urem(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Srem(x, y)           -> bprintf b "srem(%a, %a)" bpr_typ_value x bpr_typ_value y
  | And (x, y)           -> bprintf b "and(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Or  (x, y)           -> bprintf b "or(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Xor (x, y)           -> bprintf b "xor(%a, %a)" bpr_typ_value x bpr_typ_value y
  | Getelementptr(inbounds, x) -> bprintf b "getelementptr%a(%a)" (bpr_yesno " inbounds ") inbounds bpr_typ_value_list x
  | Shufflevector x -> bprintf b "shufflevector(%a)" bpr_typ_value_list x
  | Insertelement x -> bprintf b "insertelement(%a)" bpr_typ_value_list x
  | Extractelement x -> bprintf b "extractelement(%a)" bpr_typ_value_list x
  | Select         x -> bprintf b "select(%a)" bpr_typ_value_list x
  | Add(nuw, nsw, x, y) -> bprintf b "add%a%a(%a, %a)" (bpr_yesno " nuw") nuw (bpr_yesno " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Sub(nuw, nsw, x, y) -> bprintf b "sub%a%a(%a, %a)" (bpr_yesno " nuw") nuw (bpr_yesno " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Mul(nuw, nsw, x, y) -> bprintf b "mul%a%a(%a, %a)" (bpr_yesno " nuw") nuw (bpr_yesno " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Shl(nuw, nsw, x, y) -> bprintf b "shl%a%a(%a, %a)" (bpr_yesno " nuw") nuw (bpr_yesno " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Asm(sideeffect, alignstack, inteldialect, x, y) ->
      bprintf b "asm %a%a%a %s, %s"
        (bpr_yesno "sideeffect ") sideeffect
        (bpr_yesno "alignstack ") alignstack
        (bpr_yesno "inteldialect ") inteldialect x y

and bpr_typ_value b (typ, value) =
  bprintf b "%a %a" bpr_typ typ bpr_value value

and bpr_typ_value_list b l =
  (match l with [] -> ()
  | (ty,op)::tl ->
      let all_same_type = List.for_all (fun (ty',_) -> ty=ty') tl in
      if all_same_type then begin
        bprintf b "%a %a" bpr_typ ty bpr_value op;
        List.iter (fun (_,op) -> bprintf b ", %a" bpr_value op) tl
      end else
        bpr_concat ", " (fun b (ty,op) -> bprintf b "%a %a" bpr_typ ty bpr_value op) b l)

and bpr_mdnodevector b x =
  let bpr b = function
    | None -> bprintf b "null"
    | Some y -> bpr_typ_value b y in
  bprintf b "{ %a }" (bpr_concat " " bpr) x


let getElementType = function
  | Arraytyp(_,ty) -> ty
  | Pointer(ty,_) -> ty
  | Vector(_,ty) -> ty
  | _ -> failwith "getElementType: not an array, pointer, or vector"

let bpr_global b g =
  bprintf b "%a = " bpr_var g.gname;
  (match g.glinkage with None -> () | Some x -> bprintf b "%a " bpr_linkage x);
  (match g.gvisibility with None -> () | Some x -> bprintf b "%a " bpr_visibility x);
  (match g.gstorageclass with None -> () | Some x -> bprintf b "%a " bpr_storageclass x);
  (match g.gthread_local with None -> () | Some x -> bprintf b "%a " bpr_thread_local x);
  (match g.gaddrspace with None -> () | Some x -> bprintf b "addrspace(%d) " x);
  if g.gunnamed_addr then bprintf b "unnamed_addr ";
  if g.gexternally_initialized then bprintf b "externally_initialized ";
  bprintf b "%s " (if g.gconstant then "constant" else "global");
  bpr_typ b (getElementType g.gtyp);
  (match g.gvalue with None -> () | Some x -> bprintf b " %a" bpr_value x);
  (match g.gsection with None -> () | Some x -> bprintf b ", section %s" x);
  (match g.galign with None -> () | Some x -> bprintf b ", align %d" x);
  bprintf b "\n"

let bpr_alias b a =
  bprintf b "%a = " bpr_var a.aname;
  (match a.avisibility with None -> () | Some x -> bprintf b "%a " bpr_visibility x);
  bprintf b "alias ";
  (match a.alinkage with None -> () | Some x -> bprintf b "%a " bpr_linkage x);
  (match a.aaliasee with
  | A_bitcast(x, y) -> bprintf b "bitcast(%a to %a)\n" bpr_typ_value x bpr_typ y
  | A_getelementptr(inbounds, x) -> bprintf b "getelementptr%a(%a)\n" (bpr_yesno " inbounds ") inbounds bpr_typ_value_list x
  | A_typ_value x -> bprintf b "%a\n" bpr_typ_value x)

let pad_to_column b n =
  let rec column i =
    if i<=0 then 0 else
    if Buffer.nth b (i-1) = '\n' then 0 else
    1 + column (i-1) in
  let column = column (Buffer.length b) in
  for x = column to n-1 do
    Buffer.add_char b ' '
  done

let bpr_arguments b l =
  let pr (typ, value, popt) =
    bprintf b "%a %a %a" bpr_typ typ bpr_value value (opt bpr_attribute) popt in
  List.iter pr l

let bpr_ordering b = function
| Unordered -> bprintf b "unordered"
| Monotonic -> bprintf b "monotonic"
| Acquire   -> bprintf b "acquire"
| Release   -> bprintf b "release"
| Acq_rel   -> bprintf b "acq_rel"
| Seq_cst   -> bprintf b "seq_cst"

let bpr_instr b (nopt,i) =
  bprintf b "  ";
  (match nopt with None -> ()
  | Some n -> bprintf b "%a = " bpr_var n);
  (match i with
  | Add(nuw, nsw, x, y) -> bprintf b "add %a%a%a, %a" (bpr_yesno "nuw ") nuw (bpr_yesno "nsw ") nsw bpr_typ_value x bpr_value y
  | Sub(nuw, nsw, x, y) -> bprintf b "sub %a%a%a, %a" (bpr_yesno "nuw ") nuw (bpr_yesno "nsw ") nsw bpr_typ_value x bpr_value y
  | Mul(nuw, nsw, x, y) -> bprintf b "mul %a%a%a, %a" (bpr_yesno "nuw ") nuw (bpr_yesno "nsw ") nsw bpr_typ_value x bpr_value y
  | Shl(nuw, nsw, x, y) -> bprintf b "shl %a%a%a, %a" (bpr_yesno "nuw ") nuw (bpr_yesno "nsw ") nsw bpr_typ_value x bpr_value y
  | Fadd(fmf, x, y)           -> bprintf b "fadd %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fsub(fmf, x, y)           -> bprintf b "fsub %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fmul(fmf, x, y)           -> bprintf b "fmul %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fdiv(fmf, x, y)           -> bprintf b "fdiv %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Frem(fmf, x, y)           -> bprintf b "frem %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Sdiv(e, x, y)           -> bprintf b "sdiv%a %a, %a" (bpr_yesno " exact") e bpr_typ_value x bpr_value y
  | Udiv(e, x, y)           -> bprintf b "udiv%a %a, %a" (bpr_yesno " exact") e bpr_typ_value x bpr_value y
  | Lshr(e, x, y)           -> bprintf b "lshr%a %a, %a" (bpr_yesno " exact") e bpr_typ_value x bpr_value y
  | Ashr(e, x, y)           -> bprintf b "ashr%a %a, %a" (bpr_yesno " exact") e bpr_typ_value x bpr_value y
  | Urem(x, y)           -> bprintf b "urem %a, %a" bpr_typ_value x bpr_value y
  | Srem(x, y)           -> bprintf b "srem %a, %a" bpr_typ_value x bpr_value y
  | And (x, y)           -> bprintf b "and %a, %a" bpr_typ_value x bpr_value y
  | Or  (x, y)           -> bprintf b "or %a, %a" bpr_typ_value x bpr_value y
  | Xor (x, y)           -> bprintf b "xor %a, %a" bpr_typ_value x bpr_value y
  | Icmp(icmp, x, y) ->
      bprintf b "icmp %a %a, %a" bpr_icmp icmp bpr_typ_value x bpr_value y
  | Fcmp(fcmp, x, y) ->
      bprintf b "icmp %a %a, %a" bpr_fcmp fcmp bpr_typ_value x bpr_value y
  | Trunc(x,y)          -> bprintf b "trunc %a to %a" bpr_typ_value x         bpr_typ y
  | Zext(x,y)           -> bprintf b "zext %a to %a" bpr_typ_value x          bpr_typ y
  | Sext(x,y)           -> bprintf b "sext %a to %a" bpr_typ_value x          bpr_typ y
  | Fptrunc(x,y)        -> bprintf b "fptrunc %a to %a" bpr_typ_value x       bpr_typ y
  | Fpext(x,y)          -> bprintf b "fpext %a to %a" bpr_typ_value x         bpr_typ y
  | Bitcast(x,y)        -> bprintf b "bitcast %a to %a" bpr_typ_value x       bpr_typ y
  | Addrspacecast(x,y)  -> bprintf b "addrspacecast %a to %a" bpr_typ_value x bpr_typ y
  | Uitofp(x,y)         -> bprintf b "uitofp %a to %a" bpr_typ_value x        bpr_typ y
  | Sitofp(x,y)         -> bprintf b "sitofp %a to %a" bpr_typ_value x        bpr_typ y
  | Fptoui(x,y)         -> bprintf b "fptoui %a to %a" bpr_typ_value x        bpr_typ y
  | Fptosi(x,y)         -> bprintf b "fptosi %a to %a" bpr_typ_value x        bpr_typ y
  | Inttoptr(x,y)       -> bprintf b "inttoptr %a to %a" bpr_typ_value x      bpr_typ y
  | Ptrtoint(x,y)       -> bprintf b "ptrtoint %a to %a" bpr_typ_value x      bpr_typ y
  | Va_arg(x,y)         -> bprintf b "va_arg %a, %a" bpr_typ_value x      bpr_typ y
  | Getelementptr(inbounds, x) -> bprintf b "getelementptr%a %a" (bpr_yesno " inbounds") inbounds bpr_typ_value_list x
  | Shufflevector x -> bprintf b "shufflevector %a" bpr_typ_value_list x
  | Insertelement x -> bprintf b "insertelement %a" bpr_typ_value_list x
  | Extractelement x -> bprintf b "extractelement %a" bpr_typ_value_list x
  | Select         x -> bprintf b "select %a" bpr_typ_value_list x
  | Phi(ty, incoming) ->
      bprintf b "phi %a " bpr_typ ty;
      let first = ref true in
      List.iter
        (fun (x,y) ->
          if not !first then bprintf b ", " else first := false;
          bprintf b "[ %a, %a ]" bpr_value x bpr_value y
        )
        incoming;
      bprintf b "\n"
  | Landingpad(x, y, z, w) ->
      let bpr_landingpad b = function
        | Catch(typ, value) ->  bprintf b "catch %a %a" bpr_typ typ bpr_value value
        | Filter(typ, value) -> bprintf b "filter %a %a" bpr_typ typ bpr_value value in
      let rec bpr b = function
        | [] -> ()
        | hd::tl -> bprintf b " %a" bpr_landingpad hd; bpr b tl in
      bprintf b "landingpad %a personality %a%a%a" bpr_typ x bpr_typ_value y (bpr_yesno " cleanup") z bpr w
  | Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs) ->
      if is_tail_call then bprintf b "tail ";
      bprintf b "call ";
      (opt bpr_callingconv) b callconv;
      (match callee_ty with
      | Pointer(Funtyp(rt,_,false),_) -> (* false=>not is_var_arg *)
          bprintf b "%a %a(%a) %a\n"
            bpr_typ rt bpr_value callee_name bpr_arguments operands bpr_attributes callattrs
      | _ ->
          bprintf b "%a %a(%a) %a\n"
            bpr_typ callee_ty bpr_value callee_name bpr_arguments operands bpr_attributes callattrs)
  | Alloca(x, y, z, w) ->
      bprintf b "alloca ";
      if x then bprintf b "inalloca ";
      bpr_typ b y;
      (match z with None -> () | Some q -> bprintf b ", %a" bpr_typ_value q);
      (match w with None -> () | Some q -> bprintf b ", align %d" q)
  | Load(x, y, z, w, v) ->
      bprintf b "load %a%a%a" (bpr_yesno "atomic ") x (bpr_yesno "volatile ") y bpr_typ_value z;
      (match w with None -> () | Some(q,r) -> if q then bprintf b " singlethread"; bprintf b " %a" bpr_ordering r);
      (match v with None -> () | Some q -> bprintf b ", align %d" q)
  | Store(x, y, z, w, v, u) ->
      bprintf b "store %a%a%a, %a" (bpr_yesno "atomic ") x (bpr_yesno "volatile ") y bpr_typ_value z bpr_typ_value w;
      (match v with None -> () | Some(q,r) -> if q then bprintf b " singlethread"; bprintf b " %a" bpr_ordering r);
      (match u with None -> () | Some q -> bprintf b ", align %d" q)
  | Cmpxchg(x, y, z, w, v, u, t) ->
      bprintf b "cmpxchg %a%a, %a, %a%a %a %a"
        (bpr_yesno "volatile ") x bpr_typ_value y bpr_typ_value z bpr_typ_value w
        (bpr_yesno " singlthread") v
        bpr_ordering u
        bpr_ordering t
  | Atomicrmw(x,y,z,w,v,u) ->
      bprintf b "atomicrmw %a%a %a, %a%a %a"
        (bpr_yesno "volatile ") x
        bpr_binop y
        bpr_typ_value z
        bpr_typ_value w
        (bpr_yesno " singlthread") v
        bpr_ordering u
  | Fence(x, y) ->
      bprintf b "fence %a%a"
        (bpr_yesno "singlthread ") x
        bpr_ordering y
  | Extractvalue(x, y) -> bprintf b "extractvalue %a%a)" bpr_typ_value x bpr_index_list y
  | Insertvalue(x, y, z) -> bprintf b "insertvalue %a, %a%a" bpr_typ_value x bpr_typ_value y bpr_index_list z
  | Unreachable -> bprintf b "unreachable"
  | Return None -> bprintf b "ret void"
  | Return(Some(x, y)) -> bprintf b "ret %a %a" bpr_typ x bpr_value y
  | Br(x, None) -> bprintf b "br %a" bpr_typ_value x
  | Br(x, Some(y, z)) -> bprintf b "br %a, %a, %a" bpr_typ_value x bpr_typ_value y bpr_typ_value z
  | Indirectbr(x, y) -> bprintf b "indirectbr %a, [%a]" bpr_typ_value x bpr_typ_value_list y
  | Resume x -> bprintf b "resume %a" bpr_typ_value x
  | Switch(x, y, z) -> bprintf b "switch %a, %a [%a]" bpr_typ_value x bpr_typ_value y
        (bpr_concat " "
           (fun b (c, d) -> bprintf b "%a, %a" bpr_typ_value c bpr_typ_value d)) z
  | Invoke(x, y, z, w, v, u, t, s) ->
      bprintf b "invoke %a%a%a %a(%a)%a to %a unwind %a" 
        (opt bpr_callingconv) x
        bpr_attributes y
        bpr_typ z
        bpr_value w
        bpr_arguments v
        bpr_attributes u
        bpr_typ_value t
        bpr_typ_value s);
  bprintf b "\n"

let bpr_block b (nameopt, instrs) =
(*  if block.b_name <> "0" then*)
    (* TODO: predecessor blocks *)
  bprintf b "; <label>:%s%a\n" 
    (match nameopt with
    | None -> "<<UNNAMED>>"
    | Some x -> x)
    pad_to_column 50;
  List.iter (bpr_instr b) instrs;
  bprintf b "\n"

let bpr_function b f =
  bprintf b "\n";
  if f.fblocks = [] then bprintf b "declare " else bprintf b "define ";
  (opt bpr_linkage) b f.flinkage;
  (opt bpr_visibility) b f.fvisibility;
  (opt bpr_storageclass) b f.fstorageclass;
  (opt bpr_callingconv) b f.fcallingconv;
  bpr_attributes b f.freturnattrs;
  bpr_typ b f.freturntyp;
  bprintf b " %a("  bpr_var f.fname;
  bpr_concat ", " bpr_typ b (fst f.fparams);
  (match f.fparams with
  | (_, false) -> ()
  | ([], true) -> bprintf b "..."
  | (_, true) -> bprintf b ", ...");
  bprintf b ")";
  List.iter (bprintf b " %a" bpr_attribute) f.fattrs;
  if f.fblocks = [] then bprintf b "\n" else begin
    bprintf b " {\n";
    List.iter (bpr_block b) f.fblocks;
    bprintf b "}\n";
  end

let bpr_mdnode b x =
  bprintf b "!%d = %a !%a\n" x.mdid bpr_typ x.mdtyp
    bpr_mdnodevector x.mdcontents

let bpr_cu b cu =
  (match cu.cdatalayout with
  | None -> ()
  | Some x -> bprintf b "target datalayout = %s\n" x);
  (match cu.ctarget with
  | None -> ()
  | Some x -> bprintf b "target triple = %s\n" x);
  List.iter (bprintf b "module asm %s\n") cu.casms;
  List.iter
    (function
      | (x, None) -> bprintf b "%a = type opaque\n" bpr_var x
      | (x, Some t) -> bprintf b "%a = type %a\n" bpr_var x bpr_typ t)
    cu.ctyps;
  List.iter (bpr_global b) cu.cglobals;
  List.iter (bpr_alias b) cu.caliases;
  List.iter (bpr_function b) cu.cfuns;
  List.iter (bpr_attrgrp b) cu.cattrgrps;
  List.iter
    (fun (x,l) ->
      bprintf b "!%s = { %s }\n" x (String.concat ", " (List.map string_of_int l)))
    cu.cmdvars;
  List.iter (bpr_mdnode b) cu.cmdnodes
