let third (a,b,c) = c

type var =
  | Id   of bool * int    (* global?, index *)
  | Name of bool * string (* global?, name *)

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
  | Funtyp  of typ * (typ * param_attribute list * var option) list * bool (* return type, param types, var_arg? *)
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
  | Basicblock     of var
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
  | Call of bool * callingconv option * return_attribute list * typ * value * (typ * param_attribute list * value) list * call_attribute list
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
  | Invoke of callingconv option * return_attribute list * typ * value * ((typ * param_attribute list * value) list) * function_attribute list * (typ * value) * (typ * value)

type binfo = {
    mutable bname: var;
    mutable binstrs: (var option * instr) list
  }      

type finfo = {
    mutable flinkage: linkage option;
    mutable fvisibility: visibility option;
    mutable fstorageclass: dll_storageclass option;
    mutable fcallingconv: callingconv option;
    mutable freturnattrs: return_attribute list;
    mutable freturntyp: typ;
    mutable fname: var;
    mutable fparams: ((typ * param_attribute list * var option) list * bool);
    mutable funnamed_addr: bool;
    mutable fattrs: function_attribute list;
    mutable fsection: string option;
    mutable falign: int option;
    mutable fgc: string option;
    mutable fprefix: (typ * value) option;
    mutable fblocks: binfo list;
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
    mutable gtyp: typ; (* actual type of the global is a pointer to this type *)
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

let between s bpr b =
  let rec loop = function
  | [] -> ()
  | [hd] -> bpr b hd
  | hd::tl ->
      bprintf b "%a%s" bpr hd s;
      loop tl in
  loop

let before s bpr b =
  let rec loop = function
  | [] -> ()
  | hd::tl ->
      bprintf b "%s%a" s bpr hd;
      loop tl in
  loop

let after s bpr b =
  let rec loop = function
  | [] -> ()
  | hd::tl ->
      bprintf b "%a%s" bpr hd s;
      loop tl in
  loop

let opt bpr b = function
  | None -> ()
  | Some x ->
      bprintf b "%a" bpr x

let opt_before s bpr b = function
  | None -> ()
  | Some x ->
      bprintf b "%s%a" s bpr x

let opt_after s bpr b = function
  | None -> ()
  | Some x ->
      bprintf b "%a%s" bpr x s

let yes s b x =
  if x then bprintf b "%s" s

let yesno syes sno b x =
  bprintf b "%s" (if x then syes else sno)

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

let bpr_fast_math_flags = between " " bpr_fast_math_flag

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
  | Attr(x, None) -> bprintf b "%s" x
  | Attr(x, Some y) -> bprintf b "%s=%s" x y
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
  between " " bpr_attribute

let bpr_attrgrp b (x, y) =
  bprintf b "attributes #%s = { %a }\n" x bpr_attributes y

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

let string_of_var v =
  let b = Buffer.create 11 in
  bpr_var b v;
  Buffer.contents b

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
        between ", " bpr_formal b param_tys;
        if is_var_arg then
          if param_tys = [] then
            bprintf b "..."
          else bprintf b ", ...";
        bprintf b ")"
    | Structtyp(packed, typs) ->
        bprintf b "%a{%a%a%a}%a"
          (yes "<") packed
          (yes " ") (typs <> [])
          (between ", " bpr_typ) typs
          (yes " ") (typs <> [])
          (yes ">") packed;
    | Arraytyp(len, element_ty) ->
        bprintf b "[%d x " len;
        pr element_ty;
        bprintf b "]"
    | Pointer(element_ty, address_space) ->
        pr element_ty;
        (match address_space with
        | None -> ()
        | Some 0 -> ()
        | Some x -> bprintf b " addrspace(%d)" x);
        bprintf b "*"
    | Vector(len, element_ty) ->
        bprintf b "<%d x " len;
        pr element_ty;
        bprintf b ">"
  in pr typ
and bpr_formal b (typ, pattrs, formal) =
  bprintf b "%a%a%a"
    bpr_typ typ
    (before " " bpr_attribute) pattrs
    (opt_before " " bpr_var) formal

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

let rec bpr_value b op = match op with
  | Var x          -> bpr_var b x
  | Basicblock x   -> bpr_var b x
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
  | Blockaddress(x, y) ->
      bprintf b "blockaddress(%a, %a)" bpr_value x bpr_value y
  | Array ops ->
      let is_string =
        List.for_all
          (function (Integer 8, Int _) -> true | _ -> false)
          ops in
      if is_string then begin
        bprintf b "c\"";
        List.iter
          (function
            | (_, Int x) ->
                let c = Char.chr(Big_int.int_of_big_int x) in
                let isprint c = c >= '\032' && c < '\128' in
                if (isprint c && c <> '\\' && c <> '\"') then
                  bprintf b "%c" c
                else
                  bprintf b "\\%.2X" (Char.code c)
            | _ (*impossible*) -> ())
          ops;
        bprintf b "\""
      end
      else
        bprintf b "[%a]" (between ", " bpr_typ_value) ops
  | Vector ops ->
      bprintf b "<%a>" (between ", " bpr_typ_value) ops
  | Struct(is_packed, ops) ->
      bprintf b "%a{%a%a%a}%a"
        (yes "<") is_packed
        (yes " ") (ops <> [])
        (between ", " bpr_typ_value) ops
        (yes " ") (ops <> [])
        (yes ">") is_packed
  | Trunc(x, y)          -> bprintf b "trunc(%a, %a)" bpr_typ_value x              bpr_typ y
  | Zext(x, y)           -> bprintf b "zext(%a, %a)" bpr_typ_value x          bpr_typ y
  | Sext(x, y)           -> bprintf b "sext(%a, %a)" bpr_typ_value x          bpr_typ y
  | Fptrunc(x, y)        -> bprintf b "fptrunc(%a, %a)" bpr_typ_value x       bpr_typ y
  | Fpext(x, y)          -> bprintf b "fpext(%a, %a)" bpr_typ_value x         bpr_typ y
  | Bitcast(x, y)        -> bprintf b "bitcast(%a, %a)" bpr_typ_value x       bpr_typ y
  | Addrspacecast(x, y)  -> bprintf b "addrspacecast(%a, %a)" bpr_typ_value x bpr_typ y
  | Uitofp(x, y)         -> bprintf b "uitofp(%a, %a)" bpr_typ_value x        bpr_typ y
  | Sitofp(x, y)         -> bprintf b "sitofp(%a, %a)" bpr_typ_value x        bpr_typ y
  | Fptoui(x, y)         -> bprintf b "fptoui(%a, %a)" bpr_typ_value x        bpr_typ y
  | Fptosi(x, y)         -> bprintf b "fptosi(%a, %a)" bpr_typ_value x        bpr_typ y
  | Inttoptr(x, y)       -> bprintf b "inttoptr(%a, %a)" bpr_typ_value x      bpr_typ y
  | Ptrtoint(x, y)       -> bprintf b "ptrtoint(%a, %a)" bpr_typ_value x      bpr_typ y
  | Extractvalue(x, y)   -> bprintf b "extractvalue(%a%a)" bpr_typ_value x bpr_index_list y
  | Insertvalue(x, y, z) -> bprintf b "insertvalue(%a, %a%a)" bpr_typ_value x bpr_typ_value y bpr_index_list z
  | Icmp(cmp, x, y)      -> bprintf b "icmp %a (%a, %a)" bpr_icmp cmp bpr_typ_value x bpr_typ_value y
  | Fcmp(cmp, x, y)      -> bprintf b "fcmp %a (%a, %a)" bpr_fcmp cmp bpr_typ_value x bpr_typ_value y
  | Sdiv(e, x, y)        -> bprintf b "sdiv%a(%a, %a)" (yes " exact ") e bpr_typ_value x bpr_typ_value y
  | Udiv(e, x, y)        -> bprintf b "udiv%a(%a, %a)" (yes " exact ") e bpr_typ_value x bpr_typ_value y
  | Lshr(e, x, y)        -> bprintf b "lshr%a(%a, %a)" (yes " exact ") e bpr_typ_value x bpr_typ_value y
  | Ashr(e, x, y)        -> bprintf b "ashr%a(%a, %a)" (yes " exact ") e bpr_typ_value x bpr_typ_value y
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
  | Getelementptr(inbounds, x) -> bprintf b "getelementptr%a(%a)" (yes " inbounds ") inbounds bpr_typ_value_list x
  | Shufflevector x      -> bprintf b "shufflevector(%a)" bpr_typ_value_list x
  | Insertelement x      -> bprintf b "insertelement(%a)" bpr_typ_value_list x
  | Extractelement x     -> bprintf b "extractelement(%a)" bpr_typ_value_list x
  | Select x             -> bprintf b "select(%a)" bpr_typ_value_list x
  | Add(nuw, nsw, x, y)  -> bprintf b "add%a%a(%a, %a)" (yes " nuw") nuw (yes " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Sub(nuw, nsw, x, y)  -> bprintf b "sub%a%a(%a, %a)" (yes " nuw") nuw (yes " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Mul(nuw, nsw, x, y)  -> bprintf b "mul%a%a(%a, %a)" (yes " nuw") nuw (yes " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Shl(nuw, nsw, x, y)  -> bprintf b "shl%a%a(%a, %a)" (yes " nuw") nuw (yes " nsw ") nsw bpr_typ_value x bpr_typ_value y
  | Asm(sideeffect, alignstack, inteldialect, x, y) ->
      bprintf b "asm %a%a%a %s, %s"
        (yes "sideeffect ") sideeffect
        (yes "alignstack ") alignstack
        (yes "inteldialect ") inteldialect
        x
        y

and bpr_typ_value b (typ, value) =
  bprintf b "%a %a" bpr_typ typ bpr_value value

and bpr_typ_value_list b l =
  (match l with [] -> ()
  | (ty, op)::tl ->
      let all_same_type = List.for_all (fun (ty', _) -> ty=ty') tl in
      if all_same_type then begin
        bprintf b "%a %a" bpr_typ ty bpr_value op;
        List.iter (fun (_, op) -> bprintf b ", %a" bpr_value op) tl
      end else
        between ", " (fun b (ty, op) -> bprintf b "%a %a" bpr_typ ty bpr_value op) b l)

and bpr_mdnodevector b x =
  let bpr b = function
    | None -> bprintf b "null"
    | Some y -> bpr_typ_value b y in
  bprintf b "{%a}" (between ", " bpr) x

let bpr_global b g =
  bprintf b "%a = %a%a%a%a%a%a%a%a%a%a%a%a\n"
    bpr_var g.gname
    (opt_after " " bpr_linkage) g.glinkage
    (opt_after " " bpr_visibility) g.gvisibility
    (opt_after " " bpr_storageclass) g.gstorageclass
    (opt_after " " bpr_thread_local) g.gthread_local
    (opt_after " " (fun b -> bprintf b "addrspace(%d)")) g.gaddrspace
    (yes "unnamed_addr ") g.gunnamed_addr
    (yes "externally_initialized ") g.gexternally_initialized
    (yesno "constant " "global ") g.gconstant
    bpr_typ g.gtyp
    (opt_before " " bpr_value) g.gvalue
    (opt (fun b -> bprintf b ", section %s")) g.gsection
    (opt (fun b -> bprintf b ", align %d")) g.galign

let bpr_alias b a =
  bprintf b "%a = " bpr_var a.aname;
  (match a.avisibility with None -> () | Some x -> bprintf b "%a " bpr_visibility x);
  bprintf b "alias ";
  (match a.alinkage with None -> () | Some x -> bprintf b "%a " bpr_linkage x);
  (match a.aaliasee with
  | A_bitcast(x, y) -> bprintf b "bitcast(%a to %a)\n" bpr_typ_value x bpr_typ y
  | A_getelementptr(inbounds, x) -> bprintf b "getelementptr%a(%a)\n" (yes " inbounds ") inbounds bpr_typ_value_list x
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

let bpr_arguments =
  let bpr b (typ, param_attributes, value) =
    bprintf b "%a %a%a"
      bpr_typ typ
      (after " " bpr_attribute) param_attributes
      bpr_value value in
  between ", " bpr

let bpr_ordering b = function
| Unordered -> bprintf b "unordered"
| Monotonic -> bprintf b "monotonic"
| Acquire   -> bprintf b "acquire"
| Release   -> bprintf b "release"
| Acq_rel   -> bprintf b "acq_rel"
| Seq_cst   -> bprintf b "seq_cst"

let bpr_instr b (nopt, i) =
  bprintf b "  ";
  (match nopt with None -> ()
  | Some n -> bprintf b "%a = " bpr_var n);
  (match i with
  | Add(nuw, nsw, x, y) ->
      bprintf b "add %a%a%a, %a" (yes "nuw ") nuw (yes "nsw ") nsw bpr_typ_value x bpr_value y
  | Sub(nuw, nsw, x, y) ->
      bprintf b "sub %a%a%a, %a" (yes "nuw ") nuw (yes "nsw ") nsw bpr_typ_value x bpr_value y
  | Mul(nuw, nsw, x, y) ->
      bprintf b "mul %a%a%a, %a" (yes "nuw ") nuw (yes "nsw ") nsw bpr_typ_value x bpr_value y
  | Shl(nuw, nsw, x, y) ->
      bprintf b "shl %a%a%a, %a" (yes "nuw ") nuw (yes "nsw ") nsw bpr_typ_value x bpr_value y
  | Fadd(fmf, x, y)           ->
      bprintf b "fadd %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fsub(fmf, x, y)           ->
      bprintf b "fsub %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fmul(fmf, x, y)           ->
      bprintf b "fmul %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Fdiv(fmf, x, y)           ->
      bprintf b "fdiv %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Frem(fmf, x, y)           ->
      bprintf b "frem %a%a, %a" bpr_fast_math_flags fmf bpr_typ_value x bpr_value y
  | Sdiv(e, x, y)           ->
      bprintf b "sdiv%a %a, %a" (yes " exact") e bpr_typ_value x bpr_value y
  | Udiv(e, x, y)           ->
      bprintf b "udiv%a %a, %a" (yes " exact") e bpr_typ_value x bpr_value y
  | Lshr(e, x, y)           ->
      bprintf b "lshr%a %a, %a" (yes " exact") e bpr_typ_value x bpr_value y
  | Ashr(e, x, y)           ->
      bprintf b "ashr%a %a, %a" (yes " exact") e bpr_typ_value x bpr_value y
  | Urem(x, y)           ->
      bprintf b "urem %a, %a" bpr_typ_value x bpr_value y
  | Srem(x, y)           ->
      bprintf b "srem %a, %a" bpr_typ_value x bpr_value y
  | And (x, y)           ->
      bprintf b "and %a, %a" bpr_typ_value x bpr_value y
  | Or  (x, y)           ->
      bprintf b "or %a, %a" bpr_typ_value x bpr_value y
  | Xor (x, y)           ->
      bprintf b "xor %a, %a" bpr_typ_value x bpr_value y
  | Icmp(icmp, x, y) ->
      bprintf b "icmp %a %a, %a" bpr_icmp icmp bpr_typ_value x bpr_value y
  | Fcmp(fcmp, x, y) ->
      bprintf b "icmp %a %a, %a" bpr_fcmp fcmp bpr_typ_value x bpr_value y
  | Trunc(x, y)          ->
      bprintf b "trunc %a to %a" bpr_typ_value x         bpr_typ y
  | Zext(x, y)           ->
      bprintf b "zext %a to %a" bpr_typ_value x          bpr_typ y
  | Sext(x, y)           ->
      bprintf b "sext %a to %a" bpr_typ_value x          bpr_typ y
  | Fptrunc(x, y)        ->
      bprintf b "fptrunc %a to %a" bpr_typ_value x       bpr_typ y
  | Fpext(x, y)          ->
      bprintf b "fpext %a to %a" bpr_typ_value x         bpr_typ y
  | Bitcast(x, y)        ->
      bprintf b "bitcast %a to %a" bpr_typ_value x       bpr_typ y
  | Addrspacecast(x, y)  ->
      bprintf b "addrspacecast %a to %a" bpr_typ_value x bpr_typ y
  | Uitofp(x, y)         ->
      bprintf b "uitofp %a to %a" bpr_typ_value x        bpr_typ y
  | Sitofp(x, y)         ->
      bprintf b "sitofp %a to %a" bpr_typ_value x        bpr_typ y
  | Fptoui(x, y)         ->
      bprintf b "fptoui %a to %a" bpr_typ_value x        bpr_typ y
  | Fptosi(x, y)         ->
      bprintf b "fptosi %a to %a" bpr_typ_value x        bpr_typ y
  | Inttoptr(x, y)       ->
      bprintf b "inttoptr %a to %a" bpr_typ_value x      bpr_typ y
  | Ptrtoint(x, y)       ->
      bprintf b "ptrtoint %a to %a" bpr_typ_value x      bpr_typ y
  | Va_arg(x, y)         ->
      bprintf b "va_arg %a, %a" bpr_typ_value x      bpr_typ y
  | Getelementptr(inbounds, x) ->
      bprintf b "getelementptr%a %a" (yes " inbounds") inbounds bpr_typ_value_list x
  | Shufflevector x ->
      bprintf b "shufflevector %a" bpr_typ_value_list x
  | Insertelement x ->
      bprintf b "insertelement %a" bpr_typ_value_list x
  | Extractelement x ->
      bprintf b "extractelement %a" bpr_typ_value_list x
  | Select x ->
      bprintf b "select %a" bpr_typ_value_list x
  | Phi(ty, incoming) ->
      bprintf b "phi %a " bpr_typ ty;
      let first = ref true in
      List.iter
        (fun (x, y) ->
          if not !first then bprintf b ", " else first := false;
          bprintf b "[ %a, %a ]" bpr_value x bpr_value y)
        incoming
  | Landingpad(x, y, z, w) ->
      let bpr_landingpad b = function
        | Catch(typ, value) ->  bprintf b "catch %a %a" bpr_typ typ bpr_value value
        | Filter(typ, value) -> bprintf b "filter %a %a" bpr_typ typ bpr_value value in
      let rec bpr b = function
        | [] -> ()
        | hd::tl -> bprintf b " %a" bpr_landingpad hd; bpr b tl in
      bprintf b "landingpad %a personality %a%a%a" bpr_typ x bpr_typ_value y (yes " cleanup") z bpr w
  | Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs) ->
      if is_tail_call then bprintf b "tail ";
      bprintf b "call ";
      (opt bpr_callingconv) b callconv;
      bprintf b "%a %a(%a)%a"
        bpr_typ callee_ty bpr_value callee_name bpr_arguments operands (before " " bpr_attribute) callattrs
  | Alloca(x, y, z, w) ->
      bprintf b "alloca ";
      if x then bprintf b "inalloca ";
      bpr_typ b y;
      (match z with None -> () | Some q -> bprintf b ", %a" bpr_typ_value q);
      (match w with None -> () | Some q -> bprintf b ", align %d" q)
  | Load(x, y, z, w, v) ->
      bprintf b "load %a%a%a" (yes "atomic ") x (yes "volatile ") y bpr_typ_value z;
      (match w with None -> () | Some(q, r) -> if q then bprintf b " singlethread"; bprintf b " %a" bpr_ordering r);
      (match v with None -> () | Some q -> bprintf b ", align %d" q)
  | Store(x, y, z, w, v, u) ->
      bprintf b "store %a%a%a, %a" (yes "atomic ") x (yes "volatile ") y bpr_typ_value z bpr_typ_value w;
      (match v with None -> () | Some(q, r) -> if q then bprintf b " singlethread"; bprintf b " %a" bpr_ordering r);
      (match u with None -> () | Some q -> bprintf b ", align %d" q)
  | Cmpxchg(x, y, z, w, v, u, t) ->
      bprintf b "cmpxchg %a%a, %a, %a%a %a %a"
        (yes "volatile ") x bpr_typ_value y bpr_typ_value z bpr_typ_value w
        (yes " singlthread") v
        bpr_ordering u
        bpr_ordering t
  | Atomicrmw(x, y, z, w, v, u) ->
      bprintf b "atomicrmw %a%a %a, %a%a %a"
        (yes "volatile ") x
        bpr_binop y
        bpr_typ_value z
        bpr_typ_value w
        (yes " singlthread") v
        bpr_ordering u
  | Fence(x, y) ->
      bprintf b "fence %a%a"
        (yes "singlthread ") x
        bpr_ordering y
  | Extractvalue(x, y) ->
      bprintf b "extractvalue %a%a" bpr_typ_value x bpr_index_list y
  | Insertvalue(x, y, z) ->
      bprintf b "insertvalue %a, %a%a" bpr_typ_value x bpr_typ_value y bpr_index_list z
  | Unreachable ->
      bprintf b "unreachable"
  | Return None ->
      bprintf b "ret void"
  | Return(Some(x, y)) ->
      bprintf b "ret %a %a" bpr_typ x bpr_value y
  | Br(x, None) ->
      bprintf b "br %a" bpr_typ_value x
  | Br(x, Some(y, z)) ->
      bprintf b "br %a, %a, %a" bpr_typ_value x bpr_typ_value y bpr_typ_value z
  | Indirectbr(x, y) ->
      bprintf b "indirectbr %a, [%a]" bpr_typ_value x bpr_typ_value_list y
  | Resume x ->
      bprintf b "resume %a" bpr_typ_value x
  | Switch(x, y, z) ->
      bprintf b "switch %a, %a [%a]" bpr_typ_value x bpr_typ_value y
        (between " "
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

let bpr_block b {bname; binstrs} =
  (* TODO: predecessor blocks *)
  (match bname with
    | Id(false, 0) -> () (* llvm does not print a label comment for block 0 *)
    | Id(_, x) -> bprintf b "; <label>:%d%a\n" x pad_to_column 50
    | Name(_, x) -> bprintf b "; <label>:%s%a\n" x pad_to_column 50);
  List.iter (bpr_instr b) binstrs

let bpr_function b f =
  bprintf b "\n";
  bprintf b "%a %a%a%a%a%a%a %a(%a%a%a)%a"
    (yesno "declare" "define") (f.fblocks = [])

    (opt_after " " bpr_linkage) f.flinkage
    (opt_after " " bpr_visibility) f.fvisibility
    (opt_after " " bpr_storageclass) f.fstorageclass
    (opt_after " " bpr_callingconv) f.fcallingconv
    (after " " bpr_attribute) f.freturnattrs
    bpr_typ f.freturntyp

    bpr_var f.fname

    (between ", " bpr_formal) (fst f.fparams)
    (yes ", ") (snd f.fparams && (fst f.fparams) <> [])
    (yes "...") (snd f.fparams)

    (before " " bpr_attribute) f.fattrs;
  if f.fblocks = [] then bprintf b "\n" else begin
    bprintf b " {\n%a}\n"
      (between "\n" bpr_block) f.fblocks;
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
  bprintf b "\n";
  List.iter
    (function
      | (x, None)   -> bprintf b "%a = type opaque\n" bpr_var x
      | (x, Some t) -> bprintf b "%a = type %a\n" bpr_var x bpr_typ t)
    cu.ctyps;
  if cu.cglobals <> [] then bprintf b "\n";
  List.iter (bpr_global b) cu.cglobals;
  if cu.caliases <> [] then bprintf b "\n";
  List.iter (bpr_alias b) cu.caliases;
  List.iter (bpr_function b) cu.cfuns;
  if cu.cattrgrps <> [] then bprintf b "\n";
  List.iter (bpr_attrgrp b) cu.cattrgrps;
  if cu.cmdvars <> [] then bprintf b "\n";
  List.iter
    (fun (x, l) ->
      bprintf b "!%s = !{%s}\n" x (String.concat " " (List.map (fun y -> "!"^(string_of_int y)) l)))
    cu.cmdvars;
  if cu.cmdnodes <> [] then bprintf b "\n";
  List.iter (bpr_mdnode b) cu.cmdnodes

let spr bpr x =
  let b = Buffer.create 11 in
  bpr b x;
  Buffer.contents b

module VSet = Set.Make(
  struct
    type t = var
    let compare = compare
  end)

let free_of_var = function
| Name(true,_) -> VSet.empty (* globals are not free *)
| Id(true,_) -> VSet.empty (* globals are not free *)
| x -> VSet.singleton x

let rec free_of_value = function
| Var x -> free_of_var x
| Basicblock x -> VSet.empty
| Struct(_,ops)
| Vector ops
| Array ops ->
    List.fold_left VSet.union VSet.empty
      (List.map free_of_value (List.map snd ops))
| Blockaddress _
| Null
| True
| False
| Zero
| Float _
| Int _
| Undef -> VSet.empty
| Asm _
| Mdnode _
| Mdstring _
| Mdnodevector _     -> VSet.empty
| Trunc          ((_,v),_)
| Zext           ((_,v),_)
| Sext           ((_,v),_)
| Fptrunc        ((_,v),_)
| Fpext          ((_,v),_)
| Bitcast        ((_,v),_)
| Addrspacecast  ((_,v),_)
| Uitofp         ((_,v),_)
| Sitofp         ((_,v),_)
| Fptoui         ((_,v),_)
| Fptosi         ((_,v),_)
| Inttoptr       ((_,v),_)
| Ptrtoint       ((_,v),_)
| Extractvalue   ((_,v),_) ->
    free_of_value v
| Insertvalue    ((_,v1),(_,v2),_)
| Icmp           (_,(_,v1),(_,v2))
| Fcmp           (_,(_,v1),(_,v2))
| Sdiv           (_,(_,v1),(_,v2))
| Udiv           (_,(_,v1),(_,v2))
| Lshr           (_,(_,v1),(_,v2))
| Ashr           (_,(_,v1),(_,v2))
| Fadd           ((_,v1),(_,v2))
| Fsub           ((_,v1),(_,v2))
| Fmul           ((_,v1),(_,v2))
| Fdiv           ((_,v1),(_,v2))
| Urem           ((_,v1),(_,v2))
| Srem           ((_,v1),(_,v2))
| Frem           ((_,v1),(_,v2))
| And            ((_,v1),(_,v2))
| Or             ((_,v1),(_,v2))
| Xor            ((_,v1),(_,v2))
| Add            (_,_,(_,v1),(_,v2))
| Sub            (_,_,(_,v1),(_,v2))
| Mul            (_,_,(_,v1),(_,v2))
| Shl            (_,_,(_,v1),(_,v2)) ->
    VSet.union (free_of_value v1) (free_of_value v2)
| Getelementptr  (_,vs)
| Shufflevector  vs
| Insertelement  vs
| Extractelement vs
| Select         vs ->
    List.fold_left VSet.union VSet.empty
      (List.map free_of_value (List.map snd vs))

let free_of_instruction = function
| Va_arg         ((_,v),_)
| Trunc          ((_,v),_)
| Zext           ((_,v),_)
| Sext           ((_,v),_)
| Fptrunc        ((_,v),_)
| Fpext          ((_,v),_)
| Bitcast        ((_,v),_)
| Addrspacecast  ((_,v),_)
| Uitofp         ((_,v),_)
| Sitofp         ((_,v),_)
| Fptoui         ((_,v),_)
| Fptosi         ((_,v),_)
| Inttoptr       ((_,v),_)
| Ptrtoint       ((_,v),_)
| Extractvalue   ((_,v),_) ->
    free_of_value v
| Insertvalue    ((_,v1),(_,v2),_)
| Icmp           (_,(_,v1),v2)
| Fcmp           (_,(_,v1),v2)
| Sdiv           (_,(_,v1),v2)
| Udiv           (_,(_,v1),v2)
| Lshr           (_,(_,v1),v2)
| Ashr           (_,(_,v1),v2)
| Fadd           (_,(_,v1),v2)
| Fsub           (_,(_,v1),v2)
| Fmul           (_,(_,v1),v2)
| Fdiv           (_,(_,v1),v2)
| Frem           (_,(_,v1),v2)
| Urem           ((_,v1),v2)
| Srem           ((_,v1),v2)
| And            ((_,v1),v2)
| Or             ((_,v1),v2)
| Xor            ((_,v1),v2)
| Add            (_,_,(_,v1),v2)
| Sub            (_,_,(_,v1),v2)
| Mul            (_,_,(_,v1),v2)
| Shl            (_,_,(_,v1),v2) ->
    VSet.union (free_of_value v1) (free_of_value v2)
| Getelementptr  (_,vs)
| Shufflevector  vs
| Insertelement  vs
| Extractelement vs
| Select         vs ->
    List.fold_left VSet.union VSet.empty
      (List.map free_of_value (List.map snd vs))
| Phi(_,l) ->
    List.fold_left VSet.union VSet.empty
      (List.map (fun (v1, v2) -> VSet.union (free_of_value v1) (free_of_value v2)) l)
| Alloca(_,_,None,_) -> VSet.empty
| Alloca(_,_,Some(_,v),_)
| Load(_,_,(_,v),_,_) -> free_of_value v
| Store(_,_,(_,v1),(_,v2),_,_)
| Atomicrmw(_,_,(_,v1),(_,v2),_,_) -> VSet.union (free_of_value v1) (free_of_value v2)
| Cmpxchg(_,(_,v1),(_,v2),(_,v3),_,_,_) -> VSet.union (VSet.union (free_of_value v1) (free_of_value v2)) (free_of_value v3)
| Fence _
| Unreachable
| Return None -> VSet.empty
| Return Some(_,v)
| Resume(_,v) -> free_of_value v
| Br((_,v),None) -> free_of_value v
| Br((_,v1),Some((_,v2),(_,v3))) -> VSet.union (VSet.union (free_of_value v1) (free_of_value v2)) (free_of_value v3)
| Indirectbr((_,v),vs) ->
    VSet.union (free_of_value v) 
      (List.fold_left VSet.union VSet.empty
         (List.map free_of_value (List.map snd vs)))
| Switch(tv1,tv2,tvs) ->
    (List.fold_left VSet.union VSet.empty
       (List.map free_of_value (List.map snd (tv1::tv2::(List.map snd tvs)))))
| Landingpad(_,(_,v),_,lps) ->
    (List.fold_left VSet.union VSet.empty
       (List.map free_of_value (v::(List.map (function Catch(_,v) -> v | Filter(_,v) -> v) lps))))
| Call(_,_,_,_,v,params,_) ->
    let vs = v::(List.map third params) in
    List.fold_left VSet.union VSet.empty (List.map free_of_value vs)
| Invoke(_,_,_,v,params,_,tv1,tv2) ->
    let vs = v::(snd tv1)::(snd tv2)::(List.map third params) in
    List.fold_left VSet.union VSet.empty (List.map free_of_value vs)

(*
  | Landingpad of typ * (typ * value) * bool * landingpad list
*)


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
  loop bl.binstrs

let assigned_of_block bl =
  let rec loop = function
    | [] -> VSet.empty
    | (None,i)::tl -> loop tl
    | (Some x,i)::tl -> VSet.union (VSet.singleton x) (loop tl) in
  loop bl.binstrs

let value_map g f =
  let h = List.map (fun (ty,op) -> (ty, g op)) in
  let imap = function
    | Va_arg         ((t,v),t2) -> Va_arg         ((t,g v),t2)
    | Trunc          ((t,v),t2) -> Trunc          ((t,g v),t2)
    | Zext           ((t,v),t2) -> Zext           ((t,g v),t2)
    | Sext           ((t,v),t2) -> Sext           ((t,g v),t2)
    | Fptrunc        ((t,v),t2) -> Fptrunc        ((t,g v),t2)
    | Fpext          ((t,v),t2) -> Fpext          ((t,g v),t2)
    | Bitcast        ((t,v),t2) -> Bitcast        ((t,g v),t2)
    | Addrspacecast  ((t,v),t2) -> Addrspacecast  ((t,g v),t2)
    | Uitofp         ((t,v),t2) -> Uitofp         ((t,g v),t2)
    | Sitofp         ((t,v),t2) -> Sitofp         ((t,g v),t2)
    | Fptoui         ((t,v),t2) -> Fptoui         ((t,g v),t2)
    | Fptosi         ((t,v),t2) -> Fptosi         ((t,g v),t2)
    | Inttoptr       ((t,v),t2) -> Inttoptr       ((t,g v),t2)
    | Ptrtoint       ((t,v),t2) -> Ptrtoint       ((t,g v),t2)
    | Extractvalue   ((t,v),t2) -> Extractvalue   ((t,g v),t2)
    | Insertvalue    ((t1,v1),(t2,v2),il) -> Insertvalue    ((t1,g v1),(t2,g v2),il)
    | Icmp           (a,(t,v1),v2) -> Icmp           (a,(t,g v1),v2)
    | Fcmp           (a,(t,v1),v2) -> Fcmp           (a,(t,g v1),v2)
    | Sdiv           (a,(t,v1),v2) -> Sdiv           (a,(t,g v1),v2)
    | Udiv           (a,(t,v1),v2) -> Udiv           (a,(t,g v1),v2)
    | Lshr           (a,(t,v1),v2) -> Lshr           (a,(t,g v1),v2)
    | Ashr           (a,(t,v1),v2) -> Ashr           (a,(t,g v1),v2)
    | Fadd           (a,(t,v1),v2) -> Fadd           (a,(t,g v1),v2)
    | Fsub           (a,(t,v1),v2) -> Fsub           (a,(t,g v1),v2)
    | Fmul           (a,(t,v1),v2) -> Fmul           (a,(t,g v1),v2)
    | Fdiv           (a,(t,v1),v2) -> Fdiv           (a,(t,g v1),v2)
    | Frem           (a,(t,v1),v2) -> Frem           (a,(t,g v1),v2)
    | Urem           ((t,v1),v2)   -> Urem           ((t,g v1),g v2)
    | Srem           ((t,v1),v2)   -> Srem           ((t,g v1),g v2)
    | And            ((t,v1),v2)   -> And            ((t,g v1),g v2)
    | Or             ((t,v1),v2)   -> Or             ((t,g v1),g v2)
    | Xor            ((t,v1),v2)   -> Xor            ((t,g v1),g v2)
    | Add            (a,b,(t,v1),v2) -> Add            (a,b,(t,g v1),g v2)
    | Sub            (a,b,(t,v1),v2) -> Sub            (a,b,(t,g v1),g v2)
    | Mul            (a,b,(t,v1),v2) -> Mul            (a,b,(t,g v1),g v2)
    | Shl            (a,b,(t,v1),v2) -> Shl            (a,b,(t,g v1),g v2)
    | Getelementptr  (a,vs) -> Getelementptr  (a,h vs)
    | Shufflevector  vs -> Shufflevector  (h vs)
    | Insertelement  vs -> Insertelement  (h vs)
    | Extractelement vs -> Extractelement (h vs)
    | Select         vs -> Select         (h vs)
    | Phi(t,l) -> Phi(t, List.map (fun (v1,v2) -> (g v1, g v2)) l)
    | Alloca(a,b,None,c) -> Alloca(a,b,None,c)
    | Alloca(a,b,Some(t,v),c) -> Alloca(a,b,Some(t,g v),c)
    | Load(a,b,(t,v),c,d) -> Load(a,b,(t,g v),c,d)
    | Store(a,b,(t1,v1),(t2,v2),c,d) -> Store(a,b,(t1,g v1),(t2,g v2),c,d)
    | Atomicrmw(a,b,(t1,v1),(t2,v2),c,d) -> Atomicrmw(a,b,(t1,g v1),(t2,g v2),c,d)
    | Cmpxchg(a,(t1,v1),(t2,v2),(t3,v3),b,c,d) -> Cmpxchg(a,(t1,g v1),(t2,g v2),(t3,g v3),b,c,d)
    | Fence(a,b) -> Fence(a,b)
    | Unreachable -> Unreachable
    | Return None -> Return None
    | Return(Some(t,v)) -> Return(Some(t,g v))
    | Resume(t,v) -> Resume(t,g v)
    | Br((t,v),None) -> Br((t,g v),None)
    | Br((t1,v1),Some((t2,v2),(t3,v3))) -> Br((t1,g v1),Some((t2,g v2),(t3,g v3)))
    | Indirectbr((t,v),vs) -> Indirectbr((t,g v),h vs)
    | Switch((t1,v1),(t2,v2),tvs) ->
        Switch((t1,g v1),(t2,g v2),
               List.map (fun ((t1,v1),(t2,v2)) -> ((t1,g v1),(t2,g v2))) tvs)
    | Landingpad(a,(t,v),b,lps) ->
        Landingpad(a,(t,g v),b,
                   List.map (function Catch(t,v) -> Catch(t,g v) | Filter(t,v) -> Filter(t,g v)) lps)
    | Call(a,b,c,d,v,params,e) ->
        Call(a,b,c,d,g v,
             List.map (fun (a,b,v) -> (a,b,g v)) params,
             e)
    | Invoke(a,b,c,v,params,d,(t1,v1),(t2,v2)) ->
        Invoke(a,b,c,g v,
               List.map (fun (a,b,v) -> (a,b,g v)) params,
               d,(t1,g v1),(t2,g v2)) in
  (f.fblocks <-
    (List.map
      (fun bl ->
        {bname=bl.bname; binstrs=List.map (fun (nopt,i) -> (nopt, imap i)) bl.binstrs})
      f.fblocks));
  ()
