%{

(* Parser for LLVM IR.  Needs to be kept up to date with the LLVM equivalent, see: *)
(*  https://github.com/llvm-mirror/llvm/commits/master/lib/AsmParser/LLParser.cpp  *)

type toplevel =
  | Fun of Util.finfo
  | Asm of string
  | Target of string
  | Datalayout of string
  | Deplibs of string list
  | Typ of Util.var * Util.typ option
  | Global of Util.ginfo
  | Alias of Util.ainfo
  | MDNodeDefn of Util.mdinfo
  | MDVarDefn of string * int list
  | Attrgrp of int * Util.attribute list

let list_of_string s =
  if String.length s < 2 || String.get s 0 <> '"' || String.get s (String.length s - 1) <> '"' then
    failwith "list_of_string: expected quoted string constant";
  let l = ref [] in
  for i = (String.length s - 2) downto 1 do
    l := (String.get s i)::(!l)
  done;
  let is_hexdigit c =
    ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('A' <= c && c <= 'F') in
  let rec build = function
    | '\\'::x::y::tl when is_hexdigit x && is_hexdigit y ->
        (Scanf.sscanf (Printf.sprintf "%c%c" x y) "%x" (fun i -> i))::(build tl)
    | [] -> []
    | hd::tl -> (Char.code hd)::(build tl) in
  List.map
    (fun i -> (Util.Integer 8, Util.Int(Big_int.big_int_of_int i)))
    (build !l)

let process_toplevels t =
  let cu = {
    Util.ctarget=None;
    Util.cdatalayout=None;
    Util.casms=[];
    Util.cfuns=[];
    Util.ctyps=[];
    Util.cglobals=[];
    Util.caliases=[];
    Util.cmdnodes=[];
    Util.cmdvars=[];
    Util.cattrgrps=[];
  } in
  let proc = function
    | Fun fi -> cu.Util.cfuns <- fi::cu.Util.cfuns
    | Asm x -> cu.Util.casms <- x::cu.Util.casms
    | Target x ->
        if cu.Util.ctarget<>None then failwith "compilation unit with multiple targets"
        else cu.Util.ctarget <- Some x
    | Datalayout x ->
        if cu.Util.cdatalayout<>None then failwith "compilation unit with multiple datalayouts"
        else cu.Util.cdatalayout <- Some x
    | Deplibs _ -> () (* parses but ignored in LLVM 3.4, to be removed in 4.0 *)
    | Typ(x,y) -> cu.Util.ctyps <- (x,y)::cu.Util.ctyps
    | Global x -> cu.Util.cglobals <- x::cu.Util.cglobals
    | Alias x -> cu.Util.caliases <- x::cu.Util.caliases
    | MDNodeDefn x -> cu.Util.cmdnodes <- x::cu.Util.cmdnodes
    | MDVarDefn(x,y) -> cu.Util.cmdvars <- (x,y)::cu.Util.cmdvars
    | Attrgrp(x,y) -> cu.Util.cattrgrps <- (x,y)::cu.Util.cattrgrps in
  List.iter proc (List.rev t);
  cu

%}
%token <string> APFloat
%token <string> APInt
%token <string> APSint
%token <int> AttrGrpID
%token Backslash
%token Comma
%token DotDotDot
%token Eof
%token Equal
%token Exclaim
%token <Util.var> GlobalVar
%token <Util.var> GlobalID
%token <Util.var> LocalVar
%token <Util.var> LocalVarID
%token Greater
%token <string> LabelStr
%token Lbrace
%token Less
%token Lparen
%token Lsquare
%token <string> MetadataVar
%token Rbrace
%token Rparen
%token Rsquare
%token Star
%token <string> StringConstant
%token Error
%token Kw_void
%token <int> I
%token Kw_half
%token Kw_float
%token Kw_double
%token Kw_x86_fp80
%token Kw_fp128
%token Kw_ppc_fp128
%token Kw_label
%token Kw_metadata
%token Kw_x86_mmx
%token Kw_true
%token Kw_false
%token Kw_declare
%token Kw_define
%token Kw_global
%token Kw_constant
%token Kw_private
%token Kw_internal
%token Kw_linker_private
%token Kw_linker_private_weak
%token Kw_available_externally
%token Kw_linkonce
%token Kw_linkonce_odr
%token Kw_weak
%token Kw_weak_odr
%token Kw_appending
%token Kw_dllimport
%token Kw_dllexport
%token Kw_common
%token Kw_default
%token Kw_hidden
%token Kw_protected
%token Kw_unnamed_addr
%token Kw_externally_initialized
%token Kw_extern_weak
%token Kw_external
%token Kw_thread_local
%token Kw_localdynamic
%token Kw_initialexec
%token Kw_localexec
%token Kw_zeroinitializer
%token Kw_undef
%token Kw_null
%token Kw_to
%token Kw_tail
%token Kw_musttail
%token Kw_target
%token Kw_triple
%token Kw_unwind
%token Kw_deplibs
%token Kw_datalayout
%token Kw_volatile
%token Kw_atomic
%token Kw_unordered
%token Kw_monotonic
%token Kw_acquire
%token Kw_release
%token Kw_acq_rel
%token Kw_seq_cst
%token Kw_singlethread
%token Kw_nnan
%token Kw_ninf
%token Kw_nsz
%token Kw_arcp
%token Kw_fast
%token Kw_nuw
%token Kw_nsw
%token Kw_exact
%token Kw_inbounds
%token Kw_align
%token Kw_addrspace
%token Kw_section
%token Kw_alias
%token Kw_module
%token Kw_asm
%token Kw_sideeffect
%token Kw_alignstack
%token Kw_inteldialect
%token Kw_gc
%token Kw_prefix
%token Kw_ccc
%token Kw_fastcc
%token Kw_coldcc
%token Kw_x86_stdcallcc
%token Kw_x86_fastcallcc
%token Kw_x86_thiscallcc
%token Kw_x86_cdeclmethodcc
%token Kw_arm_apcscc
%token Kw_arm_aapcscc
%token Kw_arm_aapcs_vfpcc
%token Kw_msp430_intrcc
%token Kw_ptx_kernel
%token Kw_ptx_device
%token Kw_spir_kernel
%token Kw_spir_func
%token Kw_intel_ocl_bicc
%token Kw_x86_64_sysvcc
%token Kw_x86_64_win64cc
%token Kw_webkit_jscc
%token Kw_anyregcc
%token Kw_preserve_mostcc
%token Kw_preserve_allcc
%token Kw_cc
%token Kw_c
%token Kw_attributes
%token Kw_alwaysinline
%token Kw_builtin
%token Kw_byval
%token Kw_inalloca
%token Kw_cold
%token Kw_inlinehint
%token Kw_inreg
%token Kw_jumptable
%token Kw_minsize
%token Kw_naked
%token Kw_nest
%token Kw_noalias
%token Kw_nobuiltin
%token Kw_nocapture
%token Kw_noduplicate
%token Kw_noimplicitfloat
%token Kw_noinline
%token Kw_nonlazybind
%token Kw_nonnull
%token Kw_noredzone
%token Kw_noreturn
%token Kw_nounwind
%token Kw_optnone
%token Kw_optsize
%token Kw_readnone
%token Kw_readonly
%token Kw_returned
%token Kw_returns_twice
%token Kw_signext
%token Kw_sret
%token Kw_ssp
%token Kw_sspreq
%token Kw_sspstrong
%token Kw_sanitize_address
%token Kw_sanitize_thread
%token Kw_sanitize_memory
%token Kw_uwtable
%token Kw_zeroext
%token Kw_type
%token Kw_opaque
%token Kw_eq
%token Kw_ne
%token Kw_slt
%token Kw_sgt
%token Kw_sle
%token Kw_sge
%token Kw_ult
%token Kw_ugt
%token Kw_ule
%token Kw_uge
%token Kw_oeq
%token Kw_one
%token Kw_olt
%token Kw_ogt
%token Kw_ole
%token Kw_oge
%token Kw_ord
%token Kw_uno
%token Kw_ueq
%token Kw_une
%token Kw_xchg
%token Kw_nand
%token Kw_max
%token Kw_min
%token Kw_umax
%token Kw_umin
%token Kw_x
%token Kw_blockaddress
%token Kw_personality
%token Kw_cleanup
%token Kw_catch
%token Kw_filter
%token Kw_add
%token Kw_fadd
%token Kw_sub
%token Kw_fsub
%token Kw_mul
%token Kw_fmul
%token Kw_udiv
%token Kw_sdiv
%token Kw_fdiv
%token Kw_urem
%token Kw_srem
%token Kw_frem
%token Kw_shl
%token Kw_lshr
%token Kw_ashr
%token Kw_and
%token Kw_or
%token Kw_xor
%token Kw_icmp
%token Kw_fcmp
%token Kw_phi
%token Kw_call
%token Kw_trunc
%token Kw_zext
%token Kw_sext
%token Kw_fptrunc
%token Kw_fpext
%token Kw_uitofp
%token Kw_sitofp
%token Kw_fptoui
%token Kw_fptosi
%token Kw_inttoptr
%token Kw_ptrtoint
%token Kw_bitcast
%token Kw_addrspacecast
%token Kw_select
%token Kw_va_arg
%token Kw_ret
%token Kw_br
%token Kw_switch
%token Kw_indirectbr
%token Kw_invoke
%token Kw_resume
%token Kw_unreachable
%token Kw_alloca
%token Kw_load
%token Kw_store
%token Kw_cmpxchg
%token Kw_atomicrmw
%token Kw_fence
%token Kw_getelementptr
%token Kw_extractelement
%token Kw_insertelement
%token Kw_shufflevector
%token Kw_extractvalue
%token Kw_insertvalue
%token Kw_landingpad
%start main
%type <Util.cunit> main
%%
main:
| toplevel_list { process_toplevels $1 }
;
toplevel_list:
| Eof           { [] }
| toplevel toplevel_list { $1::$2 }
;
toplevel:
| Kw_declare function_header                   { Fun $2 }
| Kw_define function_header function_body      { $2.Util.fblocks <- $3; Fun $2 }
| Kw_module Kw_asm StringConstant              { Asm $3 }
| Kw_target Kw_triple Equal StringConstant     { Target $4 }
| Kw_target Kw_datalayout Equal StringConstant { Datalayout $4}
| Kw_deplibs Equal Lsquare string_list Rsquare { Deplibs $4 }
| LocalVarID Equal Kw_type Kw_opaque           { Typ($1, None) }
| LocalVarID Equal Kw_type typ                 { Typ($1, Some $4) }
| LocalVar Equal Kw_type Kw_opaque             { Typ($1, None) }
| LocalVar Equal Kw_type typ                   { Typ($1, Some $4) }
| global_eq external_linkage opt_visibility opt_dll_storageclass opt_thread_local
    opt_addrspace opt_unnamed_addr opt_externally_initialized
    constant_or_global typ opt_section_align
                                               { Global {Util.gname = $1;
                                                         Util.glinkage = Some $2;
                                                         Util.gvisibility = $3;
                                                         Util.gstorageclass = $4;
                                                         Util.gthread_local = $5;
                                                         Util.gaddrspace = $6;
                                                         Util.gunnamed_addr = $7;
                                                         Util.gexternally_initialized = $8;
                                                         Util.gconstant = $9;
                                                         Util.gtyp = $10;
                                                         Util.gvalue = None;
                                                         Util.gsection = fst $11;
                                                         Util.galign = snd $11;}
                                               }
| global_eq non_external_linkage opt_visibility opt_dll_storageclass opt_thread_local
    opt_addrspace opt_unnamed_addr opt_externally_initialized
    constant_or_global typ value opt_section_align
                                               { Global {Util.gname = $1;
                                                         Util.glinkage = $2;
                                                         Util.gvisibility = $3;
                                                         Util.gstorageclass = $4;
                                                         Util.gthread_local = $5;
                                                         Util.gaddrspace = $6;
                                                         Util.gunnamed_addr = $7;
                                                         Util.gexternally_initialized = $8;
                                                         Util.gconstant = $9;
                                                         Util.gtyp = $10;
                                                         Util.gvalue = Some $11;
                                                         Util.gsection = fst $12;
                                                         Util.galign = snd $12;}
                                               }
| global_eq external_linkage opt_visibility Kw_alias opt_linkage aliasee
    { Alias({Util.aname=$1; Util.avisibility=$3; Util.alinkage=$5; Util.aaliasee=$6}) }
| global_eq non_external_linkage opt_visibility Kw_alias opt_linkage aliasee
    { Alias({Util.aname=$1; Util.avisibility=$3; Util.alinkage=$5; Util.aaliasee=$6}) }
| Exclaim APInt Equal typ Exclaim Lbrace mdnodevector Rbrace
    { MDNodeDefn({Util.mdid=int_of_string $2; Util.mdtyp=$4; Util.mdcontents=$7}) }
| MetadataVar Equal Exclaim Lbrace mdlist Rbrace                             { MDVarDefn($1, $5) }
| Kw_attributes AttrGrpID Equal Lbrace group_attributes Rbrace               { Attrgrp($2, $5) }
;
global_eq: /* may want to allow empty here (per llvm parser) but haven't seen it yet and it causes grammar conflicts */
| GlobalID Equal  { $1 }
| GlobalVar Equal { $1 }
;
aliasee:
| Kw_bitcast       Lparen type_value Kw_to typ Rparen         { Util.A_bitcast($3, $5) }
| Kw_getelementptr opt_inbounds Lparen type_value_list Rparen { Util.A_getelementptr($2, $4) }
| type_value                                                  { Util.A_typ_value $1 }
;
string_list:
| /* empty */                { [] }
| StringConstant string_list { $1::$2 }
;
mdlist:
| /* empty */          { [] }
| Exclaim APInt mdlist { (int_of_string $2)::$3 }
;
mdnodevector:
| Kw_null                       { [None] }
| Kw_null Comma mdnodevector    { None::$3 }
| type_value                    { [Some $1] }
| type_value Comma mdnodevector { (Some $1)::$3 }
;
constant_or_global:
| Kw_constant { true }
| Kw_global   { false }
;
function_header:
| opt_linkage opt_visibility opt_dll_storageclass opt_callingconv return_attributes
 typ global_name argument_list opt_unnamed_addr function_attributes opt_section
 opt_align opt_gc opt_prefix
                             {
                               {Util.flinkage = $1;
                                Util.fvisibility = $2;
                                Util.fstorageclass = $3;
                                Util.fcallingconv = $4;
                                Util.freturnattrs = $5;
                                Util.freturntyp = $6;
                                Util.fname = $7;
                                Util.fparams = $8;
                                Util.funnamed_addr = $9;
                                Util.fattrs = $10;
                                Util.fsection = $11;
                                Util.falign = $12;
                                Util.fgc = $13;
                                Util.fprefix = $14;
                                Util.fblocks = [];}
                             }
;
typ:
| Kw_void       { Util.Void }
| non_void_type { $1 }
;
non_void_type:
| I                                    { Util.Integer $1 }
| Kw_half                              { Util.Half }
| Kw_float                             { Util.Float }
| Kw_double                            { Util.Double }
| Kw_x86_fp80                          { Util.X86_fp80 }
| Kw_fp128                             { Util.Fp128 }
| Kw_ppc_fp128                         { Util.Ppc_fp128 }
| Kw_label                             { Util.Label }
| Kw_metadata                          { Util.Metadata }
| Kw_x86_mmx                           { Util.X86_mmx }
| LocalVar                             { Util.Vartyp($1) }
| LocalVarID                           { Util.Vartyp($1) }
| Lbrace Rbrace                        { Util.Structtyp(false, []) }
| Less Lbrace Rbrace Greater           { Util.Structtyp(true, []) }
| Lbrace type_list Rbrace              { Util.Structtyp(false, $2) }
| Less Lbrace type_list Rbrace Greater { Util.Structtyp(true, $3) }
| Lsquare APInt Kw_x typ Rsquare       { Util.Arraytyp(int_of_string $2, $4) }
| Less APInt Kw_x typ Greater          { Util.Vector(int_of_string $2, $4) }
| typ opt_addrspace Star               { Util.Pointer($1, $2) }
| typ argument_list                    { Util.Funtyp($1, fst $2, snd $2) }
;
type_list:
| typ                 { [$1] }
| typ Comma type_list { $1::$3 }
;
global_name:
| GlobalID  { $1 }
| GlobalVar { $1 }
;
argument_list:
Lparen arg_type_list Rparen { $2 }
;
arg_type_list:
| /* empty */                  { ([], false) }
| DotDotDot                    { ([], true)}
| arg_type                     { ([$1], false) }
| arg_type Comma arg_type_list { ($1::(fst $3), snd $3) }
;
arg_type:
| typ param_attribute_list          { ($1, $2, None) }
| typ param_attribute_list LocalVar { ($1, $2, Some $3) }
;
opt_section:
| /* empty */               { None }
| Kw_section StringConstant { Some $2 }
;
opt_align:
| /* empty */    { None }
| Kw_align APInt { Some(int_of_string $2) }
;
opt_inbounds:
| /* empty */ { false }
| Kw_inbounds { true }
;
opt_tail:
| /* empty */ { Util.TCK_None }
| Kw_tail     { Util.TCK_Tail }
| Kw_musttail { Util.TCK_MustTail }
;
opt_cleanup:
| /* empty */ { false }
| Kw_cleanup  { true }
;
opt_section_align:
| /* empty */                                          { (None    , None) }
| Comma Kw_section StringConstant                      { (Some $3 , None) }
| Comma Kw_align APInt                                 { (None    , Some(int_of_string $3)) }
| Comma Kw_section StringConstant Comma Kw_align APInt { (Some $3 , Some(int_of_string $6)) }
;
align_metadata:
| instruction_metadata { (None, $1) }
| Comma Kw_align APInt instruction_metadata { (Some(int_of_string $3), $4) }
;
opt_gc:
| /* empty */          { None }
| Kw_gc StringConstant { Some $2 }
;
opt_prefix:
| /* empty */         { None }
| Kw_prefix typ value { Some($2, $3) }
;
opt_atomic:
| /* empty */ { false }
| Kw_atomic   { true }
;
opt_volatile:
| /* empty */ { false }
| Kw_volatile { true }
;
value:
| GlobalID                                                                                  { Util.Var $1 }
| GlobalVar                                                                                 { Util.Var $1 }
| LocalVarID                                                                                { Util.Var $1 }
| LocalVar                                                                                  { Util.Var $1 }
| Exclaim mdvalue                                                                           { $2 }
| APInt                                                                                     { Util.Int(Big_int.big_int_of_string $1) }
| APFloat                                                                                   { Util.Float $1 }
| Kw_true                                                                                   { Util.True }
| Kw_false                                                                                  { Util.False }
| Kw_null                                                                                   { Util.Null }
| Kw_undef                                                                                  { Util.Undef }
| Kw_zeroinitializer                                                                        { Util.Zero }
| Lbrace type_value_list Rbrace                                                             { Util.Struct(false, $2) }
| Less Lbrace Rbrace Greater                                                                { Util.Struct(true, []) }
| Less Lbrace type_value_LIST Rbrace Greater                                                { Util.Struct(true, $3) }
| Less type_value_list Greater                                                              { Util.Vector($2) }
| Lsquare type_value_list Rsquare                                                           { Util.Array($2) }
| Kw_c StringConstant                                                                       { Util.Array(list_of_string $2) }
| Kw_asm opt_sideeffect opt_alignstack opt_inteldialect StringConstant Comma StringConstant { Util.Asm($2, $3, $4, $5, $7) }
| Kw_blockaddress               Lparen value Comma value Rparen                             { Util.Blockaddress($3, $5) }
| Kw_trunc                      Lparen type_value Kw_to typ Rparen                          { Util.Trunc         ($3, $5) }
| Kw_zext                       Lparen type_value Kw_to typ Rparen                          { Util.Zext          ($3, $5) }
| Kw_sext                       Lparen type_value Kw_to typ Rparen                          { Util.Sext          ($3, $5) }
| Kw_fptrunc                    Lparen type_value Kw_to typ Rparen                          { Util.Fptrunc       ($3, $5) }
| Kw_fpext                      Lparen type_value Kw_to typ Rparen                          { Util.Fpext         ($3, $5) }
| Kw_bitcast                    Lparen type_value Kw_to typ Rparen                          { Util.Bitcast       ($3, $5) }
| Kw_addrspacecast              Lparen type_value Kw_to typ Rparen                          { Util.Addrspacecast ($3, $5) }
| Kw_uitofp                     Lparen type_value Kw_to typ Rparen                          { Util.Uitofp        ($3, $5) }
| Kw_sitofp                     Lparen type_value Kw_to typ Rparen                          { Util.Sitofp        ($3, $5) }
| Kw_fptoui                     Lparen type_value Kw_to typ Rparen                          { Util.Fptoui        ($3, $5) }
| Kw_fptosi                     Lparen type_value Kw_to typ Rparen                          { Util.Fptosi        ($3, $5) }
| Kw_inttoptr                   Lparen type_value Kw_to typ Rparen                          { Util.Inttoptr      ($3, $5) }
| Kw_ptrtoint                   Lparen type_value Kw_to typ Rparen                          { Util.Ptrtoint      ($3, $5) }
| Kw_extractvalue               Lparen type_value index_list Rparen                         { Util.Extractvalue($3, $4) }
| Kw_insertvalue                Lparen type_value Comma type_value index_list Rparen        { Util.Insertvalue($3, $5, $6) }
| Kw_icmp icmp_predicate        Lparen type_value Comma type_value Rparen                   { Util.Icmp($2, $4, $6) }
| Kw_fcmp fcmp_predicate        Lparen type_value Comma type_value Rparen                   { Util.Fcmp($2, $4, $6) }
| Kw_add opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Util.Add(fst $2, snd $2, $4, $6) }
| Kw_sub opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Util.Sub(fst $2, snd $2, $4, $6) }
| Kw_mul opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Util.Mul(fst $2, snd $2, $4, $6) }
| Kw_shl opt_nuw_nsw            Lparen type_value Comma type_value Rparen                   { Util.Shl(fst $2, snd $2, $4, $6) }
| Kw_sdiv opt_exact             Lparen type_value Comma type_value Rparen                   { Util.Sdiv($2, $4, $6) }
| Kw_udiv opt_exact             Lparen type_value Comma type_value Rparen                   { Util.Udiv($2, $4, $6) }
| Kw_lshr opt_exact             Lparen type_value Comma type_value Rparen                   { Util.Lshr($2, $4, $6) }
| Kw_ashr opt_exact             Lparen type_value Comma type_value Rparen                   { Util.Ashr($2, $4, $6) }
| Kw_fadd                       Lparen type_value Comma type_value Rparen                   { Util.Fadd($3, $5) }
| Kw_fsub                       Lparen type_value Comma type_value Rparen                   { Util.Fsub($3, $5) }
| Kw_fmul                       Lparen type_value Comma type_value Rparen                   { Util.Fmul($3, $5) }
| Kw_fdiv                       Lparen type_value Comma type_value Rparen                   { Util.Fdiv($3, $5) }
| Kw_urem                       Lparen type_value Comma type_value Rparen                   { Util.Urem($3, $5) }
| Kw_srem                       Lparen type_value Comma type_value Rparen                   { Util.Srem($3, $5) }
| Kw_frem                       Lparen type_value Comma type_value Rparen                   { Util.Frem($3, $5) }
| Kw_and                        Lparen type_value Comma type_value Rparen                   { Util.And($3, $5) }
| Kw_or                         Lparen type_value Comma type_value Rparen                   { Util.Or($3, $5) }
| Kw_xor                        Lparen type_value Comma type_value Rparen                   { Util.Xor($3, $5) }
| Kw_getelementptr opt_inbounds Lparen type_value_list Rparen                               { Util.Getelementptr($2, $4) }
| Kw_shufflevector              Lparen type_value_list Rparen                               { Util.Shufflevector  $3 }
| Kw_insertelement              Lparen type_value_list Rparen                               { Util.Insertelement  $3 }
| Kw_extractelement             Lparen type_value_list Rparen                               { Util.Extractelement $3 }
| Kw_select                     Lparen type_value_list Rparen                               { Util.Select         $3 }
;
mdvalue:
| APInt                      { Util.Mdnode(int_of_string $1) }
| StringConstant             { Util.Mdstring $1 }
| Lbrace mdnodevector Rbrace { Util.Mdnodevector $2 }
;
type_value_LIST_metadata:
| type_value instruction_metadata           { ([$1], $2) }
| type_value Comma type_value_LIST_metadata { ($1::(fst $3), snd $3) }
;
type_value_LIST:
| type_value                       { [$1] }
| type_value Comma type_value_LIST { $1::$3 }
;
type_value_list:
| /* empty */                      { [] }
| type_value                       { [$1] }
| type_value Comma type_value_list { $1::$3 }
;
index_list:
| Comma APInt            { [(int_of_string $2)] }
| Comma APInt index_list { (int_of_string $2)::$3 }
;
index_list_metadata:
| Comma APInt            instruction_metadata { [(int_of_string $2)], $3 }
| Comma APInt index_list_metadata             { (int_of_string $2)::(fst $3), snd $3 }
;
fcmp_predicate:
| Kw_oeq   { Util.F.Oeq   }
| Kw_one   { Util.F.One   }
| Kw_olt   { Util.F.Olt   }
| Kw_ogt   { Util.F.Ogt   }
| Kw_ole   { Util.F.Ole   }
| Kw_oge   { Util.F.Oge   }
| Kw_ord   { Util.F.Ord   }
| Kw_uno   { Util.F.Uno   }
| Kw_ueq   { Util.F.Ueq   }
| Kw_une   { Util.F.Une   }
| Kw_ult   { Util.F.Ult   }
| Kw_ugt   { Util.F.Ugt   }
| Kw_ule   { Util.F.Ule   }
| Kw_uge   { Util.F.Uge   }
| Kw_true  { Util.F.True  }
| Kw_false { Util.F.False }
;
icmp_predicate:
| Kw_eq  { Util.I.Eq  }
| Kw_ne  { Util.I.Ne  }
| Kw_slt { Util.I.Slt }
| Kw_sgt { Util.I.Sgt }
| Kw_sle { Util.I.Sle }
| Kw_sge { Util.I.Sge }
| Kw_ult { Util.I.Ult }
| Kw_ugt { Util.I.Ugt }
| Kw_ule { Util.I.Ule }
| Kw_uge { Util.I.Uge }
;
function_body:
| Lbrace basicblock_list Rbrace { $2 }
;
basicblock_list:
| basicblock                 { [$1] }
| basicblock basicblock_list { $1::$2 }
;
basicblock:
| LabelStr instruction_list { {Util.bname=Util.Name(false, $1); Util.binstrs=$2} }
| instruction_list          { {Util.bname=Util.Id(false, -1); Util.binstrs=$1} }
;
instruction_list:
| terminator_instruction { [$1] }
| instruction instruction_list { $1::$2 }
;
instruction_metadata:
| /* empty */ { [] }
| Comma MetadataVar Exclaim APInt instruction_metadata { ($2,Util.Mdnode(int_of_string $4))::$5 }
| Comma MetadataVar Exclaim Lbrace mdnodevector Rbrace instruction_metadata { ($2,Util.Mdnodevector $5)::$7 }
;
local_eq:
| LocalVarID Equal { $1 }
| LocalVar Equal   { $1 }
;
opt_local:
| /* empty */ { None }
| local_eq    { Some $1 }
;
instruction:
| local_eq Kw_add opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Util.Add(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_sub opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Util.Sub(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_mul opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Util.Mul(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_shl opt_nuw_nsw type_value Comma value      instruction_metadata { Some $1, Util.Shl(fst $3, snd $3, $4, $6, $7) }
| local_eq Kw_fadd fast_math_flags type_value Comma value instruction_metadata { Some $1, Util.Fadd($3, $4, $6, $7) }
| local_eq Kw_fsub fast_math_flags type_value Comma value instruction_metadata { Some $1, Util.Fsub($3, $4, $6, $7) }
| local_eq Kw_fmul fast_math_flags type_value Comma value instruction_metadata { Some $1, Util.Fmul($3, $4, $6, $7) }
| local_eq Kw_fdiv fast_math_flags type_value Comma value instruction_metadata { Some $1, Util.Fdiv($3, $4, $6, $7) }
| local_eq Kw_frem fast_math_flags type_value Comma value instruction_metadata { Some $1, Util.Frem($3, $4, $6, $7) }
| local_eq Kw_sdiv opt_exact type_value Comma value       instruction_metadata { Some $1, Util.Sdiv($3, $4, $6, $7) }
| local_eq Kw_udiv opt_exact type_value Comma value       instruction_metadata { Some $1, Util.Udiv($3, $4, $6, $7) }
| local_eq Kw_lshr opt_exact type_value Comma value       instruction_metadata { Some $1, Util.Lshr($3, $4, $6, $7) }
| local_eq Kw_ashr opt_exact type_value Comma value       instruction_metadata { Some $1, Util.Ashr($3, $4, $6, $7) }
| local_eq Kw_urem type_value Comma value                 instruction_metadata { Some $1, Util.Urem($3, $5, $6) }
| local_eq Kw_srem type_value Comma value                 instruction_metadata { Some $1, Util.Srem($3, $5, $6) }
| local_eq Kw_and type_value Comma value                  instruction_metadata { Some $1, Util.And($3, $5, $6) }
| local_eq Kw_or type_value Comma value                   instruction_metadata { Some $1, Util.Or($3, $5, $6) }
| local_eq Kw_xor type_value Comma value                  instruction_metadata { Some $1, Util.Xor($3, $5, $6) }
| local_eq Kw_icmp icmp_predicate type_value Comma value  instruction_metadata { Some $1, Util.Icmp($3, $4, $6, $7) }
| local_eq Kw_fcmp fcmp_predicate type_value Comma value  instruction_metadata { Some $1, Util.Fcmp($3, $4, $6, $7) }
| local_eq Kw_trunc type_value Kw_to typ                  instruction_metadata { Some $1, Util.Trunc($3, $5, $6) }
| local_eq Kw_zext type_value Kw_to typ                   instruction_metadata { Some $1, Util.Zext($3, $5, $6) }
| local_eq Kw_sext type_value Kw_to typ                   instruction_metadata { Some $1, Util.Sext($3, $5, $6) }
| local_eq Kw_fptrunc type_value Kw_to typ                instruction_metadata { Some $1, Util.Fptrunc($3, $5, $6) }
| local_eq Kw_fpext type_value Kw_to typ                  instruction_metadata { Some $1, Util.Fpext($3, $5, $6) }
| local_eq Kw_bitcast type_value Kw_to typ                instruction_metadata { Some $1, Util.Bitcast($3, $5, $6) }
| local_eq Kw_addrspacecast type_value Kw_to typ          instruction_metadata { Some $1, Util.Addrspacecast($3, $5, $6) }
| local_eq Kw_uitofp type_value Kw_to typ                 instruction_metadata { Some $1, Util.Uitofp($3, $5, $6) }
| local_eq Kw_sitofp type_value Kw_to typ                 instruction_metadata { Some $1, Util.Sitofp($3, $5, $6) }
| local_eq Kw_fptoui type_value Kw_to typ                 instruction_metadata { Some $1, Util.Fptoui($3, $5, $6) }
| local_eq Kw_fptosi type_value Kw_to typ                 instruction_metadata { Some $1, Util.Fptosi($3, $5, $6) }
| local_eq Kw_inttoptr type_value Kw_to typ               instruction_metadata { Some $1, Util.Inttoptr($3, $5, $6) }
| local_eq Kw_ptrtoint type_value Kw_to typ               instruction_metadata { Some $1, Util.Ptrtoint($3, $5, $6) }
| local_eq Kw_va_arg type_value Comma typ                 instruction_metadata { Some $1, Util.Va_arg($3, $5, $6) }
| local_eq Kw_getelementptr opt_inbounds type_value_LIST_metadata              { Some $1, Util.Getelementptr($3, fst $4, snd $4) }
| local_eq Kw_extractelement type_value_LIST_metadata                          { Some $1, Util.Extractelement(fst $3, snd $3) }
| local_eq Kw_insertelement type_value_LIST_metadata                           { Some $1, Util.Insertelement(fst $3, snd $3) }
| local_eq Kw_shufflevector type_value_LIST_metadata                           { Some $1, Util.Shufflevector(fst $3, snd $3) }
| local_eq Kw_select type_value_LIST_metadata                                  { Some $1, Util.Select(fst $3, snd $3) }
| local_eq Kw_phi typ phi_list_metadata                                        { Some $1, Util.Phi($3, fst $4, snd $4) }
| local_eq Kw_landingpad typ Kw_personality type_value opt_cleanup landingpad_list
                                                          instruction_metadata { Some $1, Util.Landingpad($3, $5, $6, $7, $8) }
| opt_local opt_tail Kw_call opt_callingconv return_attributes typ value Lparen param_list Rparen call_attributes
                                                          instruction_metadata { $1, Util.Call($2, $4, $5, $6, $7, $9, $11, $12) }
| local_eq Kw_alloca alloc_metadata                                            { Some $1, $3 }
| local_eq Kw_load opt_atomic opt_volatile type_value scopeandordering
                                                                align_metadata { Some $1, Util.Load($3, $4, $5, $6, fst $7, snd $7) }
| Kw_store opt_atomic opt_volatile type_value Comma type_value scopeandordering
                                                                align_metadata { None, Util.Store($2, $3, $4, $6, $7, fst $8, snd $8) }
| Kw_cmpxchg opt_volatile type_value Comma type_value Comma type_value opt_singlethread ordering ordering
                                                          instruction_metadata { None, Util.Cmpxchg($2, $3, $5, $7, $8, $9, $10, $11) }
| Kw_atomicrmw opt_volatile binop type_value Comma type_value opt_singlethread ordering
                                                          instruction_metadata { None, Util.Atomicrmw($2, $3, $4, $6, $7, $8, $9) }
| Kw_fence opt_singlethread ordering                      instruction_metadata { None, Util.Fence($2, $3, $4) }
| local_eq Kw_extractvalue type_value index_list_metadata                      { Some $1, Util.Extractvalue($3, fst $4, snd $4) }
| local_eq Kw_insertvalue type_value Comma type_value index_list_metadata      { Some $1, Util.Insertvalue($3, $5, fst $6, snd $6) }
;
binop:
| Kw_xchg { Util.Xchg }
| Kw_add  { Util.Add  }
| Kw_sub  { Util.Sub  }
| Kw_and  { Util.And  }
| Kw_nand { Util.Nand }
| Kw_or   { Util.Or   }
| Kw_xor  { Util.Xor  }
| Kw_max  { Util.Max  }
| Kw_min  { Util.Min  }
| Kw_umax { Util.Umax }
| Kw_umin { Util.Umin }
;
phi_list_metadata:
| Lsquare value Comma value Rsquare instruction_metadata    { [($2, $4)], $6 }
| Lsquare value Comma value Rsquare Comma phi_list_metadata { ($2, $4)::(fst $7), snd $7 }
;
landingpad_list:
| Kw_catch typ value                  { [Util.Catch($2, $3)] }
| Kw_filter typ value                 { [Util.Filter($2, $3)] }
| Kw_catch typ value landingpad_list  { (Util.Catch($2, $3))::$4 }
| Kw_filter typ value landingpad_list { (Util.Filter($2, $3))::$4 }
;
ordering:
| Kw_unordered { Util.Unordered }
| Kw_monotonic { Util.Monotonic }
| Kw_acquire   { Util.Acquire   }
| Kw_release   { Util.Release   }
| Kw_acq_rel   { Util.Acq_rel   }
| Kw_seq_cst   { Util.Seq_cst   }
;
opt_singlethread:
| /* empty */               { false }
| Kw_singlethread           { true }
scopeandordering:
| /* empty */               { None }
| opt_singlethread ordering { Some($1, $2) }
;
alloc_metadata:
| Kw_inalloca typ Comma type_value Comma Kw_align APInt instruction_metadata { Util.Alloca(true,  $2, Some $4, Some(int_of_string $7), $8) }
| Kw_inalloca typ Comma type_value                      instruction_metadata { Util.Alloca(true,  $2, Some $4, None, $5) }
| Kw_inalloca typ Comma Kw_align APInt                  instruction_metadata { Util.Alloca(true,  $2, None, Some(int_of_string $5), $6) }
| Kw_inalloca typ                                       instruction_metadata { Util.Alloca(true,  $2, None, None, $3) }
| typ Comma type_value Comma Kw_align APInt             instruction_metadata { Util.Alloca(false, $1, Some $3, Some(int_of_string $6), $7) }
| typ Comma type_value                                  instruction_metadata { Util.Alloca(false, $1, Some $3, None, $4) }
| typ Comma Kw_align APInt                              instruction_metadata { Util.Alloca(false, $1, None, Some(int_of_string $4), $5) }
| typ                                                   instruction_metadata { Util.Alloca(false, $1, None, None, $2) }
;
fast_math_flags:
| /* empty */                    { [] }
| fast_math_flag fast_math_flags { $1::$2 }
;
fast_math_flag:
| Kw_fast { Util.Fast }
| Kw_nnan { Util.Nnan }
| Kw_ninf { Util.Ninf }
| Kw_nsz  { Util.Nsz  }
| Kw_arcp { Util.Arcp }
;
terminator_instruction:
| Kw_unreachable                                                   instruction_metadata { None, Util.Unreachable $2 }
| Kw_ret Kw_void                                                   instruction_metadata { None, Util.Return(None,$3) } /* we need to distinguish void from all other types else we have a dependent grammar */
| Kw_ret non_void_type value                                       instruction_metadata { None, Util.Return(Some($2, $3),$4) }
| Kw_br type_value                                                 instruction_metadata { None, Util.Br($2, None, $3) }
| Kw_br type_value Comma type_value Comma type_value               instruction_metadata { None, Util.Br($2, Some($4, $6), $7) }
| Kw_indirectbr type_value Comma Lsquare type_value_LIST Rsquare   instruction_metadata { None, Util.Indirectbr($2, $5, $7) }
| Kw_resume type_value                                             instruction_metadata { None, Util.Resume($2, $3) }
| Kw_switch type_value Comma type_value Lsquare jump_table Rsquare instruction_metadata { None, Util.Switch($2, $4, $6, $8) }
| local_eq Kw_invoke opt_callingconv return_attributes typ value Lparen param_list Rparen function_attributes Kw_to type_value Kw_unwind type_value instruction_metadata { Some $1, Util.Invoke($3, $4, $5, $6, $8, $10, $12, $14, $15) }
;
call_attributes:
| /* empty */                    { [] }
| call_attribute call_attributes { $1::$2 }
;
call_attribute:
| AttrGrpID   { Util.Attrgrp($1) }
| Kw_noreturn { Util.Noreturn }
| Kw_nounwind { Util.Nounwind }
| Kw_readnone { Util.Readnone }
| Kw_readonly { Util.Readonly }
;
function_attributes:
| /* empty */                            { [] }
| function_attribute function_attributes { $1::$2 }
;
function_attribute:
| AttrGrpID                                { Util.Attrgrp($1) }
| StringConstant Equal StringConstant      { Util.Attr($1, Some $3) }
| Kw_alignstack Equal Lparen APInt Rparen  { Util.Alignstack(int_of_string $4) }
| Kw_alwaysinline                          { Util.Alwaysinline     }
| Kw_builtin                               { Util.Builtin          }
| Kw_cold                                  { Util.Cold             }
| Kw_inlinehint                            { Util.Inlinehint       }
| Kw_jumptable                             { Util.Jumptable        }
| Kw_minsize                               { Util.Minsize          }
| Kw_naked                                 { Util.Naked            }
| Kw_nobuiltin                             { Util.Nobuiltin        }
| Kw_noduplicate                           { Util.Noduplicate      }
| Kw_noimplicitfloat                       { Util.Noimplicitfloat  }
| Kw_noinline                              { Util.Noinline         }
| Kw_nonlazybind                           { Util.Nonlazybind      }
| Kw_noredzone                             { Util.Noredzone        }
| Kw_noreturn                              { Util.Noreturn         }
| Kw_nounwind                              { Util.Nounwind         }
| Kw_optnone                               { Util.Optnone          }
| Kw_optsize                               { Util.Optsize          }
| Kw_readnone                              { Util.Readnone         }
| Kw_readonly                              { Util.Readonly         }
| Kw_returns_twice                         { Util.Returns_twice    }
| Kw_ssp                                   { Util.Ssp              }
| Kw_sspreq                                { Util.Sspreq           }
| Kw_sspstrong                             { Util.Sspstrong        }
| Kw_sanitize_address                      { Util.Sanitize_address }
| Kw_sanitize_thread                       { Util.Sanitize_thread  }
| Kw_sanitize_memory                       { Util.Sanitize_memory  }
| Kw_uwtable                               { Util.Uwtable          }
;
group_attributes:
| /* empty */                      { [] }
| group_attribute group_attributes { $1::$2 }
;
group_attribute:
| StringConstant                      { Util.Attr($1, None) }
| StringConstant Equal StringConstant { Util.Attr($1, Some $3) }
| Kw_align Equal APInt                { Util.Align(int_of_string $3) }
| Kw_alignstack Equal APInt           { Util.Alignstack(int_of_string $3) }
| Kw_alwaysinline                     { Util.Alwaysinline    }
| Kw_builtin                          { Util.Builtin         }
| Kw_cold                             { Util.Cold            }
| Kw_inlinehint                       { Util.Inlinehint      }
| Kw_jumptable                        { Util.Jumptable        }
| Kw_minsize                          { Util.Minsize         }
| Kw_naked                            { Util.Naked           }
| Kw_nobuiltin                        { Util.Nobuiltin       }
| Kw_noduplicate                      { Util.Noduplicate     }
| Kw_noimplicitfloat                  { Util.Noimplicitfloat }
| Kw_noinline                         { Util.Noinline        }
| Kw_nonlazybind                      { Util.Nonlazybind     }
| Kw_noredzone                        { Util.Noredzone       }
| Kw_noreturn                         { Util.Noreturn        }
| Kw_nounwind                         { Util.Nounwind        }
| Kw_optnone                          { Util.Optnone         }
| Kw_optsize                          { Util.Optsize         }
| Kw_readnone                         { Util.Readnone        }
| Kw_readonly                         { Util.Readonly        }
| Kw_returns_twice                    { Util.Returns_twice   }
| Kw_ssp                              { Util.Ssp             }
| Kw_sspreq                           { Util.Sspreq          }
| Kw_sspstrong                        { Util.Sspstrong       }
| Kw_sanitize_address                 { Util.Sanitize_address}
| Kw_sanitize_thread                  { Util.Sanitize_thread }
| Kw_sanitize_memory                  { Util.Sanitize_memory }
| Kw_uwtable                          { Util.Uwtable         }
;
param_list:
| /* empty */            { [] }
| param                  { [$1] }
| param Comma param_list { $1::$3 }
;
param:
| typ param_attribute_list value { ($1, $2, $3) }
;
param_attribute_list:
| /* empty */                          { [] }
| param_attribute param_attribute_list { $1::$2 }
param_attribute:
| Kw_align APInt  { Util.Align(int_of_string $2) }
| Kw_byval        { Util.Byval     }
| Kw_inalloca     { Util.Inalloca  }
| Kw_inreg        { Util.Inreg     }
| Kw_nest         { Util.Nest      }
| Kw_noalias      { Util.Noalias   }
| Kw_nocapture    { Util.Nocapture }
| Kw_nonnull      { Util.Nonnull   }
| Kw_readnone     { Util.Readnone  }
| Kw_readonly     { Util.Readonly  }
| Kw_returned     { Util.Returned  }
| Kw_signext      { Util.Signext   }
| Kw_sret         { Util.Sret      }
| Kw_zeroext      { Util.Zeroext   }
;
jump_table:
| /* empty */                            { [] }
| type_value Comma type_value jump_table { ($1,$3)::$4 }
;
type_value:
| typ value { ($1, $2) }
;
opt_sideeffect:
| /* empty */   { false }
| Kw_sideeffect { true }
;
opt_alignstack:
| /* empty */   { false }
| Kw_alignstack { true }
;
opt_inteldialect:
| /* empty */     { false }
| Kw_inteldialect { true }
;
opt_exact:
| /* empty */ { false }
| Kw_exact    { true }
;
opt_nuw_nsw:
| /* empty */   { (false, false) }
| Kw_nuw Kw_nsw { (true, true)   }
| Kw_nsw Kw_nuw { (true, true)   }
| Kw_nuw        { (true, false)  }
| Kw_nsw        { (false, true)  }
;
opt_thread_local:
| /* empty */                                   { None }
| Kw_thread_local                               { Some None }
| Kw_thread_local Lparen Kw_localdynamic Rparen { Some (Some Util.Localdynamic) }
| Kw_thread_local Lparen Kw_initialexec Rparen  { Some (Some Util.Initialexec) }
| Kw_thread_local Lparen Kw_localexec Rparen    { Some (Some Util.Localexec) }
;
opt_addrspace:
| /* empty */                      { None }
| Kw_addrspace Lparen APInt Rparen { Some (int_of_string $3) }
;
opt_unnamed_addr:
| /* empty */     { false }
| Kw_unnamed_addr { true }
;
opt_externally_initialized:
| /* empty */               { false }
| Kw_externally_initialized { true }
;
opt_dll_storageclass:
| /* empty */  { None }
| Kw_dllimport { Some Util.Dllimport }
| Kw_dllexport { Some Util.Dllexport }
;
opt_linkage:
| external_linkage     { Some $1 }
| non_external_linkage { $1 }
;
external_linkage:
| Kw_extern_weak { Util.Extern_weak }
| Kw_external    { Util.External }
;
non_external_linkage:
| /* empty */             { None }
| Kw_private              { Some Util.Private }
| Kw_internal             { Some Util.Internal }
| Kw_linker_private       { Some Util.Linker_private }
| Kw_linker_private_weak  { Some Util.Linker_private_weak }
| Kw_weak                 { Some Util.Weak }
| Kw_weak_odr             { Some Util.Weak_odr }
| Kw_linkonce             { Some Util.Linkonce }
| Kw_linkonce_odr         { Some Util.Linkonce_odr }
| Kw_available_externally { Some Util.Available_externally }
| Kw_appending            { Some Util.Appending }
| Kw_common               { Some Util.Common }
;
opt_visibility:
| /* empty */  { None }
| Kw_default   { Some Util.Default }
| Kw_hidden    { Some Util.Hidden }
| Kw_protected { Some Util.Protected }
;
opt_callingconv:
| /* empty */          { None }
| Kw_ccc               { Some Util.Ccc               }
| Kw_fastcc            { Some Util.Fastcc            }
| Kw_intel_ocl_bicc    { Some Util.Intel_ocl_bicc    }
| Kw_coldcc            { Some Util.Coldcc            }
| Kw_x86_stdcallcc     { Some Util.X86_stdcallcc     }
| Kw_x86_fastcallcc    { Some Util.X86_fastcallcc    }
| Kw_x86_thiscallcc    { Some Util.X86_thiscallcc    }
| Kw_x86_cdeclmethodcc { Some Util.X86_cdeclmethodcc }
| Kw_arm_apcscc        { Some Util.Arm_apcscc        }
| Kw_arm_aapcscc       { Some Util.Arm_aapcscc       }
| Kw_arm_aapcs_vfpcc   { Some Util.Arm_aapcs_vfpcc   }
| Kw_msp430_intrcc     { Some Util.Msp430_intrcc     }
| Kw_ptx_kernel        { Some Util.Ptx_kernel        }
| Kw_ptx_device        { Some Util.Ptx_device        }
| Kw_spir_func         { Some Util.Spir_func         }
| Kw_spir_kernel       { Some Util.Spir_kernel       }
| Kw_x86_64_sysvcc     { Some Util.X86_64_sysvcc     }
| Kw_x86_64_win64cc    { Some Util.X86_64_win64cc    }
| Kw_webkit_jscc       { Some Util.Webkit_jscc       }
| Kw_anyregcc          { Some Util.Anyregcc          }
| Kw_preserve_mostcc   { Some Util.Preserve_mostcc   }
| Kw_preserve_allcc    { Some Util.Preserve_allcc    }
| Kw_cc                { Some Util.Cc                }
;
return_attributes:
| /* empty */                        { [] }
| return_attribute return_attributes { $1::$2 }
;
return_attribute:
| Kw_inreg   { Util.Inreg   }
| Kw_noalias { Util.Noalias }
| Kw_nonnull { Util.Nonnull }
| Kw_signext { Util.Signext }
| Kw_zeroext { Util.Zeroext }
;
