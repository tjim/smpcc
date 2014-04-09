%{
let list_of_string s =
  let a = Array.create (String.length s) 'a' in
  String.iteri (fun i c -> a.(i) <- c) s;
  List.map (fun c -> (Util.Integer 8, Util.Int(Big_int.big_int_of_int(Char.code c)))) (Array.to_list a)
%}
%token <string> APFloat
%token <string> APInt
%token <string> APSint
%token <string> AttrGrpID
%token Backslash
%token Comma
%token DotDotDot
%token Eof
%token Equal
%token Exclaim
%token <Util.variable> GlobalVar
%token <Util.variable> GlobalID
%token <Util.variable> LocalVar
%token <Util.variable> LocalVarID
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
%type <Util.toplevel list> main
%%
main:
| Eof           { [] }
| toplevel main { $1::$2 }
;
toplevel:
| Kw_declare function_header                   { Util.Fun $2 }
| Kw_define function_header function_body      { $2.Util.fblocks <- $3; Util.Fun $2 }
| Kw_module Kw_asm StringConstant              { Util.Asm $3 }
| Kw_target Kw_triple Equal StringConstant     { Util.Target $4 }
| Kw_target Kw_datalayout Equal StringConstant { Util.Datalayout $4}
| Kw_deplibs Equal Lsquare string_list Rsquare { Util.Deplibs $4 }
| LocalVarID Equal Kw_type Kw_opaque           { Util.Type($1, None) }
| LocalVarID Equal Kw_type typ                 { Util.Type($1, Some $4) }
| LocalVar Equal Kw_type Kw_opaque             { Util.Type($1, None) }
| LocalVar Equal Kw_type typ                   { Util.Type($1, Some $4) }
| global_eq external_linkage opt_visibility opt_dll_storageclass opt_thread_local
    opt_addrspace opt_unnamed_addr opt_externally_initialized
    constant_or_global typ opt_section_align
                                               { Util.Global {Util.gname = $1;
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
                                               { Util.Global {Util.gname = $1;
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
| global_eq external_linkage opt_visibility Kw_alias opt_linkage aliasee     { Util.GlobalAlias($1, Some $2, $3, $5, $6) }
| global_eq non_external_linkage opt_visibility Kw_alias opt_linkage aliasee { Util.GlobalAlias($1, $2, $3, $5, $6) }
| Exclaim APInt Equal typ Exclaim Lbrace mdnodevector Rbrace                 { Util.MDNodeDefn(int_of_string $2, $4, $7) }
| MetadataVar Equal Exclaim Lbrace mdlist Rbrace                             { Util.MDVarDefn($1, $5) }
| Kw_attributes AttrGrpID Equal Lbrace group_attributes Rbrace               { Util.Attrgrp($2, $5) }
;
global_eq: /* may want to allow empty here (per llvm parser) but haven't seen it yet and it causes grammar conflicts */
| GlobalID Equal  { $1 }
| GlobalVar Equal { $1 }
;
aliasee:
| Kw_bitcast       Lparen type_value Kw_to typ Rparen         { Util.A_bitcast($3, $5) }
| Kw_getelementptr opt_inbounds Lparen type_value_list Rparen { Util.A_getelementptr($2, $4) }
| type_value                                                  { Util.A_type_value $1 }
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
| Kw_null                 { [None] }
| Kw_null mdnodevector    { None::$2 }
| type_value              { [Some $1] }
| type_value mdnodevector { (Some $1)::$2 }
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
| LocalVar                             { Util.Varty($1) }
| LocalVarID                           { Util.Varty($1) }
| Lbrace Rbrace                        { Util.Struct(false, []) }
| Less Lbrace Rbrace Greater           { Util.Struct(true, []) }
| Lbrace type_list Rbrace              { Util.Struct(false, $2) }
| Less Lbrace type_list Rbrace Greater { Util.Struct(true, $3) }
| Lsquare APInt Kw_x typ Rsquare       { Util.Array(int_of_string $2, $4) }
| Less APInt Kw_x typ Greater          { Util.Vector(int_of_string $2, $4) }
| typ opt_addrspace Star               { Util.Pointer($1, $2) }
| typ argument_list                    { Util.FunctionTy($1, fst $2, snd $2) }
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
Lparen arg_type_list Rparen {$2}
;
arg_type_list:
| /* empty */                  { ([], false) }
| DotDotDot                    { ([], true)}
| arg_type                     { ([$1], false) }
| arg_type Comma arg_type_list { ($1::(fst $3), snd $3) }
;
arg_type:
| typ          { $1 }
| typ LocalVar { $1 } /* NB we discard parameter names in function types */
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
| /* empty */ { false }
| Kw_tail     { true }
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
opt_comma_align:
| /* empty */          { None }
| Comma Kw_align APInt { Some(int_of_string $3) }
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
| GlobalID                                                                                  { Util.Variable $1 }
| GlobalVar                                                                                 { Util.Variable $1 }
| LocalVarID                                                                                { Util.Variable $1 }
| LocalVar                                                                                  { Util.Variable $1 }
| Exclaim mdvalue                                                                           { $2 }
| APInt                                                                                     { Util.Int(Big_int.big_int_of_string $1) }
| APFloat                                                                                   { Util.Float $1 }
| Kw_true                                                                                   { Util.True }
| Kw_false                                                                                  { Util.False }
| Kw_null                                                                                   { Util.Null }
| Kw_undef                                                                                  { Util.UndefValue }
| Kw_zeroinitializer                                                                        { Util.Zero }
| Lbrace type_value_list Rbrace                                                             { Util.ConstantStruct(false, $2) }
| Less Lbrace Rbrace Greater                                                                { Util.ConstantStruct(true, []) }
| Less Lbrace type_value_LIST Rbrace Greater                                                { Util.ConstantStruct(true, $3) }
| Less type_value_list Greater                                                              { Util.ConstantVector($2) }
| Lsquare type_value_list Rsquare                                                           { Util.ConstantArray($2) }
| Kw_c StringConstant                                                                       { Util.ConstantArray(list_of_string $2) }
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
| Kw_extractvalue               Lparen type_value index_list Rparen                         { Util.ExtractValue($3, $4) }
| Kw_insertvalue                Lparen type_value Comma type_value index_list Rparen        { Util.InsertValue($3, $5, $6) }
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
| APInt                      { Util.MDNode(int_of_string $1) }
| StringConstant             { Util.MDString $1 }
| Lbrace mdnodevector Rbrace { Util.MDNodeVector $2 }
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
| opt_labelstr instruction_list { ($1, $2) }
;
instruction_list:
| terminator_instruction       { [$1] }
| instruction instruction_list { $1::$2 }
;
opt_labelstr:
| /* empty */ { None }
| LabelStr    { Some $1 }
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
 | local_eq Kw_add opt_nuw_nsw type_value Comma value      { Util.Add($3, $4, $6) }
 | local_eq Kw_sub opt_nuw_nsw type_value Comma value      { Util.Sub($3, $4, $6) }
 | local_eq Kw_mul opt_nuw_nsw type_value Comma value      { Util.Mul($3, $4, $6) }
 | local_eq Kw_shl opt_nuw_nsw type_value Comma value      { Util.Shl($3, $4, $6) }
 | local_eq Kw_fadd fast_math_flags type_value Comma value { Util.Fadd($3, $4, $6) }
 | local_eq Kw_fsub fast_math_flags type_value Comma value { Util.Fsub($3, $4, $6) }
 | local_eq Kw_fmul fast_math_flags type_value Comma value { Util.Fmul($3, $4, $6) }
 | local_eq Kw_fdiv fast_math_flags type_value Comma value { Util.Fdiv($3, $4, $6) }
 | local_eq Kw_frem fast_math_flags type_value Comma value { Util.Frem($3, $4, $6) }
 | local_eq Kw_sdiv opt_exact type_value Comma value       { Util.Sdiv($3, $4, $6) }
 | local_eq Kw_udiv opt_exact type_value Comma value       { Util.Udiv($3, $4, $6) }
 | local_eq Kw_lshr opt_exact type_value Comma value       { Util.Lshr($3, $4, $6) }
 | local_eq Kw_ashr opt_exact type_value Comma value       { Util.Ashr($3, $4, $6) }
 | local_eq Kw_urem type_value Comma value                 { Util.Urem($3, $5) }
 | local_eq Kw_srem type_value Comma value                 { Util.Srem($3, $5) }
 | local_eq Kw_and type_value Comma value                  { Util.And($3, $5) }
 | local_eq Kw_or type_value Comma value                   { Util.Or($3, $5) }
 | local_eq Kw_xor type_value Comma value                  { Util.Xor($3, $5) }
 | local_eq Kw_icmp icmp_predicate type_value Comma value  { Util.Icmp($3, $4, $6) }
 | local_eq Kw_fcmp fcmp_predicate type_value Comma value  { Util.Fcmp($3, $4, $6) }
 | local_eq Kw_trunc type_value Kw_to typ                  { Util.Trunc($3, $5) }
 | local_eq Kw_zext type_value Kw_to typ                   { Util.Zext($3, $5) }
 | local_eq Kw_sext type_value Kw_to typ                   { Util.Sext($3, $5) }
 | local_eq Kw_fptrunc type_value Kw_to typ                { Util.Fptrunc($3, $5) }
 | local_eq Kw_fpext type_value Kw_to typ                  { Util.Fpext($3, $5) }
 | local_eq Kw_bitcast type_value Kw_to typ                { Util.Bitcast($3, $5) }
 | local_eq Kw_addrspacecast type_value Kw_to typ          { Util.Addrspacecast($3, $5) }
 | local_eq Kw_uitofp type_value Kw_to typ                 { Util.Uitofp($3, $5) }
 | local_eq Kw_sitofp type_value Kw_to typ                 { Util.Sitofp($3, $5) }
 | local_eq Kw_fptoui type_value Kw_to typ                 { Util.Fptoui($3, $5) }
 | local_eq Kw_fptosi type_value Kw_to typ                 { Util.Fptosi($3, $5) }
 | local_eq Kw_inttoptr type_value Kw_to typ               { Util.Inttoptr($3, $5) }
 | local_eq Kw_ptrtoint type_value Kw_to typ               { Util.Ptrtoint($3, $5) }
 | local_eq Kw_va_arg type_value Comma typ                 { Util.Va_arg($3, $5) }
 | local_eq Kw_getelementptr opt_inbounds type_value_LIST  { Util.Getelementptr($3, $4) }
 | local_eq Kw_extractelement type_value_LIST              { Util.Extractelement($3) }
 | local_eq Kw_insertelement type_value_LIST               { Util.Insertelement($3) }
 | local_eq Kw_shufflevector type_value_LIST               { Util.Shufflevector($3) }
 | local_eq Kw_select type_value_LIST                      { Util.Select($3) }
 | local_eq Kw_phi typ phi_list                            { Util.Phi($3, $4) }
 | local_eq Kw_landingpad typ Kw_personality type_value opt_cleanup landingpad_list
                                                           { Util.Landingpad($3, $5, $6, $7) }
| opt_local opt_tail Kw_call opt_callingconv return_attributes typ value Lparen param_list Rparen call_attributes
                                                           { Util.Call($2, $4, $5, $6, $7, $9, $11) }
| local_eq Kw_alloca alloc                                 { $3 }
| local_eq Kw_load  opt_atomic opt_volatile type_value scopeandordering opt_comma_align
                                                           { Util.Load($3, $4, $5, $6, $7) }
| Kw_store opt_atomic opt_volatile type_value Comma type_value scopeandordering opt_comma_align
                                                           { Util.Store($2, $3, $4, $6, $7, $8) }
| Kw_cmpxchg opt_volatile type_value Comma type_value Comma type_value opt_singlethread ordering ordering
                                                           { Util.Cmpxchg($2, $3, $5, $7, $8, $9, $10) }
| Kw_atomicrmw opt_volatile binop type_value Comma type_value opt_singlethread ordering
                                                           { Util.Atomicrmw($2, $3, $4, $6, $7, $8) }
| Kw_fence opt_singlethread ordering                       { Util.Fence($2, $3) }
| local_eq Kw_extractvalue type_value index_list           { Util.Extractvalue($3, $4) }
| local_eq Kw_insertvalue type_value Comma type_value index_list
                                                           { Util.Insertvalue($3, $5, $6) }
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
phi_list:
| Lsquare value Comma value Rsquare                { [($2, $4)] }
| Lsquare value Comma value Rsquare Comma phi_list { ($2, $4)::$7 }
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
alloc:
| Kw_inalloca typ Comma type_value Comma Kw_align APInt { Util.Alloca(true,  $2, Some $4, Some(int_of_string $7)) }
| Kw_inalloca typ Comma type_value                      { Util.Alloca(true,  $2, Some $4, None) }
| Kw_inalloca typ Comma Kw_align APInt                  { Util.Alloca(true,  $2, None, Some(int_of_string $5))}
| Kw_inalloca typ                                       { Util.Alloca(true,  $2, None, None) }
| typ Comma type_value Comma Kw_align APInt             { Util.Alloca(false, $1, Some $3, Some(int_of_string $6)) }
| typ Comma type_value                                  { Util.Alloca(false, $1, Some $3, None) }
| typ Comma Kw_align APInt                              { Util.Alloca(false, $1, None, Some(int_of_string $4))}
| typ                                                   { Util.Alloca(false, $1, None, None) }
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
| Kw_unreachable                                                   { Util.Unreachable }
| Kw_ret Kw_void                                                   { Util.Return None } /* we need to distinguish void from all other types else we have a dependent grammar */
| Kw_ret non_void_type value                                       { Util.Return(Some($2, $3)) }
| Kw_br type_value                                                 { Util.Br($2, None) }
| Kw_br type_value Comma type_value Comma type_value               { Util.Br($2, Some($4, $6)) }
| Kw_indirectbr type_value Comma Lsquare type_value_LIST Rsquare   { Util.Indirectbr($2, $5) }
| Kw_resume type_value                                             { Util.Resume $2 }
| Kw_switch type_value Comma type_value Lsquare jump_table Rsquare { Util.Switch($2, $4, $6) }
| local_eq Kw_invoke opt_callingconv return_attributes typ value Lparen param_list Rparen function_attributes Kw_to type_value Kw_unwind type_value { Util.Invoke($3, $4, $5, $6, $8, $10, $12, $14) }
;
call_attributes:
| /* empty */                    { [] }
| call_attribute call_attributes { $1::$2 }
;
call_attribute:
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
| AttrGrpID                                { Util.AttrGrpID(int_of_string $1) }
| StringConstant Equal StringConstant      { Util.Attr($1, Some $3) }
| Kw_alignstack Equal Lparen APInt Rparen  { Util.Alignstack(int_of_string $4) }
| Kw_alwaysinline                          { Util.Alwaysinline     }
| Kw_builtin                               { Util.Builtin          }
| Kw_cold                                  { Util.Cold             }
| Kw_inlinehint                            { Util.Inlinehint       }
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
| typ opt_param_attribute value { ($1, $3, $2) }
;
opt_param_attribute:
| /* empty */     { None }
| Kw_align APInt  { Some(Util.Align(int_of_string $2)) }
| Kw_byval        { Some Util.Byval     }
| Kw_inalloca     { Some Util.Inalloca  }
| Kw_inreg        { Some Util.Inreg     }
| Kw_nest         { Some Util.Nest      }
| Kw_noalias      { Some Util.Noalias   }
| Kw_nocapture    { Some Util.Nocapture }
| Kw_readnone     { Some Util.Readnone  }
| Kw_readonly     { Some Util.Readonly  }
| Kw_returned     { Some Util.Returned  }
| Kw_signext      { Some Util.Signext   }
| Kw_sret         { Some Util.Sret      }
| Kw_zeroext      { Some Util.Zeroext   }
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
| Kw_inreg   { Util.Inreg  }
| Kw_noalias { Util.Noalias}
| Kw_signext { Util.Signext}
| Kw_zeroext { Util.Zeroext}
;
