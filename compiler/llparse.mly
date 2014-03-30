%{
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
%token <string> GlobalID
%token <string> GlobalVar
%token Greater
%token <string> LabelStr
%token Lbrace
%token Less
%token <string> LocalVar
%token <string> LocalVarID
%token Lparen
%token Lsquare
%token <string> MetadataVar
%token Rbrace
%token Rparen
%token Rsquare
%token Star
%token <string> StringConstant
%token <Util.oType> Type
%token Error
%token Kw_void
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
%type <unit list> main
%%
main:
| Eof { [] }
| toplevel main { $1::$2 }
;
toplevel:
| declare {$1}
| define  {$1}
| Kw_module Kw_asm StringConstant {()}
| Kw_target Kw_triple Equal StringConstant {()}
| Kw_target Kw_datalayout Equal StringConstant {()}
| Kw_deplibs Equal Lsquare stringlist Rsquare {()}
| LocalVarID Equal Kw_type Kw_opaque {()}
| LocalVarID Equal Kw_type typ {()}
| LocalVar Equal Kw_type Kw_opaque {()}
| LocalVar Equal Kw_type typ {()}
| global_def external_linkage opt_visibility opt_dll_storageclass opt_thread_local opt_addrspace opt_unnamed_addr opt_externally_initialized constant_or_global typ trailing_attributes {()}
| global_def non_external_linkage opt_visibility opt_dll_storageclass opt_thread_local opt_addrspace opt_unnamed_addr opt_externally_initialized constant_or_global typ value trailing_attributes {()}
| global_def external_linkage opt_visibility Kw_alias opt_linkage aliasee {()}
| global_def non_external_linkage opt_visibility Kw_alias opt_linkage aliasee {()}
| Exclaim APInt Equal typ Exclaim Lbrace mdnodevector Rbrace {()}
| MetadataVar Equal Exclaim Lbrace mdlist Rbrace {()}
| Kw_attributes AttrGrpID Equal Lbrace attribute_group Rbrace {()}
;
global_def:
| /* empty */ {()}
| GlobalID Equal {()}
| GlobalVar Equal {()}
;
aliasee:
| Kw_bitcast       Lparen type_value Kw_to typ Rparen {()}
| Kw_getelementptr opt_kw_inbounds Lparen type_value_list Rparen {()}
| type_value {()}
;
stringlist:
| /* empty */ { [] }
| StringConstant stringlist { $1::$2 }
;
mdlist:
| /* empty */ { [] }
| Exclaim APInt mdlist { $2::$3 }
;
mdnodevector:
| Kw_null {()}
| Kw_null mdnodevector {()}
| type_value {()}
| type_value mdnodevector {()}
;
constant_or_global:
| Kw_constant { true }
| Kw_global { false }
;
opt_thread_local:
| /* empty */ {()}
| Kw_thread_local {()}
| Kw_thread_local Lparen Kw_localdynamic Rparen {()}
| Kw_thread_local Lparen Kw_initialexec Rparen {()}
| Kw_thread_local Lparen Kw_localexec Rparen {()}
;
opt_addrspace:
| /* empty */ {()}
| Kw_addrspace Lparen APInt Rparen {()}
;
opt_unnamed_addr:
| /* empty */ {()}
| Kw_unnamed_addr {()}
;
opt_externally_initialized:
| /* empty */ {()}
| Kw_externally_initialized {()}
;
trailing_attributes:
| /* empty */ {()}
| Comma Kw_section StringConstant trailing_attributes {()}
| Comma Kw_align APInt trailing_attributes {()}
declare:
| Kw_declare function_header {$2}
;
define:
| Kw_define function_header function_body {()}
;
function_header:
| opt_linkage opt_visibility opt_dll_storageclass opt_callingconv opt_return_attrs
 typ global_name argument_list opt_unnamed_addr fnAttributeValuePairs opt_section
 opt_align opt_gc opt_prefix
 {()}
;
opt_dll_storageclass:
| /* empty */ {()}
| Kw_dllimport {()}
| Kw_dllexport {()}
;
opt_linkage:
| external_linkage {()}
| non_external_linkage {()}
;
external_linkage:
| Kw_extern_weak  {()}
| Kw_external  {()}
;
non_external_linkage:
| /* empty */ {()}
| Kw_private  {()}
| Kw_internal  {()}
| Kw_weak  {()}
| Kw_weak_odr  {()}
| Kw_linkonce  {()}
| Kw_linkonce_odr  {()}
| Kw_available_externally  {()}
| Kw_appending  {()}
| Kw_common  {()}
;
opt_visibility:
| /* empty */ {()}
| Kw_default {()}
| Kw_hidden {()}
| Kw_protected {()}
;
opt_callingconv:
| /* empty */ {()}
| Kw_ccc {()}
| Kw_fastcc {()}
| Kw_intel_ocl_bicc {()}
| Kw_coldcc {()}
| Kw_x86_stdcallcc {()}
| Kw_x86_fastcallcc {()}
| Kw_x86_thiscallcc {()}
| Kw_x86_cdeclmethodcc {()}
| Kw_arm_apcscc {()}
| Kw_arm_aapcscc {()}
| Kw_arm_aapcs_vfpcc {()}
| Kw_msp430_intrcc {()}
| Kw_ptx_kernel {()}
| Kw_ptx_device {()}
| Kw_spir_func {()}
| Kw_spir_kernel {()}
| Kw_x86_64_sysvcc {()}
| Kw_x86_64_win64cc {()}
| Kw_webkit_jscc {()}
| Kw_anyregcc {()}
| Kw_preserve_mostcc {()}
| Kw_preserve_allcc {()}
| Kw_cc {()}
;
opt_return_attrs:
| /* empty */ {()}
| attr opt_return_attrs {
  match $1 with
| Kw_inreg  -> ()
| Kw_noalias -> ()
| Kw_signext -> ()
| Kw_zeroext -> ()
| Kw_align
| Kw_byval
| Kw_inalloca
| Kw_nest
| Kw_nocapture
| Kw_returned
| Kw_sret ->
    Printf.eprintf "invalid use of parameter-only attribute\n";
    $2
| Kw_alignstack
| Kw_alwaysinline
| Kw_builtin
| Kw_cold
| Kw_inlinehint
| Kw_minsize
| Kw_naked
| Kw_nobuiltin
| Kw_noduplicate
| Kw_noimplicitfloat
| Kw_noinline
| Kw_nonlazybind
| Kw_noredzone
| Kw_noreturn
| Kw_nounwind
| Kw_optnone
| Kw_optsize
| Kw_returns_twice
| Kw_sanitize_address ->
    Printf.eprintf "invalid use of function-only attribute\n";
    $2
| _ -> failwith "impossible"
}
;
attr:
| Kw_inreg {Kw_inreg}
| Kw_noalias {Kw_noalias}
| Kw_signext {Kw_signext}
| Kw_zeroext {Kw_zeroext}
| Kw_align {Kw_align}
| Kw_byval {Kw_byval}
| Kw_inalloca {Kw_inalloca}
| Kw_nest {Kw_nest}
| Kw_nocapture {Kw_nocapture}
| Kw_returned {Kw_returned}
| Kw_sret {Kw_sret}
| Kw_alignstack {Kw_alignstack}
| Kw_alwaysinline {Kw_alwaysinline}
| Kw_builtin {Kw_builtin}
| Kw_cold {Kw_cold}
| Kw_inlinehint {Kw_inlinehint}
| Kw_minsize {Kw_minsize}
| Kw_naked {Kw_naked}
| Kw_nobuiltin {Kw_nobuiltin}
| Kw_noduplicate {Kw_noduplicate}
| Kw_noimplicitfloat {Kw_noimplicitfloat}
| Kw_noinline {Kw_noinline}
| Kw_nonlazybind {Kw_nonlazybind}
| Kw_noredzone {Kw_noredzone}
| Kw_noreturn {Kw_noreturn}
| Kw_nounwind {Kw_nounwind}
| Kw_optnone {Kw_optnone}
| Kw_optsize {Kw_optsize}
| Kw_returns_twice {Kw_returns_twice}
| Kw_sanitize_address {Kw_sanitize_address}
    ;
typ:
| Kw_void { () }
| non_void_type { $1 }
non_void_type:
| Type {()}
| LocalVar {()}
| LocalVarID {()}
| Lbrace Rbrace {()}
| Less Lbrace Rbrace Greater {()}
| Lbrace type_list Rbrace {()}
| Less Lbrace type_list Rbrace Greater {()}
| Lsquare APInt Kw_x typ Rsquare {()}
| Less APInt Kw_x typ Greater {()}
| typ Star {()}
| typ Kw_addrspace Lparen APInt Rparen {()}
| typ argument_list {()}
;
type_list:
| typ { [$1] }
| typ Comma type_list { $1::$3 }
global_name:
| GlobalID {()}
| GlobalVar {()}
;
argument_list:
Lparen arg_type_list Rparen {$2}
;
arg_type_list:
| /* empty */    {()}
| DotDotDot {()}
| arg_type {()}
| arg_type Comma arg_type_list {()}
;
arg_type:
| typ {()}
| typ LocalVar {()}
;
opt_section:
| /* empty */    {()}
| Kw_section StringConstant {()}
;
opt_align:
| /* empty */    {()}
| Kw_align APInt {()}
    ;
opt_inbounds:
| /* empty */    { false }
| Kw_inbounds { true }
;
opt_tail:
| /* empty */    { false }
| Kw_tail { true }
;
opt_cleanup:
| /* empty */    { false }
| Kw_cleanup { true }
;
opt_comma_align:
| /* empty */    {()}
| Comma Kw_align APInt {()}
    ;
opt_gc:
| /* empty */    {()}
| Kw_gc StringConstant {()}
    ;
opt_prefix:
| /* empty */    {()}
| Kw_prefix typ value {()}
;
opt_atomic:
| /* empty */    { false }
| Kw_atomic    { true }
;
opt_volatile:
| /* empty */    { false }
| Kw_volatile    { true }
;
value:
| GlobalID {()}
| GlobalVar {()}
| LocalVarID {()}
| LocalVar {()}
| Exclaim metadataValue {()}
| APInt {()}
| APFloat {()}
| Kw_true {()}
| Kw_false  {()}
| Kw_null {()}
| Kw_undef {()}
| Kw_zeroinitializer {()}
| Lbrace type_value_list Rbrace {()}
| Less Lbrace Rbrace Greater {()}
| Less Lbrace type_value_LIST Rbrace Greater {()}
| Less type_value_list Greater {()}
| Lsquare type_value_list Rsquare {()}
| Kw_c StringConstant {()}
| Kw_asm opt_sideeffect opt_alignstack opt_inteldialect StringConstant Comma StringConstant {()}
| Kw_blockaddress Lparen value Comma value Rparen {()}
| Kw_trunc         Lparen type_value Kw_to typ Rparen {()}
| Kw_zext          Lparen type_value Kw_to typ Rparen {()}
| Kw_sext          Lparen type_value Kw_to typ Rparen {()}
| Kw_fptrunc       Lparen type_value Kw_to typ Rparen {()}
| Kw_fpext         Lparen type_value Kw_to typ Rparen {()}
| Kw_bitcast       Lparen type_value Kw_to typ Rparen {()}
| Kw_addrspacecast Lparen type_value Kw_to typ Rparen {()}
| Kw_uitofp        Lparen type_value Kw_to typ Rparen {()}
| Kw_sitofp        Lparen type_value Kw_to typ Rparen {()}
| Kw_fptoui        Lparen type_value Kw_to typ Rparen {()}
| Kw_fptosi        Lparen type_value Kw_to typ Rparen {()}
| Kw_inttoptr      Lparen type_value Kw_to typ Rparen {()}
| Kw_ptrtoint      Lparen type_value Kw_to typ Rparen {()}
| Kw_extractvalue Lparen type_value index_list Rparen {()}
| Kw_insertvalue Lparen type_value Comma type_value index_list Rparen {()}
| Kw_icmp icmp_predicate Lparen type_value Comma type_value Rparen {()}
| Kw_fcmp fcmp_predicate Lparen type_value Comma type_value Rparen {()}

| Kw_add opt_kw_nuw opt_nsw_nuw Lparen type_value Comma type_value Rparen {()}
| Kw_sub opt_kw_nuw opt_nsw_nuw Lparen type_value Comma type_value Rparen {()}
| Kw_mul opt_kw_nuw opt_nsw_nuw Lparen type_value Comma type_value Rparen {()}
| Kw_shl opt_kw_nuw opt_nsw_nuw Lparen type_value Comma type_value Rparen {()}

| Kw_sdiv opt_exact             Lparen type_value Comma type_value Rparen {()}
| Kw_udiv opt_exact             Lparen type_value Comma type_value Rparen {()}
| Kw_lshr opt_exact             Lparen type_value Comma type_value Rparen {()}
| Kw_ashr opt_exact             Lparen type_value Comma type_value Rparen {()}

| Kw_fadd                       Lparen type_value Comma type_value Rparen {()}
| Kw_fsub                       Lparen type_value Comma type_value Rparen {()}
| Kw_fmul                       Lparen type_value Comma type_value Rparen {()}
| Kw_fdiv                       Lparen type_value Comma type_value Rparen {()}
| Kw_urem                       Lparen type_value Comma type_value Rparen {()}
| Kw_srem                       Lparen type_value Comma type_value Rparen {()}
| Kw_frem                       Lparen type_value Comma type_value Rparen {()}

| Kw_and Lparen type_value Comma type_value Rparen {()}
| Kw_or  Lparen type_value Comma type_value Rparen {()}
| Kw_xor Lparen type_value Comma type_value Rparen {()}

| Kw_getelementptr opt_kw_inbounds Lparen type_value_list Rparen {()}
| Kw_shufflevector                 Lparen type_value_list Rparen {()}
| Kw_insertelement                 Lparen type_value_list Rparen {()}
| Kw_extractelement                Lparen type_value_list Rparen {()}
| Kw_select                        Lparen type_value_list Rparen {()}
;
opt_sideeffect:
| /* empty */ { false }
| Kw_sideeffect { true }
;
opt_alignstack:
| /* empty */ { false }
| Kw_alignstack { true }
;
opt_inteldialect:
| /* empty */ { false }
| Kw_inteldialect { true }
;
opt_kw_inbounds:
| /* empty */ { false }
| Kw_inbounds { true }
;
opt_kw_nuw:
| /* empty */ { false }
| Kw_nuw { true }
opt_exact:
| /* empty */ { false }
| Kw_exact { true }
;
opt_nsw_nuw:
| /* empty */ {()}
| Kw_nsw opt_kw_nuw {()}
;
metadataValue:
| APInt    {()}
| StringConstant    {()}
| Lbrace mdnodevector Rbrace    {()}
;
type_value_LIST:
| type_value { [$1] }
| type_value Comma type_value_LIST { $1::$3 }
;
type_value_list:
| /* empty */ { [] }
| type_value { [$1] }
| type_value Comma type_value_list { $1::$3 }
;
index_list:
| Comma APInt    { [$2] }
| Comma APInt index_list    { $2::$3 }
;
fcmp_predicate:
| Kw_oeq {()}
| Kw_one {()}
| Kw_olt {()}
| Kw_ogt {()}
| Kw_ole {()}
| Kw_oge {()}
| Kw_ord {()}
| Kw_uno {()}
| Kw_ueq {()}
| Kw_une {()}
| Kw_ult {()}
| Kw_ugt {()}
| Kw_ule {()}
| Kw_uge {()}
| Kw_true {()}
| Kw_false {()}
;
icmp_predicate:
| Kw_eq {()}
| Kw_ne {()}
| Kw_slt {()}
| Kw_sgt {()}
| Kw_sle {()}
| Kw_sge {()}
| Kw_ult {()}
| Kw_ugt {()}
| Kw_ule {()}
| Kw_uge {()}
;
function_body: 
| Lbrace basicblock_list Rbrace { $2 }
;
basicblock_list:
| basicblock { [$1] }
| basicblock basicblock_list { $1::$2 }
;
basicblock:
| opt_labelstr instruction_list { ($1, $2) }
;  
instruction_list:
| terminator_instruction { [$1] }
| instruction instruction_list { $1::$2 }
;
opt_labelstr:
| /* empty */ { None }
| LabelStr { Some $1 }
;
local:
| LocalVarID Equal {()}
| LocalVar Equal {()}
;
opt_local:
| /* empty */ { None }
| local { Some $1 }
;
arithmetic:
| type_value Comma value {()}
;
logical:
| type_value Comma value {()}
;
cast:
| type_value Kw_to typ {()}
;
compare:
| type_value Comma value {()}
;
instruction:
| local Kw_add opt_kw_nuw opt_nsw_nuw arithmetic {()}
| local Kw_sub opt_kw_nuw opt_nsw_nuw arithmetic {()}
| local Kw_mul opt_kw_nuw opt_nsw_nuw arithmetic {()}
| local Kw_shl opt_kw_nuw opt_nsw_nuw arithmetic {()}
| local Kw_fadd fastmathflags arithmetic {()}
| local Kw_fsub fastmathflags arithmetic {()}
| local Kw_fmul fastmathflags arithmetic {()}
| local Kw_fdiv fastmathflags arithmetic {()}
| local Kw_frem fastmathflags arithmetic {()}
| local Kw_sdiv opt_exact arithmetic {()}
| local Kw_udiv opt_exact arithmetic {()}
| local Kw_lshr opt_exact arithmetic {()}
| local Kw_ashr opt_exact arithmetic {()}
| local Kw_urem arithmetic {()}
| local Kw_srem arithmetic {()}
| local Kw_and logical {()}
| local Kw_or logical {()}
| local Kw_xor logical {()}
| local Kw_icmp icmp_predicate compare {()}
| local Kw_fcmp fcmp_predicate compare {()}
| local Kw_trunc cast {()}
| local Kw_zext cast {()}
| local Kw_sext cast {()}
| local Kw_fptrunc cast {()}
| local Kw_fpext cast {()}
| local Kw_bitcast cast {()}
| local Kw_addrspacecast cast {()}
| local Kw_uitofp cast {()}
| local Kw_sitofp cast {()}
| local Kw_fptoui cast {()}
| local Kw_fptosi cast {()}
| local Kw_inttoptr cast {()}
| local Kw_ptrtoint cast {()}
| local Kw_va_arg type_value Comma typ {()}
| local Kw_getelementptr opt_inbounds type_value_LIST {()}
| local Kw_extractelement type_value_LIST {()}
| local Kw_insertelement type_value_LIST {()}
| local Kw_shufflevector type_value_LIST {()}
| local Kw_select type_value_LIST {()}
| local Kw_phi typ phi_list {()}
| local Kw_landingpad typ Kw_personality type_value opt_cleanup landingpad_list {()}
| opt_local opt_tail Kw_call opt_callingconv opt_return_attrs typ value parameterList call_attributes {()}
| local Kw_alloca alloc {()}
| local Kw_load  opt_atomic opt_volatile type_value scopeandordering opt_comma_align {()}
| Kw_store opt_atomic opt_volatile type_value Comma type_value scopeandordering opt_comma_align {()}
| Kw_cmpxchg opt_volatile type_value Comma type_value Comma type_value opt_singlethread ordering ordering {()}
| Kw_atomicrmw opt_volatile binop type_value Comma type_value opt_singlethread ordering {()}
| Kw_fence opt_singlethread ordering {()}
| local Kw_extractvalue type_value index_list {()}
| local Kw_insertvalue type_value Comma type_value index_list {()}
;
binop:
| Kw_xchg {()}
| Kw_add {()}
| Kw_sub {()}
| Kw_and {()}
| Kw_nand {()}
| Kw_or {()}
| Kw_xor {()}
| Kw_max {()}
| Kw_min {()}
| Kw_umax {()}
| Kw_umin {()}
;
phi_list:
| Lsquare value Comma value Rsquare {()}
| phi_list Comma Lsquare value Comma value Rsquare {()}
;
landingpad_list:
| Kw_catch type_value {()}
| Kw_filter type_value {()}
| landingpad_list Kw_catch type_value {()}
| landingpad_list Kw_filter type_value {()}
;
ordering:
| Kw_unordered {()}
| Kw_monotonic {()}
| Kw_acquire {()}
| Kw_release {()}
| Kw_acq_rel {()}
| Kw_seq_cst {()}
;
opt_singlethread:
| /* empty */ { false }
| Kw_singlethread { true }
scopeandordering:
| /* empty */ {()}
| Kw_singlethread ordering {()}
| ordering {()}
;
alloc:
| Kw_inalloca typ Comma type_value Comma Kw_align APInt {()}
| Kw_inalloca typ Comma type_value {()}
| Kw_inalloca typ Comma Kw_align APInt {()}
| Kw_inalloca typ {()}
| typ Comma type_value Comma Kw_align APInt {()}
| typ Comma type_value {()}
| typ Comma Kw_align APInt {()}
| typ {()}
fastmathflags:
| Kw_fast {()}
| Kw_nnan {()}
| Kw_ninf {()}
| Kw_nsz {()}
| Kw_arcp {()}
;
terminator_instruction:
| Kw_unreachable {()}
| Kw_ret Kw_void {()} /* we need to distinguish void from all other types else we have a dependent grammar */
| Kw_ret non_void_type value {()} 
| Kw_br type_value {()}
| Kw_br type_value Comma type_value Comma type_value {()}
| Kw_indirectbr type_value Comma Lsquare type_value_LIST Rsquare {()}
| Kw_resume type_value {()}
| Kw_switch type_value Comma type_value Lsquare jumpTable Rsquare {()}
| local Kw_invoke opt_callingconv opt_return_attrs typ value parameterList fnAttributeValuePairs Kw_to type_value Kw_unwind type_value {()}
;
call_attributes:
| /* empty */ {()}
| Kw_noreturn call_attributes {()}
| Kw_nounwind call_attributes {()}
| Kw_readnone call_attributes {()}
| Kw_readonly call_attributes {()}
;
fnAttributeValuePairs:
| /* empty */ {()}
| AttrGrpID fnAttributeValuePairs {()}
| StringConstant Equal StringConstant fnAttributeValuePairs {()}
/*| Kw_align APInt fnAttributeValuePairs {()} not needed since always printed after section, if we use this it causes 31 shift/reduce conflicts */
| Kw_alignstack Equal Lparen APSint Rparen fnAttributeValuePairs {()}
| Kw_alwaysinline fnAttributeValuePairs {()}
| Kw_builtin fnAttributeValuePairs {()}
| Kw_cold fnAttributeValuePairs {()}
| Kw_inlinehint fnAttributeValuePairs {()}
| Kw_minsize fnAttributeValuePairs {()}
| Kw_naked fnAttributeValuePairs {()}
| Kw_nobuiltin fnAttributeValuePairs {()}
| Kw_noduplicate fnAttributeValuePairs {()}
| Kw_noimplicitfloat fnAttributeValuePairs {()}
| Kw_noinline fnAttributeValuePairs {()}
| Kw_nonlazybind fnAttributeValuePairs {()}
| Kw_noredzone fnAttributeValuePairs {()}
| Kw_noreturn fnAttributeValuePairs {()}
| Kw_nounwind fnAttributeValuePairs {()}
| Kw_optnone fnAttributeValuePairs {()}
| Kw_optsize fnAttributeValuePairs {()}
| Kw_readnone fnAttributeValuePairs {()}
| Kw_readonly fnAttributeValuePairs {()}
| Kw_returns_twice fnAttributeValuePairs {()}
| Kw_ssp fnAttributeValuePairs {()}
| Kw_sspreq fnAttributeValuePairs {()}
| Kw_sspstrong fnAttributeValuePairs {()}
| Kw_sanitize_address fnAttributeValuePairs {()}
| Kw_sanitize_thread fnAttributeValuePairs {()}
| Kw_sanitize_memory fnAttributeValuePairs {()}
| Kw_uwtable fnAttributeValuePairs {()}
;
attribute_group:
| /* empty */ {()}
| StringConstant attribute_group {()}
| StringConstant Equal StringConstant attribute_group {()}
| Kw_align Equal APSint attribute_group {()}
| Kw_alignstack Equal APSint attribute_group {()}
| Kw_alwaysinline attribute_group {()}
| Kw_builtin attribute_group {()}
| Kw_cold attribute_group {()}
| Kw_inlinehint attribute_group {()}
| Kw_minsize attribute_group {()}
| Kw_naked attribute_group {()}
| Kw_nobuiltin attribute_group {()}
| Kw_noduplicate attribute_group {()}
| Kw_noimplicitfloat attribute_group {()}
| Kw_noinline attribute_group {()}
| Kw_nonlazybind attribute_group {()}
| Kw_noredzone attribute_group {()}
| Kw_noreturn attribute_group {()}
| Kw_nounwind attribute_group {()}
| Kw_optnone attribute_group {()}
| Kw_optsize attribute_group {()}
| Kw_readnone attribute_group {()}
| Kw_readonly attribute_group {()}
| Kw_returns_twice attribute_group {()}
| Kw_ssp attribute_group {()}
| Kw_sspreq attribute_group {()}
| Kw_sspstrong attribute_group {()}
| Kw_sanitize_address attribute_group {()}
| Kw_sanitize_thread attribute_group {()}
| Kw_sanitize_memory attribute_group {()}
| Kw_uwtable attribute_group {()}
;
parameterList:
| Lparen argList Rparen { $2 }
;
argList:
| /* empty */ { []}
| arg { [$1] }
| arg Comma argList { $1::$3 }
;
arg:
| typ opt_param_attrs value {()}
;
opt_param_attrs:
| /* empty */ {()}
| Kw_align APSint {()}
| Kw_byval {()}
| Kw_inalloca {()}
| Kw_inreg {()}
| Kw_nest {()}
| Kw_noalias {()}
| Kw_nocapture {()}
| Kw_readnone {()}
| Kw_readonly {()}
| Kw_returned {()}
| Kw_signext {()}
| Kw_sret {()}
| Kw_zeroext {()}
;
jumpTable:
| /* empty */ { [] }
| type_value Comma type_value jumpTable { ($1,$3)::$4 }
;
type_value:
| typ value {()}
;
