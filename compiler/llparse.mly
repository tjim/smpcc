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
%type <unit> main
%%
main: Eof {()}
| declare main {$1}
| define  main {$1}
| Kw_module Kw_asm StringConstant main {()}
| Kw_target Kw_triple Equal StringConstant main {()}
| Kw_target Kw_datalayout Equal StringConstant main {()}
| Kw_deplibs Equal Lsquare stringlist Rsquare main {()}
| LocalVarID Equal Kw_type Kw_opaque main {()}
| LocalVarID Equal Kw_type typ main {()}
| LocalVar Equal Kw_type Kw_opaque main {()}
| LocalVar Equal Kw_type typ main {()}
| GlobalID Equal optionalLinkage optionalVisibility optionalDLLStorageClass global main {()}
| GlobalID Equal optionalVisibility Kw_alias optionalLinkage aliasee main {()}
| GlobalVar Equal optionalLinkage optionalVisibility optionalDLLStorageClass global main {()}
| GlobalVar Equal optionalVisibility Kw_alias optionalLinkage aliasee main {()}
| Exclaim APInt Equal typ Exclaim Lbrace mdnodevector Rbrace main {()}
| MetadataVar Equal Exclaim Lbrace mdlist Rbrace main {()}
;
aliasee:
| Kw_bitcast       Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_getelementptr opt_kw_inbounds Lparen globalValueVector Rparen {()}
| globalTypeAndValue {()}
;
stringlist:
| /* empty */ {()}
| StringConstant stringlist {()}
;
mdlist:
| /* empty */ {()}
| Exclaim APInt mdlist {()}
;
mdnodevector:
| Kw_null {()}
| typeAndValue {()}
| mdnodevector Kw_null {()}
| mdnodevector typeAndValue {()}
;
global:
| optionalThreadLocal optionalAddrSpace optionalUnnamedAddr optionalExternallyInitialized Kw_constant typ opt_globalValue trailing_attributes {()}
| optionalThreadLocal optionalAddrSpace optionalUnnamedAddr optionalExternallyInitialized Kw_global typ opt_globalValue trailing_attributes {()}
;
optionalThreadLocal:
| /* empty */ {()}
| Kw_thread_local {()}
| Kw_thread_local Lparen Kw_localdynamic Rparen {()}
| Kw_thread_local Lparen Kw_initialexec Rparen {()}
| Kw_thread_local Lparen Kw_localexec Rparen {()}
;
optionalAddrSpace:
| /* empty */ {()}
| Kw_addrspace Lparen APInt Rparen {()}
;
optionalUnnamedAddr:
| /* empty */ {()}
| Kw_unnamed_addr {()}
;
optionalExternallyInitialized:
| /* empty */ {()}
| Kw_externally_initialized {()}
;
trailing_attributes:
| /* empty */ {()}
| Comma Kw_section StringConstant trailing_attributes {()}
| Comma Kw_align APInt trailing_attributes {()}
declare:
| Kw_declare functionHeader {$2}
;
define:
| Kw_define functionHeader functionBody {()}
;
functionHeader:
 optionalLinkage optionalVisibility optionalDLLStorageClass optionalCallingConv optionalReturnAttrs
 typ globalName argumentList optionalUnnamedAddr fnAttributeValuePairs optSection
 optionalAlign optGC optionalPrefix
 {()}
;
optionalDLLStorageClass:
| /* empty */ {()}
| Kw_dllimport {()}
| Kw_dllexport {()}
;
optionalLinkage:
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
| Kw_extern_weak  {()}
| Kw_external  {()}
;
optionalVisibility:
| /* empty */ {()}
| Kw_default {()}
| Kw_hidden {()}
| Kw_protected {()}
;
optionalCallingConv:
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
optionalReturnAttrs:
| /* empty */ {()}
| attr optionalReturnAttrs {
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
| Kw_void {()}
| non_void_type {()}
non_void_type:
| Type {()}
| LocalVar {()}
| LocalVarID {()}
| structBody {()}
| Lsquare APInt Kw_x typ Rsquare {()}
| Less APInt Kw_x typ Greater {()}
| typ Star {()}
| typ Kw_addrspace Lparen APInt Rparen {()}
| typ argumentList {()}
;
/*
typeSuffix:
| {()}
| Star typeSuffix {()}
| Kw_addrspace Lparen APSint Rparen typeSuffix {()}
| argumentList typeSuffix {()}
;
*/
structBody:
| Lbrace typeList Rbrace {()}
| Less Lbrace typeList Rbrace Greater {()}
;
typeList:
| /* empty */    {()}
| typ {()}
| typ Comma typeList {()}
globalName:
| GlobalID {()}
| GlobalVar {()}
    ;
argumentList:
Lparen argTypeList Rparen {$2}
;
argTypeList:
| /* empty */    {()}
| DotDotDot {()}
| argType {()}
| argType Comma argTypeList {()}
;
argType:
| typ {()}
| typ LocalVar {()}
;
optSection:
| /* empty */    {()}
| Kw_section StringConstant {()}
;
optionalAlign:
| /* empty */    {()}
| Kw_align APInt {()}
    ;
opt_inbounds:
| /* empty */    {()}
| Kw_inbounds {()}
;
opt_tail:
| /* empty */    {()}
| Kw_tail {()}
;
opt_cleanup:
| /* empty */    {()}
| Kw_cleanup {()}
;
opt_comma_align:
| /* empty */    {()}
| Comma Kw_align APInt {()}
    ;
optGC:
| /* empty */    {()}
| Kw_gc StringConstant {()}
    ;
optionalPrefix:
| /* empty */    {()}
| Kw_prefix typ globalValue {()}
;
globalValue:
| valID {()}
;
opt_globalValue:
| /* empty */    {()}
| globalValue    {()}
;
opt_atomic:
| /* empty */    {()}
| Kw_atomic    {()}
;
opt_volatile:
| /* empty */    {()}
| Kw_volatile    {()}
;
valID:
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
| Lbrace globalValueVector Rbrace {()}
| Less Lbrace globalValueVector Rbrace Greater {()}
| Less globalValueVector Greater {()}
| Lsquare globalValueVector Rsquare {()}
| Kw_c StringConstant {()}
| Kw_asm optSideeffect optAlignStack optIntelDialect StringConstant Comma StringConstant {()}
| Kw_blockaddress Lparen valID Comma valID Rparen {()}
| Kw_trunc         Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_zext          Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_sext          Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_fptrunc       Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_fpext         Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_bitcast       Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_addrspacecast Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_uitofp        Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_sitofp        Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_fptoui        Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_fptosi        Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_inttoptr      Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_ptrtoint      Lparen globalTypeAndValue Kw_to typ Rparen {()}
| Kw_extractvalue Lparen globalTypeAndValue indexList Rparen {()}
| Kw_insertvalue Lparen globalTypeAndValue Comma globalTypeAndValue indexList Rparen {()}
| Kw_icmp cmpPredicate Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_fcmp cmpPredicate Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}

| Kw_add opt_kw_nuw opt_nsw_nuw Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_sub opt_kw_nuw opt_nsw_nuw Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_mul opt_kw_nuw opt_nsw_nuw Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_shl opt_kw_nuw opt_nsw_nuw Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}

| Kw_sdiv opt_exact             Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_udiv opt_exact             Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_lshr opt_exact             Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_ashr opt_exact             Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}

| Kw_fadd                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_fsub                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_fmul                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_fdiv                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_urem                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_srem                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_frem                       Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}

| Kw_and Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_or  Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}
| Kw_xor Lparen globalTypeAndValue Comma globalTypeAndValue Rparen {()}

| Kw_getelementptr opt_kw_inbounds Lparen globalValueVector Rparen {()}
| Kw_shufflevector                 Lparen globalValueVector Rparen {()}
| Kw_insertelement                 Lparen globalValueVector Rparen {()}
| Kw_extractelement                Lparen globalValueVector Rparen {()}
| Kw_select                        Lparen globalValueVector Rparen {()}
;
optSideeffect:
| /* empty */ {()}
| Kw_sideeffect {()}
;
optAlignStack:
| /* empty */ {()}
| Kw_alignstack {()}
;
optIntelDialect:
| /* empty */ {()}
| Kw_inteldialect {()}
;
opt_kw_inbounds:
| /* empty */ {()}
| Kw_inbounds {()}
;
opt_kw_nuw:
| /* empty */ {()}
| Kw_nuw {()}
opt_exact:
| /* empty */ {()}
| Kw_exact {()}
;
opt_nsw_nuw:
| /* empty */ {()}
| Kw_nsw opt_kw_nuw {()}
;
metadataValue: {()};
globalValueVector:
| /* empty */ {()}
| globalTypeAndValue {()}
| globalTypeAndValue Comma globalValueVector {()}
;
globalTypeAndValue:
| typ globalValue {()}
;
indexList: {()};
cmpPredicate: {()};
functionBody: 
| Lbrace basicBlockList Rbrace {()}
;
basicBlockList:
| basicBlock {()}
| basicBlock basicBlockList {()}
;
basicBlock:
| opt_labelstr instructionList opt_local termInst {()}
;  
instructionList:
| /* empty */ {()}
| opt_local instruction instructionList {()}
;
opt_labelstr:
| /* empty */ {()}
| LabelStr {()}
;
opt_local:
| /* empty */ {()}
| LocalVarID Equal {()}
| LocalVar Equal {()}
;
arithmetic:
| typeAndValue Comma value {()}
;
logical:
| typeAndValue Comma value {()}
;
cast:
| typeAndValue Kw_to typ {()}
;
compare:
| typeAndValue Comma value {()}
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
instruction:
| Kw_add opt_kw_nuw opt_nsw_nuw arithmetic {()}
| Kw_sub opt_kw_nuw opt_nsw_nuw arithmetic {()}
| Kw_mul opt_kw_nuw opt_nsw_nuw arithmetic {()}
| Kw_shl opt_kw_nuw opt_nsw_nuw arithmetic {()}
| Kw_fadd fastmathflags arithmetic {()}
| Kw_fsub fastmathflags arithmetic {()}
| Kw_fmul fastmathflags arithmetic {()}
| Kw_fdiv fastmathflags arithmetic {()}
| Kw_frem fastmathflags arithmetic {()}
| Kw_sdiv opt_exact arithmetic {()}
| Kw_udiv opt_exact arithmetic {()}
| Kw_lshr opt_exact arithmetic {()}
| Kw_ashr opt_exact arithmetic {()}
| Kw_urem arithmetic {()}
| Kw_srem arithmetic {()}
| Kw_and logical {()}
| Kw_or logical {()}
| Kw_xor logical {()}
| Kw_icmp icmp_predicate compare {()}
| Kw_fcmp fcmp_predicate compare {()}
| Kw_trunc cast {()}
| Kw_zext cast {()}
| Kw_sext cast {()}
| Kw_fptrunc cast {()}
| Kw_fpext cast {()}
| Kw_bitcast cast {()}
| Kw_addrspacecast cast {()}
| Kw_uitofp cast {()}
| Kw_sitofp cast {()}
| Kw_fptoui cast {()}
| Kw_fptosi cast {()}
| Kw_inttoptr cast {()}
| Kw_ptrtoint cast {()}
| Kw_va_arg typeAndValue Comma typ {()}
| Kw_getelementptr opt_inbounds typeAndValueList {()}
| Kw_extractelement typeAndValueList {()}
| Kw_insertelement typeAndValueList {()}
| Kw_shufflevector typeAndValueList {()}
| Kw_select typeAndValueList {()}
| Kw_phi typ phi_list {()}
| Kw_landingpad typ Kw_personality typeAndValue opt_cleanup landingpad_list {()}
| opt_tail Kw_call optionalCallingConv optionalReturnAttrs typ valID parameterList fnAttributeValuePairs {()}
| Kw_alloca alloc {()}
| Kw_load  opt_atomic opt_volatile typeAndValue scopeandordering opt_comma_align {()}
| Kw_store opt_atomic opt_volatile typeAndValue Comma typeAndValue scopeandordering opt_comma_align {()}
| Kw_cmpxchg opt_volatile typeAndValue Comma typeAndValue Comma typeAndValue scopeandordering ordering {()}
| Kw_atomicrmw opt_volatile binop typeAndValue Comma typeAndValue scopeandordering {()}
| Kw_fence scopeandordering {()}
| Kw_extractvalue typeAndValue indexList {()}
| Kw_insertvalue typeAndValue Comma typeAndValue indexList {()}
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
| Kw_catch typeAndValue {()}
| Kw_filter typeAndValue {()}
| landingpad_list Kw_catch typeAndValue {()}
| landingpad_list Kw_filter typeAndValue {()}
;
ordering:
| Kw_unordered {()}
| Kw_monotonic {()}
| Kw_acquire {()}
| Kw_release {()}
| Kw_acq_rel {()}
| Kw_seq_cst {()}
;
scopeandordering:
| /* empty */ {()}
| Kw_singlethread ordering {()}
| ordering {()}
;
alloc:
| Kw_inalloca typ Comma typeAndValue Comma Kw_align APInt {()}
| Kw_inalloca typ Comma typeAndValue {()}
| Kw_inalloca typ Comma Kw_align APInt {()}
| Kw_inalloca typ {()}
| typ Comma typeAndValue Comma Kw_align APInt {()}
| typ Comma typeAndValue {()}
| typ Comma Kw_align APInt {()}
| typ {()}
fastmathflags:
| Kw_fast {()}
| Kw_nnan {()}
| Kw_ninf {()}
| Kw_nsz {()}
| Kw_arcp {()}
;
termInst:
| Kw_unreachable {()}
| Kw_ret Kw_void {()} /* we need to distinguish void from all other types else we have a dependent grammar */
| Kw_ret non_void_type value {()} 
| Kw_br typeAndValue {()}
| Kw_br typeAndValue Comma typeAndValue Comma typeAndValue {()}
| Kw_indirectbr typeAndValue Comma Lsquare destList Rsquare {()}
| Kw_resume typeAndValue {()}
| Kw_switch typeAndValue Comma typeAndValue Lsquare jumpTable Rsquare {()}
| Kw_invoke optionalCallingConv optionalReturnAttrs typ valID parameterList fnAttributeValuePairs Kw_to typeAndBasicBlock Kw_unwind typeAndBasicBlock {()}
;
fnAttributeValuePairs:
| /* empty */ {()}
| AttrGrpID fnAttributeValuePairs {()}
| StringConstant Equal StringConstant fnAttributeValuePairs {()}
| Kw_align Equal APSint fnAttributeValuePairs {()}
| Kw_alignstack Equal APSint fnAttributeValuePairs {()}
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
| Kw_inreg{failwith "invalid use of attribute on a function"}
| Kw_signext{failwith "invalid use of attribute on a function"}
| Kw_zeroext {failwith "invalid use of attribute on a function"}
| Kw_byval {failwith "invalid use of parameter-only attribute on a function"}
| Kw_inalloca {failwith "invalid use of parameter-only attribute on a function"}
| Kw_nest {failwith "invalid use of parameter-only attribute on a function"}
| Kw_noalias {failwith "invalid use of parameter-only attribute on a function"}
| Kw_nocapture {failwith "invalid use of parameter-only attribute on a function"}
| Kw_returned {failwith "invalid use of parameter-only attribute on a function"}
| Kw_sret {failwith "invalid use of parameter-only attribute on a function"}
;
parameterList:
| Lparen argList Rparen {()}
;
argList:
| /* empty */ {()}
| arg {()}
| arg Comma argList {()}
;
arg:
| typ optionalParamAttrs value {()}
;
optionalParamAttrs:
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
| Kw_alignstack       {failwith "invalid use of function-only attribute"}               
| Kw_alwaysinline     {failwith "invalid use of function-only attribute"}
| Kw_builtin          {failwith "invalid use of function-only attribute"}
| Kw_inlinehint       {failwith "invalid use of function-only attribute"}
| Kw_minsize          {failwith "invalid use of function-only attribute"}
| Kw_naked            {failwith "invalid use of function-only attribute"}
| Kw_nobuiltin        {failwith "invalid use of function-only attribute"}
| Kw_noduplicate      {failwith "invalid use of function-only attribute"}
| Kw_noimplicitfloat  {failwith "invalid use of function-only attribute"}
| Kw_noinline         {failwith "invalid use of function-only attribute"}
| Kw_nonlazybind      {failwith "invalid use of function-only attribute"}
| Kw_noredzone        {failwith "invalid use of function-only attribute"}
| Kw_noreturn         {failwith "invalid use of function-only attribute"}
| Kw_nounwind         {failwith "invalid use of function-only attribute"}
| Kw_optnone          {failwith "invalid use of function-only attribute"}
| Kw_optsize          {failwith "invalid use of function-only attribute"}
| Kw_returns_twice    {failwith "invalid use of function-only attribute"}
| Kw_sanitize_address {failwith "invalid use of function-only attribute"}
| Kw_sanitize_memory  {failwith "invalid use of function-only attribute"}
| Kw_sanitize_thread  {failwith "invalid use of function-only attribute"}
| Kw_ssp              {failwith "invalid use of function-only attribute"}
| Kw_sspreq           {failwith "invalid use of function-only attribute"}
| Kw_sspstrong        {failwith "invalid use of function-only attribute"}
| Kw_uwtable          {failwith "invalid use of function-only attribute"}
;
jumpTable:
| /* empty */ {()}
| typeAndValue Comma typeAndBasicBlock jumpTable {()}
;
destList:
| typeAndBasicBlock {()}
| typeAndBasicBlock Comma destList {()}
;
typeAndBasicBlock:
| typeAndValue {()}
;
typeAndValue:
| typ value {()}
;
typeAndValueList:
| typeAndValue {()}
| typeAndValue Comma typeAndValueList {()}
;
value:
| valID {()}
;
