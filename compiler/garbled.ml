(* garbled circuit back end *)

open Llabs
open Printf
open Options

let package_prefix = "github.com/tjim/smpcc/runtime/"

let string_constants = Hashtbl.create 10

let govar v =
  Str.global_replace (Str.regexp "[%@.]") "_" (State.v_map v)

let rec bpr_go_value b is_gen (typ, value) =
  let pkg = if is_gen then "gen." else "eval." in
  match value with
  | Var v ->
      let is_a_global = (match v with Name(x,_) -> x | Id(x,_) -> x) in
      if is_a_global then
        (try
          let loc = Hashtbl.find State.global_locations v in
          bprintf b "%sUint(vm, %d, 64)" pkg loc
        with Not_found ->
          eprintf "global not there %s\n" (govar v);
          bprintf b "%s" (govar v))
      else
        bprintf b "%s" (govar v)
  | Basicblock bl ->
      bprintf b "%sUint(vm, %d, %d)" pkg (State.bl_num bl) (State.get_bl_bits())
  | Int x ->
      bprintf b "%sInt(vm, %s, %d)" pkg (Big_int.string_of_big_int x) (State.bitwidth typ)
  | Zero ->
      bprintf b "%sUint(vm, 0, %d) /* CAUTION: zero */" pkg (State.bitwidth typ)
  | Null ->
      bprintf b "%sUint(vm, 0, %d) /* CAUTION: null */" pkg (State.bitwidth typ)
  | Undef ->
      bprintf b "%sUint(vm, 0, %d) /* CAUTION: undef */" pkg (State.bitwidth typ)
  | Inttoptr(x, y) ->
      bprintf b "/* CAUTION: inttoptr */ ";
      bpr_go_value b is_gen x
  | op ->
      bpr_value b op

let bpr_go_instr b is_gen declared_vars (nopt,i) =
  let pkg = if is_gen then "gen." else "eval." in
  let bpr_go_value = (fun b -> bpr_go_value b is_gen) in
  (* Look for a special case: clang declares a variable for the result of a printf that is not subsequently used.
     Go has a stricter check on this.
   *)
  let unused =
    (match i with
    | Call(_,_,_,_,Var(Name(true, ("printf" | "puts" | "putchar"))),_,_,_) -> true
    | Store _ -> true
    | _ -> false) in
  (* Go does not permit re-declaration: in var := expr, var must be a new variable.
     The modified LLVM blocks that we use rely on re-declaration, because of the way
     our phi-elimination works.
     Therefore we keep track of declared variables in a block and use assignment (=)
     instead of declaration-and-assignment (:=) as needed.
  *)
  let declared_vars =
    (match nopt with
    | None ->
        if unused then
          bprintf b "\t" (* don't declare an unused variable *)
        else
          bprintf b "\t%s := " (State.fresh());
        declared_vars
    | Some v ->
        if unused then
          bprintf b "\t" (* don't declare an unused variable *)
        else if VSet.mem v declared_vars then
          bprintf b "\t%s = " (govar v)
        else
          bprintf b "\t%s := " (govar v);
        VSet.add v declared_vars) in
  (match i with
  | Call(_,_,_,_,Var(Name(true, "printf")),(_,_,Int x)::ops,_,_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants (Big_int.int_of_big_int x)) in
        if ops = [] then
          bprintf b "%sPrintf(vm, mask, \"%s\")\n" pkg s
        else
          bprintf b "%sPrintf(vm, mask, \"%s\", %a)\n" pkg s (between ", " bpr_go_value) (List.map (fun (a,b,c) -> (a,c)) ops)
      with _ ->
        failwith "Error: first argument of printf must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "puts")),[_,_,Int x],_,_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants  (Big_int.int_of_big_int x)) in
        bprintf b "%sPrintf(vm, mask, \"%s\\n\")\n" pkg s (* puts adds a newline *)
      with _ ->
        failwith "Error: argument of puts must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "putchar")),[typ,_,value],_,_) ->
               bprintf b "%sPrintf(vm, mask, \"%%c\", %a)\n" pkg bpr_go_value (typ, value)
  | Call(_,_,_,_,Var(Name(true, "input")),[typ,_,value],_,_) ->
      bprintf b "%sInput32(vm, mask, %a, next_arg)\n" pkg bpr_go_value (typ, value)
  | Call(_,_,_,_,Var(Name(true, "unary")),[(ty,_,op);(_,_,Int l)],_,_) ->
      bprintf b "%sUnary(vm, %a, %d)\n" pkg bpr_go_value (ty, op) (Big_int.int_of_big_int l)
  | Call(_,_,_,_,Var(Name(true, "selectbit")),[(ty,_,Var v);(_,_,Int l)],_,_) ->
      let bitnum = Big_int.int_of_big_int l in
      bprintf b "%s[%d:%d]\n" (govar v) bitnum (bitnum+1)
  | Call(_,_,_,_,Var(Name(true, "llvm.lifetime.start")),_,_,_) ->
      ()
  | Call(_,_,_,_,Var(Name(true, "llvm.lifetime.end")),_,_,_) ->
      ()
  | Load(_,_,(Pointer(ety,_),_ as x),_,_,_) -> (* in case we are debugging loads *)
      bprintf b "%sLoadDebug(vm, mask, %a, Uint(vm, %d, 32))\n" pkg bpr_go_value x (State.bytewidth ety)
  | Store(_,_,x,addr,_,_,_) -> (* in case we are debugging stores*)
      bprintf b "%sStoreDebug(vm, mask, %a, Uint(vm, %d, 32), %a)\n" pkg bpr_go_value addr (State.bytewidth (fst x)) bpr_go_value x
  | Bitcast(x,_,_) ->
      bprintf b "%a\n" bpr_go_value x
  | Sext(tv,t,_) ->
      bprintf b "%sSext(vm, %a, %d)\n" pkg bpr_go_value tv (State.bitwidth t)
  | Zext(tv,t,_) ->
      bprintf b "%sZext(vm, %a, %d)\n" pkg bpr_go_value tv (State.bitwidth t)
  | Mul(_,_,tv,Int y,_) ->
      (* It would be better to do this strength reduction in LLVM *)
      let shift_bits =
        match Big_int.int_of_big_int y with
        |   1 -> Some 0
        |   2 -> Some 1
        |   4 -> Some 2
        |   8 -> Some 3
        |  16 -> Some 4
        |  32 -> Some 5
        |  64 -> Some 6
        | 128 -> Some 7
        | 256 -> Some 8
        | _   -> None in
      (match shift_bits with
      | None ->
          bprintf b "%sMul(vm, %a, %a)\n" pkg bpr_go_value tv bpr_go_value (fst tv,Int y)
      | Some shift_bits ->
          bprintf b "%sShl(vm, %a, %d)\n" pkg bpr_go_value tv shift_bits)
  | Mul(_,_,(typ,x),y,_) ->
      bprintf b "%sMul(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Lshr(_,tv,Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "%sLshr(vm, %a, %d)\n" pkg bpr_go_value tv shift_bits
  | Ashr(_,tv,Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "%sAshr(vm, %a, %d)\n" pkg bpr_go_value tv shift_bits
  | Shl(_,_,tv,Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "%sShl(vm, %a, %d)\n" pkg bpr_go_value tv shift_bits
  | Add(_,_,(typ,x),y,_) ->
      bprintf b "%sAdd(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Sub(_,_,(typ,x),y,_) ->
      bprintf b "%sSub(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | And((typ,x),y,_) ->
      bprintf b "%sAnd(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Or((typ,x),y,_) ->
      bprintf b "%sOr(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Xor((typ,x),y,_) ->
      bprintf b "%sXor(vm, %a, %a)\n" pkg bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Icmp(pred,(typ,x),y,_) ->
      bprintf b "%sIcmp_%a(vm, %a, %a)\n" pkg
        bpr_icmp pred
        bpr_go_value (typ,x) bpr_go_value (typ,y)
(*  | AssignInst(result_ty, [(ty,op)]) (* special instruction inserted by our compiler *) *)
  | Inttoptr((ty,op), result_ty,_) ->
      let bits_result = State.bitwidth result_ty in
      let bits_op = State.bitwidth ty in
      if bits_result = bits_op then
        bprintf b "%a\n" bpr_go_value (ty,op)
      else if bits_result > bits_op then
        (* NB our oTypes do not have signed/unsigned versions so we zextend for now *)
        bprintf b "%sZext(vm, %a, %d)\n" pkg bpr_go_value (ty,op) bits_result
      else (* bits_result < bits_op *)
        bprintf b "%a[:%d]\n" bpr_go_value (ty,op) bits_result;
  | Select([x;y;z],_) -> (* TODO: maybe enforce 3 args in datatype? *)
      bprintf b "%sSelect(vm, %a, %a, %a)\n" pkg bpr_go_value x bpr_go_value y bpr_go_value z
  | Switch(op0,op1,ops,_) ->
      (* op0 is the value to switch on.
         op1 is the default target.
         ops is a list of pairs; the first element of each pair should be an
         int indicating the case.
         NB: We assume that the cases are 0, 1, ... in order!
       *)
      let cases = List.map snd ops in (* throw away the 0, 1, ... part of the pairs *)
      bprintf b "%sSwitch(vm, %a, %a, %a)\n" pkg bpr_go_value op0 bpr_go_value op1
        (between ", " bpr_go_value) cases
  | Trunc(x, ty,_) ->
      bprintf b "%a[:%d]\n" bpr_go_value x (State.bitwidth ty)
  | _ -> begin
      eprintf "Error: unsupported %s\n" (spr bpr_instr (nopt,i));
      bprintf b "Unsupported(\"UNSUPPORTED %a\")\n" bpr_instr (nopt,i)
  end);
  declared_vars

let bit_type is_gen =
  if is_gen then "gc.Wire" else "gc.Key"

let gen_or_eval is_gen =
  if is_gen then "gen" else "eval"

let bpr_go_block_args b bl =
  let fv = free_of_block bl in
  VSet.iter (fun var -> bprintf b ", %s" (govar var)) fv

let outputs_of_block blocks_fv bl =
  VSet.inter (assigned_of_block bl) (VSet.union State.V.special (VSet.union blocks_fv !State.bl_vars))

let outputs_of_blocks blocks =
  let blocks_fv =
    List.fold_left VSet.union VSet.empty
      (List.map free_of_block blocks) in
  List.fold_left VSet.union VSet.empty
    (List.map (outputs_of_block blocks_fv) blocks)

let assigned_of_blocks blocks =
  List.fold_left VSet.union VSet.empty
    (List.map assigned_of_block blocks)


let bpr_go_block b blocks_fv is_gen bl =
  let pkg = if is_gen then "gen." else "eval." in
  let bpr_go_value = (fun b -> bpr_go_value b is_gen) in
  let name =
    match bl.bname with
    | Id(_,n) -> string_of_int n
    | Name(_,n) -> n in
  bprintf b "\n// <label>:%s\n" name;
  bprintf b "func %s%d(vm %sVM, ch chan []%s, mask%a []%s) {\n"
    (gen_or_eval is_gen)
    (State.bl_num bl.bname)
    pkg
    (bit_type is_gen)
    bpr_go_block_args bl
    (bit_type is_gen);
  let outputs = outputs_of_block blocks_fv bl in
  if options.debug_blocks then
    bprintf b "\t%sPrintf(vm, mask, \"Block %d\\n\")\n" pkg (State.bl_num bl.bname);
  ignore(List.fold_left (bpr_go_instr b is_gen) (free_of_block bl) bl.binstrs);
  if not(VSet.is_empty outputs) then begin
    if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
      bprintf b "\tch <- mask\n";
    VSet.iter
      (fun var ->
        let value = Var var in
        bprintf b "\tch <- %sMask(vm, mask, %a)\n" pkg bpr_go_value (State.typ_of_var var, value))
      outputs
  end;
  bprintf b "}\n"

let bpr_main b f is_gen =
  let pkg = if is_gen then "gen." else "eval." in
  let blocks = f.fblocks in
  let blocks_fv = List.fold_left VSet.union VSet.empty
      (List.map free_of_block blocks) in
  bprintf b "func %s_main(vms []%sVM) {\n" (gen_or_eval is_gen) pkg;
  bprintf b "\n";
  if is_gen then
    bprintf b "\tinitialize_ram(vms[0])\n\n";
  bprintf b "\t/* create output channels */\n";
  List.iter
    (fun bl ->
      let capacity = VSet.cardinal(outputs_of_block blocks_fv bl) in
      bprintf b "\tch%d := make(chan []%s, %d)\n" (State.bl_num bl.bname) (bit_type is_gen) capacity
      )
    blocks;
  bprintf b "\n";
  bprintf b "\t/* special variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := %sUint(vms[0], 0, %d)\n" (govar var) pkg (State.bitwidth (State.typ_of_var var));
    )
    (VSet.inter State.V.special (assigned_of_blocks blocks));
  bprintf b "\n";
  bprintf b "\t/* block masks */\n";
  VSet.iter
    (fun var -> bprintf b "\t%s := %sUint(vms[0], 0, 1)\n" (govar var) pkg)
    !State.bl_vars;
  bprintf b "\t%s = %sUint(vms[0], 1, 1)\n" (govar (State.bl_mask 0)) pkg;
  bprintf b "\n";
  bprintf b "\t/* block free variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := %sUint(vms[0], 0, %d)\n" (govar var) pkg (State.bitwidth (State.typ_of_var var));
    )
    blocks_fv;
  bprintf b "\n";
  bprintf b "\tdone := false\n";
  bprintf b "\tfor !done {\n";
  bprintf b "\n";
  bprintf b "\t\t/* one goroutine invocation per block */\n";
  List.iter
    (fun bl ->
      bprintf b "\t\tgo %s%d(vms[%d], ch%d, %s%a)\n"
        (gen_or_eval is_gen)
        (State.bl_num bl.bname)
        (State.bl_num bl.bname + 1)
        (State.bl_num bl.bname)
        (govar (State.bl_mask (State.bl_num bl.bname)))
        bpr_go_block_args bl)
    blocks;
  bprintf b "\n";
  bprintf b "\t\t/* mux the outputs*/\n";
  List.iter
    (fun bl ->
      let outputs = outputs_of_block blocks_fv bl in
      if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
        bprintf b "\t\tmask_%d := <-ch%d\n" (State.bl_num bl.bname) (State.bl_num bl.bname);
      VSet.iter
        (fun var ->
          bprintf b "\t\t%s_%d := <-ch%d\n"
            (govar var)
            (State.bl_num bl.bname)
            (State.bl_num bl.bname))
        outputs)
    blocks;
  VSet.iter
    (fun var ->
      let sources = List.filter (fun bl -> VSet.mem var (outputs_of_block blocks_fv bl)) blocks in
      if VSet.mem var State.V.special then
        (* specials are assigned 0 unless the active block assigned them *)
        bprintf b "\t\t%s = %sTreeXor(vms[0], %s)\n"
          (govar var)
          pkg
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.bname)) sources))
      else
        (* non-specials keep their value from before the blocks unless the active block assigned them *)
        bprintf b "\t\t%s = %sSelect(vms[0], %sTreeXor(vms[0], %s), %sTreeXor(vms[0], %s), %s)\n"
          (govar var)
          pkg
          pkg
          (String.concat ", " (List.map (fun bl -> sprintf "mask_%d" (State.bl_num bl.bname)) sources))
          pkg
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.bname)) sources))
          (govar var))
    (outputs_of_blocks blocks);
  if VSet.mem State.V.vMemRes blocks_fv then begin
    (* We need to load from memory iff some block uses vMemRes *)
    bprintf b "\n";
    bprintf b "\t\t/* load from memory if necessary */\n";
    bprintf b "\t\tif %sReveal(vms[0], %sIcmp_eq(vms[0], _vMemAct, %sUint(vms[0], 1, 2)))[0] {\n" pkg pkg pkg;
    bprintf b "\t\t\t_vMemRes = %sLoad(vms[0], _vMemLoc, _vMemSize)\n" pkg;
    bprintf b "\t\t}\n";
  end;
  if VSet.mem State.V.vMemVal (outputs_of_blocks blocks) then begin
    (* We need to store to memory iff some block assigns vMemVal *)
    bprintf b "\n";
    bprintf b "\t\t/* store to memory if necessary */\n";
    bprintf b "\t\tif %sReveal(vms[0], %sIcmp_eq(vms[0], _vMemAct, %sUint(vms[0], 2, 2)))[0] {\n" pkg pkg pkg;
    bprintf b "\t\t\t%sStore(vms[0], _vMemLoc, _vMemSize, _vMemVal)\n" pkg;
    bprintf b "\t\t}\n";
  end;
  bprintf b "\n";
  bprintf b "\t\t/* are we done? */\n";
  bprintf b "\t\tdone = %sReveal(vms[0], _vIsDone)[0]\n" pkg;
  bprintf b "\t}\n";
  bprintf b "\tanswer := %sRevealInt32(vms[0], _vAnswer)\n" pkg;
  bprintf b "\tfmt.Printf(\"%s: %%v\\n\", answer)\n" (gen_or_eval is_gen);
  bprintf b "}\n"

let rec bytes_of_value b bytes = function
  | typ, Int x ->
      let bytes2 = State.bitwidth typ / 8 in (* TODO: do we care if not a multiple of 8? *)
      let ff = Int64.of_int 255 in
      let y = ref(Int64.of_int(Big_int.int_of_big_int x)) in
      for i = 1 to bytes2 do
        let lowbyte = Char.chr(Int64.to_int(Int64.logand !y ff)) in
        bprintf b "%c" lowbyte;
        y := Int64.shift_right_logical !y 8
      done;
      bytes - bytes2
  | typ, Zero
  | typ, Null ->
      bytes_of_value b bytes (typ, Int(Big_int.zero_big_int))
  | typ, Struct(is_packed,ops) ->
      (* TODO: handle packing and alignment issues *)
      List.fold_left
        (fun bytes op -> bytes_of_value b bytes op)
        bytes
        ops
  | typ, Array ops
  | typ, Vector ops ->
      List.fold_left
        (fun bytes op -> bytes_of_value b bytes op)
        bytes
        ops
  | _ ->
      failwith "bytes_of_value"

let bpr_globals b m is_gen =
  let pkg = if is_gen then "gen." else "eval." in (* NB is_gen is always true in bpr_globals for now *)
  Hashtbl.reset string_constants;
  let b1 = Buffer.create 11 in
  let pr_global = function
    | { gvalue=None } -> ()
    | { gname; gtyp; gvalue=Some v } ->
        let loc = Hashtbl.find State.global_locations gname in
        let bytes =
          try State.bytewidth gtyp with _ -> 100 in
        let bytevals = Buffer.create bytes in
        let written = bytes_of_value bytevals bytes (gtyp, v) in
        if written != 0 || bytes != Buffer.length bytevals then begin
          let gname = (spr bpr_var gname) in
          eprintf "WHOA! expected %d bytes, found %d bytes for global %s\n" bytes (Buffer.length bytevals) gname;
        end;
        bprintf b1 "\t// %a at location %d == 0x%x\n" bpr_var gname loc loc;
        for i = 0 to bytes - 1 do
          (* Only print non-zeros *)
          let x = Char.code(Buffer.nth bytevals i) in
          if x <> 0 then
            bprintf b1 "\tram[0x%x] = 0x%x\n" (loc + i) x
        done;
        (* Look for string constants, might be used by Printf *)
        (match gtyp with
        | Arraytyp(len, Integer 8) ->
            if (0 = Char.code (Buffer.nth bytevals (bytes - 1))) then
              let s = Buffer.sub bytevals 0 (len - 1) in
              Hashtbl.add string_constants loc s
        | _ -> ()) in
  List.iter pr_global m.cglobals;
  bprintf b "func initialize_ram(vm %sVM) {\n" pkg;
  if !State.loc <> 0 then begin
    bprintf b "\tram := make([]byte, 0x%x)\n" !State.loc;
    Buffer.add_buffer b b1;
    bprintf b "\t%sInitRam(ram)\n" pkg;
  end;
  bprintf b "}\n";
  bprintf b "\n"

let print_function_circuit m f =
  let b = Buffer.create 11 in
  (* prelude *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import \"%sgc/gen\"\n" package_prefix;
  bprintf b "import \"%sgc/eval\"\n" package_prefix;
  bprintf b "import \"%sgc\"\n" package_prefix;
  bprintf b "import \"%sgc/runtime\"\n" package_prefix;
  bprintf b "import \"fmt\"\n";
  bprintf b "\n";
  (* gen side *)
  bpr_globals b m true;
  bpr_main b f true;
  let blocks_fv = List.fold_left VSet.union VSet.empty (* TODO: eliminate duplicate code *)
      (List.map free_of_block f.fblocks) in
  List.iter (bpr_go_block b blocks_fv true) f.fblocks;
  bprintf b "\n";
  (* eval side *)
  bpr_main b f false;
  List.iter (bpr_go_block b blocks_fv false) f.fblocks;
  bprintf b "\n";
  (* main function *)
  bprintf b "func main() {\n";
  bprintf b "\truntime.Run(%d, gen_main, eval_main)\n" (List.length f.fblocks);
  bprintf b "}\n";
  pr_output_file ".go" (Buffer.contents b)
