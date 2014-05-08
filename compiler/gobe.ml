(* go back end *)

open Util
open Printf
open Options

let package_prefix = "github.com/tjim/smpcc/runtime/"

let global_locations = Hashtbl.create 10
let string_constants = Hashtbl.create 10

let govar v =
  Str.global_replace (Str.regexp "[%@.]") "_" (State.v_map v)

let rec bpr_go_value b (typ, value) =
  match value with
  | Var v ->
      let is_a_global = (match v with Name(x,_) -> x | Id(x,_) -> x) in
      if is_a_global then
        (try
          let loc = Hashtbl.find global_locations v in
          bprintf b "Uint(io, %d, 64)" loc
        with Not_found ->
          eprintf "global not there %s\n" (govar v);
          bprintf b "%s" (govar v))
      else
        bprintf b "%s" (govar v)
  | Basicblock bl ->
      bprintf b "Uint(io, %d, %d)" (State.bl_num bl) (State.get_bl_bits())
  | Int x ->
      bprintf b "Int(io, %s, %d)" (Big_int.string_of_big_int x) (State.bitwidth typ)
  | Zero ->
      bprintf b "Uint(io, 0, %d) /* CAUTION: zero */" (State.bitwidth typ)
  | Null ->
      bprintf b "Uint(io, 0, %d) /* CAUTION: null */" (State.bitwidth typ)
  | Undef ->
      bprintf b "Uint(io, 0, %d) /* CAUTION: undef */" (State.bitwidth typ)
  | Inttoptr(x, y) -> 
      bprintf b "/* CAUTION: inttoptr */ ";
      bpr_go_value b x
  | op ->
      bpr_value b op

let bpr_go_instr b is_gen declared_vars (nopt,i) =
  (* Look for a special case: clang declares a variable for the result of a printf that is not subsequently used.
     Go has a stricter check on this.
   *)
  let unused =
    (match i with
    | Call(_,_,_,_,Var(Name(true, ("printf" | "puts" | "putchar"))),_,_) -> true
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
  | Call(_,_,_,_,Var(Name(true, "printf")),(_,_,Int x)::ops,_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants (Big_int.int_of_big_int x)) in
        if ops = [] then
          bprintf b "Printf(io, mask, \"%s\")\n" s
        else
          bprintf b "Printf(io, mask, \"%s\", %a)\n" s (between ", " bpr_go_value) (List.map (fun (a,b,c) -> (a,c)) ops)
      with _ ->
        failwith "Error: first argument of printf must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "puts")),[_,_,Int x],_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants  (Big_int.int_of_big_int x)) in
        bprintf b "Printf(io, mask, \"%s\\n\")\n" s (* puts adds a newline *)
      with _ ->
        failwith "Error: argument of puts must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "putchar")),[typ,_,value],_) ->
               bprintf b "Printf(io, mask, \"%%c\", %a)\n" bpr_go_value (typ, value)
  | Call(_,_,_,_,Var(Name(true, "gen_int")),_,_) ->
      if is_gen then
        bprintf b "Gen_int(io, mask, next_arg)\n"
      else
        bprintf b "Gen_int(io, mask)\n"
  | Call(_,_,_,_,Var(Name(true, "eval_int")),_,_) ->
      if is_gen then
        bprintf b "Eval_int(io, mask)\n"
      else
        bprintf b "Eval_int(io, mask, next_arg)\n"
  | Load(_,_,(Pointer(ety,_),_ as x),_,_) -> (* in case we are debugging loads *)
      bprintf b "LoadDebug(io, mask, %a, Uint(io, %d, 32))\n" bpr_go_value x (State.bytewidth ety)
  | Store(_,_,x,addr,_,_) -> (* in case we are debugging stores*)
      bprintf b "StoreDebug(io, mask, %a, Uint(io, %d, 32), %a)\n" bpr_go_value addr (State.bytewidth (fst x)) bpr_go_value x
  | Bitcast(x,_) ->
      bprintf b "%a\n" bpr_go_value x
  | Sext(tv,t) ->
      bprintf b "Sext(io, %a, %d)\n" bpr_go_value tv (State.bitwidth t)
  | Zext(tv,t) ->
      bprintf b "Zext(io, %a, %d)\n" bpr_go_value tv (State.bitwidth t)
  | Mul(_,_,tv,Int y) ->
      (* It would be better to do this strength reduction in LLVM *)
      let shift_bits =
        match Big_int.int_of_big_int y with
        |   1 -> 0
        |   2 -> 1
        |   4 -> 2
        |   8 -> 3
        |  16 -> 4
        |  32 -> 5
        |  64 -> 6
        | 128 -> 7
        | 256 -> 8
        | _   -> failwith "the go back end does not support Mul" in
      bprintf b "Shl(io, %a, %d)\n" bpr_go_value tv shift_bits
  | Lshr(_,tv,Int y) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Lshr(io, %a, %d)\n" bpr_go_value tv shift_bits
  | Ashr(_,tv,Int y) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Ashr(io, %a, %d)\n" bpr_go_value tv shift_bits
  | Shl(_,_,tv,Int y) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Shl(io, %a, %d)\n" bpr_go_value tv shift_bits
  | Add(_,_,(typ,x),y) ->
      bprintf b "Add(io, %a, %a)\n" bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Sub(_,_,(typ,x),y) ->
      bprintf b "Sub(io, %a, %a)\n" bpr_go_value (typ,x) bpr_go_value (typ,y)
  | And((typ,x),y) ->
      bprintf b "And(io, %a, %a)\n" bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Or((typ,x),y) ->
      bprintf b "Or(io, %a, %a)\n" bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Xor((typ,x),y) ->
      bprintf b "Xor(io, %a, %a)\n" bpr_go_value (typ,x) bpr_go_value (typ,y)
  | Icmp(pred,(typ,x),y) ->
      bprintf b "Icmp_%a(io, %a, %a)\n"
        bpr_icmp pred
        bpr_go_value (typ,x) bpr_go_value (typ,y)
(*  | AssignInst(result_ty, [(ty,op)]) (* special instruction inserted by our compiler *) *)
  | Inttoptr((ty,op), result_ty) ->
      let bits_result = State.bitwidth result_ty in
      let bits_op = State.bitwidth ty in
      if bits_result = bits_op then
        bprintf b "%a\n" bpr_go_value (ty,op)
      else if bits_result > bits_op then
        (* NB our oTypes do not have signed/unsigned versions so we zextend for now *)
        bprintf b "Zext(io, %a, %d)\n" bpr_go_value (ty,op) bits_result
      else (* bits_result < bits_op *)
        bprintf b "%a[:%d]\n" bpr_go_value (ty,op) bits_result;
  | Select([x;y;z]) -> (* TODO: maybe enforce 3 args in datatype? *)
      bprintf b "Select(io, %a, %a, %a)\n" bpr_go_value x bpr_go_value y bpr_go_value z
  | Switch(op0,op1,ops) ->
      (* op0 is the value to switch on.
         op1 is the default target.
         ops is a list of pairs; the first element of each pair should be an
         int indicating the case.
         NB: We assume that the cases are 0, 1, ... in order!
       *)
      let cases = List.map snd ops in (* throw away the 0, 1, ... part of the pairs *)
      bprintf b "Switch(io, %a, %a, %a)\n" bpr_go_value op0 bpr_go_value op1
        (between ", " bpr_go_value) cases
  | Trunc(x, ty) ->
      bprintf b "%a[:%d]\n" bpr_go_value x (State.bitwidth ty)
  | _ -> begin
      eprintf "Error: unsupported %s\n" (let b = Buffer.create 11 in bpr_instr b (nopt,i); Buffer.contents b);
      bprintf b "Unsupported(\"UNSUPPORTED %a\")\n" bpr_instr (nopt,i)
  end);
  declared_vars

let io_type is_gen =
  if is_gen then "GenVM" else "EvalVM"

let bit_type is_gen =
  if is_gen then "base.Wire" else "base.Key"

let gen_or_eval is_gen =
  if is_gen then "gen" else "eval"

let bpr_go_block_args b bl =
  let fv = free_of_block bl in
  VSet.iter (fun var -> bprintf b ", %s" (govar var)) fv

let outputs_of_block blocks_fv bl =
  VSet.inter (assigned_of_block bl) (VSet.union State.V.special blocks_fv)

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
  let name =
    match bl.bname with
    | Id(_,n) -> string_of_int n
    | Name(_,n) -> n in
  bprintf b "// <label>:%s\n" name;
  bprintf b "func %s%d(io %s, ch chan []%s, block_num%a []%s) {\n"
    (gen_or_eval is_gen)
    (State.bl_num bl.bname)
    (io_type is_gen)
    (bit_type is_gen)
    bpr_go_block_args bl
    (bit_type is_gen);
  let outputs = outputs_of_block blocks_fv bl in
  let block_requires_mask =
    options.debug_blocks
  || not(VSet.is_empty outputs)
  || (List.fold_left
        (fun a (_,i) ->
          a ||
          (match i with (* see bpr_go_instr, these are all only only cases using mask *)
          | Call(_,_,_,_,Var(Name(true, ("printf" | "puts" | "putchar" | "gen_int" | "eval_int"))),_,_)
          | Load _
          | Store _ ->
              true
          | _ ->
              false))
        false
        bl.binstrs) in
  if block_requires_mask then
    bprintf b "\tmask := Icmp_eq(io, block_num, Uint(io, %d, 32))\n" (State.bl_num bl.bname);
  if options.debug_blocks then
    bprintf b "\tPrintf(io, mask, \"Block %d\\n\")\n" (State.bl_num bl.bname);
  ignore(List.fold_left (bpr_go_instr b is_gen) (free_of_block bl) bl.binstrs);
  if not(VSet.is_empty outputs) then begin
    if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
      bprintf b "\tch <- mask\n";
    VSet.iter
      (fun var ->
        let value = Var var in
        bprintf b "\tch <- Mask(io, mask, %a)\n" bpr_go_value (State.typ_of_var var, value))
      outputs
  end;
  bprintf b "}\n\n"

let bpr_main b f is_gen =
  let blocks = f.fblocks in
  let blocks_fv = List.fold_left VSet.union VSet.empty
      (List.map free_of_block blocks) in
  let blockios = String.concat "" (List.map (fun bl -> sprintf ", io%d" (State.bl_num bl.bname)) blocks) in
  bprintf b "func %s_main(io%s %s) {\n" (gen_or_eval is_gen) blockios (io_type is_gen);
  bprintf b "\n";
  if is_gen then
    bprintf b "\tinitialize_ram(io)\n\n";
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
      bprintf b "\t%s := Uint(io, 0, %d)\n" (govar var) (State.bitwidth (State.typ_of_var var));
    )
    (VSet.add (State.V.attsrcStateO()) (* if there is only one block this is used but not assigned *)
       (VSet.inter State.V.special (assigned_of_blocks blocks)));
  bprintf b "\n";
  bprintf b "\t/* block free variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := Uint(io, 0, %d)\n" (govar var) (State.bitwidth (State.typ_of_var var));
    )
    blocks_fv;
  bprintf b "\n";
  bprintf b "\tdone := false\n";
  bprintf b "\tfor !done {\n";
  bprintf b "\n";
  bprintf b "\t\t/* one goroutine invocation per block */\n";
  List.iter
    (fun bl ->
      bprintf b "\t\tgo %s%d(io%d, ch%d, _attsrcStateO%a)\n"
        (gen_or_eval is_gen)
        (State.bl_num bl.bname)
        (State.bl_num bl.bname)
        (State.bl_num bl.bname)
        bpr_go_block_args bl)
    blocks;
  bprintf b "\n";
  bprintf b "\t\t/* mux the outputs*/\n";
  List.iter
    (fun bl ->
      let outputs = outputs_of_block blocks_fv bl in
      if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
        bprintf b "\t\tmask_%d := <- ch%d\n" (State.bl_num bl.bname) (State.bl_num bl.bname);
      VSet.iter
        (fun var ->
          bprintf b "\t\t%s_%d := <- ch%d\n"
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
        bprintf b "\t\t%s = TreeXor(io, %s)\n"
          (govar var)
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.bname)) sources))
      else
        (* non-specials keep their value from before the blocks unless the active block assigned them *)
        bprintf b "\t\t%s = Select(io, TreeXor(io, %s), TreeXor(io, %s), %s)\n"
          (govar var)
          (String.concat ", " (List.map (fun bl -> sprintf "mask_%d" (State.bl_num bl.bname)) sources))
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.bname)) sources))
          (govar var))
    (outputs_of_blocks blocks);
  if VSet.mem State.V.attsrcMemRes blocks_fv then begin
    (* We need to load from memory iff some block uses attsrcMemRes *)
    bprintf b "\n";
    bprintf b "\t\t/* load from memory if necessary */\n";
    bprintf b "\t\tif Reveal(io, Icmp_eq(io, _attsrcMemAct, Uint(io, 1, 2)))[0] {\n";
    bprintf b "\t\t\t_attsrcMemRes = Load(io, _attsrcMemLoc, _attsrcMemSize)\n";
    bprintf b "\t\t}\n";
  end;
  if VSet.mem State.V.attsrcMemVal (outputs_of_blocks blocks) then begin
    (* We need to store to memory iff some block assigns attsrcMemVal *)
    bprintf b "\n";
    bprintf b "\t\t/* store to memory if necessary */\n";
    bprintf b "\t\tif Reveal(io, Icmp_eq(io, _attsrcMemAct, Uint(io, 2, 2)))[0] {\n";
    bprintf b "\t\t\tStore(io, _attsrcMemLoc, _attsrcMemSize, _attsrcMemVal)\n";
    bprintf b "\t\t}\n";
  end;
  bprintf b "\n";
  bprintf b "\t\t/* are we done? */\n";
  bprintf b "\t\tdone = Reveal(io, _attsrcIsDone)[0]\n";
  bprintf b "\t}\n";
  bprintf b "\tanswer := RevealInt32(io, _attsrcAnswer)\n";
  bprintf b "\tfmt.Printf(\"%s: %%v\\n\", answer)\n" (gen_or_eval is_gen);
  bprintf b "\t%s_done <- true\n" (govar f.fname);
  bprintf b "}\n";
  bprintf b "\n"

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

let bpr_globals b m =
  let loc = ref 0 in (* smallest memory location *)
  Hashtbl.reset global_locations;
  Hashtbl.reset string_constants;
  let align n =
    let md = !loc mod n in
    if md = 0 then () else
    loc := !loc + (n - md) in
  let b1 = Buffer.create 11 in
  let alloc_global = function
    | { gvalue=None } -> ()
    | { gname; gtyp; gvalue=Some v } ->
    Hashtbl.add global_locations gname !loc;
    let bytes =
      try State.bytewidth gtyp with _ -> 100 in
    let bytevals = Buffer.create bytes in
    let written = bytes_of_value bytevals bytes (gtyp, v) in
    if written != 0 || bytes != Buffer.length bytevals then begin
      let gname = (let b2 = Buffer.create 11 in bpr_var b2 gname; Buffer.contents b2) in
      eprintf "WHOA! expected %d bytes, found %d bytes for global %s\n" bytes (Buffer.length bytevals) gname;
    end;
    bprintf b1 "\t// %a at location %d == 0x%x\n" bpr_var gname !loc !loc;
    for i = 0 to bytes - 1 do
      (* Only print non-zeros *)
      let x = Char.code(Buffer.nth bytevals i) in
      if x <> 0 then
        bprintf b1 "\tram[0x%x] = 0x%x\n" (!loc + i) x
    done;
    (* Look for string constants, might be used by Printf *)
    (match gtyp with
    | Arraytyp(len, Integer 8) ->
        if (0 = Char.code (Buffer.nth bytevals (bytes - 1))) then
          let s = Buffer.sub bytevals 0 (len - 1) in
          Hashtbl.add string_constants !loc s
    | _ -> ());
    loc := !loc + bytes;
    align 4 in
  List.iter alloc_global m.cglobals;
  bprintf b "func initialize_ram(io GenVM) {\n";
  if !loc <> 0 then begin
    bprintf b "\tram := make([]byte, 0x%x)\n" !loc;
    Buffer.add_buffer b b1;
    bprintf b "\tInitRam(ram)\n";
  end;
  bprintf b "}\n";
  bprintf b "\n"

let gep_elim_value =
  value_map (function
    | Getelementptr(_, (Pointer(ety,s),Var v)::tl) as c ->
        if not(Hashtbl.mem global_locations v) then
          (let b = Buffer.create 11 in
          bpr_value b c;
          eprintf "Warning: unable to eliminate '%s'\n" (Buffer.contents b);
          c) else
        let ety = Arraytyp(0,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
        let rec loop ety = function
          | [] -> Big_int.big_int_of_int(Hashtbl.find global_locations v)
          | (y_typ,y)::tl ->
              (match ety,y with
              | Arraytyp(_,ety'),(Int(i)) -> (* NB we ignore the bitwidth of the constant *)
                  Big_int.add_big_int
                    (Big_int.mult_big_int i (Big_int.big_int_of_int(State.bytewidth ety')))
                    (loop ety' tl)
              | Structtyp(_,typs),(Int(i)) ->
                  let width, ety' =
                    let rec f i = function
                      | [] ->
                          failwith "gep_elim_value: struct does not have enough fields"
                      | hd::tl ->
                          if i=0 then
                            (State.bytewidth hd, hd)
                          else
                            let width', ety' = f (i-1) tl in
                            (State.bytewidth hd + width', ety') in
                    f (Big_int.int_of_big_int i) typs in
                  Big_int.add_big_int
                    (Big_int.big_int_of_int width)
                    (loop ety' tl)
              | Vartyp v, _ ->
                  loop (State.typ_of_var v) ((y_typ,y)::tl)
              | _ ->
                  let b = Buffer.create 11 in
                  bprintf b "gep_elim_value error: type is %a, value is %a\n" bpr_typ ety bpr_value y;
                  eprintf "%s" (Buffer.contents b);
                  failwith "gep_elim_value") in
        let sum = loop ety tl in
        Int sum
    | c -> c)

let print_function_circuit m f =
  let b = Buffer.create 11 in
  (* gen side *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import . \"%s%s\"\n" package_prefix (gen_or_eval true);
  bprintf b "import \"%sbase\"\n" package_prefix;
  bprintf b "import \"fmt\"\n";
  bprintf b "\n";
  bpr_globals b m;
  gep_elim_value f;
  bpr_main b f true;
  let blocks_fv = List.fold_left VSet.union VSet.empty (* TODO: eliminate duplicate code *)
      (List.map free_of_block f.fblocks) in
  List.iter (bpr_go_block b blocks_fv true) f.fblocks;
  let gen_go = Buffer.contents b in
  let b = Buffer.create 11 in
  (* eval side *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import . \"%s%s\"\n" package_prefix (gen_or_eval false);
  bprintf b "import \"%sbase\"\n" package_prefix;
  bprintf b "import \"fmt\"\n";
  bprintf b "\n";
  bpr_main b f false;
  List.iter (bpr_go_block b blocks_fv false) f.fblocks;
  let eval_go = Buffer.contents b in
  let b = Buffer.create 11 in
  (* main function *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import \"%s%s/eval\"\n" package_prefix (match options.circuitlib with None -> "yao" | Some x -> x);
  bprintf b "import \"fmt\"\n";
  bprintf b "import \"os\"\n";
  bprintf b "import \"runtime/pprof\"\n";
  bprintf b "\n";
  bprintf b "var args []string\n";
  bprintf b "func init_args() {\n";
  bprintf b "	args = os.Args[1:]\n";
  bprintf b "	if len(args) > 0 && args[0] == \"-pprof\" {\n";
  bprintf b "		args = os.Args[1:]\n";
  bprintf b "		file := \"cpu.pprof\"\n";
  bprintf b "		f, err := os.Create(file)\n";
  bprintf b "		if err != nil {\n";
  bprintf b "			fmt.Println(\"Error: \", err)\n";
  bprintf b "		}\n";
  bprintf b "		pprof.StartCPUProfile(f)\n";
  bprintf b "		defer pprof.StopCPUProfile()\n";
  bprintf b "	}\n";
  bprintf b "}\n";
  bprintf b "func next_arg() uint64 {\n";
  bprintf b "\tif len(args) <= 0 {\n";
  bprintf b "\t\tpanic(\"Not enough command-line arguments\")\n";
  bprintf b "\t}\n";
  bprintf b "\targ := 0\n";
  bprintf b "\tfmt.Sscanf(args[0], \"%%d\", &arg)\n";
  bprintf b "\targs = args[1:]\n";
  bprintf b "\treturn uint64(arg)\n";
  bprintf b "}\n";
  bprintf b "\n";
  bprintf b "var %s_done = make(chan bool, 1)\n" (govar f.fname);
  bprintf b "\n";
  bprintf b "func main() {\n";
  bprintf b "\tinit_args()\n";
  let blocknums = List.map (fun bl -> State.bl_num bl.bname) f.fblocks in
  bprintf b "\tgio, eio := eval.IO(-1)\n";
  List.iter
    (fun n ->
      bprintf b "\tgio%d, eio%d := eval.IO(%d)\n" n n n)
    blocknums;
  bprintf b "\tgo gen_main(gio, %s)\n" (String.concat ", " (List.map (fun n -> sprintf "gio%d" n) blocknums));
  bprintf b "\tgo eval_main(eio, %s)\n" (String.concat ", " (List.map (fun n -> sprintf "eio%d" n) blocknums));
  bprintf b "\t<-%s_done\n" (govar f.fname);
  bprintf b "\t<-%s_done\n" (govar f.fname);
  bprintf b "\n";
  bprintf b "\tfmt.Println(\"Done\")\n";
  bprintf b "}\n";
  let main_go = Buffer.contents b in
  pr_output_file "_gen.go" gen_go;
  pr_output_file "_eval.go" eval_go;
  pr_output_file "_main.go" main_go
