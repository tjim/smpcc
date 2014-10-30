(* GMW back end *)

open Util
open Printf
open Options

let package_prefix = "github.com/tjim/smpcc/runtime/"

let string_constants = Hashtbl.create 10

let roundup_bitwidth typ =
  let w = State.bitwidth typ in
  if w = 1 || w mod 8 = 0 then w else begin
    let w' = w + (8 - w mod 8) in
    w'
  end

let rec bpr_gmw_value b (typ, value) =
  match value with
  | Var v ->
      let is_a_global = (match v with Name(x,_) -> x | Id(x,_) -> x) in
      if is_a_global then
        (try
          let loc = Hashtbl.find State.global_locations v in
          bprintf b "Uint64(io, %d)" loc
        with Not_found ->
          eprintf "global not there %s\n" (Gc.govar v);
          bprintf b "%s" (Gc.govar v))
      else
        bprintf b "%s" (Gc.govar v)
  | Basicblock bl ->
      bprintf b "Uint%d(io, %d)" (State.get_bl_bits()) (State.bl_num bl)
  | Int x ->
      if Big_int.sign_big_int x < 0 then
        (* We can't use a negative constant as an unsigned int in go, e.g., uint32(-1).  Use (1<<32)-1 instead. *)
        bprintf b "Uint%d(io, (1<<%d)%s)" (roundup_bitwidth typ) (roundup_bitwidth typ) (Big_int.string_of_big_int x)
      else
        bprintf b "Uint%d(io, %s)" (roundup_bitwidth typ) (Big_int.string_of_big_int x)
  | Zero ->
      bprintf b "Uint%d(io, 0) /* CAUTION: zero */" (roundup_bitwidth typ)
  | Null ->
      bprintf b "Uint%d(io, 0) /* CAUTION: null */" (roundup_bitwidth typ)
  | Undef ->
      bprintf b "Uint%d(io, 0) /* CAUTION: undef */" (roundup_bitwidth typ)
  | Inttoptr(x, y) ->
      bprintf b "/* CAUTION: inttoptr */ ";
      bpr_gmw_value b x
  | op ->
      bpr_value b op

let bpr_gmw_instr b declared_vars (nopt,i) =
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
          bprintf b "\t%s = " (Gc.govar v)
        else
          bprintf b "\t%s := " (Gc.govar v);
        VSet.add v declared_vars) in
  (match i with
  | Call(_,_,_,_,Var(Name(true, "printf")),(_,_,Int x)::ops,_,_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants (Big_int.int_of_big_int x)) in
        if ops = [] then
          bprintf b "Printf(io, mask, \"%s\")\n" s
        else
          bprintf b "Printf(io, mask, \"%s\", %a)\n"
            s
            (between ", " (fun b -> bprintf b "uint64(%a)" bpr_gmw_value))
            (List.map (fun (a,b,c) -> (a,c)) ops)
      with _ ->
        failwith "Error: first argument of printf must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "puts")),[_,_,Int x],_,_) ->
      (try
        let s = String.escaped(Hashtbl.find string_constants (Big_int.int_of_big_int x)) in
        bprintf b "Printf(io, mask, \"%s\\n\")\n" s (* puts adds a newline *)
      with _ ->
        failwith "Error: argument of puts must be a string constant")
  | Call(_,_,_,_,Var(Name(true, "putchar")),[typ,_,Int x],_,_) ->
      bprintf b "Printf(io, mask, \"\\x%02x\")\n" (Big_int.int_of_big_int x)
  | Call(_,_,_,_,Var(Name(true, "input")),[typ,_,value],_,_) ->
      bprintf b "Input32(io, mask, %a, next_arg)\n" bpr_gmw_value (typ, value)
  | Call(_,_,_,_,Var(Name(true, "num_peers")),_,_,_) ->
      bprintf b "NumPeers32(io)\n"
  | Call(_,_,_,_,Var(Name(true, "llvm.lifetime.start")),_,_,_) ->
      ()
  | Call(_,_,_,_,Var(Name(true, "llvm.lifetime.end")),_,_,_) ->
      ()
  | Load(_,_,(Pointer(ety,_),_ as x),_,_,_) -> (* in case we are debugging loads *)
      bprintf b "LoadDebug(io, mask, %a, Uint32(io, %d))\n" bpr_gmw_value x (State.bytewidth ety)
  | Store(_,_,x,addr,_,_,_) -> (* in case we are debugging stores*)
      bprintf b "StoreDebug(io, mask, %a, Uint32(io, %d), %a)\n" bpr_gmw_value addr (State.bytewidth (fst x)) bpr_gmw_value x
  | Bitcast(x,_,_) ->
      bprintf b "%a\n" bpr_gmw_value x
  | Sext(tv,t,_) ->
      bprintf b "Sext(io, %a, %d)\n" bpr_gmw_value tv (roundup_bitwidth t)
  | Zext((typ,x),t,_) ->
      bprintf b "Zext%d_%d(io, %a)\n" (roundup_bitwidth typ) (roundup_bitwidth t) bpr_gmw_value (typ,x)
  | Mul(_,_,(typ,x),Int y,_) ->
      let y' = Big_int.int_of_big_int y in
      (match y' with
      |   1 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 0
      |   2 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 1
      |   4 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 2
      |   8 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 3
      |  16 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 4
      |  32 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 5
      |  64 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 6
      | 128 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 7
      | 256 -> bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) 8
      | _   -> bprintf b "Mul%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,Int y))
  | Mul(_,_,(typ,x),y,_) ->
      bprintf b "Mul%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | Lshr(_,(typ,x),Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Lshr%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) shift_bits
  | Ashr(_,(typ,x),Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Ashr%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) shift_bits
  | Shl(_,_,(typ,x),Int y,_) ->
      let shift_bits = Big_int.int_of_big_int y in
      bprintf b "Shl%d(io, %a, %d)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) shift_bits
  | Add(_,_,(typ,x),y,_) ->
      bprintf b "Add%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | Sub(_,_,(typ,x),y,_) ->
      bprintf b "Sub%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | And((typ,x),y,_) ->
      bprintf b "And%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | Or((typ,x),y,_) ->
      bprintf b "Or%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | Xor((typ,x),y,_) ->
      bprintf b "Xor%d(io, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
  | Icmp(pred,(typ,x),y,_) ->
      bprintf b "Icmp_%a%d(io, %a, %a)\n"
        bpr_icmp pred
        (roundup_bitwidth typ)
        bpr_gmw_value (typ,x) bpr_gmw_value (typ,y)
(*  | AssignInst(result_ty, [(ty,op)]) (* special instruction inserted by our compiler *) *)
  | Inttoptr((ty,op), result_ty,_) ->
      let bits_result = roundup_bitwidth result_ty in
      let bits_op = roundup_bitwidth ty in
      if bits_result = bits_op then
        bprintf b "%a\n" bpr_gmw_value (ty,op)
      else if bits_result > bits_op then
        (* NB our oTypes do not have signed/unsigned versions so we zextend for now *)
        bprintf b "Zext%d_%d(io, %a)\n" bits_op bits_result bpr_gmw_value (ty,op)
      else (* bits_result < bits_op *)
        bprintf b "uint%d(%a)\n" bits_result bpr_gmw_value (ty,op);
  | Select([x;(typ,y);z],_) -> (* TODO: maybe enforce 3 args in datatype? *)
      bprintf b "Select%d(io, %a, %a, %a)\n" (roundup_bitwidth typ) bpr_gmw_value x bpr_gmw_value (typ,y) bpr_gmw_value z
  | Switch(op0,op1,ops,_) ->
      (* op0 is the value to switch on.
         op1 is the default target.
         ops is a list of pairs; the first element of each pair should be an
         int indicating the case.
         NB: We assume that the cases are 0, 1, ... in order!
       *)
      let cases = List.map snd ops in (* throw away the 0, 1, ... part of the pairs *)
      bprintf b "Switch%d(io, %a, %a, %a)\n" (roundup_bitwidth (fst op0)) bpr_gmw_value op0 bpr_gmw_value op1
        (between ", " bpr_gmw_value) cases
  | Trunc(x, ty,_) ->
      let bits_result = roundup_bitwidth ty in
      (match bits_result with
      | 1 ->
          bprintf b "(%a > 0)\n" bpr_gmw_value x
      | 8
      | 32
      | 64 ->
          bprintf b "uint%d(%a)\n" bits_result bpr_gmw_value x
      | _ -> failwith(Printf.sprintf "Cannot truncate to %d" bits_result))
  | _ -> begin
      eprintf "Error: unsupported %s\n" (spr bpr_instr (nopt,i));
      bprintf b "Unsupported(\"UNSUPPORTED %a\")\n" bpr_instr (nopt,i)
  end);
  declared_vars

let bpr_sharetyp b typ =
  let w = roundup_bitwidth typ in
  if w = 1 then
    bprintf b "bool"
  else
    bprintf b "uint%d" w

let bpr_gmw_block_args print_types b bl =
  let fv = free_of_block bl in
  if print_types then
    VSet.iter (fun var -> bprintf b ", %s %a" (Gc.govar var) bpr_sharetyp (State.typ_of_var var)) fv
  else
    VSet.iter (fun var -> bprintf b ", %s" (Gc.govar var)) fv

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

let bpr_gmw_block b blocks_fv bl =
  let name =
    match bl.bname with
    | Id(_,n) -> string_of_int n
    | Name(_,n) -> n in
  bprintf b "// <label>:%s\n" name;
  bprintf b "func block%d(io Io, ch chan uint64, block_num uint32%a) {\n"
    (State.bl_num bl.bname)
    (bpr_gmw_block_args true) bl;
  let outputs = outputs_of_block blocks_fv bl in
  let block_requires_mask =
    options.debug_blocks
  || not(VSet.is_empty outputs)
  || (List.fold_left
        (fun a (_,i) ->
          a ||
          (match i with (* see bpr_gmw_instr, these are all cases using mask *)
          | Call(_,_,_,_,Var(Name(true, ("printf" | "puts" | "putchar" | "input"))),_,_,_)
          | Load _
          | Store _ ->
              true
          | _ ->
              false))
        false
        bl.binstrs) in
  if block_requires_mask then
    bprintf b "\tmask := Icmp_eq32(io, block_num, Uint32(io, %d))\n" (State.bl_num bl.bname);
  if options.debug_blocks then
    bprintf b "\tPrintf(io, mask, \"Block %d\\n\")\n" (State.bl_num bl.bname);
  ignore(List.fold_left (bpr_gmw_instr b) (free_of_block bl) bl.binstrs);
  if not(VSet.is_empty outputs) then begin
    if not(VSet.is_empty (VSet.diff outputs State.V.special)) then begin
      bprintf b "\tif mask {\n";
      bprintf b "\t\tch <- 1\n";
      bprintf b "\t} else {\n";
      bprintf b "\t\tch <- 0\n";
      bprintf b "\t}\n"
    end;
    VSet.iter
      (fun var ->
        let value = Var var in
        let typ = State.typ_of_var var in
        let width = roundup_bitwidth (State.typ_of_var var) in
        if width = 1 then begin
          bprintf b "\tif Mask%d(io, mask, %a) {\n" width bpr_gmw_value (typ, value);
          bprintf b "\t\tch <- 1\n";
          bprintf b "\t} else {\n";
          bprintf b "\t\tch <- 0\n";
          bprintf b "\t}\n"
        end else
          bprintf b "\tch <- uint64(Mask%d(io, mask, %a))\n" width bpr_gmw_value (typ, value))
      outputs
  end;
  bprintf b "}\n\n"

let bpr_main b f =
  let blocks = f.fblocks in
  let blocks_fv = List.fold_left VSet.union VSet.empty
      (List.map free_of_block blocks) in
  bprintf b "func blocks_main(io Io, ios []Io) {\n";
  bprintf b "\n";
  bprintf b "\tinitialize_ram(io)\n\n";
  bprintf b "\t/* create output channels */\n";
  List.iter
    (fun bl ->
      let capacity = VSet.cardinal(outputs_of_block blocks_fv bl) in
      bprintf b "\tch%d := make(chan uint64, %d)\n" (State.bl_num bl.bname) capacity
      )
    blocks;
  bprintf b "\n";
  bprintf b "\t/* special variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := Uint%d(io, 0)\n" (Gc.govar var) (roundup_bitwidth (State.typ_of_var var));
    )
    (VSet.add (State.V.attsrcStateO()) (* if there is only one block this is used but not assigned *)
       (VSet.inter State.V.special (assigned_of_blocks blocks)));
  bprintf b "\n";
  bprintf b "\t/* block free variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := Uint%d(io, 0)\n" (Gc.govar var) (roundup_bitwidth (State.typ_of_var var));
    )
    blocks_fv;
  bprintf b "\n";
  bprintf b "\tdone := false\n";
  bprintf b "\tfor !done {\n";
  bprintf b "\n";
  bprintf b "\t\t/* one goroutine invocation per block */\n";
  List.iter
    (fun bl ->
      bprintf b "\t\tgo block%d(ios[%d], ch%d, _attsrcStateO%a)\n"
        (State.bl_num bl.bname)
        (State.bl_num bl.bname)
        (State.bl_num bl.bname)
        (bpr_gmw_block_args false) bl)
    blocks;
  bprintf b "\n";
  bprintf b "\t\t/* mux the outputs*/\n";
  List.iter
    (fun bl ->
      let outputs = outputs_of_block blocks_fv bl in
      if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
        bprintf b "\t\tmask_%d := (<-ch%d) > 0\n" (State.bl_num bl.bname) (State.bl_num bl.bname);
      VSet.iter
        (fun var ->
          let w = (roundup_bitwidth (State.typ_of_var var)) in
          if w = 1 then
          bprintf b "\t\t%s_%d := (<-ch%d) > 0\n"
              (Gc.govar var)
              (State.bl_num bl.bname)
              (State.bl_num bl.bname)
          else
            bprintf b "\t\t%s_%d := uint%d(<-ch%d)\n"
              (Gc.govar var)
              (State.bl_num bl.bname)
              w
              (State.bl_num bl.bname))
        outputs)
    blocks;
  VSet.iter
    (fun var ->
      let sources = List.filter (fun bl -> VSet.mem var (outputs_of_block blocks_fv bl)) blocks in
      if VSet.mem var State.V.special then
        (* specials are assigned 0 unless the active block assigned them *)
        bprintf b "\t\t%s = TreeXor%d(io, %s)\n"
          (Gc.govar var)
          (roundup_bitwidth (State.typ_of_var var))
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (Gc.govar var) (State.bl_num bl.bname)) sources))
      else
        (* non-specials keep their value from before the blocks unless the active block assigned them *)
        bprintf b "\t\t%s = Select%d(io, TreeXor1(io, %s), TreeXor%d(io, %s), %s)\n"
          (Gc.govar var)
          (roundup_bitwidth (State.typ_of_var var))
          (String.concat ", " (List.map (fun bl -> sprintf "mask_%d" (State.bl_num bl.bname)) sources))
          (roundup_bitwidth (State.typ_of_var var))
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (Gc.govar var) (State.bl_num bl.bname)) sources))
          (Gc.govar var))
    (outputs_of_blocks blocks);
  if VSet.mem State.V.attsrcMemRes blocks_fv then begin
    (* We need to load from memory iff some block uses attsrcMemRes *)
    bprintf b "\n";
    bprintf b "\t\t/* load from memory if necessary */\n";
    bprintf b "\t\tif Reveal1(io, Icmp_eq8(io, _attsrcMemAct, Uint8(io, 1))) {\n";
    bprintf b "\t\t\t_attsrcMemRes = Load(io, _attsrcMemLoc, _attsrcMemSize)\n";
    bprintf b "\t\t}\n";
  end;
  if VSet.mem State.V.attsrcMemVal (outputs_of_blocks blocks) then begin
    (* We need to store to memory iff some block assigns attsrcMemVal *)
    bprintf b "\n";
    bprintf b "\t\t/* store to memory if necessary */\n";
    bprintf b "\t\tif Reveal1(io, Icmp_eq8(io, _attsrcMemAct, Uint8(io, 2))) {\n";
    bprintf b "\t\t\tStore(io, _attsrcMemLoc, _attsrcMemSize, _attsrcMemVal)\n";
    bprintf b "\t\t}\n";
  end;
  bprintf b "\n";
  bprintf b "\t\t/* are we done? */\n";
  bprintf b "\t\tdone = Reveal1(io, _attsrcIsDone)\n";
  bprintf b "\t}\n";
  bprintf b "\tanswer := Reveal32(io, _attsrcAnswer)\n";
  bprintf b "\tfmt.Printf(\"%%d: %%v\\n\", io.Id(), answer)\n";
  bprintf b "\t%s_done <- true\n" (Gc.govar f.fname);
  bprintf b "}\n";
  bprintf b "\n"

let rec bytes_of_value b bytes = function
  | typ, Int x ->
      let bytes2 = roundup_bitwidth typ / 8 in
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
            bprintf b1 "\tram[0x%x] = Uint8(io, 0x%x)\n" (loc + i) x
        done;
        (* Look for string constants, might be used by Printf *)
        (match gtyp with
        | Arraytyp(len, Integer 8) ->
            if (0 = Char.code (Buffer.nth bytevals (bytes - 1))) then
              let s = Buffer.sub bytevals 0 (len - 1) in
              Hashtbl.add string_constants loc s
        | _ -> ()) in
  List.iter pr_global m.cglobals;
  bprintf b "func initialize_ram(io Io) {\n";
  if !State.loc <> 0 then begin
    bprintf b "\tram := make([]byte, 0x%x)\n" !State.loc;
    Buffer.add_buffer b b1;
    bprintf b "\tio.InitRam(ram)\n";
  end;
  bprintf b "}\n";
  bprintf b "\n"

let print_function_circuit m f =
  let b = Buffer.create 11 in
  (* blocks *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import . \"%sgmw\"\n" package_prefix;
  bprintf b "import \"fmt\"\n";
  bprintf b "\n";
  bpr_globals b m;
  bpr_main b f;
  let blocks_fv = List.fold_left VSet.union VSet.empty (* TODO: eliminate duplicate code *)
      (List.map free_of_block f.fblocks) in
  List.iter (bpr_gmw_block b blocks_fv) f.fblocks;
  let blocks_go = Buffer.contents b in

  let b = Buffer.create 11 in
  (* main function *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import \"%sgmw\"\n" package_prefix;
  bprintf b "import \"flag\"\n";
  bprintf b "import \"fmt\"\n";
  bprintf b "import \"os\"\n";
  bprintf b "import \"runtime/pprof\"\n";
  bprintf b "\n";
  bprintf b "var args []string\n";
  bprintf b "var do_pprof bool\n";
  bprintf b "\n";
  bprintf b "func init_args() {\n";
  bprintf b "\tflag.BoolVar(&do_pprof, \"pprof\", false, \"run for profiling\")\n";
  bprintf b "\tflag.Parse()\n";
  bprintf b "\targs = flag.Args()\n";
  bprintf b "}\n";
  bprintf b "func next_arg() uint32 {\n";
  bprintf b "\tif len(args) <= 0 {\n";
  bprintf b "\t\tpanic(\"Not enough command-line arguments\")\n";
  bprintf b "\t}\n";
  bprintf b "\targ := 0\n";
  bprintf b "\tfmt.Sscanf(args[0], \"%%d\", &arg)\n";
  bprintf b "\targs = args[1:]\n";
  bprintf b "\treturn uint32(arg)\n";
  bprintf b "}\n";
  bprintf b "\n";
  bprintf b "var %s_done = make(chan bool, 1)\n" (Gc.govar f.fname);
  bprintf b "\n";
  bprintf b "func main() {\n";
  bprintf b "\tinit_args()\n";
  bprintf b "\tif do_pprof {\n";
  bprintf b "\t\tfile := \"cpu.pprof\"\n";
  bprintf b "\t\tf, err := os.Create(file)\n";
  bprintf b "\t\tif err != nil {\n";
  bprintf b "\t\t\tfmt.Println(\"Error: \", err)\n";
  bprintf b "\t\t}\n";
  bprintf b "\t\tpprof.StartCPUProfile(f)\n";
  bprintf b "\t\tdefer pprof.StopCPUProfile()\n";
  bprintf b "\t}\n";
  bprintf b "\tgmw.Simulation(len(args), %d, blocks_main, _main_done)\n" (List.length f.fblocks);
  bprintf b "}\n";
  let main_go = Buffer.contents b in
  pr_output_file "_blocks.go" blocks_go;
  pr_output_file "_main.go" main_go
