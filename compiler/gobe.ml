(* go back end *)

open Oli
open Printf
open Options

let package_prefix = "github.com/tjim/smpcc/runtime/"

let global_locations = Hashtbl.create 10
let string_constants = Hashtbl.create 10

let govar v =
  Str.global_replace (Str.regexp "[%@.]") "_" (fst (State.v_map v))

let bpr_go_opcode b opcode args =
  match opcode with
  | Llvm.Opcode.Add    -> bprintf b "Add%s\n" args
  | Llvm.Opcode.Sub    -> bprintf b "Sub%s\n" args
  | Llvm.Opcode.Shl    -> bprintf b "Shl%s\n" args
  | Llvm.Opcode.LShr   -> bprintf b "Lshr%s\n" args
  | Llvm.Opcode.AShr   -> bprintf b "Ashr%s\n" args
  | Llvm.Opcode.And    -> bprintf b "And%s\n" args
  | Llvm.Opcode.Or     -> bprintf b "Or%s\n" args
  | Llvm.Opcode.Xor    -> bprintf b "Xor%s\n" args
  | Llvm.Opcode.Trunc  -> bprintf b "Trunc%s\n" args
  | Llvm.Opcode.ZExt   -> bprintf b "Zext%s\n" args
  | Llvm.Opcode.SExt   -> bprintf b "Sext%s\n" args
  | Llvm.Opcode.Select -> bprintf b "Select%s\n" args
(* remainder are unsupported in the go back end *)
  | Llvm.Opcode.Invalid
  | Llvm.Opcode.Ret
  | Llvm.Opcode.Br
  | Llvm.Opcode.Switch
  | Llvm.Opcode.IndirectBr
  | Llvm.Opcode.Invoke
  | Llvm.Opcode.Invalid2
  | Llvm.Opcode.Unreachable
  | Llvm.Opcode.FAdd
  | Llvm.Opcode.FSub
  | Llvm.Opcode.Mul
  | Llvm.Opcode.FMul
  | Llvm.Opcode.UDiv
  | Llvm.Opcode.SDiv
  | Llvm.Opcode.FDiv
  | Llvm.Opcode.URem
  | Llvm.Opcode.SRem
  | Llvm.Opcode.FRem
  | Llvm.Opcode.Alloca
  | Llvm.Opcode.Load
  | Llvm.Opcode.Store
  | Llvm.Opcode.GetElementPtr
  | Llvm.Opcode.FPToUI
  | Llvm.Opcode.FPToSI
  | Llvm.Opcode.UIToFP
  | Llvm.Opcode.SIToFP
  | Llvm.Opcode.FPTrunc
  | Llvm.Opcode.FPExt
  | Llvm.Opcode.PtrToInt
  | Llvm.Opcode.IntToPtr
  | Llvm.Opcode.BitCast
  | Llvm.Opcode.ICmp
  | Llvm.Opcode.FCmp
  | Llvm.Opcode.PHI
  | Llvm.Opcode.Call
  | Llvm.Opcode.UserOp1
  | Llvm.Opcode.UserOp2
  | Llvm.Opcode.VAArg
  | Llvm.Opcode.ExtractElement
  | Llvm.Opcode.InsertElement
  | Llvm.Opcode.ShuffleVector
  | Llvm.Opcode.ExtractValue
  | Llvm.Opcode.InsertValue
  | Llvm.Opcode.Fence
  | Llvm.Opcode.AtomicCmpXchg
  | Llvm.Opcode.AtomicRMW
  | Llvm.Opcode.Resume
  | Llvm.Opcode.LandingPad
(*  | Llvm.Opcode.Unwind*)
    -> begin
      eprintf "Error: unsupported %s%s\n" (let b = Buffer.create 11 in bpr_opcode b opcode; Buffer.contents b) args;
      bprintf b "Unsupported(\"UNSUPPORTED %a%s\")\n" bpr_opcode opcode args
    end

let rec bpr_go_oValue b = function
  | GlobalVariable v ->
      (try
        let loc = Hashtbl.find global_locations v in
        bprintf b "Uint(io, %d, 64)" loc
      with Not_found ->
        eprintf "global not there %s\n" (govar v);
        bprintf b "%s" (govar v))
  | GlobalAlias v
  | Argument v
  | Function v
  | Variable v ->
      let (isaglobal,_,_) = v in
      if isaglobal then
        (try
          let loc = Hashtbl.find global_locations v in
          bprintf b "Uint(io, %d, 64)" loc
        with Not_found ->
          eprintf "global not there %s\n" (govar v);
          bprintf b "%s" (govar v))
      else
        bprintf b "%s" (govar v)
  | (ConstantInt(bw,Some x)) ->
      bprintf b "Int(io, %Ld, %d)" x bw
  | ConstantPointerNull width ->
      bprintf b "Uint(io, 0, %d) /* CAUTION: null */" width
  | UndefValue ->
      bprintf b "Uint(io, 0, 32) /* CAUTION: undef */" (* TODO: get correct width, we assume 32 for now *)
  | BasicBlock bl ->
      bprintf b "Uint(io, %d, %d)" (State.bl_num bl) (State.get_bl_bits())
  | ConstantExpr(Llvm.Opcode.IntToPtr,[(ty,op)]) ->
      bprintf b "/* CAUTION: inttoptr */ ";
      bpr_go_oValue b op
  | op ->
      bpr_oValue b op

let bpr_go_instr b is_gen declared_vars (nopt,i) =
  (* Look for a special case: clang declares a variable for the result of a printf that is not subsequently used.
     Go has a stricter check on this.
   *)
  let unused =
    (match i with
    | CallInst(_,_,_,(_,("printf" | "puts" | "putchar"),_), ops) -> true
    | StoreInst(a,ty,[(_,x);(_,addr)]) -> true
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
  | CallInst(_,_,_,(_,"printf",_),
             (_, ConstantExpr(Llvm.Opcode.GetElementPtr,
                              [(_,GlobalVariable(v));(_,ConstantInt(_, Some 0L));(_,ConstantInt(_, Some 0L))]))
             ::ops) ->
               (try
                 let s = String.escaped(Hashtbl.find string_constants v) in
                 if ops = [] then
                   bprintf b "Printf(io, mask, \"%s\")\n" s
                 else
                   bprintf b "Printf(io, mask, \"%s\", %a)\n" s (bpr_concat ", " bpr_go_oValue) (List.map snd ops)
               with _ ->
                 failwith "Error: first argument of printf must be a string constant")
  | CallInst(_,_,_,(_,"puts",_),
             (_, ConstantExpr(Llvm.Opcode.GetElementPtr,
                              [(_,GlobalVariable(v));(_,ConstantInt(_, Some 0L));(_,ConstantInt(_, Some 0L))]))
             ::[]) ->
               (try
                 let s = String.escaped(Hashtbl.find string_constants v) in
                 bprintf b "Printf(io, mask, \"%s\\n\")\n" s (* puts adds a newline *)
               with _ ->
                 failwith "Error: argument of puts must be a string constant")
  | CallInst(_,_,_,(_,"putchar",_),
             [(_, value)]) ->
               bprintf b "Printf(io, mask, \"%%c\", %a)\n" bpr_go_oValue value
  | CallInst(true,_,_,(true,"gen_int",_), ops) ->
      if is_gen then
        bprintf b "Gen_int(io, mask, next_arg)\n"
      else
        bprintf b "Gen_int(io, mask)\n"
  | CallInst(true,_,_,(true,"eval_int",_), ops) ->
      if is_gen then
        bprintf b "Eval_int(io, mask)\n"
      else
        bprintf b "Eval_int(io, mask, next_arg)\n"
  | LoadInst(a,result_ty,[(ty,addr)]) -> (* in case we are debugging loads *)
      bprintf b "LoadDebug(io, mask, %a)\n" bpr_go_oValue addr
  | StoreInst(a,ty,[(_,x);(_,addr)]) -> (* in case we are debugging stores*)
      bprintf b "StoreDebug(io, mask, %a, %a)\n" bpr_go_oValue addr bpr_go_oValue x
  | BitCastInst(ty,[(_,x)]) ->
      bprintf b "%a\n" bpr_go_oValue x
  | SExtInst(ty,[(_,x)]) ->
      bprintf b "Sext(io, %a, %d)\n" bpr_go_oValue x (bitwidth ty)
  | ZExtInst(ty,[(_,x)]) ->
      bprintf b "Zext(io, %a, %d)\n" bpr_go_oValue x (bitwidth ty)
  | BinaryOperator(ty,[x;(ty_y,(ConstantInt(_, Some y)))],Llvm.Opcode.Mul) ->
      (* It would be better to do this strength reduction in LLVM *)
      let shift_bits =
        match Int64.to_int y with
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
      bprintf b "Shl(io, %a, %d)\n" bpr_go_oValue (snd x) shift_bits
  | BinaryOperator(ty,[x;(ty_y,(ConstantInt(_, Some y)))],Llvm.Opcode.LShr) ->
      let shift_bits = Int64.to_int y in
      bprintf b "Lshr(io, %a, %d)\n" bpr_go_oValue (snd x) shift_bits
  | BinaryOperator(ty,[x;(ty_y,(ConstantInt(_, Some y)))],Llvm.Opcode.AShr) ->
      let shift_bits = Int64.to_int y in
      bprintf b "Ashr(io, %a, %d)\n" bpr_go_oValue (snd x) shift_bits
  | BinaryOperator(ty,[x;(ty_y,(ConstantInt(_, Some y)))],Llvm.Opcode.Shl) ->
      let shift_bits = Int64.to_int y in
      bprintf b "Shl(io, %a, %d)\n" bpr_go_oValue (snd x) shift_bits
  | BinaryOperator(_,ops,opcode) ->
      let args = Buffer.create 11 in
      if ops = [] then
        bprintf args "(io"
      else
        bprintf args "(io, ";
      let rec loop = function
        | hd::((_::_) as tl) -> bprintf args "%a, " bpr_go_oValue (snd hd); loop tl
        | [hd] -> bprintf args "%a" bpr_go_oValue (snd hd)
        | [] -> () in
      loop ops;
      bprintf args ")";
      bpr_go_opcode b opcode (Buffer.contents args)
  | ICmpInst(pred, [(_,op1);(_,op2)]) ->
      bprintf b "Icmp_";
      (match pred with
      | None -> bprintf b "ERROR"
      | Some x -> bpr_icmp b x);
      bprintf b "(io, %a, %a)\n" bpr_go_oValue op1 bpr_go_oValue op2;
  | IntToPtrInst(result_ty, [(ty,op)])
  | AssignInst(result_ty, [(ty,op)]) -> (* special instruction inserted by our compiler *)
      let bits_result = bitwidth result_ty in
      let bits_op = bitwidth ty in
      if bits_result = bits_op then
        bprintf b "%a\n" bpr_go_oValue op
      else if bits_result > bits_op then
        (* NB our oTypes do not have signed/unsigned versions so we zextend for now *)
        bprintf b "Zext(io, %a, %d)\n" bpr_go_oValue op bits_result
      else (* bits_result < bits_op *)
        bprintf b "%a[:%d]\n" bpr_go_oValue op bits_result;
  | SelectInst(ty, [x;y;z]) ->
      bprintf b "Select(io, %a, %a, %a)\n" bpr_go_oValue (snd x) bpr_go_oValue (snd y) bpr_go_oValue (snd z)
  | SwitchInst(ty, (ty0,op0)::(ty1,op1)::ops) ->
      (* The ocaml interface for switches is incomplete, e.g., there is
         no getCondition(), getCaseValue(), getCaseSuccessor().
         So we can't get what we need from ops.
         However what I see coming out of LLVM is cases 0, 1, 2, etc.
       *)
      (* op0 is the value to switch on, op1 is the default target *)
      let rec get_cases = function
        | [] -> []
        | _::t::tl -> t::(get_cases tl)
        | _ -> failwith "odd length switch ops, should be impossible" in
      let cases = get_cases ops in
      bprintf b "Switch(io, %a, %a, %a)\n" bpr_go_oValue op0 bpr_go_oValue op1
        (bpr_concat ", " bpr_go_oValue) (List.map snd cases)
  | TruncInst(ty, [(_,x)]) ->
      bprintf b "%a[:%d]\n" bpr_go_oValue x (bitwidth ty)
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
  bprintf b "// <label>:%s\n" (let (_,x,_) = bl.b_name in x);
  bprintf b "func %s%d(io %s, ch chan []%s, block_num%a []%s) {\n"
    (gen_or_eval is_gen)
    (State.bl_num bl.b_name)
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
          | CallInst(_,_,_,(_,("printf" | "puts"),_),
                     (_, ConstantExpr(Llvm.Opcode.GetElementPtr,
                                      [(_,GlobalVariable _);(_,ConstantInt(_, Some 0L));(_,ConstantInt(_, Some 0L))]))
                     ::_)
          | CallInst(_,_,_,(_,"putchar",_),[_])
          | CallInst(true,_,_,(true,"gen_int",_), _)
          | CallInst(true,_,_,(true,"eval_int",_), _)
          | LoadInst(_,_,[_])
          | StoreInst(_, _,[_;_]) ->
              true
          | _ ->
              false))
        false
        bl.b_instrs) in
  if block_requires_mask then
    bprintf b "\tmask := Icmp_eq(io, block_num, Uint(io, %d, 32))\n" (State.bl_num bl.b_name);
  if options.debug_blocks then
    bprintf b "\tPrintf(io, mask, \"Block %d\\n\")\n" (State.bl_num bl.b_name);
  ignore(List.fold_left (bpr_go_instr b is_gen) (free_of_block bl) bl.b_instrs);
  if not(VSet.is_empty outputs) then begin
    if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
      bprintf b "\tch <- mask\n";
    VSet.iter
      (fun var ->
        let value = Variable var in
        bprintf b "\tch <- Mask(io, mask, %a)\n" bpr_go_oValue value)
      outputs
  end;
  bprintf b "}\n\n"

let bpr_main b f is_gen =
  let blocks = f.f_blocks in
  let blocks_fv = List.fold_left VSet.union VSet.empty
      (List.map free_of_block blocks) in
  let blockios = String.concat "" (List.map (fun bl -> sprintf ", io%d" (State.bl_num bl.b_name)) blocks) in
  bprintf b "func %s_main(io%s %s) {\n" (gen_or_eval is_gen) blockios (io_type is_gen);
  bprintf b "\n";
  if is_gen then
    bprintf b "\tinitialize_ram(io)\n\n";
  bprintf b "\t/* create output channels */\n";
  List.iter
    (fun bl ->
      let capacity = VSet.cardinal(outputs_of_block blocks_fv bl) in
      bprintf b "\tch%d := make(chan []%s, %d)\n" (State.bl_num bl.b_name) (bit_type is_gen) capacity
      )
    blocks;
  bprintf b "\n";
  bprintf b "\t/* special variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := Uint(io, 0, %d)\n" (govar var) (bitwidth ((fun (_,_,ty) -> ty) var));
    )
    (VSet.add (State.V.attsrcStateO()) (* if there is only one block this is used but not assigned *)
       (VSet.inter State.V.special (assigned_of_blocks blocks)));
  bprintf b "\n";
  bprintf b "\t/* block free variables */\n";
  VSet.iter
    (fun var ->
      bprintf b "\t%s := Uint(io, 0, %d)\n" (govar var) (bitwidth ((fun (_,_,ty) -> ty) var));
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
        (State.bl_num bl.b_name)
        (State.bl_num bl.b_name)
        (State.bl_num bl.b_name)
        bpr_go_block_args bl)
    blocks;
  bprintf b "\n";
  bprintf b "\t\t/* mux the outputs*/\n";
  List.iter
    (fun bl ->
      let outputs = outputs_of_block blocks_fv bl in
      if not(VSet.is_empty (VSet.diff outputs State.V.special)) then
        bprintf b "\t\tmask_%d := <- ch%d\n" (State.bl_num bl.b_name) (State.bl_num bl.b_name);
      VSet.iter
        (fun var ->
          bprintf b "\t\t%s_%d := <- ch%d\n"
            (govar var)
            (State.bl_num bl.b_name)
            (State.bl_num bl.b_name))
        outputs)
    blocks;
  VSet.iter
    (fun var ->
      let sources = List.filter (fun bl -> VSet.mem var (outputs_of_block blocks_fv bl)) blocks in
      if VSet.mem var State.V.special then
        (* specials are assigned 0 unless the active block assigned them *)
        bprintf b "\t\t%s = TreeXor(io, %s)\n"
          (govar var)
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.b_name)) sources))
      else
        (* non-specials keep their value from before the blocks unless the active block assigned them *)
        bprintf b "\t\t%s = Select(io, TreeXor(io, %s), TreeXor(io, %s), %s)\n"
          (govar var)
          (String.concat ", " (List.map (fun bl -> sprintf "mask_%d" (State.bl_num bl.b_name)) sources))
          (String.concat ", " (List.map (fun bl -> sprintf "%s_%d" (govar var) (State.bl_num bl.b_name)) sources))
          (govar var))
    (outputs_of_blocks blocks);
  if VSet.mem State.V.attsrcMemRes blocks_fv then begin
    (* We need to load from memory iff some block uses attsrcMemRes *)
    bprintf b "\n";
    bprintf b "\t\t/* load from memory if necessary */\n";
    bprintf b "\t\tif Reveal(io, Icmp_eq(io, _attsrcMemAct, Uint(io, 1, 2)))[0] {\n";
    bprintf b "\t\t\t_attsrcMemRes = Load(io, _attsrcMemLoc, _attsrcNumElts, _attsrcMemSize)\n";
    bprintf b "\t\t}\n";
  end;
  if VSet.mem State.V.attsrcMemVal (outputs_of_blocks blocks) then begin
    (* We need to store to memory iff some block assigns attsrcMemVal *)
    bprintf b "\n";
    bprintf b "\t\t/* store to memory if necessary */\n";
    bprintf b "\t\tif Reveal(io, Icmp_eq(io, _attsrcMemAct, Uint(io, 2, 2)))[0] {\n";
    bprintf b "\t\t\tStore(io, _attsrcMemLoc, _attsrcNumElts, _attsrcMemSize, _attsrcMemVal)\n";
    bprintf b "\t\t}\n";
  end;
  bprintf b "\n";
  bprintf b "\t\t/* are we done? */\n";
  bprintf b "\t\tdone = Reveal(io, _attsrcIsDone)[0]\n";
  bprintf b "\t}\n";
  bprintf b "\tanswer := RevealInt32(io, _attsrcAnswer)\n";
  bprintf b "\tfmt.Printf(\"%s: %%v\\n\", answer)\n" (gen_or_eval is_gen);
  bprintf b "\t%s_done <- true\n" (govar f.f_name);
  bprintf b "}\n";
  bprintf b "\n"

let rec bytes_of_value b bytes = function
  | ConstantInt(1,_) ->
      failwith "bytes_of_value: bitwidth 1"
  | ConstantInt(bitwidth,_) when (bitwidth mod 8 <> 0) ->
      failwith "bytes_of_value: value does not fit evenly into bytes"
  | ConstantInt(bitwidth,value) ->
      let bytes2 = bitwidth / 8 in
      (match value with
        None ->
          for i = 1 to bytes2 do
            bprintf b "%c" (Char.chr 0)
          done
      | Some x ->
          let ff = Int64.of_int 255 in
          let y = ref x in
          for i = 1 to bytes2 do
            let lowbyte = Char.chr(Int64.to_int(Int64.logand !y ff)) in
            bprintf b "%c" lowbyte;
            y := Int64.shift_right_logical !y 8
          done);
      bytes - bytes2
  | ConstantAggregateZero ->
      for i = 1 to bytes do
        bprintf b "%c" (Char.chr 0)
      done;
      0
  | NullValue ->
      bprintf b "%c%c%c%c" (Char.chr 0) (Char.chr 0) (Char.chr 0) (Char.chr 0);
      (bytes - 4)
  | ConstantStruct(is_packed,ops) ->
      (* TODO: handle packing and alignment issues *)
      List.fold_left
        (fun bytes op -> bytes_of_value b bytes op)
        bytes
        ops
  | ConstantPointerNull bitwidth ->
      let bytes2 = bitwidth / 8 in
      for i = 1 to bytes2 do
        bprintf b "%c" (Char.chr 0)
      done;
      (bytes - bytes2)
  | ConstantArray ops
  | ConstantVector ops
  | ConstantDataArray ops
  | ConstantDataVector ops ->
      List.fold_left
        (fun bytes op -> bytes_of_value b bytes op)
        bytes
        ops
  | ConstantExpr _
  | UndefValue
  | Function _
  | BlockAddress _
  | ConstantFP
  | GlobalAlias _
  | GlobalVariable _
  | Variable _
  | InlineAsm
  | MDNode
  | MDString _
  | Argument _
  | BasicBlock _
  | Instruction _ ->
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
  let alloc_global {g_name; g_ty; g_val; g_linkage; g_visibility; g_is_constant} =
    Hashtbl.add global_locations g_name !loc;
    let bytes =
      try bytewidth (getElementType g_ty) with _ -> 100 in
    let bytevals = Buffer.create bytes in
    let written = bytes_of_value bytevals bytes g_val in
    if written != 0 || bytes != Buffer.length bytevals then begin
      let gname = (let b2 = Buffer.create 11 in bpr_variable b2 g_name; Buffer.contents b2) in
      eprintf "WHOA! expected %d bytes, found %d bytes for global %s\n" bytes (Buffer.length bytevals) gname;
    end;
    for i = 0 to bytes - 1 do
      bprintf b1 "\tRam[0x%x] = 0x%x\n" (!loc + i) (Char.code (Buffer.nth bytevals i))
    done;
    (* Look for string constants, might be used by Printf *)
    (match g_ty with
    | Pointer(_, Array(len, Integer 8)) ->
        if (0 = Char.code (Buffer.nth bytevals (bytes - 1))) then
          let s = Buffer.sub bytevals 0 (len - 1) in
          Hashtbl.add string_constants g_name s
    | _ -> ());
    loc := !loc + bytes;
    align 4 in
  List.iter alloc_global m.globals;
  bprintf b "func initialize_ram(io GenVM) {\n";
  if !loc <> 0 then begin
    bprintf b "\tRam := make([]byte, 0x%x)\n" !loc;
    Buffer.add_buffer b b1;
    bprintf b "\tio.InitRam(Ram)\n";
  end;
  bprintf b "}\n";
  bprintf b "\n"

let gep_elim_ovalue =
  ovalue_map (function
(*
    | ConstantExpr(Llvm.Opcode.GetElementPtr, (Pointer(s,ety),Variable v)::tl) as c
*)
    | ConstantExpr(Llvm.Opcode.GetElementPtr, (Pointer(s,ety),GlobalVariable v)::tl) as c ->
        if Hashtbl.mem string_constants v || not(Hashtbl.mem global_locations v) then c else
        let ety = Array(0,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
        let rec loop ety = function
          | [] -> Int64.of_int(Hashtbl.find global_locations v)
          | (_,y)::tl ->
              (match ety,y with
              | Array(_,ety'),(ConstantInt(_,Some(i))) -> (* NB we ignore the bitwidth of the constant *)
                  Int64.add
                    (Int64.mul i (Int64.of_int(bytewidth ety')))
                    (loop ety' tl)
              | Struct(_),(ConstantInt(_,Some(i))) ->
                  (* TEMPORARY HACK, assume all fields have width 4 *)
                  if tl=[] then
                    Int64.add
                      (Int64.mul i (Int64.of_int 4))
                      (Int64.of_int(Hashtbl.find global_locations v))
                  else
                    failwith "gep_elim_ovalue: struct"
              | _ ->
                  let b = Buffer.create 11 in
                  bprintf b "gep_elim_ovalue error: type is %a, value is %a\n" bpr_oType ety bpr_oValue y;
                  eprintf "%s" (Buffer.contents b);
                  failwith "gep_elim_ovalue") in
        let sum = loop ety tl in
        ConstantInt(64,Some sum)
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
  gep_elim_ovalue f;
  bpr_main b f true;
  let blocks_fv = List.fold_left VSet.union VSet.empty (* TODO: eliminate duplicate code *)
      (List.map free_of_block f.f_blocks) in
  List.iter (bpr_go_block b blocks_fv true) f.f_blocks;
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
  List.iter (bpr_go_block b blocks_fv false) f.f_blocks;
  let eval_go = Buffer.contents b in
  let b = Buffer.create 11 in
  (* main function *)
  bprintf b "package main\n";
  bprintf b "\n";
  bprintf b "import \"%s%s/eval\"\n" package_prefix (match options.circuitlib with None -> "yao" | Some x -> x);
  bprintf b "import \"fmt\"\n";
  bprintf b "import \"os\"\n";
  bprintf b "\n";
  bprintf b "var args []string\n";
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
  bprintf b "var %s_done = make(chan bool, 1)\n" (govar f.f_name);
  bprintf b "\n";
  bprintf b "func main() {\n";
  bprintf b "\targs = os.Args[1:]\n";
  let blocknums = List.map (fun bl -> State.bl_num bl.b_name) f.f_blocks in
  bprintf b "\tgio, eio := eval.IO(-1)\n";
  List.iter
    (fun n ->
      bprintf b "\tgio%d, eio%d := eval.IO(%d)\n" n n n)
    blocknums;
  bprintf b "\tgo gen_main(gio, %s)\n" (String.concat ", " (List.map (fun n -> sprintf "gio%d" n) blocknums));
  bprintf b "\tgo eval_main(eio, %s)\n" (String.concat ", " (List.map (fun n -> sprintf "eio%d" n) blocknums));
  bprintf b "\t<-%s_done\n" (govar f.f_name);
  bprintf b "\t<-%s_done\n" (govar f.f_name);
  bprintf b "\n";
  bprintf b "\tfmt.Println(\"Done\")\n";
  bprintf b "}\n";
  let main_go = Buffer.contents b in
  pr_output_file "_gen.go" gen_go;
  pr_output_file "_eval.go" eval_go;
  pr_output_file "_main.go" main_go
