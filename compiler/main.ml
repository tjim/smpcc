(* mpcc: compile LLVM object file to circuit *)

(* Compile this program with
 *
 *     ocamlbuild mpcc.byte
 *
 *)

open Oli
open Printf
open Options
module V = State.V

(*
  An LLVM file produced by clang, etc., obeys the following conventions:
  * No two instructions bind the same variable (SSA)
  * Every block ends in a branch or return
  * Every function has a distinguished initial block (block 0)

  We are going to transform an LLVM file to a circuit in a series of
  phases.  Each phase except the last will produce a set of LLVM
  blocks; however the above invariants will be changed by our phases.

  We will be using a stepped execution model where in each step, all
  blocks are executed.  One block will be the designated "current"
  block, and the result of a step of execution will be the results of
  the current block; results of all other blocks are discarded.  The
  input variable "attsrcState" will indicate the current block, and
  the output variable "attsrcStateO" will indicate the current block
  for the next step.

  The compiler has the following phases.

  PHI ELIMINATION

  Suppose we have block L with

      a = phi(a if from L1, b if from L2)
      b = phi(b if from L1, a if from L2)

  We need to have a simultaneous assignment of a and b on the branches
  from L1 and L2 to L.

  So we create new blocks L1' and L2', each of which will branch to L.

  In block L1 we replace the branch to L with a branch to L1', 
  and in block L2 we replace the branch to L with a branch to L2'.

  Then in L1' we add the appropriate assignments, using a temporary so
  that the assignment is guaranteed to have the affect of a
  simultaneous assignment:

      fresh1 = a
      fresh2 = b
      a = fresh1
      b = fresh2
      br L

  Similarly in L2' we have

      fresh1 = b
      fresh2 = a
      a = fresh1
      b = fresh2
      br L

  The output of phi elimination is no longer in SSA form.

  This strategy adds more overhead than I'd like but it is easy to see
  that it is correct.  Note that if L1 has a single branch then we
  could merge L1' into L1 (the same for L2 of course).

  BRANCH ELIMINATION

  We transform

      br i1 %1, label %split13, label %.lr.ph

  into

      %attsrcMemAct = i2 0                                        // do not access memory
      %attsrcIsDone = i1 false                                    // ... and evaluation should continue
      %attsrcStateO = select i1 %1, label %split13, label %.lr.ph // ... in this next state

  There is a similar transformation for switch instructions.

  We don't support indirectbr for the moment (it computes a label and
  branches to it).

  LOAD/STORE ELIMINATION

  For each llvm block, split any block that has a memory access into two blocks.
  The second block should expect the result of the memory access in attsrcMemRes.
  So for example

      %1 = load %2
      ...

  becomes

      attsrcMemAct = 2  // load
      attsrcNumElts = 1 // ... 1 element
      attsrcMemSize = 4 // ... of 4 bytes
      attsrcMemLoc = %2 // ... from memory at location %2
      attsrcIsDone = 1  // ... and continue after the load
      attsrcStateO %9   // ... at label %9
      // end of old block

      // new block
      %9:
      %1 = attsrcMemRes
      ...

  We modify the load/store instruction to refer to the new block to branch to,
  unconditionally, rather than add a branch, because that makes the implementation easier.

  GEP ELIMINATION

  getelementptr (GEP) is an address-calculation instruction.
  We convert it into arithmetic instructions.
  For example,

      %6 = getelementptr i32* %x, i64 %5

  turns into

      %x0 = mul i64 %5, 4
      %x1 = add [0 x i32]* %x, i64 %x0
      %6 = i32* %x1

  FREE VARIABLE ELIMINATION

  Now consider variables that cross blocks.  They appear free in some
  block and they are defined in another block.  They might have been
  defined in PHI nodes, but if there is only one definition PHI need
  not have been used.

  We'll make these variables into input variables, and for each we
  need an output variable.  So look now at places where they are
  defined.  Instead of defining the variable there define its output
  variable, and substitute for it in case it is both used and defined
  there.

  There is a problem with load/store elimination.  Pre-split you might
  see a block define a variable for another block, then post split the
  second block does not define the variable.  You can only see that it
  should pass it on by the control flow.  However, for any block that
  does not define the variable we should just pass on the input
  variable.  In this way control flow still saves us.

  We call these variables "loop variables".  It is the job of the
  stepper take loop outputs and flow them to loop inputs.  On the
  initial step, loop inputs can be set to zero.

 *)

(* wow this is ugly *)
let rec repl_oValue var replacement x =
  match x with
  | Variable var2   -> if var=var2 then replacement else x
  | Argument var2   -> if var=var2 then replacement else x
  | BasicBlock var2 -> if var=var2 then replacement else x
  | InlineAsm       -> x
  | MDNode          -> x
  | NullValue       -> x
  | MDString _      -> x
  | BlockAddress x           -> BlockAddress(repl_oValue var replacement x)
  | ConstantAggregateZero    -> x
  | ConstantArray vals       -> ConstantArray(List.map (repl_oValue var replacement) vals)
  | ConstantDataArray vals   -> ConstantDataArray(List.map (repl_oValue var replacement) vals)
  | ConstantDataVector vals  -> ConstantDataVector(List.map (repl_oValue var replacement) vals)
  | ConstantExpr(opcode,ops) -> ConstantExpr(opcode,repl_ops var replacement ops)
  | ConstantFP               -> x
  | ConstantInt _            -> x
  | ConstantPointerNull _    -> x
  | ConstantStruct(y,vals)   -> ConstantStruct(y,List.map (repl_oValue var replacement) vals)
  | ConstantVector vals      -> ConstantVector(List.map (repl_oValue var replacement) vals)
  | Function       _ -> x (* doesn't make sense to replace with oValue *)
  | GlobalAlias    _ -> x (* doesn't make sense to replace with oValue *)
  | GlobalVariable _ -> x (* doesn't make sense to replace with oValue *)
  | UndefValue               -> x
  | Instruction x   -> Instruction (repl_oInstruction var replacement x)
and repl_ops var replacement = function
  | [] -> []
  | (ty,op)::tl -> (ty, repl_oValue var replacement op)::(repl_ops var replacement tl)
and repl_oInstruction var replacement x =
  match x with
  | BinaryOperator     (ty,ops,opcode) -> BinaryOperator     (ty, repl_ops var replacement ops, opcode)
  | CallInst           (y,z,ty,w,ops)  -> CallInst           (y,z,ty,w, repl_ops var replacement ops)
  | FCmpInst           (ty,ops)        -> FCmpInst           (ty,repl_ops var replacement ops)
  | ICmpInst           (icmp,ops)      -> ICmpInst           (icmp,repl_ops var replacement ops)
  | AssignInst         (ty,ops)        -> AssignInst         (ty, repl_ops var replacement ops)
  | ExtractElementInst (ty,ops)        -> ExtractElementInst (ty, repl_ops var replacement ops)
  | GetElementPtrInst  (ty,ops)        -> GetElementPtrInst  (ty, repl_ops var replacement ops)
  | InsertElementInst  (ty,ops)        -> InsertElementInst  (ty, repl_ops var replacement ops)
  | InsertValueInst    (ty,ops)        -> InsertValueInst    (ty, repl_ops var replacement ops)
  | LandingPadInst     (ty,ops)        -> LandingPadInst     (ty, repl_ops var replacement ops)
  | PHINode            (ty,incoming)   -> PHINode            (ty, List.map
                                                                (fun (op,block_name) -> (repl_oValue var replacement op, block_name))
                                                                incoming)
  | SelectInst         (ty,ops)        -> SelectInst         (ty, repl_ops var replacement ops)
  | ShuffleVectorInst  (ty,ops)        -> ShuffleVectorInst  (ty, repl_ops var replacement ops)
  | StoreInst          (a,ty,ops)      -> StoreInst          (a, ty, repl_ops var replacement ops)
  | BranchInst         (ty,ops)        -> BranchInst         (ty,repl_ops var replacement ops)
  | IndirectBrInst     (ty,ops)        -> IndirectBrInst     (ty,repl_ops var replacement ops)
  | InvokeInst         (ty,ops)        -> InvokeInst         (ty,repl_ops var replacement ops)
  | ReturnInst         (ty,ops)        -> ReturnInst         (ty,repl_ops var replacement ops)
  | SwitchInst         (ty,ops)        -> SwitchInst         (ty,repl_ops var replacement ops)
  | UnreachableInst    (ty,ops)        -> UnreachableInst    (ty,repl_ops var replacement ops)
  | ResumeInst         (ty,ops)        -> ResumeInst         (ty,repl_ops var replacement ops)
  | AllocaInst         (a,ty,ops)      -> AllocaInst         (a,ty,repl_ops var replacement ops)
  | BitCastInst        (ty,ops)        -> BitCastInst        (ty,repl_ops var replacement ops)
  | FPExtInst          (ty,ops)        -> FPExtInst          (ty,repl_ops var replacement ops)
  | FPToSIInst         (ty,ops)        -> FPToSIInst         (ty,repl_ops var replacement ops)
  | FPToUIInst         (ty,ops)        -> FPToUIInst         (ty,repl_ops var replacement ops)
  | FPTruncInst        (ty,ops)        -> FPTruncInst        (ty,repl_ops var replacement ops)
  | IntToPtrInst       (ty,ops)        -> IntToPtrInst       (ty,repl_ops var replacement ops)
  | PtrToIntInst       (ty,ops)        -> PtrToIntInst       (ty,repl_ops var replacement ops)
  | SExtInst           (ty,ops)        -> SExtInst           (ty,repl_ops var replacement ops)
  | SIToFPInst         (ty,ops)        -> SIToFPInst         (ty,repl_ops var replacement ops)
  | TruncInst          (ty,ops)        -> TruncInst          (ty,repl_ops var replacement ops)
  | UIToFPInst         (ty,ops)        -> UIToFPInst         (ty,repl_ops var replacement ops)
  | ZExtInst           (ty,ops)        -> ZExtInst           (ty,repl_ops var replacement ops)
  | ExtractValueInst   (ty,ops)        -> ExtractValueInst   (ty,repl_ops var replacement ops)
  | LoadInst           (a,ty,ops)      -> LoadInst           (a,ty,repl_ops var replacement ops)
  | VAArgInst          (ty,ops)        -> VAArgInst          (ty,repl_ops var replacement ops)
  | FenceInst          (ty,ops)        -> FenceInst          (ty, repl_ops var replacement ops)
  | AtomicCmpXchgInst  (ty,ops)        -> AtomicCmpXchgInst  (ty, repl_ops var replacement ops)
  | AtomicRMWInst      (ty,ops)        -> AtomicRMWInst      (ty, repl_ops var replacement ops)
and repl_oCallInst var replacement x =
  match x with
  | IntrinsicInst x -> repl_oIntrinsicInst var replacement x
and repl_oIntrinsicInst var replacement x =
  match x with
  | DbgInfoIntrinsic x -> DbgInfoIntrinsic (repl_oDbgInfoIntrinsic var replacement x)
  | MemIntrinsic     x -> MemIntrinsic     (repl_oMemIntrinsic var replacement x)
and repl_oDbgInfoIntrinsic var replacement x =
  match x with
  | DbgDeclareInst -> x
and repl_oMemIntrinsic var replacement x =
  match x with
  | MemCpyInst  -> x
  | MemMoveInst -> x
  | MemSetInst  -> x

let repl_instrs var replacement instrs =
  let rec loop = function
    | [] -> []
    | (nopt,instr)::tl ->
        let replaced = repl_oInstruction var replacement instr in
        (match nopt with
        | None -> (nopt, replaced)::(loop tl)
        | Some x ->
            if x=var then (Some x,replaced)::tl else
            (Some x, replaced)::(loop tl)) in
  loop instrs

let assign_instr nopt result_ty ty op =
  (nopt,AssignInst(result_ty, [(ty,op)]))

(*
  bl_target has var = phi (value,bl_source) --> record and remove
  record means
    tbl[(bl_target, bl_source)] = bl_fresh, [(var,value)]


  each (bl_target, bl_source) -->
    fresh block bl_fresh
    replace bl_target by bl_fresh in bl_source
  build bl_fresh
    

*)
let phi_elimination f =
  let bl_assoc =
    (* maps block name to block *)
    List.map (fun bl -> bl.b_name, bl) f.f_blocks in
  let tbl =
    (* maps a (target_block_name, source_block_name) to (fresh_block_name, assignments) *)
    Hashtbl.create 11 in
  (* Gather PHI information *)
  List.iter
    (fun bl ->
      List.iter
        (function
          | (Some var,PHINode(_, incoming)) ->
              List.iter
                (fun (value,source_block_name) ->
                  let (_, assignments) =
                    if not(Hashtbl.mem tbl (bl.b_name, source_block_name)) then
                      Hashtbl.add tbl (bl.b_name, source_block_name) (State.fresh_label(), ref []);
                    Hashtbl.find tbl (bl.b_name, source_block_name) in
                  assignments := (var,value)::!assignments)
                incoming
          | (None, PHINode _) -> failwith "Error: PHINode without variable"
          | _ -> ())
        bl.b_instrs)
    f.f_blocks;
  (* Remove PHI nodes *)
  List.iter
    (fun bl ->
      let non_phi =
        List.filter
          (function (_,PHINode(_, _)) -> false
            | _ -> true)
          bl.b_instrs in
      bl.b_instrs <- non_phi)
    f.f_blocks;
  (* Retarget branches *)
  Hashtbl.iter
    (fun (target_block_name, source_block_name) (fresh_block_name, _) ->
      let source_bl =
        try List.assoc source_block_name bl_assoc
        with _ -> failwith "Error: PHINode source does not exist" in
      source_bl.b_instrs <- repl_instrs target_block_name (BasicBlock fresh_block_name) source_bl.b_instrs)
    tbl;
  (* Add the new blocks *)
  let new_blocks = ref [] in
  Hashtbl.iter
    (fun (target_block_name, _) (fresh_block_name, assignments) ->
      let b_name = fresh_block_name in
      let before_and_afters =
        List.map
          (fun (var, value) ->
            let (_,_,ty) = var in
            let temp_var = (false, State.fresh(), ty) in
            let before = assign_instr (Some temp_var) ty ty value in
            let after = assign_instr (Some var) ty ty (Variable temp_var) in
            (before,after))
          !assignments in
      let (befores, afters) = List.split before_and_afters in
      let branch = (None, BranchInst(Label,[(Label, BasicBlock target_block_name)])) in
      let b_instrs = befores @ afters @ [branch] in
      new_blocks := {b_name;b_instrs}::!new_blocks)
    tbl;
  f.f_blocks <- f.f_blocks @ !new_blocks;
  ()

(* Replace getelementptr by arithmetic *)
(* http://www.llvm.org/docs/GetElementPtr.html *)
let gep_elimination f =
  let elim (nopt,i) =
    (match i with
    | GetElementPtrInst(ty,(Pointer(s,ety),x)::tl) ->
        (* begin *)
        (*   let pty_string = let b = Buffer.create 11 in bpr_oType b (Pointer(s,ety)); Buffer.contents b in *)
        (*   let x_string = let b = Buffer.create 11 in bpr_oValue b x; Buffer.contents b in *)
        (*   eprintf "getelementptr working on %s, %s\n" pty_string x_string *)
        (* end; *)
        let ety = Array(0,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
        let rec loop x ety = function
          | [] -> [assign_instr nopt (Pointer(s,ety)) (Pointer(s,ety)) x]
          | (yty,y)::tl ->
              (match ety,y with
              | Array(_,ety'),(ConstantInt(_,Some(i))) -> (* NB we ignore the bitwidth of the constant *)
                  (* invariant: yty = i64 *)
                  let b = (ConstantInt(64,Some(Int64.mul i (Int64.of_int(bytewidth ety'))))) in
                  let new_x = (false,State.fresh(),Pointer(s,ety')) in
                  (Some(new_x),BinaryOperator(Pointer(s,ety'),     (* the types here involve casting *)
                                              [(Pointer(s,ety),x);
                                               (Integer 64,b)],
                                              Llvm.Opcode.Add))
                  ::(loop (Variable new_x) ety' tl)
              | Array(_,ety'),Variable _ ->
                  (* invariant: yty = i64 *)
                  let b = (false,State.fresh(),Integer 64) in
                  let new_x = (false,State.fresh(),Pointer(s,ety')) in
                  (Some(b),BinaryOperator(Integer 64,
                                          [(yty,y);
                                           (Integer 64,(ConstantInt(64,Some(Int64.of_int(bytewidth ety')))))],
                                          Llvm.Opcode.Mul))
                  ::(Some(new_x),BinaryOperator(Pointer(s,ety'),     (* the types here involve casting *)
                                                [(Pointer(s,ety),x);
                                                 (Integer 64,Variable b)],
                                                Llvm.Opcode.Add))
                  ::(loop (Variable new_x) ety' tl)
              | Struct(_),(ConstantInt(32,Some(i))) ->
                  (* TEMPORARY HACK *)
                  let b = (ConstantInt(64,Some(Int64.mul i (Int64.of_int 4)))) in
                  let new_x = (false,State.fresh(),Pointer(0,Integer 32)) in
                  (Some(new_x),BinaryOperator(Pointer(0,Integer 32),     (* the types here involve casting *)
                                              [(Pointer(0,Integer 32),x);
                                               (Integer 64,b)],
                                              Llvm.Opcode.Add))
                  ::(loop (Variable new_x) (Integer 32) tl)
                  (* failwith "TODO: struct in getelementptr" *)
              | _ ->
                  let ty_string = let b = Buffer.create 11 in bpr_oType b ety; Buffer.contents b in
                  let y_string = let b = Buffer.create 11 in bpr_oValue b y; Buffer.contents b in
                  failwith (sprintf "getelementptr: %s, %s" ty_string y_string)) in
        loop x ety tl
    | i -> [(nopt,i)]) in
  (f.f_blocks <-
    (List.map
      (fun bl ->
        { b_name = bl.b_name;
          b_instrs = List.concat (List.map elim bl.b_instrs);
        })
      f.f_blocks));
  ()

(* If a block contains a load or store, we split the block into two.  The second block should
   expect the result of the load/store in attsrcMemRes. *)
let load_store_elimination f =
  let split_memory_accesses =
    let split_block bl =
      let rec split = function
        | [] ->
            ([],[])
        | (nopt,(LoadInst(a,result_ty,[(ty,x)])))::tl ->
            (*TODO:alignment*)
            let b_name = State.fresh_label() in
            let b_instrs, bl_list = split tl in
            let b_instrs =
              (assign_instr nopt result_ty ty (Variable V.attsrcMemRes))::b_instrs in
            [
(*              assign_instr (Some(V.attsrcIsDone)) (Integer 1) (Integer 1) (ConstantInt(1, Some Int64.zero));*)
              assign_instr (Some(V.attsrcMemAct)) (Integer 2) (Integer 2) (ConstantInt(2, Some Int64.one));
              assign_instr (Some(V.attsrcNumElts)) (Integer 32) (Integer 32) (ConstantInt(32, Some Int64.one));
(*FIX: look at type for MemSize, on assign_instr you might have to select subset of bits*)
              assign_instr (Some(V.attsrcMemSize)) (Integer 32) (Integer 32) (ConstantInt(32, Some(Int64.of_int 4)));
              assign_instr (Some(V.attsrcMemLoc)) (Integer 64) (Integer 64) x;
              assign_instr (Some(V.attsrcStateO())) Label Label (BasicBlock b_name) ],
            {b_name;b_instrs}::bl_list
        | (nopt,StoreInst(a,ty,[(_,x);(_,addr)]))::tl ->
            (*TODO:alignment*)
(*
            (match addr with
              ConstantExpr(opcode,_) ->
                if opcode = Llvm.Opcode.GetElementPtr then eprintf "Yep\n"
            | _ -> ());
*)
            let b_name = State.fresh_label() in
            let b_instrs, bl_list = split tl in
            [ assign_instr (Some(V.attsrcIsDone)) (Integer 1) (Integer 1) (ConstantInt(1, Some Int64.zero));
              assign_instr (Some(V.attsrcMemAct)) (Integer 2) (Integer 2) (ConstantInt(2, Some(Int64.of_int 2)));
              assign_instr (Some(V.attsrcNumElts)) (Integer 32) (Integer 32) (ConstantInt(32, Some Int64.one));
              assign_instr (Some(V.attsrcMemSize)) (Integer 32) (Integer 32) (ConstantInt(32, Some(Int64.of_int 4)));
              assign_instr (Some(V.attsrcMemLoc)) (Integer 64) (Integer 64) addr;
              assign_instr (Some(V.attsrcMemVal)) (Integer 32) (Integer 32) x;
              assign_instr (Some(V.attsrcStateO())) Label Label (BasicBlock b_name) ],
            {b_name;b_instrs}::bl_list

        | hd::tl ->
            let b_instrs, bl_list = split tl in
            (hd::b_instrs, bl_list) in
      let b_instrs, bl_list = split bl.b_instrs in
      let blocks = {b_name=bl.b_name;b_instrs}::bl_list in
      blocks in
    let rec loop = function
      | [] -> []
      | hd::tl ->
          (split_block hd)@(loop tl) in
    loop in
  if not options.debug_load_store then
    f.f_blocks <- split_memory_accesses f.f_blocks

let cfg f =
  let pr_escape s =
    printf "\"";
    for i = 0 to String.length s - 1 do
        let c = s.[i] in
        match c with
        | '\"' -> printf "\\\""
        | '%' -> printf "\\%%"
        | _ -> printf "%c" c
    done;
    printf "\"" in
  let tbl = Hashtbl.create 11 in
  List.iter
    (fun bl ->
      let add_target = function
        | _, BasicBlock target -> Hashtbl.add tbl bl.b_name target
        | _ -> () in
      match List.rev bl.b_instrs with
      | (_,SwitchInst(ty, _::ops))::_ -> (* first arg determines which of remaining args to branch to *)
          List.iter add_target ops
      | (_,BranchInst(_, [target]))::_ -> (* unconditional branch *)
          add_target target
      | (_,BranchInst(_, [_; target; target2]))::_ -> (* conditional branch *)
          add_target target;
          add_target target2;
      | (_,IndirectBrInst(ty, _::ops))::_ -> (* indirect branch computed by first arg, remaining list the possible targets *)
          failwith "Error: indirectbr is unsupported"
      | (_,ReturnInst _)::_ -> ()
      | _ ->
          failwith "Error: block does not end in branch")
    f.f_blocks;
  printf "digraph \"%s\" {\n" (string_of_variable f.f_name);
  (match f.f_blocks with [] -> () | hd::_ -> pr_escape (string_of_variable hd.b_name); printf "\n"); (* root will be layed out in rank 0 *)
  Hashtbl.iter
    (fun source target ->
      pr_escape (string_of_variable source);
      printf " -> ";
      pr_escape (string_of_variable target);
      printf "\n")
    tbl;
  printf "}\n"

let branch_elimination f =
  List.iter
    (fun bl ->
      let elim (nopt,instr) =
        match instr with
        | (SwitchInst(ty, ops)) ->
            (* Ought to deal with switch better here but see gobe.ml instead *)
            [(Some(V.attsrcStateO()), instr)]
        | (BranchInst(ty, ops))
        | (IndirectBrInst(ty, ops)) ->
            [
(*
             (assign_instr (Some(V.attsrcIsDone)) (Integer 1) (Integer 1) (ConstantInt(1, Some Int64.zero)));
             (assign_instr (Some(V.attsrcMemAct)) (Integer 2) (Integer 2) (ConstantInt(2, Some Int64.zero)));
*)
             (match ops with
             | [x;y;z] ->
                 (Some(V.attsrcStateO()), SelectInst(ty,[x;z;y])) (* NB swap order of y,z *)
             | [(ty,op)] ->
                 assign_instr (Some(V.attsrcStateO())) ty ty op
             | _ ->
                 failwith "unanticipated branch type")
           ]
        | (ReturnInst(ty, ops)) ->
            [
             (assign_instr (Some(V.attsrcAnswer)) (Integer 32) (Integer 32) (snd(List.hd ops)));
             (assign_instr (Some(V.attsrcIsDone)) (Integer 1) (Integer 1) (ConstantInt(1, Some Int64.one)))
           ]
        | i -> [(nopt,i)] in
      bl.b_instrs <- List.concat(List.map elim bl.b_instrs))
    f.f_blocks

let optional_print f flag thunk =
  match flag with
  | false -> thunk()
  | true ->
      let b = Buffer.create 11 in
      bpr_function b f;
      pr_output_file "" (Buffer.contents b);
      if not options.delta then exit 0

let run_phases f =
  optional_print f options.prf (fun () ->
    phi_elimination f;
    optional_print f options.phi (fun () ->
      branch_elimination f;
      optional_print f options.branch (fun () ->
        load_store_elimination f;
        optional_print f options.load_store (fun () ->
          gep_elimination f;
          optional_print f options.gep (fun () ->
            State.set_bl_bits (List.length f.f_blocks))))))

let file2module cil_extra_args file =
  if Filename.check_suffix file ".c" then
    (* TODO: clean up temp files in case of error *)
    let src_file =
      if not options.cil then
        file
      else
        (* Run cilly *)
        let cil_basename =
          (* ex: foo/bar.c --> bar.cil.c *)
          Filename.chop_suffix (Filename.basename file) ".c" ^ ".cil.c" in
        let cil_file =
          (*               --> foo/bar.cil.c *)
          Filename.concat (Filename.dirname file) cil_basename in
        let temp_dir =
          Filename.temp_file "smpcc" ".cil" in (* this creates a temp FILE not directory... *)
        Unix.unlink temp_dir;                  (* ... so delete FILE *)
        Unix.mkdir temp_dir 0o700;             (* ... and create DIR with same name *)
        let abs_file =
          if String.length file > 0 && (String.get file 0 = '/' || String.get file 0 = '~')
          then file
          else Filename.concat (Unix.getcwd()) file in
        let ret =
          (* run cilly after changing to temp_dir, so cilly temp files are left there *)
          let cmd =
            sprintf "(cd %s; cilly -c --load=flattener %s --doflattener --none-to-main --save-temps %s)"
              (Filename.quote temp_dir)
              (Filename.quote abs_file)
              (String.concat " " cil_extra_args) in
          Sys.command cmd in
        if ret <> 0 then
          failwith(sprintf "Error: cilly failed with exit code %d" ret);
        (* The cilly command creates a bunch of files, move the one we need to cil_file *)
        let ret =
          (* Use mv instead of Unix.rename because rename fails on cross-device move *)
          Sys.command(sprintf "mv %s %s" (Filename.quote (Filename.concat temp_dir cil_basename)) (Filename.quote cil_file)) in
        if ret <> 0 then
          failwith(sprintf "Error: mv failure %s -> %s" (Filename.quote (Filename.concat temp_dir cil_basename)) (Filename.quote cil_file));
        let ret = Sys.command(sprintf "rm -r %s" (Filename.quote temp_dir)) in
        if ret <> 0 then
          failwith(sprintf "Error: failed to remove temporary directory %s, exit code %d" (Filename.quote temp_dir) ret);
        cil_file in
    (* Run clang *)
    let obj_file = Filename.temp_file "mpcc" ".o" in
    let cmd =
      sprintf "clang -c %s -emit-llvm %s -o %s" options.optflag (Filename.quote src_file) (Filename.quote obj_file) in
    let ret = Sys.command(cmd) in
    if ret <> 0 then
      failwith(sprintf "Error: clang failed with exit code %d" ret);
    (* Obtain clang output *)
    let m = get_module obj_file in
    (Sys.remove obj_file; m)
  else if Filename.check_suffix file ".o" then
    get_module file
  else
    failwith(sprintf "Error: unrecognized file extension (%s)" file)

let getopt opt_string args =
  let (opt,args) = List.partition (fun s -> s=opt_string) args in
  let opt = not(opt=[]) in
  (opt,args)
let getopt1 opt_string =
  let rec loop = function
    | a::b::tl ->
        if a=opt_string then (Some b,tl) else
        let (x,args) = loop (b::tl) in
        (x,a::args)
    | [a] ->
        if a=opt_string then failwith(sprintf "Error: flag %s requires an argument" opt_string)
        else (None,[a])
    | [] -> (None,[]) in
  loop
let getallopts =
  List.partition
    (fun s -> String.length s > 0 && String.get s 0 = '-')
;;
begin
  let args = List.tl (Array.to_list Sys.argv) in
  let (x,args) = getopt "-help" args             in options.help <- x;
  let (x,args) = getopt "-debug-blocks" args     in options.debug_blocks <- x;
  let (x,args) = getopt "-debug-load-store" args in options.debug_load_store <- x;
  let (x,args) = getopt "-no-cil" args           in if x then options.delta <- false;
  let (x,args) = getopt "-delta" args            in options.delta <- x;
  let (x,args) = getopt1 "-circuitlib" args      in options.circuitlib <- x;
  let (x,args) = getopt1 "-fname" args           in options.fname <- x;
  let (x,args) = getopt1 "-o" args               in options.output <- x;
  let (x,args) = getopt "-fv" args               in options.fv <- x;
  let (x,args) = getopt "-pr" args               in options.pr <- x;
  let (x,args) = getopt "-cfg" args              in options.cfg <- x;
  let (x,args) = getopt "-prf" args              in options.prf <- x;
  let (x,args) = getopt "-phi" args              in options.phi <- x;
  let (x,args) = getopt "-branch" args           in options.branch <- x;
  let (x,args) = getopt "-load-store" args       in options.load_store <- x;
  let (x,args) = getopt "-gep" args              in options.gep <- x;
  let (x,args) = getopt "-O0" args               in if x then options.optflag <- "-O0";
  let (x,args) = getopt "-O1" args               in if x then options.optflag <- "-O1";
  let (x,args) = getopt "-O2" args               in if x then options.optflag <- "-O2";
  let (x,args) = getopt "-Os" args               in if x then options.optflag <- "-Os";
  let (x,args) = getopt "-Oz" args               in if x then options.optflag <- "-Oz";
  let (x,args) = getopt "-O3" args               in if x then options.optflag <- "-O3";
  let (x,args) = getopt "-Ofast" args            in if x then options.optflag <- "-Ofast";
  let (x,args) = getopt "-O4" args               in if x then options.optflag <- "-O4";
  let (cil_extra_args,args) = getallopts args    in
  if options.help then
    (printf "Usage: ./mpcc.byte foo.o [options]\n";
     printf "Options: -help                       Print this help message\n";
     printf "         -no-cil                     Do not run cil transformation (flattening)\n";
     printf "         -delta                      Delta printing\n";
     printf "         -circuitlib <lib>           Specify the circuit library (default is yao)\n";
     printf "         -fname <function name>      Specify the function to compile (default is first function)\n";
     printf "         -o <file name>              Specify the output file (default is standard out)\n";
     printf "         -fv                         Print the free variables of the function\n";
     printf "         -ram                        Print the RAM assignment\n";
     printf "         -pr                         Print the LLVM assembly language of the file\n";
     printf "         -prf                        Print the LLVM assembly language of the function\n";
     printf "         -phi                        Stop after phi elimination\n";
     printf "         -branch                     Stop after branch elimination\n";
     printf "         -load-store                 Stop after load-store elimination\n";
     printf "         -gep                        Stop after getelementptr elimination\n";
     printf "         -O0|-O1|-O2|-Os|-Ofast|-O4  Set clang optimization level";
     exit 0)
  else if options.fv then
    List.iter
      (fun file ->
        let m = file2module cil_extra_args file in
        printf "****FREE VARIABLE ANALYSIS**************************************\n";
        List.iter
          (fun f ->
            let (gl,f_name,_) = f.f_name in
            printf "Function %s()\n" (if gl then "@"^f_name else "%"^f_name);
            List.iter
              (fun bl ->
                let (_,b_name,_) = bl.b_name in
                printf " Free in block %s:" b_name;
                let fv = free_of_block bl in
                VSet.iter (fun var -> printf " %s" (string_of_variable var)) fv;
                printf "\n")
              f.f_blocks)
          m.functions)
      args
  else if options.pr then
    List.iter
      (fun file ->
        let m = file2module cil_extra_args file in
        let b = Buffer.create 11 in
        bpr_module b m;
        printf "%s" (Buffer.contents b))
      args
  else if options.cfg then
    List.iter
      (fun file ->
        let m = file2module cil_extra_args file in
        cfg (List.hd m.functions))
      args
  else
    let file = List.hd args in
    let m = file2module cil_extra_args file in
    if options.output = None then
      options.output <- Some(Filename.basename (Filename.chop_suffix file ".c"));
    let f =
      (match options.fname with
      | None ->
          List.hd(m.functions)
      | Some fname ->
          List.hd(List.filter
                    (fun f ->
                      let (_,f_name,_) = f.f_name in
                      f_name = fname)
                    m.functions)) in
    if not options.delta then begin
      run_phases f;
      Gobe.print_function_circuit m f
    end
    else begin
      let toggle_before =
        if options.branch then
          (options.prf <- true; (fun () -> options.prf <- false))
        else if options.phi then
          (options.branch <- true; (fun () -> options.branch <- false))
        else if options.load_store then
          (options.phi <- true; (fun () -> options.phi <- false))
        else if options.gep then
          (options.load_store <- true; (fun () -> options.load_store <- false))
        else (fun () -> ()) in
      let file_before = Filename.temp_file "mpcc" ".diff" in
      options.output <- Some file_before;
      run_phases f;
      toggle_before();
      let file_after = Filename.temp_file "mpcc" ".diff" in
      options.output <- Some file_after;
      run_phases f;
      ignore(Sys.command(sprintf "colordiff -U -1 %s %s | more" file_before file_after));
      Sys.remove file_before;
      Sys.remove file_after;
      ()
    end
end
