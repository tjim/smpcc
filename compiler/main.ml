(* mpcc: compile LLVM object file to circuit *)

(* Compile this program with
 *
 *     ocamlbuild mpcc.byte
 *
 *)

open Util
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

let rec repl_typ_value var replacement (typ,value) =
  (typ, repl_value var replacement value)
and repl_value var replacement x =
  let rv = repl_value var replacement in
  let rtv (typ,value) = (typ, rv value) in
  match x with
  | Var var2                   -> if var=var2 then replacement else x
  | Basicblock var2            -> if var=var2 then replacement else x
  | Mdnode _                   -> x
  | Mdstring _                 -> x
  | Null                       -> x
  | Undef                      -> x
  | Zero                       -> x
  | True                       -> x
  | False                      -> x
  | Int _                      -> x
  | Float _                    -> x
  | Asm _                      -> x
  | Mdnodevector l             -> Mdnodevector (List.map (function None -> None | Some(typ, value) -> Some(typ, rv value)) l)
  | Blockaddress(x, y)         -> Blockaddress(rv x, rv y)
  | Array ops                  -> Array(List.map rtv ops)
  | Vector ops                 -> Vector(List.map rtv ops)
  | Struct(is_packed, ops)     -> Struct(is_packed, List.map rtv ops)
  | Trunc(x, y)                -> Trunc(rtv x, y)
  | Zext(x, y)                 -> Zext(rtv x, y)         
  | Sext(x, y)                 -> Sext(rtv x, y)         
  | Fptrunc(x, y)              -> Fptrunc(rtv x, y)      
  | Fpext(x, y)                -> Fpext(rtv x, y)        
  | Bitcast(x, y)              -> Bitcast(rtv x, y)      
  | Addrspacecast(x, y)        -> Addrspacecast(rtv x, y)
  | Uitofp(x, y)               -> Uitofp(rtv x, y)       
  | Sitofp(x, y)               -> Sitofp(rtv x, y)       
  | Fptoui(x, y)               -> Fptoui(rtv x, y)       
  | Fptosi(x, y)               -> Fptosi(rtv x, y)       
  | Inttoptr(x, y)             -> Inttoptr(rtv x, y)     
  | Ptrtoint(x, y)             -> Ptrtoint(rtv x, y)     
  | Extractvalue(x, y)         -> Extractvalue(rtv x, y)
  | Insertvalue(x, y, z)       -> Insertvalue(rtv x, rtv y, z)
  | Icmp(cmp, x, y)            -> Icmp(cmp, rtv x, rtv y)
  | Fcmp(cmp, x, y)            -> Fcmp(cmp, rtv x, rtv y)
  | Sdiv(e, x, y)              -> Sdiv(e, rtv x, rtv y)  
  | Udiv(e, x, y)              -> Udiv(e, rtv x, rtv y)  
  | Lshr(e, x, y)              -> Lshr(e, rtv x, rtv y)  
  | Ashr(e, x, y)              -> Ashr(e, rtv x, rtv y)  
  | Fadd(x, y)                 -> Fadd(rtv x, rtv y)     
  | Fsub(x, y)                 -> Fsub(rtv x, rtv y)     
  | Fmul(x, y)                 -> Fmul(rtv x, rtv y)     
  | Fdiv(x, y)                 -> Fdiv(rtv x, rtv y)     
  | Frem(x, y)                 -> Frem(rtv x, rtv y)     
  | Urem(x, y)                 -> Urem(rtv x, rtv y)     
  | Srem(x, y)                 -> Srem(rtv x, rtv y)     
  | And (x, y)                 -> And (rtv x, rtv y)     
  | Or  (x, y)                 -> Or  (rtv x, rtv y)     
  | Xor (x, y)                 -> Xor (rtv x, rtv y)     
  | Getelementptr(inbounds, x) -> Getelementptr(inbounds, List.map rtv x)
  | Shufflevector x            -> Shufflevector(List.map rtv x)
  | Insertelement x            -> Insertelement(List.map rtv x) 
  | Extractelement x           -> Extractelement(List.map rtv x)
  | Select x                   -> Select(List.map rtv x)
  | Add(nuw, nsw, x, y)        -> Add(nuw, nsw, rtv x, rtv y)
  | Sub(nuw, nsw, x, y)        -> Sub(nuw, nsw, rtv x, rtv y)
  | Mul(nuw, nsw, x, y)        -> Mul(nuw, nsw, rtv x, rtv y)
  | Shl(nuw, nsw, x, y)        -> Shl(nuw, nsw, rtv x, rtv y)
and repl_instr var replacement x =
  let rv = repl_value var replacement in
  let rtv = repl_typ_value var replacement in
  let rtvl = List.map (repl_typ_value var replacement) in
  match x with 
  | Add(nuw, nsw, x, y) -> Add(nuw, nsw, rtv x, rv y)
  | Sub(nuw, nsw, x, y) -> Sub(nuw, nsw, rtv x, rv y)
  | Mul(nuw, nsw, x, y) -> Mul(nuw, nsw, rtv x, rv y)
  | Shl(nuw, nsw, x, y) -> Shl(nuw, nsw, rtv x, rv y)
  | Fadd(fmf, x, y)           -> Fadd(fmf, rtv x, rv y)
  | Fsub(fmf, x, y)           -> Fsub(fmf, rtv x, rv y)
  | Fmul(fmf, x, y)           -> Fmul(fmf, rtv x, rv y)
  | Fdiv(fmf, x, y)           -> Fdiv(fmf, rtv x, rv y)
  | Frem(fmf, x, y)           -> Frem(fmf, rtv x, rv y)
  | Sdiv(e, x, y)           -> Sdiv(e, rtv x, rv y)
  | Udiv(e, x, y)           -> Udiv(e, rtv x, rv y)
  | Lshr(e, x, y)           -> Lshr(e, rtv x, rv y)
  | Ashr(e, x, y)           -> Ashr(e, rtv x, rv y)
  | Urem(x, y)           -> Urem(rtv x, rv y)
  | Srem(x, y)           -> Srem(rtv x, rv y)
  | And (x, y)           -> And (rtv x, rv y)
  | Or  (x, y)           -> Or  (rtv x, rv y)
  | Xor (x, y)           -> Xor (rtv x, rv y)
  | Icmp(icmp, x, y) -> Icmp(icmp, rtv x, rv y)
  | Fcmp(fcmp, x, y) -> Fcmp(fcmp, rtv x, rv y)
  | Trunc(x, y)          -> Trunc(rtv x, y)        
  | Zext(x, y)           -> Zext(rtv x, y)         
  | Sext(x, y)           -> Sext(rtv x, y)         
  | Fptrunc(x, y)        -> Fptrunc(rtv x, y)      
  | Fpext(x, y)          -> Fpext(rtv x, y)        
  | Bitcast(x, y)        -> Bitcast(rtv x, y)      
  | Addrspacecast(x, y)  -> Addrspacecast(rtv x, y)
  | Uitofp(x, y)         -> Uitofp(rtv x, y)       
  | Sitofp(x, y)         -> Sitofp(rtv x, y)       
  | Fptoui(x, y)         -> Fptoui(rtv x, y)       
  | Fptosi(x, y)         -> Fptosi(rtv x, y)       
  | Inttoptr(x, y)       -> Inttoptr(rtv x, y)     
  | Ptrtoint(x, y)       -> Ptrtoint(rtv x, y)     
  | Va_arg(x, y)         -> Va_arg(rtv x, y)       
  | Getelementptr(inbounds, x) -> Getelementptr(inbounds, x)
  | Shufflevector x -> Shufflevector(rtvl x)
  | Insertelement x -> Insertelement(rtvl x)
  | Extractelement x -> Extractelement(rtvl x)
  | Select x -> Select(rtvl x)
  | Phi(ty, incoming) -> Phi(ty,
                             List.map (fun (v1,v2) -> (rv v1, rv v2)) incoming)
  | Landingpad(x, y, z, w) ->
      Landingpad(x, rtv y, z,
                 List.map (function
                   | Catch(typ, value) -> Catch(typ, rv value)
                   | Filter(typ, value) ->Filter(typ, rv value)) w)
  | Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs) ->
      Call(is_tail_call, callconv, retattrs, callee_ty, rv callee_name,
           List.map (fun (typ,attrs,value) -> (typ,attrs,rv value)) operands,
           callattrs)
  | Alloca(x, y, z, w) ->
      Alloca(x, y,
             (match z with None -> None | Some q -> Some(rtv q)),
             w)
  | Load(x, y, z, w, v) ->
      Load(x, y, rtv z, w, v)
  | Store(x, y, z, w, v, u) ->
      Store(x, y, rtv z, rtv w, v, u)
  | Cmpxchg(x, y, z, w, v, u, t) ->
      Cmpxchg(x, rtv y, rtv z, rtv w, v, u, t)
  | Atomicrmw(x, y, z, w, v, u) ->
      Atomicrmw(x, y, rtv z, rtv w, v, u)
  | Fence(x, y) -> Fence(x, y)
  | Extractvalue(x, y) ->
      Extractvalue(rtv x, y)
  | Insertvalue(x, y, z) ->
      Insertvalue(rtv x, rtv y, z)
  | Unreachable ->
      Unreachable
  | Return None ->
      Return None
  | Return(Some tv) ->
      Return(Some(rtv tv))
  | Br(x, None) ->
      Br(rtv x, None)
  | Br(x, Some(y, z)) ->
      Br(rtv x, Some(rtv y, rtv z))
  | Indirectbr(x, y) ->
      Indirectbr(rtv x, rtvl y)
  | Resume x ->
      Resume(rtv x)
  | Switch(x, y, z) ->
      Switch(rtv x, rtv y, List.map (fun (a,b) -> (rtv a, rtv b)) z)
  | Invoke(x, y, z, w, v, u, t, s) ->
      Invoke(x, y, z, rv w, List.map (fun (typ,attrs,value) -> (typ,attrs,rv value)) v, u, rtv t, rtv s)

let repl_instrs var replacement instrs =
  let rec loop = function
    | [] -> []
    | (nopt,instr)::tl ->
        let replaced = repl_instr var replacement instr in
        (match nopt with
        | None -> (nopt, replaced)::(loop tl)
        | Some x ->
            if x=var then (Some x,replaced)::tl else
            (Some x, replaced)::(loop tl)) in
  loop instrs

let assign_instr n result_ty ty op =
  (Some n, Inttoptr((ty, op), result_ty)) (* unlike LLVM we will not force result_ty to be a ptr or ty to be an int typ *)

(*
  bl_target has var = phi (value,bl_source) --> record and remove
  record means
    tbl[(bl_target, bl_source)] = bl_fresh, [(var,value)]

  each (bl_target, bl_source) -->
    fresh block bl_fresh
    replace bl_target by bl_fresh in bl_source
  build bl_fresh
*)

let unopt = function
  | None -> failwith "unopt"
  | Some x -> x

let phi_elimination f =
  let bl_assoc : (var * binfo) list =
    (* maps block name to block *)
    List.map (fun bl -> bl.bname, bl) f.fblocks in
  let tbl : ((var * var), (var * (var * value) list ref)) Hashtbl.t =
    (* maps a (target_block_name, source_block_name) to (fresh_block_name, assignments) *)
    Hashtbl.create 11 in
  (* Gather PHI information *)
  List.iter
    (fun bl ->
      List.iter
        (function
          | (Some var,Phi(_, incoming)) ->
              List.iter
                (fun (value,source_block) ->
                  let source_block_name = (match source_block with Var v -> v | Basicblock v -> v | _ -> failwith "phi elimination") in
                  let (_, assignments) =
                    if not(Hashtbl.mem tbl (bl.bname, source_block_name)) then
                      Hashtbl.add tbl (bl.bname, source_block_name) (State.fresh_label(), ref []);
                    Hashtbl.find tbl (bl.bname, source_block_name) in
                  assignments := (var,value)::!assignments)
                incoming
          | (None, Phi _) -> failwith "Error: PHINode without variable"
          | _ -> ())
        bl.binstrs)
    f.fblocks;
  (* Remove PHI nodes *)
  List.iter
    (fun bl ->
      let non_phi =
        List.filter
          (function (_,Phi(_, _)) -> false
            | _ -> true)
          bl.binstrs in
      bl.binstrs <- non_phi)
    f.fblocks;
  (* Retarget branches *)
  Hashtbl.iter
    (fun (target_block_name, source_block_name) (fresh_block_name, _) ->
      let source_bl =
        try List.assoc source_block_name bl_assoc
        with _ -> failwith "Error: PHINode source does not exist" in
      source_bl.binstrs <- repl_instrs target_block_name (Basicblock fresh_block_name) source_bl.binstrs)
    tbl;
  (* Add the new blocks *)
  let new_blocks = ref [] in
  Hashtbl.iter
    (fun (target_block_name, _) (fresh_block_name, assignments) ->
      let bname = fresh_block_name in
      let before_and_afters =
        List.map
          (fun (var, value) ->
            let ty = State.typ_of_var var in
            let temp_var = Name(false, State.fresh()) in
            let before = assign_instr temp_var ty ty value in
            let after = assign_instr var ty ty (Var temp_var) in
            (before,after))
          !assignments in
      let (befores, afters) = List.split before_and_afters in
      let branch = (None, Br((Label, Basicblock target_block_name), None)) in
      let binstrs = befores @ afters @ [branch] in
      new_blocks := {bname;binstrs}::!new_blocks)
    tbl;
  f.fblocks <- f.fblocks @ !new_blocks;
  ()

(* Replace getelementptr by arithmetic *)
(* http://www.llvm.org/docs/GetElementPtr.html *)
let gep_elimination f =
  let elim = function
    | Some n, Getelementptr(_,(Pointer(ety,aspace),x)::tl) ->
        let ety = Arraytyp(1,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
        let rec loop x ety = function
          | [] -> [assign_instr n (Pointer(ety,aspace)) (Pointer(ety,aspace)) x]
          | (yty,y)::tl ->
              (match ety,y with
              | Arraytyp(_,ety'),Int(i) -> (* NB we ignore the bitwidth of the constant *)
                  (* invariant: yty = i64 *)
                  let b = Int(Big_int.mult_big_int i (Big_int.big_int_of_int(bytewidth ety'))) in
                  let new_x = Name(false,State.fresh()) in (* typ is Pointer(aspace,ety') *)
                  (Some(new_x),Add(false, false, (Integer 64,b), x))
                  ::(loop (Var new_x) ety' tl)
              | Arraytyp(_,ety'),Var _ ->
                  (* invariant: yty = i64 *)
                  let b = Name(false,State.fresh()) in
                  let new_x = Name(false,State.fresh()) in (* type is Pointer(ety',aspace) *)
                  (Some(b),Mul(false, false, (Integer 64,y), Int(Big_int.big_int_of_int(bytewidth ety'))))
                  ::(Some(new_x),Add(false, false, (Pointer(ety',aspace),x), Var b))
                  ::(loop (Var new_x) ety' tl)
              | Structtyp(_),Int(i) ->
                  (* TEMPORARY HACK *)
                  let b = Int(Big_int.mult_big_int i (Big_int.big_int_of_int 4)) in
                  let new_x = Name(false,State.fresh()) in (* type is Pointer(Integer 32, None) *)
                  (Some(new_x),Add(false, false, (Pointer(Integer 32, None), x), b))
                  ::(loop (Var new_x) (Integer 32) tl)
                  (* failwith "TODO: struct in getelementptr" *)
              | _ ->
                  let ty_string = let b = Buffer.create 11 in bpr_typ b ety; Buffer.contents b in
                  let y_string = let b = Buffer.create 11 in bpr_value b y; Buffer.contents b in
                  failwith (sprintf "getelementptr: %s, %s" ty_string y_string)) in
        loop x ety tl
    | (nopt,i) -> [(nopt,i)] in
  (f.fblocks <-
    (List.map
      (fun bl ->
        { bname = bl.bname;
          binstrs = List.concat (List.map elim bl.binstrs);
        })
      f.fblocks));
  ()

let big d = Int(Big_int.big_int_of_int d)

(* If a block contains a load or store, we split the block into two.  The second block should
   expect the result of the load/store in attsrcMemRes. *)
let load_store_elimination f =
  let split_memory_accesses =
    let split_block bl =
      let rec split = function
        | [] ->
            ([],[])
        | (Some nopt,Load(_,_,(Pointer(result_ty,aspace),x),_,_))::tl ->
            (*TODO:alignment*)
            let bname = State.fresh_label() in
            let binstrs, bl_list = split tl in
            let binstrs =
              (assign_instr nopt result_ty (Integer 64) (Var V.attsrcMemRes))::binstrs in
            [
(*              assign_instr (Some(V.attsrcIsDone)) (Integer 1) (Integer 1) (ConstantInt(1, Some Int64.zero));*)
              assign_instr V.attsrcMemAct (Integer 2) (Integer 2) (big 1);
              assign_instr V.attsrcNumElts (Integer 32) (Integer 32) (big 1);
(*FIX: look at type for MemSize, on assign_instr you might have to select subset of bits*)
              assign_instr V.attsrcMemSize (Integer 32) (Integer 32) (big 4);
              assign_instr V.attsrcMemLoc (Integer 64) (Integer 64) x;
              assign_instr (V.attsrcStateO()) Label Label (Basicblock bname) ],
            {bname;binstrs}::bl_list
        | (Some nopt,Store(_,_,(_,x),(_,addr),_,_))::tl ->
            (*TODO:alignment*)
            let bname = State.fresh_label() in
            let binstrs, bl_list = split tl in
            [ assign_instr V.attsrcIsDone (Integer 1) (Integer 1) (big 0);
              assign_instr V.attsrcMemAct (Integer 2) (Integer 2) (big 2);
              assign_instr V.attsrcNumElts (Integer 32) (Integer 32) (big 1);
              assign_instr V.attsrcMemSize (Integer 32) (Integer 32) (big 4);
              assign_instr V.attsrcMemLoc (Integer 64) (Integer 64) addr;
              assign_instr V.attsrcMemVal (Integer 32) (Integer 32) x;
              assign_instr (V.attsrcStateO()) Label Label (Basicblock bname) ],
            {bname;binstrs}::bl_list
        | hd::tl ->
            let binstrs, bl_list = split tl in
            (hd::binstrs, bl_list) in
      let binstrs, bl_list = split bl.binstrs in
      let blocks = {bname=bl.bname;binstrs}::bl_list in
      blocks in
    let rec loop = function
      | [] -> []
      | hd::tl ->
          (split_block hd)@(loop tl) in
    loop in
  if not options.debug_load_store then
    f.fblocks <- split_memory_accesses f.fblocks

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
        | _, Basicblock target -> Hashtbl.add tbl bl.bname target
        | _ -> () in
      match List.rev bl.binstrs with
      | (_,Switch(_,_,ops))::_ -> (* first arg determines which of remaining args to branch to *)
          List.iter add_target (List.map snd ops)
      | (_,Br(target, None))::_ -> (* unconditional branch *)
          add_target target
      | (_,Br(_, Some(target, target2)))::_ -> (* conditional branch *)
          add_target target;
          add_target target2;
      | (_,Indirectbr(_, ops))::_ -> (* indirect branch computed by first arg, remaining list the possible targets *)
          failwith "Error: indirectbr is unsupported"
      | (_,Return _)::_ -> ()
      | _ ->
          failwith "Error: block does not end in branch")
    f.fblocks;
  printf "digraph \"%s\" {\n" (string_of_var f.fname);
  (match f.fblocks with [] -> () | hd::_ -> pr_escape (string_of_var hd.bname); printf "\n"); (* root will be layed out in rank 0 *)
  Hashtbl.iter
    (fun source target ->
      pr_escape (string_of_var source);
      printf " -> ";
      pr_escape (string_of_var target);
      printf "\n")
    tbl;
  printf "}\n"

let branch_elimination f =
  List.iter
    (fun bl ->
      let elim (nopt,instr) =
        match instr with
        | Switch _ ->
            (* Ought to deal with switch better here but see gobe.ml instead *)
            [(Some(V.attsrcStateO()), instr)]
        | Br((ty,op), None) ->
            [assign_instr (V.attsrcStateO()) ty ty op]
        | Br(x, Some(y,z)) ->
            [Some(V.attsrcStateO()), Select([x;y;z])] (* NB do NOT swap order of y,z *)
        | Indirectbr _ ->
            failwith "branch elimination: indirectbr is unsupported"
        | Return None ->
            [(assign_instr V.attsrcIsDone (Integer 1) (Integer 1) (big 1))]
        | Return(Some(ty, v)) ->
            [
             (assign_instr V.attsrcAnswer (Integer 32) ty v);
             (assign_instr V.attsrcIsDone (Integer 1) (Integer 1) (big 1))
           ]
        | i -> [(nopt,i)] in
      bl.binstrs <- List.concat(List.map elim bl.binstrs))
    f.fblocks

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
            State.set_bl_bits (List.length f.fblocks))))))

let file2cu cil_extra_args file =
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
    let ll_file = Filename.temp_file "mpcc" ".ll" in
    let cmd =
      sprintf "clang -S %s -emit-llvm %s -o %s" options.optflag (Filename.quote src_file) (Filename.quote ll_file) in
    printf "running: %s\n" cmd;
    let ret = Sys.command(cmd) in
    if ret <> 0 then
      failwith(sprintf "Error: clang failed with exit code %d" ret);
    (* Obtain clang output *)
    let ch = open_in ll_file in
    let cu =
      try
        Lllex.parse ch
      with e ->
        close_in ch;
        Sys.remove ll_file;
        raise e in
    close_in ch;
    Sys.remove ll_file;
    cu
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
  let (x,args) = getopt "-no-cil" args           in options.cil <- not x;
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
    (printf "Usage: ./smpcc foo.c [options]\n";
     printf "Options: -help                       Print this help message\n";
     printf "         -debug-blocks               Print the active block during execution\n";
     printf "         -debug-load-store           Execute loads and stores inside blocks (without splitting)\n";
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
     printf "         -O0|-O1|-O2|-Os|-Ofast|-O4  Set clang optimization level\n";
     exit 0)
  else if options.fv then
    List.iter
      (fun file ->
        let m = file2cu cil_extra_args file in
        printf "****FREE VARIABLE ANALYSIS**************************************\n";
        List.iter
          (fun (f:finfo) ->
            printf "Function %s()\n" (string_of_var f.fname);
            List.iter
              (fun bl ->
                printf " Free in block %s:" (string_of_var bl.bname);
                let fv = free_of_block bl in
                VSet.iter (fun var -> printf " %s" (string_of_var var)) fv;
                printf "\n")
              f.fblocks)
          m.cfuns)
      args
  else if options.pr then
    List.iter
      (fun file ->
        let m = file2cu cil_extra_args file in
        let b = Buffer.create 11 in
        bpr_cu b m;
        printf "%s" (Buffer.contents b))
      args
  else if options.cfg then
    List.iter
      (fun file ->
        let m = file2cu cil_extra_args file in
        cfg (List.hd m.cfuns))
      args
  else
    let file = List.hd args in
    let m = file2cu cil_extra_args file in
    if options.output = None then
      options.output <- Some(Filename.basename (Filename.chop_suffix file ".c"));
    let f =
      (match options.fname with
      | None ->
          List.hd(m.cfuns)
      | Some fname ->
          List.hd(List.filter
                    (fun (f:finfo) -> (string_of_var f.fname) = fname)
                    m.cfuns)) in
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
