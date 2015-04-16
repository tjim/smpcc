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
  the current block; results of all other blocks are discarded.

  The current block is maintained in unary form: boolean variables
  block0, block1, ..., exactly one of which is true at any step.

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

  Phi elimination is also called "back translation" or the
  "out-of-SSA" transform.  A better algorithm is given in

    Vugranam C. Sreedhar, Roy Dz-Ching Ju, David M. Gillies, and Vatsa
    Santhanam. Translating out of static single assignment form. In
    Proceedings of the Static Analysis Symposium, volume 1694 of
    Lecture Notes in Computer Science, pages 194-210, 1999.
    (http://www.tjhsst.edu/~rlatimer/papers/sreedharTranslatingOutOfStaticSingleAssignmentForm.pdf)

  LOAD/STORE ELIMINATION

  For each llvm block, split any block that has a memory access into two blocks.
  The second block should expect the result of the memory access in vMemRes.
  So for example

      %1 = load %2
      ...

  becomes

      %vMemAct = 2  // load
      %vMemSize = 4 // ... of 4 bytes
      %vMemLoc = %2 // ... from memory at location %2
      br label %9 // ... at label %9
      // end of old block

      // new block
      %9:
      %1 = %vMemRes
      ...

  BRANCH ELIMINATION

  We transform

      br i1 %1, label %split13, label %.lr.ph

  into

      %blockX = 0
      %blockY = %1
      %blockZ = xor i1 %1, 1

  where %blockX variable for the current block (containing the br instruction),
  %blockY is the variable for the block with label %split13,
  %blockZ is the variable for the block with label %.lr.ph.

  Note that X might be Y or Z; so the output of branch elimination can
  include multiple assignments in a block.  This must be handled
  properly in our eventual conversion to the back end.

  There is a similar transformation for switch instructions, however,
  it is much more complicated.  Switch requires us to convert a binary
  value into our unary representation.

  We don't support indirectbr for the moment (it computes a label and
  branches to it).

  We transform

      return i32 0

  into

      %vAnswer = i32 0
      %vIsDone = i1 1

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
  | Add(nuw, nsw, x, y, md) -> Add(nuw, nsw, rtv x, rv y, md)
  | Sub(nuw, nsw, x, y, md) -> Sub(nuw, nsw, rtv x, rv y, md)
  | Mul(nuw, nsw, x, y, md) -> Mul(nuw, nsw, rtv x, rv y, md)
  | Shl(nuw, nsw, x, y, md) -> Shl(nuw, nsw, rtv x, rv y, md)
  | Fadd(fmf, x, y, md)           -> Fadd(fmf, rtv x, rv y, md)
  | Fsub(fmf, x, y, md)           -> Fsub(fmf, rtv x, rv y, md)
  | Fmul(fmf, x, y, md)           -> Fmul(fmf, rtv x, rv y, md)
  | Fdiv(fmf, x, y, md)           -> Fdiv(fmf, rtv x, rv y, md)
  | Frem(fmf, x, y, md)           -> Frem(fmf, rtv x, rv y, md)
  | Sdiv(e, x, y, md)           -> Sdiv(e, rtv x, rv y, md)
  | Udiv(e, x, y, md)           -> Udiv(e, rtv x, rv y, md)
  | Lshr(e, x, y, md)           -> Lshr(e, rtv x, rv y, md)
  | Ashr(e, x, y, md)           -> Ashr(e, rtv x, rv y, md)
  | Urem(x, y, md)           -> Urem(rtv x, rv y, md)
  | Srem(x, y, md)           -> Srem(rtv x, rv y, md)
  | And (x, y, md)           -> And (rtv x, rv y, md)
  | Or  (x, y, md)           -> Or  (rtv x, rv y, md)
  | Xor (x, y, md)           -> Xor (rtv x, rv y, md)
  | Icmp(icmp, x, y, md) -> Icmp(icmp, rtv x, rv y, md)
  | Fcmp(fcmp, x, y, md) -> Fcmp(fcmp, rtv x, rv y, md)
  | Trunc(x, y, md)          -> Trunc(rtv x, y, md)
  | Zext(x, y, md)           -> Zext(rtv x, y, md)
  | Sext(x, y, md)           -> Sext(rtv x, y, md)
  | Fptrunc(x, y, md)        -> Fptrunc(rtv x, y, md)
  | Fpext(x, y, md)          -> Fpext(rtv x, y, md)
  | Bitcast(x, y, md)        -> Bitcast(rtv x, y, md)
  | Addrspacecast(x, y, md)  -> Addrspacecast(rtv x, y, md)
  | Uitofp(x, y, md)         -> Uitofp(rtv x, y, md)
  | Sitofp(x, y, md)         -> Sitofp(rtv x, y, md)
  | Fptoui(x, y, md)         -> Fptoui(rtv x, y, md)
  | Fptosi(x, y, md)         -> Fptosi(rtv x, y, md)
  | Inttoptr(x, y, md)       -> Inttoptr(rtv x, y, md)
  | Ptrtoint(x, y, md)       -> Ptrtoint(rtv x, y, md)
  | Va_arg(x, y, md)         -> Va_arg(rtv x, y, md)
  | Getelementptr(inbounds, x, md) -> Getelementptr(inbounds, x, md)
  | Shufflevector(x,md) -> Shufflevector(rtvl x, md)
  | Insertelement(x,md) -> Insertelement(rtvl x, md)
  | Extractelement(x,md) -> Extractelement(rtvl x, md)
  | Select(x,md) -> Select(rtvl x, md)
  | Phi(ty, incoming, md) -> Phi(ty,
                             List.map (fun (v1,v2) -> (rv v1, rv v2)) incoming, md)
  | Landingpad(x, y, z, w, md) ->
      Landingpad(x, rtv y, z,
                 List.map (function
                   | Catch(typ, value) -> Catch(typ, rv value)
                   | Filter(typ, value) ->Filter(typ, rv value)) w,
                md)
  | Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs, md) ->
      Call(is_tail_call, callconv, retattrs, callee_ty, rv callee_name,
           List.map (fun (typ,attrs,value) -> (typ,attrs,rv value)) operands,
           callattrs, md)
  | Alloca(x, y, z, w, md) ->
      Alloca(x, y,
             (match z with None -> None | Some q -> Some(rtv q)),
             w, md)
  | Load(x, y, z, w, v, md) ->
      Load(x, y, rtv z, w, v, md)
  | Store(x, y, z, w, v, u, md) ->
      Store(x, y, rtv z, rtv w, v, u, md)
  | Cmpxchg(x, y, z, w, v, u, t, md) ->
      Cmpxchg(x, rtv y, rtv z, rtv w, v, u, t, md)
  | Atomicrmw(x, y, z, w, v, u, md) ->
      Atomicrmw(x, y, rtv z, rtv w, v, u, md)
  | Fence(x, y, md) -> Fence(x, y, md)
  | Extractvalue(x, y, md) ->
      Extractvalue(rtv x, y, md)
  | Insertvalue(x, y, z, md) ->
      Insertvalue(rtv x, rtv y, z, md)
  | Unreachable md ->
      Unreachable md
  | Return(None, md) ->
      Return(None, md)
  | Return(Some tv, md) ->
      Return(Some(rtv tv), md)
  | Br(x, None, md) ->
      Br(rtv x, None, md)
  | Br(x, Some(y, z), md) ->
      Br(rtv x, Some(rtv y, rtv z), md)
  | Indirectbr(x, y, md) ->
      Indirectbr(rtv x, rtvl y, md)
  | Resume(x, md) ->
      Resume(rtv x, md)
  | Switch(x, y, z, md) ->
      Switch(rtv x, rtv y, List.map (fun (a,b) -> (rtv a, rtv b)) z, md)
  | Invoke(x, y, z, w, v, u, t, s, md) ->
      Invoke(x, y, z, rv w, List.map (fun (typ,attrs,value) -> (typ,attrs,rv value)) v, u, rtv t, rtv s, md)

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
  (Some n, Inttoptr((ty, op), result_ty, [])) (* unlike LLVM we will not force result_ty to be a ptr or ty to be an int typ *)

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
          | (Some var,Phi(_, incoming, _)) ->
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
          (function (_,Phi(_, _, _)) -> false
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
      let branch = (None, Br((Label, Basicblock target_block_name), None, [])) in
      let binstrs = befores @ afters @ [branch] in
      new_blocks := {bname;binstrs}::!new_blocks)
    tbl;
  f.fblocks <- f.fblocks @ !new_blocks;
  ()

(* Replace getelementptr by arithmetic *)
let gep_elim_value =
  value_map (function
    | Getelementptr(_, (Pointer(ety,s), Var v)::tl) as c ->
        if not(Hashtbl.mem State.global_locations v) then
          (eprintf "Warning: unable to eliminate '%s'\n" (spr bpr_value c); c)
        else begin
          let ety = Arraytyp(0,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
          let rec loop ety = function
            | [] -> Big_int.big_int_of_int(Hashtbl.find State.global_locations v)
            | (y_typ,y)::tl ->
                (match ety,y with
                | Arraytyp(_,ety'),(Int(i)) -> (* NB we ignore the bitwidth of the constant *)
                    Big_int.add_big_int
                      (Big_int.mult_big_int i (Big_int.big_int_of_int(State.bytewidth ety')))
                      (loop ety' tl)
                | Structtyp(_,typs),(Int(i)) ->
                    let ety' = List.nth typs (Big_int.int_of_big_int i) in
                    let skip_bytes =
                      let rec skip n = function
                        | [] -> failwith "getelementptr: insufficient fields"
                        | hd::tl -> if n = 0 then 0 else State.bytewidth hd + skip (n-1) tl in
                      skip (Big_int.int_of_big_int i) typs in
                    Big_int.add_big_int
                      (Big_int.big_int_of_int skip_bytes)
                      (loop ety' tl)
                | Vartyp vty, _ ->
                    loop (State.typ_of_var vty) ((y_typ,y)::tl)
                | _ ->
                    let b = Buffer.create 11 in
                    bprintf b "gep_elim_value error: type is %a, value is %a\n" bpr_typ ety bpr_value y;
                    eprintf "%s" (Buffer.contents b);
                    failwith "gep_elim_value") in
          let sum = loop ety tl in
          Int sum
        end
    | c -> c)

(* http://www.llvm.org/docs/GetElementPtr.html *)
let gep_elimination ctyps f =
  gep_elim_value f;
  let elim (nopt, i) = match (nopt, i) with
    | Some n, Getelementptr(_,(Pointer(ety,aspace),x)::tl,_) ->
        let buf = Buffer.create 11 in
        let ety = Arraytyp(1,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
        let rec loop x ety = function
          | [] -> [assign_instr n (Pointer(ety,aspace)) (Pointer(ety,aspace)) x]
          | (yty,y)::tl ->
              bprintf buf "(%a, %a)\n" bpr_typ yty bpr_value y;
              (match ety,y with
              | Arraytyp(_,ety'),Int(i) -> (* NB we ignore the bitwidth of the constant *)
                  (* invariant: yty = i64 *)
                  let b = Int(Big_int.mult_big_int i (Big_int.big_int_of_int(State.bytewidth ety'))) in
                  let new_x = Name(false,State.fresh()) in (* typ is Pointer(aspace,ety') *)
                  (Some(new_x),Add(false, false, (Integer 64,b), x, []))
                  ::(loop (Var new_x) ety' tl)
              | Arraytyp(_,ety'),Var _ ->
                  (* invariant: yty = i64 *)
                  let b = Name(false,State.fresh()) in
                  let new_x = Name(false,State.fresh()) in (* type is Pointer(ety',aspace) *)
                  (Some(b),Mul(false, false, (Integer 64,y), Int(Big_int.big_int_of_int(State.bytewidth ety')), []))
                  ::(Some(new_x),Add(false, false, (Pointer(ety',aspace),x), Var b, []))
                  ::(loop (Var new_x) ety' tl)
              | Structtyp(_,typs),Int(i) ->
                  let ety' = List.nth typs (Big_int.int_of_big_int i) in
                  let skip_bytes =
                    let rec skip n = function
                      | [] -> failwith "getelementptr: insufficient fields"
                      | hd::tl -> if n = 0 then 0 else State.bytewidth hd + skip (n-1) tl in
                    skip (Big_int.int_of_big_int i) typs in
                  let new_x = Name(false,State.fresh()) in
                  (Some(new_x),Add(false, false, (Pointer(ety', None), x), Int(Big_int.big_int_of_int skip_bytes), []))
                  ::(loop (Var new_x) ety' tl)
              | Vartyp v, _ ->
                  let ety' =
                    try
                      match List.assoc v ctyps with
                      | None -> failwith ("getelementptr: opaque type "^(Util.string_of_var v))
                      | Some typ -> typ
                    with _ -> failwith ("getelementptr: unknown type "^(Util.string_of_var v)) in
                  loop x ety' ((yty,y)::tl)
              | _ ->
                  bpr_instr buf (nopt, i);
                  failwith (sprintf "getelementptr failed: %s" (Buffer.contents buf))) in
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
   expect the result of the load/store in vMemRes. *)
let load_store_elimination f =
  let split_memory_accesses =
    let split_block bl =
      let rec split = function
        | [] ->
            ([],[])
        | (Some nopt,Load(_,_,(Pointer(result_ty,_),addr),_,_,_))::tl ->
            (* TODO: alignment *)
            let bname = State.fresh_label() in
            let binstrs, bl_list = split tl in
            let binstrs = (assign_instr nopt result_ty (Integer 64) (Var V.vMemRes))::binstrs in
            [ assign_instr V.vMemAct (Integer 2) (Integer 2) (big 1);
              assign_instr V.vMemSize (Integer 32) (Integer 32) (big (State.bytewidth result_ty));
              assign_instr V.vMemLoc (Integer 64) (Integer 64) addr;
              (None, Br((Label,Basicblock bname),None,[])) ],
(*              assign_instr (V.vStateO()) Label Label (Basicblock bname) ],*)
            {bname;binstrs}::bl_list
        | (None, Store(_,_,(typ,x),(_,addr),_,_,_))::tl ->
            (* TODO: alignment *)
            let bname = State.fresh_label() in
            let binstrs, bl_list = split tl in
            [ assign_instr V.vMemAct (Integer 2) (Integer 2) (big 2);
              assign_instr V.vMemSize (Integer 32) (Integer 32) (big (State.bytewidth typ));
              assign_instr V.vMemLoc (Integer 64) (Integer 64) addr;
              assign_instr V.vMemVal (Integer 32) typ x; (* TODO: memval should be 64 bits *)
              (None, Br((Label,Basicblock bname),None,[])) ],
(*              assign_instr (V.vStateO()) Label Label (Basicblock bname) ],*)
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
      | (_,Switch(_,_,ops,_))::_ -> (* first arg determines which of remaining args to branch to *)
          List.iter add_target (List.map snd ops)
      | (_,Br(target, None, _))::_ -> (* unconditional branch *)
          add_target target
      | (_,Br(_, Some(target, target2), _))::_ -> (* conditional branch *)
          add_target target;
          add_target target2;
      | (_,Indirectbr(_, ops, _))::_ -> (* indirect branch computed by first arg, remaining list the possible targets *)
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
  List.iter (fun bl -> ignore(State.bl_num bl.bname)) f.fblocks; (* assign block numbers *)
  List.iter
    (fun bl ->
      let my_block = State.bl_num bl.bname in
      let elim (nopt,instr) =
        match instr with
        | Switch((ty,e),(Label, Basicblock switchDefault),branches,md) ->
            (* Usually the branches are sorted but not always *)
            let sort_branches x y =
              match (x,y) with
              | ((_,Int xn),_),((_,Int yn),_) -> Big_int.compare_big_int xn yn
              | _ -> failwith "Non-integer case in switch" in
            let branches = List.sort sort_branches branches in
            let max_branch =
              match List.nth branches (List.length branches - 1) with 
              | ((_,Int max_branch),_) -> Big_int.int_of_big_int max_branch
              | _ -> List.length branches in
            let x = Name(false, State.fresh()) in
            [(Some x, Call(false, None, [], Integer(max_branch+1),
                           (Var(Util.Name(true, "unary"))),
                           [(ty,[],e);(Integer 32, [], big(List.length(branches)))],
                           [],[]))]@
              [(assign_instr (State.bl_mask(my_block)) (Integer 1) (Integer 1) (big 0))]@ (* NB my_block could be an explicit branch target *)
              (let rec loop i =
                let call = Call(false, None, [], Integer 1,
                                (Var(Util.Name(true, "selectbit"))),
                                [(Integer(List.length(branches)+1), [], Var x);(Integer 32, [], big i)],
                                [],[]) in
                function
                  | [] -> [(Some(State.bl_mask(State.bl_num switchDefault)), call)]
                  | (((Integer _, Int j),(Label, Basicblock target)) as hd)::tl ->
                      if Big_int.int_of_big_int j <> i then
                        (* There is a gap in the explicit switch cases, go to default *)
                        (Some(State.bl_mask(State.bl_num switchDefault)), call)::(loop (i+1) (hd::tl))
                      else
                        (Some(State.bl_mask(State.bl_num target)), call)::(loop (i+1) tl)
                  | _ -> failwith "unexpected switch in branch elimination"
              in loop 0 branches)
        | Br((Label,Basicblock target), None, _) ->
            let target_block = State.bl_num target in
            if target_block = my_block then
              []
            else
              [(assign_instr (State.bl_mask(my_block)) (Integer 1) (Integer 1) (big 0));
               (assign_instr (State.bl_mask(target_block)) (Integer 1) (Integer 1) (big 1))]
        | Br(x, Some((Label, Basicblock y), (Label, Basicblock z)), _) ->
            let y_block = State.bl_num y in
            let z_block = State.bl_num z in
            (if my_block <> y_block && my_block <> z_block then
              [(assign_instr (State.bl_mask(my_block)) (Integer 1) (Integer 1) (big 0))]
            else [])@
            [(assign_instr (State.bl_mask(y_block)) (Integer 1) (Integer 1) (snd x));
             (Some (State.bl_mask(z_block)), Xor(x,(big 1),[])) (* not(x) *)]
        | Indirectbr _ ->
            failwith "branch elimination: indirectbr is unsupported"
        | Return(None,_) ->
            [(assign_instr V.vIsDone (Integer 1) (Integer 1) (big 1))]
        | Return(Some(ty, v), _) ->
            [
             (assign_instr V.vAnswer (Integer 32) ty v);
             (assign_instr V.vIsDone (Integer 1) (Integer 1) (big 1))
           ]
        | i -> [(nopt,i)] in
      bl.binstrs <- List.concat(List.map elim bl.binstrs))
    f.fblocks

let optional_print f flag thunk =
  match flag with
  | false -> thunk()
  | true ->
      pr_output_file "" (spr bpr_function f);
      if not options.delta then exit 0

let run_phases ctyps f =
  optional_print f options.prf (fun () ->
    phi_elimination f;
    optional_print f options.phi (fun () ->
      load_store_elimination f;
      optional_print f options.load_store (fun () ->
        branch_elimination f;
        optional_print f options.branch (fun () ->
          gep_elimination ctyps f;
          optional_print f options.gep (fun () ->
            ())))))

let file2cu cil_extra_args file =
  if Filename.check_suffix file ".ll" then
    let ch = open_in file in
    let cu =
      try
        Lllex.parse ch
      with e ->
        close_in ch;
        raise e in
    (close_in ch; cu)
  else if Filename.check_suffix file ".c" then
    (* TODO: clean up temp files in case of error *)
    let src_file, temp_dir =
      if not options.cil then
        file, None
      else
        (* Run cilly *)
        let cil_basename =
          (* ex: foo/bar.c --> bar.cil.c *)
          Filename.chop_suffix (Filename.basename file) ".c" ^ ".cil.c" in
        let temp_dir =
          Filename.temp_file "smpcc" ".cil" in (* this creates a temp FILE not directory... *)
        Unix.unlink temp_dir;                  (* ... so delete FILE *)
        Unix.mkdir temp_dir 0o700;             (* ... and create DIR with same name *)
        let cil_file =
          Filename.concat temp_dir cil_basename in
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
          if options.verbose then
            eprintf "%s\n" cmd;
          Sys.command cmd in
        if ret <> 0 then
          failwith(sprintf "Error: cilly failed with exit code %d" ret);
        cil_file, Some temp_dir in
    (* Run clang *)
    let ll_file = Filename.temp_file "smpcc" ".ll" in
    let cmd =
      sprintf "clang -S %s -emit-llvm %s -o %s" options.optflag (Filename.quote src_file) (Filename.quote ll_file) in
    if options.verbose then
      eprintf "%s\n" cmd;
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
    (match temp_dir with None -> ()
    | Some temp_dir ->
        if options.keep_cil then begin
          let ret =
            (* Use mv instead of Unix.rename because rename fails on cross-device move *)
            Sys.command(sprintf "mv %s %s" (Filename.quote src_file) (Filename.quote (Filename.basename src_file))) in
          if ret <> 0 then
            failwith(sprintf "Error: mv failure %s -> %s" (Filename.quote src_file) (Filename.quote (Filename.basename src_file)))
        end;
        let ret = Sys.command(sprintf "rm -r %s" (Filename.quote temp_dir)) in
        if ret <> 0 then
          failwith(sprintf "Error: failed to remove temporary directory %s, exit code %d" (Filename.quote temp_dir) ret));
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
  let (x,args) = getopt "-keep-cil" args         in options.keep_cil <- x;
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
  let (x,args) = getopt "-v" args                in options.verbose <- x;
  let (x,args) = getopt "-run" args              in options.run <- x;
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
     printf "         -run                        Compile and run the program immediately\n";
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
        printf "%s" (spr bpr_cu m))
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
    State.alloc_globals m;
    let f =
      (match options.fname with
      | None ->
          List.hd(m.cfuns)
      | Some fname ->
          List.hd(List.filter
                    (fun (f:finfo) -> (string_of_var f.fname) = fname)
                    m.cfuns)) in
    if not options.delta then begin
      run_phases m.ctyps f;
      if options.run then
        options.output <- Some(Filename.temp_file "smpcc" ".c")
      else if options.output = None then
        options.output <- Some(Filename.basename (Filename.chop_suffix file ".c"));
      if options.circuitlib = Some "gmw" then
        Gmw.print_function_circuit m f
      else
        Gc.print_function_circuit m f;
      if options.run then begin
        let prefix = match options.output with Some x -> x | None -> failwith "impossible" in
        if options.circuitlib = Some "gmw" then begin
          ignore(Sys.command(Printf.sprintf "go run %s_blocks.go %s_main.go" prefix prefix));
          Sys.remove (Printf.sprintf "%s_blocks.go" prefix);
          Sys.remove (Printf.sprintf "%s_main.go" prefix)
        end else begin
          ignore(Sys.command(Printf.sprintf "go run %s_gen.go %s_eval.go %s_main.go" prefix prefix prefix));
          Sys.remove (Printf.sprintf "%s_gen.go" prefix);
          Sys.remove (Printf.sprintf "%s_eval.go" prefix);
          Sys.remove (Printf.sprintf "%s_main.go" prefix)
        end;
        Sys.remove (Printf.sprintf "%s" prefix)
      end
    end
    else begin
      let toggle_before =
        if options.phi then
          (options.prf <- true; (fun () -> options.prf <- false))
        else if options.load_store then
          (options.phi <- true; (fun () -> options.phi <- false))
        else if options.branch then
          (options.load_store <- true; (fun () -> options.load_store <- false))
        else if options.gep then
          (options.branch <- true; (fun () -> options.branch <- false))
        else (fun () -> ()) in
      let file_before = Filename.temp_file "smpcc." ".diff" in
      options.output <- Some file_before;
      run_phases m.ctyps f;
      toggle_before();
      let file_after = Filename.temp_file "smpcc." ".diff" in
      options.output <- Some file_after;
      run_phases m.ctyps f;
      ignore(Sys.command(sprintf "colordiff -U -1 %s %s | more" file_before file_after));
      Sys.remove file_before;
      Sys.remove file_after;
      ()
    end
end
