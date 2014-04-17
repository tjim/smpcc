open Printf

let x_count = ref 0
let bl_count = ref 0
let bl_tbl : (Util.var, int) Hashtbl.t = Hashtbl.create 11
let vartyp_tbl : (Util.var, Util.typ) Hashtbl.t = Hashtbl.create 11
let initialized = ref false
let muxes : (Util.var * Util.var * string) list ref = ref []
let add_mux x = muxes := x::!muxes
let get_muxes() = !muxes
let bl_bits = ref 32
let get_bl_bits() = !bl_bits
let set_bl_bits n =
(* TEMPORARY because Labels have fixed bitwidth 32 in my llvm interface
      let rec loop n = if n <= 2 then 1 else 1 + loop(n/2) in
      bl_bits <- loop n;
*)
  initialized := true

let v_map = function
  | Util.Name(true, n) -> "@"^n
  | Util.Name(false, n) -> "%"^n
  | Util.Id(true, n) -> "@"^(string_of_int n)
  | Util.Id(false, n) -> "%"^(string_of_int n)

let fresh() =
  let x = sprintf "x%d" !x_count in
  x_count := !x_count + 1;
  x
let freshen name =
  let freshtail = Str.regexp "^\\(.\\)+_[0-9]+_$" in
  let x =
    sprintf "%s_%d_"
      (if Str.string_match freshtail name 0
      then Str.matched_group 1 name
      else name)
      !x_count in
  x_count := !x_count + 1;
  x
let bl_num bl =
  if Hashtbl.mem bl_tbl bl then Hashtbl.find bl_tbl bl else
  let num = !bl_count in
  bl_count := !bl_count + 1;
  Hashtbl.add bl_tbl bl num;
  num
let bl_map bl =
  if not(!initialized) then failwith "bl_map: must set_bl_bits first" else
  sprintf "%d:%d" (bl_num bl) !bl_bits
let dump() =
  let b = Buffer.create 11 in
  bprintf b "// BLOCK ASSIGNMENT, %d blocks, %d bits\n" !bl_count !bl_bits;
  Hashtbl.iter
    (fun var bl ->
      bprintf b "// %s <-> %a\n" (sprintf "%d:%d" bl !bl_bits) Util.bpr_var var)
    bl_tbl;
  printf "%s" (Buffer.contents b)

let typ_of_var var =
  try Hashtbl.find vartyp_tbl var
  with Not_found -> failwith("Could not find type of " ^ (Util.string_of_var var))
let add_vartyp var typ = Hashtbl.add vartyp_tbl var typ; var

let fresh_label() =
  let x = sprintf "attsrcLabel%d" !x_count in
  x_count := !x_count + 1;
  add_vartyp (Util.Name(false,x)) Util.Label

let dump_vartyps() =
  let b = Buffer.create 11 in
  Hashtbl.iter
    (fun var typ ->
      bprintf b "%a: %a\n" Util.bpr_var var Util.bpr_typ typ)
    vartyp_tbl;
  Printf.printf "%s" (Buffer.contents b)

module V = struct
let attsrcIsDone =  add_vartyp (Util.Name(false,"attsrcIsDone"))  (Util.Integer 1)
let attsrcMemAct =  add_vartyp (Util.Name(false,"attsrcMemAct"))  (Util.Integer 2)
let attsrcMemLoc =  add_vartyp (Util.Name(false,"attsrcMemLoc"))  (Util.Integer 64)
let attsrcMemVal =  add_vartyp (Util.Name(false,"attsrcMemVal"))  (Util.Integer 32)
let attsrcMemRes =  add_vartyp (Util.Name(false,"attsrcMemRes"))  (Util.Integer 64)
let attsrcMemSize = add_vartyp (Util.Name(false,"attsrcMemSize")) (Util.Integer 32)
let attsrcNumElts = add_vartyp (Util.Name(false,"attsrcNumElts")) (Util.Integer 32)
let attsrcAnswer =  add_vartyp (Util.Name(false,"attsrcAnswer"))  (Util.Integer 32)
let attsrcStateO() = Util.Name(false,"attsrcStateO")
let special = (* NB: Works now because we have hard-coded bl_bits to 32 *)
  ignore(add_vartyp (Util.Name(false,"attsrcStateO")) (Util.Integer(get_bl_bits())));
  List.fold_right Util.VSet.add
    [attsrcIsDone; attsrcMemAct; attsrcMemLoc; attsrcMemVal; attsrcMemSize; attsrcNumElts; attsrcAnswer;
     attsrcStateO();]
    Util.VSet.empty
end

(* Blocks may not have explicit names (labels) when parsed.
   Any unnamed block is assigned name Id(false,-1) by the parser.
   This function finds a correct name. *)
let number_cu cu =
  let number_blocks f =
    let number_block n (name, instrs) =
      let rec max n = function
        | [] -> n
        | hd::tl -> if n>hd then max n tl else max hd tl in
      let instr_numbers =
        List.concat
          (List.map
             (function
               | (Some(Util.Id(false, x)), _) -> [x]
               | _ -> [])
             instrs) in
      match name with
      | Util.Id(false,-1) -> Util.Id(false, n), max n instr_numbers + 1
      | _ -> name, if instr_numbers = [] then n else max n instr_numbers + 1 in
    let num = ref 0 in
    f.Util.fblocks <-
      List.map
        (fun {Util.bname=name; Util.binstrs=instrs} ->
          let name', num' = number_block !num (name, instrs) in
          num := num';
          {Util.bname=name'; Util.binstrs=instrs})
        f.Util.fblocks in
  List.iter number_blocks cu.Util.cfuns

let assign_vartyps_instr (nopt, i) =
  let typ =
    let typ_of (t,v) = t in
    (match i with
    | Util.Add(nuw, nsw, x, y) -> typ_of x
    | Util.Sub(nuw, nsw, x, y) -> typ_of x
    | Util.Mul(nuw, nsw, x, y) -> typ_of x
    | Util.Shl(nuw, nsw, x, y) -> typ_of x
    | Util.Fadd(fmf, x, y)           -> typ_of x
    | Util.Fsub(fmf, x, y)           -> typ_of x
    | Util.Fmul(fmf, x, y)           -> typ_of x
    | Util.Fdiv(fmf, x, y)           -> typ_of x
    | Util.Frem(fmf, x, y)           -> typ_of x
    | Util.Sdiv(e, x, y)           -> typ_of x
    | Util.Udiv(e, x, y)           -> typ_of x
    | Util.Lshr(e, x, y)           -> typ_of x
    | Util.Ashr(e, x, y)           -> typ_of x
    | Util.Urem(x, y)           -> typ_of x
    | Util.Srem(x, y)           -> typ_of x
    | Util.And (x, y)           -> typ_of x
    | Util.Or  (x, y)           -> typ_of x
    | Util.Xor (x, y)           -> typ_of x
    | Util.Icmp(icmp, x, y) -> typ_of x
    | Util.Fcmp(fcmp, x, y) -> typ_of x
    | Util.Trunc(x, y)          -> typ_of x
    | Util.Zext(x, y)           -> typ_of x
    | Util.Sext(x, y)           -> typ_of x
    | Util.Fptrunc(x, y)        -> typ_of x
    | Util.Fpext(x, y)          -> typ_of x
    | Util.Bitcast(x, y)        -> typ_of x
    | Util.Addrspacecast(x, y)  -> typ_of x
    | Util.Uitofp(x, y)         -> typ_of x
    | Util.Sitofp(x, y)         -> typ_of x
    | Util.Fptoui(x, y)         -> typ_of x
    | Util.Fptosi(x, y)         -> typ_of x
    | Util.Inttoptr(x, y)       -> typ_of x
    | Util.Ptrtoint(x, y)       -> typ_of x
    | Util.Va_arg(x, y)         -> typ_of x
    | Util.Getelementptr(inbounds, x) ->
        (match x with
        | (Util.Pointer(ety,_),_)::tl ->
            let ety = Util.Arraytyp(1,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
            let rec loop ety = function
              | [] -> ety
              | (_,y)::tl ->
                  (match ety with
                  | Util.Arraytyp(_,ety') ->
                      (* assume that y is in-bounds *)
                      loop ety' tl
                  | Util.Structtyp(_,field_typs) ->
                      (match y with
                      | Util.Int i ->
                          let ety' =
                            (try List.nth field_typs (Big_int.int_of_big_int i)
                            with _ -> failwith "getelementptr: out-of-bounds struct field selection") in
                          loop ety' tl
                      | _ -> failwith "getelementptr: non-int selector for struct field")
                  | _ ->
                      failwith "getelementptr: pointer does not point into an array or struct") in
            loop ety tl
        | _ -> failwith "getelementptr: must be applied to a pointer")
    | Util.Shufflevector [(Util.Vector(_,typ),_);_;(Util.Vector(m,_),_)] -> Util.Vector(m,typ)
    | Util.Insertelement [(typ,_);_;_] -> typ
    | Util.Extractelement [(Util.Vector(_,typ),_);_;_] -> typ
    | Util.Select[_;(typ,_);_] -> typ
    | Util.Phi(typ, incoming) -> typ
    | Util.Landingpad(x, y, z, w) -> x
    | Util.Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs) -> callee_ty
    | Util.Alloca(x, y, z, w) -> y
    | Util.Load(x, y, z, w, v) -> typ_of z
    | Util.Store(x, y, z, w, v, u) -> typ_of z
    | Util.Cmpxchg(x, y, z, w, v, u, t) -> typ_of z
    | Util.Atomicrmw(x, y, z, w, v, u) -> typ_of w
    | Util.Fence(x, y) -> Util.Void
    | Util.Extractvalue((typ,_), y) ->
        let rec loop = function
          | typ, [] -> typ
          | Util.Structtyp(_,l), hd::tl ->
              if hd < 0 || hd >= List.length l then failwith "extractvalue struct index out of range" else
              let typ = List.nth l hd in
              loop (typ, tl)
          | Util.Arraytyp(len,typ), hd::tl ->
              if hd < 0 || hd >= len then  failwith "extractvalue array index out of range" else
              loop (typ, tl)
          | _ -> failwith "extractvalue: not a struct or array" in
        loop (typ, y)
    | Util.Insertvalue(x, y, z) -> typ_of x
    | Util.Unreachable -> Util.Void
    | Util.Return None -> Util.Void
    | Util.Return(Some(x, y)) -> Util.Void
    | Util.Br(x, None) -> Util.Void
    | Util.Br(x, Some(y, z)) -> Util.Void
    | Util.Indirectbr(x, y) -> Util.Void
    | Util.Resume x -> Util.Void
    | Util.Switch(x, y, z) -> Util.Void
    | Util.Invoke(x, y, z, w, v, u, t, s) -> z
    | _ -> failwith "assign_vartyps_instr") in
  (match nopt with None -> () | Some var -> ignore(add_vartyp var typ))

let assign_vartyps_block b =
  ignore(add_vartyp b.Util.bname Util.Label);
  List.iter assign_vartyps_instr b.Util.binstrs

let assign_vartyps cu =
  List.iter
    (fun {Util.gname;Util.gtyp} -> ignore(add_vartyp gname (Util.Pointer(gtyp,None))))
    cu.Util.cglobals;
  List.iter
    (fun f ->
      let ftyp = Util.Pointer(Util.Funtyp(f.Util.freturntyp,fst f.Util.fparams,snd f.Util.fparams), None) in
      ignore(add_vartyp f.Util.fname ftyp);
      List.iter assign_vartyps_block f.Util.fblocks)
    cu.Util.cfuns;
  List.iter
    (Util.value_map (function
      | Util.Var v ->
          if typ_of_var v = Util.Label then Util.Basicblock v else Util.Var v
      | x -> x))
    cu.Util.cfuns
