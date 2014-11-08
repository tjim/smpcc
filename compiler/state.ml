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
let add_vartyp var typ =
  if Hashtbl.mem vartyp_tbl var then
    eprintf "Warning: more than one value for %s in vartyp_tbl\n%!" (Util.string_of_var var);
  Hashtbl.add vartyp_tbl var typ;
  var

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
let vIsDone =  add_vartyp (Util.Name(false,"vIsDone"))  (Util.Integer 1)
let vMemAct =  add_vartyp (Util.Name(false,"vMemAct"))  (Util.Integer 2)
let vMemLoc =  add_vartyp (Util.Name(false,"vMemLoc"))  (Util.Integer 64)
let vMemVal =  add_vartyp (Util.Name(false,"vMemVal"))  (Util.Integer 32)
let vMemRes =  add_vartyp (Util.Name(false,"vMemRes"))  (Util.Integer 64)
let vMemSize = add_vartyp (Util.Name(false,"vMemSize")) (Util.Integer 32)
let vAnswer =  add_vartyp (Util.Name(false,"vAnswer"))  (Util.Integer 32)
let vStateO() = Util.Name(false,"vStateO")
let special = (* NB: Works now because we have hard-coded bl_bits to 32 *)
  ignore(add_vartyp (Util.Name(false,"vStateO")) (Util.Integer(get_bl_bits())));
  List.fold_right Util.VSet.add
    [vIsDone; vMemAct; vMemLoc; vMemVal; vMemSize; vAnswer;
     vStateO();]
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

let assign_vartyps_instr ctyps (nopt, i) =
  let typ =
    let typ_of (t,v) = t in
    (match i with
    | Util.Add(nuw, nsw, x, y, md) -> typ_of x
    | Util.Sub(nuw, nsw, x, y, md) -> typ_of x
    | Util.Mul(nuw, nsw, x, y, md) -> typ_of x
    | Util.Shl(nuw, nsw, x, y, md) -> typ_of x
    | Util.Fadd(fmf, x, y, md)     -> typ_of x
    | Util.Fsub(fmf, x, y, md)     -> typ_of x
    | Util.Fmul(fmf, x, y, md)     -> typ_of x
    | Util.Fdiv(fmf, x, y, md)     -> typ_of x
    | Util.Frem(fmf, x, y, md)     -> typ_of x
    | Util.Sdiv(e, x, y, md)       -> typ_of x
    | Util.Udiv(e, x, y, md)       -> typ_of x
    | Util.Lshr(e, x, y, md)       -> typ_of x
    | Util.Ashr(e, x, y, md)       -> typ_of x
    | Util.Urem(x, y, md)          -> typ_of x
    | Util.Srem(x, y, md)          -> typ_of x
    | Util.And (x, y, md)          -> typ_of x
    | Util.Or  (x, y, md)          -> typ_of x
    | Util.Xor (x, y, md)          -> typ_of x
    | Util.Icmp(icmp, x, y, md)    -> Util.Integer 1
    | Util.Fcmp(fcmp, x, y, md)    -> Util.Integer 1
    | Util.Trunc(x, y, md)         -> y
    | Util.Zext(x, y, md)          -> y
    | Util.Sext(x, y, md)          -> y
    | Util.Fptrunc(x, y, md)       -> y
    | Util.Fpext(x, y, md)         -> y
    | Util.Bitcast(x, y, md)       -> y
    | Util.Addrspacecast(x, y, md) -> y
    | Util.Uitofp(x, y, md)        -> y
    | Util.Sitofp(x, y, md)        -> y
    | Util.Fptoui(x, y, md)        -> y
    | Util.Fptosi(x, y, md)        -> y
    | Util.Inttoptr(x, y, md)      -> y
    | Util.Ptrtoint(x, y, md)      -> y
    | Util.Va_arg(x, y, md)        -> y
    | Util.Getelementptr(inbounds, x, md) ->
        (match x with
        | (Util.Pointer(ety,aspace),_)::tl ->
            let ety = Util.Arraytyp(1,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
            let rec loop ety = function
              | [] -> ety
              | (ytyp,y)::tl ->
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
                  | Util.Vartyp v ->
                      let ety' =
                        try 
                          match List.assoc v ctyps with
                          | None -> failwith ("getelementptr: opaque type "^(Util.string_of_var v))
                          | Some typ -> typ
                        with _ -> failwith ("getelementptr: unknown type "^(Util.string_of_var v)) in
                      loop ety' ((ytyp,y)::tl)
                  | _ ->
                      failwith "getelementptr: pointer does not point into an array or struct") in
            Util.Pointer(loop ety tl,aspace)
        | _ -> failwith "getelementptr: must be applied to a pointer")
    | Util.Shufflevector([(Util.Vector(_,typ),_);_;(Util.Vector(m,_),_)], md) -> Util.Vector(m,typ)
    | Util.Insertelement([(typ,_);_;_], md) -> typ
    | Util.Extractelement([(Util.Vector(_,typ),_);_;_], md) -> typ
    | Util.Select([_;(typ,_);_], md) -> typ
    | Util.Phi(typ, incoming, md) -> typ
    | Util.Landingpad(x, y, z, w, md) -> x
    | Util.Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs, md) -> callee_ty
    | Util.Alloca(x, y, z, w, md) -> y
    | Util.Load(x, y, (Util.Pointer(typ,_), z), w, v, md) -> typ
    | Util.Store(x, y, z, w, v, u, md) -> typ_of z
    | Util.Cmpxchg(x, y, z, w, v, u, t, md) -> typ_of z
    | Util.Atomicrmw(x, y, z, w, v, u, md) -> typ_of w
    | Util.Fence(x, y, md) -> Util.Void
    | Util.Extractvalue((typ,_), y, md) ->
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
    | Util.Insertvalue(x, y, z, md) -> typ_of x
    | Util.Unreachable md -> Util.Void
    | Util.Return(None, md) -> Util.Void
    | Util.Return(Some(x, y), md) -> Util.Void
    | Util.Br(x, None, md) -> Util.Void
    | Util.Br(x, Some(y, z), md) -> Util.Void
    | Util.Indirectbr(x, y, md) -> Util.Void
    | Util.Resume(x, md) -> Util.Void
    | Util.Switch(x, y, z, md) -> Util.Void
    | Util.Invoke(x, y, z, w, v, u, t, s, md) -> z
    | _ -> failwith "assign_vartyps_instr") in
  (match nopt with None -> () | Some var -> ignore(add_vartyp var typ))

let assign_vartyps_block ctyps b =
  ignore(add_vartyp b.Util.bname Util.Label);
  List.iter (assign_vartyps_instr ctyps) b.Util.binstrs

let assign_vartyps cu =
  List.iter
    (function
      | (x, None)   -> ()
      | (x, Some t) -> ignore(add_vartyp x t))
    cu.Util.ctyps;
  List.iter
    (fun {Util.gname;Util.gtyp} -> ignore(add_vartyp gname (Util.Pointer(gtyp,None))))
    cu.Util.cglobals;
  List.iter
    (fun f ->
      let ftyp = Util.Pointer(Util.Funtyp(f.Util.freturntyp,fst f.Util.fparams,snd f.Util.fparams), None) in
      ignore(add_vartyp f.Util.fname ftyp);
      List.iter (assign_vartyps_block cu.Util.ctyps) f.Util.fblocks)
    cu.Util.cfuns;
  List.iter
    (Util.value_map (function
      | Util.Var v ->
          if typ_of_var v = Util.Label then Util.Basicblock v else Util.Var v
      | x -> x))
    cu.Util.cfuns

open Util

let rec bitwidth = function
  | Vartyp v                                 -> (bitwidth (typ_of_var v))
  | Void                                     -> 0
  | Half                                     -> 16
  | Float                                    -> 32
  | Double                                   -> 64
  | X86_fp80                                 -> 80
  | X86_mmx                                  -> 64
  | Fp128                                    -> 128
  | Ppc_fp128                                -> 128
  | Label                                    -> 32 (* ??? *)
  | Metadata                                 -> 0
  | Integer x                                -> x
  | Funtyp(return_ty, param_tys, is_var_arg) -> 0
  | Structtyp(false, tys) (* TODO: not packed struct, depends on datalayout *)
  | Structtyp(true, tys)                     ->
      List.fold_left (fun x y -> x+y) 0 (List.map bitwidth tys)
  | Arraytyp(len,element_ty)                 -> len*(bitwidth element_ty)
  | Pointer(address_space,element_ty)        -> 64 (* TODO: depends on datalayout *)
  | Vector(len,element_ty)                   -> len*(bitwidth element_ty)

let bytewidth ty =
  let bits = bitwidth ty in
  if (bits mod 8) <> 0 then Printf.eprintf "Warning: bitwidth not divisible by 8";
  bits/8 + (if (bits mod 8) <> 0 then 1 else 0)

let global_locations = Hashtbl.create 10
let loc = ref 0
let alloc_globals m =
  loc := 0; (* smallest memory location *)
  Hashtbl.reset global_locations;
  let align n =
    let md = !loc mod n in
    if md = 0 then () else
    loc := !loc + (n - md) in
  let alloc_global = function
    | { gvalue=None } -> ()
    | { gname; gtyp; gvalue=Some v } ->
        Hashtbl.add global_locations gname !loc;
        let bytes =
          try bytewidth gtyp with _ -> 100 in
        loc := !loc + bytes;
        align 4 in
  List.iter alloc_global m.cglobals
