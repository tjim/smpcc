open Printf

let x_count = ref 0
let bl_count = ref 0
let bl_tbl : (Llabs.var, int) Hashtbl.t = Hashtbl.create 11
let vartyp_tbl : (Llabs.var, Llabs.typ) Hashtbl.t = Hashtbl.create 11
let initialized = ref false
let muxes : (Llabs.var * Llabs.var * string) list ref = ref []
let add_mux x = muxes := x::!muxes
let get_muxes() = !muxes
let bl_bits = ref 32
let get_bl_bits() = !bl_bits

let v_map = function
  | Llabs.Name(true, n) -> "@"^n
  | Llabs.Name(false, n) -> "%"^n
  | Llabs.Id(true, n) -> "@"^(string_of_int n)
  | Llabs.Id(false, n) -> "%"^(string_of_int n)

let typ_of_var var =
  try Hashtbl.find vartyp_tbl var
  with Not_found -> failwith("Could not find type of " ^ (Llabs.string_of_var var))
let add_vartyp var typ =
  if Hashtbl.mem vartyp_tbl var then
    eprintf "Warning: more than one value for %s in vartyp_tbl\n%!" (Llabs.string_of_var var);
  Hashtbl.add vartyp_tbl var typ;
  var

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
let bl_vars = ref Llabs.VSet.empty
let bl_num bl =
  if Hashtbl.mem bl_tbl bl then Hashtbl.find bl_tbl bl else
  let num = !bl_count in
  bl_count := !bl_count + 1;
  Hashtbl.add bl_tbl bl num;
  let x = sprintf "block%d" num in
  bl_vars := Llabs.VSet.add (add_vartyp (Llabs.Name(false,x)) (Llabs.Integer 1)) !bl_vars;
  num
let bl_mask num =
  let x = sprintf "block%d" num in
  Llabs.Name(false,x)
let dump() =
  let b = Buffer.create 11 in
  bprintf b "// BLOCK ASSIGNMENT, %d blocks, %d bits\n" !bl_count !bl_bits;
  Hashtbl.iter
    (fun var bl ->
      bprintf b "// %s <-> %a\n" (sprintf "%d:%d" bl !bl_bits) Llabs.bpr_var var)
    bl_tbl;
  printf "%s" (Buffer.contents b)

let fresh_label() =
  let x = sprintf "vLabel%d" !x_count in
  x_count := !x_count + 1;
  add_vartyp (Llabs.Name(false,x)) Llabs.Label

let dump_vartyps() =
  let b = Buffer.create 11 in
  Hashtbl.iter
    (fun var typ ->
      bprintf b "%a: %a\n" Llabs.bpr_var var Llabs.bpr_typ typ)
    vartyp_tbl;
  Printf.printf "%s" (Buffer.contents b)

module V = struct
let vIsDone =  add_vartyp (Llabs.Name(false,"vIsDone"))  (Llabs.Integer 1)
let vMemAct =  add_vartyp (Llabs.Name(false,"vMemAct"))  (Llabs.Integer 2)
let vMemLoc =  add_vartyp (Llabs.Name(false,"vMemLoc"))  (Llabs.Integer 64)
let vMemVal =  add_vartyp (Llabs.Name(false,"vMemVal"))  (Llabs.Integer 32)
let vMemRes =  add_vartyp (Llabs.Name(false,"vMemRes"))  (Llabs.Integer 64)
let vMemSize = add_vartyp (Llabs.Name(false,"vMemSize")) (Llabs.Integer 32)
let vAnswer =  add_vartyp (Llabs.Name(false,"vAnswer"))  (Llabs.Integer 32)
let vStateO() = Llabs.Name(false,"vStateO")
let special = (* NB: Works now because we have hard-coded bl_bits to 32 *)
  ignore(add_vartyp (Llabs.Name(false,"vStateO")) (Llabs.Integer(get_bl_bits())));
  List.fold_right Llabs.VSet.add
    [vIsDone; vMemAct; vMemLoc; vMemVal; vMemSize; vAnswer;
     vStateO();]
    Llabs.VSet.empty
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
               | (Some(Llabs.Id(false, x)), _) -> [x]
               | _ -> [])
             instrs) in
      match name with
      | Llabs.Id(false,-1) -> Llabs.Id(false, n), max n instr_numbers + 1
      | _ -> name, if instr_numbers = [] then n else max n instr_numbers + 1 in
    let num = ref 0 in
    f.Llabs.fblocks <-
      List.map
        (fun {Llabs.bname=name; Llabs.binstrs=instrs} ->
          let name', num' = number_block !num (name, instrs) in
          num := num';
          {Llabs.bname=name'; Llabs.binstrs=instrs})
        f.Llabs.fblocks in
  List.iter number_blocks cu.Llabs.cfuns

let assign_vartyps_instr ctyps (nopt, i) =
  let typ =
    let typ_of (t,v) = t in
    (match i with
    | Llabs.Add(nuw, nsw, x, y, md) -> typ_of x
    | Llabs.Sub(nuw, nsw, x, y, md) -> typ_of x
    | Llabs.Mul(nuw, nsw, x, y, md) -> typ_of x
    | Llabs.Shl(nuw, nsw, x, y, md) -> typ_of x
    | Llabs.Fadd(fmf, x, y, md)     -> typ_of x
    | Llabs.Fsub(fmf, x, y, md)     -> typ_of x
    | Llabs.Fmul(fmf, x, y, md)     -> typ_of x
    | Llabs.Fdiv(fmf, x, y, md)     -> typ_of x
    | Llabs.Frem(fmf, x, y, md)     -> typ_of x
    | Llabs.Sdiv(e, x, y, md)       -> typ_of x
    | Llabs.Udiv(e, x, y, md)       -> typ_of x
    | Llabs.Lshr(e, x, y, md)       -> typ_of x
    | Llabs.Ashr(e, x, y, md)       -> typ_of x
    | Llabs.Urem(x, y, md)          -> typ_of x
    | Llabs.Srem(x, y, md)          -> typ_of x
    | Llabs.And (x, y, md)          -> typ_of x
    | Llabs.Or  (x, y, md)          -> typ_of x
    | Llabs.Xor (x, y, md)          -> typ_of x
    | Llabs.Icmp(icmp, x, y, md)    -> Llabs.Integer 1
    | Llabs.Fcmp(fcmp, x, y, md)    -> Llabs.Integer 1
    | Llabs.Trunc(x, y, md)         -> y
    | Llabs.Zext(x, y, md)          -> y
    | Llabs.Sext(x, y, md)          -> y
    | Llabs.Fptrunc(x, y, md)       -> y
    | Llabs.Fpext(x, y, md)         -> y
    | Llabs.Bitcast(x, y, md)       -> y
    | Llabs.Addrspacecast(x, y, md) -> y
    | Llabs.Uitofp(x, y, md)        -> y
    | Llabs.Sitofp(x, y, md)        -> y
    | Llabs.Fptoui(x, y, md)        -> y
    | Llabs.Fptosi(x, y, md)        -> y
    | Llabs.Inttoptr(x, y, md)      -> y
    | Llabs.Ptrtoint(x, y, md)      -> y
    | Llabs.Va_arg(x, y, md)        -> y
    | Llabs.Getelementptr(inbounds, x, md) ->
        (match x with
        | (Llabs.Pointer(ety,aspace),_)::tl ->
            let ety = Llabs.Arraytyp(1,ety) in (* This is the key to understanding gep --- ety should start out as an Array *)
            let rec loop ety = function
              | [] -> ety
              | (ytyp,y)::tl ->
                  (match ety with
                  | Llabs.Arraytyp(_,ety') ->
                      (* assume that y is in-bounds *)
                      loop ety' tl
                  | Llabs.Structtyp(_,field_typs) ->
                      (match y with
                      | Llabs.Int i ->
                          let ety' =
                            (try List.nth field_typs (Big_int.int_of_big_int i)
                            with _ -> failwith "getelementptr: out-of-bounds struct field selection") in
                          loop ety' tl
                      | _ -> failwith "getelementptr: non-int selector for struct field")
                  | Llabs.Vartyp v ->
                      let ety' =
                        try
                          match List.assoc v ctyps with
                          | None -> failwith ("getelementptr: opaque type "^(Llabs.string_of_var v))
                          | Some typ -> typ
                        with _ -> failwith ("getelementptr: unknown type "^(Llabs.string_of_var v)) in
                      loop ety' ((ytyp,y)::tl)
                  | _ ->
                      failwith "getelementptr: pointer does not point into an array or struct") in
            Llabs.Pointer(loop ety tl,aspace)
        | _ -> failwith "getelementptr: must be applied to a pointer")
    | Llabs.Shufflevector([(Llabs.Vector(_,typ),_);_;(Llabs.Vector(m,_),_)], md) -> Llabs.Vector(m,typ)
    | Llabs.Insertelement([(typ,_);_;_], md) -> typ
    | Llabs.Extractelement([(Llabs.Vector(_,typ),_);_;_], md) -> typ
    | Llabs.Select([_;(typ,_);_], md) -> typ
    | Llabs.Phi(typ, incoming, md) -> typ
    | Llabs.Landingpad(x, y, z, w, md) -> x
    | Llabs.Call(is_tail_call, callconv, retattrs, callee_ty, callee_name, operands, callattrs, md) -> callee_ty
    | Llabs.Alloca(x, y, z, w, md) -> y
    | Llabs.Load(x, y, (Llabs.Pointer(typ,_), z), w, v, md) -> typ
    | Llabs.Store(x, y, z, w, v, u, md) -> typ_of z
    | Llabs.Cmpxchg(a, x, y, z, w, v, u, t, md) -> typ_of z
    | Llabs.Atomicrmw(x, y, z, w, v, u, md) -> typ_of w
    | Llabs.Fence(x, y, md) -> Llabs.Void
    | Llabs.Extractvalue((typ,_), y, md) ->
        let rec loop = function
          | typ, [] -> typ
          | Llabs.Structtyp(_,l), hd::tl ->
              if hd < 0 || hd >= List.length l then failwith "extractvalue struct index out of range" else
              let typ = List.nth l hd in
              loop (typ, tl)
          | Llabs.Arraytyp(len,typ), hd::tl ->
              if hd < 0 || hd >= len then  failwith "extractvalue array index out of range" else
              loop (typ, tl)
          | _ -> failwith "extractvalue: not a struct or array" in
        loop (typ, y)
    | Llabs.Insertvalue(x, y, z, md) -> typ_of x
    | Llabs.Unreachable md -> Llabs.Void
    | Llabs.Return(None, md) -> Llabs.Void
    | Llabs.Return(Some(x, y), md) -> Llabs.Void
    | Llabs.Br(x, None, md) -> Llabs.Void
    | Llabs.Br(x, Some(y, z), md) -> Llabs.Void
    | Llabs.Indirectbr(x, y, md) -> Llabs.Void
    | Llabs.Resume(x, md) -> Llabs.Void
    | Llabs.Switch(x, y, z, md) -> Llabs.Void
    | Llabs.Invoke(x, y, z, w, v, u, t, s, md) -> z
    | _ -> failwith "assign_vartyps_instr") in
  (match nopt with None -> () | Some var -> ignore(add_vartyp var typ))

let assign_vartyps_block ctyps b =
  ignore(add_vartyp b.Llabs.bname Llabs.Label);
  List.iter (assign_vartyps_instr ctyps) b.Llabs.binstrs

let assign_vartyps cu =
  List.iter
    (function
      | (x, None)   -> ()
      | (x, Some t) -> ignore(add_vartyp x t))
    cu.Llabs.ctyps;
  List.iter
    (fun {Llabs.gname;Llabs.gtyp} -> ignore(add_vartyp gname (Llabs.Pointer(gtyp,None))))
    cu.Llabs.cglobals;
  List.iter
    (fun f ->
      let ftyp = Llabs.Pointer(Llabs.Funtyp(f.Llabs.freturntyp,fst f.Llabs.fparams,snd f.Llabs.fparams), None) in
      ignore(add_vartyp f.Llabs.fname ftyp);
      List.iter (assign_vartyps_block cu.Llabs.ctyps) f.Llabs.fblocks)
    cu.Llabs.cfuns;
  List.iter
    (Llabs.value_map (function
      | Llabs.Var v ->
          if typ_of_var v = Llabs.Label then Llabs.Basicblock v else Llabs.Var v
      | x -> x))
    cu.Llabs.cfuns

open Llabs

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
