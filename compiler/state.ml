open Oli
open Printf

let x_count = ref 0
let bl_count = ref 0
let bl_tbl : (variable,int) Hashtbl.t = Hashtbl.create 11
let initialized = ref false
let muxes : (Oli.variable * Oli.variable * string) list ref = ref []
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
let v_map v =
  let (gl,n,ty) = v in if gl then ("@"^n,ty) else ("%"^n,ty)
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
let fresh_label() =
  let x = sprintf "attsrcLabel%d" !x_count in
  x_count := !x_count + 1;
  (false,x,Label)
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
      bprintf b "// %s <-> %a\n" (sprintf "%d:%d" bl !bl_bits) bpr_variable var)
    bl_tbl;
  printf "%s" (Buffer.contents b)

module V = struct
let attsrcIsDone = (false,"attsrcIsDone",Integer 1)
let attsrcMemAct = (false,"attsrcMemAct",Integer 2)
let attsrcMemLoc = (false,"attsrcMemLoc",Integer 64)
let attsrcMemVal = (false,"attsrcMemVal",Integer 32)
let attsrcMemRes = (false,"attsrcMemRes",Integer 64)
let attsrcMemSize = (false,"attsrcMemSize",Integer 32)
let attsrcNumElts = (false,"attsrcNumElts",Integer 32)
let attsrcAnswer = (false,"attsrcAnswer",Integer 32)

let attsrcStateO() = (false,"attsrcStateO",Integer(get_bl_bits()))
let special = (* NB: Works now because we have hard-coded bl_bits to 32 *)
  List.fold_right VSet.add
    [attsrcIsDone; attsrcMemAct; attsrcMemLoc; attsrcMemVal; attsrcMemSize; attsrcNumElts; attsrcAnswer;
     attsrcStateO();]
    VSet.empty
end
