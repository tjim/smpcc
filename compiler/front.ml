(* ocamlfind ocamlopt -package findlib,cil callgraph.cmxa oneret.cmxa -linkpkg front.ml *)

open Cil
open Feature

(* The flatten transform *)
module Flattener = struct
module StringSet = Set.Make(String)

open Callgraph

(** TJIM: from ciltools.ml **)
module Ciltools = struct
let makeZeroInitCheck tp =
  match tp with
    TArray(_, Some len , _)  -> begin
      match constFold !Cil.lowerConstants len with
	(Const(CInt64(_, _, _))) -> true
      | _ -> false
    end
  | _ -> true
let rec string_substitute s c b =
  try
    let i = String.index s c in
    let s1 = String.sub s 0 i in
    let s2 = String.sub s (i+1) ((String.length s) - (i+1)) in
    s1 ^ b ^ (string_substitute s2 c b)
  with Not_found ->
    s
(** START OF deepHasAttribute ************************************************)
let visited = ref []
class attribute_checker target rflag = object (self)
  inherit nopCilVisitor
  method vtype t =
    match t with
      TComp(cinfo, a) ->
	if(not (List.exists (fun x -> cinfo.cname = x) !visited )) then begin
	  visited := cinfo.cname :: !visited;
	  List.iter
	    (fun f ->
	      if (hasAttribute target f.fattr) then
		rflag := true
	      else
		ignore(visitCilType (new attribute_checker target rflag)
			 f.ftype)) cinfo.cfields;
	end;
	DoChildren
    | TNamed(t1, a) ->
	if(not (List.exists (fun x -> t1.tname = x) !visited )) then begin
	  visited := t1.tname :: !visited;
	  ignore(visitCilType (new attribute_checker target rflag) t1.ttype);
	end;
	DoChildren
    | _ ->
	DoChildren
  method vattr (Attr(name,params)) =
    if (name = target) then rflag := true;
    DoChildren
end

let deepHasAttribute s t =
  let found = ref false in
  visited := [];
  ignore(visitCilType (new attribute_checker s found) t);
  !found
(** END OF deepHasAttribute **************************************************)

(*****************************************************************************
 * A transformation to make every instruction be in its own statement.
 ****************************************************************************)

class callBBVisitor = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
      Instr(il) -> begin
	if (List.length il > 1) then
          let list_of_stmts = List.map (fun one_inst ->
            mkStmtOneInstr one_inst) il in
          let block = mkBlock list_of_stmts in
	  s.skind <- Block block;
	  ChangeTo(s)
	else
	  SkipChildren
      end
    | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end

let one_instruction_per_statement f =
  let thisVisitor = new callBBVisitor in
  visitCilFileSameGlobals thisVisitor f

(*****************************************************************************
 * A transformation that gives each variable a unique identifier.
 ****************************************************************************)

class resetVidVisitor = object
  inherit nopCilVisitor

  method vvdec v =
    v.vid <- -1 ;
    DoChildren

  method vvrbl v =
    v.vid <- -1 ;
    SkipChildren

end
class setVidVisitor = object
  inherit nopCilVisitor

  method vvdec v =
    if (v.vid = -1) then
      v.vid <- newVID () ;
    DoChildren

  method vvrbl v =
    if (v.vid = -1) then
      v.vid <- newVID () ;
    SkipChildren
end

let globally_unique_vids f =
(* (*TJIM: no longer supported by Cil module *) setGlobalVID 0;*)
  visitCilFileSameGlobals (new resetVidVisitor) f;
  visitCilFileSameGlobals (new setVidVisitor) f;
end
open Ciltools
(** TJIM: end ciltools.ml **)

(** TJIM: from entries.ml **)
module Entries = struct
let entry_points = ref (StringSet.add "main" StringSet.empty)

let entry_filter g =
  match g with
    GFun (fd, _) when
      (StringSet.mem fd.svar.vname !entry_points) -> true
  | GVar (v,_,_)
  | GVarDecl (v,_) when (StringSet.mem v.vname !entry_points) -> true
  | _ -> false
end
(** TJIM: end entries.ml **)

exception Bug

(** Have debugging output when code is bloating **)

let debug = ref false
let none_to_main = ref false
let not_to_main = ref []
let bzero_threshold = ref (-1)
let noconst = ref false
let labelvalues = ref false

(* Keep in mind multiple entry points? How about re-entrancy? *)
  (* DEFINITELY HANDLE THIS ^ *)
(* Keep in mind composition with other tools *)

(* From Xuejun
 * 1) the flattened file name should in the form of
 *    "[original_name]_flatten.c".  for example, when processing
 *    "prog.c", the output file name should be "prog_flatten.c"
 *
 * 2) To avoid name collisions, all variable names should be prefixed
 *    with their function name. For example, if there is a variable "i"
 *    in function "foo", the name becomes "_foo_i" after flattening. For
 *    gobal variables, the prefix is "_static_".
 *)

(* Maybe eventually do function pointers? *)

(* Check that a function can flatten to main first, then do it. *)
(* Avoid recursive functions *)
(* Make a "return" local and store to that in end *)
(* Rename all locals using names from Xuejun *)
(* Have call and return changed as outlined by John's code *)

(* Handle static variables? *)

(*****************************************************************************)

(* This could be done as a preprocessing step instead of for each variable *)
let recursive_check name cg =
  let visited = ref [name] in
  let rec do_it current =
    let node = Hashtbl.find cg current in
    Inthash.fold
      (fun i d acc ->
	if acc then true
	else
	  match d.cnInfo with
	    NIVar (vi,br) when vi.vname = name ->
	      true
	  | NIVar (vi,br) ->
	      if List.mem vi.vname !visited then acc
	      else begin
		visited := vi.vname :: !visited ;
		do_it vi.vname
	      end
	  | NIIndirect (_,vilr) ->
	      List.fold_left
		(fun acc vi ->
		  acc || (do_it vi.vname))
		false
		!vilr
		(* Not sure what I was smoking with this one?
		   print_endline "FOO";
		   true
		 *)
		(* Note: not really recursive, but should be weeded out
		 * by addrof predicate in calling function *) )
      node.cnCallees
      false
  in
  do_it name

let get_descendants cg ancestor=
  let visited = ref [ancestor] in
  let rec do_it current =
    let node = Hashtbl.find cg current in
    Inthash.fold
      (fun i d acc ->
	match d.cnInfo with
	  NIVar (vi,br) ->
	    if List.mem vi.vname !visited then acc
	    else begin
	      visited := vi.vname :: !visited ;
	      (do_it vi.vname) @ (vi.vname :: acc)
	    end
	| NIIndirect (_,vilr) -> acc) (* should be filtered later *)
      node.cnCallees
      []
  in
  do_it ancestor

let fcns_to_flatten = ref []
let use_flattening_list = ref false

let get_flattenable f cg s =
  (** Pass in entry points here **)
  (** Give option for different algorithms **)
  (** Beefier analysis **)
  let descendants = get_descendants cg s in
  foldGlobals f
    (fun acc g ->
      match g with
	GFun(fd,l) ->
	  if ((fd.svar.vaddrof) || (* is address-taken *)
	      (not (List.mem fd.svar.vname descendants)) || (* not desc *)
	      (recursive_check fd.svar.vname cg) || (* is recursive *)
	      ((!use_flattening_list) &&
	       (not (List.mem fd.svar.vname !fcns_to_flatten))))
	  then begin
	    if (!debug) && (List.mem fd.svar.vname descendants) then begin
	      print_string ("Not flattening " ^ fd.svar.vname ^ " into " ^ s ^
			    ": ");
	      if (fd.svar.vaddrof) then
		print_string "(Aliased) ";
	      if (recursive_check fd.svar.vname cg) then
		print_string "(Recursive) ";
	      if ((!use_flattening_list) &&
		  (not (List.mem fd.svar.vname !fcns_to_flatten))) then
		print_string "(List) ";
	      print_newline ();
	    end;
	    acc (* No flattening *)
	  end else
	    fd::acc (* Flattening *)
      | _ -> acc)
    []

let mkswitchret fname hash retsite loc =
  let revstmtlist =
    match Inthash.tolist hash with
    | [] -> failwith "Error: flattener with no calls"
    | (id,stmt)::tl ->
	let gtstmt = mkStmt (Goto ((ref stmt), loc)) in
	gtstmt.labels <- [Default(loc)];
        gtstmt::(List.map
                   (fun (id,stmt) ->
	             let gtstmt = mkStmt (Goto ((ref stmt), loc)) in
	             gtstmt.labels <- [Case (integer id, loc)];
	             gtstmt)
                   tl) in
  let stmtlist = List.rev revstmtlist in
  mkStmt
    (Switch (Lval(Var retsite, NoOffset), mkBlock stmtlist, stmtlist, loc))

(*****************************************************************************)
class updateLabelVisitor fname = object (self)
  inherit nopCilVisitor as super
  method vstmt s =
    s.labels <- List.map
	(fun l ->
	  match l with
	    Label(str,loc,b) -> Label("_" ^ fname ^ "_" ^str,loc,b)
	  | _ -> l)
	s.labels;
    DoChildren
end

class variableReplaceVisitor vi newvi = object (self)
  inherit nopCilVisitor as super
  method vvrbl v =
    if v.vid = vi.vid then
      ChangeTo(newvi)
    else
      SkipChildren
end

    (* pass in:
     * list of temporary arguments
     * return location variable
     * body head for stmt *)
class callReplaceVisitor fd newformals retsite retval count hsh = object (self)
  inherit nopCilVisitor as super
  method vstmt s =
    match s.skind with
      (* looking for specific function approach *)
      Instr((Call(lvo,Lval(Var fvi,NoOffset),el,loc))::_)
      when fvi.vname = fd.svar.vname ->
	(* assign temporary arguments *)
	let arg_assignments =
	  List.map2
	    (fun nf arg ->
	      mkStmtOneInstr (Set((Var nf,NoOffset),arg,loc)))
	    newformals
	    el
	in
	(* set return location *)
	let ret_assignment =
	  mkStmtOneInstr (Set((Var retsite,NoOffset), integer !count, loc))
	in
	(* create goto *)
	let goto_stmt =
	  mkStmt (Goto ((ref (List.hd fd.sbody.bstmts)), loc))
	in
	(* _b_ret_label_1 *)
	(* assign return (with label) (lvo) *)
	let return_stmt =
	  match lvo with
	    Some (lv) ->
	      mkStmtOneInstr (Set(lv, Lval(Var retval,NoOffset), loc))
	  | None ->
	      mkEmptyStmt ()
	in
	(* add label *)
	let labelname =
	  "_" ^ fd.svar.vname ^ "_ret_label_" ^ (string_of_int !count)
	in
	return_stmt.labels <- [Label(labelname,loc,false)];
	Inthash.add hsh !count return_stmt;

	(* put it all together *)
	let newblock =
	  mkBlock
	    (arg_assignments @ [ ret_assignment ; goto_stmt ; return_stmt ])
	in
	s.skind <- (Block newblock);

	count := !count+1;
	SkipChildren
    | _ -> DoChildren
end

class returnReplaceVisitor fd hash flag retsite retval = object (self)
  inherit nopCilVisitor as super
  method vstmt s =
    match s.skind with
      Return (eo,loc) ->
	flag := true;
	let retstmt =
	  match eo with
	    Some e ->
	      mkStmtOneInstr (Set((Var retval,NoOffset), e, loc))
	  | None ->
	      mkEmptyStmt ()
	in
	let switchstmt = mkswitchret fd.svar.vname hash retsite loc in
	s.skind <-(Block (mkBlock [retstmt ; switchstmt]));
	SkipChildren
    | _ ->
	DoChildren
end
(*****************************************************************************)

(* Flattens down to a given function *)
let opportunistic_flattening f cg s =

  (* Find function with name of s in file f *)
  let destination =
    foldGlobals f
      (fun acc g ->
	match g with GFun(fd,l) when fd.svar.vname = s -> fd | _ -> acc)
      dummyFunDec
  in
  if destination.svar.vname <> s then raise Bug;

  (* rename all locals and arguments of destination *)
  List.iter
    (fun vi ->
      vi.vname <- "_" ^ destination.svar.vname ^ "_" ^ vi.vname)
    (destination.slocals @ destination.sformals);

  (* Collect list of functions to flatten *)
  let fundecs =  get_flattenable f cg s in
  (* Copy and prep *)
  let newfundecs =
    List.rev_map (fun fd ->
      let newfd = copyFunction fd fd.svar.vname in
      (* make into one return *)
      Oneret.oneret newfd;

      newfd) fundecs
  in

  (* For each function (in place in list) *)
  List.iter
    (fun fd ->

      (* Modify all the labels! *)
      ignore(visitCilStmt
	       (new updateLabelVisitor fd.svar.vname)
	       (mkStmt (Block(fd.sbody))));

      (* lift and replace all the arguments *)
      (* lift and replace all the locals *)
      let mknewvars vlist =
	List.fold_left
	  (fun acc vi ->
	    let newname = "_" ^ fd.svar.vname ^ "_" ^ vi.vname in
	    let newvi = makeLocalVar destination newname vi.vtype in
	    ignore(visitCilStmt
		     (new variableReplaceVisitor vi newvi)
		     (mkStmt (Block(fd.sbody))));
	    acc @ [newvi])
	  []
	  vlist
      in
      let newformals = mknewvars fd.sformals in
      ignore (mknewvars fd.slocals);

      (* create the return site storage thing *)
      let retsitename = "_" ^ fd.svar.vname ^ "_ret_site" in

      (* Was uintType but is now a char *)
      (* has a check elsewhere *)
      let retsite = makeLocalVar destination retsitename charType in

      (* create the return value storage thing *)
      let retvalname = "_" ^ fd.svar.vname ^ "_ret_val" in
      let retvaltype,_,_,_ = splitFunctionTypeVI fd.svar in
      let retval = makeLocalVar destination retvalname retvaltype in

      (* replace and count all the calls *)
      let count = ref 0 in (* pascal numbering *)
      let retdesthash = Inthash.create 20 in
      List.iter
	(fun myfd ->
	  ignore(visitCilFunction
		   (new callReplaceVisitor
		      fd newformals retsite retval count retdesthash)
		   myfd))
	(destination::newfundecs);

      (* check to make sure count does not overflow char *)
      if (!count > 256) then begin
	print_endline "ERROR: Number of return sites excedes 256";
	raise Bug;
      end;

      (* YOU ARE HERE *)
      if (!debug) then begin
	print_string ("Flattening " ^ fd.svar.vname ^ ": callsites=");
	print_int !count;
	print_newline ();
      end;

      (* create return code based on total and other stuff *)
      let flag = ref false in
      ignore(visitCilStmt
	       (new returnReplaceVisitor fd retdesthash flag retsite retval)
	       (mkStmt (Block(fd.sbody))));
      let post_return =
	if (!flag) then
	  []
	else
	  [mkswitchret fd.svar.vname retdesthash retsite locUnknown]
      in
      fd.sbody.bstmts <- fd.sbody.bstmts @ post_return;

      (* lable first statement with function name *)
      let first = List.hd fd.sbody.bstmts in
      let flabelname = 	"_" ^ fd.svar.vname ^ "_func" in

      first.labels <- Label(flabelname,locUnknown,false) :: first.labels;

      (* ignore(Pretty.printf "%a\n" d_stmt first); *)

      ())
    newfundecs;

  (* drop in functions at end of main *)
  List.iter
    (fun fd ->
      let deadstmt = mkStmt (If(zero, fd.sbody, mkBlock [],locUnknown)) in
      destination.sbody.bstmts <- destination.sbody.bstmts @ [deadstmt] )
    newfundecs

(* keep track of what I have already flattened and change as things get
 * (I'm talking about CALLS here)
 * put into main *)


(*****************************************************************************)
class dollarVisitor = object (self)
  inherit nopCilVisitor as super
  method vvdec v =
    let newname = string_substitute v.vname '$' "_xx_" in
    v.vname <- newname;
    DoChildren
end
let remove_dollars f =
  ignore(visitCilFileSameGlobals (new dollarVisitor) f)
(*****************************************************************************)
let main_to_end f =
  let rec doit l =
    match l with
      [] -> []
    | GFun(func,loc) :: tl when func.svar.vname = "main" ->
	tl @ [GFun(func,loc)]
    | hd::tl ->
	hd :: (doit tl)
  in
  f.globals <- doit f.globals

(*****************************************************************************)
class clearReferenceVisitor = object (self)
  inherit nopCilVisitor as super
  method vvdec v =
    v.vreferenced <- false;
    DoChildren
end
class setReferenceVisitor = object (self)
  inherit nopCilVisitor as super
  method  vvrbl v =
    v.vreferenced <- true;
    DoChildren
end

(******************************************)

    (* If the variable's address is taken anywhere in an interrupt *)
    (* This case will already be covered by the referenced visitor *)

    (* If the variable's address is taken by a global ... *)
    (* and that global is used in an interrupt *)
    (* or used anywhere other than dereferencing *)

    (* If global variable aliases something and that is referenced or aliased,
       then no go. Also, if main aliases something and that is aliased, then no
       go either *)

type hash  = (Cil.varinfo, Cil.varinfo list) Hashtbl.t

class setAliasVisitor aliases = object (self)
  inherit nopCilVisitor as super
      (* need to know which global I am at... *)
  val mutable g = makeVarinfo false "dummy" intType
  method vvdec v =
    g <- v;
    DoChildren
  method  vexpr e =
    match e with
      AddrOf(Var v, _)
    | StartOf(Var v, _) -> begin
	try
	  let references = Hashtbl.find aliases v in
	  if (not (List.mem g references)) then
	    Hashtbl.replace aliases v (g :: references);
	  DoChildren
	with Not_found ->
	  Hashtbl.add aliases v [g];
	  DoChildren
    end
    | _ ->
	DoChildren
end

(* returns true if variable is okay *)
let check_aliases v aliases check =
  try
    let references = Hashtbl.find aliases v in
    List.fold_left
      (fun b a ->
	(check a) && b)
      true
      references
  with Not_found ->
    true


(******************************************)
let goes_in_code x =
  (deepHasAttribute "progmem" x.vtype) ||
  (hasAttribute "progmem" x.vattr) ||
  (List.exists
     (fun y ->
       match y with
	 Attr(s,[AStr(p)]) when (s = "section") && (p = ".eeprom") -> true
       | _ -> false )
     x.vattr) ||
  ((!noconst) &&
   ((deepHasAttribute "const" x.vtype) ||
    (hasAttribute "const" x.vattr)))

let vars_to_main f =
  (* clear references *)
  ignore(visitCilFileSameGlobals (new clearReferenceVisitor) f);
  (* A hashtable for some of the aliases *)
  let aliases = Hashtbl.create 511 in
  (* set references for outside of main *)
  iterGlobals f
    (fun g ->
      match g with
      | GFun (func,loc) when func.svar.vname <> "main" ->
	  ignore(visitCilFunction (new setReferenceVisitor) func)
      | GFun (func,loc) when func.svar.vname = "main" ->
          ()
      | GVar(v,i,l) -> ignore(visitCilGlobal (new setAliasVisitor aliases) g)
      | _ -> ());

  if (!debug) then begin
    print_endline "\nAliases:";
    Hashtbl.iter
      (fun v l ->
	ignore (Pretty.printf "%a : " d_lval (Var v,NoOffset) );
	List.iter
	  (fun x ->
	    ignore (Pretty.printf "%a " d_lval (Var x, NoOffset) ))
	  l;
	print_newline ())
      aliases;
  end;

  (* pull out variables to move into main *)
  if (!debug) then
    print_endline "\nFailed to move to main:";
  let tomove, tostay =
    List.fold_right
      (fun g (lm,ls) ->
	(* bad init, function prototype, storage Extern *)
	let rec check_valid v =
	  ((not !none_to_main) &&
           (not v.vreferenced) &&
	   (* (not (is_volatile_vi v)) &&  Maybe okay? Think of examples... *)
	   (v.vstorage <> Extern)&&
	   (not (isFunctionType v.vtype)) &&
	   (not (goes_in_code v)) &&
	   (makeZeroInitCheck v.vtype) &&
	   (check_aliases v aliases check_valid) &&
	   (not (List.mem v.vname !not_to_main)))
	in
	match g with
	  GVarDecl (vinfo, loc) when check_valid vinfo ->
	    if (List.exists (fun x -> (fst x).vid = vinfo.vid) lm) then
	      (lm, ls)
	    else
	      ((vinfo,{init=None})::lm,ls)
	| GVar (vinfo,iinfo,loc) when check_valid vinfo ->
	    vinfo.vname <- "_static_" ^ vinfo.vname;
	    if (vinfo.vstorage = Static) then vinfo.vstorage <- NoStorage ;
	    let newlm = List.filter (fun x -> (fst x).vid <> vinfo.vid) lm in
	    ((vinfo,iinfo)::newlm,ls)
	| GVar (v,_,_) ->
	    v.vname <- "_global_" ^ v.vname;
	    if (!debug) then begin
	      print_string ("Fails a check (" ^ v.vname ^ "):");
	      if (v.vreferenced <> false) then print_string " referenced";
	      if not (makeZeroInitCheck v.vtype) then print_string " init";
	      if (isFunctionType v.vtype) then print_string " function";
	      if (v.vstorage = Extern) then print_string " storage";
	      if (not (check_aliases v aliases check_valid)) then
		print_string " aliased";
	      if (goes_in_code v) then
		print_string " progmem";
	      print_newline ();
	    end;
	    (lm, g::ls)
	| _ -> ( lm, g::ls  ) )
      f.globals
      ([],[])
  in
  (* delete them *)
  f.globals <- tostay;
  (* find main *)
  let main =
    let res =
      foldGlobals f
	(fun a g ->
	  match g with
	    GFun(func,loc)  when func.svar.vname = "main" -> Some(func)
	  | _ -> a )
	None
    in
    match res with
      None -> raise Bug
    | Some (cosa) -> cosa
  in
  (* make all variables local *)
  List.iter (fun x -> (fst x).vglob <- false) tomove;
  (* remove all const attributes *)
  let filter_const_type incoming  =
    let const_filtered_cis = ref [] in
    let rec rec_filter_const_type x =
      let y = typeRemoveAttributes ["const"] x in
      match y with
	TArray(ytp,yeo,yas) ->
	  TArray((rec_filter_const_type ytp), yeo, yas)
      | TNamed (yti,_) ->
	  yti.ttype <- rec_filter_const_type yti.ttype;
	  y
      | TComp(yci,_) when (not (List.mem yci.cname !const_filtered_cis))->
	  const_filtered_cis := yci.cname :: !const_filtered_cis;
	  yci.cattr <- dropAttribute "const" yci.cattr;
	  List.iter
	    (fun ycfi ->
	      ycfi.fattr <- dropAttribute "const" ycfi.fattr;
	      ycfi.ftype <- rec_filter_const_type ycfi.ftype )
	    yci.cfields;
	  y
      | _ -> y
    in
    rec_filter_const_type incoming
  in
  List.iter
    (fun x ->
      let vi = (fst x) in
      vi.vattr <- dropAttribute "const" vi.vattr;
      vi.vtype <- filter_const_type vi.vtype )
    tomove;


  (* add variables *)
  main.slocals <- main.slocals @ (List.map (fun x -> (fst x)) tomove);
  (* make initializers (zero for none) *)
  let newinstrs =
    List.fold_left
      (fun instrs (vinfo, iinfo) ->
	let rec recursive_init var off init =
	  match init with
	    SingleInit(e) ->
	      [Set((Var var, off), e, locUnknown)]
	  | CompoundInit(tp,oil) ->
	      let fold_func (o:offset) (i:init) (ot:typ) acc =
		let new_o = addOffset o off in
		(recursive_init var new_o i) @ acc
	      in
	      Cil.foldLeftCompound
                ~implicit:true
(* (*TJIM*)
	      Cil.foldLeftCompoundAll
*)
		~doinit:fold_func
		~ct:tp
		~initl:oil
		~acc:[]
	in
	let vinit, size, bzeroflag =
	  match iinfo.init with
	    None -> begin
	      let res =	makeZeroInit vinfo.vtype in
	      try
		let bits = bitsSizeOf vinfo.vtype in
		let bytes = bits / 8 in
		res, bytes,
		((!bzero_threshold <> -1) && (bytes > !bzero_threshold))
	      with SizeOfError _ ->
		res, 0, false
	    end
	  | Some (i) -> i,0,false
	in

	let newinstrs =
	  if (bzeroflag) then
	    let bzerovi =
	      makeVarinfo true "bzero"
		(TFun(voidType,
		      Some([("s",charPtrType,[]);("n",intType,[])]),
		      false,
		      []))
	    in
	    [Call(None,
		  Lval(Var bzerovi,NoOffset),
		  [CastE(charPtrType,AddrOf(Var vinfo, NoOffset));
		   integer size],
		  locUnknown)]
	  else
	    recursive_init vinfo NoOffset vinit
	in
	newinstrs @ instrs )
      []
      tomove
  in
  (* add isntrs to main *)
  main.sbody.bstmts  <- (mkStmt (Instr(newinstrs))) :: main.sbody.bstmts ;
  setMaxId main;
  ()

let doit (f: file) =
      (* Replace all '$' in variable names with '_xx_' *)
      remove_dollars f;

      (* Make constants progmem *)
      (*
	 iterGlobals f
	 (fun x ->
	 match x with
	 GVar (vi, ii, loc) when
	 (not (goes_in_code vi)) &&
	 ((deepHasAttribute "const" vi.vtype) or
	 (hasAttribute "const" vi.vattr))
	 ->
	 vi.vattr <- addAttribute (Attr("progmem",[])) vi.vattr
	 | _ -> ());
       *)

      (* Move main to end of program *)
      main_to_end f;

      globally_unique_vids f;
      one_instruction_per_statement f; (* The world I am used to working in *)
      let graph = Callgraph.computeGraph f in

      (* Now pull info from entry points... (default is just main) *)
      let cosas =
	foldGlobals f
	  (fun acc g ->
	    match g with
	      GFun(fd,l)
	      when StringSet.mem fd.svar.vname !Entries.entry_points ->
		fd.svar.vname :: acc
	    | _ -> acc )
	  []
      in
      List.iter (opportunistic_flattening f graph) cosas;

      (* Do a cleanup pass *)
      Rmtmps.removeUnusedTemps ~isRoot:Entries.entry_filter f;

      (* Move globals only accessed in main into main *)
      vars_to_main f;

      (* Do another cleanup pass? *)
      Rmtmps.removeUnusedTemps ~isRoot:Entries.entry_filter f;

      (* Make everything other than entry points static *)
      iterGlobals f
	(fun x ->
	  match x with
	    GVar (vi, _, _) ->
	      vi.vstorage <- Static
	  | GFun (fd, _)
	    when (not (StringSet.mem fd.svar.vname !Entries.entry_points))->
	      fd.svar.vstorage <- Static
	  | _ -> ());

      ()

(*******************************************************************************)
(* Set up flattening as a feature in CIL.  We don't use this directly anymore, *)
(* it is retained for possible future use.                                     *)
(*******************************************************************************)

let feature : Feature.t =
  { fd_name = "flattener";
    fd_enabled = false;
    fd_description = "Flattens C code into main for Xuejun's work.";
    fd_extraopt = [
    ("--no_main_entry",
     Arg.Unit (fun _ ->
       Entries.entry_points := StringSet.remove "main" !Entries.entry_points ),
     "Do not consider main to be an entry point.");
    ("--none-to-main",
     Arg.Unit (fun _ -> none_to_main := true),
     "Do not move globals into main.");
    ("--add_entry",
     Arg.String (fun s ->
       Entries.entry_points := StringSet.add s !Entries.entry_points ),
     "String following this is another entry point (beyond \"main\"). For now, must be of the form '--clean_entry=function_name' because of weird cil processing.");
    ("--flatten_debug",
     Arg.Unit (fun _ -> debug := true),
     "Turns on debugging output for flattener");
    ("--flatten_labels_as_values",
     Arg.Unit (fun _ -> labelvalues := true),
     "Makes the flattener use labels as values instead of using switches");
    ("--flatten_no_const",
     Arg.Unit (fun _ -> noconst := true),
     "No longer lift constant globals into main, even if possible");
    ("--flatten_bzero",
     Arg.Int(fun i ->
       bzero_threshold := i),
     "Non-initialized variables over size specified (in bytes) initialized with bzero");
    ("--flatten_nomain",
     Arg.String (
     fun (st:string) -> not_to_main := st :: !not_to_main),
     "Flattener does not move this global into main");
    ("--flatten_list",
     Arg.String (
     fun (st:string) ->
       fcns_to_flatten := st :: !fcns_to_flatten;
       use_flattening_list := true),
     "Flattener does not move this global into main");  ];
    fd_doit = doit;
    fd_post_check = true;
  }

let () = Feature.register feature

end (* module Flattener *)

(* Cil printer that does not print #line directives and compiler builtins *)
class cilPrinterClass = object
  inherit defaultCilPrinterClass as super
  method! pGlobal () = function
    | GVarDecl(vi,l) when (not !printCilAsIs && Hashtbl.mem Cil.builtinFunctions vi.vname) -> Pretty.nil
    | g -> super#pGlobal () g
  method! pLineDirective ?(forcefile=false) l = Pretty.nil
end

(* Flatten to out_channel *)
let doit_outch infile outch =
  initCIL ();
  let cil = (Frontc.parse infile ()) in
  Flattener.none_to_main := true;
  Flattener.feature.fd_doit cil;
  let pr = new cilPrinterClass in
  iterGlobals cil (dumpGlobal pr outch)

(* Flatten to file *)
let doit infile outfile =
  let outch = open_out outfile in
  doit_outch infile outch;
  close_out outch

(* Main program, useful for testing *)
let main() =
  if Array.length(Sys.argv) <> 2 then (Printf.eprintf "Usage: %s <file>\n" (Filename.basename Sys.argv.(0)); exit 1);
  let file = Sys.argv.(1) in
  if not (Sys.file_exists file) then (Printf.eprintf "File '%s' does not exist\n" file; exit 1);
  doit_outch file stdout
