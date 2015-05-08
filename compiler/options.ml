type options = {
    mutable help: bool;
    mutable debug_blocks: bool;
    mutable debug_load_store: bool;
    mutable no_cil: bool;
    mutable keep_ll: bool;
    mutable keep_cil: bool;
    mutable delta: bool;
    mutable circuitlib: string option;
    mutable fname: string option;
    mutable output: string option;
    mutable fv: bool;
    mutable pr: bool;
    mutable cfg: bool;
    mutable graph: bool;
    mutable prf: bool;
    mutable branch: bool;
    mutable load_store: bool;
    mutable gep: bool;
    mutable phi: bool;
    mutable optflag: string;
    mutable verbose: bool;
    mutable run: bool;
  }

let options = {
  help = false;
  debug_blocks = false;
  debug_load_store = false;
  no_cil = false;
  keep_ll = false;
  keep_cil = false;
  delta = false;
  circuitlib = None;
  fname = None;
  output = None;
  fv = false;
  pr = false;
  cfg = false;
  graph = false;
  prf = false;
  branch = false;
  load_store = false;
  gep = false;
  phi = false;
  optflag = "-O1";
  verbose = false;
  run = false;
}

let pr_output_file suffix s =
  let outchan, cleanup =
    (match options.output with
    | None -> stdout, (fun () -> ())
    | Some file ->
        let outchan = open_out (file^suffix) in
        outchan, (fun () -> close_out outchan)
    ) in
  Printf.fprintf outchan "%s" s;
  cleanup()
