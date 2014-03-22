type options = {
    mutable help: bool;
    mutable debug_blocks: bool;
    mutable debug_load_store: bool;
    mutable cil: bool;
    mutable delta: bool;
    mutable circuitlib: string option;
    mutable fname: string option;
    mutable output: string option;
    mutable fv: bool;
    mutable pr: bool;
    mutable cfg: bool;
    mutable prf: bool;
    mutable branch: bool;
    mutable load_store: bool;
    mutable gep: bool;
    mutable phi: bool;
    mutable optflag: string;
  }

let options = {
  help = false;
  debug_blocks = false;
  debug_load_store = false;
  cil = true;
  delta = false;
  circuitlib = None;
  fname = None;
  output = None;
  fv = false;
  pr = false;
  cfg = false;
  prf = false;
  branch = false;
  load_store = false;
  gep = false;
  phi = false;
  optflag = "-O1";
}

let pr_output_file suffix s =
  let outchan, cleanup =
    (match suffix, options.output with
    | "",_
    | _,None -> stdout, (fun () -> ())
    | _,Some file ->
        let outchan = open_out (file^suffix) in
        outchan, (fun () -> close_out outchan)
    ) in
  Printf.fprintf outchan "%s" s;
  cleanup()
