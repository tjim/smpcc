let ch, module_id =
  if Array.length Sys.argv > 1 then
    open_in Sys.argv.(1), Sys.argv.(1)
  else
    stdin, "<stdin>"

let cu = Lllex.parse ch
;;
Printf.printf "; ModuleID = '%s'\n%s" module_id (Llabs.spr Llabs.bpr_cu cu);;
