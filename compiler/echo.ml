let ch =
  if Array.length Sys.argv > 1 then
    open_in Sys.argv.(1)
  else
    stdin

let cu = Lllex.parse ch
;;
Printf.printf "%s" (Util.spr Util.bpr_cu cu);;
