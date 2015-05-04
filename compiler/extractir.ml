(* extractir: a compiler driver for extracting LLVM IR *)

(* If you want to get samples of LLVM IR (to use as inputs to test an
   LLVM parser, for example) and you have a C program that is built
   using a Makefile, you can do

      CC=extractir make

   This will compile the program as usual and also create a .ll file
   for each .c program file.  These .ll files will be placed in the
   current directory.  If you want them placed in a different
   directory use

      CC="extractir -extractir-dir directory" make

   instead, where "directory" is the target directory.

   If using configure you may need

      ./configure CC=extractir
      make
*)

let exec program args =
  let argv = Array.of_list(program::args) in
  let child_pid = Unix.fork() in
  if child_pid = 0 then
    Unix.execvp program argv
  else
    ignore(Unix.waitpid [] child_pid)

let outdir = ref None

let args = match Array.to_list Sys.argv with
| _::"-extractir-dir"::dir::rest ->
    outdir := Some dir;
    rest
| _::rest -> rest
| [] -> failwith "impossible"
;;

if List.exists (fun x -> x = "-c") args then begin
  let llfile =
    let dir =
      match !outdir with
      | None -> "."
      | Some dir -> dir in
    Filename.temp_file ~temp_dir:dir "extractir-" ".ll" in
  let args =
    List.filter (fun x -> x <> "-c") args in
  let args =
    let rec loop = function
      | "-o"::file::tl -> loop tl
      | hd::tl -> hd::(loop tl)
      | [] -> [] in
    loop args in
  let args = "-S"::"-emit-llvm"::"-o"::llfile::args in
  Printf.printf "EXTRACTING TO %s\n%!" llfile;
  exec "clang" args
end;;
exec "gcc" args
