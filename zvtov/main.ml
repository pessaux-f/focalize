(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.5 2004-06-03 19:50:53 doligez Exp $  *)

let print_version () =
  Printf.printf "zvtov: version 0.1.0 [2004-06-03]";
  exit 0;
;;

let infile = ref None;;

let speclist = [
  "-p0", Arg.Unit (fun () -> Invoke.progress_level := 0),
      "    do not display progress info";
  "-p1", Arg.Unit (fun () -> Invoke.progress_level := 1),
      "    display progress bar (default)";
  "-p2", Arg.Unit (fun () -> Invoke.progress_level := 2),
      "    display progress messages";
  "-v", Arg.Unit print_version,
     "     print version string and exit";
  "-v7", Arg.Unit (fun () -> Invoke.coq_version := Invoke.V7),
      "    generate proofs in Coq V7 syntax (default)";
  "-v8", Arg.Unit (fun () -> Invoke.coq_version := Invoke.V8),
      "    generate proofs in Coq V8 syntax";
  "-zenon", Arg.Set_string Invoke.zcmd,
     Printf.sprintf "<command>  how to invoke zenon (default: \"%s\")"
                   !Invoke.zcmd;
  "-zopt", Arg.Set_string Invoke.zopt,
    Printf.sprintf "<options>   options passed to zenon\n%s(default: \"%s\")"
                   (String.make 12 ' ') !Invoke.zopt;
];;

let anon s = infile := Some s;;

let usage = "usage: zvtov {option} <file.zv>\noptions are:"

let main () =
  Arg.parse speclist anon usage;
  match !infile with
  | None ->
      Printf.eprintf "error: no input file specified\n";
      Arg.usage speclist usage;
  | Some inf ->
      let ic = open_in_bin inf in
      let lb = Lexing.from_channel ic in
      let ouf = (Filename.chop_extension inf) ^ ".v" in
      let oc = open_out_bin ouf in
      Parser.parse inf lb oc
;;

main ();;
