(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.3 2004-06-02 17:08:10 doligez Exp $  *)

let print_version () =
  Printf.printf "zvtov: version 0.1.0 [2004-06-02]";
  exit 0;
;;

let infile = ref None;;

let speclist = [
  "-p", Arg.Set Invoke.progress_flag,
     "           display progress messages";
  "-v", Arg.Unit print_version,
     "           print version string and exit";
  "-zenon", Arg.Set_string Invoke.zcmd,
    Printf.sprintf "<command>  how to invoke zenon (default: \"%s\")"
                   !Invoke.zcmd;
  "-zopt", Arg.Set_string Invoke.zopt,
    Printf.sprintf "<options>  options passed to zenon\n%s(default: \"%s\")"
                   (String.make 20 ' ') !Invoke.zopt;
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
