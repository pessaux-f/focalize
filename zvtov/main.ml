(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.1 2004-05-19 15:13:41 doligez Exp $  *)

let infile = ref None;;

let speclist = [
  "-zenon", Arg.Set_string Invoke.zcmd,
    Printf.sprintf "<command>  how to invoke zenon (default %s)"
                   !Invoke.zcmd;
  "-zopt", Arg.Set_string Invoke.zopt,
    Printf.sprintf "<options>  options passed to zenon (default %s)"
                   !Invoke.zopt;
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
