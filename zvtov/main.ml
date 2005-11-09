(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.15 2005-11-09 15:22:03 doligez Exp $  *)

open Printf;;

let infile = ref None;;

let speclist = Options.get_options ();;

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
      let base = try Filename.chop_extension inf with _ -> inf in
      let ouf = base ^ ".v" in
      let oc = open_out_bin ouf in
      Cache.init base Version.version (Invoke.signature ());
      Parser.parse inf lb oc;
      Cache.close ();
;;

Sys.catch_break true;
try main () with
| Sys_error msg -> eprintf "%s\n" msg; exit 3;
| Sys.Break -> Cache.close (); eprintf "interrupt\n"; exit 4;
;;
