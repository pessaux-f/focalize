(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.17 2007-07-25 19:41:39 doligez Exp $  *)

open Misc;;
open Printf;;

let infiles = ref [];;

let speclist = Options.get_options ();;

let anon s = infiles := s :: !infiles;;

let usage = "usage: zvtov {option} <file.zv>\noptions are:"

let do_file inf =
  let ic = open_in_bin inf in
  let lb = Lexing.from_channel ic in
  let base = try Filename.chop_extension inf with _ -> inf in
  let ouf = base ^ ".v" in
  let oc = open_out_bin ouf in
  Cache.init base Version.version (Invoke.signature ());
  Parser.parse inf lb oc;
  Cache.close ();
  close_out oc;
  close_in ic;
;;

let main () =
  Arg.parse speclist anon usage;
  match !infiles with
  | [] ->
      eprintf "error: no input file specified\n";
      Arg.usage speclist usage;
  | [f] -> do_file f;
  | l ->
      let f x =
        if !progress_level > 0 then begin
          eprintf "%s        \n" x;
          flush stderr;
        end;
        do_file x;
      in
      List.iter f (List.rev l);
;;

Sys.catch_break true;
try main () with
| Misc.Error msg -> eprintf "Error: %s\n" msg; exit 5;
| Sys_error msg -> eprintf "%s\n" msg; exit 3;
| Sys.Break -> Cache.close (); eprintf "interrupt\n"; exit 4;
;;
