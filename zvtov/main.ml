(*  Copyright 2004 INRIA  *)
(*  $Id: main.ml,v 1.18 2008-10-23 13:06:47 doligez Exp $  *)

open Misc;;
open Printf;;

let infiles = ref [];;

let speclist = Options.get_options ();;

let anon s = infiles := s :: !infiles;;

let usage = "usage: zvtov {option} <file.zv>\noptions are:"

let current_outfile = ref "";;

let cleanup_and_exit code =
  if !current_outfile <> "" then Sys.remove !current_outfile;
  exit code;
;;

let do_file inf =
  let ic = open_in_bin inf in
  let lb = Lexing.from_channel ic in
  let base = try Filename.chop_extension inf with _ -> inf in
  current_outfile := base ^ ".v";
  let oc = open_out_bin !current_outfile in
  Cache.init base Version.version (Invoke.signature ());
  Parser.parse inf lb oc;
  Cache.close ();
  close_out oc;
  current_outfile := "";
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
| Misc.Error msg -> eprintf "Error: %s\n" msg; cleanup_and_exit 5;
| Sys_error msg -> eprintf "%s\n" msg; cleanup_and_exit 3;
| Sys.Break -> Cache.close (); eprintf "interrupt\n"; cleanup_and_exit 4;
;;
