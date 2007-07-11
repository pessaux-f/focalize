(* $Id: check_file.ml,v 1.12 2007-07-11 08:29:50 weis Exp $ *)

(** The focalize concrete syntax file checker. *)

(* The main procedure *)
let main () =
  Arg.parse
    [ ("-v", Arg.Unit Configuration.print_focal_short_version,
       " print the focalize version.");
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " print the full focalize version, sub-version and release date.");
      ("-c",
       Arg.String Configuration.set_input_file_name,
       " check input file argument.");
      ("--pretty",
       Arg.Unit (fun () -> Configuration.set_pretty_print true),
       " pretty-prints the parse tree of the focalize file as \
         a focalize source.");
      ("--old-pretty",
       Arg.String Configuration.set_old_pretty_print,
       " pretty-prints the parse tree of the focalize file as \
         an old focal source.");
      ("--verbose",
       Arg.Unit (fun () -> Configuration.set_verbose true),
       " be verbose.") ]
    Configuration.set_input_file_name
    "Usage: focal_check <options> <.foc file>";
  (* First, let's lex and parse the input source file. *)
  let ast =
    Parse_file.parse_file
      Format.err_formatter (Configuration.get_input_file_name ()) in
  (* Hard-dump the AST if requested. *)
  if Configuration.get_verbose () then
    Dump_ptree.pp_file Format.err_formatter ast;
  (* Pretty the AST as a new-focal-syntax source if requested. *)
  if Configuration.get_pretty_print () then
    Sourcify.pp_file Format.err_formatter ast;
  (* Pretty the AST as an old-focal-syntax source if requested. *)
  (match Configuration.get_old_pretty_print () with
   | None -> ()
   | Some fname ->
     let out_hd = open_out_bin fname in
     let out_fmt = Format.formatter_of_out_channel out_hd in
     Oldsourcify.pp_file out_fmt ast;
     close_out out_hd);
  exit 0
;;

(** If something unexpected arrises when proceeding, we exit with the proper
    error code. *)
try main () with
| x ->
  Printf.eprintf
     "Unexpected error: \"%s\".\
    \nPlease report.\n"
     (Printexc.to_string x);
  flush stderr;
  exit 2
;;
