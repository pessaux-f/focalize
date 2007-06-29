(* $Id: check_file.ml,v 1.9 2007-06-29 16:19:44 pessaux Exp $ *)


(* The main procedure *)
let main () =
  Arg.parse
    [ ("-v", Arg.Unit Configuration.print_focal_short_version,
       " print the focal version.") ;
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " print the full focal version, sub-version and release date.") ;
      ("-c",
       Arg.String Configuration.set_input_file_name,
       " check input file argument.") ;
      ("--downgrade",
       Arg.Unit Configuration.set_downgrade,
       "mostly undocumented") ;
      ("--pretty",
       Arg.Unit (fun () -> Configuration.set_pretty_print true),
       " pretty-prints the parse tree of the focal file as a focal source.") ;
      ("--verbose",
       Arg.Unit (fun () -> Configuration.set_verbose true),
       " be verbose.") ]
    Configuration.set_input_file_name
    "Usage: focal_check <options> <.foc file>" ;
  (* First, let's lex and parse the input source file. *)
  let ast =
    Parse_file.parse_file
      Format.err_formatter (Configuration.get_input_file_name ()) in
  (* Hard-dump the AST if requested. *)
  if (Configuration.get_verbose ()) then
    Dump_ptree.pp_file Format.err_formatter ast ;
  (* Pretty the AST as a new-focal-syntax source if requested. *)
  if (Configuration.get_pretty_print ()) then
    Sourcify.pp_file Format.err_formatter ast ;
  (* Translate the AST into an old-focal-AST if requested. *)
  if Configuration.get_downgrade () then ignore (New2old.downgrade_file ast) ;
  exit 0
;;



try main ()
with whatever ->
  (* Firewall to catch anything we could have forgotten... *)
  Printf.eprintf "Unexpected error: \"%s\".\n" (Printexc.to_string whatever) ;
  flush stderr
;;
