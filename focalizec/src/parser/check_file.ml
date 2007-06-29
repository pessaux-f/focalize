(* $Id: check_file.ml,v 1.8 2007-06-29 15:33:13 pessaux Exp $ *)


(* The main procedure *)
let main () =
  Arg.parse
    [ ("--verbose",
       Arg.Unit (fun () -> Configuration.set_verbose true),
       " be verbose.") ;
      ("-v", Arg.Unit Configuration.print_focal_short_version,
       " print the focal version.") ;
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " print the full focal version, sub-version and release date.") ;
      ("-c",
       Arg.String Configuration.set_input_file_name,
       " check input file argument.") ;
      ("-downgrade",
       Arg.Unit Configuration.set_downgrade,
       "mostly undocumented") ;
      ("-p",
       Arg.String Configuration.set_output_file_name,
       " print the parse tree of the focal file read into the given file name.")
    ]
    Configuration.set_input_file_name
    "Usage: focal_check <options> <.foc file>" ;
  let ast =
    Parse_file.parse_file
      Format.err_formatter (Configuration.get_input_file_name ()) in
  if (Configuration.get_verbose ()) then
    Dump_ptree.pp_file Format.err_formatter ast ;
  if Configuration.get_downgrade () then ignore (New2old.downgrade_file ast);
  exit 0
;;



try main ()
with whatever ->
  Printf.eprintf "Unexpected error: \"%s\".\n" (Printexc.to_string whatever) ;
  flush stderr
;;
