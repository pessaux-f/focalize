open Parse_file;;

let get_input_file_name, set_input_file_name =
  let input_file_name = ref "-" in
  (fun () -> !input_file_name),
  (fun fname ->
     if !input_file_name = "-" then input_file_name := fname
     else failwith "Input file name is already set.");;

let get_output_file_name, set_output_file_name =
  let output_file_name = ref "" in
  (fun () -> !output_file_name),
  (fun fname ->
     if !output_file_name = "" then output_file_name := fname
     else failwith "Input file name is already set.");;

(* Parsing the command line. *)

let get_usage () =
  "Usage: focal_check <options> <.foc file>";;

let print_focal_version v =
  prerr_endline
   (Printf.sprintf
      "The Focal compiler, version %s" v);
  exit 0;;

let print_focal_short_version () =
  print_focal_version
    (Printf.sprintf "%.2f" Configuration.focal_version_number);;

let print_focal_full_version () =
  print_focal_version Configuration.focal_full_version;;

let set_focal_to_verbose () = Configuration.set_verbose true;;

let check_file_syntax fname =
  try 
    let ast = Parse_file.parse_file Format.err_formatter fname in
    let fname = get_output_file_name () in
    if fname <> "" then
    Printer.print_file ast with
  | _ -> exit 2;;

(* The main procedure *)

let main () =
  Arg.parse [
    ("--verbose", Arg.Unit set_focal_to_verbose,
     " be verbose.");
    ("-v", Arg.Unit print_focal_short_version,
     " print the focal version.");
    ("--version", Arg.Unit print_focal_full_version,
     " print the full focal version, sub-version and release date.");
    ("-c", Arg.String set_input_file_name,
     " check input file argument.");
    ("-p", Arg.String set_output_file_name,
     " print the parse tree of the focal file read into the given file name.");
  ]
  set_input_file_name
  (get_usage ());
 check_file_syntax (get_input_file_name ());
 exit 0;;

main ();;
