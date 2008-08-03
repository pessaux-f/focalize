(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: focalizec.ml,v 1.28.2.5 2008-08-03 20:57:54 blond Exp $ *)


exception Bad_file_suffix of string ;;


(* The main procedure *)
let main () =
  Arg.parse
    [ ("--dot-non-rec-dependencies",
       Arg.String Configuration.set_dotty_dependencies,
       " dumps species non-let-rec- dependencies as dotty\n\tfiles into the \
         argument directory.") ;
      ("-i",
       Arg.Unit (fun () -> Configuration.set_do_interface_output true),
       " prints the source file interface.") ;
      ("-I",
       Arg.String (fun path -> Files.add_lib_path path),
       " adds the specified path to the path list where to search for \
         compiled\n\tinterfaces.") ;
      ("--methods-history-to-text",
       Arg.String Configuration.set_methods_history_to_text,
       " dumps species' methods' inheritance history as plain text\n\tfiles \
         into the argument directory.") ;
      ("--no-ansi-escape",
       Arg.Unit Configuration.unset_fancy_ansi,
       " disables ANSI escape sequences in the error messages.") ;
      ("--no-coq-code",
       Arg.Unit Configuration.unset_generate_coq,
       " disables the Coq code generation.") ;
      ("--no-ocaml-code",
       Arg.Unit Configuration.unset_generate_ocaml,
       " disables the OCaml code generation.") ;
      ("--c-code",
       Arg.Unit Configuration.set_generate_c,
       " enables the C code generation.") ;
      ("-no-stdlib-path",
       Arg.Unit Configuration.unset_use_default_lib,
       " does not include by default the standard library installation\n\t\
         directory in the search path.") ;
      ("--pretty",
       Arg.String Configuration.set_pretty_print,
       " pretty-prints the parse tree of the focal file as a focal \
         source\n\t into the argument file.") ;
      ("--raw-ast-dump",
       Arg.Unit Configuration.set_raw_ast_dump,
       " (undocumented) prints on stderr the raw AST structure \
         after\n\tparsing stage.") ;
      ("--scoped_pretty",
       Arg.String Configuration.set_pretty_scoped,
       " (undocumented) pretty-prints the parse tree of the focal \
         file\n\tonce scoped as a focal source into the argument file.") ;
      ("--verbose",
       Arg.Unit Configuration.set_verbose,
       " be verbose.") ;
      ("-v", Arg.Unit Configuration.print_focal_short_version,
       " prints the focalize version then exit.") ;
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " prints the full focalize version, sub-version and release date,\n\t\
         then exit.") ;
       ("--where",
        Arg.Unit Configuration.print_install_dirs,
        " prints the binaries and libraries installation directories then \
          exit.")
     ]
    Configuration.set_input_file_name
    "Usage: focal_check <options> <.foc file>" ;
  (* First, let's lex and parse the input source file. *)
  let input_file_name = Configuration.get_input_file_name () in
  (* Create the current compilation unit "fname". In fact, this *)
  (* is the current filename without dirname and extention.     *)
  if not (Filename.check_suffix input_file_name ".foc") then
    raise (Bad_file_suffix input_file_name) ;
  let current_unit =
    Filename.chop_extension (Filename.basename input_file_name) in
  (* Include the installation libraries directory in the search path. *)
  if Configuration.get_use_default_lib () then
    Files.add_lib_path Installation.install_lib_dir ;
  (* Parse the file. *)
  let ast = Parse_file.parse_file input_file_name in
  (* Hard-dump the AST if requested. *)
  if Configuration.get_raw_ast_dump () then
    Dump_ptree.pp_file Format.err_formatter ast ;
  (* Pretty the AST as a new-focal-syntax source if requested. *)
  (match Configuration.get_pretty_print () with
   | None -> ()
   | Some fname ->
       let out_hd = open_out_bin fname in
       let out_fmt = Format.formatter_of_out_channel out_hd in
       Sourcify.pp_file out_fmt ast ;
       close_out out_hd) ;
  (* Scopes AST. *)
  let (scoped_ast, scoping_toplevel_env) =
    (let tmp = Scoping.scope_file current_unit ast in
    (* Pretty the scoped AST if requested. *)
    (match Configuration.get_pretty_scoped () with
     | None -> ()
     | Some fname ->
         let out_hd = open_out_bin fname in
         let out_fmt = Format.formatter_of_out_channel out_hd in
         Sourcify.pp_file out_fmt (fst tmp) ;
         close_out out_hd) ;
    tmp) in
  (* Typechecks the AST. *)
  let (typing_toplevel_env, stuff_to_compile) =
    Infer.typecheck_file ~current_unit scoped_ast in
  (* Now go to the OCaml code generation if requested. *)
  let mlgen_toplevel_env =
    if Configuration.get_generate_ocaml () then
      (begin
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".ml" in
      Some
        (Main_ml_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
      end)
    else None in
  (* Finally, go to the Coq code generation if requested. *)
  let coqgen_toplevel_env =
    if Configuration.get_generate_coq () then
      (begin
      let out_file_name = (Filename.chop_extension input_file_name) ^ ".v" in
      Some
        (Main_coq_generation.root_compile
           ~current_unit ~out_file_name stuff_to_compile)
      end)
    else None in
  (* Generating the C code if requested. *)
  begin
    if Configuration.get_generate_c () then
      begin
	let Parsetree.File l = scoped_ast.Parsetree.ast_desc in
	let stuff = (input_file_name, List.combine l stuff_to_compile) in
	Genc.compile stuff
      end
  end;
  (* Now, generate the persistent interface file. *)
  Env.make_fo_file
    ~source_filename: input_file_name
    scoping_toplevel_env typing_toplevel_env mlgen_toplevel_env
    coqgen_toplevel_env ;
  exit 0
;;
