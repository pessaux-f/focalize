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


(* $Id: focalizec.ml,v 1.15 2007-09-25 11:15:59 pessaux Exp $ *)


exception Bad_file_suffix of string ;;


(* The main procedure *)
let main () =
  Arg.parse
    [ ("-c",
       Arg.String Configuration.set_input_file_name,
       " check input file argument.") ;
      ("--dot-non-rec-dependencies",
       Arg.String Configuration.set_dotty_dependencies,
       " dump species non-let-rec- dependencies as dotty files into the \
	 argument directory.") ;
      ("-i",
       Arg.Unit (fun () -> Configuration.set_do_interface_output true),
       " prints the source file interface.") ;
      ("-I",
       Arg.String (fun path -> Files.add_lib_path path),
       " adds the specified path to the path list where to search for \
	 compiled interfaces.") ;
      ("--no-ocaml-code",
       Arg.Unit Configuration.unset_generate_ocaml,
       " disables the OCaml code generation.") ;
      ("--old-pretty",
       Arg.String Configuration.set_old_pretty_print,
       " pretty-prints the parse tree of the focalize file as \
         an old focal source into the argument file") ;
      ("--pretty",
       Arg.String Configuration.set_pretty_print,
       " pretty-prints the parse tree of the focal file as a focal source \
	 into the argument file.") ;
      ("--raw-ast-dump",
       Arg.Unit Configuration.set_raw_ast_dump,
       " prints on stderr the raw AST structure after parsing stage.") ;
      ("--scoped_pretty",
       Arg.String Configuration.set_pretty_scoped,
       " pretty-prints the parse tree of the focal file once scoped as a \
	 focal source into the argument file.") ;
      ("--verbose",
       Arg.Unit Configuration.set_verbose,
       " be verbose.") ;
      ("-v", Arg.Unit Configuration.print_focal_short_version,
       " print the focalize version.") ;
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " print the full focalize version, sub-version and release date.") ]
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
  let ast =
    Parse_file.parse_file Format.err_formatter input_file_name in
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
  (* Pretty the AST as an old-focal-syntax source if requested. *)
  (match Configuration.get_old_pretty_print () with
   | None -> ()
   | Some fname ->
     let out_hd = open_out_bin fname in
     let out_fmt = Format.formatter_of_out_channel out_hd in
     Oldsourcify.pp_file out_fmt ast ;
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
  (* Now, generate the persistent interface file. *)
  Env.make_fo_file
    ~source_filename: input_file_name scoping_toplevel_env typing_toplevel_env ;
  (* Now go to the OCaml code generation if requested. *)
  if Configuration.get_generate_ocaml () then
    (begin
    let out_file_name = (Filename.chop_extension input_file_name) ^ ".ml" in
    Core_ml_generation.root_compile
      ~current_unit ~out_file_name typing_toplevel_env stuff_to_compile
    end) ;
  exit 0
;;
