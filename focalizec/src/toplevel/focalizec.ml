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


(* $Id: focalizec.ml,v 1.8 2007-08-14 11:04:19 pessaux Exp $ *)

(** The focalize concrete syntax file checker. *)



(* The main procedure *)
let main () =
  Arg.parse
    [ ("-v", Arg.Unit Configuration.print_focal_short_version,
       " print the focalize version.") ;
      ("--version",
       Arg.Unit Configuration.print_focal_full_version,
       " print the full focalize version, sub-version and release date.") ;
      ("-c",
       Arg.String Configuration.set_input_file_name,
       " check input file argument.") ;
      ("-i",
       Arg.Unit (fun () -> Configuration.set_do_interface_output true),
       " prints the source file interface.") ;
      ("-I",
       Arg.String (fun path -> Files.add_lib_path path),
       " adds the specified path to the path list where to search for compiled interfaces.") ;
      ("--pretty",
       Arg.String Configuration.set_pretty_print,
       " pretty-prints the parse tree of the focal file as a focal source.") ;
      ("--old-pretty",
       Arg.String Configuration.set_old_pretty_print,
       " pretty-prints the parse tree of the focalize file as \
         an old focal source.") ;
      ("--scoped_pretty",
       Arg.String Configuration.set_pretty_scoped,
       " pretty-prints the parse tree of the focal file once scoped as a focal source.") ;
      ("--verbose",
       Arg.Unit (fun () -> Configuration.set_verbose true),
       " be verbose.") ]
    Configuration.set_input_file_name
    "Usage: focal_check <options> <.foc file>" ;
  (* First, let's lex and parse the input source file. *)
  let input_file_name = Configuration.get_input_file_name () in
  (* Create the current compilation unit "fname". In fact, this *)
  (* is the current filename without dirname and extention.     *)
  let current_unit =
    Filename.chop_extension (Filename.basename input_file_name) in
  let ast =
    Parse_file.parse_file Format.err_formatter input_file_name in
  (* Hard-dump the AST if requested. *)
  if Configuration.get_verbose () then
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
  let typing_toplevel_env =
    Infer.typecheck_file current_unit scoped_ast in
  (* Now, generate the persistent interface file. *)
  Env.make_fo_file
    ~source_filename: input_file_name scoping_toplevel_env typing_toplevel_env ;
  exit 0
;;



(** If something unexpected arises when proceeding, we exit with the proper
    error code. *)
try main () with
| Types.Conflict (ty1, ty2) ->
    Format.fprintf Format.err_formatter
      "Type incompatibility between %a@ and@ %a.@."
      Types.pp_type_simple ty1 Types.pp_type_simple ty2 ;
| Types.Circularity (ty1, ty2) ->
    Format.fprintf Format.err_formatter
      "Circulary between types %a@ and@ %a@."
      Types.pp_type_simple ty1 Types.pp_type_simple ty2
| Types.Arity_mismatch (cstr_name, arity1, arity2) ->
    Format.fprintf Format.err_formatter
      "Type constructor %s used with arity %d and %d@."
      cstr_name arity1 arity2
| Env.Unbound_identifier vname ->
    Format.fprintf Format.err_formatter "Unbound identifier \"%s\".@."
      (Parsetree_utils.name_of_vname vname)
| Parse_file.Lex_error (pos_s, pos_e, reason) ->
    Format.fprintf Format.err_formatter "Lexical error, %a. %s@."
      Parse_file.pp_err_loc (pos_s, pos_e) reason
| Parse_file.Syntax_error position ->
    Format.fprintf Format.err_formatter "Syntax error, %a.@."
      Parse_file.pp_err_loc position
| Parse_file.Unclear_error position ->
    Format.fprintf Format.err_formatter "Unclear syntax error, %a.@."
      Parse_file.pp_err_loc position
| x ->
    Format.fprintf Format.err_formatter
      "Unexpected error: \"%s\".\nPlease report.@."
      (Printexc.to_string x) ;
    exit 2
;;
