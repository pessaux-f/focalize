(* $Id: focalizec.ml,v 1.1 2007-07-19 12:01:51 pessaux Exp $ *)

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
       Arg.String Configuration.set_pretty_print,
       " pretty-prints the parse tree of the focal file as a focal source.") ;
      ("--old-pretty",
       Arg.String Configuration.set_old_pretty_print,
       " pretty-prints the parse tree of the focalize file as \
         an old focal source.") ;
      ("--typecheck",
       Arg.Unit (fun () -> Configuration.set_do_typechecking true),
       " performs type inference.") ;
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
  (* Typechecks the AST if requested. *)
  if Configuration.get_do_typechecking () then
    Infer.typecheck_file ast ;
  exit 0
;;



(** If something unexpected arrises when proceeding, we exit with the proper
    error code. *)
try main () with
| Types.Conflict (ty1, ty2) ->
    Format.fprintf Format.err_formatter
      "Type incompatibility between %a@ and@ %a.@\n@?"
      Types.pp_simple_type ty1 Types.pp_simple_type ty2 ;
| Types.Circularity (ty1, ty2) ->
    Format.fprintf Format.err_formatter "Circulary between types %a@ and@ %a@\n@?"
    Types.pp_simple_type ty1 Types.pp_simple_type ty2
| Types.Arity_mismatch (cstr_name, arity1, arity2) ->
    Format.fprintf Format.err_formatter
      "Type constructor %s used with arity %d and %d@\n@?"
      cstr_name arity1 arity2
| Env.Unbound_identifier vname ->
    Format.fprintf Format.err_formatter "Unbound identifier \"%s\".@\n@?"
      (Parsetree_utils.string_of_vname vname)
| Parse_file.Lex_error (pos_s, pos_e, reason) ->
    Format.fprintf Format.err_formatter "Lexical error, %a. %s@\n@?"
      Parse_file.pp_err_loc (pos_s, pos_e) reason
| Parse_file.Syntax_error position ->
    Format.fprintf Format.err_formatter "Syntax error, %a.@\n@?"
      Parse_file.pp_err_loc position
| Parse_file.Unclear_error position ->
    Format.fprintf Format.err_formatter "Unclear syntax error, %a.@\n@?"
      Parse_file.pp_err_loc position
| x ->
    Format.fprintf Format.err_formatter
      "Unexpected error: \"%s\".\nPlease report.@\n@?"
      (Printexc.to_string x) ;
    flush stderr ;
    exit 2
;;
