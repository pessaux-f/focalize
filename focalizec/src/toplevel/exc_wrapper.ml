(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: exc_wrapper.ml,v 1.4 2007-08-15 17:00:01 pessaux Exp $ *)

(* ************************************************************************** *)
(** {b Descr} : Wrapper used to protect the call to the "main". If something
              unexpected arises when proceeding, we exit with the proper
              error code.                                                     *)
(* ************************************************************************** *)
try Check_file.main () with
| anything ->
    (begin
    match anything with
 (* General files access. *)
     | Files.Cant_access_file_in_search_path fname ->
	 (* In fact, should always be caught by env.ml. *)
	 Format.fprintf Format.err_formatter
	   "Unable to find file '%s' in the search path.@." fname
     | Files.Corrupted_fo fname ->
	 Format.fprintf Format.err_formatter
	   "Invalid or corrupted compiled interface '%s'.@." fname
     | Check_file.Bad_file_suffix fname ->
	 Format.fprintf Format.err_formatter
	   "Invalid file extention for '%s'.@." fname
     | Sys_error m ->
	 Format.fprintf Format.err_formatter "System error - %s.@." m
 (* Lexing / parsing stage. *)
     | Parse_file.Lex_error (pos_s, pos_e, reason) ->
	 Format.fprintf Format.err_formatter "Lexical error, %a. %s@."
	   Parse_file.pp_err_loc (pos_s, pos_e) reason
     | Parse_file.Syntax_error position ->
	 Format.fprintf Format.err_formatter "Syntax error, %a.@."
	   Parse_file.pp_err_loc position
     | Parse_file.Unclear_error position ->
	 Format.fprintf Format.err_formatter "Unclear syntax error, %a.@."
	   Parse_file.pp_err_loc position
(* Scoping stage. *)
     | Scoping.Multiply_used_module modname ->
	 Format.fprintf Format.err_formatter
	   "Module '%s' was \"use\" several times.@." modname
     | Scoping.Module_not_specified_as_used modname ->
	 Format.fprintf Format.err_formatter
	   "Module '%s' was not declared as \"use\".@." modname
(* Generic environments stuff. *)
     | Env.Unbound_constructor vname ->
	 Format.fprintf Format.err_formatter "Unbound constructor \'%s\'.@."
	   (Parsetree_utils.name_of_vname vname)
     | Env.Unbound_label lname ->
	 Format.fprintf Format.err_formatter
	   "Unbound record field label \'%s\'.@." lname
     | Env.Unbound_identifier vname ->
	 Format.fprintf Format.err_formatter "Unbound identifier \'%s\'.@."
	   (Parsetree_utils.name_of_vname vname)
     | Env.Unbound_type tname ->
	 Format.fprintf Format.err_formatter "Unbound type \'%s\'.@." tname
     | Env.Unbound_module fname ->
	 Format.fprintf Format.err_formatter "Unbound module \'%s\'.@." fname
     | Env.Unbound_species sname ->
	 Format.fprintf Format.err_formatter "Unbound species \'%s\'.@." sname
(* Core types problems. *)
     | Types.Conflict (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "Type incompatibility between %a@ and@ %a : %a@."
	   Types.pp_type_simple ty1 Types.pp_type_simple ty2
	   Location.pp_location at
     | Types.Circularity (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "Circulary between types %a@ and@ %a : %a@."
	   Types.pp_type_simple ty1 Types.pp_type_simple ty2
	   Location.pp_location at
     | Types.Arity_mismatch (cstr_name, arity1, arity2, at) ->
	 Format.fprintf Format.err_formatter
	   "Type constructor %s used with arity %d and %d : %a@."
	   cstr_name arity1 arity2 Location.pp_location at
(* Type inference stuff. *)
     | Infer.Bad_sum_type_constructor_arity (ident, defined_arity) ->
	 let (expected, used) =
	   (match defined_arity with
	    | Env.TypeInformation.CA_zero -> ("no", "1")
	    | Env.TypeInformation.CA_one -> ("1", "no")) in
	 Format.fprintf Format.err_formatter
	   "Sum type constructor '%a' expected %s argument but was used with %s argument.@."
	   Sourcify.pp_ident ident expected used
     | Infer.Unbound_type_variable var_name ->
	 Format.fprintf Format.err_formatter "Unbound type variable %s.@." var_name
     | Infer.Method_multiply_defined (m_vname, s_name) ->
	 Format.fprintf Format.err_formatter
	   "Method '%s' multiply defined in species '%s'.@."
	   (Parsetree_utils.name_of_vname m_vname) s_name
     | Infer.Bad_type_arity (ident, expected, used) ->
	 Format.fprintf Format.err_formatter
	   "Type constructor '%a' expected %d arguments but was used with %d arguments.@."
	   Sourcify.pp_ident ident expected used
(* The ultimate firewall. *)
     | x ->
	 Format.fprintf Format.err_formatter
	   "Unexpected error: \"%s\".\nPlease report.@."
	   (Printexc.to_string x)
    end) ;
    (* Anf anyway, if an exception occured, exit with -1 error code. *)
    exit (-1)
;;
