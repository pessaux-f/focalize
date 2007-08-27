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

(* $Id: exc_wrapper.ml,v 1.11 2007-08-27 12:33:26 pessaux Exp $ *)

(* ************************************************************************** *)
(** {b Descr} : Wrapper used to protect the call to the "main". If something
              unexpected arises when proceeding, we exit with the proper
              error code.                                                     *)
(* ************************************************************************** *)
try Check_file.main () with
| anything ->
    (begin
    match anything with
(* ********************* *)
(* General files access. *)
     | Files.Cant_access_file_in_search_path fname ->
	 (* In fact, should always be caught by env.ml. *)
	 Format.fprintf Format.err_formatter
	   "Unable@ to@ find@ file@ '%s'@ in@ the@ search@ path.@." fname
     | Files.Corrupted_fo fname ->
	 Format.fprintf Format.err_formatter
	   "Invalid@ or@ corrupted@ compiled@ interface@ '%s'.@." fname
     | Check_file.Bad_file_suffix fname ->
	 Format.fprintf Format.err_formatter
	   "Invalid@ file@ extention@ for@ '%s'.@." fname
     | Sys_error m ->
	 Format.fprintf Format.err_formatter "System@ error -@ %s.@." m
     | Configuration.Input_file_already_set ->
	 Format.fprintf Format.err_formatter
	   "Input@ file@ name@ is@ already@ set.@."
(* *********************** *)
(* Lexing / parsing stage. *)
     | Parse_file.Lex_error (pos_s, pos_e, reason) ->
	 Format.fprintf Format.err_formatter "Lexical@ error,@ %a.@ %s@."
	   Parse_file.pp_err_loc (pos_s, pos_e) reason
     | Parse_file.Syntax_error position ->
	 Format.fprintf Format.err_formatter "Syntax@ error,@ %a.@."
	   Parse_file.pp_err_loc position
     | Parse_file.Unclear_error position ->
	 Format.fprintf Format.err_formatter "Unclear@ syntax@ error,@ %a.@."
	   Parse_file.pp_err_loc position
(* ************** *)
(* Scoping stage. *)
     | Scoping.Multiply_used_module modname ->
	 Format.fprintf Format.err_formatter
	   "Module@ '%s'@ was@ \"use\"@ several@ times.@." modname
     | Scoping.Module_not_specified_as_used modname ->
	 Format.fprintf Format.err_formatter
	   "Module@ '%s'@ was@ not@ declared@ as@ \"use\".@." modname
(* *************************** *)
(* Generic environments stuff. *)
     | Env.Unbound_constructor (vname, at) ->
	 Format.fprintf Format.err_formatter "Unbound@ constructor@ \'%s\' :@ %a@."
	   (Parsetree_utils.name_of_vname vname) Location.pp_location at
     | Env.Unbound_label (lname, at) ->
	 Format.fprintf Format.err_formatter
	   "Unbound@ record@ field@ label@ \'%s\' : %a.@."
	   lname Location.pp_location at
     | Env.Unbound_identifier (vname, at) ->
	 Format.fprintf Format.err_formatter "Unbound identifier \'%s\' :@ %a.@."
	   (Parsetree_utils.name_of_vname vname) Location.pp_location at
     | Env.Unbound_type (tname, at) ->
	 Format.fprintf Format.err_formatter "Unbound@ type@ \'%s\' :@ %a.@."
	   tname Location.pp_location at
     | Env.Unbound_module (fname, at) ->
	 Format.fprintf Format.err_formatter
	   "Unbound@ module@ \'%s\':@ %a.@." fname Location.pp_location at
     | Env.Unbound_species (sname, at) ->
	 Format.fprintf Format.err_formatter
	   "Unbound@ species@ \'%s\' :@ %a.@." sname Location.pp_location at
(* ******************** *)
(* Core types problems. *)
     | Types.Conflict (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "Types@ @[%a@]@ and@ @[%a@]@ are@ not@ compatible :@ %a@."
	   Types.pp_type_simple ty1 Types.pp_type_simple ty2
	   Location.pp_location at
     | Types.Circularity (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "Type@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ to@ a@ cycle :@ %a@."
	   Types.pp_type_simple ty1 Types.pp_type_simple ty2
	   Location.pp_location at
     | Types.Arity_mismatch (cstr_name, arity1, arity2, at) ->
	 Format.fprintf Format.err_formatter
	   "Type@ constructor@ '%s'@ used@ with@ the@ different@ arities@ %d@ and@ %d :@ %a@."
	   cstr_name arity1 arity2 Location.pp_location at
(* ************************** *)
(* Core type inference stuff. *)
     | Infer.Bad_sum_type_constructor_arity (ident, defined_arity) ->
	 let (expected, used) =
	   (match defined_arity with
	    | Env.TypeInformation.CA_zero -> ("no", "1")
	    | Env.TypeInformation.CA_one -> ("1", "no")) in
	 Format.fprintf Format.err_formatter
	   "Sum@ type@ constructor@ '%a'@ expected@ %s@ argument@ but@ was@ used@ with@ %s@ argument.@."
	   Sourcify.pp_ident ident expected used
     | Infer.Unbound_type_variable var_name ->
	 Format.fprintf Format.err_formatter "Unbound@ type@ variable@ %s.@." var_name
     | Infer.Method_multiply_defined (m_vname, s_name) ->
	 Format.fprintf Format.err_formatter
	   "Method@ '%s'@ multiply@ defined@ in@ species@ '%s'.@."
	   (Parsetree_utils.name_of_vname m_vname) s_name
     | Infer.Bad_type_arity (ident, expected, used) ->
	 Format.fprintf Format.err_formatter
	   "Type@ constructor@ '%a'@ expected@ %d@ arguments@ but@ was@ used@ with@ %d@ arguments.@."
	   Sourcify.pp_ident ident expected used
(* *********************** *)
(* Species type inference. *)
     | Infer.Rep_multiply_defined at ->
	 Format.fprintf Format.err_formatter "Carrier@ 'rep'@ is@ multiply@ defined :@ %a@."
	   Location.pp_location at
     | Scoping.Self_cant_parameterize_itself at ->
	 Format.fprintf Format.err_formatter
	   "'Self'@ can't@ be@ parameterized@ by@ itself :@ %a@." Location.pp_location at
     | Scoping.Is_parameter_only_coll_ident at ->
	 Format.fprintf Format.err_formatter
	   "A@ 'is'@ parameter@ can@ only@ be@ a@ collection@ identifier :@ %a@."
	   Location.pp_location at
     | Infer.Not_subspecies_conflicting_field (c1, c2, field, ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ In@ field@ '%a',@ types@ @[%a@]@ and@ @[%a@] are@ not@ compatible :@ %a@."
	   Types.pp_type_collection c1 Types.pp_type_collection c2
	     Sourcify.pp_vname field Types.pp_type_simple ty1
	     Types.pp_type_simple ty2 Location.pp_location at
     | Infer.Not_subspecies_circular_field (c1, c2, field, ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	     "Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ In@ field@ '%a',@ type@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ to@ a@ cycle :@ %a@."
	     Types.pp_type_collection c1 Types.pp_type_collection c2
	     Sourcify.pp_vname field Types.pp_type_simple ty1
	     Types.pp_type_simple ty2 Location.pp_location at
     | Infer.Not_subspecies_arity_mismatch
	 (c1, c2, field, ty_name, ar1, ar2, at) ->
	   Format.fprintf Format.err_formatter
	     "Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ In@ field@ '%a',@ the@ type@ constructor@ '%s'@ is@ used@ with@ the@ different@ arities@ %d@ and@ %d :@ %a@."
	     Types.pp_type_collection c1 Types.pp_type_collection c2
	     Sourcify.pp_vname field ty_name ar1 ar2 Location.pp_location at
     | Infer.Not_subspecies_missing_field (c1, c2, field, at) ->
	 Format.fprintf Format.err_formatter
	   "Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ Field@ '%a'@ is@ not@ present@ in@ '%a' :@ %a@."
	   Types.pp_type_collection c1 Types.pp_type_collection c2
	   Sourcify.pp_vname field Types.pp_type_collection c1
	   Location.pp_location at
     | Infer.Parameterized_species_arity_mismatch msg ->
	 Format.fprintf Format.err_formatter
	   "Parameterized@ specie@ is@ applied@ to@ %s@ arguments@."  msg
     | Infer.Collection_not_fully_defined (coll_name, field_name) ->
	 Format.fprintf Format.err_formatter
	   "Species@ '%s'@ cannot@ be@ turned@ into@ a@ collection.@ Field@ '%a'@ is@ not@ defined@."
	   coll_name Sourcify.pp_vname field_name
(* ********************** *)
(* The ultimate firewall. *)
     | x ->
	 Format.fprintf Format.err_formatter
	   "Unexpected@ error :@ \"%s\".\nPlease@ report.@."
	   (Printexc.to_string x)
    end) ;
    (* Anf anyway, if an exception occured, exit with -1 error code. *)
    exit (-1)
;;
