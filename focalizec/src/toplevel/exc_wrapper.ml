(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: exc_wrapper.ml,v 1.26 2007-10-29 15:48:12 pessaux Exp $ *)

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
	   "@[%tUnable@ to@ find@ file%t@ '%t%s%t'@ %tin@ the@ search@ path%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined fname Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Files.Corrupted_fo fname ->
	 Format.fprintf Format.err_formatter
	   "@[%tInvalid@ or@ corrupted@ compiled@ interface%t@ '%t%s%t'.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined fname Handy.pp_reset_effects
     | Check_file.Bad_file_suffix fname ->
	 Format.fprintf Format.err_formatter
	   "@[%tInvalid@ file@ extention@ for%t@ '%t%s%t'.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined fname Handy.pp_reset_effects
     | Sys_error m ->
	 Format.fprintf Format.err_formatter "@[%tSystem@ error%t -@ %s.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects m
     | Configuration.Input_file_already_set ->
	 Format.fprintf Format.err_formatter
	   "@[%tInput@ file@ name@ is@ already@ set%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Configuration.No_input_file ->
	 Format.fprintf Format.err_formatter
	   "@[%tNo@ input@ file.@ FoCaL@ is@ cowardly@ and@ gives@ out...%t@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
(* *********************** *)
(* Lexing / parsing stage. *)
     | Parse_file.Lex_error (pos_s, pos_e, reason) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tLexical@ error%t@ %s@].@."
	   Parse_file.pp_err_loc (pos_s, pos_e)
	   Handy.pp_set_bold Handy.pp_reset_effects reason
     | Parse_file.Syntax_error position ->
	 Format.fprintf Format.err_formatter "%a:@\n@[%tSyntax@ error%t.@]@."
	   Parse_file.pp_err_loc position
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Parse_file.Unclear_error (exc_string, pos_s, pos_e) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnclear@ syntax@ error%t:@ %s.@]@."
	   Parse_file.pp_err_loc (pos_s, pos_e)
	   Handy.pp_set_bold Handy.pp_reset_effects exc_string
(* ************** *)
(* Scoping stage. *)
     | Scoping.Multiply_used_module modname ->
	 Format.fprintf Format.err_formatter
	   "@[%tModule%t@ '%s'@ %twas@ \"use\"@ several@ times%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   modname
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Scoping.Module_not_specified_as_used modname ->
	 Format.fprintf Format.err_formatter
	   "@[%tModule%t@ '%s'@ %twas@ not@ declared@ as@ \"use\"%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   modname
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Scoping.Parametrized_species_wrong_arity (at, expected, used_with) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tSpecies@ application@ expression@ expected%t@ %d@ %t@ arguments@ but@ was@ provided%t@ %d.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   expected
	   Handy.pp_set_bold Handy.pp_reset_effects
	   used_with
(* *************************** *)
(* Generic environments stuff. *)
     | Env.Unbound_constructor (vname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnbound@ sum@ type@ constructor%t@ '%t%a%t'.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname vname
	   Handy.pp_reset_effects
     | Env.Unbound_label (lvname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnbound@ record@ field@ label%t@ '%t%a%t'.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname lvname
	   Handy.pp_reset_effects
     | Env.Unbound_identifier (vname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnbound@ identifier%t@ '%t%a%t'.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname vname
	   Handy.pp_reset_effects
     | Env.Unbound_type (tname, at) ->
	 Format.fprintf Format.err_formatter
	   "@ %a :@\n@[%tUnbound@ type%t@ '%t%a%t'.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname tname
	   Handy.pp_reset_effects
     | Env.Unbound_module (fname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnbound@ module%t@ '%t%s%t'.@]@." Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined fname
	   Handy.pp_reset_effects
     | Env.Unbound_species (sname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tUnbound@ species%t@ '%t%a%t'.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname sname
	   Handy.pp_reset_effects
     | Env.Rebound_type (vname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tType@ name%t@ '%t%a%t' %talready@ bound@ in@ the@ current@ scope%t.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname vname
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Env.Rebound_species (vname, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tSpecies@ name%t@ '%t%a%t' %talready@ bound@ in@ the@ current@ scope%t.@]@."
	   Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname vname
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
(* ******************** *)
(* Core types problems. *)
     | Types.Conflict (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tTypes%t@ @[%t%a%t@]@ %tand%t@ @[%t%a%t@]@ %tare@ not@ compatible%t.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Types.pp_type_simple ty1
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined  Types.pp_type_simple ty2
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Types.Circularity (ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[Type@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ to@ a@ cycle.@]@."
	   Location.pp_location at
	   Types.pp_type_simple ty1 Types.pp_type_simple ty2
     | Types.Arity_mismatch (ty_cstr_name, arity1, arity2, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[Type@ constructor@ '%a'@ used@ with@ the@ different@ arities@ %d@ and@ %d.@]@."
	   Location.pp_location at Types.pp_type_name ty_cstr_name arity1 arity2
     | Infer.Scheme_contains_type_vars (field_name, sch, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tIn@ field%t@ '%t%a%t'%t,@ type scheme%t@ @[%a@]@ %tcontains@ variables@ than@ cannot@ be@ generalized or is polymorphic%t.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname field_name
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Types.pp_type_scheme sch
	   Handy.pp_set_bold Handy.pp_reset_effects
(* ************************** *)
(* Core type inference stuff. *)
     | Infer.Bad_sum_type_constructor_arity (ident, defined_arity) ->
	 let (expected, used) =
	   (match defined_arity with
	    | Env.TypeInformation.CA_zero -> ("no", "1")
	    | Env.TypeInformation.CA_one -> ("1", "no")) in
	 Format.fprintf Format.err_formatter
	   "@[%tSum@ type@ constructor%t@ '%t%a%t'@ %texpected@ %s@ argument@ but@ was@ used@ with@ %s@ argument%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_constructor_ident ident
	   Handy.pp_reset_effects
	   Handy.pp_set_bold expected used Handy.pp_reset_effects
     | Infer.Unbound_type_variable var_name ->
	 Format.fprintf Format.err_formatter
	   "@[%tUnbound@ type@ variable%t@ %t%s%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined (Parsetree_utils.name_of_vname var_name)
	   Handy.pp_reset_effects
     | Infer.Method_multiply_defined (m_vname, s_name) ->
	 Format.fprintf Format.err_formatter
	   "@[%tMethod%t@ '%t%a%t'@ %tmultiply@ defined@ in@ species%t@ '%t%a%t'.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_vname m_vname
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_qualified_vname s_name
	   Handy.pp_reset_effects
     | Infer.Bad_type_arity (ident, expected, used) ->
	 Format.fprintf Format.err_formatter
	   "@[Type@ constructor@ '%a'@ expected@ %d@ arguments@ but@ was@ used@ with@ %d@ arguments.@]@."
	   Sourcify.pp_ident ident expected used
(* *********************** *)
(* Species type inference. *)
     | Infer.Rep_multiply_defined at ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tCarrier@ 'rep'@ is@ multiply@ defined%t.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Scoping.Self_cant_parameterize_itself at ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%t'Self'@ can't@ be@ parameterized@ by@ itself%t.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Scoping.Is_parameter_only_coll_ident at ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tA@ \"is\"@ parameter@ can@ only@ be@ a@ collection@ identifier%t.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
     | Infer.Not_subspecies_conflicting_field (c1, c2, field, ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[%tSpecies%t@ '%t%a%t'@ %tis@ not@ a@ subspecies@ of%t@ '%t%a%t'.@ In@ field@ '%a',@ types@ @[%a@]@ and@ @[%a@]@ are@ not@ compatible.@]@."
	   Location.pp_location at
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Types.pp_type_collection c1
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Types.pp_type_collection c2
	   Handy.pp_reset_effects
	   Sourcify.pp_vname field Types.pp_type_simple ty1
	   Types.pp_type_simple ty2
     | Infer.Not_subspecies_circular_field (c1, c2, field, ty1, ty2, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ In@ field@ '%a',@ type@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ to@ a@ cycle.@]@."
	   Location.pp_location at
	   Types.pp_type_collection c1 Types.pp_type_collection c2
	   Sourcify.pp_vname field Types.pp_type_simple ty1
	   Types.pp_type_simple ty2
     | Infer.Not_subspecies_arity_mismatch
	 (c1, c2, field, ty_name, ar1, ar2, at) ->
	   Format.fprintf Format.err_formatter
	     "%a:@\n@[Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ In@ field@ '%a',@ the@ type@ constructor@ '%a'@ is@ used@ with@ the@ different@ arities@ %d@ and@ %d.@]@."
	     Location.pp_location at
	     Types.pp_type_collection c1 Types.pp_type_collection c2
	     Sourcify.pp_vname field Types.pp_type_name ty_name ar1 ar2
     | Infer.Not_subspecies_missing_field (c1, c2, field, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[Species@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ Field@ '%a'@ is@ not@ present@ in@ '%a'.@]@."
	   Location.pp_location at
	   Types.pp_type_collection c1 Types.pp_type_collection c2
	   Sourcify.pp_vname field Types.pp_type_collection c1
     | Infer.Parameterized_species_arity_mismatch msg ->
	 Format.fprintf Format.err_formatter
	   "@[Parameterized@ specie@ is@ applied@ to@ %s@ arguments.@]@."  msg
     | Infer.Collection_not_fully_defined (coll_name, field_name) ->
	 Format.fprintf Format.err_formatter
	   "@[Species@ '%a'@ cannot@ be@ turned@ into@ a@ collection.@ Field@ '%a'@ is@ not@ defined.@]@."
	   Sourcify.pp_qualified_vname coll_name
	   Sourcify.pp_vname field_name
(* ********************** *)
(* Dependencies analysis. *)
     | Dep_analysis.Ill_formed_species species_name ->
	 Format.fprintf Format.err_formatter
	   "@[%tSpecies%t@ '%t%a%t'@ %tis@ not@ well-formed%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   Handy.pp_set_underlined Sourcify.pp_qualified_vname species_name
	   Handy.pp_reset_effects
	   Handy.pp_set_bold Handy.pp_reset_effects
(* ********************** *)
(* OCaml code generation. *)
     | Externals_ml_generation.No_external_value_caml_def (def_name, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[No OCaml mapping given for the external value definition '%a'.@]@."
	   Location.pp_location at Sourcify.pp_vname def_name
     | Externals_ml_generation.No_external_type_caml_def (def_name, at) ->
	 Format.fprintf Format.err_formatter
	   "%a:@\n@[No OCaml mapping given for the external type definition '%a'.@]@."
	   Location.pp_location at Sourcify.pp_vname def_name
(* ********************** *)
(* The ultimate firewall. *)
     | x ->
	 Format.fprintf Format.err_formatter
	   "@[%tUnexpected@ error%t:@ \"%s\".\n%tPlease@ report%t.@]@."
	   Handy.pp_set_bold Handy.pp_reset_effects
	   (Printexc.to_string x)
	   Handy.pp_set_videoinv Handy.pp_reset_effects
    end) ;
    (* Anf anyway, if an exception occured, exit with -1 error code. *)
    exit (-1)
;;

