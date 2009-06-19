(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: exc_wrapper.ml,v 1.87 2009-06-19 10:42:52 pessaux Exp $ *)

let header ppf =
  Format.fprintf ppf "%tError:%t@ " Handy.pp_set_bold Handy.pp_reset_effects
;;

let print_focalize_exception ppf = function
  (* ************** *)
  (* Miscellaneous. *)
  | Configuration.Invalid_OCaml_compiler str ->
      Format.fprintf ppf
        "@[%tInvalid@ OCaml@ compiler@ kind \"%t%s%t\" for option \
        -ocaml-comp-mode. Must be \"byt\", \"bin\" or \"both\".@]@."
        header
        Handy.pp_set_underlined str Handy.pp_reset_effects
  (* ********************* *)
  (* General files access. *)
  | Files.Cant_access_file_in_search_path fname ->
      (* In fact, should always be caught by env.ml. *)
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ file@ '%t%s%t'@ in@ the@ search@ path.@]@."
        header
        Handy.pp_set_underlined fname Handy.pp_reset_effects
  | Files.Corrupted_fo fname ->
      Format.fprintf ppf
        "@[%tInvalid@ or@ corrupted@ compiled@ interface@ '%t%s%t'. Maybe \
        it@ was@ compiled@ with@ another@ version@ of@ the@ compiler.@]@."
        header
        Handy.pp_set_underlined fname Handy.pp_reset_effects
  | Focalizec.Bad_file_suffix fname ->
      Format.fprintf ppf
        "@[%tInvalid@ file@ extension@ for@ '%t%s%t'.@]@."
        header
        Handy.pp_set_underlined fname Handy.pp_reset_effects
  | Sys_error m ->
      Format.fprintf ppf "@[%tSystem@ error -@ %s.@]@." header m
  | Configuration.No_input_file ->
      Format.fprintf ppf
        "@[%tNo@ input@ file.@ FoCaLize@ is@ cowardly@ giving@ up...@]@."
        header
  (* *********************** *)
  (* Lexing / parsing stage. *)
  | Parse_file.Lex_error (pos_s, pos_e, reason) ->
      Format.fprintf ppf
        "%a:@\n@[%tLexical@ error@ %s@].@."
        Parse_file.pp_err_loc (pos_s, pos_e) header reason
  | Parse_file.Syntax_error (pos_s, pos_e) ->
      Format.fprintf ppf "%a:@\n@[%tSyntax@ error.@]@."
        Parse_file.pp_err_loc (pos_s, pos_e) header
  | Parse_file.Unclear_error (exc_string, pos_s, pos_e) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnclear@ syntax@ error:@ %s.@]@."
        Parse_file.pp_err_loc (pos_s, pos_e) header exc_string
  (* ************** *)
  (* Scoping stage. *)
  | Scoping.Module_not_specified_as_used (at, modname) ->
      Format.fprintf ppf
        "%a:@\n@[%tCompilation@ unit@ '%s'@ was@ not@ declared@ as@ \
         \"use\".@]@."
        Location.pp_location at header modname
  | Scoping.Parametrized_species_wrong_arity (at, expected, used_with) ->
      Format.fprintf ppf
        "%a:@\n@[%tSpecies@ application@ expression@ expected@ %d@ \
         @ arguments@ but@ was@ provided@ %d.@]@."
        Location.pp_location at header expected used_with
  | Scoping.Non_logical_let_cant_define_logical_expr (name, at) ->
       Format.fprintf ppf
        "%a:@\n@[%tNon-logical@ let@ must@ not@ bind@ '%t%a%t'@ \
         to@ a@ property.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name Handy.pp_reset_effects
  | Scoping.Termination_proof_delayed_only_on_self_meth (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tDelayed@ termination@ proof@ refers@ to@ an@ unknown@ \
         method '%t%a%t' of the species.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name Handy.pp_reset_effects
  | Scoping.Ambiguous_logical_expression_or (pos, at) ->
      let side =
        (match pos with 0 -> "left" | 1 -> "right" | _ -> assert false) in
      Format.fprintf ppf
        "%a:@\n@[%tAmbiguous logical expression. Add explicit parentheses \
        to associate the %s argument of the \\/ properly.@]@."
        Location.pp_location at header side
  | Scoping.Ambiguous_logical_expression_and (pos, at) ->
      let side =
        (match pos with 0 -> "left" | 1 -> "right" | _ -> assert false) in
      Format.fprintf ppf
        "%a:@\n@[%tAmbiguous logical expression. Add explicit parentheses \
        to associate the %s argument of the /\\ properly.@]@."
        Location.pp_location at header side
  (* *************************** *)
  (* Generic environments stuff. *)
  | Env.Unbound_constructor (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ sum@ type@ constructor@ '%t%a%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Unbound_label (lvname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ record@ type@ label@ '%t%a%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname lvname
        Handy.pp_reset_effects
  | Env.Unbound_identifier (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ identifier@ '%t%a%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Unbound_type (tname, at) ->
      Format.fprintf ppf
        "@ %a :@\n@[%tUnbound@ type@ '%t%a%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname tname
        Handy.pp_reset_effects
  | Env.Unbound_module (fname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ compilation@ unit@ '%t%s%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined fname
        Handy.pp_reset_effects
  | Env.Unbound_species (sname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ species@ '%t%a%t'.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname sname
        Handy.pp_reset_effects
  | Env.Unbound_collection (sname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tSpecies@ '%t%a%t'@ is@ not@ a@ collection.@ Its@ carrier@ \
        can't@ be@ used@ in@ type@ expressions.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname sname
        Handy.pp_reset_effects
  | Env.Rebound_type (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ name@ '%t%a%t'@ already@ bound@ in@ the@ \
         current@ scope.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Rebound_toplevel_let (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tToplevel value@ name@ '%t%a%t'@ already@ bound@ in@ the@ \
         current@ scope.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Rebound_species (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tSpecies@ name@ '%t%a%t'@ already@ bound@ in@ the@ \
         current@ scope.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  (* ******************** *)
  (* Core types problems. *)
  | Types.Conflict (ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tTypes@ @[%t%a%t@]@ and@ @[%t%a%t@]@ are@ not@ \
         compatible.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Types.pp_type_simple ty1
        Handy.pp_reset_effects
        Handy.pp_set_underlined  Types.pp_type_simple ty2
        Handy.pp_reset_effects
  | Types.Circularity (ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ \
         to@ a@ cycle.@]@."
        Location.pp_location at header
        Types.pp_type_simple ty1 Types.pp_type_simple ty2
  | Types.Arity_mismatch (ty_cstr_name, arity1, arity2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ constructor@ '%a'@ used@ with@ conflicting@ \
         arities:@ %d@ and@ %d.@]@."
        Location.pp_location at header
        Types.pp_type_name ty_cstr_name arity1 arity2
  | Types.Arity_mismatch_unexpected_args (at) ->
      (* To handle error message of bug report #180 (sum type constructor
         used with no argument although it needs some). *)
      Format.fprintf ppf
        "%a:@\n@[%tNo expected argument(s).@]@." Location.pp_location at header
  | Infer.Scheme_contains_type_vars (field_name, sch, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ method@ '%t%a%t',@ type scheme@ @[%a@]@ \
         contains@ free@ variables.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname field_name
        Handy.pp_reset_effects
        Types.pp_type_scheme sch
  (* ************************** *)
  (* Core type inference stuff. *)
  | Infer.Bad_sum_type_constructor_arity (ident, defined_arity) ->
      let (expected, used) =
        (match defined_arity with
         | Env.TypeInformation.CA_zero -> ("no", "1")
         | Env.TypeInformation.CA_some -> ("1", "no")) in
      Format.fprintf ppf
        "@[%tSum@ type@ constructor@ '%t%a%t'@ expected@ %s@ \
         argument@ but@ was@ used@ with@ %s@ argument.@]@."
        header
        Handy.pp_set_underlined Sourcify.pp_constructor_ident ident
        Handy.pp_reset_effects
        expected used
  | Infer.Unbound_type_variable (at, var_name) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ type@ variable@ %t%s%t.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined (Parsetree_utils.name_of_vname var_name)
        Handy.pp_reset_effects
  | Infer.Method_multiply_defined (m_vname, s_name) ->
      Format.fprintf ppf
        "@[%tMethod@ '%t%a%t'@ multiply@ defined@ in@ species@ \
         '%t%a%t'.@]@."
        header
        Handy.pp_set_underlined Sourcify.pp_vname m_vname
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species s_name
        Handy.pp_reset_effects
  | Infer.Bad_type_arity (at, ident, expected, used) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ constructor@ '%t%a%t'@ used@ with@ conflicting@ \
        arities:@ %d@ and@ %d.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_ident ident
        Handy.pp_reset_effects
        expected
        used
  (* *********************** *)
  (* Species type inference. *)
  | Infer.Proof_of_multiply_defined (at1, name, at2) ->
      Format.fprintf ppf
        "%a:@\n@[%tDelayed@ proof@ of@ '%t%a%t'@ was@ found@ several@ \
        times@ in@ the@ species.@ Other@ occurrence@ is@ at:@ %t%a%t.@]@."
        Location.pp_location at1 header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_underlined Location.pp_location at2
        Handy.pp_reset_effects
  | Infer.Proof_of_unknown_property (at, species, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ species@ '%t%a%t',@ proof@ of@ '%t%a%t'@ is@ \
        not@ related@ to@ an@ existing@ property.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_qualified_species species
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
  | Infer.Rep_multiply_defined at ->
      Format.fprintf ppf
        "%a:@\n@[%tCarrier@ 'representation'@ is@ multiply@ defined.@]@."
        Location.pp_location at header
  | Infer.Rep_multiply_defined_by_multiple_inheritance (prev, newer, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCarrier@ 'representation'@ is@ multiply@ defined@ by@ \
        multiple@ inheritance@ and@ was@ formerly@ found@ of@ type@ \
        @[%a@]@ and@ newly@ found@ of@ type@ @[%a@].@]@."
        Location.pp_location at header
        Types.pp_type_simple prev
        Types.pp_type_simple newer
  | Scoping.Self_cant_parameterize_itself at ->
      Format.fprintf ppf
        "%a:@\n@[%t'Self'@ can't@ be@ parametrised@ by@ itself.@]@."
        Location.pp_location at header
  | Scoping.Is_parameter_only_coll_ident at ->
      Format.fprintf ppf
        "%a:@\n@[%tA@ \"is\"@ parameter@ can@ only@ be@ an@ identifier@ \
         of@ a@ collection.@]@."
        Location.pp_location at header
  | Infer.Not_subspecies_conflicting_field (c1, c2, field, ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCollection@ '%t%a%t'@ is@ not@ a@ subspecies@ \
         of@ '%t%a%t'.@ In@ method@ '%a',@ types@ @[%a@]@ and@ @[%a@]@ \
         are@ not@ compatible.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Types.pp_type_collection c1
        Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_collection c2
        Handy.pp_reset_effects
        Sourcify.pp_vname field Types.pp_type_simple ty1
        Types.pp_type_simple ty2
  | Infer.Not_subspecies_circular_field (c1, c2, field, ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCollection@ '%t%a%t'@ is@ not@ a@ subspecies@ of@ \
        '%t%a%t'.@ In@ method@ '%t%a%t',@ type@ @[%t%a@%t]@ occurs@ \
        in@ @[%t%a%t@]@ and@ would@ lead@ to@ a@ cycle.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Types.pp_type_collection c1
        Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_collection c2
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname field
        Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_simple ty1
        Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_simple ty2
        Handy.pp_reset_effects
  | Infer.Not_subspecies_arity_mismatch
      (c1, c2, field, ty_name, ar1, ar2, at) ->
        Format.fprintf ppf
          "%a:@\n@[%tCollection@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ \
           In@ method@ '%a',@ the@ type@ constructor@ '%a'@ is@ used@ \
           with@ the@ different@ arities@ %d@ and@ %d.@]@."
          Location.pp_location at header
          Types.pp_type_collection c1 Types.pp_type_collection c2
          Sourcify.pp_vname field Types.pp_type_name ty_name ar1 ar2
  | Infer.Not_subspecies_missing_field (c1, c2, field, from, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCollection@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ \
         Field@ '%a'@ initially@ coming@ from@ '%a' is@ not@ present@ in@ \
         '%a'.@]@."
        Location.pp_location at header
        Types.pp_type_collection c1 Types.pp_type_collection c2
        Sourcify.pp_vname field
        Sourcify.pp_qualified_species from.Env.fh_initial_apparition
        Types.pp_type_collection c1
  | Infer.Parameterized_species_arity_mismatch msg ->
      Format.fprintf ppf
        "@[%tParameterised@ species@ is@ applied@ to@ %s@ arguments.@]@."
        header msg
  | Infer.Collection_not_fully_defined (coll_name, fields_names) ->
      Format.fprintf ppf
        "@[%tSpecies@ '%a'@ cannot@ be@ turned@ into@ a@ collection.@ \
         Following@ field(s)@ is(are)@ not@ defined:@\n"
        header
        Sourcify.pp_qualified_species coll_name ;
      List.iter
        (function
	  | Infer.NDMK_prototype field_name ->
              Format.fprintf ppf "\t%a@\n" Sourcify.pp_vname field_name
	  | Infer.NDMK_termination_proof fct_name ->
	      Format.fprintf ppf "\t%a@ (missing@ termination@ proof)@\n"
		Sourcify.pp_vname fct_name)
        fields_names ;
      Format.fprintf ppf"@]@."
  | Infer.Invalid_parameter_in_delayed_proof_termination (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ the@ delayed@ termination@ proof,@ parameter@ \
        '%t%a%t'@ does@ not@ refer@ to@ a@ parameter@ of@ the@ original@ \
        function.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
  | Infer.Wrong_recursion_kind_while_fusion at ->
       Format.fprintf ppf
        "%a:@\n@[Inheritance@ tries@ to@ merge@ recursive@ functions@ \
        having@ different@ recursion@ scheme.@]@."
        Location.pp_location at
  | Infer.Wrong_type_by_inheritance (at, name, ty1, ty2, from1, from2) ->
      Format.fprintf ppf
        "%a:@\n@[%tMethod@ '%t%a%t'@ was@ found@ with@ incompatible@ \
        types@ during@ inheritance.@ In@ species@ '%t%a%t':@ \
        @[%a@],@ in@ species@ '%t%a%t':@ @[%a@].@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species
          from1.Env.fh_initial_apparition
        Handy.pp_reset_effects
        Types.pp_type_simple ty1
        Handy.pp_set_underlined Sourcify.pp_qualified_species
          from2.Env.fh_initial_apparition
        Handy.pp_reset_effects
        Types.pp_type_simple ty2
  | Infer.Logical_statements_mismatch (name, sp1, loc1, sp2, loc2) ->
      Format.fprintf ppf
        "%a:@\n@[%tLogical@ method@ '%t%a%t'@ appearing@ in@ species@ \
        '%t%a%t'@ should@ have@ the@ same@ statement@ than@ in@ species@ \
        '%t%a%t'@ at@ %a.@]@."
        Location.pp_location loc1 header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species sp1
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species sp2
        Handy.pp_reset_effects
        Location.pp_location loc2
  | Infer.No_mix_between_logical_defs (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tDefinition@ '%t%a%t'@ is@ considered@ as@ both@ \
        logical@ and@ non-logical.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
  (* ********************** *)
  (* Dependencies analysis. *)
  | Dep_analysis.Ill_formed_species (species_name, field_node, found_path) ->
      Format.fprintf ppf
        "@[%tSpecies@ '%t%a%t'@ is@ not@ well-formed.@ \
        Field@ '%t%a%t'@ involves@ a@ non-declared@ recursion@ \
        for@ the@ following@ dependent@ methods:@ "
        header
        Handy.pp_set_underlined Sourcify.pp_qualified_vname species_name
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname
        field_node.DepGraphData.nn_name
        Handy.pp_reset_effects ;
      Format.fprintf ppf "%a.@]@."
        (Handy.pp_generic_separated_list
          " ->"
          (fun ppf node -> Sourcify.pp_vname ppf node.DepGraphData.nn_name))
        found_path
  (* ********************** *)
  (* External code generation. *)
  | Externals_generation_errs.No_external_value_def (lang, def_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ value@ \
         definition@ '%t%a%t'.@]@."
        Location.pp_location at header
        lang
        Handy.pp_set_underlined Sourcify.pp_vname def_name
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_type_def (lang, def_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ type@ \
         definition@ '%t%a%t'.@]@."
        Location.pp_location at header
        lang
        Handy.pp_set_underlined Sourcify.pp_vname def_name
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_constructor_def
      (lang, cstr_ident) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ sum@ \
         type@ value@ constructor@ '%t%a%t'.@]@."
        Location.pp_location cstr_ident.Parsetree.ast_loc header
        lang
        Handy.pp_set_underlined Sourcify.pp_constructor_ident cstr_ident
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_field_def (lang, label_ident) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ \
         record@ field@ '%t%a%t'.@]@."
        Location.pp_location label_ident.Parsetree.ast_loc header
        lang
        Handy.pp_set_underlined Sourcify.pp_label_ident label_ident
        Handy.pp_reset_effects
  (* ***************************** *)
  (* OCaml code generation errors. *)
  | Env.No_available_OCaml_code_generation_envt file ->
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ OCaml@ generation@ information@ for@ \
        compiled@ file@ '%t%s.fcl%t'.@ Source@ file@ may@ have@ been@ \
        compiled@ without@ OCaml@ code@ generation@ enabled.@]@."
        header
        Handy.pp_set_underlined file Handy.pp_reset_effects
  (* *************************** *)
  (* Coq code generation errors. *)
  | Type_coq_generation.Mutable_record_fields_not_in_coq (at, field) ->
       Format.fprintf ppf
        "%a:@\n@[%tRecord@ type@ definition@ contains@ a@ mutable@ field@ \
        '%t%a%t'@ that@ cannot@ be@ compiled@ to@ Coq.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname field
        Handy.pp_reset_effects
  | Env.No_available_Coq_code_generation_envt file ->
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ Coq@ generation@ information@ for@ \
        compiled@ file@ '%t%s.fcl%t'.@ Source@ file@ may@ have@ been@ \
        compiled@ without@ Coq@ code@ generation@ enabled.@]@."
        header
        Handy.pp_set_underlined file Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_species_param (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ collection@ parameter's@ method@ \
        (%t%a%t)@ in@ a@ Zenon@ proof@ with@ \
        \"by definition\"@ is@ not@ allowed.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_declared_method_of_self
        (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ an@ only@ declared@ method@ of@ Self@ \
        (%t%a%t)@ in@ a@ Zenon@ proof@ with@ \"by definition\"@ \
        is@ not@ allowed.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_local_ident (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ local@ identifier@ (%t%a%t)@ in@ a@ \
        Zenon@ proof@ with@ \"by definition\"@ is@ not@ allowed.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_prop_of_local_ident (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ local@ identifier@ (%t%a%t)@ in@ a@ \
        Zenon@ proof@ with@ \"by property\"@ is@ not@ allowed.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_unknown_hypothesis (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tAssumed@ hypothesis@ '%t%a%t'@ in@ a@ Zenon@ \
        proof@ was@ not@ found.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_unknown_step
     (at, (node_num, node_name)) ->
      Format.fprintf ppf
        "%a:@\n@[%tStep@ '%t<%d>%s%t'@ in@ a@ Zenon@ proof@ was@ not@ \
         found.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined node_num node_name
        Handy.pp_reset_effects
  (* ************************** *)
  (* Recursion analysis errors. *)
  | Recursion.MutualRecursion (name1, name2) ->
      Format.fprintf ppf
        "@[%tMutual@ recursion@ is not@ yet@ supported@ for@ Coq@ code@ \
        generation.@ At@ least@ functions@ '%t%a%t'@ and@ \
        '%t%a%t'@ are@ involved@ in@ a@ mutual@ recursion.@]@."
        header
        Handy.pp_set_underlined Sourcify.pp_vname name1
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name2
        Handy.pp_reset_effects
  | Recursion.NestedRecursiveCalls (function_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tRecursive@ call@ to@ '%t%a%t'@ contains@ nested@ \
         recursion.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname function_name
        Handy.pp_reset_effects
  | Recursion.PartialRecursiveCall (function_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tRecursive@ call@ to@ '%t%a%t'@ is@ incomplete.@]@."
        Location.pp_location at header
        Handy.pp_set_underlined Sourcify.pp_vname function_name
        Handy.pp_reset_effects
  (* ********************** *)
  (* The ultimate firewall. *)
  | x ->
     Format.fprintf ppf
       "@[%tUnexpected@ error:@ \"%s\".@\n%tPlease@ report.%t@]@."
       header
       (Printexc.to_string x)
       Handy.pp_set_videoinv Handy.pp_reset_effects
;;


(* ************************************************************************** *)
(** {b Descr} : Wrapper used to protect the call to the "main". If something
              unexpected arises when proceeding, we exit with the proper
              error code.                                                     *)
(* ************************************************************************** *)
try Focalizec.main () with
| exc ->
  print_focalize_exception Format.err_formatter exc;
  (* And anyway, if an exception occured, we exit with 2 as error code. *)
  exit 2
;;
