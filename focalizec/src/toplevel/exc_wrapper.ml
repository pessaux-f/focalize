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

(* $Id: exc_wrapper.ml,v 1.70 2008-12-17 13:57:04 pessaux Exp $ *)

let print_focalize_exception ppf = function
  (* ********************* *)
  (* General files access. *)
  | Files.Cant_access_file_in_search_path fname ->
      (* In fact, should always be caught by env.ml. *)
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ file%t@ '%t%s%t'@ %tin@ the@ search@ \
         path%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined fname Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Files.Corrupted_fo fname ->
      Format.fprintf ppf
        "@[%tInvalid@ or@ corrupted@ compiled@ interface%t@ '%t%s%t'. May@ \
        be@ it@ was@ compiled@ with@ another@ version@ of@ the@ compiler.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined fname Handy.pp_reset_effects
  | Focalizec.Bad_file_suffix fname ->
      Format.fprintf ppf
        "@[%tInvalid@ file@ extension@ for%t@ '%t%s%t'.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined fname Handy.pp_reset_effects
  | Sys_error m ->
      Format.fprintf ppf "@[%tSystem@ error%t -@ %s.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects m
  | Configuration.Input_file_already_set ->
      Format.fprintf ppf
        "@[%tInput@ file@ name@ is@ already@ set%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
  | Configuration.No_input_file ->
      Format.fprintf ppf
        "@[%tNo@ input@ file.@ FoCaLize@ is@ cowardly@ giving@ up...%t@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
  (* *********************** *)
  (* Lexing / parsing stage. *)
  | Parse_file.Lex_error (pos_s, pos_e, reason) ->
      Format.fprintf ppf
        "%a:@\n@[%tLexical@ error%t@ %s@].@."
        Parse_file.pp_err_loc (pos_s, pos_e)
        Handy.pp_set_bold Handy.pp_reset_effects reason
  | Parse_file.Syntax_error (pos_s, pos_e) ->
      Format.fprintf ppf "%a:@\n@[%tSyntax@ error%t.@]@."
        Parse_file.pp_err_loc (pos_s, pos_e)
        Handy.pp_set_bold Handy.pp_reset_effects
  | Parse_file.Unclear_error (exc_string, pos_s, pos_e) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnclear@ syntax@ error%t:@ %s.@]@."
        Parse_file.pp_err_loc (pos_s, pos_e)
        Handy.pp_set_bold Handy.pp_reset_effects exc_string
  (* ************** *)
  (* Scoping stage. *)
  | Scoping.Multiply_used_module modname ->
      Format.fprintf ppf
        "@[%tModule%t@ '%s'@ %twas@ \"use\"@ several@ times%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        modname
        Handy.pp_set_bold Handy.pp_reset_effects
  | Scoping.Module_not_specified_as_used modname ->
      Format.fprintf ppf
        "@[%tModule%t@ '%s'@ %twas@ not@ declared@ as@ \"use\"%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        modname
        Handy.pp_set_bold Handy.pp_reset_effects
  | Scoping.Parametrized_species_wrong_arity (at, expected, used_with) ->
      Format.fprintf ppf
        "%a:@\n@[%tSpecies@ application@ expression@ expected%t@ %d@ \
         %t@ arguments@ but@ was@ provided%t@ %d.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        expected
        Handy.pp_set_bold Handy.pp_reset_effects
        used_with
  | Scoping.Non_logical_let_cant_define_logical_expr (name, at) ->
       Format.fprintf ppf
        "%a:@\n@[%tNon-logical@ let@ must@ not@ bind%t@ '%t%a%t'@ \
         %tto@ a@ property%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Scoping.Termination_proof_delayed_only_on_self_meth (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tDelayed@ termination@ proof@ refers@ to@ an@ unknown@ \
         method%t %t'%a'%t %tof the species.%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Scoping.Ambiguous_logical_expression_or (pos, at) ->
      let side =
        (match pos with 0 -> "left" | 1 -> "right" | _ -> assert false) in
      Format.fprintf ppf
        "%a:@\n@[%tAmbiguous logical expression. Add explicit parentheses \
        to associate the %s argument of the \\/ properly.%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold side Handy.pp_reset_effects
  | Scoping.Ambiguous_logical_expression_and (pos, at) ->
      let side =
        (match pos with 0 -> "left" | 1 -> "right" | _ -> assert false) in
      Format.fprintf ppf
        "%a:@\n@[%tAmbiguous logical expression. Add explicit parentheses \
        to associate the %s argument of the /\\ properly.%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold side Handy.pp_reset_effects
  (* *************************** *)
  (* Generic environments stuff. *)
  | Env.Unbound_constructor (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ sum@ type@ constructor%t@ '%t%a%t'.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Unbound_label (lvname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ record@ field@ label%t@ '%t%a%t'.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname lvname
        Handy.pp_reset_effects
  | Env.Unbound_identifier (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ identifier%t@ '%t%a%t'.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
  | Env.Unbound_type (tname, at) ->
      Format.fprintf ppf
        "@ %a :@\n@[%tUnbound@ type%t@ '%t%a%t'.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname tname
        Handy.pp_reset_effects
  | Env.Unbound_module (fname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ module%t@ '%t%s%t'.@]@." Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined fname
        Handy.pp_reset_effects
  | Env.Unbound_species (sname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ species%t@ '%t%a%t'.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname sname
        Handy.pp_reset_effects
  | Env.Rebound_type (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ name%t@ '%t%a%t' %talready@ bound@ in@ the@ \
         current@ scope%t.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Env.Rebound_species (vname, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tSpecies@ name%t@ '%t%a%t' %talready@ bound@ in@ the@ \
         current@ scope%t.@]@."
        Location.pp_location at Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname vname
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  (* ******************** *)
  (* Core types problems. *)
  | Types.Conflict (ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tTypes%t@ @[%t%a%t@]@ %tand%t@ @[%t%a%t@]@ %tare@ not@ \
         compatible%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_simple ty1
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined  Types.pp_type_simple ty2
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Types.Circularity (ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[Type@ @[%a@]@ occurs@ in@ @[%a@]@ and@ would@ lead@ \
         to@ a@ cycle.@]@."
        Location.pp_location at
        Types.pp_type_simple ty1 Types.pp_type_simple ty2
  | Types.Arity_mismatch (ty_cstr_name, arity1, arity2, at) ->
      Format.fprintf ppf
        "%a:@\n@[Type@ constructor@ '%a'@ used@ with@ conflicting@ \
         arities:@ %d@ and@ %d.@]@."
        Location.pp_location at Types.pp_type_name ty_cstr_name arity1 arity2
  | Types.Arity_mismatch_unexpected_args (at) ->
      (* To handle error message of bug report #180 (sum type constructor
         used with no argument although it needs some). *)
      Format.fprintf ppf
        "%a:@\n@[No expected argument(s).@]@." Location.pp_location at
  | Infer.Scheme_contains_type_vars (field_name, sch, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ field%t@ '%t%a%t'%t,@ type scheme%t@ @[%a@]@ \
         %tcontains@ variables@ than@ cannot@ be@ generalized@ or@ is@ \
         polymorphic%t.@]@."
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
      Format.fprintf ppf
        "@[%tSum@ type@ constructor%t@ '%t%a%t'@ %texpected@ %s@ \
         argument@ but@ was@ used@ with@ %s@ argument%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_constructor_ident ident
        Handy.pp_reset_effects
        Handy.pp_set_bold expected used Handy.pp_reset_effects
  | Infer.Unbound_type_variable (at, var_name) ->
      Format.fprintf ppf
        "%a:@\n@[%tUnbound@ type@ variable%t@ %t%s%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined (Parsetree_utils.name_of_vname var_name)
        Handy.pp_reset_effects
  | Infer.Method_multiply_defined (m_vname, s_name) ->
      Format.fprintf ppf
        "@[%tMethod%t@ '%t%a%t'@ %tmultiply@ defined@ in@ species%t@ \
         '%t%a%t'.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname m_vname
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species s_name
        Handy.pp_reset_effects
  | Infer.Bad_type_arity (at, ident, expected, used) ->
      Format.fprintf ppf
        "%a:@\n@[%tType@ constructor%t@ '%t%a%t'@ %tused@ with@ conflicting@ \
        arities:%t@ %d@ %tand%t@ %d.@]@."
        Location.pp_location at 
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_ident ident
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        expected
        Handy.pp_set_bold Handy.pp_reset_effects
        used
  (* *********************** *)
  (* Species type inference. *)
  | Infer.Proof_of_multiply_defined (at1, name, at2) ->
      Format.fprintf ppf
        "%a:@\n@[%tDelayed@ proof@ of%t@ '%t%a%t'@ %twas@ found@ several@ \
        times@ in@ the@ species.@ Other@ occurence@ is@ at%t:@ %t%a%t.@]@."
        Location.pp_location at1
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Location.pp_location at2
        Handy.pp_reset_effects
  | Infer.Proof_of_unknown_property (at, species, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ species%t@ '%t%a%t'%t,@ proof@ of%t@ '%t%a%t'%t@ is@ \
        not@ related@ to@ an@ existing@ property%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species species
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Rep_multiply_defined at ->
      Format.fprintf ppf
        "%a:@\n@[%tCarrier@ 'representation'@ is@ multiply@ defined%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Rep_multiply_defined_by_multiple_inheritance (prev, newer, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCarrier@ 'representation'@ is@ multiply@ defined@ by@ \
        multiple@ inheritance@ and@ was@ formerly@ found@ of@ type@ %t@ \
        @[%a@]@ %tand@ newly@ found@ of@ type@ %t@ @[%a@].@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Types.pp_type_simple prev
        Handy.pp_set_bold Handy.pp_reset_effects
        Types.pp_type_simple newer
  | Scoping.Self_cant_parameterize_itself at ->
      Format.fprintf ppf
        "%a:@\n@[%t'Self'@ can't@ be@ parametrised@ by@ itself%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
  | Scoping.Is_parameter_only_coll_ident at ->
      Format.fprintf ppf
        "%a:@\n@[%tA@ \"is\"@ parameter@ can@ only@ be@ a@ collection@ \
         identifier%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Not_subspecies_conflicting_field (c1, c2, field, ty1, ty2, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tCollection%t@ '%t%a%t'@ %tis@ not@ a@ subspecies@ \
         of%t@ '%t%a%t'.@ In@ field@ '%a',@ types@ @[%a@]@ and@ @[%a@]@ \
         are@ not@ compatible.@]@."
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
      Format.fprintf ppf
        "%a:@\n@[%tCollection%t@ '%t%a%t'@ %tis@ not@ a@ subspecies@ of%t@ \
        '%t%a%t'.@ %tIn@ field%t@ '%t%a%t',@ %ttype%t@ @[%t%a@%t]@ %toccurs@ \
        in%t@ @[%t%a%t@]@ %tand@ would@ lead@ to@ a@ cycle%t.@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_collection c1
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_collection c2
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname field
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_simple ty1
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Types.pp_type_simple ty2
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Not_subspecies_arity_mismatch
      (c1, c2, field, ty_name, ar1, ar2, at) ->
        Format.fprintf ppf
          "%a:@\n@[Collection@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ \
           In@ field@ '%a',@ the@ type@ constructor@ '%a'@ is@ used@ \
           with@ the@ different@ arities@ %d@ and@ %d.@]@."
          Location.pp_location at
          Types.pp_type_collection c1 Types.pp_type_collection c2
          Sourcify.pp_vname field Types.pp_type_name ty_name ar1 ar2
  | Infer.Not_subspecies_missing_field (c1, c2, field, at) ->
      Format.fprintf ppf
        "%a:@\n@[Collection@ '%a'@ is@ not@ a@ subspecies@ of@ '%a'.@ \
         Field@ '%a'@ is@ not@ present@ in@ '%a'.@]@."
        Location.pp_location at
        Types.pp_type_collection c1 Types.pp_type_collection c2
        Sourcify.pp_vname field Types.pp_type_collection c1
  | Infer.Parameterized_species_arity_mismatch msg ->
      Format.fprintf ppf
        "@[Parameterized@ specie@ is@ applied@ to@ %s@ arguments.@]@."  msg
  | Infer.Collection_not_fully_defined_missing_term_proof
        (coll_name, field_name) ->
      Format.eprintf
        "@[%tSpecies%t@ %t'%a'%t@ %tcannot@ be@ turned@ into@ a@ collection.@ \
        Field%t@ %t'%a'%t@ %tdoes@ not@ have@ a@ termination@ proof.%t@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species coll_name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname field_name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Collection_not_fully_defined (coll_name, field_name) ->
      Format.fprintf ppf
        "@[Species@ '%a'@ cannot@ be@ turned@ into@ a@ collection.@ \
         Field@ '%a'@ is@ not@ defined.@]@."
        Sourcify.pp_qualified_species coll_name
        Sourcify.pp_vname field_name
  | Infer.Invalid_parameter_in_delayed_proof_termination (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tIn@ the@ delayed@ termination@ proof,@ parameter%t@ \
        %t'%a'%t@ %tdoes@ not@ refer@ to@ a@ parameter@ of@ the@ original@ \
        function.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Infer.Wrong_type_by_inheritance (at, name, ty1, ty2, from1, from2) ->
      Format.fprintf ppf
        "%a:@\n@[%tMethod%t@ %t'%a'%t@ %twas@ found@ with@ incompatible@ \
        types@ during@ inheritance.@ In@ species%t@ %t'%a'%t:@ \
        @[%a@],@ %tin@ species%t@ %t'%a'%t:@ @[%a@].@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species
          from1.Env.fh_initial_apparition
        Handy.pp_reset_effects
        Types.pp_type_simple ty1
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species
          from2.Env.fh_initial_apparition
        Handy.pp_reset_effects
        Types.pp_type_simple ty2
  | Infer.Logical_statements_mismatch (name, sp1, loc1, sp2, loc2) ->
      Format.fprintf ppf
        "%a:@\n@[%tLogical@ method%t@ %t'%a'%t@ %tappearing@ in@ species%t@ \
        %t'%a'%t@ %tshould@ have@ the@ same@ statement@ than@ in@ species%t@ \
        %t'%a'%t@ %tat%t@ %a@]@."
        Location.pp_location loc1
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species sp1
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_species sp2
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Location.pp_location loc2
  (* ********************** *)
  (* Dependencies analysis. *)
  | Dep_analysis.Ill_formed_species (species_name, field_node, found_path) ->
      Format.fprintf ppf
        "@[%tSpecies%t@ %t'%a'%t@ %tis@ not@ well-formed.@ \
        Field%t@ %t'%a'%t@ %tinvolves@ a@ non-declared@ recursion@ \
        for@ the@ following@ dependent@ fields:%t@ "
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_qualified_vname species_name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname
        field_node.DepGraphData.nn_name
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects;
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
         definition%t@ '%t%a%t'.@]@."
        Location.pp_location at
        Handy.pp_set_bold lang Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname def_name
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_type_def (lang, def_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ type@ \
         definition%t@ '%t%a%t'.@]@."
        Location.pp_location at
        Handy.pp_set_bold lang Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname def_name
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_constructor_def
      (lang, cstr_ident) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ sum@ \
         type@ constructor%t@ '%t%a%t'.@]@."
        Location.pp_location cstr_ident.Parsetree.ast_loc
        Handy.pp_set_bold lang Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_constructor_ident cstr_ident
        Handy.pp_reset_effects
  | Externals_generation_errs.No_external_field_def (lang, label_ident) ->
      Format.fprintf ppf
        "%a:@\n@[%tNo@ %s@ mapping@ given@ for@ the@ external@ \
         record@ field%t@ '%t%a%t'.@]@."
        Location.pp_location label_ident.Parsetree.ast_loc
        Handy.pp_set_bold lang Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_label_ident label_ident
        Handy.pp_reset_effects
  (* ***************************** *)
  (* OCaml code generation errors. *)
  | Env.No_available_OCaml_code_generation_envt file ->
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ OCaml@ generation@ information@ for@ \
        compiled@ file@ %t'%t%s.foc%t'%t.@ Source@ file@ may@ have@ been@ \
        compiled@ without@ OCaml@ code@ generation@ enabled.%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined file Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  (* *************************** *)
  (* Coq code generation errors. *)
  | Type_coq_generation.Mutable_record_fields_not_in_coq (at, field) ->
       Format.fprintf ppf
        "%a:@\n@[%tType@ definition@ contains@ a@ mutable@ field%t@ \
        '%t%a%t'@ %tthat@ can't@ be@ compiled@ to@ Coq.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname field
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Env.No_available_Coq_code_generation_envt file ->
      Format.fprintf ppf
        "@[%tUnable@ to@ find@ Coq@ generation@ information@ for@ \
        compiled@ file@ %t'%t%s.foc%t'%t.@ Source@ file@ may@ have@ been@ \
        compiled@ without@ Coq@ code@ generation@ enabled.%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined file Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_species_param (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ collection@ parameter's@ method@ \
        %t(%t%a%t)%t@ in@ a@ Zenon@ proof@ with@ \
        \"by definition\"@ is@ not@ allowed.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_declared_method_of_self
        (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ an@ only@ declared@ method@ of@ Self@ \
        %t(%t%a%t)%t@ in@ a@ Zenon@ proof@ with@ \"by definition\"@ \
        is@ not@ allowed.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_def_of_local_ident (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ local@ identifier@ %t(%t%a%t)%t@ in@ a@ \
        Zenon@ proof@ with@ \"by definition\"@ is@ not@ allowed.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_prop_of_local_ident (at, id) ->
      Format.fprintf ppf
        "%a:@\n@[%tUsing@ a@ local@ identifier@ %t(%t%a%t)%t@ in@ a@ \
        Zenon@ proof@ with@ \"by property\"@ is@ not@ allowed.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_expr_ident id
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_unknown_hypothesis (at, name) ->
      Format.fprintf ppf
        "%a:@\n@[%tAssumed@ hypothesis@ %t'%t%a%t'%t@ in@ a@ Zenon@ \
        proof@ was@ not@ found.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Species_coq_generation.Attempt_proof_by_unknown_step
     (at, (node_num, node_name)) ->
      Format.fprintf ppf
        "%a:@\n@[%tStep@ %t'%t<%d>%s%t'%t@ in@ a@ Zenon@ proof@ was@ not@ \
         found.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined node_num node_name
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  (* ************************** *)
  (* Recursion analysis errors. *)
  | Recursion.MutualRecursion (name1, name2) ->
      Format.fprintf ppf
        "@[%tMutual@ recursion@ is not@ yet@ supported@ for@ Coq@ code@ \
        generation.@ At@ least@ functions%t@ '%t%a%t'@ %tand%t@ \
        '%t%a%t'@ %tare@ involved@ in@ a@ mutual@ recursion%t.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name1
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname name2
        Handy.pp_reset_effects
        Handy.pp_set_bold Handy.pp_reset_effects
  | Recursion.NestedRecursiveCalls (function_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tRecursive@ call@ to%t@ '%t%a%t'%t@ contains@ nested@ \
         recursion.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname function_name
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  | Recursion.PartialRecursiveCall (function_name, at) ->
      Format.fprintf ppf
        "%a:@\n@[%tRecursive@ call@ to%t@ '%t%a%t'%t@ is@ incomplete.%t@]@."
        Location.pp_location at
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname function_name
        Handy.pp_reset_effects Handy.pp_set_bold Handy.pp_reset_effects
  (* ********************** *)
  (* The ultimate firewall. *)
  | x ->
     Format.fprintf ppf
       "@[%tUnexpected@ error%t:@ \"%s\".@\n%tPlease@ report%t.@]@."
       Handy.pp_set_bold Handy.pp_reset_effects
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
  (* And anyway, if an exception occured, we exit with -1 as error code. *)
  exit (-1)
;;
