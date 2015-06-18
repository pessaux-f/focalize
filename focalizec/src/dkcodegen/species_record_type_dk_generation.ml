(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************* *)
(** {b Descr}: Exception raised when a termination proof stated as structural
    refers to an identifier not being a parameter of the recursive function.
    [Unsure] on my mind, this should have been done ealier, may be at scoping.

    {b Visibility}: Exported outside this module.                            *)
(* ************************************************************************* *)
exception Wrong_decreasing_argument of
  (Location.t * Parsetree.qualified_species * Parsetree.vname *
   Parsetree.vname)
;;



(* ************************************************************************* *)
(* only_for_Self: bool -> Format.formatter -> Parsetree.vname list ->        *)
(*  (Env.TypeInformation.species_param *                                     *)
(*   Env.ordered_methods_from_params) list ->                                *)
(*     Parsetree.vname list -> unit                                          *)
(** {b Args} :
      - [~only_for_Self_meths] : This serves to the bug fix #211. In effect,
        this bug report shown the need in a Zenon proof to instantiate the
        species parameters while creating the "Let"s needed in the Section.
        Otherwise, the generated code would assume that the extra arguments
        due to species parameters of the inherited are still parameters of
        the inheriting species, hence generating unbound "_p_XXX"s.
        Because this fix involved [generate_def_dependency_equivalence], we
        made the check in that function. And that function calls us to
        generate lambda-liftings. Then, if
        [generate_def_dependency_equivalence] already generated the
        abstractions related to species parameters, we do not need to do them
        using our "_p_XXX" scheme. So, if this flag is true, then we only
        generate lambda-liftings for methods of "Self" and nothing for
        species parameters carriers and methods.

        Note: The present function is used in various locations, where it is
        not clear if the bug fix #211 should also be applied. In other words,
        this function is used in other locations where generating
        lambda-liftings by our "_p_XXX" scheme seems to work. So to prevent
        breaking anything, at this locations, we let the code behaving like
        before, and passing [false] to call us. May be a deeper investigation
        is needed to understand if at these other locations, the same
        principle should be applied. If another bug similar to #211 arises,
        we should remind to have a look here.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let generate_method_lambda_lifted_arguments ~only_for_Self_meths out_fmter
    used_species_parameter_tys sorted_deps_from_params abstracted_methods =
  if not only_for_Self_meths then (
    (* We first instanciate the parameters corresponding to the carriers types
       of species parameters and appearing in the method's type *)
    List.iter
      (fun n ->
        Format.fprintf out_fmter "@ _p_%a_T"
          Parsetree_utils.pp_vname_with_operators_expanded n)
      used_species_parameter_tys ;
    (* Now apply the abstracted methods from the species params we depend on. *)
    List.iter
      (fun (sparam, (Env.ODFP_methods_list meths)) ->
        (* Recover the species parameter's name. *)
        let species_param_name =
          match sparam with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
               Parsetree.Vuident n in
        (* Each abstracted method will be named like "_p_", followed by the
           species parameter name, followed by "_", followed by the method's
           name.
           We don't care here about whether the species parameters is "in" or
           "is". *)
        let prefix =
          "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
        List.iter
          (fun (meth, _) ->
            Format.fprintf out_fmter "@ %s%a"
              prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
          meths)
      sorted_deps_from_params
   ) ;
  (* And finally, apply to the methods from ourselves we depend on. *)
  List.iter
    (fun n ->
      if n = Parsetree.Vlident "rep" then Format.fprintf out_fmter "@ abst_T"
      else
        Format.fprintf out_fmter "@ abst_%a"
          Parsetree_utils.pp_vname_with_operators_expanded n)
    abstracted_methods ;
;;



(** Just helpers to make a binding for self for a [collection_carrier_mapping]
    in order to make known how Self must be printed while printing **TYPES**
    The "_T" will be added automatically by the printing routine. We add the
    species as a [CCMI_is] to have it printed as "xxx_T" and not as an entity
    parameter. *)
let make_Self_cc_binding_abst_T ~current_species =
  let (module_name, _) = current_species in
  ((module_name, "Self"), ("abst" ,Types.CCMI_is))
;;
let make_Self_cc_binding_rf_T ~current_species =
  let (module_name, species_name) = current_species in
  ((module_name, "Self"),
   (Printf.sprintf "%s__rf"
      (Parsetree_utils.name_of_vname species_name),
    Types.CCMI_is))
;;
let make_Self_cc_binding_species_param ~current_species spe_param_name =
  let (module_name, _) = current_species in
  ((module_name, "Self"),
   (("_p_" ^ (Parsetree_utils.name_of_vname spe_param_name)), Types.CCMI_is))
;;



type self_methods_status =
  | SMS_from_param of Parsetree.vname  (** Must be called "_p_Param_<meth>". *)
  | SMS_abstracted     (** Must be called "abst_<meth>". *)
  | SMS_from_record    (** Must be called "(hosting_species. if needed)
                           <rf_meth>". *)
;;



type recursive_methods_status =
  | RMS_abstracted     (** Must be called "abst_<meth>". *)
  | RMS_regular        (** Must be called directly by its name. *)
;;



let generate_expr_ident_for_E_var ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  let (_, species_name) = ctx.Context.scc_current_species in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname -> (
       (* Thanks to the scoping pass, identifiers remaining "local" are either
          really let-bound in the context of the expression, hence have a
          direct mapping between FoCaL and OCaml code, or species
          "IN"-parameters and then must be mapped onto the lambda-lifted
          parameter introduced for it in the context of the current species.
          Be careful, because a recursive method called in its body is scoped
          AS LOCAL ! And because recursive methods do not have dependencies
          together, there is no way to recover the extra parameters to apply to
          them in this configuration. Hence, to avoid forgetting these extra
          arguments, we must use here the information recorded in the context,
          i.e. the extra arguments of the recursive functions.
          To check if a "smelling local" identifier is really local or a
          "IN"-parameter of the species, we use the same reasoning that in
          [param_dep_analysis.ml]. Check justification over there ! *)
       if (List.exists
             (fun species_param ->
               match species_param with
                | Env.TypeInformation.SPAR_in (vn, _, _) -> vn = vname
                | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                    (Parsetree.Vuident vn) = vname)
             ctx.Context.scc_species_parameters_names) &&
         (not (List.mem vname local_idents)) then (
         (* In fact, a species "IN"-parameter. This parameter was of the form
            "foo in C". Then it's naming scheme will be "_p_" + the species
            parameter's name + the method's name that is trivially the
            parameter's name again (because this last one is computed as the
            "stuff" a dependency was found on, and in the case of a
            "IN"-parameter, the dependency can only be on the parameter's value
            itself, not on any method since there is none !). *)
         Format.fprintf out_fmter "_p_%a_%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
           Parsetree_utils.pp_vname_with_operators_expanded vname
         )
       else (
         (* Really a local identifier or a call to a recursive method. *)
         let is_a_rec_fun = List.mem vname in_recursive_let_section_of in
         (* If the function is recursive, we must apply to it the naming scheme
            applied to recursive functions of Self. This means if
            must be called "abst_xxx" or "xxx" depending on the current
            "recursive_methods_status". In effect, when giving to Zenon the
            body of a recursive function, since we are in a Section, the
            methods of "Self" are abstracted and named "abst_xxx". In all
            other places, the recursive functions names must be generated
            without anything more than their name. *)
         if is_a_rec_fun then
           (match recursive_methods_status with
             | RMS_abstracted ->
                 Format.fprintf out_fmter "abst_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
             | RMS_regular ->
                 Format.fprintf out_fmter "%a"
                   Parsetree_utils.pp_vname_with_operators_expanded vname)
         else
           Format.fprintf out_fmter "%a"
             Parsetree_utils.pp_vname_with_operators_expanded vname ;
         (* Because this method can be recursive, we must apply it to its
            extra parameters if it has some ONLY if the function IS NOT a
            recursive one. In effect, in this last case, a Section has been
            created with all the abstrations the function requires. So no need
            to apply each recursive call. *)
         if not is_a_rec_fun then (
            try
            (* We are not in a recursive definition, so we can apply to the
               lambda-lifted extra arguments. *)
            let extra_args =
               List.assoc vname ctx.Context.scc_lambda_lift_params_mapping in
             List.iter (fun s -> Format.fprintf out_fmter "@ %s" s) extra_args
              with Not_found -> ()
            )
         )
      )
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
   | Parsetree.EI_global (Parsetree.Qualified (mod_name, vname)) ->
       (* Call the Dk corresponding identifier in the corresponding   *)
       (* module (i.e. the [mod_name]). If the module is the currently *)
       (* compiled one, then do not qualify the identifier.            *)
       if mod_name <> ctx.Context.scc_current_unit then
         Format.fprintf out_fmter "%s.%a"
           mod_name Parsetree_utils.pp_vname_with_operators_expanded vname
       else
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.EI_method (coll_specifier_opt, vname) -> (
       match coll_specifier_opt with
       | None
       | Some (Parsetree.Vname (Parsetree.Vuident "Self")) -> (
           (* Method call from the current species. *)
           match self_methods_status with
           | SMS_abstracted ->
              (* On the Dedukti side, def dependencies are not
                 abstracted because Dedukti lacks let. *)
              (* We check here if the method is defined,
                 in which case we print its full name and dependencies *)
              let is_defined = false in
              if is_defined
              then
                Format.fprintf out_fmter "%a__%a"
                  Sourcify.pp_vname (snd ctx.Context.scc_current_species)
                  Parsetree_utils.pp_vname_with_operators_expanded vname
              else
                Format.fprintf out_fmter "abst_%a"
                  Parsetree_utils.pp_vname_with_operators_expanded vname
           | SMS_from_record ->
               Format.fprintf out_fmter "%a__rf_%a"
                 Sourcify.pp_vname species_name
                 Parsetree_utils.pp_vname_with_operators_expanded vname
           | SMS_from_param spe_param_name ->
               Format.fprintf out_fmter "_p_%a_%a"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 spe_param_name
                 Parsetree_utils.pp_vname_with_operators_expanded vname
          )
       | Some coll_specifier -> (
           match coll_specifier with
           | Parsetree.Vname coll_name -> (
               (* Method call from a species that is not the current but is
                  implicitely in the current compilation unit. May be
                  either a paramater or a toplevel defined collection. *)
               if List.exists
                   (fun species_param ->
                     match species_param with
                     | Env.TypeInformation.SPAR_in (vn, _, _) ->
                         vn = coll_name
                     | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                         (Parsetree.Vuident vn) = coll_name)
                   ctx.Context.scc_species_parameters_names then (
                 (* It comes from a parameter. To retrieve the related
                    method name we build it the same way we built it
                    while generating the extra Dk function's parameters due
                    to depdencencies coming from the species parameter.
                    I.e: "_p_", followed by the species parameter name,
                    followed by "_", followed by the method's name. *)
                 let prefix =
                   "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^ "_" in
                 Format.fprintf out_fmter "%s%a"
                   prefix Parsetree_utils.pp_vname_with_operators_expanded
                   vname
                )
               else (
                 if coll_name = (snd ctx.Context.scc_current_species) then (
                   (* In fact, the name is qualified but with ourself
                      implicitely in the current compilation unit. Then, we
                      are not in the case of a toplevel species but in the
                      case where a substitution replaced Self by ourself.
                      We then must refer to our local record field. *)
                   Format.fprintf out_fmter "%a__rf_%a"
                     Sourcify.pp_vname species_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                  )
                 else (
                   (* It comes from a toplevel stuff, hence not abstracted by
                      lambda-lifting. Then, we get the field of the
                      module representing the collection. *)
                   Format.fprintf out_fmter "%a__%a"
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                  )
                )
              )
           | Parsetree.Qualified (module_name, coll_name) -> (
               if module_name = ctx.Context.scc_current_unit then (
                 (* Exactly like when it is method call from a species that
                    is not the current but is implicitely in the current
                    compilation unit : the call is performed to a method a
                    species that is EXPLICITELY in the current compilation
                    unit. *)
                 if List.exists
                     (fun species_param ->
                       match species_param with
                       | Env.TypeInformation.SPAR_in (vn, _, _) ->
                           vn = coll_name
                       | Env.TypeInformation.SPAR_is ((_, vn), _, _, _, _) ->
                           (Parsetree.Vuident vn) = coll_name)
                     ctx.Context.scc_species_parameters_names then (
                   (* It comes from one of our species parameters. *)
                   let prefix =
                     "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^"_" in
                   Format.fprintf out_fmter "%s%a"
                     prefix Parsetree_utils.pp_vname_with_operators_expanded
                     vname
                  )
                 else (
                   (* It's not from one of our species parameter but it comes
                      from the current compilation unit. Let's check if the
                      species is ourself. In this case, liek above we must
                      refer to our local record field. *)
                   if coll_name = (snd ctx.Context.scc_current_species) then
                     Format.fprintf out_fmter "%a__rf_%a"
                       Sourcify.pp_vname species_name
                       Parsetree_utils.pp_vname_with_operators_expanded vname
                   else (
                     Format.fprintf out_fmter "%a__%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       coll_name
                       Parsetree_utils.pp_vname_with_operators_expanded vname
                    )
                  )
                )
               else (
                 (* The called method belongs to a species that is not
                    ourselves and moreover belongs to another compilation
                    unit. May be a species from the toplevel of another
                    FoCaL source file. *)
                 Format.fprintf out_fmter "%s.%a__%a"
                   module_name
                   Parsetree_utils.pp_vname_with_operators_expanded coll_name
                   Parsetree_utils.pp_vname_with_operators_expanded vname
                )
              )
          )
      )
;;

let butfst (str : string) : string =
  String.sub str 1 (String.length str - 1)
;;

let butlast (str : string) : string =
  String.sub str 0 (String.length str - 1)
;;

let last (str : string) : char =
  str.[String.length str - 1]
;;

(* TODO *)
let generate_constant_pattern ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int _ ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "C_int"
   | Parsetree.C_float _ ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "C_float"
   | Parsetree.C_bool str ->
       (* [true] maps on Dk "true". [false] maps on Dk "false". *)
       Format.fprintf ctx.Context.scc_out_fmter "%s" str
   | Parsetree.C_string str ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "\"%s\"%%string" str
   | Parsetree.C_char c ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "\"%c\"%%char" c
;;

(* Print a char code (7 bits representing a char using ascii)
   in Dedukti. *)
let rec print_char_code_to_dk fmter n =
  if n == 0 then Format.fprintf fmter "dk_char._O"
  else Format.fprintf fmter "@[<2>(dk_char.S%d@ %a)@]"
                      (n mod 2)
                      print_char_code_to_dk (n / 2)
;;

(* Print a char to Dedukti.
   If an alias exists in dk_char, then use it to increase readability.
 *)
let print_char_to_dk fmter c =
  if
    (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9')
  then Format.fprintf fmter "dk_char.%c" c
  else
    match c with
    | '-' -> Format.fprintf fmter "dk_char.hyphen"
    | '_' -> Format.fprintf fmter "dk_char.__"
    | ' ' -> Format.fprintf fmter "dk_char.space"
    | c -> Format.fprintf fmter "@[<2>(dk_char.cast@ %a)@]"
                         print_char_code_to_dk (int_of_char c)
;;

let generate_constant ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int str ->
      let sign = (str.[0] = '-') in
      let abs_str =
        if str.[0] = '-' || str.[0] = '+'
        then butfst str
        else str
      in
      let rec print_abs fmter s =
        if s = "" then (
          Format.fprintf fmter "dk_nat.dnil"
        ) else (
          Format.fprintf fmter
                         "@[<2>(dk_nat.dcons@ %a@ dk_nat._%c)@]"
                         print_abs (butlast s)
                         (last s)
        )
      in
      if sign
      then Format.fprintf ctx.Context.scc_out_fmter
                          "@[<2>(dk_int.opp@ ";
      Format.fprintf ctx.Context.scc_out_fmter
                     "@[<2>(dk_int.from_nat@ @[<2>(dk_nat.list_to_nat %a)@])@]"
                     print_abs abs_str;
      if sign
      then Format.fprintf ctx.Context.scc_out_fmter
                          ")@]";
   | Parsetree.C_float _str ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "C_float"
   | Parsetree.C_bool str ->
       (* [true] maps on Dk "true". [false] maps on Dk "false". *)
       Format.fprintf ctx.Context.scc_out_fmter "dk_bool.%s" str
   | Parsetree.C_string str ->
      Format.fprintf ctx.Context.scc_out_fmter "\"%s\"" str;
   | Parsetree.C_char c ->
      Format.fprintf ctx.Context.scc_out_fmter "(; \'%c\' ;)@ " c;
      print_char_to_dk ctx.Context.scc_out_fmter c
;;


(* Special pair constructor *)
let pair_cident =
  let pair_ident_desc = Parsetree.I_global
                          (Parsetree.Qualified
                             ("dk_tuple",
                              Parsetree.Vlident "pair")) in
  let pair_ident = Parsetree_utils.make_ast pair_ident_desc in
  let pair_cident_desc = Parsetree.CI pair_ident in
  Parsetree_utils.make_ast pair_cident_desc
;;

let generate_constructor_ident_for_method_generator ctx env cstr_expr =
  if cstr_expr = pair_cident then 2 else
    (begin
        let mapping_info =
          try
            Env.DkGenEnv.find_constructor
              ~loc: cstr_expr.Parsetree.ast_loc
              ~current_unit: ctx.Context.scc_current_unit cstr_expr env
          (* Since in Dk all the constructors must be inserted in the generation
       environment, if we don't find the constructor, then we were wrong
       somewhere else before. *)

          with _ -> assert false
        in
        (match mapping_info.Env.DkGenInformation.cmi_external_translation with
         | None -> ()
           (* The constructor isn't coming from an external definition. *)
         | Some external_expr -> (
           (* The constructor comes from an external definition. *)
           if not (List.exists
                   (function
                     | (Parsetree.EL_Dk, _) -> true
                     | (Parsetree.EL_Caml, _)
                     | (Parsetree.EL_Coq, _)
                     | ((Parsetree.EL_external _), _) -> false)
                   external_expr)
           then
               (* No Dk mapping found. *)
               raise
                 (Externals_generation_errs.No_external_constructor_def
                    ("Dk", cstr_expr))
        )) ;
        (* Always returns the number of type arguments that must be printed
       after the constructor. *)
        mapping_info.Env.DkGenInformation.cmi_num_polymorphics_extra_args
      end)
;;



(* Exactly the same principle than for sum type constructors in the above
   function [generate_constructor_ident_for_method_generator]. *)
let generate_record_label_for_method_generator ctx env label =
  try
    let mapping_info =
      Env.DkGenEnv.find_label
        ~loc: label.Parsetree.ast_loc
        ~current_unit: ctx.Context.scc_current_unit label env in
    (match mapping_info.Env.DkGenInformation.lmi_external_translation with
    | None -> (
        (* The label isn't coming from an external definition. *)
        let Parsetree.LI global_ident = label.Parsetree.ast_desc in
        match global_ident.Parsetree.ast_desc with
          | Parsetree.I_local name
          | Parsetree.I_global (Parsetree.Vname name) ->
              Format.fprintf ctx.Context.scc_out_fmter "%a"
                Parsetree_utils.pp_vname_with_operators_expanded name
          | Parsetree.I_global (Parsetree.Qualified (fname, name)) ->
              (* If the constructor belongs to the current compilation unit
                 then one must not qualify it. *)
              if fname <> ctx.Context.scc_current_unit then
                Format.fprintf ctx.Context.scc_out_fmter "%s.%a"
                  fname          (* No module name capitalization in Dk. *)
                  Parsetree_utils.pp_vname_with_operators_expanded name
              else
                Format.fprintf ctx.Context.scc_out_fmter "%a"
                  Parsetree_utils.pp_vname_with_operators_expanded name
        )
    | Some external_expr ->
        (* The constructor comes from an external definition. *)
        let (_, dk_binding) =
          try
            List.find
              (function
                | (Parsetree.EL_Dk, _) -> true
                | (Parsetree.EL_Caml, _)
                | (Parsetree.EL_Coq, _)
                | ((Parsetree.EL_external _), _) -> false)
              external_expr
          with Not_found ->
            (* No Dk mapping found. *)
            raise
              (Externals_generation_errs.No_external_field_def
                 ("Dk", label)) in
        (* Now directly generate the name the label is mapped onto. *)
        Format.fprintf ctx.Context.scc_out_fmter "%s" dk_binding) ;
    (* Always returns the number of type arguments that must be printed
       after the label. *)
    mapping_info.Env.DkGenInformation.lmi_num_polymorphics_extra_args
  with _ ->
    (* Since in Dk all the record labels must be inserted in the generation
       environment, if we don't find the label, then we were wrong
       somewhere else before. *)
    assert false
;;


(* Takes an Parsetree.ast and print its type *)
let generate_simple_type_of_ast dkctx out_fmter a =
 match a.Parsetree.ast_type with
 | Parsetree.ANTI_none ->
    Format.fprintf out_fmter "no_type"
 | Parsetree.ANTI_irrelevant ->
    Format.fprintf out_fmter "irrelevant_type"
 | Parsetree.ANTI_scheme ts ->
    Dk_pprint.pp_type_simple_to_dk dkctx out_fmter (Types.specialize ts)
 | Parsetree.ANTI_type st ->
    Dk_pprint.pp_type_simple_to_dk dkctx out_fmter st
;;

(* ************************************************************************** *)
(* force_polymorphic_explicit_args: bool -> Context.species_compil_context -> *)
(*   Env.DkGenEnv.t -> Parsetree.pattern -> unit                             *)
(** {b Descr} : Emits dk code for a pattern. Attention, this function can
    also be used to generate code from a pettern but not in the context of
    generating a pattern in the target code (see description of
    [~force_polymorphic_explicit_args]).

    This function does more than its Coq counterpart because Dedukti patterns
    are only allowed in rewrite-rules, so at toplevel.
    Moreover, Dedukti patterns are not equivalent to ml patterns because
    confluence has to be guaranteed. Hence
      match x with
        | 0 -> 0
        | _ -> 1
    cannot be translated in Dedukti by two rewrite rules.
    TODO (future optimization): realize when patterns are orthogonal and can
    hence be compiled by rewrite-rules.

    In Dedukti, each pattern is translated as a function with a continuation.
    Exhaustivity is not checked, empty pattern-matching compiles to "run-time"
    failure.

    The code produced by this function is equivalent to
    (match e with pat -> d | _ -> k) : ret_type

    {b Args} :
      - [~d]: A function to print the term bound to the pattern.
      - [~k]: A continuation in case the pattern is not matched.
      - [~ret_type]: The type returned by the pattern.
      - [~e]: a function to print the expression matched

    {b Exported} : Yes                                                        *)
(* ************************************************************************** *)
let generate_pattern ctx dkctx env pattern
                     ~d ~k ~ret_type ~e =
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec rec_gen_pat pat ~d ~k ~ret_type ~e =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant_pattern ctx constant
     | Parsetree.P_var name ->
        (* "match e with x -> d | _ -> k" is the same as "(fun x -> d) e" *)
        Format.fprintf out_fmter "(%a :@ cc.eT@ "
                       Parsetree_utils.pp_vname_with_operators_expanded name;
        generate_pattern_type pat;
        Format.fprintf out_fmter " =>@ ";
        d ();
        Format.fprintf out_fmter ")@ ";
        e ()
     | Parsetree.P_as (p, name) ->
        (* "match e with p(y) as x -> d(x,y) | _ -> k"
           is the same as
           "(fun x -> (match e with p(y) -> d(x,y))) e" *)
        Format.fprintf out_fmter "(%a :@ "
                       Parsetree_utils.pp_vname_with_operators_expanded name;
        generate_pattern_type pat;
        Format.fprintf out_fmter " =>@ ";
        rec_gen_pat p ~d ~k ~ret_type ~e;
        Format.fprintf out_fmter ")@ ";
        e ()
     | Parsetree.P_wild ->
        (* "match e with _ -> d" is the same as "d" *)
        d ();
     | Parsetree.P_record _labs_pats ->
         Format.eprintf "generate_pattern P_record TODO@."
     | Parsetree.P_tuple [] -> assert false (* Tuples should not be empty *)
     | Parsetree.P_tuple [ p ] -> rec_gen_pat p ~d ~k ~ret_type ~e
     | Parsetree.P_tuple [ p1 ; p2 ] -> (* Tuples are a special case of constructor *)
        let desc = Parsetree.P_constr (pair_cident, [p1 ; p2] ) in
        (* Update pattern type to reflect current use *)
        let ast = Parsetree_utils.make_ast desc in
        ast.Parsetree.ast_type <- pat.Parsetree.ast_type;
        rec_gen_pat ast ~d ~k ~ret_type ~e
     | Parsetree.P_tuple (p :: pats) -> (* Tuples are a special case of constructor *)
        let tail = Parsetree_utils.make_ast (Parsetree.P_tuple pats) in
        let desc = Parsetree.P_constr (pair_cident, [p ; tail] ) in
        (* Update pattern type to reflect current use *)
        let ast = Parsetree_utils.make_ast desc in
        ast.Parsetree.ast_type <- pat.Parsetree.ast_type;
        rec_gen_pat ast ~d ~k ~ret_type ~e
     | Parsetree.P_paren p ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_gen_pat p ~d ~k ~ret_type ~e;
         Format.fprintf out_fmter ")@]"
     | Parsetree.P_constr (cident, pats) -> (* Most interesting case *)
        (* A function match__C : PV (Polymorphic variables) : * ->
                                 DT PV ->
                                 RT (Return type) : * ->
                                 then_case : (ty_1 -> .. ty_n-> RT) ->
                                 else_case : RT ->
                                 RT
                      is available in the same dedukti file than the constructor
         *)

        (* match e with P(p1, p2) -> d | _ -> k
           is the same as
           match e with
           | P(x, y) -> (match x with
              | p1 -> (match y with
                | p2 -> d
                | _ -> k)
              | _ -> k)
           | _ -> k
         where x and y are fresh variables (pattern_variable_%d)
         *)

        let Parsetree.CI ident = cident.Parsetree.ast_desc in
        let pattern_file_name, pattern_vname = match ident.Parsetree.ast_desc with
          | Parsetree.I_global (Parsetree.Qualified (f, v))  -> (f ^ ".", v)
          | Parsetree.I_global (Parsetree.Vname v)
          | Parsetree.I_local v -> ("", v)
        in
        Format.fprintf out_fmter "@[<1>%smatch__%a"
                       pattern_file_name
                       Sourcify.pp_vname pattern_vname;
        (* Now polymorphic variables *)
        (* Number of polymorphic variables *)
        let extras =
           generate_constructor_ident_for_method_generator ctx env cident
        in
        (begin
            match pat.Parsetree.ast_type with
            | Parsetree.ANTI_type t ->
               Dk_pprint.pp_type_simple_args_to_dk dkctx out_fmter t extras
            | _ ->
               Format.fprintf out_fmter "(unknown_pattern_type %a)"
                              Parsetree_utils.pp_vname_with_operators_expanded
                              (Parsetree_utils.unqualified_vname_of_constructor_ident
                                 cident)
          end) ;
        (* Now the return type *)
        Format.fprintf out_fmter "@ ";
        generate_simple_type_of_ast dkctx out_fmter ret_type ;
        (* Now the matched term *)
        Format.fprintf out_fmter "@ ";
        e ();
        (* Now the pattern function (then case) *)
        Format.fprintf out_fmter "@ (";
        (* Then case 1/2: Abstract over fresh variables *)
        let count = ref 0 in
        List.iter (fun pat ->
                   Format.fprintf out_fmter "pattern_var_%d_ :@ cc.eT@ " !count;
                   generate_simple_type_of_ast dkctx out_fmter pat;
                   Format.fprintf out_fmter " =>@ ";
                   incr count)
                  pats;
        (* Then case 2/2: Generate the matching on the fresh variables *)
        rec_generate_pats_list 0 ~k ~d ~ret_type pats;
        Format.fprintf out_fmter ")";

        (* Now the continuation (else case) *)
        Format.fprintf out_fmter "@ (";
        k ();
        Format.fprintf out_fmter ")";
        Format.fprintf out_fmter "@]";


  and rec_generate_pats_list count ~k ~d ~ret_type = function
    | [] -> d ()
    | pat :: pats ->
       (* we produce the term
          match pattern_var_%{count}_ with
            | pat -> recursive_call
            | _ -> k
        *)
       let fresh_var_name =
         Parsetree.Vlident (Printf.sprintf "pattern_var_%d_" count)
       in
       (* let fresh_var_pat = Parsetree.P_var fresh_var_name in *)
       (* let fresh_var_desc = Parsetree.EI_local fresh_var_name in *)
       (* let fresh_var = Parsetree_utils.make_ast fresh_var_desc in *)
       rec_gen_pat
         pat
         ~d: (fun () ->
                    rec_generate_pats_list (count+1) ~k ~d ~ret_type pats)
         ~k
         ~ret_type
         ~e: (fun () -> Sourcify.pp_vname out_fmter fresh_var_name)

  and generate_pattern_type (p : Parsetree.pattern) =
    generate_simple_type_of_ast dkctx out_fmter p
  in

  (* ********************** *)
  (* Now, let's do the job. *)
  rec_gen_pat pattern ~d ~k ~ret_type ~e
;;



type let_binding_pre_computation = {
  lbpc_value_body : Env.DkGenInformation.value_body ;
  lbpc_params_names : Parsetree.vname list ;
  lbpc_nb_polymorphic_args : int ;
  lbpc_params_with_type : (Parsetree.vname * Types.type_simple option) list ;
  lbpc_result_ty : Types.type_simple option ;
  lbpc_generalized_vars : Types.type_variable list
} ;;



(* ************************************************************************** *)
(** {b Descr}: Initiate computation of things needed by [let_binding_compile]
    and also needed to pre-enter recursive identifiers in the Dk env in order
    to know their number of extra arguments due to polymorphism. In effect, in
    case of recursivity (and moreover mutual recursivity), this info is needed
    in order to apply recursive identifiers in recursive call to the right
    number of arguments.
    Since the determination of this number of extra arguments involves
    computation of other things useful for effective code generation, we
    factorize these computation here and make these results available to
    [let_binding_compile] to avoid computing them again.
    It directly returns an environment in which the identifier is bound to the
    correct information. In effect, code generation (occuring after the
    present function is called) doesn't modify this information.              *)
(* ************************************************************************** *)
let pre_compute_let_binding_info_for_rec env bd ~rec_status ~toplevel =
  (* Generate the parameters if some, with their type constraints. *)
  let params_names = List.map fst bd.Parsetree.ast_desc.Parsetree.b_params in
  (* Recover the type scheme of the bound ident. *)
  let def_scheme =
    (match bd.Parsetree.ast_type with
     | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
     | Parsetree.ANTI_type _ -> assert false
     | Parsetree.ANTI_scheme s -> s) in
  (* We do not have anymore information about "Self"'s structure... *)
  let (params_with_type, result_ty, generalized_vars) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest: None (Some def_scheme) params_names in
  (* Record NOW in the environment the number of extra arguments due to
     polymorphism the current bound ident has in case of recursive definition.
     Otherwise, it will only be done later. *)
  let nb_polymorphic_args = List.length generalized_vars in
  let value_body =
    if not toplevel then Env.DkGenInformation.VB_non_toplevel
    else
      Env.DkGenInformation.VB_toplevel_let_bound
        (rec_status, params_names, def_scheme,
         bd.Parsetree.ast_desc.Parsetree.b_body) in
  let env' =
    (match rec_status with
    | Env.DkGenInformation.RC_rec _ ->
        let toplevel_loc =
          if toplevel then Some bd.Parsetree.ast_loc else None in
        Env.DkGenEnv.add_value
          ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
          (nb_polymorphic_args, value_body) env
    | Env.DkGenInformation.RC_non_rec -> env) in
  (env',
   { lbpc_value_body = value_body ;
     lbpc_params_names = params_names ;
     lbpc_params_with_type = params_with_type ;
     lbpc_nb_polymorphic_args = nb_polymorphic_args ;
     lbpc_result_ty = result_ty ;
     lbpc_generalized_vars = generalized_vars })
;;



(* ************************************************************************** *)
(** {b Descr}: Simply folds the pre-computation of one binding on a list of
    bindings (not forcely mutually recursive), accumulating the obtained
    environment at each step.                                                 *)
(* ************************************************************************** *)
let pre_compute_let_bindings_infos_for_rec ~rec_status ~toplevel env bindings =
  (* And not [List.fold_right otherwise the bindings will be processed in
     reverse order. However, the list of infos needs be reversed to
     keep them in the same order than the list of bindings (was bug #60). *)
  let (new_env, reved_infos) =
    List.fold_left
      (fun (env_accu, infos_accu) binding ->
        let (env', info) =
          pre_compute_let_binding_info_for_rec
            ~rec_status ~toplevel env_accu binding in
        (env', info :: infos_accu))
      (env, [])
      bindings in
  (new_env, (List.rev reved_infos))
;;


let print_ident out i =
  match i.Parsetree.ast_desc with
  | Parsetree.I_local vname
  | Parsetree.I_global (Parsetree.Vname vname) ->
     Parsetree_utils.pp_vname_with_operators_expanded out vname
  (* Hack for unit *)
  | Parsetree.I_global (Parsetree.Qualified ("basics", Parsetree.Vuident "()")) ->
     Format.fprintf out "dk_builtins.tt"
  | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
     Format.fprintf out "%s.%a"
       fname
       Parsetree_utils.pp_vname_with_operators_expanded vname
;;

let print_constr_ident out i =
  let Parsetree.CI id = i.Parsetree.ast_desc in
  print_ident out id
;;


(* ************************************************************************** *)
(** {b Descr}: Code generation for *one* let binding, recursive of not.
    If the binding is recursive, then whatever the choosen Dk primitive ("fix"
    or "Fixpoint"),
    This function is called by [Main_dk_generation.toplevel_let_def_compile]
    to generate code for toplevel definitions and by [let_in_def_compile] to
    generate code for local definitions.
    This function properly handles termination proofs stated as "structural"
    and inserts the right "{struct xxx}" in the generated code.
    However, in case of recursive function with no termination proof or a
    non-"structural" proof, it considers invariably that the recursion decreases
    on the fisrt argument of the function and dumps a {struct fst arg}.

    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec let_binding_compile ctx ~opt_term_proof
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~rec_status ~toplevel env bd
    pre_computed_bd_info =
  (* Create once for all the flag used to insert the let-bound idents in the
     environment. *)
  let toplevel_loc = if toplevel then Some bd.Parsetree.ast_loc else None in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the binder and the bound name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded
    bd.Parsetree.ast_desc.Parsetree.b_name ;
  (* Build the print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.lbpc_generalized_vars in
  (* If the original scheme is polymorphic, then we must add extra Dk
     parameters of type "Set" for each of the generalized variables. Hence,
     printing the variables used to instanciate the polymorphic ones in front
     of the function, they will appear and moreover they will be "tagged" as
     "seen" in the variable mapping. Hence, when we will print the arguments
     having these variables as type, the same variable name will be used,
     hence establishing the correct link between the type of the variable and
     the type variable of the function argument's type. *)
  List.iter
    (fun var ->
      Format.fprintf out_fmter "@ (%a : cc.uT)"
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  let params_with_type = pre_computed_bd_info.lbpc_params_with_type in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : cc.eT %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Dk_pprint.pp_type_simple_to_dk print_ctx) param_ty
       | None ->
           (* Because we provided a type scheme to the function
              [bind_parameters_to_types_from_type_scheme], MUST get one type
              for each parameter name ! *)
           assert false)
    params_with_type ;
  (* If the definition is a recursive function, then one must exhibit one
     decreasing argument. If a structural termination proof is provided, we
     annotate the function with the related {struct xxx}.
     If there is no parameter, then the binding is not a function and we do
     not need to exhibit any decreasing argument. *)
  (match opt_term_proof with
  | None ->
      (* If there is no termination proof, then we must just worry in the case
         the definition is recursive. *)
      if rec_status <> Env.DkGenInformation.RC_non_rec then (  (* Is rec. *)
        (* The function is not satisfactory since it is recursive and has
           no termination proof. Issue a warning and [Unsure] choose to consider
           it by default as structural on its first argument. *)
        Format.eprintf
          "@[%tWarning:%t@ In@ species@ '%t%a%t'@ method@ '%t%a%t'@ is@ \
           recursive@ but@ has@ no@ termination@ proof.@ It@ is@ assumed@ \
           to@ be@ structural@ on@ its@ first@ argument..@]@."
          Handy.pp_set_bold Handy.pp_reset_effects
          Handy.pp_set_underlined
          Sourcify.pp_qualified_species ctx.Context.scc_current_species
          Handy.pp_reset_effects
          Handy.pp_set_underlined
          Sourcify.pp_vname bd.Parsetree.ast_desc.Parsetree.b_name
          Handy.pp_reset_effects ;
        match params_with_type with
        | (param_vname, _) :: _ ->
            Format.fprintf out_fmter "@ {struct %a}"
              Parsetree_utils.pp_vname_with_operators_expanded param_vname
        | _ -> ()
       )
  | Some term_proof -> (
      (* Take the termination proof into account only if the definition is
         recursive. Otherwise, issue a warning. *)
      if rec_status <> Env.DkGenInformation.RC_non_rec then (  (* Is rec. *)
        match term_proof.Parsetree.ast_desc with
        | Parsetree.TP_structural decr_arg ->
            (* First, ensure that the identifier is really a parameter of this
               function. [Unsure] on my mind, this should have been done
               ealier, may be at scoping. *)
            if not
              (List.exists (fun (n, _) -> n = decr_arg) params_with_type) then
              raise
                (Wrong_decreasing_argument
                   (bd.Parsetree.ast_loc, ctx.Context.scc_current_species,
                    bd.Parsetree.ast_desc.Parsetree.b_name, decr_arg)) ;
            Format.fprintf out_fmter "@ {struct %a}"
              Parsetree_utils.pp_vname_with_operators_expanded decr_arg
        | Parsetree.TP_lexicographic _
        | Parsetree.TP_measure (_, _, _) | Parsetree.TP_order (_, _, _) -> (
            (* Like when there is no given proof and the function is however
               recursive, we choose by default. *)
            Format.eprintf
              "@[%tWarning:%t@ In@ species@ '%t%a%t'@ method@ '%t%a%t'@ is@ \
               recursive@ but@ has@ a@ not@ yet@ supported@ termination@ \
               proof.@ It@ is@ assumed@ to@ be@ structural@ on@ its@ first@ \
               argument..@]@."
              Handy.pp_set_bold Handy.pp_reset_effects
              Handy.pp_set_underlined
              Sourcify.pp_qualified_species ctx.Context.scc_current_species
              Handy.pp_reset_effects
              Handy.pp_set_underlined
              Sourcify.pp_vname bd.Parsetree.ast_desc.Parsetree.b_name
              Handy.pp_reset_effects ;
            match params_with_type with
            | (param_vname, _) :: _ ->
                Format.fprintf out_fmter "@ {struct %a}"
                  Parsetree_utils.pp_vname_with_operators_expanded param_vname
            | _ -> ()
          )
       )
      else (
        (* Definition is not recursive but has a useless termination proof.
           By definition of the syntax, this should never arise. *)
        assert false
        )
     )
  ) ;
  (* Now, print the result type of the "definition". *)
  (match pre_computed_bd_info.lbpc_result_ty with
   | None ->
       (* Because we provided a type scheme to the function
          [bind_parameters_to_types_from_type_scheme], MUST get one type for
          the result value of the "let". *)
       assert false
   | Some t ->
       Format.fprintf out_fmter "@ :@ cc.eT %a"
         (Dk_pprint.pp_type_simple_to_dk print_ctx) t
  ) ;
  (* Output now the ":=" sign ending the Dk function's "header".
     With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " :=@\n" ;
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' = pre_computed_bd_info.lbpc_params_names @ local_idents in
  (* Now, let's generate the bound body. *)
  (match bd.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e ->
      let in_recursive_let_section_of =
        if rec_status <> Env.DkGenInformation.RC_non_rec then  (* Is rec. *)
          bd.Parsetree.ast_desc.Parsetree.b_name ::
          in_recursive_let_section_of
        else in_recursive_let_section_of in
      generate_expr
        ctx ~in_recursive_let_section_of ~local_idents: local_idents'
        ~self_methods_status ~recursive_methods_status env e
  | Parsetree.BB_logical _ -> assert false) ;
  (* Finally, we record, (except if it was already done in [env'] in case of
     recursive binding) the number of extra arguments due to polymorphism the
     current bound identifier has. *)
  if rec_status <> Env.DkGenInformation.RC_non_rec then env  (* Is rec. *)
  else
    Env.DkGenEnv.add_value
      ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
      (pre_computed_bd_info.lbpc_nb_polymorphic_args,
       pre_computed_bd_info.lbpc_value_body) env



and generate_expr ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status initial_env
    initial_expression =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the dk type print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in

  let rec rec_generate_expr loc_idents env expression =
    (* Now, dissecate the expression core. *)
    match expression.Parsetree.ast_desc with
     | Parsetree.E_self ->
         (* "Self" is not a first-class value ! *)
         assert false
     | Parsetree.E_const cst -> generate_constant ctx cst
     | Parsetree.E_fun (vnames, body) ->
         (* Get the type of the function. *)
         let fun_ty =
           (match expression.Parsetree.ast_type with
            | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
            | Parsetree.ANTI_scheme _ -> assert false
            | Parsetree.ANTI_type t -> t) in
         Format.fprintf out_fmter "@[<2>(fun " ;
         (* Now, print each parameter with it's type until we arrive to the
            return type of the function. DO NOT fold_right ! *)
         ignore
           (List.fold_left
              (fun accu_ty arg_name ->
                (* We do not have anymore information about "Self"'s
                   structure... *)
                let arg_ty =
                  Types.extract_fun_ty_arg ~self_manifest: None accu_ty in
                let res_ty =
                  Types.extract_fun_ty_result ~self_manifest: None accu_ty in
                Format.fprintf out_fmter "(%a :@ cc.eT %a)@ "
                  Parsetree_utils.pp_vname_with_operators_expanded arg_name
                  (Dk_pprint.pp_type_simple_to_dk print_ctx) arg_ty ;
                (* Return the remainder of the type to continue. *)
                res_ty)
              fun_ty
              vnames) ;
         Format.fprintf out_fmter "=>@ " ;
         rec_generate_expr loc_idents env body ;
         Format.fprintf out_fmter ")@]" ;
     | Parsetree.E_var ident -> (
       let current_species_name =
         Some
           (Parsetree_utils.name_of_vname
              (snd ctx.Context.scc_current_species)) in
       let id_type_simple : Types.type_simple =
         (match expression.Parsetree.ast_type with
          | Parsetree.ANTI_none | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_scheme _ -> assert false
          | Parsetree.ANTI_type t -> t) in
       let (nb_polymorphic_args, id_type_scheme) =
         try
           let (nb, vb) =
             (Env.DkGenEnv.find_value
                ~loc: ident.Parsetree.ast_loc
                ~current_unit: ctx.Context.scc_current_unit
                ~current_species_name ident env) in
           (nb, match vb with
             | Env.DkGenInformation.VB_toplevel_let_bound (_, _, ts, _) -> Some ts
             | Env.DkGenInformation.VB_non_toplevel
             | Env.DkGenInformation.VB_toplevel_property _ -> None
           )
         with
           (* If the identifier was not found, then it was may be a local
                identifier bound by a pattern. Then we can safely ignore it. *)
           Env.Unbound_identifier (_, _) -> (0, None)
       in
       assert (nb_polymorphic_args =
         match id_type_scheme with
         | Some ts ->
            let (l, _) = Types.scheme_split ts in List.length l
         | None -> 0
              );

       (* If some extra "_" are needed, then enclose the whole expression
            between parens (was bug #50). *)
         if nb_polymorphic_args > 0 then Format.fprintf out_fmter "@[<2>(" ;
         generate_expr_ident_for_E_var
           ctx ~in_recursive_let_section_of ~local_idents: loc_idents
           ~self_methods_status ~recursive_methods_status ident ;
         (* Now, add the extra type parameters if the identifier is polymorphic. *)
         if nb_polymorphic_args > 0 then (
           let type_scheme =
             match id_type_scheme with Some ts -> ts | None -> assert false
           in
           let type_arguments =
             Types.unify_with_instance
               type_scheme
               id_type_simple
           in
           assert (List.length type_arguments = nb_polymorphic_args);
           List.iter (fun st ->
                      Format.fprintf out_fmter "@ (%a)"
                                     (Dk_pprint.pp_type_simple_to_dk print_ctx) st
                     )
                     type_arguments;
           (* Close the opened parenthesis if one was opened. *)
           if nb_polymorphic_args > 0 then Format.fprintf out_fmter ")@]"
         )
     )
     | Parsetree.E_app (func_expr, args) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate_expr loc_idents env func_expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list ~comma: false loc_idents env args ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_ident, args) ->
         Format.fprintf out_fmter "@[<1>(%a"
           print_constr_ident cstr_ident;
         let extras =
           generate_constructor_ident_for_method_generator ctx env cstr_ident
         in
         (* Add the type arguments of the constructor. *)
         begin match expression.Parsetree.ast_type with
         | Parsetree.ANTI_type t ->
             Dk_pprint.pp_type_simple_args_to_dk print_ctx out_fmter t extras
         | _ -> assert false
         end;
         begin match args with
          | [] -> ()
          | _ ->
              Format.fprintf out_fmter "@ " ;
              rec_generate_exprs_list ~comma: false loc_idents env args ;
         end;
         Format.fprintf out_fmter ")@]" ;
     | Parsetree.E_match (expr, pats_exprs) ->
        let rec generate_pattern_matching = function
          | [] ->
             Format.fprintf out_fmter "(dk_fail.fail@ ";
             generate_simple_type_of_ast print_ctx out_fmter expression;
             Format.fprintf out_fmter ")"
          | (pat, d) :: pats ->
             generate_pattern ctx print_ctx env pat
                              ~d: (fun () -> rec_generate_expr loc_idents env d)
                              ~ret_type: d
                              ~k: (fun () -> generate_pattern_matching pats)
                              ~e: (fun () -> rec_generate_expr loc_idents env expr)
        in
        generate_pattern_matching pats_exprs;
     (*  *)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>(dk_bool.ite@ " ;
         generate_simple_type_of_ast print_ctx out_fmter expr2;
         Format.fprintf out_fmter "@ " ;
         rec_generate_expr loc_idents env expr1 ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_expr loc_idents env expr2 ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_expr loc_idents env expr3 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_let (let_def, in_expr) ->
        let (env, pre_comp_infos) =
          pre_compute_let_bindings_infos_for_rec
            ~rec_status: Env.DkGenInformation.RC_non_rec
            ~toplevel: false env
            let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
        let rec aux out_fmter = function
         | ([], []) -> rec_generate_expr loc_idents env in_expr
         | (fst_bnd :: next_bnds,
            fst_pre_comp_info :: next_pre_comp_info) ->
            (* Simply translate by a beta redex,
               this only works for non-recursive local lets *)
            Format.fprintf out_fmter "@[<2>((%a : cc.eT %a =>@ %a)@ %a)@]"
              Parsetree_utils.pp_vname_with_operators_expanded
              fst_bnd.Parsetree.ast_desc.Parsetree.b_name
              (Dk_pprint.pp_type_simple_to_dk print_ctx)
              (match fst_pre_comp_info.lbpc_result_ty with
               | None -> assert false
               | Some t -> t)
              aux (next_bnds, next_pre_comp_info)
              (fun _ -> rec_generate_expr loc_idents env)
              (match fst_bnd.Parsetree.ast_desc.Parsetree.b_body with
               | Parsetree.BB_logical _ -> assert false
               | Parsetree.BB_computational e -> e)
         | _ -> assert false
        in
        aux
          out_fmter
          (let_def.Parsetree.ast_desc.Parsetree.ld_bindings,
           pre_comp_infos)
     | Parsetree.E_record labs_exprs ->
         (* Use the Dk syntax {| .. := .. ; .. := .. |}. *)
         Format.fprintf out_fmter "@[<1>{|@ " ;
         rec_generate_record_field_exprs_list env loc_idents labs_exprs ;
         Format.fprintf out_fmter "@ |}@]"
     | Parsetree.E_record_access (expr, label) -> (
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter ".@[<2>(" ;
         let extras =
           generate_record_label_for_method_generator ctx env label in
         (* Add the type arguments of the ***record type***, not the full
            expression type. *)
         (match expr.Parsetree.ast_type with
         | Parsetree.ANTI_type t ->
             Dk_pprint.pp_type_simple_args_to_dk print_ctx out_fmter t extras
         | _ -> assert false) ;
         Format.fprintf out_fmter ")@]"
        )
     | Parsetree.E_record_with (_expr, _labels_exprs) ->
         Format.fprintf out_fmter "E_record_with"
     | Parsetree.E_tuple exprs -> (
         match exprs with
          | [] -> assert false
          | [one] -> rec_generate_expr loc_idents env one
          | _ ->
              Format.fprintf out_fmter "@[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents env exprs ;
              Format.fprintf out_fmter ")@]"
        )
     | Parsetree.E_sequence exprs ->
         let rec loop ppf = function
          | [] -> ()
          | [one] -> rec_generate_expr loc_idents env one
          | _ :: exprs -> loop ppf exprs in
         Format.fprintf out_fmter "@[<1>(%a)@]" loop exprs
     | Parsetree.E_external external_expr ->
         (begin
         let e_translation =
           external_expr.Parsetree.ast_desc.Parsetree.ee_external in
         try
           (* Simply a somewhat verbatim output of the Dk translation. *)
           let (_, dk_code) =
             List.find
               (function
                 | (Parsetree.EL_Dk, _) -> true
                 | (Parsetree.EL_Caml, _)
                 | (Parsetree.EL_Coq, _)
                 | ((Parsetree.EL_external _), _) -> false)
               e_translation.Parsetree.ast_desc in
           Format.fprintf out_fmter "%s" dk_code
         with Not_found ->
           (* No Dk mapping found. *)
           raise
             (Externals_generation_errs.No_external_value_def
                ("Dk", (Parsetree.Vlident "<expr>"),
                 expression.Parsetree.ast_loc))
         end)
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter ")@]"



  (* [Same] Quasi same code than for OCaml. *)
  and rec_generate_record_field_exprs_list env loc_idents = function
    | [] -> ()
    | [(label, last)] ->
        (* In record expression, no need to print extra _ even if the record
           type is polymorphic. *)
        ignore (generate_record_label_for_method_generator ctx env label) ;
        Format.fprintf out_fmter " :=@ " ;
        rec_generate_expr loc_idents env last
    | (h_label, h_expr) :: q ->
        ignore (generate_record_label_for_method_generator ctx env h_label) ;
        Format.fprintf out_fmter " :=@ " ;
        rec_generate_expr loc_idents env h_expr ;
        Format.fprintf out_fmter ";@ " ;
        rec_generate_record_field_exprs_list env loc_idents q


  and generate_expression_type (e : Parsetree.expr) =
    match e.Parsetree.ast_type with
    | Parsetree.ANTI_none
    | Parsetree.ANTI_irrelevant
    | Parsetree.ANTI_scheme _ ->
       assert false     (* An expression should have a meaningful type *)
    | Parsetree.ANTI_type st ->
       Dk_pprint.pp_type_simple_to_dk print_ctx out_fmter st

  and rec_generate_exprs_type_list ~comma loc_idents env = function
    | [] -> ()
    | [last] -> generate_expression_type last
    | h :: q ->
       if comma then Format.fprintf out_fmter "dk_tuple.prod@ ";
       generate_expression_type h;
       Format.fprintf out_fmter "@ ";
       rec_generate_exprs_type_list ~comma loc_idents env q

  and rec_generate_exprs_list ~comma loc_idents env = function
    | [] -> ()
    | [last] -> rec_generate_expr loc_idents env last
    | h :: q ->
       if comma then            (* There is no builtin syntax for pairs in Dedukti, we use a pair function instead. *)
         (Format.fprintf out_fmter "dk_tuple.pair@ ";
          generate_expression_type h;
          Format.fprintf out_fmter "@ ";
          rec_generate_exprs_type_list ~comma loc_idents env q;
          Format.fprintf out_fmter "@ ";
         );
        rec_generate_expr loc_idents env h ;
        Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list ~comma loc_idents env q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate_expr local_idents initial_env initial_expression
;;



let generate_logical_expr ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status initial_env
    initial_proposition =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the dk type print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in

  let rec rec_generate_logical_expr loc_idents env proposition =
    match proposition.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, logical_expr)
     | Parsetree.Pr_exists (vnames, ty_expr, logical_expr) ->
         (begin
         (* Recover the *scheme* annotating the type expression. *)
         let scheme =
           (match ty_expr.Parsetree.ast_type with
            | Parsetree.ANTI_none
            | Parsetree.ANTI_irrelevant
            | Parsetree.ANTI_type _ -> assert false
            | Parsetree.ANTI_scheme s -> s) in
         let (generalized_vars, ty) = Types.scheme_split scheme in
         (* The header... *)
         Format.fprintf out_fmter "@[<2>";
         (* Now, print the polymorphic extra args. We use the same trick than
            in [let_binding_compile]. Consult comment over there... *)
         List.iter
           (fun var ->
             Format.fprintf out_fmter "dk_logic.forall_type (%a : cc.uT => @ "
               Dk_pprint.pp_type_variable_to_dk var)
           generalized_vars ;
         (* Now, print the binder and the real bound variables. *)
         (match proposition.Parsetree.ast_desc with
          | Parsetree.Pr_forall (_, _, _) ->
              List.iter
                (fun vn ->
                 Format.fprintf out_fmter
                                "dk_logic.forall (%a) (%s :@ cc.eT (%a)@ =>@ "
                                (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                                (Parsetree_utils.vname_as_string_with_operators_expanded
                                   vn)
                                (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
                vnames
          | Parsetree.Pr_exists (_, _, _) ->
              List.iter
                (fun vn ->
                 Format.fprintf out_fmter
                                "dk_logic.exists (%a) (%s :@ cc.eT (%a)@ =>@ "
                                (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                                (Parsetree_utils.vname_as_string_with_operators_expanded
                                   vn)
                                (Dk_pprint.pp_type_simple_to_dk print_ctx) ty)
                vnames
          | _ -> assert false) ;
         (* Here, the bound variables name may mask a "in"-parameter. *)
         let loc_idents' = vnames @ loc_idents in
         (* Add the bound variable in the environment. ATTENTION: inside the
            logical expression, the bound variables ARE NOT polymorphic (no
            mu-rule). Hence we insert them with 0. *)
         let env' =
           List.fold_left
             (fun accu_env vname ->
               Env.DkGenEnv.add_value
                 ~toplevel: None vname
                 (0, Env.DkGenInformation.VB_non_toplevel) accu_env)
             env
             vnames in
         rec_generate_logical_expr loc_idents' env' logical_expr ;
         (* Close the parens opened for binders *)
         List.iter
           (fun _ -> Format.fprintf out_fmter ")")
           vnames ;
         List.iter
           (fun _ -> Format.fprintf out_fmter ")")
           generalized_vars;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.Pr_imply (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>dk_logic.imp@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter ")@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_or (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>dk_logic.or@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter ")@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_and (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>dk_logic.and@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter ")@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>dk_logic.eqv@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter ")@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_not logical_expr ->
         Format.fprintf out_fmter "@[<2>" ;
         Format.fprintf out_fmter "dk_logic.not@ (" ;
         rec_generate_logical_expr loc_idents env logical_expr ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_expr expr ->
         (* The wrapper surrounding the expression by Dk's "ebP" if the
            expression's type is [bool].
            Bug #45 exhibited that the type here may also be Self in case
            the representation was bool. Because logical propositions
            are expected to be well-typed at this point, if the type is Self,
            then for sure representation was bool. It could also have been
            Prop ... but not because Prop is not a legal type directly usable
            by the user. So, if the type is Self, we also wrap. *)
         let is_bool =
           (match expr.Parsetree.ast_type with
            | Parsetree.ANTI_type ty -> Types.is_bool_or_self_type ty
            | Parsetree.ANTI_none
            | Parsetree.ANTI_irrelevant
            | Parsetree.ANTI_scheme _ ->
                (* Note that expression never has a type scheme, but only a
                   type. *)
                assert false) in
         if is_bool then Format.fprintf out_fmter "@[<2>dk_logic.ebP (" ;
         generate_expr
           ctx ~in_recursive_let_section_of ~local_idents: loc_idents
           ~self_methods_status ~recursive_methods_status env expr ;
         (* The end of the wrapper surrounding the expression if it has type
            bool. *)
         if is_bool then Format.fprintf out_fmter ")@]"
     | Parsetree.Pr_paren logical_expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_logical_expr loc_idents env logical_expr ;
         Format.fprintf out_fmter ")@]" in

  (* ************************************************ *)
  (* Now, let's really do the job of [generate_logical_expr]. *)
  rec_generate_logical_expr local_idents initial_env initial_proposition
;;

(* ************************************************************************* *)
(* Env.DkGenEnv.t ->
   Abstractions.field_abstraction_info list ->
   (Parsetree.vname * Env.ordered_methods_from_params) list                  *)
(** {b Descr}: Generate the Dk code of a species parameters. It outputs
    both the parameters names and their type as a Dk expression.
    Either the parameter is a "is" parameter and then it's type will be
    rebuilt from its species expression.
    Or it is a "in" parameter and then it is a parameter of the record
    only if its type is built from another of our species parameters (i.e.
    not from a toplevel species/collection).
    Next come the extra parameters coming from the methods we depend on.
    Returns the list of species params and methods required to create a value
    of the type record, i.e. the one we found dependencies on in the body of
    the record type ordered the same way they were lambda-lifted.

    Used when generating the record type definition.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type_parameters ctx env species_descr
    fields_abstraction_infos =
  let ppf = ctx.Context.scc_out_fmter in
  let current_unit = ctx.Context.scc_current_unit in
  (* We first abstract the species/entity parameters carriers and entity
     parameters. *)
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      match param_kind with
       | Types.CCMI_is ->
           Format.fprintf ppf "@[<1>(%s_T :@ cc.uT)@ @]" param_name
       | Types.CCMI_in provenance ->
           (* We generate the lambda-lifting for "IN" parameter here (not their
              carrier, since if needed it has mandatorily be generated as a
              species parameter carrier during this current process).
              When we will inspect the methods to abstract their dependencies on
              species parameters, we the will skip "IN" parameters (that
              trivially lead to a dependency on a pseudo method wearing their
              name) to avoid doubles. *)
           match provenance with
            | Types.SCK_species_parameter ->
                Format.fprintf ppf "@[<1>(_p_%s_%s :@ cc.eT %s_T)@ @]"
                               param_name param_name param_ty_coll
            | Types.SCK_toplevel_collection
            | Types.SCK_toplevel_species ->
                Format.fprintf ppf "@[<1>(_p_%s_%s :@ cc.eT "
                                  param_name param_name ;
                   if param_ty_mod <> current_unit then
                     Format.fprintf ppf "%s." param_ty_mod ;
                   Format.fprintf ppf "%s__me_as_carrier)@ @]" param_ty_coll
               )
    ctx.Context.scc_collections_carrier_mapping ;
  (* Now, we will find the methods of the species parameters we decl-depend on
     in the Dk type expressions. Such dependencies can only appear through
     properties and theorems bodies. *)
  let species_parameters_names = ctx.Context.scc_species_parameters_names in
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
      Dk_pprint.dpc_collections_carrier_mapping =
        ctx.Context.scc_collections_carrier_mapping } in
  (* We first build the lists of dependent methods for each property and
     theorem fields. *)
  let tmp_deps_for_fields =
    Abstrs.make_empty_param_deps species_parameters_names in
  let deps_for_fields =
    List.map (fun (a, b) -> (a, ref b)) tmp_deps_for_fields in
  List.iter
    (function
      | Env.TypeInformation.SF_sig _
      | Env.TypeInformation.SF_let _
      | Env.TypeInformation.SF_let_rec _ -> ()
      | Env.TypeInformation.SF_theorem (_, name, _, _, _, _)
      | Env.TypeInformation.SF_property (_, name, _, _, _) ->
          let ai = List.assoc name fields_abstraction_infos in
          (* Recover the dependencies on parameters from the abstractions
             but only taking care of dependencies induced by [TYPE] and
             [COMPLETIONS]. *)
          List.iter
            (fun (species_param, (Env.ODFP_methods_list deps_on_meths)) ->
              match species_param with
               | Env.TypeInformation.SPAR_in (_, _, _) ->
                   ()   (* Skip to avoid double (c.f. comment above). *)
               | Env.TypeInformation.SPAR_is
                     ((_, spe_param_vname), _, _, _, _) ->
                   let spe_param_name = Parsetree.Vuident spe_param_vname in
                   (* Merge the found dependencies by side effect. *)
                   try
                     let param_bucket =
                       Handy.list_assoc_custom_eq
                         (fun sp n ->
                           (Env.TypeInformation.vname_of_species_param sp) = n)
                         spe_param_name deps_for_fields in
                     (* Make the union of all the elements of the list and
                        the already found methods for this parameter. *)
                     param_bucket :=
                       List.fold_left
                         (fun accu d ->
                           Parsetree_utils.ParamDepSet.add d accu)
                           !param_bucket deps_on_meths
                   with Not_found -> assert false)
            ai.Env.TypeInformation.ad_dependencies_from_parameters_in_type)
    species_descr.Env.TypeInformation.spe_sig_methods ;
  (* Just remove the references inside the assoc list. *)
  let deps_for_fields_no_ref =
    List.map (fun (a, b) -> (a, !b)) deps_for_fields in
  (* We now sort these methods according to the parameters' dependency graph. *)
  let ordered_deps_for_fields =
    Dep_analysis.order_species_params_methods deps_for_fields_no_ref in
  (* By the way, returns the list methods per species params required to
     create a value of the type record. *)
  List.map
    (fun (species_param, (Env.ODFP_methods_list meths)) ->
      let species_param_name =
        Env.TypeInformation.vname_of_species_param species_param in
      (* Each abstracted method will be named like "_p_", followed by the
         species parameter name, followed by "_", followed by the method's
         name. *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
        "_" in
      List.iter
        (fun (meth, meth_ty_kind) ->
          let llift_name =
            prefix ^
            (Parsetree_utils.vname_as_string_with_operators_expanded meth) in
          match meth_ty_kind with
           | Parsetree_utils.DETK_computational ty ->
               Format.fprintf ppf "(%s : cc.eT %a)@ "
                              llift_name
                              (Dk_pprint.pp_type_simple_to_dk print_ctx)
                              ty
           | Parsetree_utils.DETK_logical lexpr ->
               Format.fprintf ppf "(%s : cc.eP " llift_name ;
               generate_logical_expr
                 ctx
                 ~in_recursive_let_section_of: [] ~local_idents: []
                 ~self_methods_status: (SMS_from_param species_param_name)
                 (* Anyway, in the record type, bodies of recursive are
                    never expanded. Hence this choice or another for
                    [~recursive_methods_status] is not important.
                    It could be if we allowed recursive logical methods. *)
                                        ~recursive_methods_status: RMS_regular env lexpr ;
                  Format.fprintf ppf ")@ ")
        meths ;
      (* Just to avoid having the reference escaping... *)
      (species_param_name, (Env.ODFP_methods_list meths)))
    ordered_deps_for_fields
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_description ->      *)
(*   (Parsetree.vname * Parsetree.vname) list                                *)
(** {b Descr} : Generate the record type representing a species. This type
    contains one field per method. This type is always named "me_as_species".
    Returns the list of species params with for each, the list of methods
    required to create a value of the type record, i.e. the ones we found
    dependencies on in the body of the record type definition.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx env species_descr field_abstraction_infos =
  let out_fmter = ctx.Context.scc_out_fmter in
  let collections_carrier_mapping =
    ctx.Context.scc_collections_carrier_mapping in
  let (_, species_name) = ctx.Context.scc_current_species in
  (* The header of the Dk record definition for the species. *)
  Format.fprintf out_fmter "@[<2>Record %a__me_as_species "
    Sourcify.pp_vname species_name;
  (* We do not add any bindings to the [collections_carrier_mapping]
     before printing the record type parameters for 2 reasons:
       - species parameters carriers of the record are in the
         [collections_carrier_mapping], hence any added binding would make
         think to an extra species parameter, hence to an extra parameter to
         the record (obviously wrong).
       - since species carriers are not recursive, there is no reason
         to have "Self" parametrizing its own record type.
     Generate the record parameters mapping the species parameters and the
     methods from them we depend on ! *)
  let abstracted_params_methods_in_record_type =
    generate_record_type_parameters
      ctx env species_descr field_abstraction_infos in
  let (_, species_name) = ctx.Context.scc_current_species in
  (* Print the constructor. *)
  Format.fprintf out_fmter " := %a__mk_record {@\n"
      Sourcify.pp_vname species_name;
  (* Always generate the "rep". *)
  Format.fprintf out_fmter "@[<2>%a__rf_T :@ cc.uT"
    Sourcify.pp_vname species_name;
  (* We now extend the collections_carrier_mapping with ourselve known.
     Hence, if we refer to our "rep" we will be directly mapped onto the
     "rf_T" without needing to re-construct this name each time. Do same
     thing for "Self". *)
  let (my_fname, my_species_name) = ctx.Context.scc_current_species in
  let my_species_name = Parsetree_utils.name_of_vname my_species_name in
  let collections_carrier_mapping =
    (make_Self_cc_binding_rf_T
      ~current_species: ctx.Context.scc_current_species) ::
    ((my_fname, my_species_name),(("rf"), Types.CCMI_is)) ::
    collections_carrier_mapping in
  (* We mask the old ctx to take benefit of the new one with the bindings. *)
  let ctx = {
    ctx with
    Context.scc_collections_carrier_mapping = collections_carrier_mapping } in
  (* Create the dk type print context with the context new bindings. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping = collections_carrier_mapping } in
  (* Put a trailing semi only if there are other fields to generate. *)
  (match species_descr.Env.TypeInformation.spe_sig_methods with
   | [] -> ()
   | [Env.TypeInformation.SF_sig (_, n, _)] ->
       if (Parsetree_utils.name_of_vname n) = "rep" then
         ()   (* Case where there was only 1 field and that field was "rep". *)
       else Format.fprintf out_fmter ","
   | _ -> Format.fprintf out_fmter ",") ;
  Format.fprintf out_fmter "@]@\n" ;
  (* We must now generate the record's fields types. *)
  let output_one_field ~semi = function
    | Env.TypeInformation.SF_sig (from, n, sch)
    | Env.TypeInformation.SF_let (from, n, _, sch, _, _, _, _) ->
        (begin
        (* Skip "rep", because it is processed just above. *)
        if (Parsetree_utils.name_of_vname n) <> "rep" then
          (begin
          let ty = Types.specialize sch in
          Format.fprintf out_fmter "(; From species %a. ;)@\n"
            Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
          (* Field is prefixed by the species name for sake of unicity. *)
          Format.fprintf out_fmter "@[<2>%a__rf_%a :@ cc.eT (%a)"
            Sourcify.pp_vname species_name
            Parsetree_utils.pp_vname_with_operators_expanded n
            (Dk_pprint.pp_type_simple_to_dk print_ctx) ty ;
          if semi then Format.fprintf out_fmter "," ;
          Format.fprintf out_fmter "@]@\n"
          end)
        end)
    | Env.TypeInformation.SF_let_rec l ->
        List.iter
          (fun (from, n, _, sch, _, _, _, _) ->
            let ty = Types.specialize sch in
            Format.fprintf out_fmter "(; From species %a. ;)@\n"
              Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
            (* Field is prefixed by the species name for sake of unicity. *)
            Format.fprintf out_fmter "@[<2>%a__rf_%a : cc.eT (%a)"
              Sourcify.pp_vname species_name
              Parsetree_utils.pp_vname_with_operators_expanded n
              (Dk_pprint.pp_type_simple_to_dk print_ctx) ty ;
            if semi then Format.fprintf out_fmter "," ;
            Format.fprintf out_fmter "@]@\n")
          l
    | Env.TypeInformation.SF_theorem
        (from, n, _polymorphic_vars_map, logical_expr, _, _)
    | Env.TypeInformation.SF_property
        (from, n, _polymorphic_vars_map, logical_expr, _) ->
        (* In the record type, theorems and properties are displayed in same
           way. *)
        Format.fprintf out_fmter "(; From species %a. ;)@\n"
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
        (* Field is prefixed by the species name for sake of unicity. *)
        Format.fprintf out_fmter "@[<2>%a__rf_%a :@ dk_logic.eP ("
          Sourcify.pp_vname species_name
          Parsetree_utils.pp_vname_with_operators_expanded n;
        (* Generate the Dk code representing the proposition.
           No local idents in the context because we just enter the scope of a
           species fields and so we are not under a core expression.
           In the record type, methods of "Self" are always named using
           "rf_" + the method name; hence print using [~self_methods_status]
           to [SMS_from_record]. *)
        generate_logical_expr ctx
          ~in_recursive_let_section_of: [] ~local_idents: []
          ~self_methods_status: SMS_from_record
          (* Anyway, in the record type, bodies of recursive are never
             expanded. Hence this choice or another for
             [~recursive_methods_status] is not important.
             It could be if we allowed recursive logical methods. *)
          ~recursive_methods_status: RMS_regular env logical_expr ;
        Format.fprintf out_fmter ")";
        if semi then Format.fprintf out_fmter "," ;
        Format.fprintf out_fmter "@]@\n" in
  (* Dk syntax required not semi after the last field. That's why a simple
     [List.iter] of [output_one_field]'s body doesn't work.
     One must separate the case of the last element of the list. *)
  let rec iter_semi_separated = function
    | [] -> ()
    | [last] -> output_one_field ~semi:false last
    | h :: q ->
        output_one_field ~semi:true h ;
        iter_semi_separated q in
  iter_semi_separated species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}.@\n@\n" ;
  abstracted_params_methods_in_record_type
;;
