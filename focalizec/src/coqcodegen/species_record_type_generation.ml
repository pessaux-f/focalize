(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*                               LIP6  --  INRIA Rocquencourt                 *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(* $Id: species_record_type_generation.ml,v 1.100 2012-10-30 11:55:07 pessaux Exp $ *)


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
  if not only_for_Self_meths then
    (begin
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
    end) ;
  (* And finally, apply to the methods from ourselves we depend on. *)
  List.iter
    (fun n ->
      if n = Parsetree.Vlident "rep" then Format.fprintf out_fmter "@ abst_T"
      else
        Format.fprintf out_fmter "@ abst_%a"
          Parsetree_utils.pp_vname_with_operators_expanded n)
    abstracted_methods
;;



(** Just helpers to make a binding for self for a [collection_carrier_mapping]
    in order to make known how Self must be printed while printing **TYPES**
    The "_T" will be added automatically by the printing routine. We add the
    species as a [CCMI_is] to have it printed as "xxx_T" and not as an entity
    parameter. *)
let make_Self_cc_binding_abst_T  ~current_species =
  let (module_name, _) = current_species in
  ((module_name, "Self"), ("abst" ,Types.CCMI_is))
;;
let make_Self_cc_binding_rf_T ~current_species =
  let (module_name, _) = current_species in
  ((module_name, "Self"), ("rf", Types.CCMI_is))
;;
let make_Self_cc_binding_species_param ~current_species spe_param_name =
  let (module_name, _) = current_species in
  ((module_name, "Self"),
   (("_p_" ^ (Parsetree_utils.name_of_vname spe_param_name)), Types.CCMI_is))
;;



(* ************************************************************************ *)
(* current_unit: Parsetree.modname -> Format.formatter ->                   *)
(*   Parsetree.qualified_vname -> unit                                      *)
(** {b Descr}: Pretty prints a [Parsetree.qualified_vname] as a Coq regular
    identifier. If the identifier has a qualification that is different of
    the current compilation unit, then we use the dot-notation.
    Otherwise no qualification is printed.
    No transformation is performed on the ident (no abstraction stuff, no
    change, no prefix) : the name is directly generated as it is.
    If the ident , in fact, has no qualification, then the scoping process
    may have failed earlier because any qualified name must have and
    explicit qualification after the scoping pass.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************ *)
let simply_pp_to_coq_qualified_vname ~current_unit ppf = function
  | Parsetree.Vname _ ->
      (* In this case, may be there is some scoping process missing. *)
      assert false
  | Parsetree.Qualified (modname, vname) ->
      if modname <> current_unit then
        Format.fprintf ppf "%s." modname ;
      Parsetree_utils.pp_vname_with_operators_expanded ppf vname
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
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       (begin
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
         (not (List.mem vname local_idents)) then
         (begin
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
         end)
       else
         (begin
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
         if not is_a_rec_fun then
           (begin
            try
            (* We are not in a recursive definition, so we can apply to the
               lambda-lifted extra arguments. *)
            let extra_args =
               List.assoc vname ctx.Context.scc_lambda_lift_params_mapping in
             List.iter (fun s -> Format.fprintf out_fmter "@ %s" s) extra_args
              with Not_found -> ()
             end)
         end)
       end)
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
   | Parsetree.EI_global (Parsetree.Qualified (mod_name, vname)) ->
       (* Call the Coq corresponding identifier in the corresponding   *)
       (* module (i.e. the [mod_name]). If the module is the currently *)
       (* compiled one, then do not qualify the identifier.            *)
       if mod_name <> ctx.Context.scc_current_unit then
         Format.fprintf out_fmter "%s.%a"
           mod_name Parsetree_utils.pp_vname_with_operators_expanded vname
       else
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. *)
            match self_methods_status with
             | SMS_abstracted ->
                 Format.fprintf out_fmter "abst_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
             | SMS_from_record ->
                 Format.fprintf out_fmter "rf_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
             | SMS_from_param spe_param_name ->
                 Format.fprintf out_fmter "_p_%a_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded
                   spe_param_name
                   Parsetree_utils.pp_vname_with_operators_expanded vname
            end)
        | Some coll_specifier ->
            (begin
            match coll_specifier with
             | Parsetree.Vname coll_name ->
                 (begin
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
                     ctx.Context.scc_species_parameters_names then
                   (begin
                   (* It comes from a parameter. To retrieve the related
                      method name we build it the same way we built it
                      while generating the extra Coq function's parameters due
                      to depdencencies coming from the species parameter.
                      I.e: "_p_", followed by the species parameter name,
                      followed by "_", followed by the method's name. *)
                   let prefix =
                     "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^ "_" in
                   Format.fprintf out_fmter "%s%a"
                     prefix
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 else
                   (begin
                   if coll_name = (snd ctx.Context.scc_current_species) then
                     (begin
                     (* In fact, the name is qualified but with ourself
                        implicitely in the current compilation unit. Then, we
                        are not in the case of a toplevel species but in the
                        case where a substitution replaced Self by ourself.
                        We then must refer to our local record field. *)
                     Format.fprintf out_fmter "rf_%a"
                       Parsetree_utils.pp_vname_with_operators_expanded vname
                     end)
                   else
                     (begin
                   (* It comes from a toplevel stuff, hence not abstracted by
                      lambda-lifting. Then, we get the field of the
                      collection's record obtained by the collection's effective
                      value. *)
                   Format.fprintf out_fmter
                     "%a.effective_collection.(%a.rf_%a)"
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                     end)
                   end)
                 end)
             | Parsetree.Qualified (module_name, coll_name) ->
                 (begin
                 if module_name = ctx.Context.scc_current_unit then
                   (begin
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
                       ctx.Context.scc_species_parameters_names then
                     (begin
                     (* It comes from one of our species parameters. *)
                     let prefix =
                       "_p_" ^ (Parsetree_utils.name_of_vname coll_name) ^"_" in
                     Format.fprintf out_fmter "%s%a"
                       prefix Parsetree_utils.pp_vname_with_operators_expanded
                       vname
                       end)
                   else
                     (begin
                     (* It's not from one of our species parameter but it comes
                        from the current compilation unit. Let's check if the
                        species is ourself. In this case, liek above we must
                        refer to our local record field. *)
                     if coll_name = (snd ctx.Context.scc_current_species) then
                       Format.fprintf out_fmter "rf_%a"
                         Parsetree_utils.pp_vname_with_operators_expanded vname
                     else
                       (begin
                       Format.fprintf out_fmter
                         "%a.effective_collection.(%a.rf_%a)"
                         Parsetree_utils.pp_vname_with_operators_expanded
                         coll_name
                         Parsetree_utils.pp_vname_with_operators_expanded
                         coll_name
                         Parsetree_utils.pp_vname_with_operators_expanded
                         vname
                       end)
                     end)
                   end)
                 else
                   (begin
                   (* The called method belongs to a species that is not
                      ourselves and moreover belongs to another compilation
                      unit. May be a species from the toplevel of another
                      FoCaL source file. *)
                   Format.fprintf out_fmter
                     "%s.%a.effective_collection.(%s.%a.rf_%a)"
                     module_name
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     module_name
                     Parsetree_utils.pp_vname_with_operators_expanded coll_name
                     Parsetree_utils.pp_vname_with_operators_expanded vname
                   end)
                 end)
            end)
       end)
;;




let generate_constant ctx cst =
  match cst.Parsetree.ast_desc with
   | Parsetree.C_int str ->
       (* Integers are directly mapped in Coq. *)
       Format.fprintf ctx.Context.scc_out_fmter "%s" str
   | Parsetree.C_float _str ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "C_float"
   | Parsetree.C_bool str ->
       (* [true] maps on Coq "true". [false] maps on Coq "false". *)
       Format.fprintf ctx.Context.scc_out_fmter "%s" str
   | Parsetree.C_string str ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "\"%s\"%%string" str
   | Parsetree.C_char c ->
       (* [Unsure] *)
       Format.fprintf ctx.Context.scc_out_fmter "\"%c\"%%char" c
;;




let generate_constructor_ident_for_method_generator ctx env cstr_expr =
  try
    let mapping_info =
      Env.CoqGenEnv.find_constructor
        ~loc: cstr_expr.Parsetree.ast_loc
        ~current_unit: ctx.Context.scc_current_unit cstr_expr env in
    (match mapping_info.Env.CoqGenInformation.cmi_external_translation with
     | None ->
         (begin
         (* The constructor isn't coming from an external definition. *)
         let Parsetree.CI global_ident = cstr_expr.Parsetree.ast_desc in
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
                  fname          (* No module name capitalization in Coq. *)
                  Parsetree_utils.pp_vname_with_operators_expanded name
              else
                Format.fprintf ctx.Context.scc_out_fmter "%a"
                  Parsetree_utils.pp_vname_with_operators_expanded name
         end)
     | Some external_expr ->
         (begin
         (* The constructor comes from an external definition. *)
         let (_, coq_binding) =
           try
             List.find
               (function
                 | (Parsetree.EL_Coq, _) -> true
                 | (Parsetree.EL_Caml, _)
                 | ((Parsetree.EL_external _), _) -> false)
               external_expr
           with Not_found ->
             (* No Coq mapping found. *)
             raise
               (Externals_generation_errs.No_external_constructor_def
                  ("Coq", cstr_expr)) in
         (* Now directly generate the name the constructor is mapped onto. *)
         Format.fprintf ctx.Context.scc_out_fmter "%s" coq_binding
         end)) ;
    (* Always returns the number of type arguments that must be printed
       after the constructor. *)
    mapping_info.Env.CoqGenInformation.cmi_num_polymorphics_extra_args
  with _ ->
    (* Since in Coq all the constructors must be inserted in the generation
       environment, if we don't find the constructor, then we were wrong
       somewhere else before. *)
    assert false
;;



(* ************************************************************************** *)
(* force_polymorphic_explicit_args: bool -> Context.species_compil_context -> *)
(*   Env.CoqGenEnv.t -> Parsetree.pattern -> unit                             *)
(** {b Descr} : Emits coq code for a pattern. Attention, this function can
    also be used to generate code from a pettern but not in the context of
    generating a pattern in the target code (see description of
    [~force_polymorphic_explicit_args]).

    {b Args} :
      - [~force_polymorphic_explicit_args]: Normally, when generating sum value
        constructors as pattern we must never add the "_" denoting the
        polymorphism if the contructor belongs to a parametrized type. The
        only exception is when we generate the recursive functions termination
        lemmas with [Rec_let_gen.generate_binding_match].
        In effect, in this case, we must generate the hypotheses induced by
        conditions (hence also match) and separate them by ->. And in this
        case, we do not re-generate a real pattern-matching, but an expression.
        And in expression, polymorphic arguments must be explicitely writen
        with some "_"s. Moreover, this means that we must disable the implicit
        arguments by prefixing the constructor by "@".

    {b Exported} : Yes                                                        *)
(* ************************************************************************** *)
let generate_pattern ~force_polymorphic_explicit_args ctx coqctx env pattern =
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec rec_gen_pat pat =
    match pat.Parsetree.ast_desc with
     | Parsetree.P_const constant -> generate_constant ctx constant
     | Parsetree.P_var name ->
         Format.fprintf out_fmter "%a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_as (p, name) ->
         Format.fprintf out_fmter "(" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter "as@ %a)"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | Parsetree.P_wild -> Format.fprintf out_fmter "_"
     | Parsetree.P_constr (ident, pats) ->
         (* Disallow implicit arguments if needed. *)
         if force_polymorphic_explicit_args then Format.fprintf out_fmter "@@" ;
         let extras =
           generate_constructor_ident_for_method_generator ctx env ident
         in
         (* If we must force the apparition of polymorphic extra arguments... *)
         if force_polymorphic_explicit_args then
           (begin
             (* Add the type arguments of the constructor. *)
             match pat.Parsetree.ast_type with
             | Parsetree.ANTI_type t ->
                 Types.pp_type_simple_args_to_coq coqctx out_fmter t extras
             | _ -> assert false
           end) ;
         (* In "match" patterns, extra arguments of the constructor due to
            polymorphism never appear in Coq syntax. *)
         Format.fprintf out_fmter "@ " ;
         rec_generate_pats_list ~comma: false pats ;
     | Parsetree.P_record _labs_pats ->
         Format.eprintf "generate_pattern P_record TODO@."
     | Parsetree.P_tuple pats ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_generate_pats_list ~comma: true pats ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.P_paren p ->
         Format.fprintf out_fmter "(@[<1>" ;
         rec_gen_pat p ;
         Format.fprintf out_fmter ")@]"


  and rec_generate_pats_list ~comma = function
    | [] -> ()
    | [last] -> rec_gen_pat last
    | h :: q ->
        rec_gen_pat h ;
        if comma then Format.fprintf out_fmter "," ;
        Format.fprintf out_fmter "@ " ;
        rec_generate_pats_list ~comma: comma q in
  (* ********************** *)
  (* Now, let's do the job. *)
  rec_gen_pat pattern
;;



type let_binding_pre_computation = {
  lbpc_value_body : Env.CoqGenInformation.value_body ;
  lbpc_params_names : Parsetree.vname list ;
  lbpc_nb_polymorphic_args : int ;
  lbpc_params_with_type : (Parsetree.vname * Types.type_simple option) list ;
  lbpc_result_ty : Types.type_simple option ;
  lbpc_generalized_vars : Types.type_variable list
} ;;



(* ************************************************************************** *)
(** {b Descr}: Initiate computation of things needed by [let_binding_compile]
    and also needed to pre-enter recursive identifiers in the Coq env in order
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
let pre_compute_let_binding_info_for_rec env bd ~is_rec ~toplevel =
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
    if not toplevel then Env.CoqGenInformation.VB_non_toplevel
    else
      Env.CoqGenInformation.VB_toplevel_let_bound
        (params_names, def_scheme, bd.Parsetree.ast_desc.Parsetree.b_body) in
  let env' =
    if is_rec then
      let toplevel_loc = if toplevel then Some bd.Parsetree.ast_loc else None in
      Env.CoqGenEnv.add_value
        ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
        (nb_polymorphic_args, value_body) env
    else env in
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
    bindings, accumulating the obtained environment at each step.             *)
(* ************************************************************************** *)
let pre_compute_let_bindings_infos_for_rec ~is_rec ~toplevel env bindings =
  (* And not [List.fold_right otherwise the list of infos will be reversed
     compared to the list of bindings. *)
  List.fold_left
    (fun (env_accu, infos_accu) binding ->
      let (env', info) = 
        pre_compute_let_binding_info_for_rec
          ~is_rec ~toplevel env_accu binding in
      (env', info :: infos_accu))
    (env, [])
    bindings
;;

    

(* ************************************************************************** *)
(** {b Descr}: Code generation for *one* let binding, recursive of not.
    If the binding is recursive, then whatever the choosen Coq primitive ("fix"
    or "Fixpoint"), 
    This function is called by [Main_coq_generation.toplevel_let_def_compile]
    to generate code for toplevel definitions and by [let_in_def_compile] to
    generate code for local definitions.
    This function properly handles termination proofs stated as "structural"
    and inserts the right "{struct xxx}" in the generated code.
    However, in case of recursive function with no termination proof or a
    non-"structural" proof, it considers invariably that the recursion decreases
    on the fisrt argument of the function and dumps a {struct fst arg}.
    
    {b Args}:
     - [binder]: What Coq construct to use to introduce the definition, i.e.
       "Let", "let", "let fix", "Fixpoint" or "with".

    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let rec let_binding_compile ctx ~binder ~opt_term_proof
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~is_rec ~toplevel env bd pre_computed_bd_info =
  (* Create once for all the flag used to insert the let-bound idents in the
     environment. *)
  let toplevel_loc = if toplevel then Some bd.Parsetree.ast_loc else None in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the binder and the bound name. *)
  Format.fprintf out_fmter "%s@ %a"
    binder Parsetree_utils.pp_vname_with_operators_expanded
    bd.Parsetree.ast_desc.Parsetree.b_name ;
  (* Build the print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.lbpc_generalized_vars in
  (* If the original scheme is polymorphic, then we must add extra Coq
     parameters of type "Set" for each of the generalized variables. Hence,
     printing the variables used to instanciate the polymorphic ones in front
     of the function, they will appear and moreover they will be "tagged" as
     "seen" in the variable mapping. Hence, when we will print the arguments
     having these variables as type, the same variable name will be used,
     hence establishing the correct link between the type of the variable and
     the type variable of the function argument's type. *)
  List.iter
    (fun var ->
      Format.fprintf out_fmter "@ (%a : Set)"
        Types.pp_type_variable_to_coq var)
    generalized_vars ;
  let params_with_type = pre_computed_bd_info.lbpc_params_with_type in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Types.pp_type_simple_to_coq print_ctx) param_ty
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
      if is_rec then (
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
      if is_rec then (
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
       Format.fprintf out_fmter "@ :@ %a"
         (Types.pp_type_simple_to_coq print_ctx) t
  ) ;
  (* Output now the ":=" sign ending the Coq function's "header".
     With a NON-breakable space before to prevent uggly hyphenation ! *)
  Format.fprintf out_fmter " :=@\n" ;
  (* Here, each parameter name of the binding may mask a "in"-parameter. *)
  let local_idents' = pre_computed_bd_info.lbpc_params_names @ local_idents in
  (* Now, let's generate the bound body. *)
  (match bd.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e ->
      let in_recursive_let_section_of =
        if is_rec then
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
  if is_rec then env
  else
    Env.CoqGenEnv.add_value
      ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
      (pre_computed_bd_info.lbpc_nb_polymorphic_args,
       pre_computed_bd_info.lbpc_value_body) env



(* ************************************************************************** *)
(** {b Descr} : Starts compiling *local* recursive or not functions. Currently,
    recursive one are always compiled with the "fix" (lowercase !) construct of
    Coq.                                                                      *)
(* ************************************************************************** *)
and let_in_def_compile ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status env let_def =
  if let_def.Parsetree.ast_desc.Parsetree.ld_logical = Parsetree.LF_logical then
    failwith "Coq compilation of logical let in TODO" ;  (* [Unsure]. *)
  let out_fmter = ctx.Context.scc_out_fmter in
  let is_rec =
    (match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
     | Parsetree.RF_no_rec -> false | Parsetree.RF_rec -> true) in
  (* Generates the binder ("fix" or non-"fix"). *)
  Format.fprintf out_fmter "@[<2>" ;
  let initial_binder =
    (match is_rec with
     | false -> "let"
     | true ->
         (* [Unsure] We don't known now how to compile several local mutually
            recursive functions. *)
         if (List.length let_def.Parsetree.ast_desc.Parsetree.ld_bindings) > 1
         then failwith "TODO: local mutual recursive functions." ;
         "let fix") in
  let opt_term_proof =
    let_def.Parsetree.ast_desc.Parsetree.ld_termination_proof in
  (* Recover pre-compilation info and extended environment in case of
     recursivity for all the bindings. *)
  let (env, pre_comp_infos) =
    pre_compute_let_bindings_infos_for_rec
      ~is_rec ~toplevel: false env
      let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
  (* Now generate each bound definition. *)
  let env' =
    (match (let_def.Parsetree.ast_desc.Parsetree.ld_bindings, pre_comp_infos)
    with
     | ([], _) ->
         (* The "let" construct should always at least bind one identifier ! *)
         assert false
     | ([one_bnd], [one_pre_comp_info]) ->
         let_binding_compile
           ctx ~opt_term_proof ~binder: initial_binder
           ~in_recursive_let_section_of ~local_idents ~self_methods_status
           ~recursive_methods_status ~toplevel: false ~is_rec env one_bnd
           one_pre_comp_info
     | ((first_bnd :: next_bnds),
        (first_pre_comp_info :: next_pre_comp_infos)) ->
         let accu_env =
           ref
             (let_binding_compile
                ctx ~opt_term_proof ~binder: initial_binder
                ~in_recursive_let_section_of ~local_idents ~self_methods_status
                ~recursive_methods_status ~toplevel: false ~is_rec env first_bnd
                first_pre_comp_info) in
         List.iter2
           (fun binding pre_comp_info ->
             (* We transform "let and" non recursive functions into several
                "let in" definitions. *)
             Format.fprintf out_fmter "@ in@]@\n@[<2>" ;
             accu_env :=
               let_binding_compile
                 ctx ~opt_term_proof ~binder: "let" ~in_recursive_let_section_of
                 ~local_idents ~self_methods_status ~recursive_methods_status
                 ~is_rec ~toplevel: false env binding pre_comp_info)
           next_bnds next_pre_comp_infos ;
           !accu_env
     | (_, _) ->
         (* Case where we would not have the same number og pre-compiled infos
            and of bindings. Should never happen. *)
         assert false) in
  Format.fprintf out_fmter "@]" ;
  env'



and generate_expr ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status initial_env
    initial_expression =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the coq type print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
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
                Format.fprintf out_fmter "(%a :@ %a)@ "
                  Parsetree_utils.pp_vname_with_operators_expanded arg_name
                  (Types.pp_type_simple_to_coq print_ctx) arg_ty ;
                (* Return the remainder of the type to continue. *)
                res_ty)
              fun_ty
              vnames) ;
         Format.fprintf out_fmter "=>@ " ;
         rec_generate_expr loc_idents env body ;
         Format.fprintf out_fmter ")@]" ;
     | Parsetree.E_var ident ->
         (begin
         generate_expr_ident_for_E_var
           ctx ~in_recursive_let_section_of ~local_idents: loc_idents
           ~self_methods_status ~recursive_methods_status ident ;
         (* Now, add the extra "_"'s if the identifier is polymorphic. *)
         try
           let current_species_name =
             Some
               (Parsetree_utils.name_of_vname
                 (snd ctx.Context.scc_current_species)) in
           let (nb_polymorphic_args, _) =
             Env.CoqGenEnv.find_value
               ~loc: ident.Parsetree.ast_loc
               ~current_unit: ctx.Context.scc_current_unit
               ~current_species_name ident env in
           for _i = 0 to nb_polymorphic_args - 1 do
             Format.fprintf out_fmter "@ _"
           done
         with
           (* If the identifier was not found, then it was may be a local  *)
           (* identifier bound by a pattern. Then we can safely ignore it. *)
           Env.Unbound_identifier (_, _) -> ()
         end)
     | Parsetree.E_app (func_expr, args) ->
         Format.fprintf out_fmter "@[<2>(" ;
         rec_generate_expr loc_idents env func_expr ;
         Format.fprintf out_fmter "@ " ;
         rec_generate_exprs_list ~comma: false loc_idents env args ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_constr (cstr_ident, args) ->
         Format.fprintf out_fmter "@[<1>(@@" ;
         let extras =
           generate_constructor_ident_for_method_generator ctx env cstr_ident
         in
         (* Add the type arguments of the constructor. *)
         begin match expression.Parsetree.ast_type with
         | Parsetree.ANTI_type t ->
             Types.pp_type_simple_args_to_coq print_ctx out_fmter t extras
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
         (begin
         Format.fprintf out_fmter "@[<1>match " ;
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter " with" ;
         List.iter
           (fun (pattern, expr) ->
             (* My indentation style: indent of 4 between *)
             (* the pattern and its related processing.   *)
             Format.fprintf out_fmter "@\n@[<4>| " ;
             generate_pattern
               ~force_polymorphic_explicit_args: false ctx print_ctx env
               pattern;
             Format.fprintf out_fmter " =>@\n" ;
             (* Here, each name of the pattern may mask a "in"-parameter. *)
             let loc_idents' =
               (Parsetree_utils.get_local_idents_from_pattern pattern) @
               loc_idents in
             rec_generate_expr loc_idents' env expr ;
             Format.fprintf out_fmter "@]")
           pats_exprs ;
         Format.fprintf out_fmter "@\nend@]"
         end)
     | Parsetree.E_if (expr1, expr2, expr3) ->
         Format.fprintf out_fmter "@[<2>(if@ " ;
         rec_generate_expr loc_idents env expr1 ;
         Format.fprintf out_fmter "@ @[<2>then@ @]" ;
         rec_generate_expr loc_idents env expr2 ;
         Format.fprintf out_fmter "@ @[<2>else@ @]" ;
         rec_generate_expr loc_idents env expr3 ;
         Format.fprintf out_fmter ")@]"
     | Parsetree.E_let (let_def, in_expr) ->
         let env' =
           let_in_def_compile
             ctx ~in_recursive_let_section_of ~local_idents
             ~self_methods_status ~recursive_methods_status env let_def in
         Format.fprintf out_fmter "@ in@\n" ;
         rec_generate_expr loc_idents env' in_expr
     | Parsetree.E_record _labs_exprs ->
         (* [Unsure] *)
         Format.fprintf out_fmter "E_record"
     | Parsetree.E_record_access (_expr, _label) ->
         Format.fprintf out_fmter "E_record_access"
     | Parsetree.E_record_with (_expr, _labels_exprs) ->
         Format.fprintf out_fmter "E_record_with"
     | Parsetree.E_tuple exprs ->
         (begin
         match exprs with
          | [] -> assert false
          | [one] -> rec_generate_expr loc_idents env one
          | _ ->
              Format.fprintf out_fmter "@[<1>(" ;
              rec_generate_exprs_list ~comma: true loc_idents env exprs ;
              Format.fprintf out_fmter ")@]"
         end)
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
           (* Simply a somewhat verbatim output of the Coq translation. *)
           let (_, coq_code) =
             List.find
               (function
                 | (Parsetree.EL_Coq, _) -> true
                 | (Parsetree.EL_Caml, _)
                 | ((Parsetree.EL_external _), _) -> false)
               e_translation.Parsetree.ast_desc in
           Format.fprintf out_fmter "%s" coq_code
         with Not_found ->
           (* No Coq mapping found. *)
           raise
             (Externals_generation_errs.No_external_value_def
                ("Coq", (Parsetree.Vlident "<expr>"),
                 expression.Parsetree.ast_loc))
         end)
     | Parsetree.E_paren expr ->
         Format.fprintf out_fmter "@[<1>(" ;
         rec_generate_expr loc_idents env expr ;
         Format.fprintf out_fmter ")@]"



  and rec_generate_exprs_list ~comma loc_idents env = function
    | [] -> ()
    | [last] -> rec_generate_expr loc_idents env last
    | h :: q ->
        rec_generate_expr loc_idents env h ;
        if comma then Format.fprintf out_fmter ",@ "
        else Format.fprintf out_fmter "@ " ;
        rec_generate_exprs_list ~comma loc_idents env q in


  (* ************************************************ *)
  (* Now, let's really do the job of [generate_expr]. *)
  rec_generate_expr local_idents initial_env initial_expression
;;



let generate_logical_expr ctx ~in_recursive_let_section_of ~local_idents
    ~self_methods_status ~recursive_methods_status initial_env
    initial_proposition =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the coq type print context. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
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
             Format.fprintf out_fmter "forall %a : Set,@ "
               Types.pp_type_variable_to_coq var)
           generalized_vars ;
         (* In Coq, we must write: "forall x y : Set, ..."
            but "exists x : Set, exists y : Set, ..." so just change the way
            we print depending on the binder. *)
         (match proposition.Parsetree.ast_desc with
          | Parsetree.Pr_forall (_, _, _) ->
              (* Now, print the binder and the real bound variables. *)
              Format.fprintf out_fmter "forall@ %a :@ %a,@ "
                (Handy.pp_generic_separated_list
                   " "
                   (fun ppf vn ->
                     Format.fprintf ppf "%s"
                       (Parsetree_utils.vname_as_string_with_operators_expanded
                          vn)))
                vnames
                (Types.pp_type_simple_to_coq print_ctx) ty
          | Parsetree.Pr_exists (_, _, _) ->
              (* Now, print the real bound variables. *)
              List.iter
                (fun vn ->
                  Format.fprintf out_fmter "exists %s :@ %a,@ "
                    (Parsetree_utils.vname_as_string_with_operators_expanded
                       vn)
                    (Types.pp_type_simple_to_coq print_ctx) ty)
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
               Env.CoqGenEnv.add_value
                 ~toplevel: None vname
                 (0, Env.CoqGenInformation.VB_non_toplevel) accu_env)
             env
             vnames in
         rec_generate_logical_expr loc_idents' env' logical_expr ;
         Format.fprintf out_fmter "@]"
         end)
     | Parsetree.Pr_imply (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter " ->@ " ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_or (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter " \\/@ " ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_and (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter " /\\@ " ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
         Format.fprintf out_fmter "@[<2>" ;
         rec_generate_logical_expr loc_idents env logical_expr1 ;
         Format.fprintf out_fmter " <->@ " ;
         rec_generate_logical_expr loc_idents env logical_expr2 ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_not logical_expr ->
         Format.fprintf out_fmter "@[<2>" ;
         Format.fprintf out_fmter "~" ;
         rec_generate_logical_expr loc_idents env logical_expr ;
         Format.fprintf out_fmter "@]"
     | Parsetree.Pr_expr expr ->
         (* The wrapper surrounding the expression by Coq's "Is_true" if the
            expression's type is [bool]. *)
         let is_bool =
           (match expr.Parsetree.ast_type with
            | Parsetree.ANTI_type ty -> Types.is_bool_type ty
            | Parsetree.ANTI_none
            | Parsetree.ANTI_irrelevant
            | Parsetree.ANTI_scheme _ ->
                (* Note that expression never has a type scheme, but only a
                   type. *)
                assert false) in
         if is_bool then Format.fprintf out_fmter "@[<2>Is_true (" ;
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



(* ************************************************************************ *)
(* current_unit: Parsetree.modname -> Format.formatter -> Parsetree.expr -> *)
(*   unit                                                                   *)
(** {b Descr} : Translate an [expr] expected to be a species parameter
    expression into a Coq type.
    Because species names are capitalized, they must appear as sum
    constructors [expr]s. We also allow to have parentheses surrounding the
    expression. Hence, this function only handles these 2 kinds of [expr]s.

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let rec generate_expr_as_species_parameter_expression ~current_unit ppf expr =
  match expr.Parsetree.ast_desc with
   | Parsetree.E_constr (constr_ident, exprs) ->
       (* Remember that species names are capitalized. Hence the only legal
          core expression denoting species are sum type constructors. *)
       let Parsetree.CI glob_ident = constr_ident.Parsetree.ast_desc in
       let cstr_qual_name =
         (match glob_ident.Parsetree.ast_desc with
          | Parsetree.I_local vn -> Parsetree.Vname vn
          | Parsetree.I_global qvn -> qvn) in
       Format.fprintf ppf "%a"
         (simply_pp_to_coq_qualified_vname ~current_unit) cstr_qual_name ;
       if exprs <> [] then
         Format.fprintf ppf "@[<2>@ (%a)@]"
           (Handy.pp_generic_separated_list ";"
              (generate_expr_as_species_parameter_expression ~current_unit))
           exprs
   | Parsetree.E_paren expr' ->
       generate_expr_as_species_parameter_expression ~current_unit ppf expr'
   | _ -> assert false
;;



(* *********************************************************************** *)
(* Context.species_compil_context ->                                       *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     (Parsetree.vname * Parsetree.vname) list                            *)
(** {b Descr}: Generate the Coq code of a species parameters. It outputs
    both the parameters names and their type as a Coq expression.
    Either the parameter is a "is" parameter and then it's type will be
    rebuilt from its species expression.
    Or it is a "in" parameter and then it is a parameter of the record
    only if its type is built from another of our species parameters (i.e.
    not from a toplevel species/collection).
    Next come the extra parameters coming from the methods we depend on.
    Returns the list of species params and methods required 
    to create a value of the type record, i.e. the one we found
    dependencies on in the body of the record type.

    Used when generating the record type definition.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_record_type_parameters ctx env field_abstraction_infos =
  let ppf = ctx.Context.scc_out_fmter in
  let current_unit = ctx.Context.scc_current_unit in
  (* We first abstract the species/entity parameters carriers and entity
     parameters. *)
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      match param_kind with
       | Types.CCMI_is ->
           Format.fprintf ppf "@[<1>(%s_T :@ Set)@ @]" param_name
       | Types.CCMI_in provenance ->
           (* We generate the lambda-lifting for the the type of the   *)
           (* "IN" parameter here. When we will inspect the methods to *)
           (* abstract their dependencies on species parameters, we    *)
           (* the will skip "IN" parameters (that trivially lead to a  *)
           (* dependency on a pseudo method wearing their name) to     *)
           (* avoid doubles.                                           *)
           match provenance with
            | Types.SCK_species_parameter ->
                Format.fprintf ppf "@[<1>(_p_%s_%s :@ %s_T)@ @]"
                  param_name param_name param_ty_coll
            | Types.SCK_toplevel_collection | Types.SCK_toplevel_species ->
                Format.fprintf ppf "@[<1>(_p_%s_%s :@ "
                  param_name param_name ;
                if param_ty_mod <> current_unit then
                  Format.fprintf ppf "%s." param_ty_mod ;
                Format.fprintf ppf "%s.effective_collection.(" param_ty_coll ;
                if param_ty_mod <> current_unit then
                  Format.fprintf ppf "%s." param_ty_mod ;
                Format.fprintf ppf "%s.rf_T))@ @]" param_ty_coll)
    ctx.Context.scc_collections_carrier_mapping ;
  (* Now, we will find the methods of the parameters we decl-depend on in the
     Coq type expressions. Such dependencies can only appear through properties
     and theorems bodies. *)
  let species_parameters_names = ctx.Context.scc_species_parameters_names in
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
      Types.cpc_collections_carrier_mapping =
        ctx.Context.scc_collections_carrier_mapping } in
  (* We first build the lists of dependent methods for each property and
     theorem fields. *)
  let tmp_deps_for_fields =
    Abstractions.make_empty_param_deps species_parameters_names in
  let deps_for_fields =
    List.map (fun (a, b) -> (a, ref b)) tmp_deps_for_fields in
  List.iter
    (function
      | Abstractions.FAI_sig ( _, _)
      | Abstractions.FAI_let (_, _)
      | Abstractions.FAI_let_rec _ -> ()
      | Abstractions.FAI_theorem (_, ai)
      | Abstractions.FAI_property (_, ai) ->
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
            ai.Abstractions.ai_dependencies_from_params_for_record_type)
    field_abstraction_infos ;
  (* Just remove the references inside the assoc list. *)
  let deps_for_fields_no_ref =
    List.map (fun (a, b) -> (a, !b)) deps_for_fields in
  (* We now sort these methods according to the parameters' dependency graph. *)
  let ordered_deps_for_fields =
    Dep_analysis.order_species_params_methods deps_for_fields_no_ref in
  (* By the way, returns the list of species params and methods required to
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
               Format.fprintf ppf "(%s : %a)@ "
                 llift_name
                 (Types.pp_type_simple_to_coq print_ctx)
                 ty
           | Parsetree_utils.DETK_logical lexpr ->
               Format.fprintf ppf "(%s : " llift_name ;
               generate_logical_expr ctx
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
    contains a field per method. This type is named as the species.
    Returns the list of species params and methods required to create a
    value of the type record, i.e. the one we found dependencies on in the
    body of the record type.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx env species_descr field_abstraction_infos =
  let out_fmter = ctx.Context.scc_out_fmter in
  let collections_carrier_mapping =
    ctx.Context.scc_collections_carrier_mapping in
  (* The header of the Coq record definition for the species. *)
  let (my_fname, my_species_name) = ctx.Context.scc_current_species in
  (* Directly trasform into a string, that's easier. *)
  let my_species_name = Parsetree_utils.name_of_vname my_species_name in
  (* We do not add any bindings to the [collections_carrier_mapping]  *)
  (* before printing the record type parameters for 2 reasons:        *)
  (*   - species parameters carriers of the record are in the         *)
  (*     [collections_carrier_mapping], hence any added binding would *)
  (*     make think to an extra species parameter, hence to an extra  *)
  (*     parameter to the record (obviously wrong).                   *)
  (*   - since species carriers are not recursive, there is no reason *)
  (*     to have "Self" parametrizing its own record type.            *)
  Format.fprintf out_fmter "@[<2>Record %s " my_species_name ;
  (* Generate the record parameters mapping the species  *)
  (* parameters and the methods from them we depend on ! *)
  let abstracted_params_methods_in_record_type =
    generate_record_type_parameters
      ctx env field_abstraction_infos in
  (* Print the type of the record and it's constructor. *)
  Format.fprintf out_fmter ": Type :=@ mk_record {@\n"  ;
  (* Always generate the "rep" coercion. *)
  Format.fprintf out_fmter "@[<2>rf_T :> Set" ;
  (* We now extend the collections_carrier_mapping with ourselve known.
     Hence, if we refer to our "rep" we will be directly mapped onto the
     "rf_T" without needing to re-construct this name each time. Do same
     thing for "Self". *)
  let collections_carrier_mapping =
    (make_Self_cc_binding_rf_T
      ~current_species: ctx.Context.scc_current_species) ::
    ((my_fname, my_species_name),(("rf"), Types.CCMI_is)) ::
    collections_carrier_mapping in
  (* We mask the old ctx to take benefit of the new one with the bindings. *)
  let ctx = {
    ctx with
    Context.scc_collections_carrier_mapping = collections_carrier_mapping } in
  (* Create the coq type print context with the context new bindings. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping = collections_carrier_mapping } in
  (* Put a trailing semi only if there are other fields to generate. *)
  (match species_descr.Env.TypeInformation.spe_sig_methods with
   | [] -> ()
   | [Env.TypeInformation.SF_sig (_, n, _)] ->
       if (Parsetree_utils.name_of_vname n) = "rep" then
         ()   (* Case where there was only 1 field and that field was "rep". *)
       else Format.fprintf out_fmter " ;"
   | _ -> Format.fprintf out_fmter " ;") ;
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
          Format.fprintf out_fmter "(* From species %a. *)@\n"
            Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
          (* Field is prefixed by the species name for sake of unicity. *)
          Format.fprintf out_fmter "@[<2>rf_%a : %a"
            Parsetree_utils.pp_vname_with_operators_expanded n
            (Types.pp_type_simple_to_coq print_ctx) ty ;
          if semi then Format.fprintf out_fmter " ;" ;
          Format.fprintf out_fmter "@]@\n"
          end)
        end)
    | Env.TypeInformation.SF_let_rec l ->
        List.iter
          (fun (from, n, _, sch, _, _, _, _) ->
            let ty = Types.specialize sch in
            Format.fprintf out_fmter "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
            (* Field is prefixed by the species name for sake of unicity. *)
            Format.fprintf out_fmter "@[<2>rf_%a : %a"
              Parsetree_utils.pp_vname_with_operators_expanded n
              (Types.pp_type_simple_to_coq print_ctx) ty ;
            if semi then Format.fprintf out_fmter " ;" ;
            Format.fprintf out_fmter "@]@\n")
          l
    | Env.TypeInformation.SF_theorem
        (from, n, _polymorphic_vars_map, logical_expr, _, _)
    | Env.TypeInformation.SF_property
        (from, n, _polymorphic_vars_map, logical_expr, _) ->
        (* In the record type, theorems and properties are displayed in same
           way. *)
        Format.fprintf out_fmter "(* From species %a. *)@\n"
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
        (* Field is prefixed by the species name for sake of unicity. *)
        Format.fprintf out_fmter "@[<2>rf_%a :@ "
          Parsetree_utils.pp_vname_with_operators_expanded n ;
        (* Generate the Coq code representing the proposition.
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
        if semi then Format.fprintf out_fmter " ;" ;
        Format.fprintf out_fmter "@]@\n" in
  (* Coq syntax required not semi after the last field. That's why a simple
     [List.iter] of [output_one_field]'s body doesn't work.
     One must separate the case of the last element of the list. *)
  let rec iter_semi_separated = function
    | [] -> ()
    | [last] -> output_one_field ~semi: false last
    | h :: q ->
        output_one_field ~semi: true h ;
        iter_semi_separated q in
  iter_semi_separated species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}.@\n@\n" ;
  abstracted_params_methods_in_record_type
;;
