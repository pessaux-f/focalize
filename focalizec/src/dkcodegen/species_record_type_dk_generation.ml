(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
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
                 Env.DkGenInformation.VB_non_toplevel accu_env)
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
         Expr_dk_generation.generate_expr
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
                 ~self_methods_status: (Expr_dk_generation.SMS_from_param species_param_name)
                 (* Anyway, in the record type, bodies of recursive are
                    never expanded. Hence this choice or another for
                    [~recursive_methods_status] is not important.
                    It could be if we allowed recursive logical methods. *)
                                        ~recursive_methods_status: Expr_dk_generation.RMS_regular env lexpr ;
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
          ~self_methods_status: Expr_dk_generation.SMS_from_record
          (* Anyway, in the record type, bodies of recursive are never
             expanded. Hence this choice or another for
             [~recursive_methods_status] is not important.
             It could be if we allowed recursive logical methods. *)
          ~recursive_methods_status: Expr_dk_generation.RMS_regular env logical_expr ;
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


let rec let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~rec_status ~toplevel env bd
    pre_computed_bd_info =
  if rec_status = Env.DkGenInformation.RC_rec then
    (rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~toplevel env bd
    pre_computed_bd_info)
  else (
  (* Create once for all the flag used to insert the let-bound idents in the
     environment. *)
  let toplevel_loc = if toplevel then Some bd.Parsetree.ast_loc else None in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the binder and the bound name. *)
  let fun_name = bd.Parsetree.ast_desc.Parsetree.b_name in
  Format.fprintf out_fmter "def %a"
    Parsetree_utils.pp_vname_with_operators_expanded
    fun_name;
  (* Build the print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.Expr_dk_generation.lbpc_generalized_vars in
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
  let params_with_type = pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type in
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
  (* Now, print the result type of the "definition". *)
  (match pre_computed_bd_info.Expr_dk_generation.lbpc_result_ty with
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
  let local_idents' =
    (List.map fst pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type) @ local_idents in
  (* Now, let's generate the bound body. *)
  (match bd.Parsetree.ast_desc.Parsetree.b_body with
  | Parsetree.BB_computational e ->
      Expr_dk_generation.generate_expr
        ctx ~in_recursive_let_section_of ~local_idents: local_idents'
        ~self_methods_status ~recursive_methods_status env e
  | Parsetree.BB_logical _ -> assert false) ;
  (* Finally, we record, (except if it was already done in [env'] in case of
     recursive binding) the number of extra arguments due to polymorphism the
     current bound identifier has. *)
  Env.DkGenEnv.add_value
    ~toplevel: toplevel_loc bd.Parsetree.ast_desc.Parsetree.b_name
    pre_computed_bd_info.Expr_dk_generation.lbpc_value_body env)

and rec_let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status ~toplevel env bd
    pre_computed_bd_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  let fun_name = bd.Parsetree.ast_desc.Parsetree.b_name in

  (* Generate the binder and the bound name for the function. *)
  Format.fprintf out_fmter "def %a :@ "
    Parsetree_utils.pp_vname_with_operators_expanded
    fun_name ;
  (* Build the print context. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Context.scc_current_species) ;
    Dk_pprint.dpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  let generalized_vars = pre_computed_bd_info.Expr_dk_generation.lbpc_generalized_vars in
  List.iter
    (fun var ->
      Format.fprintf out_fmter "%a : cc.uT ->@ "
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;
  let params_with_type = pre_computed_bd_info.Expr_dk_generation.lbpc_params_with_type in
  (* Now, generate each of the real function's parameter with its type. *)
  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "%a : cc.eT (%a) ->@ "
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Dk_pprint.pp_type_simple_to_dk print_ctx) param_ty
       | None ->
           (* Because we provided a type scheme to the function
              [bind_parameters_to_types_from_type_scheme], MUST get one type
              for each parameter name ! *)
           assert false)
    params_with_type ;

  let return_ty =
  (match pre_computed_bd_info.Expr_dk_generation.lbpc_result_ty with
   | None ->
       (* Because we provided a type scheme to the function
          [bind_parameters_to_types_from_type_scheme], MUST get one type for
          the result value of the "let". *)
       assert false
   | Some t -> t) in
  Format.fprintf out_fmter "@ cc.eT (%a)"
                 (Dk_pprint.pp_type_simple_to_dk print_ctx) return_ty;
  (* Now, print the result type of the "definition". *)
  Format.fprintf out_fmter ".@\n";

  let result =
    let_binding_compile ctx
    ~in_recursive_let_section_of ~local_idents ~self_methods_status
    ~recursive_methods_status
    ~rec_status:Env.DkGenInformation.RC_non_rec ~toplevel env
    {bd with
      Parsetree.ast_desc =
        {bd.Parsetree.ast_desc with
          Parsetree.b_name =
            Parsetree.Vlident
              (Format.fprintf Format.str_formatter "rec_%a"
                 Parsetree_utils.pp_vname_with_operators_expanded fun_name;
               Format.flush_str_formatter ()
              )}}
    pre_computed_bd_info in


  (* Copied from species_dk_generation.ml, see the explanation there. *)
  (* Generate the CBV version. *)
  Format.fprintf out_fmter ".@]@\n@[<2>[] %a -->@ "
    Parsetree_utils.pp_vname_with_operators_expanded fun_name;

  List.iter
    (fun var ->
      Format.fprintf out_fmter "%a : cc.uT =>@ "
        Dk_pprint.pp_type_variable_to_dk var)
    generalized_vars ;

  List.iter
    (fun (param_vname, pot_param_ty) ->
      match pot_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "%a : cc.eT (%a) =>@ "
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Dk_pprint.pp_type_simple_to_dk print_ctx) param_ty
       | None ->
           (* Because we provided a type scheme to the function
              [bind_parameters_to_types_from_type_scheme], MUST get one type
              for each parameter name ! *)
          assert false)
    params_with_type;

  let rec print_cbv_types_as_arrows out = function
    | [] -> Dk_pprint.pp_type_simple_to_dk print_ctx out return_ty
    | ty :: l ->
       Format.fprintf out "(@[cc.Arrow %a %a@])"
                      (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows l
  in

  let rec print_cbv accu out = function
    | [] ->
       Format.fprintf out "@[rec_%a"
                      Parsetree_utils.pp_vname_with_operators_expanded fun_name;
       (* TODO something *)
       Format.fprintf out "@]"
    | (a, Some ty) :: l when Dk_pprint.has_cbv ty ->
       Format.fprintf out "@[%a@ %a@ (%a)@ %a@]"
                      (Dk_pprint.pp_for_cbv_type_simple_to_dk print_ctx) ty
                      print_cbv_types_as_arrows accu
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (a, Some ty) :: l ->
       Format.fprintf out "@[%a@ %a@]"
                      (print_cbv (ty :: accu)) l
                      Parsetree_utils.pp_vname_with_operators_expanded a
    | (_, None) :: _ -> assert false
  in

  print_cbv [] out_fmter (List.rev params_with_type);
  result
