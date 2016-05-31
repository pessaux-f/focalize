(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Fran�ois Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ...  LIP6 and INRIA                                      *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
    Coq of FoCaL's collections and species.                        *)
(* *************************************************************** *)

exception Attempt_proof_by_def_of_species_param of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_def_of_declared_method_of_self of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_def_of_local_ident of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_prop_of_local_ident of
  (Location.t * Parsetree.expr_ident) ;;


exception Attempt_proof_by_unknown_hypothesis of
  (Location.t * Parsetree.vname) ;;


exception Attempt_proof_by_unknown_step of
  (Location.t * Parsetree.node_label) ;;


let section_gen_sym =
  let cnt = ref 0 in
  (fun () ->
    let tmp = !cnt in
    incr cnt ;
    tmp)
;;



(* ************************************************************************ *)
(** {b Descr} : Recover the parameters abstracted from our method to apply
    to a method generator. This function must only be used when the
    method whom we search the generator has been identified as inherited.
    Otherwise, the search will fail.

    {b Args} :
      - [~current_unit] : The current compilation unit.
      - [from_species] : The [Parsetree.qualified_species]
      - [method_name] : The name of the method the generator is looked for.
      - [env] : The current Coq code generation environment.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let find_inherited_method_generator_abstractions ~current_unit from_species
    method_name env =
  (* This ident is temporary and created just to lookup in the environment. *)
  let from_as_ident =
    Parsetree_utils.make_pseudo_species_ident
      ~current_unit from_species in
  try
    let (_, species_meths_infos, _, _) =
      Env.CoqGenEnv.find_species
        ~loc: Location.none ~current_unit from_as_ident env in
    (* Now, find the method in the species information. *)
    let method_info =
      List.find
        (fun { Env.mi_name = n } -> n = method_name) species_meths_infos in
    method_info.Env.mi_abstracted_methods
  with
  | Env.No_available_Coq_code_generation_envt file ->
      (* Ok, re-raise it to catch it at toplevel. *)
      raise (Env.No_available_Coq_code_generation_envt file)
  | _ ->
    (* Because the generator is inherited, the species where it is hosted
       MUST be in the environment. Otherwise, something went wrong... *)
    assert false
;;



(* ************************************************************************ *)
(* Parsetree.vname -> compiled_species_fields list -> compiled_field_memory *)
(** {b Descr} : Search for the [compiled_field_memory] of the method [name]
    in the list [fields]. This function is used to recover while generating
    a generator application which arguments it takes.
    Obviously the [name] must be found because by construction of the
    FoCaL model, generators somebody depends on ARE created before this
    somebody.
    Basically, while processing a field, one will search for its
    dependencies among the list of the previously compiled fields of the
    species. And this list will grow wih the newly ompiled field and wil be
    passed to compile the next field on the species. And so on...

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let find_compiled_field_memory name fields =
  let rec find = function
    | [] -> assert false
    | h :: q -> (
        match h with
        | Misc_common.CSF_let field_memory
        | Misc_common.CSF_theorem field_memory ->
            if field_memory.Misc_common.cfm_method_name = name then field_memory
            else find q
        | Misc_common.CSF_let_rec fields_memories -> (
            try
              List.find
                (fun fm -> fm.Misc_common.cfm_method_name = name)
                fields_memories
            with Not_found -> find q
           )
        | _ -> find q
       ) in
  find fields
;;


(** Finish the job of [instanciate_parameters_through_inheritance] in the
    case where the parameter has been indentified as a IS parameter. *)
let instanciate_IS_parameter_through_inheritance ctx env original_param_index
    field_memory meths_from_param =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Instanciation process of "IS" parameter. We start processing from the
     oldest species where the currently compiled method appeared, i.e. the
     species where it was really DEFINED. *)
  let instancied_with =
    Misc_common.follow_instanciations_for_is_param
      ctx (Abstrs.EK_coq env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  (match instancied_with with
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       let prefix =
         if coll_mod = current_unit then coll_name ^ "."
         else coll_mod ^ "." ^ coll_name ^ "." in
       List.iter
         (fun (meth, _) ->
           (* Don't print the type to prevent being too verbose. *)
           Format.fprintf out_fmter "@ %s%a"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Coq, species parameters are abstracted by "_p_species_xxx". *)
       let species_param_name =
         match prm with
          | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
          | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
              Parsetree.Vuident n in
       let prefix = (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
       List.iter
         (fun (meth, _) ->
           (* Don't print the type to prevent being too verbose. *)
           Format.fprintf out_fmter "@ _p_%s%a"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param)
;;



(** Finish the job of [instanciate_parameters_through_inheritance] in the
    case where we instanciate parameter's carrier and this parameter has been
    indentified as a IS parameter. *)
let instanciate_IS_parameter_carrier_through_inheritance ctx env
    original_param_index field_memory =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Instanciation process of "IS" parameter. We start processing from the
     oldest species where the currently compiled method appeared, i.e. the
     species where it was really DEFINED. *)
  let instancied_with =
    Misc_common.follow_instanciations_for_is_param
      ctx (Abstrs.EK_coq env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  match instancied_with with
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       Format.fprintf out_fmter "@ " ;
       if coll_mod <> current_unit then Format.fprintf out_fmter "%s." coll_mod;
       Format.fprintf out_fmter "%s.me_as_carrier" coll_name
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Coq, species parameters are abstracted by "_p_species_xxx". *)
       let species_param_name =
         match prm with
          | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
          | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) -> n in
       Format.fprintf out_fmter "@ _p_%s_T" species_param_name
;;



let instanciate_parameters_through_inheritance ctx env field_memory =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We first must search at the origin of the method generator, the arguments
     it had. Since the method we are dealing with is inherited it is
     mandatorily hosted in an existing species reachable via the environment. *)
  (* This ident is temporary and created just to lookup in the environment. *)
  let host_ident =
    Parsetree_utils.make_pseudo_species_ident
      ~current_unit
      field_memory.Misc_common.cfm_from_species.Env.fh_initial_apparition in
  let (original_host_species_params, host_method_infos, _, _) =
    Env.CoqGenEnv.find_species
      ~loc: Location.none ~current_unit host_ident env in
  if Configuration.get_verbose () then
    Format.eprintf "Originally hosting species '%a' has %d parameters.@."
      Sourcify.pp_ident host_ident (List.length original_host_species_params);
  (* We search the dependencies the original (i.e at the level where we found
      the method generator) method had on its species parameters' methods. *)
  let meth_info =
    List.find
      (fun inf -> inf.Env.mi_name = field_memory.Misc_common.cfm_method_name)
      host_method_infos in
  if Configuration.get_verbose () then (
    Format.eprintf "Method '%a' has the following dependencies on parameters:@."
      Sourcify.pp_vname field_memory.Misc_common.cfm_method_name;
    List.iter
      (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
        let species_param_name =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
               Parsetree.Vuident n in
        Format.eprintf "\t From parameter '%a', dependencies on methods: "
          Sourcify.pp_vname species_param_name ;
        List.iter
          (fun (meth, _) -> Format.eprintf "%a " Sourcify.pp_vname meth)
          meths_from_param;
        Format.eprintf "@.")
      meth_info.Env.mi_dependencies_from_parameters
   ) ;
  (* Since in Coq, types are explicit, now we apply to each extra parameter
     coming from the lambda liftings that represent the types of the species
     parameters used in the method. The stuff to apply is not always "_p_" +
     the species name + "_T" since the species may have no parameters and the
     parent one (from where the method generator comes) may have some. For this
     reason, we must instanciate the original species parameters (i.e. the
     ones of the species from where the method generator comes). *)
  List.iter
    (fun species_param_type_name ->
      (* Since we are dealing with carrier types, we are only interested by
         IS parameters. IN parameters have their type abstracted only if it
         is the one of a IS parameter (hence, this last one is a IS and is
         found just as said above). If the IN parameter has the type of a
         toplevel species/collection, then this type is not abstracted, hence
         do not need to be instanciated ! *)
      let as_string = Parsetree_utils.name_of_vname species_param_type_name in
      let original_param_index =
        Handy.list_first_index
          (function
            | Env.TypeInformation.SPAR_in (_, _, _) -> false
            | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                n = as_string)
          original_host_species_params in
      instanciate_IS_parameter_carrier_through_inheritance
        ctx env original_param_index field_memory)
    meth_info.Env.mi_used_species_parameter_tys ;
  (* Now, we address the instanciation of the species parameters' methods.
     For each species parameter, we must trace by what it was instanciated. *)
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
      (* Find the index of the parameter in the species's signature from where
         the method was REALLY defined (not the one where it is inherited). *)
      let original_param_index =
        Handy.list_first_index
          (fun p -> p = species_param) original_host_species_params in
      match species_param with
       | Env.TypeInformation.SPAR_in (param_name, _, _) ->
           (* By construction, in dependencies of "in" parameter, the list of
              methods is always 1-length at most and contains directly the
              name of the parameter itself if it is really used. *)
           let number_meth = List.length meths_from_param in
           assert (number_meth <= 1);
           if number_meth = 1 then (
             (* For substitution, we technically need to know in which
                compilation unit the parameter, hence in fact the species,
                was. *)
             let (original_param_unit, _) =
               field_memory.Misc_common.cfm_from_species.
                 Env.fh_initial_apparition in
             (* We get the FoCaL expression once substitutions are done. *)
             let instancied_expr =
               Misc_common.follow_instanciations_for_in_param
                 ctx (Abstrs.EK_coq env) param_name
                 original_param_unit original_param_index
                 field_memory.Misc_common.cfm_from_species.
                   Env.fh_inherited_along in
             (* We must now generate the Coq code for this FoCaL expression. *)
             Format.fprintf out_fmter "@ @[<1>(";
             Species_record_type_coq_generation.generate_expr
               ctx ~local_idents: [] ~in_recursive_let_section_of: []
               (* Or whatever, "Self" will never appear at this point. *)
               ~self_methods_status:
                 Species_record_type_coq_generation.SMS_abstracted
               ~recursive_methods_status:
                 Species_record_type_coq_generation.RMS_regular
               env instancied_expr ;
             Format.fprintf out_fmter ")@]"
            )
       | Env.TypeInformation.SPAR_is ((_, _), _, _, _, _) ->
           instanciate_IS_parameter_through_inheritance
             ctx env original_param_index field_memory meths_from_param)
    meth_info.Env.mi_dependencies_from_parameters
;;



(** G�n�re l'expression � mettre derri�re le :=  dans les arguments d'une
    m�thode afin de forcer une equivalence due � une def-d�pendance de cette
    methode vis � vis de la m�thode [name] venant de [from].
    Ceci revient � trouver le g�n�rateur de m�thode de [name] puis �
    l'appliquer � tous ses arguments. "Tous ses arguments" signifie les
    carriers de param�tres, les m�thodes de param�tres et surtout les
    m�thodes de l'esp�ce courante EN FONCTION DE SI ELLES SONT ENCORE
    ABSTRAITES OU EFFECTIVEMENT D�FINIES. Cel� signifie qu'il faut �tre capable
    de savoir si dans l'esp�ce courante une m�thode dont on d�pend est rest�e
    lambda-lift�e dans la m�thode courante ou est d�finie dans l'esp�ce.
    Dans le premier cas, il faudra utiliser le param�tre _p_xxx qui se
    trouvera (forc�ment, par construction) en argument de la m�thode courante.
    Dans le second cas, il faudra utiliser directement le nom de la methode
    qui se trouvera (forc�ment, par construction) g�n�r� dans l'esp�ce
    courante ou un de ses parents (=> notation point�e).
    Donc pour savoir cela, il faut que l'on ait une trace parmis les champs
    d�j� g�n�r�s desquels sont effectivement d�finis et desquels sont encore
    abstraits dans l'esp�ce courante. Avant, on s'en foutait car on nommait
    les m�thodes de Self par abst_xxx tout le temps et la seule diff�rence
    �tait que si elles �taient d�finies on en faisait des Definition et si
    elles �taient abstraites, on en faisait des Variables ! *)
let generate_def_dependency_equivalence env ctx generated_fields from name =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the application of the method generator. First, its name. *)
  let defined_from = from.Env.fh_initial_apparition in
  if (fst defined_from) <> ctx.Context.scc_current_unit then
    Format.fprintf out_fmter "%s." (fst defined_from);
  (* If the generator comes from another species, qualify by its module. *)
  if defined_from <> ctx.Context.scc_current_species then
    Format.fprintf out_fmter "%a."
      Parsetree_utils.pp_vname_with_operators_expanded
      (snd defined_from);
  (* Now, print the generator's name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Now, recover from the already generated fields, what to apply to this
     generator. *)
  let memory = find_compiled_field_memory name generated_fields in
  (* We first instanciate the parameters corresponding to the carriers types
     of species parameters and appearing in the method's type, then the
     abstracted methods from the species params we depend on.
     Bug fix #211: if the method comes from an inherited species, then we must
     instanciate the parameters of the inherited species by what they were
     instantiated in the "inherit" clause. In this case, the function
     [generate_method_lambda_lifted_arguments] must be told not to generate
     the lambda-liftings corresponding to the parameters carriers and
     methods. *)
  let only_for_Self_meths =
    if defined_from <> ctx.Context.scc_current_species then
      (instanciate_parameters_through_inheritance ctx env memory; true)
    else false in
  Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
    ~only_for_Self_meths out_fmter
    memory.Misc_common.cfm_used_species_parameter_tys
    memory.Misc_common.cfm_dependencies_from_parameters
    memory.Misc_common.cfm_coq_min_typ_env_names
;;



let find_method_type_kind_by_name vname coll_meths =
  let method_info = List.find (fun mi -> mi.Env.mi_name = vname) coll_meths in
  method_info.Env.mi_type_kind
;;



(** Factorise la gen�ration des abstrations pour un champ d�fini. Ca colle
    donc les abstractions dues aux types des param�tres d'esp�ce, puis
    aux m�thodes des param�tres d'esp�ce dont on d�pend, puis enfin aux
    m�thodes de nous-m�mes dont on d�pend.
    En cas de non-Section, on met l'espace de s�paration AVANT.

  Args:
    - [~in_section] : True when this function is called to generate abstractions
    in a Section. Is is only the case when used to generate the code of a
    temporary theorem for Zenon purpose. In this case, instead of abstracting
    dependencies by adding extra arguments to the current definition, we
    generate Variable, Let and Hypothesis.*)
let generate_field_definition_prelude ~in_section ctx print_ctx env
    generalized_vars min_coq_env used_species_parameter_tys
    dependencies_from_params generated_fields =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the polymorphic type variables. *)
  List.iter
    (fun var ->
      if in_section then
        Format.fprintf out_fmter "@[<2>Variable %a :@ Set.@]@\n"
          Coq_pprint.pp_type_variable_to_coq var
      else
        Format.fprintf out_fmter "@ (%a : Set)"
          Coq_pprint.pp_type_variable_to_coq var)
    generalized_vars ;
  (* Generate the parameters from the species parameters' types we use.
     By the way, we get the stuff to add to the current collection carrier
     mapping to make so the type expressions representing some species
     parameter carrier types, will be automatically be mapped onto our
     freshly created extra args. The trailing "_T" will be automatically added
     by the type printing routine. *)
  let cc_mapping_extension =
    List.map
      (fun species_param_type_name ->
        let as_string =
          Parsetree_utils.vname_as_string_with_operators_expanded
            species_param_type_name in
        let param_name =  "_p_" ^ as_string in
        (* First, generate the parameter. *)
        if in_section then
          Format.fprintf out_fmter "@[<2>Variable %s_T :@ Set.@]@\n" param_name
        else Format.fprintf out_fmter "@ (%s_T :@ Set)" param_name;
        (* Return the stuff to extend the collection_carrier_mapping. *)
        ((ctx.Context.scc_current_unit, as_string),
         (param_name, Types.CCMI_is)))
      used_species_parameter_tys in
  (* Extend the collection_carrier_mapping of the context with species
     parameters stuff. *)
  let new_ctx = { ctx with
    Context.scc_collections_carrier_mapping =
       cc_mapping_extension @ ctx.Context.scc_collections_carrier_mapping } in
  (* Same thing for the printing comtext. *)
  let new_print_ctx = {
    print_ctx with
      Coq_pprint.cpc_collections_carrier_mapping =
        cc_mapping_extension @
          print_ctx.Coq_pprint.cpc_collections_carrier_mapping } in
  (* Abstract according to the species's parameters the current method depends
     on. *)
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
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
        (fun (meth, meth_ty_kind) ->
          if in_section then (
            match meth_ty_kind with
             | Parsetree_utils.DETK_computational meth_ty ->
                 Format.fprintf out_fmter "@[<2>Variable %s%a :@ %a.@]@\n"
                   prefix Parsetree_utils.pp_vname_with_operators_expanded meth
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) meth_ty
             | Parsetree_utils.DETK_logical lexpr ->
                 (* Inside the logical expression of the method of the
                    parameter "Self" must be printed as "_p_param_name_T". *)
                 let self_map =
                   Species_record_type_coq_generation.
                     make_Self_cc_binding_species_param
                       ~current_species: ctx.Context.scc_current_species
                        species_param_name in
                 let new_ctx' = { new_ctx with
                   Context.scc_collections_carrier_mapping =
                     self_map ::
                       new_ctx.Context.scc_collections_carrier_mapping } in
                 Format.fprintf out_fmter "@[<2>Variable %s%a :@ "
                   prefix
                   Parsetree_utils.pp_vname_with_operators_expanded meth;
                 (* Even if we are generating the prelude of a recursive
                    function, we can't have a recursion via the dependencies
                    from a species parameter. *)
                 Species_record_type_coq_generation.generate_logical_expr
                   new_ctx' ~in_recursive_let_section_of: [] ~local_idents: []
                   ~self_methods_status:
                     (Species_record_type_coq_generation.SMS_from_param
                        species_param_name)
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular
                   env lexpr ;
                 Format.fprintf out_fmter "@].@\n"
           )
          else (
            match meth_ty_kind with
             | Parsetree_utils.DETK_computational meth_ty ->
                 Format.fprintf out_fmter "@ (%s%a :@ %a)"
                   prefix Parsetree_utils.pp_vname_with_operators_expanded meth
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) meth_ty
             | Parsetree_utils.DETK_logical lexpr ->
                 (* Inside the logical expression of the method of the
                    parameter "Self" must be printed as "_p_param_name_T". *)
                 let self_map =
                   Species_record_type_coq_generation.
                     make_Self_cc_binding_species_param
                       ~current_species: ctx.Context.scc_current_species
                        species_param_name in
                 let new_ctx' = { new_ctx with
                   Context.scc_collections_carrier_mapping =
                     self_map ::
                       new_ctx.Context.scc_collections_carrier_mapping } in
                 Format.fprintf out_fmter "@ (%s%a :@ "
                   prefix
                   Parsetree_utils.pp_vname_with_operators_expanded meth;
                 (* Even if we are generating the prelude of a recursive
                    function, we can't have a recursion via the dependencies
                    from a species parameter. *)
                 Species_record_type_coq_generation.generate_logical_expr
                   new_ctx' ~in_recursive_let_section_of: [] ~local_idents: []
                   ~self_methods_status:
                     (Species_record_type_coq_generation.SMS_from_param
                        species_param_name)
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular
                   env lexpr ;
                 Format.fprintf out_fmter ")"
           ))
        meths_from_param)
    dependencies_from_params ;
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  let abstracted_methods =
    List.flatten
      (List.map
         (function (_, meth_dep)  ->
           (* Reason ignored since in Coq we take all the kinds of methods in
              account. *)
           match meth_dep with
           | Env.TypeInformation.MCEM_Defined_carrier sch ->
               (* Now, if "rep" is defined, then we generate an equivalence
                  between "abst_T" and it's representation using the
                  abstracted types passed as arguments to the Definition to
                  represent carriers of the species parameters we depend
                  on. *)
               let ty = Types.specialize sch in
               if in_section then
                 Format.fprintf out_fmter "Let abst_T := %a.@\n"
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) ty
               else
                 Format.fprintf out_fmter "@ (abst_T := %a)"
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) ty ;
               (* Anything defined is not abstracted. *)
               []
           | Env.TypeInformation.MCEM_Defined_computational (fr, _, n, _, _, _)
           | Env.TypeInformation.MCEM_Defined_logical (fr, n, _) ->
               (* We must add an equivalence to enforce de def-dependency. *)
               if in_section then
                 Format.fprintf out_fmter "Let abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n
               else
                 Format.fprintf out_fmter "@ (abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n;
               (* We must recover the method generator of the method we
                  def-depend on. Then we must apply this generator to the
                  arguments it needs, parameters carriers, parameters methods
                  and methods of Self depending on wether these last ones are
                  really defined in the current species or still abstract. *)
               generate_def_dependency_equivalence
                 env new_ctx generated_fields fr n;
               if in_section then Format.fprintf out_fmter ".@\n"
               else Format.fprintf out_fmter ")";
               []                  (* Anything defined is not abstracted. *)
           | Env.TypeInformation.MCEM_Declared_carrier ->
               (* Note that by construction, the carrier is first in the env. *)
               if in_section then
                 Format.fprintf out_fmter "@[<2>Variable abst_T : Set.@]@\n"
               else Format.fprintf out_fmter "@ (abst_T : Set)";
               [Parsetree.Vlident "rep"]
           | Env.TypeInformation.MCEM_Declared_computational (n, sch) ->
               (* Due to a decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               if in_section then
                 Format.fprintf out_fmter "@[<2>Variable abst_%a : %a.@]@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) ty
               else
                 Format.fprintf out_fmter "@ (abst_%a : %a)"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Coq_pprint.pp_type_simple_to_coq new_print_ctx) ty ;
               [n]
           | Env.TypeInformation.MCEM_Declared_logical (n, b) ->
               if in_section then
                 Format.fprintf out_fmter "@[<2>Hypothesis abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n
               else
                 Format.fprintf out_fmter "@ (abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n;
               (* Methods from Self are printed "abst_XXX" since dependencies
                  have leaded to extra parameters "abst_XXX".
                  Even if we are generating the prelude of a recursive
                  function, we can't have a recursion via the dependencies
                  from other methods of ourselves. *)
               Species_record_type_coq_generation.generate_logical_expr
                 new_ctx ~local_idents: [] ~in_recursive_let_section_of: []
                 ~self_methods_status:
                   Species_record_type_coq_generation.SMS_abstracted env b
                 ~recursive_methods_status:
                   Species_record_type_coq_generation.RMS_regular ;
               if in_section then Format.fprintf out_fmter ".@]@\n"
               else Format.fprintf out_fmter ")";
               [n])
         min_coq_env) in
  (abstracted_methods, new_ctx, new_print_ctx)
;;




(* {b Descr} : Generates the postlude of the prototype of a definec method. It
   Prints its arguments with their type (not those induced by lambda-liftings),
   its return type and its body (if some is provided) WITH methods
   abstracted by "abst_xxx".
   If [decr_arg_name] is provided, then it is assumed that this prototype it
   the one of a function defined with "Fixpoint" and in this case we insert
   a "{struct xxx}" directive before the return type. *)
let generate_defined_method_proto_postlude ctx print_ctx env
    ~self_manifest params scheme decr_arg_name_opt body_opt =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Add the parameters of the let-binding with their type. *)
  (* Ignore the result type of the "let" if it's a function because we never
     print the type constraint on the result of the "let". We only print them
     in the arguments of the let-bound ident.
     Note by the whay that we do not have anymore information about "Self"'s
     structure... *)
  let (params_with_type, ending_ty_opt, _) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      ~self_manifest (Some scheme) params in
  let ending_ty =
    (match ending_ty_opt with
     | None ->
         (* Because we always provide a type scheme (a [Some ...]), one must
            always be returned a type, i.e, something [Some ...].  *)
         assert false
     | Some t -> t) in
  List.iter
    (fun (param_vname, opt_param_ty) ->
      match opt_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Coq_pprint.pp_type_simple_to_coq print_ctx) param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Generate the { struct } clause. Since only functions are recursive,
     there is issue about finding no argument. We however ensure that
     the specified decreasing identifier is really a parameter of the
     function. [Unsure] on my mind, this should have been done ealier, may
     be at scoping. *)
   (match decr_arg_name_opt with
   | Some (proof_loc, fct_name, decr_arg_name) ->
       if not
           (List.exists (fun (n, _) -> n = decr_arg_name) params_with_type) then
         raise
           (Species_record_type_coq_generation.Wrong_decreasing_argument
              (proof_loc, ctx.Context.scc_current_species, fct_name,
               decr_arg_name)) ;
       Format.fprintf out_fmter "@ { struct %a }@ "
         Parsetree_utils.pp_vname_with_operators_expanded decr_arg_name
   | None -> ()) ;
  (* Now, we print the ending type of the method. *)
  Format.fprintf out_fmter " :@ %a "
    (Coq_pprint.pp_type_simple_to_coq print_ctx) ending_ty ;
  (* Generates the body's code of the method if some is provided.
     No local idents in the context because we just enter the scope of a species
     fields and so we are not under a core expression. Since we are generating
     a "let", methods from Self are printed "abst_XXX" since dependencies have
     leaded to "Variables abst_XXX" before this new "Variable". *)
  (match body_opt with
   | None -> ()
   | Some body ->
       Format.fprintf out_fmter ":=@ ";
       (match body with
        | Parsetree.BB_computational e ->
            Species_record_type_coq_generation.generate_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              env e
        | Parsetree.BB_logical p ->
            Species_record_type_coq_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env p))
;;



(* ************************************************************************* *)
(* Context.species_compil_context ->                                         *)
(*  Types.coq_print_context -> Env.CoqGenEnv.t ->                            *)
(*    min_coq_env_element list -> Parsetree.vname list ->                    *)
(*      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ->             *)
(*        Parsetree.vname -> Parsetree.expr -> let_connect: let_connector -> *)
(*          Parsetree.vname list -> Types.type_scheme ->                     *)
(*            Parsetree.vname list                                           *)
(** {b Descr} Gererate the Coq code for a method defined in the current
    species (i.e. not inherited). In fact, this generates the method
    generator for this method in this species.
    It returns the list of methods of ourselves we depend on and that were
    lambda-lifted according to th minimal coq environment.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let generate_defined_non_recursive_method ctx print_ctx env min_coq_env
    ~self_manifest used_species_parameter_tys dependencies_from_params
    generated_fields name body params scheme =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Start the Coq function definition. *)
  Format.fprintf out_fmter "@[<2>Definition %a"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  let (generalized_vars, _) = Types.scheme_split scheme in
  (* Generate the prelude of the method, i.e the sequence of parameters induced
     by the various lamda-liftings and their types .
     By the way, we get updated in the [new_print_ctx] the way "Self" must be
     printed. *)
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_field_definition_prelude
      ~in_section: false ctx print_ctx env generalized_vars min_coq_env
      used_species_parameter_tys dependencies_from_params generated_fields in
  (* We now generate the postlude of the method, i.e the sequence of real
     parameters of the method, not those induced by abstraction and finally
     the method's body. Inside, methods we depend on are abstracted by
     "abst_xxx". Since the method is non recursive, there is no decreasing
     argument to pass. *)
  generate_defined_method_proto_postlude
    new_ctx new_print_ctx env ~self_manifest params scheme None (Some body) ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n" ;
  abstracted_methods
;;



(* ********************************************************************** *)
(** {b Descr} Generate the coq code for a non-recursive method of the
    current species.
    If the method is defined in this species, then it generates the
    method generator. If the method is inherited, it recovers the
    methods abstracted in the generator without generating again the code.
    And in any case, it generate the local definition "self_..." by
    applying the generator to the local definitions.

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let generate_non_recursive_field_binding ctx print_ctx env min_coq_env
    ~self_manifest used_species_parameter_tys
    dependencies_from_params generated_fields
    (from, name, params, scheme, body) =
  (* First of all, only methods defined in the current species must be
     generated. Inherited methods ARE NOT generated again ! *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_non_recursive_method
        ctx print_ctx env min_coq_env ~self_manifest
        used_species_parameter_tys dependencies_from_params
        generated_fields name body params scheme
    else (
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited from species '%a' but not (re)-declared is \
          not generated again. @."
          Parsetree_utils.pp_vname_with_operators_expanded name
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
      (* Recover the arguments for abstracted methods of Self in the inherited
         generator. *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit
        from.Env.fh_initial_apparition name env
     ) in
  (* Finally, return the methods we found abstracted in the minimal Coq typing
     environment and that leaded to extra parameters via lambda-lifting. *)
  abstracted_methods
;;



(* ************************************************************************* *)
(** {b Descr} : Helper to find, during a Zenon proof by steps, an hypothesis
    by its name among the list of available material to do the proof.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let find_hypothesis_by_name name l =
  let rec rec_find = function
    | [] -> raise Not_found
    | h :: q -> (
        match h.Parsetree.ast_desc with
         | Parsetree.H_hypothesis (n, body) ->
             if n = name then body else rec_find q
         | _ -> rec_find q
       ) in
  rec_find l
;;



(** Kind of things that one can "assume" in a Zenon script. *)
type assumed_hypothesis =
  | AH_variable of (Parsetree.vname * Parsetree.type_expr)
  | AH_lemma of Parsetree.logical_expr
;;



(** {b Descr} : Helper. *)
let rec find_assumed_variables_and_lemmas_in_hyps = function
  | [] -> []
  | h :: q -> (
      let found_stuff = find_assumed_variables_and_lemmas_in_hyps q in
      match h.Parsetree.ast_desc with
      | Parsetree.H_variable (n, ty_expr) ->
          (AH_variable (n, ty_expr)) :: found_stuff
      | Parsetree.H_hypothesis (_, body) -> (AH_lemma body) :: found_stuff
      | Parsetree.H_notation (_, _) -> found_stuff
     )
;;



(** {b Descr} : Helper. *)
let rec find_only_PN_subs_in_proof_nodes = function
  | [] -> []
  | n :: q ->
      match n.Parsetree.ast_desc with
      | Parsetree. PN_sub (node_label, _, _) ->
          node_label :: (find_only_PN_subs_in_proof_nodes q)
      | _ -> find_only_PN_subs_in_proof_nodes q
;;



(* The [~in_zenon_by_def] boolean says if we are in a Zenon
   "by definition of a rec function".
   If we are in a Zenon "by definition of a rec function" we must emit the
   "Termination_fct_namespace.species__fct".

   {b Rem} : For Function.    *)
let generate_final_recursive_definifion_body_With_Function out_fmter
    ~in_zenon_by_def species_name name used_species_parameter_tys
    dependencies_from_params abstracted_methods generalized_vars =
  if in_zenon_by_def then
    Format.fprintf out_fmter "Termination_%a_namespace.%a_equation@ "
      Parsetree_utils.pp_vname_with_operators_expanded name
      Parsetree_utils.pp_vname_with_operators_expanded name
  else
    Format.fprintf out_fmter "Termination_%a_namespace.%a__%a@ "
      Parsetree_utils.pp_vname_with_operators_expanded name
      Parsetree_utils.pp_vname_with_operators_expanded species_name
      Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Apply to the generalized type variables. *)
   List.iter
     (fun var ->
       Format.fprintf out_fmter "@ %a" Coq_pprint.pp_type_variable_to_coq var)
     generalized_vars ; 
  (* We directly apply the abstracted arguments. That's a kind of
     eta-expansion. *)
  List.iter
    (fun n ->
      Format.fprintf out_fmter "_p_%a_T@ "
        Parsetree_utils.pp_vname_with_operators_expanded n)
    used_species_parameter_tys;
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
          Format.fprintf out_fmter "%s%a@ "
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths)
    dependencies_from_params;
  List.iter
    (fun n ->
      if n = Parsetree.Vlident "rep" then
        Format.fprintf out_fmter "abst_T@ "
      else
        Format.fprintf out_fmter "abst_%a@ "
          Parsetree_utils.pp_vname_with_operators_expanded n)
    abstracted_methods
;;



(** To make recursive definitions working with Zenon.

    {b Rem} : For Function. *)
let zenonify_by_recursive_meth_definition ctx print_ctx env
    ~self_manifest ~rec_kind used_species_parameter_tys
    dependencies_from_params abstracted_methods vname params scheme body =
  let out_fmter = ctx.Context.scc_out_fmter in
  let species_name = snd ctx.Context.scc_current_species in
  (* Check if the recursive fonction was generated using "Function" or
     "Fixpoint". *)
  (match rec_kind with
   | Env.RPK_other ->
       (* Assumed to have been generated with "Function". *)
       (* For bug #199, to make so that Zenon identifies "abst_xx" and "xx", we
          generate a fake Definition before generating the body of the recursive
          fonction whose body is needed because of the "by definition of...". *)
       Format.fprintf out_fmter
         "(* Method \"%a\" is recursive. Special syntax \"Function\" for \
         Zenon. *)@\n"
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       (* Use specific syntax to tell Zenon that the function is recursive. *)
       Format.fprintf out_fmter "@[<2>Function abst_%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       (* We now generate the sequence of real parameters of the method, NOT
          those induced by abstraction AND NOT the method's body. Inside this
          body, methods we depend on are abstracted by "abst_xxx". Since we are
          in the case of "by definition" of a function generated by "Function",
          it is not structural, hence there is no decreasing argument to
          pass. *)
       generate_defined_method_proto_postlude
         ctx print_ctx env ~self_manifest params scheme None None ;
       (* Now, for Zenon, we print the **real** definition of the recursive
          function, i.e. the one using the temporaries of the
          Section/namespace. *)
       Format.fprintf out_fmter "@\n{ ";
       (* Say that we are in a Zenon "by definition of a rec function" in order
          to have the name "Termination_fct_namespace.fct_equation" instead of
          "Termination_fct_namespace.species__fct". *)
       generate_final_recursive_definifion_body_With_Function
         out_fmter ~in_zenon_by_def: true species_name vname
         used_species_parameter_tys dependencies_from_params
         abstracted_methods
         [(*empty generalized vars*)] (* [Unsure] ... and wrong! *) ;
       Format.fprintf out_fmter " }@\n:=@\n"
   | Env.RPK_struct decr_arg_name ->
       (* Case where the recursive function was generated with "Fixpoint". In
          this case, we need to use the special syntax "Fixpoint" also for
          Zenon, and no definition equation. *)
       Format.fprintf out_fmter
         "(* Method \"%a\" is recursive structural. Special syntax \
         \"Fixpoint\" for Zenon. *)@\n"
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       (* Use specific syntax to tell Zenon that the function is recursive. *)
       Format.fprintf out_fmter "@[<2>Fixpoint abst_%a "
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       (* We now generate the sequence of real parameters of the method, NOT
          those induced by abstraction AND NOT the method's body. Inside this
          body, methods we depend on are abstracted by "abst_xxx". Since we are
          in the case of "by definition" of a function generated by "Fixpoint",
          we MUST pass the decreasing argument to have the "{ struct xxx }"
          directive emitted. We pass a [Location.none] (which is only used to
          raise an error if the decreasing arg doesn't exist) because there is
          no reason for such error occuring. In effect, we are in a
          "by definition", hence the real definition of the used method has
          inevitably be generated before, hence the check has already be
          done. *)
       generate_defined_method_proto_postlude
         ctx print_ctx env ~self_manifest params scheme
         (Some (Location.none, vname, decr_arg_name)) None ;
       Format.fprintf out_fmter ":=@\n"
  ) ;
  (* Now, we generate the body of the recursive function as given in the
     regular way (i.e. exactly like
     [generate_defined_proto_postlude] does if a body is
     provided.
     ATTENTION: Since here the recursively defined function is an "abst_xxx",
     we must ensure that occurrences of this function in its body will also
     be named "abst_xxx". For this, we explicitly say that the
     [recursive_methods_status] is [RMS_abstracted]. *)
  (match body with
   | Parsetree.BB_computational e ->
       Species_record_type_coq_generation.generate_expr
         ctx ~local_idents: [] ~in_recursive_let_section_of: [vname]
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_abstracted
         env e
   | Parsetree.BB_logical p ->
       Species_record_type_coq_generation.generate_logical_expr
         ctx ~local_idents: [] ~in_recursive_let_section_of: [vname]
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_abstracted env p) ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n"
;;



(** Ensure that the enforced dependencies of a coq script or assumed proof are
    related to entities that are really definitions, and not signatures or
    properties. This task is done for regular dependencies of Zenon proofs while
    generating the Zenon stuff. But for assumed or Coq script, since we directly
    emit verbatim code, dependencies must be checked aside.

    This function works like the below [zenonify_by_definition] but only does
    not emit any code. *)
let ensure_enforced_by_definition_is_definition min_coq_env def_expr_ident =
  match def_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       raise
         (Attempt_proof_by_def_of_local_ident
            (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
   | Parsetree.EI_global _ ->
       (* Since stuff is at toplevel, it cannot be a declaration and can only
          be a definition. *)
       ()
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
       | None -> (
           (* The method comes from ourselves (Self). So we will search it
              inside the coq minimal typing environment. *)
           let method_info =
             snd (MinEnv.find_coq_env_element_by_name vname min_coq_env) in
           match method_info with
           | Env.TypeInformation.MCEM_Declared_carrier
           | Env.TypeInformation.MCEM_Defined_carrier _ ->
               (* Syntax does not allow to mention "Self" as a proof
                  element. *)
               assert false
           | Env.TypeInformation.MCEM_Declared_computational (_, _)
           | Env.TypeInformation.MCEM_Declared_logical (_, _) ->
               (* We can't prove "by definition" of something only declared ! *)
               raise
                 (Attempt_proof_by_def_of_declared_method_of_self
                    (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
           | Env.TypeInformation.MCEM_Defined_computational (_, _, _, _, _, _)
           | Env.TypeInformation.MCEM_Defined_logical (_, _, _) -> ()
          )
       | Some (Parsetree.Qualified (_, _)) ->
           (* The method comes from another module's species. Hence it is for
              sure from a toplevel species. And this is not correct since the
              methods used for proofs must only come from our methods or
              species parameters' ones. [Unsure] *)
           failwith "I think this is a toplevel species method (1)."
       | Some (Parsetree.Vname _) -> (
           (* The method belongs to a species parameters. Since they are
              always abstract, it is forbidden to prove "by definition" of a
              species parameter method. *)
           raise
             (Attempt_proof_by_def_of_species_param
                (def_expr_ident.Parsetree.ast_loc, def_expr_ident))
          )
      )
;;



let ensure_enforced_dependencies_by_definition_are_definitions min_coq_env
    enf_deps =
  List.iter
    (fun enf_dep ->
      match enf_dep.Parsetree.ast_desc with
       | Parsetree.Ed_definition expr_idents ->
           List.iter
             (ensure_enforced_by_definition_is_definition min_coq_env)
             expr_idents
       | Parsetree.Ed_property _ -> ())
    enf_deps
;;



let zenonify_by_definition ctx print_ctx env min_coq_env ~self_manifest
    generated_fields available_hyps by_def_expr_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  match by_def_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       let rec lookup x l =
         match l with
         | { Parsetree.ast_desc =
               Parsetree.H_notation (((Parsetree.Vuident id
                                     | Parsetree.Vlident id) as y), body) }
             :: _ when x = y ->
            (id, body)
         | _ :: t -> lookup x t
         | [] -> raise
                   (Attempt_proof_by_def_of_local_ident
                      (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
       in
       let (id, body) = lookup vname available_hyps in
       Format.fprintf out_fmter
         "(* For notation used via \"by definition of %a\". *)@\n"
         Sourcify.pp_expr_ident by_def_expr_ident;
       Format.fprintf out_fmter "@[<2>Definition %s :=" id ;
       Species_record_type_coq_generation.generate_expr
         ctx ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status:
           Species_record_type_coq_generation.SMS_abstracted env
         ~recursive_methods_status:
           Species_record_type_coq_generation.RMS_regular
         body ;
       (* Done... Then, final carriage return. *)
       Format.fprintf out_fmter ".@]@\n"
   | Parsetree.EI_global qvname -> (
       (* The stuff is in fact a toplevel definition, not a species method. We
          must recover its type kind from the environment. *)
       let current_species_name =
         Some
           (Parsetree_utils.name_of_vname
              (snd ctx.Context.scc_current_species)) in
       let (_, value_body) =
         Env.CoqGenEnv.find_value
           ~loc: by_def_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit
           ~current_species_name by_def_expr_ident env in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(* For toplevel definition used via \"by definition of %a\". *)@\n"
         Sourcify.pp_expr_ident by_def_expr_ident ;
       let name_for_zenon =
         Parsetree_utils.make_concatenated_name_with_operators_expanded_from_qualified_vname
           ~current_unit: ctx.Context.scc_current_unit
           ~dont_qualify_if_local: true qvname in
       match value_body with
        | Env.CoqGenInformation.VB_non_toplevel -> assert false
        | Env.CoqGenInformation.VB_toplevel_let_bound
              (rec_status, params, scheme, body) -> (
            match rec_status with
            | Env.RC_non_rec ->
                (* Non recursive toplevel function: use a "Definition". *)
                Format.fprintf out_fmter "@[<2>Definition %s" name_for_zenon ;
                (* Attention, generalized type variables are normally
                   emitted in [generate_field_definition_prelude] !
                   Since we do call this function, we must manually
                   generate them here ! *)
                let (generalized_vars, _) = Types.scheme_split scheme in
                List.iter
                  (fun var ->
                    Format.fprintf out_fmter "@ (%a : Set)"
                      Coq_pprint.pp_type_variable_to_coq var)
                  generalized_vars ;
                (* We now generate the sequence of real parameters of the
                   method, NOT those induced by abstractions and finally the
                   method's body. Anyway, since the used definition is at
                   toplevel, there is no abstraction no notion of "Self", no
                   dependencies. *)
              generate_defined_method_proto_postlude
                ctx print_ctx env ~self_manifest params scheme None (Some body)
           | Env.RC_rec pr_kind ->
               (* Recursive function. Because toplevel, currently always
                  generated by "Fixpoint". *)
               Format.fprintf out_fmter "@[<2>Fixpoint %s" name_for_zenon ;
               (* Same than above: generate polymorphic type variables. *)
               let (generalized_vars, _) = Types.scheme_split scheme in
               List.iter
                 (fun var ->
                   Format.fprintf out_fmter "@ (%a : Set)"
                     Coq_pprint.pp_type_variable_to_coq var)
                 generalized_vars ;
               let decr_arg =
                 (match pr_kind with
                 | Env.RPK_struct n -> n
                 | Env.RPK_other ->
                    (* Currently, we shamely assume that the function is
                       structural on it first argument. *)
                    List.hd params) in
               generate_defined_method_proto_postlude
                 ctx print_ctx env ~self_manifest params scheme
                 (Some
                   (Location.none,
                    Parsetree_utils.unqualified_vname_of_expr_ident
                      by_def_expr_ident,
                    decr_arg))
                 (Some body)
            );
            (* Then, final carriage return. *)
            Format.fprintf out_fmter ".@]@\n"
        | Env.CoqGenInformation.VB_toplevel_property lexpr ->
            Format.fprintf out_fmter "@[<2>Definition %s :=@ " name_for_zenon ;
            (* Since the used definition is at toplevel, there is no abstraction
               no notion of "Self", no dependencies. *)
            Species_record_type_coq_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_from_record (* Or anything *)
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env lexpr ;
            (* Done... Then, final carriage return. *)
            Format.fprintf out_fmter ".@]@\n"
      )
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
        | None -> (
            (* The method comes from ourselves (Self). So we will search it
               inside the coq minimal typing environment. *)
            let method_info =
              snd (MinEnv.find_coq_env_element_by_name vname min_coq_env) in
            match method_info with
             | Env.TypeInformation.MCEM_Declared_carrier
             | Env.TypeInformation.MCEM_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | Env.TypeInformation.MCEM_Declared_computational (_, _)
             | Env.TypeInformation.MCEM_Declared_logical (_, _) ->
                 (* We can't prove "by definition" of something only
                    declared ! *)
                 raise
                   (Attempt_proof_by_def_of_declared_method_of_self
                      (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
             | Env.TypeInformation.MCEM_Defined_computational
                   (_, is_rec, _, params, scheme, body) -> (
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by definition of \
                   %a\". *)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident ;
                 match is_rec with
                 | Env.RC_rec rec_kind ->
                     (* Since we are in the case of a method of Self, we must
                        find the abstraction_info and the abstracted_methods
                        in the already [generated_fields]. *)
                     let memory =
                       find_compiled_field_memory vname generated_fields in
                     zenonify_by_recursive_meth_definition
                       ctx print_ctx env ~self_manifest ~rec_kind
                       memory.Misc_common.cfm_used_species_parameter_tys
                       memory.Misc_common.cfm_dependencies_from_parameters
                       memory.Misc_common.cfm_coq_min_typ_env_names vname params
                       scheme body
                 | Env.RC_non_rec  ->
                     Format.fprintf out_fmter "@[<2>Definition abst_%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       vname ;
                     (* We now generate the sequence of real parameters of the
                        method, not those induced by abstraction and finally the
                        method's body. Inside, methods we depend on are
                        abstracted by "abst_xxx". Non recursive, hence not
                        decreasing argument to pass. *)
                     generate_defined_method_proto_postlude
                       ctx print_ctx env ~self_manifest params scheme None
                       (Some body) ;
                     (* Done... Then, final carriage return. *)
                     Format.fprintf out_fmter ".@]@\n"
                 )
             | Env.TypeInformation.MCEM_Defined_logical (_, _, body) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by definition of \
                   %a\". *)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident;
                 Format.fprintf out_fmter "@[<2>Definition abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded vname ;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx".
                    No recursion problem here since recursive properties are
                    not allowed. *)
                 Species_record_type_coq_generation.generate_logical_expr
                   ctx ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Species_record_type_coq_generation.SMS_abstracted env
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
           )
        | Some (Parsetree.Qualified (_, _)) ->
            (* The method comes from another module's species. Hence it is for
               sure from a toplevel species. And this is not correct since the
               methods used for proofs must only come from our methods or
               species parameters' ones. [Unsure] *)
            failwith "I think this is a toplevel species method (2)."
        | Some (Parsetree.Vname _) -> (
            (* The method belongs to a species parameters. Since they are
               always abstract, it is forbidden to prove "by definition" of a
               species parameter method. *)
            raise
              (Attempt_proof_by_def_of_species_param
                 (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
           )
      )
;;



(* ******************************************************************* *)
(** {b Descr} : Helper type used to discriminate in the function
    [zenonify_by_property_when_qualified_method] the cases where the
    ident refers to a species parameter of the current species or to a
    toplevel species (possiliy in another module).

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
type species_param_or_topleve_species =
  | SPOTS_param of Parsetree.vname  (** The ident was refering to a species
                                        parameter of the current species. *)
  | SPOTS_toplevel_species of
      (Parsetree.module_name * (** The ident was refering to a toplevel species
                                   and here is the module name hosting the
                                   toplevel species. *)
       Parsetree.vname) (** The species name the ident was refering to. *)
;;



(** {b Descr} : Helper... *)
let exists_among_parameters_from_dependencies param_name deps_from_params =
  List.exists
    (fun (p ,_) ->
      match p with
       | Env.TypeInformation.SPAR_in (_, _, _) -> false
       | Env.TypeInformation.SPAR_is ((_, p), _, _, _, _) ->
           (Parsetree.Vuident p) = param_name)
    deps_from_params
;;




(* *********************************************************************** *)
(** {b Descr} : Handle the subcase of [zenonify_by_property] in the case
    where the method used by the proof has a qualified name.
    This decomposition in 2 functions is mostly to ease readability of the
    compiler because otherwise the function  [zenonify_by_property] gets
    really a mess to read !

    {b Args} :
      - [from_qcollname] : The qualified species name hosting the method
          used in the proof under a "by property...".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let zenonify_by_property_when_qualified_method ctx print_ctx env
    dependencies_from_params by_prop_expr_ident from_qcollname meth_vname =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We search if the [from_qcollname] has the same name than one of the
     species parameters. For this, either [from_qcollname] has no qualification
     hence is implicitely in the current compilation unit or is qualified with
     the same file name that the current compilation unit. *)
  let param_or_topl_species_to_search_opt =
    (match from_qcollname with
     | Parsetree.Vname param_name ->
         (* Implicitely in the current compilation unit. *)
         if exists_among_parameters_from_dependencies
             param_name dependencies_from_params then
           SPOTS_param param_name
         else
           SPOTS_toplevel_species (ctx.Context.scc_current_unit, param_name)
     | Parsetree.Qualified (mod_name, species_name) ->
         if exists_among_parameters_from_dependencies
             species_name dependencies_from_params then
           SPOTS_param species_name
         else SPOTS_toplevel_species (mod_name, species_name)) in
  match param_or_topl_species_to_search_opt with
   | SPOTS_toplevel_species (mod_name, topl_species_name) ->
       (begin
       (* The method comes from another module's species. Hence it
          is for sure from a toplevel species. We must recover its
          type. *)
       let fake_ident = {
         Parsetree.ast_desc =
           Parsetree.I_global
             (Parsetree.Qualified (mod_name, topl_species_name));
         (* Roughly correction as a location, even is not exact. *)
         Parsetree.ast_loc = by_prop_expr_ident.Parsetree.ast_loc;
         Parsetree.ast_annot = [];
         Parsetree.ast_type = Parsetree.ANTI_none } in
       let (_, coll_info, _, _) =
         Env.CoqGenEnv.find_species
           ~loc: by_prop_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit fake_ident env in
       (* Now, look for the of the method. *)
       let meth_ty_kind = find_method_type_kind_by_name meth_vname coll_info in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(* For toplevel collection's method used via \"by \
         property %a\". *)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       match meth_ty_kind with
        | Env.MTK_computational meth_sch ->
            let meth_ty = Types.specialize meth_sch in
            Format.fprintf out_fmter "@[<2>Parameter ";
            if mod_name <> ctx.Context.scc_current_unit then
              Format.fprintf out_fmter "%s." mod_name;
            Format.fprintf out_fmter "%a.%a"
              Parsetree_utils.pp_vname_with_operators_expanded topl_species_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname ;
            Format.fprintf out_fmter
              " :@ %a.@]@\n" (Coq_pprint.pp_type_simple_to_coq print_ctx) meth_ty
        | Env.MTK_logical lexpr ->
            Format.fprintf out_fmter
              "@[<2>Parameter ";
            if mod_name <> ctx.Context.scc_current_unit then
              Format.fprintf out_fmter "%s." mod_name;
            Format.fprintf out_fmter "%a.%a"
              Parsetree_utils.pp_vname_with_operators_expanded topl_species_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname;
            Format.fprintf out_fmter " :@ ";
            (* We must substitute Self by the toplevel collection. In effect,
               each method of Self of this toplevel collection must be called
               anyway
                  module.species.meth,
               possibly without module if it is the current one. This is needed
               only when generating in Zenon's Section the methods to used via
               "by property" or "by definition" when they are identified to be
               coming from a toplevel collection. In effect, in the structure we
               keep to be able to generate the text of these methods for Zenon,
               we have the body of the method BUT in its environment, that is,
               with methods of Self referencing the methods of the species.
               Unfortunately, when we generate the code, we are not in the scope
               of this species. Hence we must rename on the fly occurrences of
               Self'methods by explicitely qualifying them. This is hence done
               by applying a substitution of Self in the body of the method. *)
            let lexpr' =
              SubstColl.subst_logical_expr
                ~current_unit: ctx.Context.scc_current_unit
                SubstColl.SRCK_self
                (Types.SBRCK_coll
                   (mod_name,
                    (Parsetree_utils.name_of_vname topl_species_name))) lexpr in
            (* Now, let's dump the code of the modified body. *)
            Species_record_type_coq_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                (* Or whatever since we substituted Self by the effective
                   collection. *)
                Species_record_type_coq_generation.SMS_from_record
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env lexpr' ;
            Format.fprintf out_fmter ".@]@\n"
       end)
   | SPOTS_param param_name ->
       (begin
       (* The method belongs to a species parameters. We first get the
          species parameter's bunch of methods. *)
       let (Env.ODFP_methods_list param_meths) =
         Handy.list_assoc_custom_eq
           (fun spe_param searched ->
             match spe_param with
              | Env.TypeInformation.SPAR_in (_, _, _) ->
                  (* Proofs never use methods of "IN" parameters. *)
                  false
              | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                  (Parsetree.Vuident n) = searched)
           param_name
           dependencies_from_params in
       (* Now, get the type of the specified method. *)
       let (_, meth_ty_kind) =
         List.find (fun (n, _) -> n = meth_vname) param_meths in
       (* A bit of comment. *)
       Format.fprintf out_fmter
         "(* For species parameter method used via \"by \
         property %a\". *)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       (* The method is name by "_p_" + the species parameter's name
          + "_" + the method's name. *)
       match meth_ty_kind with
        | Parsetree_utils.DETK_computational meth_ty ->
            Format.fprintf out_fmter
              "@[<2>Parameter _p_%a_%a :@ %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded param_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname
              (Coq_pprint.pp_type_simple_to_coq print_ctx)
              meth_ty
        | Parsetree_utils.DETK_logical lexpr ->
            (* Inside the logical expression of the method of the parameter
               "Self" must be printed as "_p_param_name_T". *)
            let self_map =
              Species_record_type_coq_generation.make_Self_cc_binding_species_param
                ~current_species: ctx.Context.scc_current_species param_name in
            let ctx' = { ctx with
              Context.scc_collections_carrier_mapping =
                self_map :: ctx.Context.scc_collections_carrier_mapping } in
            Format.fprintf out_fmter
              "@[<2>Parameter _p_%a_%a :@ "
              Parsetree_utils.pp_vname_with_operators_expanded param_name
              Parsetree_utils.pp_vname_with_operators_expanded meth_vname;
            Species_record_type_coq_generation.generate_logical_expr
              ctx' ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                (Species_record_type_coq_generation.SMS_from_param param_name)
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env lexpr ;
            Format.fprintf out_fmter ".@]@\n"
       end)
;;



let zenonify_by_property ctx print_ctx env min_coq_env
    dependencies_from_params by_prop_expr_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  match by_prop_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       raise
         (Attempt_proof_by_prop_of_local_ident
            (by_prop_expr_ident.Parsetree.ast_loc, by_prop_expr_ident))
   | Parsetree.EI_global qvname -> (
       (* The stuff is in fact a toplevel definition, not a species method. We
           must recover its type kind from the environment. *)
       let current_species_name =
         Some
           (Parsetree_utils.name_of_vname
              (snd ctx.Context.scc_current_species)) in
       let (_, value_body) =
         Env.CoqGenEnv.find_value
           ~loc: by_prop_expr_ident.Parsetree.ast_loc
           ~current_unit: ctx.Context.scc_current_unit
           ~current_species_name by_prop_expr_ident env in
                 (* A bit of comment. *)
       Format.fprintf out_fmter
         "(* For toplevel definition used via \"by property of %a\". *)@\n"
         Sourcify.pp_expr_ident by_prop_expr_ident;
       let name_for_zenon =
         Parsetree_utils.make_concatenated_name_from_qualified_vname qvname in
       match value_body with
        | Env.CoqGenInformation.VB_non_toplevel -> assert false
        | Env.CoqGenInformation.VB_toplevel_let_bound (_, _, scheme, _) ->
            (* We just need to print the type of the method. *)
            let meth_ty = Types.specialize scheme in
            Format.fprintf out_fmter "@[<2>Parameter %s :@ %a.@]@\n"
              name_for_zenon (Coq_pprint.pp_type_simple_to_coq print_ctx) meth_ty
        | Env.CoqGenInformation.VB_toplevel_property lexpr ->
            Format.fprintf out_fmter "@[<2>Parameter %s :@ " name_for_zenon;
            (* Since the used definition is at toplevel, there is no abstraction
               no notion of "Self", no dependencies. *)
            Species_record_type_coq_generation.generate_logical_expr
              ctx ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_from_record (* Or anything *)
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env lexpr ;
            (* Done... Then, final carriage return. *)
            Format.fprintf out_fmter ".@]@\n"
      )
   | Parsetree.EI_method (qcollname_opt, vname) -> (
       match qcollname_opt with
        | None -> (
            (* The method comes from ourselves (Self). So we will search it
               inside the coq minimal typing environment. *)
            let method_info =
              snd (MinEnv.find_coq_env_element_by_name vname min_coq_env) in
            match method_info with
             | Env.TypeInformation.MCEM_Declared_carrier
             | Env.TypeInformation.MCEM_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | Env.TypeInformation.MCEM_Declared_computational (_, scheme)
             | Env.TypeInformation.MCEM_Defined_computational
                   (_, _, _, _, scheme, _) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by property %a\". *)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident;
                 (* We just need to print the type of the method. *)
                 let meth_ty = Types.specialize scheme in
                 Format.fprintf out_fmter "@[<2>Parameter abst_%a :@ %a.@]@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
                   (Coq_pprint.pp_type_simple_to_coq print_ctx) meth_ty
             | Env.TypeInformation.MCEM_Declared_logical (_, body)
             | Env.TypeInformation.MCEM_Defined_logical (_, _, body) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by property %a\". *)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident;
                 (* We need to print the logical expression of the method. *)
                 Format.fprintf out_fmter "@[<2>Parameter abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded vname;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx". *)
                 Species_record_type_coq_generation.generate_logical_expr
                   ctx ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Species_record_type_coq_generation.SMS_abstracted
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular env body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
           )
        | Some qcollname ->
            zenonify_by_property_when_qualified_method
              ctx print_ctx env dependencies_from_params by_prop_expr_ident
              qcollname vname
      )
;;



(** We must pass to Zenon the definition of the type in Coq-like syntax. *)
let zenonify_by_type ctx env type_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We first search for the type definition in the code generation
     environment. *)
  let ty_def =
    Env.CoqGenEnv.find_type
      ~loc: type_ident.Parsetree.ast_loc
      ~current_unit: ctx.Context.scc_current_unit type_ident env in
  (* A bit of comment. *)
  Format.fprintf out_fmter
    "(* For type definition used via \"by type %a\". *)@\n"
    Sourcify.pp_ident type_ident;
  (* Now, generate the definition like we usually do in Coq syntax. *)
  let reduced_ctx = {
    Context.rcc_current_unit = ctx.Context.scc_current_unit ;
    (* Since type definitions are at toplevel, they can never reference
       species parameters, so we can safely leave this list empty. *)
    Context.rcc_species_parameters_names = [] ;
    (* Since type definitions are at toplevel, they can never reference
       species parameters carriers, so we can safely leave this list empty. *)
    Context.rcc_collections_carrier_mapping = [] ;
    (* For the same reason than above, we can leave this list empty. *)
    Context.rcc_lambda_lift_params_mapping = [] ;
    Context.rcc_out_fmter = out_fmter } in
  (* Unqualify the type name since in a type definition, the name is always
     without any qualification. We never write "type foo#t = ..." ! *)
  let type_vname = Parsetree_utils.unqualified_vname_of_ident type_ident in
  (* Throw the resulting environment since we do not to bind any thing
     anymore. The returned value of [type_def_compile] is only useful when
     compiling the type definition the first time it appears, i.e. when we
     encounter it as a FoCaLize phrase. By the way, tell not to enrich the
     environment otherwise, since the type is already defined, when inserting
     it again in the environment, we will have an error telling "already
     bound". We also must qualify the constructors in the definition if
     the type is not hosted in the compilation unit where the proof appears.
     In effect, in functions using this type and on which we depend to make
     proofs, constructors WILL be qualified. Not qualifying constructors in
     the "fake" definition for Zenon would prevent ot from finding proofs. *)
  ignore
    (Type_coq_generation.type_def_compile
       ~as_zenon_fact: true reduced_ctx env type_vname ty_def)
;;



type proof_step_availability = {
  psa_node_label : Parsetree.node_label ;
  psa_lemma_name : Parsetree.vname ;
  psa_base_logical_expr : Parsetree.logical_expr ;
  psa_assumed_variables_and_lemmas : assumed_hypothesis list
}
;;



let add_quantifications_and_implications ctx print_ctx env avail_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  let rec rec_print = function
    | [] -> ()
    | assumed :: q -> (
        match assumed with
         | AH_variable (var_vname, ty_expr) -> (
             (* Quantify all the assumed variables. *)
             (match ty_expr.Parsetree.ast_type with
              | Parsetree.ANTI_type ty ->
                  Format.fprintf out_fmter "forall %a :@ %a,@ "
                    Parsetree_utils.pp_vname_with_operators_expanded var_vname
                    (Coq_pprint.pp_type_simple_to_coq print_ctx) ty
              | _ -> assert false) ;
             rec_print q
            )
         | AH_lemma log_expr ->
             (* Make a string of implications with the assumed logical
                expressions. *)
             Format.fprintf out_fmter "@[<1>(";
             Species_record_type_coq_generation.generate_logical_expr
               ctx ~local_idents: [] ~in_recursive_let_section_of: []
               ~self_methods_status:
                 Species_record_type_coq_generation.SMS_abstracted
               ~recursive_methods_status:
                 Species_record_type_coq_generation.RMS_regular env log_expr ;
             Format.fprintf out_fmter ") ->@ ";
             rec_print q ;
             Format.fprintf out_fmter "@]"
       ) in
  rec_print avail_info.psa_assumed_variables_and_lemmas
;;



(* *********************************************************************** *)
(** {b Descr} : Generate the Definition and Parameter for Coq that Zenon
    needs to automatically prove the current theorem. Methods used by the
    the proof are present in the minimal typing environment or the
    parameters dependencies since they induce either a def or a
    decl-dependency. Hence, to recover them, we will search inside these 2
    kinds of information.

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let zenonify_fact ctx print_ctx env min_coq_env ~self_manifest
    dependencies_from_params generated_fields available_hyps available_steps
    fact =
  let out_fmter = ctx.Context.scc_out_fmter in
  match fact.Parsetree.ast_desc with
   | Parsetree.F_definition expr_idents ->
       (* Syntax: "by definition ...". This leads to a Coq Definition. *)
       List.iter
         (zenonify_by_definition
           ctx print_ctx env min_coq_env ~self_manifest generated_fields
           available_hyps)
         expr_idents
   | Parsetree.F_property expr_idents ->
       (* Syntax: "by property ...". This leads to a Coq Parameter. *)
       List.iter
         (zenonify_by_property
            ctx print_ctx env min_coq_env dependencies_from_params)
         expr_idents
   | Parsetree.F_hypothesis vnames ->
       (* Syntax: "by hypothesis ...". *)
       List.iter
         (fun vname ->
           let hyp_logical_expr =
             (try find_hypothesis_by_name vname available_hyps with
             | Not_found ->
                 raise
                   (Attempt_proof_by_unknown_hypothesis
                      (fact.Parsetree.ast_loc, vname))) in
           Format.fprintf out_fmter "(* For hypothesis \"%a\". *)@\n"
             Sourcify.pp_vname vname;
           Format.fprintf out_fmter "@[<2>Parameter %a :@ "
             Parsetree_utils.pp_vname_with_operators_expanded vname ;
           Species_record_type_coq_generation.generate_logical_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status:
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular env hyp_logical_expr ;
           (* Done... Then, final carriage return. *)
           Format.fprintf out_fmter ".@]@\n")
         vnames
   | Parsetree.F_node node_labels ->
       (* Syntax: "by step ...". We must search among the [available_steps] the
          logical expression related to this step and print it in the current
          auto-proof section as a Parameter. This Parameter must be universally
          quantified by all the "assume" [H_variable]s found in the available
          hypotheses and be the consequence of the implications strings
          compound of all the assumed [H_hypothesis]s found in the closed
          Sections since its creation. *)
       List.iter
         (fun node_label ->
           let avail_info =
             (try
               List.find
                 (fun a -> a.psa_node_label = node_label) available_steps with
             | Not_found ->
                 raise
                   (Attempt_proof_by_unknown_step
                      (fact.Parsetree.ast_loc, node_label))) in
           Format.fprintf out_fmter "(* For step <%d>%s. *)@\n"
             (fst node_label) (snd node_label);
           Format.fprintf out_fmter "@[<2>Parameter %a :@ "
             Parsetree_utils.pp_vname_with_operators_expanded
             avail_info.psa_lemma_name;
           add_quantifications_and_implications ctx print_ctx env avail_info ;
           (* Now, print the lemma's body. Be careful to enclose it between
              parens to avoid associativity issues. Was bug #62. *)
           Format.fprintf out_fmter "(" ;
           Species_record_type_coq_generation.generate_logical_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular
             env avail_info.psa_base_logical_expr ;
           Format.fprintf out_fmter ")" ;
           (* Done... Then, final carriage return. *)
           Format.fprintf out_fmter ".@]@\n")
         node_labels
   | Parsetree.F_type type_idents ->
       (* Syntax: "by type ...". *)
       List.iter
         (fun type_ident -> zenonify_by_type ctx env type_ident)
         type_idents
;;



let zenonify_hyp ctx print_ctx env hyp =
  let out_fmter = ctx.Context.scc_out_fmter in
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_variable  (vname, type_expr) -> (
       match type_expr.Parsetree.ast_type with
        | Parsetree.ANTI_type ty ->
            (* Notation "assume ... in ...". This leads to a Variable in the
               current Coq Section. *)
            Format.fprintf out_fmter "@[<2>Variable %a :@ %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded vname
              (Coq_pprint.pp_type_simple_to_coq print_ctx) ty
        | _ -> assert false
      )
   | Parsetree.H_hypothesis (vname, logical_expr) ->
       (* Notation "H: all blabla in Self, foo -> bar...". This leads to a
          Variable in the current Coq Section. *)
       Format.fprintf out_fmter "@[<2>Variable %a :@ "
         Parsetree_utils.pp_vname_with_operators_expanded vname;
       Species_record_type_coq_generation.generate_logical_expr
         ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
         ctx env logical_expr ;
       Format.fprintf out_fmter ".@]@\n"
   | Parsetree.H_notation (vname, expr) ->
       (* Leads to a Definition. *)
       Format.fprintf out_fmter "@[<2>Let %a :=@ "
         Parsetree_utils.pp_vname_with_operators_expanded vname;
       Species_record_type_coq_generation.generate_expr
         ctx ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
         env expr ;
       Format.fprintf out_fmter ".@]@\n"
;;


(*
let debug_available_steps steps =
  let rec rec_debug = function
    | [] -> ()
    | h :: q ->
        Format.eprintf "***********@.";
        Format.eprintf "\tNODE: <%d>%s@."
          (fst h.psa_node_label) (snd h.psa_node_label);
        Format.eprintf "\tNAME: %a@." Sourcify.pp_vname h.psa_lemma_name;
        Format.eprintf "\tBASE LOG EXPR: %a@."
          Sourcify.pp_logical_expr h.psa_base_logical_expr;
        List.iter
          (function
            | AH_variable (n, ty) ->
                Format.eprintf "\t\tVARIABLE %a : %a@."
                  Sourcify.pp_vname n Sourcify.pp_type_expr ty
            | AH_lemma _ -> ())
          h.psa_assumed_variables_and_lemmas ;
        List.iter
          (function
            | AH_lemma log_expr ->
                Format.eprintf "\t\tLEMMA %a@."
                  Sourcify.pp_logical_expr log_expr
            | AH_variable (_, _) -> ())
          h.psa_assumed_variables_and_lemmas ;
        rec_debug q in
  Format.eprintf "debug_available_steps START@.";
  rec_debug steps;
  Format.eprintf "debug_available_steps STOP@."
;;
*)



(* ************************************************************************* *)
(** {b Descr} : Just allows the function interfacing to zenon to generate
    their logical statement to prove either by a simple [logical_expr] or
    using the recursive calls information.
    In the first case, we are in a proof whose statement is an existing
    [logical_expr] in the AST.
    In the seconbd case, we are in a termination proof. In effect, in this
    case, there is no *real* [logical_expr] in the AST to express the
    termination property/statement. This property is generated in Coq on the
    fly by the function [Rec_let_coq_gen.generate_termination_lemmas]. Hence in
    this case, functions used to interface with zenon don't have any
    [logical_expr] and must  use [Rec_let_coq_gen.generate_termination_lemmas] to
    dump the required Coq code for Zenon.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
type zenon_statement_coq_generation_method =
  | ZSGM_from_logical_expr of Parsetree.logical_expr
  | ZSGM_from_termination_lemma of
      ((Parsetree.vname * Types.type_simple) list *  (** The parameters of the
           function with their types *appearing in the order or measure
           expression*. Does *not* contains the other parameters! *)
       Rec_let_coq_gen.termination_expr_kind *  (** Expression representing the
          measure or theorder. *)
        int *      (** Index of the recursive function parameter
           applied to the order/measure expression. This index will serve to
           identify which one will have to be passed to the user-order. *)
       Recursion.recursive_calls_description)
;;



(** section_name_seed : the base of name to use if one need to open a fresh
    Section.
    available_hyps : Assoc list mapping any previously seen step onto its
    related logical expression it demonstrates and the name given to this
    lemma.
    Return new available steps to be added to the already known. Does NOT
    return the concatenation of fresh ones and already know. Hence, returns
    and EXTENSION of the steps that must be manually appended !
 *)
let rec zenonify_proof_node ~in_nested_proof ctx print_ctx env min_coq_env
    ~self_manifest dependencies_from_params generated_fields available_hyps
    available_steps section_name_seed parent_proof_opt node default_aim_name
    aim_coq_gen_method =
  let out_fmter = ctx.Context.scc_out_fmter in
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub ((label_num, label_name), stmt, proof) ->
       (begin
       (* Start a new nested Section to prove the statement. *)
       let section_name = section_name_seed ^ "_" ^ label_name in
       Format.fprintf out_fmter "@[<2>Section __%s.@\n" section_name;
       let stmt_desc = stmt.Parsetree.ast_desc in
       (* First, generate the hypotheses of the statement. We also recover the
          "Hypothesis" of the statement because we will need to print them
          again later if they are mentionned as used in a [F_hypothesis] .*)
       List.iter (zenonify_hyp ctx print_ctx env) stmt_desc.Parsetree.s_hyps;
       (* We extend the context of available hypothesis with the new ones. *)
       let available_hyps' = stmt_desc.Parsetree.s_hyps @ available_hyps in
       (* Finally, we deal with the conclusion of the statement. *)
       let new_aim =
         (match stmt_desc.Parsetree.s_concl with
          | None -> (
              match aim_coq_gen_method with
               | ZSGM_from_logical_expr lexpr -> lexpr
               | ZSGM_from_termination_lemma (_, _, _, _) -> assert false
             )
          | Some logical_expr -> logical_expr) in
       (* Now, handle the nested proof of the conclusion of the statement or
          the default one if there is no new aim provided. *)
       let lemma_name = Parsetree.Vlident ("__" ^ section_name ^ "_LEMMA") in
       zenonify_proof
         ~in_nested_proof: true ~qed:false ctx print_ctx env min_coq_env
         ~self_manifest dependencies_from_params generated_fields
         available_hyps' available_steps section_name
         (ZSGM_from_logical_expr new_aim) lemma_name parent_proof_opt proof;
       Format.fprintf out_fmter "End __%s.@]@\n" section_name;
       (* Since we end a Section, all the lemma will have now to be explicitely
          quantified/implified by the assumed variables and hypotheses. *)
       let assumed_variables_and_lemmas =
         find_assumed_variables_and_lemmas_in_hyps stmt_desc.Parsetree.s_hyps in
       (* We return the extra step known thanks to the current [PN_sub] This
          extra step will be made available for the rest of the surrounding
          proof and sibling [PN_sub]/[PN_qed]. *)
       [{ psa_node_label = (label_num, label_name);
          psa_lemma_name = lemma_name;
          psa_base_logical_expr = new_aim;
          psa_assumed_variables_and_lemmas = assumed_variables_and_lemmas }]
       end)
   | Parsetree.PN_qed ((_label_num, _label_name), proof) ->
       zenonify_proof ~in_nested_proof ~qed:true ctx print_ctx env min_coq_env
         ~self_manifest dependencies_from_params generated_fields available_hyps
         available_steps section_name_seed aim_coq_gen_method default_aim_name
         parent_proof_opt proof;
       [(* No new extra step available. *)]




(** Factorize theorem generation for Zenon, with dependencies enforcement. *)
and emit_zenon_theorem_for_proof ~in_nested_proof ctx print_ctx env min_coq_env
    available_hyps aim_coq_gen_method aim_name enforced_deps =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* [Unsure] Bad place to make the check. This should be made in something
     like "abstration.ml". Ensure that the *)
  ensure_enforced_dependencies_by_definition_are_definitions
    min_coq_env enforced_deps ;
  (* Now, print the lemma body. Inside, any method of "Self" is abstracted
     (without lambda-lift) and named "abst_xxx". That's why we use the mode
     [SMS_abstracted]. *)
  Format.fprintf out_fmter "(* Theorem's body. *)@\n";
  (* Be careful if the coq proof appears in a nested proof of a Zenon script
     under a Zenon script then we must not apply the naming scheme
     "for_zenon__xxx". Otherwise, if the coq proof is at toplevel of the Zenon
     proof (i.e. in fact ends the Zenon proof), we must directly give the
     theorem the real name since we do not have to (and we do not) create the
     various intermediate theorems induced by Zenon stuff, especially the fact
     that we open some Sections to "look like" first-order. *)
  let opt_for_zenon = if in_nested_proof then "" else "for_zenon_" in
  Format.fprintf out_fmter "@[<2>Theorem %s%a :@ "
    opt_for_zenon Parsetree_utils.pp_vname_with_operators_expanded aim_name ;
  (* Generate the aim depending on if we are in a regular proof or in the
     initial stage of a termination proof. *)
  (match aim_coq_gen_method with
  | ZSGM_from_logical_expr aim ->
      Species_record_type_coq_generation.generate_logical_expr
        ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
        ~recursive_methods_status:
          Species_record_type_coq_generation.RMS_regular
        ctx env aim
  | ZSGM_from_termination_lemma
        (fun_prms_in_expr, expr, used_param_index, rec_calls) ->
      (* [Unsure] Same code than in [generate_theorem_section_if_by_zenon].
         Need to factorize ? *)
      Rec_let_coq_gen.generate_termination_lemmas
        ctx print_ctx env
        [(* empty generalized vars *)] (* [Unsure] *)
        ~explicit_order: (Rec_let_coq_gen.OK_expr (expr, used_param_index))
        rec_calls ;
      (match expr with
      | Rec_let_coq_gen.TEK_order e ->
          (* Always end by the obligation of well-formation of the user-order
             eta-expanded to wrap its result with a Is_true. *)
          Format.fprintf out_fmter "@ (" ;
          (* Bind the parameters of the function that are used in the order
             expression. *)
          List.iter
            (fun (n, t) ->
              Format.fprintf out_fmter "forall@ %a@ :@ %a,@ "
                Parsetree_utils.pp_vname_with_operators_expanded
                n (Coq_pprint.pp_type_simple_to_coq print_ctx) t)
            fun_prms_in_expr ;
          Format.fprintf out_fmter
            "well_founded@ (fun __a1 __a2 =>@ Is_true@ (" ;
          Species_record_type_coq_generation.generate_expr
            ~local_idents: [] ~in_recursive_let_section_of: []
            ~self_methods_status:
              Species_record_type_coq_generation.SMS_abstracted
            ~recursive_methods_status:
            Species_record_type_coq_generation.RMS_regular
            ctx env e ;
          Format.fprintf out_fmter "@ __a1 __a2)))"
        | Rec_let_coq_gen.TEK_measure e ->
            (* Always end by the obligation of user measure always >= 0,
               eta-expanded to wrap its result with a Is_true. *)
            Format.fprintf out_fmter "(" ;
            (* Same than above for an order, bind the parameters of the
               function. *)
            List.iter
              (fun (n, t) ->
                Format.fprintf out_fmter "forall@ %a@ :@ %a,@ "
                  Parsetree_utils.pp_vname_with_operators_expanded
                  n (Coq_pprint.pp_type_simple_to_coq print_ctx) t)
              fun_prms_in_expr ;
            Format.fprintf out_fmter
              "forall@ __x,@ Is_true@ (basics._lt__equal_@ 0@ (" ;
            Species_record_type_coq_generation.generate_expr
              ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              ctx env e ;
            Format.fprintf out_fmter "@ __x))) "
      )
  ) ;
  Format.fprintf out_fmter ".@]@\n";
  (* Enforce Hypothesis and Variables to be used to prevent Coq from removing
     it. *)
  List.iter
    (fun h ->
      match h.Parsetree.ast_desc with
      | Parsetree.H_notation (_, _) -> ()
      | Parsetree.H_variable (n, _) | Parsetree.H_hypothesis (n, _) ->
          Format.fprintf out_fmter
            "@[<2>assert (__force_use_%a :=@ %a).@]@\n"
            Parsetree_utils.pp_vname_with_operators_expanded n
            Parsetree_utils.pp_vname_with_operators_expanded n)
    available_hyps



and zenonify_proof ~in_nested_proof ~qed ctx print_ctx env min_coq_env
    ~self_manifest dependencies_from_params generated_fields available_hyps
    available_steps section_name_seed aim_coq_gen_method aim_name
    parent_proof_opt proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_dk (enforced_deps, _)
   | Parsetree.Pf_assumed enforced_deps ->
       emit_zenon_theorem_for_proof
         ~in_nested_proof ctx print_ctx env min_coq_env available_hyps
         aim_coq_gen_method aim_name enforced_deps ;
       (* Proof is assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter "(* Proof was flagged as assumed. *)@\n";
       Format.fprintf out_fmter "apply coq_builtins.magic_prove.@\nQed.@\n"
   | Parsetree.Pf_coq (enforced_deps, script) ->
       emit_zenon_theorem_for_proof
         ~in_nested_proof ctx print_ctx env min_coq_env available_hyps
         aim_coq_gen_method aim_name enforced_deps ;
       (* Dump verbatim the Coq code. *)
       (* Was "daube" ^_^. Just a hack until we finish the work with Cath. *)
       if script = "wf_qed" || script = "mf_qed" then (
         (* One must harvest all the available steps at the current level
            and generalize (in the sens of Coq) their related (generated)
            lemma. *)
         List.iter
           (fun step ->
              Format.fprintf out_fmter "generalize %a.@\n"
                Parsetree_utils.pp_vname_with_operators_expanded
                step.psa_lemma_name)
           available_steps ;
         Format.fprintf out_fmter
           "unfold basics.is_well_founded.@\nintros.@\n\
            SplitandAssumption.@\nQed.@\n"
        )
       else Format.fprintf out_fmter "%s@\n" script
   | Parsetree.Pf_node nodes ->
       (* For each successive node, we remember the previously seen **extra**
          steps that will be available for the trailing Qed node. *)
       let rec rec_dump accu_avail_steps = function
         | [] -> ()
         | node :: q ->
             let extra_avail_steps =
               zenonify_proof_node
                 ~in_nested_proof ctx print_ctx env min_coq_env
                 ~self_manifest dependencies_from_params generated_fields
                 available_hyps accu_avail_steps section_name_seed (Some proof)
                 node aim_name aim_coq_gen_method in
             rec_dump
               (* And not not append in the other way otherwise, the newly
                  found steps will be in tail of the list and of we look for
                  a step that has the same name than an older one, we will
                  find the older one and that's wrong ! (Exactly like just
                  above). *)
               (extra_avail_steps @ accu_avail_steps) q in
       rec_dump available_steps nodes
   | Parsetree.Pf_auto facts ->
       (* Generate Zenon's header. *)
       Format.fprintf out_fmter "%%%%begin-auto-proof@\n";
       (* Location is not the while theorem, but its body instead. I think
          this is sufficient *)
       Format.fprintf out_fmter "%%%%location: [%a]@\n"
         Location.pp_location proof.Parsetree.ast_loc;
       Format.fprintf out_fmter "%%%%name: for_zenon_%a@\n@\n"
         Parsetree_utils.pp_vname_with_operators_expanded aim_name;
       Format.fprintf out_fmter
         "@\n@\n(* Methods to use for automated proof. *)@\n";
       (* Now, print Definition and Hypothesis mentionned in the "by" clause
          without using the method generator. This means that one must
          "inline" the Definitions' bodies (in fact, for Hypothesis, since we
          have no body, only declaration, the inlining has no effect since they
          don't have method generator). If the list of facts is empty, this
          implicitly means "by all the previous steps of THIS proof level",
          i.e. by the [PN_sub]s of the closest [Pf_node] hosting us. And the
          closest [Pf_node] hosting us is in our parent proof. *)
       let real_facts =
         (match facts with
          | [] when qed ->
              let parent_proof_nodes =
                (match parent_proof_opt with
                 | None -> None
                 | Some p ->
                     match p.Parsetree.ast_desc with
                      | Parsetree.Pf_node ns -> Some ns
                      | _ -> assert false) in
              (* Make a pseudo list with all the encountered steps (node
                 labels). *)
              [{ Parsetree.ast_loc = proof.Parsetree.ast_loc;
                 Parsetree.ast_desc =
                   Parsetree.F_node (
                     match parent_proof_nodes with
                     | None -> []
                     | Some x -> find_only_PN_subs_in_proof_nodes x);
                 Parsetree.ast_annot = [];
                 Parsetree.ast_type = Parsetree.ANTI_irrelevant }]
          | _ -> facts) in
       List.iter
         (zenonify_fact
            ctx print_ctx env min_coq_env ~self_manifest
            dependencies_from_params generated_fields available_hyps
            available_steps)
         real_facts;
       (* Now, print the lemma body. Inside, any method of "Self" is
          abstracted (without lambda-lift) and named "abst_xxx". That's why we
          use the mode [SMS_abstracted]. *)
       Format.fprintf out_fmter "(* Theorem's body. *)@\n";
       Format.fprintf out_fmter "Theorem for_zenon_%a :@\n"
         Parsetree_utils.pp_vname_with_operators_expanded aim_name;
       (* Generate the aim depending on if we are in a regular proof or in the
          initial stage of a termination proof. *)
       (match aim_coq_gen_method with
        | ZSGM_from_logical_expr aim ->
            Species_record_type_coq_generation.generate_logical_expr
              ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              ctx env aim
        | ZSGM_from_termination_lemma (_, _, _, _) ->
            (* Termination proofs are always initiated by
               generate_theorem_section_if_by_zenon and are never nested.
               So there is no reason to find a nested step/proof dealing with
               termination. *)
            assert false
       ) ;
       Format.fprintf out_fmter ".@\n" ;
       (* End of Zenon stuff. *)
       Format.fprintf out_fmter "%%%%end-auto-proof@\n";
       (* Now, let's print the theorem/lemma and prove it unless we are at
          toplevel (i.e. not in a nested proof). In this last case, this will
          be done directly by [generate_defined_theorem]. *)
       if in_nested_proof then (
         (* In a nested proof, we are always provided a real [logical_expr]
            since the way to generate a statement directly via the recursive
            calls description only arises a the toplevel of a (termination)
            proof. *)
         let aim =
           (match aim_coq_gen_method with
           | ZSGM_from_logical_expr lexpr -> lexpr
           | ZSGM_from_termination_lemma (_, _, _, _) -> assert false) in
         Format.fprintf out_fmter "@[<2>Theorem %a :@ "
           Parsetree_utils.pp_vname_with_operators_expanded aim_name;
         Species_record_type_coq_generation.generate_logical_expr
           ~local_idents: [] ~in_recursive_let_section_of: []
           ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
           ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
           ctx env aim ;
         Format.fprintf out_fmter ".@]@\n" ;
         (* Enforce Hypothesis to be used to prevent Coq from removing it. *)
         List.iter
           (fun h ->
             match h.Parsetree.ast_desc with
              | Parsetree.H_variable (_, _) | Parsetree.H_notation (_, _) -> ()
              | Parsetree.H_hypothesis (hn, _) ->
                  Format.fprintf out_fmter
                    "@[<2>assert (__force_use_%a :=@ %a).@]@\n"
                    Parsetree_utils.pp_vname_with_operators_expanded hn
                    Parsetree_utils.pp_vname_with_operators_expanded hn)
           available_hyps;
         (* Apply Zenon's result to prove the lemma... *)
         Format.fprintf out_fmter "apply for_zenon_%a;@\nauto.@\nQed.@\n"
           Parsetree_utils.pp_vname_with_operators_expanded aim_name
        )
;;



let generate_asserts_for_dependencies out_fmter dependencies_from_params
    min_coq_env used_species_parameter_tys =
  (* Abstract according to the species's parameters carriers the current method
      depends on. *)
  List.iter
    (fun carrier_name ->
      Format.fprintf out_fmter
        "@[<2>assert (__force_use_p_%a_T :=@ _p_%a_T).@]@\n"
        Parsetree_utils.pp_vname_with_operators_expanded carrier_name
        Parsetree_utils.pp_vname_with_operators_expanded carrier_name)
    used_species_parameter_tys;
  (* Abstract according to the species's parameters the current method depends
     on. *)
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             Parsetree.Vuident n in
      (* Each abstracted method is named like "_p_", followed by the
         species parameter name, followed by "_", followed by the method's
         name.
         We don't care here about whether the species parameters is "in" or
         "is". *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      List.iter
        (fun (meth, _) ->
          Format.fprintf out_fmter
            "@[<2>assert (__force_use_%s%a :=@ %s%a).@]@\n"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths_from_param)
    dependencies_from_params;
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  List.iter
    (function (_, meth_dep) ->
      match meth_dep with
      | Env.TypeInformation.MCEM_Defined_carrier _
      | Env.TypeInformation.MCEM_Declared_carrier ->
          Format.fprintf out_fmter
            "@[<2>assert (__force_use_abst_T :=@ abst_T).@]@\n"
      | Env.TypeInformation.MCEM_Defined_computational (_, _, n, _, _, _)
      | Env.TypeInformation.MCEM_Defined_logical (_, n, _)
      | Env.TypeInformation.MCEM_Declared_computational (n, _)
      | Env.TypeInformation.MCEM_Declared_logical (n, _) ->
          Format.fprintf out_fmter
            "@[<2>assert (__force_use_abst_%a :=@ abst_%a).@]@\n"
            Parsetree_utils.pp_vname_with_operators_expanded n
            Parsetree_utils.pp_vname_with_operators_expanded n)
    min_coq_env
;;



(* ************************************************************************* *)
(* {b Descr} : This function generates the Coq Section needed by Zenon ONLY
      if the theorem has to be proved by Zenon. Otherwise, do nothing.
      It first dump Variables, Let and Hypothesis for all the things we
      usually lambda-lift in a regular definition of method. They represent
      thing that were abstracted in the theorem.
      Next, it prints the Zenon header.
      Next, since the current collection carrier mapping is empty, it re-build
      it so that following definitions will map carriers onto Variables
      generated at the previous step.
      Next the theorem's body is printed.
      Finally, we generate definitions required by Zenon for the facts of the
      proof.
      And we end the Section.

   {b Rem} : Not exported outside this module.
      This function generates the Coq Section needed by Zenon ONLY
      if the theorem has to be proved by Zenon. Otherwise, do nothing.       *)
(* ************************************************************************* *)
let generate_theorem_section_if_by_zenon ctx print_ctx env min_coq_env
    ~self_manifest used_species_parameter_tys dependencies_from_params
    generated_fields name logical_expr_or_term_stuff proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Local function that prints the common stuff required by Zenon in case
     of proofs done by [Pf_auto] or [Pf_node]. It prints the opening of the
     main Section and the theorem. *)
  let print_common_prelude_for_zenon () =
    Format.fprintf out_fmter "(* Section for proof of theorem '%a'. *)@\n"
      Parsetree_utils.pp_vname_with_operators_expanded name;
    (* Start the Section. *)
    Format.fprintf out_fmter "@[<2>Section Proof_of_%a.@\n"
      Parsetree_utils.pp_vname_with_operators_expanded name;
    (* We must now dump Variables, Let and Hypothesis for all the things we
       usually lambda-lift in a regular definition of method. This is due to
       the fact that here we still use the Section mechanism. Hence, we do
       the same job than for regular field definition prelude but changing
       abstractions that are performed by extra parameters by Variable, Let
       or Hypothesis. *)
    ignore
      (generate_field_definition_prelude
         ~in_section: true ctx print_ctx env
         [(* Empty list of generalized vars *)] (* [Unsure] *)
         min_coq_env used_species_parameter_tys dependencies_from_params
         generated_fields) in
  (* *********************** *)
  (* Start really the job... *)
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed _ | Parsetree.Pf_coq _ | Parsetree.Pf_dk _ ->
       () (* No Section needed. *)
   | Parsetree.Pf_node _ | Parsetree.Pf_auto _ -> (
       (* Generate the common code for proofs done by Zenon either by [Pf_auto]
          of by [Pf_node]. *)
       print_common_prelude_for_zenon () ;
       (* Get the stuff to add to the current collection carrier mapping to
          make so the type expressions representing some species parameter
          carrier types, will be automatically be mapped onto our freshly
          created extra args. The trailing "_T" will be automatically added
          by the type printing routine.
          In fact, thsi process is already done by the function
          [generate_field_definition_prelude] but we don't remind it. So we need
          to do it again. That's not efficient, but it's not a big deal. *)
       let cc_mapping_extension =
         List.map
           (fun species_param_type_name ->
             let as_string =
               Parsetree_utils.vname_as_string_with_operators_expanded
                 species_param_type_name in
             let param_name =  "_p_" ^ as_string in
             (* Return the stuff to extend the collection_carrier_mapping. *)
             ((ctx.Context.scc_current_unit, as_string),
              (param_name, Types.CCMI_is)))
           used_species_parameter_tys in
       (* Overwrite the [ctx]�and [print_ctx]. *)
       let ctx = { ctx with
         Context.scc_collections_carrier_mapping =
         cc_mapping_extension @ ctx.Context.scc_collections_carrier_mapping } in
       let print_ctx = {
         print_ctx with
           Coq_pprint.cpc_collections_carrier_mapping =
             cc_mapping_extension @
               print_ctx.Coq_pprint.cpc_collections_carrier_mapping } in
       (* Create a unique name seed for Sections of this theorem. *)
       let section_name_seed =
         String.uppercase (Handy.int_to_base_26 (section_gen_sym ())) in
       (* Handle the proof, telling not to print the Theorem's body once the
          auto-proof is ended because this will be done directly by
          [generate_defined_theorem]. *)
       ignore
         (zenonify_proof ~in_nested_proof: false ~qed: true ctx print_ctx env
            min_coq_env ~self_manifest dependencies_from_params generated_fields
            [(* No available hypothesis at the beginning. *)]
            [(* No available steps at the beginning. *)] section_name_seed
            logical_expr_or_term_stuff name
            None (* No parent proof at the beginning. *)
            proof) ;
       (* Now, unfortunately when Coq closes a Section, it only abstracts
          Parameters and Variables really used. Since WE lambda-lift all the
          dependencies, even if they are not used, we must enforce all the
          Parameters and Variables declared in the Section to be used. Hence
          we define a dummy extra Theorem in which we simply put an "assert"
          on each method and species parameter carrier type we depend on to be
          sure that Coq will abstract it. *)
       Format.fprintf out_fmter
         "(* Dummy theorem to enforce Coq abstractions. *)@\n";
       Format.fprintf out_fmter "@[<2>Theorem ";
       Format.fprintf out_fmter "for_zenon_abstracted_%a "
         Parsetree_utils.pp_vname_with_operators_expanded name;
       Format.fprintf out_fmter ":@ ";
       (* Generate the aim depending on if we are in a regular proof or in the
          initial stage of a termination proof. *)
       (match logical_expr_or_term_stuff with
        | ZSGM_from_logical_expr logical_expr ->
            Species_record_type_coq_generation.generate_logical_expr
              ~local_idents: [] ~in_recursive_let_section_of: []
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              ctx env logical_expr
        | ZSGM_from_termination_lemma
            (fun_prms_in_expr, expr, used_param_index, rec_calls) ->
              (* [Unsure] Same code than in [emit_zenon_theorem_for_proof].
                 Need to factorize ? *)
              Rec_let_coq_gen.generate_termination_lemmas
                ctx print_ctx env
                [(* empty generalized vars *)] (* [Unsure] *)
                ~explicit_order:
                  (Rec_let_coq_gen.OK_expr (expr, used_param_index))
                  rec_calls ;
            (match expr with
             | Rec_let_coq_gen.TEK_order e ->
                 (* Always end by the obligation of well-formation of the
                    user-order eta-expanded to wrap its result with a
                    Is_true. *)
                 Format.fprintf out_fmter "@ (" ;
                 (* Bind the parameters of the function that are used in the
                    order expression. *)
                 List.iter
                   (fun (n, t) ->
                     Format.fprintf out_fmter "forall@ %a@ :@ %a,@ "
                       Parsetree_utils.pp_vname_with_operators_expanded
                       n (Coq_pprint.pp_type_simple_to_coq print_ctx) t)
                   fun_prms_in_expr ;
                 Format.fprintf out_fmter
                   "well_founded@ (fun __a1 __a2 =>@ Is_true@ (" ;
                 Species_record_type_coq_generation.generate_expr
                   ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Species_record_type_coq_generation.SMS_abstracted
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular
                   ctx env e ;
                 Format.fprintf out_fmter "@ __a1 __a2)))"
             | Rec_let_coq_gen.TEK_measure e ->
                 (* Always end by the obligation of user measure always >= 0,
                    eta-expanded to wrap its result with a Is_true. *)
                 Format.fprintf out_fmter "(" ;
                 (* Same than above for an order, bind the parameters of the
                    function. *)
                 List.iter
                   (fun (n, t) ->
                     Format.fprintf out_fmter "forall@ %a@ :@ %a,@ "
                       Parsetree_utils.pp_vname_with_operators_expanded
                       n (Coq_pprint.pp_type_simple_to_coq print_ctx) t)
                   fun_prms_in_expr ;
                 Format.fprintf out_fmter
                   "forall@ __x,@ Is_true@ (basics._lt__equal_@ 0@ (" ;
                 Species_record_type_coq_generation.generate_expr
                   ~local_idents: [] ~in_recursive_let_section_of: []
                   ~self_methods_status:
                     Species_record_type_coq_generation.SMS_abstracted
                   ~recursive_methods_status:
                     Species_record_type_coq_generation.RMS_regular
                   ctx env e ;
                 Format.fprintf out_fmter "@ __x))) "
            )
       );
       Format.fprintf out_fmter ".@]@\n";
       (* Now, for each abstracted method we depend on we generate an assert. *)
       generate_asserts_for_dependencies
         out_fmter dependencies_from_params min_coq_env
         used_species_parameter_tys;
       Format.fprintf out_fmter
         "apply for_zenon_%a;@\nauto.@\nQed.@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name;
       (* End the Section. *)
       Format.fprintf out_fmter "End Proof_of_%a.@]@\n@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name
      )
;;




(* ************************************************************************* *)
(* Context.species_compil_context -> Types.coq_print_context ->              *)
(*   Env.CoqGenEnv.t -> min_coq_env_element list ->                          *)
(*     compiled_species_fields list -> Parsetree.qualified_species ->        *)
(*       Parsetree.vname -> Parsetree.logical_expr -> Parsetree.vname list   *)
(** {b Descr} Gererate the Coq code for a theorem defined in the current
    species (i.e. not inherited). In fact, this generates the theorem
    generator for this theorem in this species.
    It returns the list of methods of ourselves we depend on and that were
    abstracted by a "Variable abst_..." in the current Coq theorem's section
    according to the minimal coq environment.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
let generate_defined_theorem ctx print_ctx env min_coq_env ~self_manifest
    used_species_parameter_tys dependencies_from_params generated_fields
    from name logical_expr proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Put an extra newline before the theorem to make some air ! *)
  Format.fprintf out_fmter "@\n(* From species %a. *)@\n"
    Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
  (* Create the Section for Zenon in order to build the temporary theorem that
     will be used with its proof to build the REAL and proved theorem. This
     does the work only if the theorem must be proved by Zenon. Otherwise, it
     does nothing ! *)
  generate_theorem_section_if_by_zenon
    ctx print_ctx env min_coq_env ~self_manifest used_species_parameter_tys
    dependencies_from_params generated_fields name
    (ZSGM_from_logical_expr logical_expr) proof;
  (* Now, generate the real theorem, using the temporarily created and applying
     the proof. *)
  Format.fprintf out_fmter "@[<2>Theorem ";
  Format.fprintf out_fmter "%a "
    Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Generate the prelude of the method, i.e the sequence of parameters and
     their types induced by the various lamda-liftings. *)
  let (abstracted_methods, new_ctx, _) =
    generate_field_definition_prelude
      ~in_section: false ctx print_ctx env
      [(* Empty list of generalized vars *)] (* [Unsure] *)
      min_coq_env
      used_species_parameter_tys dependencies_from_params generated_fields in
  Format.fprintf out_fmter ":@ " ;
  (* Finally, the theorem itself. Inside, any method of "Self" is abstracted
     (i.e. is lambda-lifted), hence named "abst_xxx". That's why we use the
     mode [SMS_abstracted]. *)
  Species_record_type_coq_generation.generate_logical_expr
    ~local_idents: [] ~in_recursive_let_section_of: []
    ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
    ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
    new_ctx env logical_expr ;
  Format.fprintf out_fmter ".@]@\n";
  (* End the proof matter. *)
  (match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed _ | Parsetree.Pf_dk _ ->
       (* Proof assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter "(* Proof was flagged as assumed *)@\n";
       Format.fprintf out_fmter "apply coq_builtins.magic_prove.@\nQed.@\n"
   | Parsetree.Pf_auto _  | Parsetree.Pf_node _ ->
       (* Proof done by Zenon. Apply the temporary theorem. *)
       Format.fprintf out_fmter
         "apply for_zenon_abstracted_%a;@\nauto.@\nQed.@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name
   | Parsetree.Pf_coq (_, script) ->
       (* Dump verbatim the Coq code. *)
       Format.fprintf out_fmter "%s@\n" script);
  abstracted_methods
;;



let generate_theorem ctx print_ctx env min_coq_env used_species_parameter_tys
    ~self_manifest dependencies_from_params generated_fields
   (from, name, logical_expr) proof =
  (* A "theorem" defined in the species leads to a Coq "Theorem". *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_theorem
        ctx print_ctx env min_coq_env ~self_manifest used_species_parameter_tys
        dependencies_from_params generated_fields from name logical_expr
        proof
    else (
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited from species '%a' but not (re)-declared is \
          generated again.@."
          Parsetree_utils.pp_vname_with_operators_expanded name
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
      (* Recover the arguments for abstracted methods of self in the inherited
         generator. *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit
        from.Env.fh_initial_apparition name env
      ) in
  (* Return the names abstracted in the minimal typing environment. *)
  abstracted_methods
;;



(**
    Since this function is called after the function
    [bind_parameters_to_types_from_type_scheme] who was provided a type
    scheme, the optionnal typec in the list are always or the form [Some] !

    {b Rem} : Not exported outside this module. *)
let print_types_as_tuple_if_several print_ctx out_fmter types =
  let rec rec_print = function
    | [] -> assert false
    | [(_, ty)] ->
        (* We force parentheses to prevent any associability problems since
           we don't really print 1 unique type but several arbitrary type
           expressions we want to group as a tuple. *)
        Format.fprintf out_fmter "(%a)"
          (Coq_pprint.pp_type_simple_to_coq print_ctx) ty
    | (_, ty) :: q ->
        (* Same remark than above for parentheses. *)
        Format.fprintf out_fmter "(%a)@ *@ "
          (Coq_pprint.pp_type_simple_to_coq print_ctx) ty ;
        rec_print q in
  match types with
   | [] -> assert false
   | [(_, _)] -> rec_print types
   | _ ->
       Format.fprintf out_fmter "(@[<1>(";
       rec_print types;
       Format.fprintf out_fmter ")%%type@])"
;;



(** {b Descr}: Prints the tuple of function arguments involved in a termination
    order. Each function argument is obtained by a projection of the order
    argument named [arg_name]. The projection is done using builtin Coq
    definitions, depending on the indice of the initial function argument and
    the number of arguments this initial function has.
    The file coq_builtins.v contains a set of pre-defined (and non-exhaustive)
    projection functions allwing to recover individual components of tuples
    between 2 and 6 components. This is mostly to avoid using nested pattern
    matchings to extract components from the tuples passed to the order (and
    that represent the tuplification of all the initial function's arguments).
    In effect, with pattern matching, we don't know how to automate parts of
    the proof termination in Coq. *)
let print_order_args_as_tuple out_fmter ~fun_arity arg_name indices =
  if fun_arity = 1 then Format.fprintf out_fmter "%s" arg_name
  else
    let rec rec_print = function
      | [] -> assert false
      | [last] ->
          if last = 0 then
            Format.fprintf out_fmter
              "(coq_builtins.__tpl_firstprj%d@ " fun_arity
          else
            Format.fprintf out_fmter
              "(coq_builtins.__tpl_lastprj%d@ " (fun_arity - last) ;
          for _i = 1 to fun_arity do Format.fprintf out_fmter "_@ " done ;
          Format.fprintf out_fmter "%s)" arg_name
      | h :: q ->
          if h = 0 then
            Format.fprintf out_fmter
              "(coq_builtins.__tpl_firtprj%d@ " fun_arity
          else Format.fprintf out_fmter
            "(coq_builtins.__tpl_lastprj%d@ " (fun_arity - h) ;
          for _i = 1 to fun_arity do Format.fprintf out_fmter "_@ " done ;
          Format.fprintf out_fmter "%s),@ " arg_name ;
          rec_print q in
    Format.fprintf out_fmter "(" ;
    rec_print indices ;
    Format.fprintf out_fmter ")"
;;



(* ************************************************************************ *)
(* **************** CODE GENERATION FOR RECURSIVE METHODS. **************** *)
(* ************************************************************************ *)


(* ************************************************************************ *)
(** {b Descr}: Dumps the code of the Coq Definition of the "xxx_wforder"
    according to the kind of the provided termination proof. This order
    will appear in the statement of the termination obligation as a part of
    the whole goal. This part will state that this order is well-founded.
    The parameters of this Definition contains the lambda-lifts induced by
    dependencies and two tuples of the original arguments of the recursive
    function. The order will have to be suitable to compare these two
    tuples.

    {b Rem} : For Function.

    {b Exported}: No.                                                       *)
(* ************************************************************************ *)
let generate_termination_order_for_Function ctx print_ctx env name
    fun_params_n_tys generalized_vars ai sorted_deps_from_params
    generated_fields (* Only needed for "prelude". *)
    opt_term_pr =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* The order's name: the function's name + "_wforder". *)
  Format.fprintf out_fmter "@[<2>Definition %a_wforder"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Generate the lambda-lifts for our dependencies. *)
  let (_, ctx, print_ctx) =
    generate_field_definition_prelude
      ~in_section: false ctx print_ctx env generalized_vars
      ai.Env.TypeInformation.ad_min_coq_env
      ai.Env.TypeInformation.ad_used_species_parameter_tys
      sorted_deps_from_params generated_fields in
  (* The 2 arguments of any decent order (i.e. the compared values). *)
  Format.fprintf out_fmter "@ (__x __y :@ " ;
  (* Print the tuple that is the method's arguments' types. *)
  Format.fprintf out_fmter "%a) :@ Prop :=@ "
    (print_types_as_tuple_if_several print_ctx) fun_params_n_tys ;
  (* Now we need to generate the order's body depending on the kind of the used
     termination method. *)
  match opt_term_pr with
   | None ->
       (* No termination proof is done. We will generate by default an assumed
          proof. Anyway, the compiler issued a warning to the user.
          Don't forget to close the whole box of the definition and to go to
          the next line. *)
       Format.fprintf out_fmter "coq_builtins.magic_order@ __x@ __y.@]@\n"
   | Some term_pr ->
       (begin
       match term_pr.Parsetree.ast_desc with
        | Parsetree.TP_structural _ ->
            failwith "TODO: structural1."  (* [Unsure] *)
        | Parsetree.TP_lexicographic _ ->
            failwith "TODO: lexicographic1."  (* [Unsure] *)
        | Parsetree.TP_measure (measure_expr, (used_param, _), _) -> (
            (*  Since < returns a [bool], we must surround the generated
                application expression by a "Is_true" for Coq. *)
            Format.fprintf out_fmter "@[<2>(Is_true@ @[<2>(" ;
            let local_idents =
              [ Parsetree.Vlident "__x"; Parsetree.Vlident "__y" ] in
            (* Compute the index of the recursive function's parameter used
               in the order. *)
            let used_param_index =
              Handy.list_index_of used_param (List.map fst fun_params_n_tys) in
            let fun_arity = List.length fun_params_n_tys in
            (* Generate the first part telling that the second argument applied
               to the measure gives a result always >= 0. *)
            Format.fprintf out_fmter
              "basics._amper__amper_@ \
                @[<2>(basics._lt__equal_@ \
                  0 @[<2>(" ;
            (* Generate the second application of the measure (hence to the
               second argument).
               We must replace in the expression of the measure, all the
               possible apparitions of any parameter of the function by the
               corresponding projection. In effet, we are in the context of
               the order as expected by Function. Hence, the function
               definition receives its arguments as a tuple, not as separate
               parameters. *)
            let measure_expr__y =
              Rec_let_coq_gen.subst_params_by_tuple_projections
                "__y" measure_expr fun_arity fun_params_n_tys in
            Species_record_type_coq_generation.generate_expr
              ctx ~local_idents ~in_recursive_let_section_of: [name]
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              env measure_expr__y ;
            Format.fprintf out_fmter "@ " ;
            print_order_args_as_tuple
              out_fmter ~fun_arity "__y" [used_param_index] ;
            Format.fprintf out_fmter ")@])@]@ " ;
            (* *)
            (* Now apply this expression to the 2 tuples of used arguments
               extracted by calls to builtin extractors depending on their
               indice among the function parameters.
               This generates the application to < on integers (basics._lt_).
               Form is hence:
                 basics._lt_ (abst_"mesfct" ... x) (abst_"mesfct" ... y) *)
            Format.fprintf out_fmter "@[<2>(basics._lt_@ @[<2>(" ;
            (* Generate the first application of the measure (hence to the
               first argument).
               Same substitution process than above, but with the first
               argument. *)
            let measure_expr__x =
              Rec_let_coq_gen.subst_params_by_tuple_projections
                "__x" measure_expr fun_arity fun_params_n_tys in
            Species_record_type_coq_generation.generate_expr
              ctx ~local_idents ~in_recursive_let_section_of: [name]
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              env measure_expr__x ;
            Format.fprintf out_fmter "@ " ;
            print_order_args_as_tuple
              out_fmter ~fun_arity "__x" [used_param_index] ;
            Format.fprintf out_fmter ")@]@ @[<2>(" ;
            (* Generate the second application of the measure (hence to the
               second argument). *)
            Species_record_type_coq_generation.generate_expr
              ctx ~local_idents ~in_recursive_let_section_of: [name]
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              env measure_expr__y ;
            Format.fprintf out_fmter "@ ";
            print_order_args_as_tuple
              out_fmter ~fun_arity "__y" [used_param_index] ;
            Format.fprintf out_fmter ")@]" ;
            (* Close various parentheses and the box of the whole
               "Definition" of the order. *)
            Format.fprintf out_fmter ")@])@])@].@]@\n@\n"
           )
        | Parsetree.TP_order (order_expr, (used_param, _), _) -> (
            (* Generate the Coq translation of the expression of the
               order. Since the order returns a [bool], we must surround the
               generated application  expression by a "Is_true" for Coq. *)
            Format.fprintf out_fmter "@[<2>Is_true (" ;
            let local_idents =
              [ Parsetree.Vlident "__x"; Parsetree.Vlident "__y" ] in
            (* Compute the list of positionnal indices of the recursive
               function's parameters used in the order. *)
            let used_param_index =
              Handy.list_index_of used_param (List.map fst fun_params_n_tys) in
            (* Now apply this expression to the 2 tuples of used arguments
               extracted by calls to builtin extractors depending on their
               indice among the function parameters. *)
            let fun_arity = List.length fun_params_n_tys in
            Species_record_type_coq_generation.generate_expr
              ctx ~local_idents ~in_recursive_let_section_of: [name]
              ~self_methods_status:
                Species_record_type_coq_generation.SMS_abstracted
              ~recursive_methods_status:
                Species_record_type_coq_generation.RMS_regular
              env order_expr ;
            Format.fprintf out_fmter "@ " ;
            print_order_args_as_tuple
              out_fmter ~fun_arity "__x" [used_param_index] ;
            Format.fprintf out_fmter "@ " ;
            print_order_args_as_tuple
              out_fmter ~fun_arity "__y" [used_param_index] ;
            (* Close the parenthese and the box of the "Is_true". *)
            Format.fprintf out_fmter ")@]" ;
            (* Close the box of the whole "Definition" of the order. *)
            Format.fprintf out_fmter ".@]@\n@\n"
           )
       end)
;;



(* ************************************************************************ *)
(** {b Descr}: Generate the ultimate termination proof expected by Function
    for a recursive function whose termination fact is a measure.

    {b Rem} : For Function.

    {b Exported}: No.                                                       *)
(* ************************************************************************ *)
let generate_measure_term_proof_for_Function
    ctx print_ctx env name fun_params_n_tys ai sorted_deps_from_params
    abstracted_methods recursive_calls meas_expr used_param proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Compute the list of positionnal indices of the recursive function's
     parameters used in the order. *)
  let used_param_index =
    Handy.list_index_of used_param (List.map fst fun_params_n_tys) in
  let used_param_ty = List.assoc used_param fun_params_n_tys in
  (* Print to the user the termination obligations. *)
  if (Configuration.get_show_term_obls ()) then
    Rec_let_coq_gen.print_user_termination_obls_for_measure
      name fun_params_n_tys recursive_calls meas_expr used_param_index
      used_param used_param_ty ;
  match proof.Parsetree.ast_desc with
  | Parsetree.Pf_assumed _
  | Parsetree.Pf_dk _ ->
      (* Proof assumed, then simply use "magic_prove". *)
      Format.fprintf out_fmter
        "(* Proof was flagged as assumed. *)@\n";
      Format.fprintf out_fmter
        "apply coq_builtins.magic_prove.@\nQed."
  | Parsetree.Pf_auto _  | Parsetree.Pf_node _ ->
      (* Proof done by Zenon. Apply soldering stuff. Make arithmetic operation
         opaque to prevent Coq from simplifying (by the below 'simpl' tactic)
         them, possibly altering the goal which would not have any more the
         shape we are expecting.
         After having invoked 'simpl', restore arithmetic stuff transparent.
         IMHO, it is useless, bit this does not hurt. *)
      Format.fprintf out_fmter
        "Opaque basics._plus_ basics._dash_ basics._slash_ \
                basics._star_ basics._tilda__tilda_.@\n\
        unfold %a_wforder;simpl.@\n\
        Transparent basics._plus_ basics._dash_ basics._slash_ \
                basics._star_ basics._tilda__tilda_.@\n\
        elim (for_zenon_abstracted_%a@ "
         Parsetree_utils.pp_vname_with_operators_expanded name
         Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* Apply the theorem to its arguments due to lambda-lifts. *)
      Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
        ~only_for_Self_meths: false out_fmter
        ai.Env.TypeInformation.ad_used_species_parameter_tys
        sorted_deps_from_params abstracted_methods ;
      Format.fprintf out_fmter
        ").@\n\
         intro __user_dec1.@\nintro __user_rem_dec_n_wf.@\n\
         (* Separate decreasing obligations and well-foundation. *)@\n" ;
      let nb_rec_calls = List.length recursive_calls in
      (* Repeat nb rec call times... *)
      for _i = 1 to nb_rec_calls do
        Format.fprintf out_fmter
          "split.@ intros.@ apply coq_builtins.andb_intro;@ eauto.@\n"
      done ;
      (* Some verbatim Coq. *)
      Format.fprintf out_fmter
        "(* There, only remains the well-foundation obligation. *)@\n\
         set@ (R := fun x y : basics.int__t =>@\n\
         Is_true@\n\
           (basics._amper__amper_@ (basics._lt__equal_@ 0@ y)@\n\
           (basics._lt_ x y))).@\n\
         @[<2>change@ (well_founded@ (fun@ __c __d@ :@ " ;
      Format.fprintf out_fmter "%a@ =>@ R "
        (print_types_as_tuple_if_several print_ctx) fun_params_n_tys ;
      let fun_arity = List.length fun_params_n_tys in
      (* Apply the user measure on the first projected argument.
         Same substitution mechanism than in
         [generate_termination_order_for_Function]. *)
      let measure_expr__c =
        Rec_let_coq_gen.subst_params_by_tuple_projections
          "__c" meas_expr fun_arity fun_params_n_tys in
      Format.fprintf out_fmter "@[<2>(" ;
      Species_record_type_coq_generation.generate_expr
        ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
        ~recursive_methods_status:
          Species_record_type_coq_generation.RMS_regular
        ctx env measure_expr__c ;
      (* Print a space in case the function has only 1 argument, hence is not
         a tuple, hence is not between parens. Otherwise, all is glued together
         and may raise an "unbound identifier". *)
      Format.fprintf out_fmter "@ " ;
      print_order_args_as_tuple
        out_fmter ~fun_arity "__c" [used_param_index] ;
      Format.fprintf out_fmter ")@ (" ;
      (* Same substitution mechanism than above. *)
      let measure_expr__d =
        Rec_let_coq_gen.subst_params_by_tuple_projections
          "__d" meas_expr fun_arity fun_params_n_tys in
      Species_record_type_coq_generation.generate_expr
        ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
        ~recursive_methods_status:
          Species_record_type_coq_generation.RMS_regular
       ctx env measure_expr__d ;
      (* Again a space as just above, for the same reason. *)
      Format.fprintf out_fmter "@ " ;
      print_order_args_as_tuple
        out_fmter ~fun_arity "__d" [used_param_index] ;
      Format.fprintf out_fmter ")@]))@].@\n" ;
      (* Now copy verbatim the script. This handles the well-foundation proof
         of the order defined on Z by embedding it in N thank's to the proof
         of "is always positive". *)
      Format.fprintf out_fmter "\
        apply@ wf_inverse_image.@\n\
        apply@ wf_incl@ with@ (R2 := (fun x y : Z => 0 <= y /\\ x < y)).@\n\
        unfold inclusion,@ R.@\n\
        unfold basics.int__t,@ basics._amper__amper_,@ \
        basics._lt__equal_,@\n\
        basics._lt_,@ coq_builtins.bi__and_b,@ coq_builtins.bi__int_leq,@ \
        coq_builtins.bi__int_lt.@\n\
        intros x y.@\n\
        elim@ (Z_le_dec@ 0@ y);@ intro;@ elim@ (Z_lt_dec@ x@ y);@ \
        simpl;@ intros;@ intuition.@\n\
        apply@ (Zwf_well_founded@ 0).@\n\
        Qed."
  | Parsetree.Pf_coq (_, script) ->
      (* Dump verbatim the Coq code. *)
      Format.fprintf out_fmter "%s" script
;;



(* ************************************************************************ *)
(** {b Descr}: Generate the ultimate termination proof expected by Function
    for a recursive function whose termination fact is a well-founded
    relation.

    {b Rem} : For Function.

    {b Exported}: No.                                                       *)
(* ************************************************************************ *)
let generate_order_term_proof_for_Function
    ctx print_ctx env name fun_params_n_tys ai sorted_deps_from_params
    abstracted_methods recursive_calls order_expr used_param proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Compute the list of positionnal indices of the recursive function's
     parameters used in the order. *)
  let used_param_index =
    Handy.list_index_of used_param (List.map fst fun_params_n_tys) in
  (* Print to the user the termination obligations. *)
  if (Configuration.get_show_term_obls ()) then
    Rec_let_coq_gen.print_user_termination_obls_for_order
      name fun_params_n_tys recursive_calls order_expr used_param_index ;
  match proof.Parsetree.ast_desc with
  | Parsetree.Pf_assumed _
  | Parsetree.Pf_dk _ ->
      (* Proof assumed, then simply use "magic_prove". *)
      Format.fprintf out_fmter "(* Proof was flagged as assumed. *)@\n" ;
      Format.fprintf out_fmter "apply coq_builtins.magic_prove.@\nQed."
  | Parsetree.Pf_auto _  | Parsetree.Pf_node _ ->
      (* Proof done by Zenon. Apply soldering stuff. *)
      Format.fprintf out_fmter
        "unfold %a_wforder;simpl.@\n\
         elim (for_zenon_abstracted_%a@ "
        Parsetree_utils.pp_vname_with_operators_expanded name
        Parsetree_utils.pp_vname_with_operators_expanded name ;
     (* Apply the theorem to its arguments due to lambda-lifts. *)
     Species_record_type_coq_generation.
       generate_method_lambda_lifted_arguments
         ~only_for_Self_meths: false out_fmter
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         sorted_deps_from_params abstracted_methods ;
     Format.fprintf out_fmter
       ").@\n\
        intro __user_dec1.@\nintro __user_rem_dec_n_wf.@\n" ;
     let nb_rec_calls = List.length recursive_calls in
     (* Repeat nb -1 rec call times... *)
     for i = 2 to nb_rec_calls do
       Format.fprintf out_fmter
         "elim __user_rem_dec_n_wf.@\n\
          clear __user_rem_dec_n_wf.@\n\
          intro __user_dec%d.@\n\
          intro __user_rem_dec_n_wf.@\n" i
      done ;
      (* Repeat nb rec call times... *)
      for _i = 1 to nb_rec_calls do
        Format.fprintf out_fmter "split. auto.@\n"
      done ;
      Format.fprintf out_fmter
        "set (R := (fun __a __b => Is_true (" ;
      (* Same code than generated for the theorem representing the  proof
         obligation as expected by the user. *)
      Species_record_type_coq_generation.generate_expr
        ~local_idents: [] ~in_recursive_let_section_of: []
        ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
        ~recursive_methods_status:
          Species_record_type_coq_generation.RMS_regular
        ctx env order_expr ;
      Format.fprintf out_fmter " __a __b))).@\n" ;
      Format.fprintf out_fmter
        "change@ @[<2>(well_founded (fun@ __c __d@ :@ " ;
      Format.fprintf out_fmter "%a@ =>@ R "
        (print_types_as_tuple_if_several print_ctx) fun_params_n_tys ;
      (* Same arguments than for the xxx_wforder. *)
      let fun_arity = List.length fun_params_n_tys in
      print_order_args_as_tuple
        out_fmter ~fun_arity "__c" [used_param_index] ;
      Format.fprintf out_fmter "@ " ;
      print_order_args_as_tuple
        out_fmter ~fun_arity "__d" [used_param_index] ;
      Format.fprintf out_fmter "))@].@\n" ;
      Format.fprintf out_fmter "apply wf_inverse_image.@\nassumption.@\n" ;
      Format.fprintf out_fmter "Qed.@\n"
  | Parsetree.Pf_coq (_, script) ->
      (* Dump verbatim the Coq code. *)
      Format.fprintf out_fmter "%s" script
;;



(** {b Rem} : For Function. Uses the "fname"_wforder previously generated
    (i.e. the order as Function expects, not the user-order) and uses, for
    this former application, the tuple of all variables / recursive args
    of the function: not only those on which the user-order operates. *)
let generate_termination_proof_for_Function ctx print_ctx env ~self_manifest
    name fun_params_n_tys generalized_vars ai
    sorted_deps_from_params generated_fields (* Only needed for "prelude". *)
    recursive_calls opt_term_pr =
  let out_fmter = ctx.Context.scc_out_fmter in
  (match opt_term_pr with
   | None -> ()
   | Some term_pr -> (
       let (order_expr, used_param, proof) =
         (match term_pr.Parsetree.ast_desc with
         | Parsetree.TP_structural _ ->
             failwith "TODO: structural3."  (* [Unsure] *)
         | Parsetree.TP_lexicographic _ ->
             failwith "TODO: lexicographic3."  (* [Unsure] *)
         | Parsetree.TP_measure (e, (prm, _), pr) ->
             ((Rec_let_coq_gen.TEK_measure e), prm, pr)
         | Parsetree.TP_order  (e, (prm, _), pr) ->
             ((Rec_let_coq_gen.TEK_order e), prm, pr)) in
       (* Compute the index of the recursive function's parameter used in the
          order. *)
       let used_param_index =
         Handy.list_index_of used_param (List.map fst fun_params_n_tys) in
       (* Record the variables of the function appearing in the order
          expression. *)
       let vars_of_order_expr =
         Parsetree_utils.get_free_local_vnames_from_expr_desc
           (match order_expr with
           | Rec_let_coq_gen.TEK_order e | Rec_let_coq_gen.TEK_measure e ->
               e.Parsetree.ast_desc) in
       let params_in_order_expr =
         List.filter
           (fun (n, _) -> List.mem n vars_of_order_expr)
           fun_params_n_tys in
       generate_theorem_section_if_by_zenon
         ctx print_ctx env
         ai.Env.TypeInformation.ad_min_coq_env ~self_manifest
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         sorted_deps_from_params generated_fields name
         (ZSGM_from_termination_lemma
            (params_in_order_expr, order_expr, used_param_index,
             recursive_calls))
         proof
      )) ;
  (* Generate the Variables and Definitions for our dependencies. Since we
      are under a Section, do not lambda-lift. *)
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_field_definition_prelude
      ~in_section: true ctx print_ctx env generalized_vars
      ai.Env.TypeInformation.ad_min_coq_env
      ai.Env.TypeInformation.ad_used_species_parameter_tys
      sorted_deps_from_params generated_fields in
  (* The termination theorem... *)
  Format.fprintf out_fmter "@[<2>Theorem %a_termination"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  Format.fprintf out_fmter "@ :@[@ " ;
  (* Generate the statement of the theorem representing the termination proof
     obligation. We use the "Function"-order there, i.e. the one that
     Function expects and that deals with all the function's parameters, not
     ont the ones involved in recursion decreasing. *)
  let explicit_order =
    Rec_let_coq_gen.OK_wfounded
      (name, ai.Env.TypeInformation.ad_used_species_parameter_tys,
       sorted_deps_from_params, abstracted_methods) in
  Rec_let_coq_gen.generate_termination_lemmas
    new_ctx new_print_ctx env generalized_vars ~explicit_order recursive_calls ;
  (* Always end by the obligation of well-formation of the order as Function
     expects (not the user-order). *)
  Format.fprintf out_fmter "@ (well_founded (%a_wforder"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Apply to generalized vars. *)
   List.iter
     (fun var ->
       Format.fprintf out_fmter "@ %a" Coq_pprint.pp_type_variable_to_coq var)
    generalized_vars ;
  (* Apply the order to its arguments due to lambda-lifts. *)
  Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
     ~only_for_Self_meths: false out_fmter
     ai.Env.TypeInformation.ad_used_species_parameter_tys
     sorted_deps_from_params abstracted_methods ;
  Format.fprintf out_fmter ")).@]@\n@\n";
  (* Now, manage the proof of this termination theorem if some is provided. *)
  (match opt_term_pr with
   | None ->
       Format.fprintf out_fmter "apply coq_builtins.magic_prove.@\nQed."
   | Some term_pr -> (
       match term_pr.Parsetree.ast_desc with
        | Parsetree.TP_structural _ ->
            failwith "TODO: structural2."  (* [Unsure] *)
        | Parsetree.TP_lexicographic _ ->
            failwith "TODO: lexicographic2."  (* [Unsure] *)
        | Parsetree.TP_measure (meas_expr, (used_param, _), proof) ->
            generate_measure_term_proof_for_Function
              ctx new_print_ctx env name fun_params_n_tys ai
              sorted_deps_from_params abstracted_methods recursive_calls
              meas_expr used_param proof
        | Parsetree.TP_order (order_expr, (used_param, _), proof) ->
            generate_order_term_proof_for_Function
              ctx new_print_ctx env name fun_params_n_tys ai
              sorted_deps_from_params abstracted_methods recursive_calls
              order_expr used_param proof
     ));
  Format.fprintf out_fmter "@]@\n@\n" ;
  (abstracted_methods, new_ctx, new_print_ctx)
;;



(** ***************************************************************************
    {b Descr}: Used to generate a recursive function that is not (assumed)
    structural.
    In this case, the function is generated using the Function construct of
    Coq.
    It generates a termination order and a termination proof instead of
    using fake and generic ones.

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let generate_defined_recursive_let_definition_With_Function ctx print_ctx env
    ~self_manifest generated_fields from name params scheme body opt_term_pr
    ai =
  let out_fmter = ctx.Context.scc_out_fmter in
  match body with
   | Parsetree.BB_logical _ ->
       failwith "recursive logical : TODO"  (* [Unsure] *)
   | Parsetree.BB_computational body_expr ->
       let species_name = snd (ctx.Context.scc_current_species) in
       (* Extend the context with the mapping between these recursive
          functions and their extra arguments. Since we are in Coq, we
          need to take care of the logical definitions and of the
          explicite types abstraction management. *)
       let ctx' = {
         ctx with
           Context.scc_lambda_lift_params_mapping =
             [(name,
               Misc_common.make_params_list_from_abstraction_info
                 ~care_logical: true ~care_types: true ai)] } in
       (* Open the "Section" and "Module" for the recursive definition. *)
       Format.fprintf out_fmter "@\n@[<2>Module Termination_%a_namespace.@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name;
       (* We get the function's parameters and their types. This will serve
          at various stage, each time we will need to speak about a parameter.
          For [bind_parameters_to_types_from_type_scheme], not that we do not
          have anymore information about "Self"'s structure... *)
       let (params_with_type, return_ty_opt, generalized_vars) =
         MiscHelpers.bind_parameters_to_types_from_type_scheme
           ~self_manifest: None (Some scheme) params in
       (* Just remove the option that must always be Some since we provided
          a scheme. *)
       let params_with_type =
         List.map
           (fun (n, opt_ty) ->
             match opt_ty with None -> assert false | Some t -> (n, t))
           params_with_type in
       let return_ty =
         match return_ty_opt with None -> assert false | Some t -> t in
       (* Compute the recursive calls information to generate the termination
          proof obligation. *)
       let recursive_calls =
         Recursion.list_recursive_calls
           ~current_unit: ctx.Context.scc_current_unit name params_with_type
           [] body_expr in
       (* ---> Generate the order depending on the kind of proof. *)
       generate_termination_order_for_Function
         ctx' print_ctx env name params_with_type generalized_vars ai
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         generated_fields opt_term_pr ;
       (* ---> Start the Coq "Section" containing the termination theorem as
          expected by Function and the definition of the Coq Function. *)
       Format.fprintf out_fmter "@\n@[<2>Section %a.@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       (* ---> Generate the termination proof. *)
       let (abstracted_methods, new_ctx, new_print_ctx) =
         generate_termination_proof_for_Function ctx' print_ctx env
           ~self_manifest name params_with_type generalized_vars ai
           ai.Env.TypeInformation.ad_dependencies_from_parameters
           generated_fields recursive_calls opt_term_pr in
       (* Generate the recursive uncurryed function. *)
       Format.fprintf out_fmter
         "@[<2>Function %a@ (__arg:@ %a)@ {wf "
         Parsetree_utils.pp_vname_with_operators_expanded name
         (print_types_as_tuple_if_several new_print_ctx) params_with_type ;
       Format.fprintf out_fmter "(%a_wforder@ "
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       (* Apply the order to the polymorphic type variables. *)
       List.iter
         (fun var ->
           Format.fprintf out_fmter "@ %a"
             Coq_pprint.pp_type_variable_to_coq var)
         generalized_vars ;
       (* Apply the order to its arguments due to lambda-lifts. *)
       Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
         ~only_for_Self_meths: false out_fmter
         ai.Env.TypeInformation.ad_used_species_parameter_tys
           ai.Env.TypeInformation.ad_dependencies_from_parameters
         abstracted_methods ;
       Format.fprintf out_fmter ")@ __arg}" ;
       Format.fprintf out_fmter ":@ %a@ :=@\n"
         (Coq_pprint.pp_type_simple_to_coq new_print_ctx) return_ty ;
       (* Unfortunately, we can't simply generate "let (x, y, ..) := __arg"
          because Coq only allows pairs as let-binding pattern. So instead,
          we generate a "match". *)
       Format.fprintf out_fmter "@[<2>match __arg with@\n| (";
       Format.fprintf out_fmter "%a"
         (Handy.pp_generic_separated_list ","
            Parsetree_utils.pp_vname_with_operators_expanded) params ;
       Format.fprintf out_fmter ") =>@\n" ;
       (* We must transform the recursive function's body si that all the
          recursive calls send their arguments as a unique tuple rather than as
          several arguments. This is because we "tuplified" the arguments of
          the recursive function in order to be able to exhibit a lexicographic
          order if needed. *)
       let tuplified_body =
         Rec_let_coq_gen.transform_recursive_calls_args_into_tuple
           new_ctx ~current_unit: ctx.Context.scc_current_unit
           ~local_idents: [] name body_expr in
       (* We specify here that we must not apply recursive calls to the extra
          arguments due to lambda-liftings. *)
       Species_record_type_coq_generation.generate_expr
         new_ctx ~local_idents: [] ~in_recursive_let_section_of: [name]
         ~self_methods_status: Species_record_type_coq_generation.SMS_abstracted
         ~recursive_methods_status: Species_record_type_coq_generation.RMS_regular
         env tuplified_body ;
       (* Print the "end" of the "match" introduced to split the tuple of
          "__arg". *)
       Format.fprintf out_fmter "@\nend.@]@\n";
       Format.fprintf out_fmter "@[<v 2>Proof.@\n";
       (* Enforce "Variables" to be used to prevent Coq from removing it. We
          generate "assert" for this sake. *)
       generate_asserts_for_dependencies
         out_fmter ai.Env.TypeInformation.ad_dependencies_from_parameters
         ai.Env.TypeInformation.ad_min_coq_env
         ai.Env.TypeInformation.ad_used_species_parameter_tys;
       (* Print the proof using the above material. *)
       if opt_term_pr <> None then (
         (* ---> A termination proof was given. Generate the soldering Coq
            script. *)
         let nb_rec_calls = List.length recursive_calls in
         Format.fprintf out_fmter
           "elim %a_termination.@\n\
           intros __for_function_dec1 __for_function_rem_dec_n_wf.@\n"
           Parsetree_utils.pp_vname_with_operators_expanded name ;
         (* Repeat nb rec call - 1 times... *)
         for i = 2 to nb_rec_calls do
           Format.fprintf out_fmter
             "elim __for_function_rem_dec_n_wf.@\n\
              clear __for_function_rem_dec_n_wf.@\n\
              intros __for_function_dec%d __for_function_rem_dec_n_wf.@\n" i
         done ;
         (* Repeat for each recursive call. *)
         let call_num = ref 1 in (* Recursive calls counter. *)
         List.iter
           (fun (_, _) ->
             Format.fprintf out_fmter "split.@\n" ;
             (* Remove stuff due to matching the tuple of args.
                n - 1 intermediate variables and their type due to the tuple
                decomposition because of the pattern matching --> 2 (n - 1)
                intros.
                1 type per function argument --> n
                So: 2 (n - 1) + n = 3 n - 2.
                Then intros induced by the bindings. There are as many as the
                condition-string (i.e. bindings) of the current recursive
                call.
                All this stuff can be globally introduced by a simple intros.
                Then all the binding subgoals can be globally solved by
                a "try-retry-on-error" on the goals generated by the
                elimination of the hypothesis __for_function_dec%d. *)
             Format.fprintf out_fmter
               "intros.@\neapply __for_function_dec%d ; \
                eauto || (apply coq_builtins.EqTrue_is_true; assumption) || \
                (apply coq_builtins.IsTrue_eq_false2; assumption) || \
                (apply coq_builtins.syntactic_equal_refl).@\n" !call_num ;
                incr call_num)
           recursive_calls ;
         (* The finally remaining stuff related to the well-foundation. *)
         Format.fprintf out_fmter
           "(* Remaining well-foundation... *)@\n\
           assumption.@\n"
        )
       else (
         (* No termination proof given. Hence, generate a fake proof. *)
         Format.fprintf out_fmter "apply coq_builtins.magic_prove.@\n"
        );
       (* Close the pretty print box of the "Function". *)
       Format.fprintf out_fmter "Qed.@]@\n";
       (* ---> Generate the curryed version. *)
       Format.fprintf out_fmter
         "@[<2>Definition %a__%a %a :=@ %a (%a).@]@\n"
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name
         (Handy.pp_generic_separated_list " "
            Parsetree_utils.pp_vname_with_operators_expanded) params
         Parsetree_utils.pp_vname_with_operators_expanded name
         (Handy.pp_generic_separated_list ","
            Parsetree_utils.pp_vname_with_operators_expanded) params ;
       (* Finally close the opened "Section"and "Module" . *)
       Format.fprintf out_fmter "End %a.@]@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       Format.fprintf out_fmter "End Termination_%a_namespace.@]@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       (* We must now generate the function applied to its order and
          termination proof and so on... *)
       Format.fprintf out_fmter "@\n@[<2>Definition %a"
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       ignore
         (generate_field_definition_prelude
            ~in_section: false new_ctx new_print_ctx env generalized_vars
            ai.Env.TypeInformation.ad_min_coq_env
            ai.Env.TypeInformation.ad_used_species_parameter_tys
            ai.Env.TypeInformation.ad_dependencies_from_parameters
            generated_fields) ;
       Format.fprintf out_fmter " :=@ " ;
       (* Now, emit the code of the final definition, using the definition
          created in the above Section enclosed by the above namespace.
          Say that we are NOT in a Zenon "by definition of a rec function" in
          order to have the name "Termination_fct_namespace.species__fct"
          instead of "Termination_fct_namespace.fct_equation". *)
       generate_final_recursive_definifion_body_With_Function
         out_fmter ~in_zenon_by_def: false species_name name
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         abstracted_methods generalized_vars ;
       (* Close the pretty print box. *)
       Format.fprintf out_fmter ".@]@\n" ;
       let compiled = {
         Misc_common.cfm_is_logical = false ;
         Misc_common.cfm_from_species = from ;
         Misc_common.cfm_method_name = name ;
         Misc_common.cfm_method_scheme = Env.MTK_computational scheme ;
         Misc_common.cfm_used_species_parameter_tys =
           ai.Env.TypeInformation.ad_used_species_parameter_tys ;
         Misc_common.cfm_raw_dependencies_from_parameters =
           ai.Env.TypeInformation.ad_raw_dependencies_from_params ;
         Misc_common.cfm_dependencies_from_parameters =
           ai.Env.TypeInformation.ad_dependencies_from_parameters ;
         Misc_common.cfm_dependencies_from_parameters_in_type =
           ai.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
         Misc_common.cfm_coq_min_typ_env_names = abstracted_methods } in
       Misc_common.CSF_let_rec [compiled]
;;



(** ***************************************************************************
    {b Descr}: Generates the definition of a recursive function using
    "Fixpoint" instead of "Function". This function is assumed to be
    structurally recursive with its argument [decr_arg_name] decreasing.

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let generate_defined_recursive_let_definition_With_Fixpoint ctx print_ctx env
    generated_fields from name params decr_arg_name proof_loc scheme body ai =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Extend the context with the mapping between these recursive
     functions and their extra arguments. Since we are in Coq, we need to take
     care of the logical definitions and of the explicite types abstraction
     management. *)
  let ctx' = {
    ctx with
      Context.scc_lambda_lift_params_mapping =
        [(name,
          Misc_common.make_params_list_from_abstraction_info
            ~care_logical: true ~care_types: true ai)] } in
  (* Now, generate the prelude of the only method introduced by "let rec". *)
  Format.fprintf out_fmter "@[<2>Fixpoint %a@ "
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  let (generalized_vars, _) = Types.scheme_split scheme in
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_field_definition_prelude
      ~in_section: false ctx' print_ctx env generalized_vars
      ai.Env.TypeInformation.ad_min_coq_env
      ai.Env.TypeInformation.ad_used_species_parameter_tys
      ai.Env.TypeInformation.ad_dependencies_from_parameters generated_fields in
  (* Generate the postlude of the prototype, i.e. non-lifted args with
     their type, the "{ struct xxx }" clause and the return type. Also
     generate the body of the function by the way. *)
  generate_defined_method_proto_postlude
    new_ctx new_print_ctx env ~self_manifest: None params scheme
    (Some (proof_loc, name, decr_arg_name)) (Some body) ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n@\n" ;
  let compiled = {
    Misc_common.cfm_is_logical = false ;
    Misc_common.cfm_from_species = from ;
    Misc_common.cfm_method_name = name ;
    Misc_common.cfm_method_scheme = Env.MTK_computational scheme ;
         Misc_common.cfm_used_species_parameter_tys =
           ai.Env.TypeInformation.ad_used_species_parameter_tys ;
         Misc_common.cfm_raw_dependencies_from_parameters =
           ai.Env.TypeInformation.ad_raw_dependencies_from_params ;
         Misc_common.cfm_dependencies_from_parameters =
           ai.Env.TypeInformation.ad_dependencies_from_parameters ;
         Misc_common.cfm_dependencies_from_parameters_in_type =
           ai.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
         Misc_common.cfm_coq_min_typ_env_names = abstracted_methods } in
       Misc_common.CSF_let_rec [compiled]
;;



(** ***************************************************************************
   {b Descr}: Is in charge to generate the Coq code for recursive functions.
   It deals and differentiate structural and non-structural recursive
   functions.

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let generate_recursive_let_definition ctx print_ctx env ~self_manifest
    fields_abstraction_infos generated_fields l =
  match l with
   | [] ->
       (* A "let", then a fortiori "let rec" construct *)
       (* must at least bind one identifier !          *)
       assert false
   | [(from, name, params, scheme, body, opt_term_pr, _, _)] -> (
       let ai = List.assoc name fields_abstraction_infos in
       (* First of all, only methods defined in the current species must be
          generated. Inherited methods ARE NOT generated again ! *)
       if from.Env.fh_initial_apparition = ctx.Context.scc_current_species
       then (
         match opt_term_pr with
         | None ->
             (* For the moment, if no termination proof is stated, we continue
                using the "Function" scheme until better thing is available. *)
             generate_defined_recursive_let_definition_With_Function
               ctx print_ctx env ~self_manifest generated_fields from name
               params scheme body opt_term_pr ai
         | Some term_pr -> (
             match term_pr.Parsetree.ast_desc with
               | Parsetree.TP_structural decr_arg_name ->
                   generate_defined_recursive_let_definition_With_Fixpoint
                     ctx print_ctx env generated_fields from name params
                     decr_arg_name term_pr.Parsetree.ast_loc scheme body ai
               | Parsetree.TP_lexicographic _
               | Parsetree.TP_measure (_, _, _)
               | Parsetree.TP_order (_, _, _) ->
                   (* For the moment, in other kinds of termination proof, we
                      continue using the "Function" scheme until better thing is
                      available. *)
                   generate_defined_recursive_let_definition_With_Function
                     ctx print_ctx env ~self_manifest generated_fields from name
                     params scheme body opt_term_pr ai
            )
        )
       else (
         (* Just inherited, no code to emit.
            Start by a bit of debug/information if requested. *)
         if Configuration.get_verbose () then
           Format.eprintf
             "Field '%a' inherited from species '%a' but not \
             (re)-declared is not generated again. @."
             Parsetree_utils.pp_vname_with_operators_expanded name
             Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
         (* Recover the arguments for abstracted methods of Self in the
            inherited generator.*)
         let abstracted_methods =
           find_inherited_method_generator_abstractions
             ~current_unit: ctx.Context.scc_current_unit
             from.Env.fh_initial_apparition name env in
         (* Now, build the [compiled_field_memory], even if the method was not
            really generated because it was inherited. *)
         let compiled_field = {
           Misc_common.cfm_is_logical = false ;
           Misc_common.cfm_from_species = from ;
           Misc_common.cfm_method_name = name ;
           Misc_common.cfm_method_scheme = Env.MTK_computational scheme ;
           Misc_common.cfm_used_species_parameter_tys =
             ai.Env.TypeInformation.ad_used_species_parameter_tys ;
           Misc_common.cfm_raw_dependencies_from_parameters =
             ai.Env.TypeInformation.ad_raw_dependencies_from_params ;
           Misc_common.cfm_dependencies_from_parameters =
             ai.Env.TypeInformation.ad_dependencies_from_parameters ;
           Misc_common.cfm_dependencies_from_parameters_in_type =
             ai.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
           Misc_common.cfm_coq_min_typ_env_names = abstracted_methods } in
         Misc_common.CSF_let_rec [compiled_field]
         )
      )
   | (_, name1, _, _, _, _, _, _) :: (_, name2, _, _, _, _, _, _) :: _ ->
       raise (Recursion.MutualRecursion (name1, name2))
;;



(** generated_fields : The list of previous fields of the species that have
    already be generated. Used while generating theorems to know what to apply
        to the methods generators the theorem depends on. *)
let generate_methods ctx print_ctx env ~self_manifest fields_abstraction_infos
    generated_fields = function
  | Env.TypeInformation.SF_sig (from, name, sch) ->
      (* Only declared, hence, no code to generate yet ! *)
      if Configuration.get_verbose () then
        Format.eprintf "Coq code for signature '%a' leads to void code.@."
          Parsetree_utils.pp_vname_with_operators_expanded name;
      let abstraction_info = List.assoc name fields_abstraction_infos in
      (* Nothing very exciting to keep for the collection generator. *)
      let compiled_field = {
        Misc_common.cfm_is_logical = false ;
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = Env.MTK_computational sch ;
        (* Since no code is generated for "sig", no need to get bored with
           species parameters carriers that may appear in the type of the
           "sig". *)
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys ;
        (* Since the "sig " has no code, it can't refer to parameters'
           methods ! *)
        Misc_common.cfm_raw_dependencies_from_parameters = [] ;
        Misc_common.cfm_dependencies_from_parameters = [] ;
        Misc_common.cfm_dependencies_from_parameters_in_type = [] ;
        (* Since the "sig " has no code, it can't refer to some of our
           methods ! *)
        Misc_common.cfm_coq_min_typ_env_names = [] } in
      Misc_common.CSF_sig compiled_field
  | Env.TypeInformation.SF_let (from, name, params, scheme, body, _, _, _) ->
      let abstraction_info = List.assoc name fields_abstraction_infos in
      (* No recursivity, then the method cannot call itself in its body then
         no need to set the [scc_lambda_lift_params_mapping] of the context. *)
      let coq_min_typ_env_names =
        generate_non_recursive_field_binding
          ctx print_ctx env abstraction_info.Env.TypeInformation.ad_min_coq_env
         ~self_manifest
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters
          generated_fields (from, name, params, scheme, body) in
      (* Now, build the [compiled_field_memory], even if the method was not
         really generated because it was inherited. *)
      let compiled_field = {
        Misc_common.cfm_is_logical =
          (match body with
           | Parsetree.BB_logical _ -> true
           | Parsetree.BB_computational _ -> false) ;
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = Env.MTK_computational scheme ;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys ;
        Misc_common.cfm_raw_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_raw_dependencies_from_params ;
        Misc_common.cfm_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters;
        Misc_common.cfm_dependencies_from_parameters_in_type =
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
        Misc_common.cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
      Misc_common.CSF_let compiled_field
  | Env.TypeInformation.SF_let_rec l ->
      generate_recursive_let_definition
        ctx print_ctx env ~self_manifest fields_abstraction_infos
        generated_fields l
  | Env.TypeInformation.SF_theorem  (from, name, _, logical_expr, pr, _) ->
      let abstraction_info = List.assoc name fields_abstraction_infos in
      let coq_min_typ_env_names =
        generate_theorem
          ctx print_ctx env abstraction_info.Env.TypeInformation.ad_min_coq_env
          ~self_manifest
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters
          generated_fields (from, name, logical_expr) pr in
      let compiled_field = {
        Misc_common.cfm_is_logical = true ;
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = Env.MTK_logical logical_expr;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys;
        Misc_common.cfm_raw_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_raw_dependencies_from_params;
        Misc_common.cfm_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters;
        Misc_common.cfm_dependencies_from_parameters_in_type =
         abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
        Misc_common.cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
      Misc_common.CSF_theorem compiled_field
  | Env.TypeInformation.SF_property (from, name, _, lexpr, _) ->
      (* "Property"s are discarded. However we compute their dependencies. *)
      let abstraction_info = List.assoc name fields_abstraction_infos in
      let compiled_field = {
        Misc_common.cfm_is_logical = true ;
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = Env.MTK_logical lexpr;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys ;
        Misc_common.cfm_raw_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_raw_dependencies_from_params;
        Misc_common.cfm_dependencies_from_parameters =
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters ;
        Misc_common.cfm_dependencies_from_parameters_in_type =
          abstraction_info.Env.TypeInformation.ad_dependencies_from_parameters_in_type ;
        Misc_common.cfm_coq_min_typ_env_names = [] } in
      Misc_common.CSF_property compiled_field
;;



(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*  (Types.type_collection *                                                *)
(*    (Types.collection_name * Types.collection_carrier_mapping_info)) list *)
(** {b Descr} : Create the correspondance between the collection type of
    the species definition parameters and the names to be used later during
    the Coq creation of the record type.
    For a species parameter [A is/in ... ], the name that will be used is
    the name of the species parameter + "_T". No need like in OCaml to add
    a stamp because we don't lowercase names. Hence parameters will never
    wear the same name. The trailing "_T" will be automatically added by
    the type printing routine.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping_for_record ~current_unit species_descr =
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, carrier_name), _, _, _, _) ->
          (* Now, build the "collection type" this name will be bound to.
             According to how the "collection type" of parameters are built,
             this will be the couple of the current compilation unit and the
             name of the parameter. *)
          let type_coll = (current_unit, carrier_name) in
          (* And now create the binding... Record that the parameter is a
             "IS" parameter. *)
          (type_coll, (carrier_name, Types.CCMI_is))
      | Env.TypeInformation.SPAR_in (n, type_coll, provenance) ->
          (* Build the name that will represent this "IN" parameter seen from
             Coq. *)
          let param_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't need
             any species expression to annotate this parameter in the Coq type
             expression annotating this parameter in the hosting species
             record type: it will simply be of the type [type_coll]. *)
          (type_coll, (param_name, (Types.CCMI_in provenance))))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(** Utiliser uniquement lorsque l'on g�n�re la repr�sentation de Self dans
    la cr�ation d'un g�n�rateur de collection. Ca permet de changer les noms
    vers lesquels on mappe une carrier en pr�fixant les "IS" par "_p_".
    En effet, le carrier mapping que l'on a � ce moment mappe directement
    sur "Bla_T". Or dans le contexte d'un g�n�rateur de collection, ben on a
    cr�� des param�tres pour abstraire les carriers des "IS" et ces param�tres
    sont la la forme "_p_Bla". Et comme on ne veut pas refaire un carrier
    mapping from scratch, on le patche comme �a. *)
let make_carrier_mapping_using_lambda_lifts lst =
  List.map
    (fun (type_coll, (carrier_name, param_kind)) ->
      (type_coll, ("_p_" ^ carrier_name, param_kind)))
    lst
;;



(* ************************************************************************ *)
(* Env.TypeInformation.species_field list ->                                *)
(*  (Parsetree.vname * Env.method_type_kind) list                           *)
(** {b Descr}: Maps a list of fields to a list of [method_type_kind]. This
    is used to record for each field if its "type" is a ML-like type or
    a logical expression.
    Computational methods will be [MTK_computational] with a [type_scheme].
    Logical methods will be [MTK_logical] with a [logical_expr].

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let make_meths_type_kinds species_fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_property (_, n, _, lexpr, _)
       | Env.TypeInformation.SF_theorem (_, n, _, lexpr, _, _) ->
           (n, (Env.MTK_logical lexpr)) :: accu
       | Env.TypeInformation.SF_sig (_, n, sch)
       | Env.TypeInformation.SF_let (_, n, _, sch, _, _, _, _) ->
           (n, (Env.MTK_computational sch)) :: accu
       | Env.TypeInformation.SF_let_rec l ->
           List.fold_right
             (fun (_, n, _, sch, _, _, _, _) accu' ->
               (n, (Env.MTK_computational sch)) :: accu')
             l accu)
    species_fields
    []
;;



(* ********************************************************************** *)
(* Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->          *)
(*   Env.CoqGenEnv.t                                                      *)
(** {b Descr} : This function extend the coq code generation envionnment
    for a species generation. Because in Coq we need information about
    the number of extra parameters to add to function idents due to the
    fact that in Coq polymorphism is explicit, we need to make methods of
    a species known before generating its body. It's the same problem for
    the species's parameters that must be bound in the environment, in
    order to inductively known their methods.
    This function adds all this information in the current environment
    and returns the extended environment.
    Note that because in FoCaL methods are not polymorphic, the number
    of extra parameters due to polymorphism is trivially always 0.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let extend_env_for_species_def ~current_species env species_descr =
  (* We first add the species methods. Because methods are not polymorphic, we
     can safely bind them to 0 extra parameters-induced-by-polymorphism. *)
  let species_methods_names =
    Dep_analysis.ordered_names_list_of_fields
      species_descr.Env.TypeInformation.spe_sig_methods in
  let env_with_methods_as_values =
    List.fold_left
      (fun accu_env (m_name, _) ->
        (* Methods are trivially not toplevel bound idents. *)
        Env.CoqGenEnv.add_value
          ~toplevel: None m_name (0, Env.CoqGenInformation.VB_non_toplevel)
          accu_env)
      env
      species_methods_names in
  (* Now, add the species's parameters in the environment. And do not
     [fold_right] otherwise species will be inserted in reverse order in the
     environment ! *)
  List.fold_left
    (fun accu_env species_param ->
      match species_param with
       | Env.TypeInformation.SPAR_in (_, _, _) ->
           (* "IN" parameters are not species. They are "values" of species,
              "instances". Hence they do not lead to any species in the
              environment. *)
           accu_env
       | Env.TypeInformation.SPAR_is ((_, param_name), _, param_methods, _,_) ->
           let methods_n_kinds = make_meths_type_kinds param_methods in
           (* A "IS" parameter is a collection. Hence it is fully instanciated
              and doesn't have anymore lifted extra parameters. Then the
              built [method_info] is trivially empty about
              [mi_dependencies_from_parameters] and [mi_abstracted_methods]. *)
           let bound_methods =
             List.map
               (fun (n, tk) -> {
                 Env.mi_name = n;
                 Env.mi_history = {
                   Env.fh_initial_apparition = current_species;
                   Env.fh_inherited_along = [] };
                 Env.mi_type_kind = tk;
                 Env.mi_used_species_parameter_tys = [];
                 Env.mi_dependencies_from_parameters = [];
                 Env.mi_dependencies_from_parameters_in_type = [];
                 Env.mi_abstracted_methods = [] })
               methods_n_kinds in
           (* Because species names are capitalized, we explicitely build a
              [Parsetree.Vuident] to wrap the species name string.
              Since we don't need any collection generator information, we
              simply build the species binding in the environment by just
              putting None inside.
              In the same way, a parameter never has itself parameters.
              Hence the list of parameters is trivially empty. *)
           Env.CoqGenEnv.add_species
             ~loc: Location.none (Parsetree.Vuident param_name)
             ([], bound_methods, None, Env.COS_species) accu_env)
    env_with_methods_as_values
    species_descr.Env.TypeInformation.spe_sig_params
;;




(* ************************************************************************** *)
(* Format.formatter -> Misc_common.compiled_species_fields list ->
   (Parsetree.vname * Env.ordered_methods_from_params) list                   *)
(** {b Descr} : Dumps as Coq code the parameters required to the collection
    generator in order to make them bound in the collection generator's body.
    These parameters come from the methods of the species parameters that some
    of our methods depend on. This means that a closed species with no species
    parameters will have NO extra parameters in its collection generator.

    This function must UNIQUELY find the names of all the extra parameters the
    methods will need to make them arguments of the collection generator and
    record then in a precise order that must be made public for the guys who
    want to instanciate the collection.

    Attention, we do not hunt these names in the remapped dependencies since
    these latter may have forgotten some effective dependencies in the
    methods at this species level. Read full explainations in the function
    [dump_collection_coq_generator_arguments_for_params_methods] of file
    "species_ml_coq_generation.ml".

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let dump_collection_coq_generator_arguments_for_params_methods out_fmter
    compiled_species_fields =
  (* Let's create an assoc list mapping for each species paramater name the
     set of methods names from it that needed to be lambda-lifted, hence that
     will lead to parameters of the collection generator.
     For sake of efficiency, the list is built in reverse order. We will
     put it back in the right order just after it is finished. *)
  let revd_species_params_and_methods = ref [] in
  (* ************************************************************************ *)
  (** {b Descr} :  Local function to process only one [compiled_field_memory].
      Handy to factorize the code in both the cases of [CSF_let] and
      [CSF_let_rec]. This function effectivly accumulates by side effect for
      each species parameter the set of methods we depend on.

      {b Rem} : Local to the enclosing [dump_collection_coq_generator_arguments]
      function. Not exported.                                                 *)
  (* ************************************************************************ *)
  let process_one_field_memory field_memory =
    List.iter
      (fun (spe_param, (Env.ODFP_methods_list meths_set)) ->
        match spe_param with
         | Env.TypeInformation.SPAR_in (_, _, _) ->
             (* Attention, as previously said, "IN" parameters are handled
                among the species parameters CARRIERS extra arguments. So we
                now skip them to prevent having them twice ! *)
             ()
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             (* Recover the species parameter's name. *)
             let spe_param_name = Parsetree.Vuident n in
             (* Create for this species parameter name, the bucket recording
                all the methods someone depends on.
                We don't care here about whether the species parameters is
                "IN" or "IS". *)
             let spe_param_bucket =
               (try
                 Handy.list_assoc_custom_eq
                   (fun sp n ->
                     (Env.TypeInformation.vname_of_species_param sp) = n)
                   spe_param_name !revd_species_params_and_methods
               with Not_found ->
                 let bucket = ref [] in
                 revd_species_params_and_methods :=
                   (spe_param, bucket) :: !revd_species_params_and_methods;
                 bucket) in
             (* And now, union the current methods we depend on with the
                already previously recorded. *)
             spe_param_bucket := meths_set @ !spe_param_bucket)
      (* Use **non-remapped** dependencies !!! See explaination in header !!! *)
      field_memory.Misc_common.cfm_raw_dependencies_from_parameters in

  (* ********************************************************** *)
  (* Now, really work, building by side effect for each species *)
  (* parameter the set of methods we depend on.                 *)
  List.iter
    (function
      | Misc_common.CSF_sig field_memory
      | Misc_common.CSF_property field_memory
      | Misc_common.CSF_theorem field_memory
      | Misc_common.CSF_let field_memory ->
          process_one_field_memory field_memory
      | Misc_common.CSF_let_rec l -> List.iter process_one_field_memory l)
    compiled_species_fields ;
  (* Reverse the list, remove the inner ref and transform into a set. *)
  let species_params_and_methods_no_ref =
    List.map
      (fun (a, b) -> (a, (Parsetree_utils.list_to_param_dep_set !b)))
      (List.rev !revd_species_params_and_methods) in
  (* We now order the methods of the parameters in accordance with their
     dependencies. *)
  let ordered_species_params_names_and_methods =
    Dep_analysis.order_species_params_methods
      species_params_and_methods_no_ref in
  (* Now we get the assoc list complete, we can dump the parameters of the
     collection generator. To make them correct with their usage inside the
     local functions of the collection generator, we must give them a name
     shaped in the same way, i.e:
     "_p_" + species parameter name + "_" + called method name. *)
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_set)) ->
      let species_param_name =
        Env.TypeInformation.vname_of_species_param species_param in
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
        "_" in
      List.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths_set)
  ordered_species_params_names_and_methods;
  (* Finally, make this parameters information public by returning it. By the
     way, the ref on the inner set is not anymore needed, then remove it. *)
  List.map
    (fun (species_param, meths) ->
       ((Env.TypeInformation.vname_of_species_param species_param), meths))
    ordered_species_params_names_and_methods
;;



(* ************************************************************************** *)
(* Context.species_compil_context -> Format.formatter -> Parsetree.vname list *)
(** {b Descr} : Remind the arguments needed to apply the mk_record to the IS
    species parameters carriers representation that .
    For "IN" parameters, they are also abstracted. We speak of the parameter
    itself, not of its type (that is only abstracted if it is based on a
    species parameter, but that's another story, check function
    [dump_collection_coq_generator_arguments_for_params_methods] for the detail).
    Returns the list of the parameters names to later make them public in
    order to know what must be applied to the collection generator. This list

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let remind_collection_coq_generator_arguments_for_params_carriers ctx =
  (* The species parameters carrier types in reverse order for efficiency. *)
  let params_carriers_abstr_for_record =
    List.map
      (fun  ((_, param_ty_coll), (param_name, param_kind)) ->
        match param_kind with
         | Types.CCMI_is ->
             (* "IS" parameters are capitalized vnames. *)
             (Env.ScopeInformation.SPK_is, (Parsetree.Vuident param_ty_coll))
         | Types.CCMI_in _ -> (
             (* We generate the parameter (that's not its TYPE !). *)
             (Env.ScopeInformation.SPK_in, (Parsetree.Vlident param_name))
            ))
      ctx.Context.scc_collections_carrier_mapping in
  params_carriers_abstr_for_record
;;



(** {b Descr} : Really prints the code for arguments computed by
    [remind_collection_coq_generator_arguments_for_params_carriers]. *)
let dump_collection_coq_generator_arguments_for_params_carriers out_fmter lst =
  List.iter
    (fun (param_kind, param_name) ->
      match param_kind with
       | Env.ScopeInformation.SPK_is ->
           (* We need to enforce the type for Coq because I don't know what. *)
           Format.fprintf out_fmter "@ (_p_%a_T : Set)"
             Parsetree_utils.pp_vname_with_operators_expanded param_name;
       | Env.ScopeInformation.SPK_in ->
           (* One must use the parameter of the collection generator to
              abstract the "IN" parameter. This parameter is losely named by
              the dependency process as: name of the "in"  parameter twice,
              separated by "_". *)
           Format.fprintf out_fmter "@ _p_%a_%a"
             Parsetree_utils.pp_vname_with_operators_expanded param_name
             Parsetree_utils.pp_vname_with_operators_expanded param_name)
    lst
;;






let build_collection_coq_generator_arguments_for_params_methods out_fmter
    abstracted_params_methods_in_record_type =
  List.iter
    (fun (species_param_name, (Env.ODFP_methods_list meths)) ->
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      List.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths)
    abstracted_params_methods_in_record_type
;;




let generate_collection_coq_generator ctx env compiled_species_fields
    abstracted_params_methods_in_record_type =
  let current_species_name = snd ctx.Context.scc_current_species in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "@\nSpecies %a is fully defined. Generating its collection generator@."
      Sourcify.pp_vname current_species_name ;


  (* *********************************************************************** *)
  (** {b Descr} : A local function to process one field. This allows to
      factorize the processing for both [Let] and [Let_rec] [Property] and
      [Theorem]�definitions.

      {b Rem} : Local to the [generate_collection_coq_generator] function.
               Not exported.                                                 *)
  (* *********************************************************************** *)
  let process_one_field field_memory =
    let from = field_memory.Misc_common.cfm_from_species in
    Format.fprintf out_fmter "(* From species %a. *)@\n"
      Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
    Format.fprintf out_fmter "@[<2>let local_%a :=@ "
      Parsetree_utils.pp_vname_with_operators_expanded
      field_memory.Misc_common.cfm_method_name;
    if Configuration.get_verbose () then
      Format.eprintf "Generating Coq code for collection generator for '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this
       inheritance level or if it was inherited from another species. *)
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then (
      if Configuration.get_verbose () then
        Format.eprintf
          "Method '%a' not inherited, building collection generator using \
          abstracted local species parameters as arguments.@."
          Sourcify.pp_vname field_memory.Misc_common.cfm_method_name;
      (* It comes from the current inheritance level. Then its name is simply
         the method's name. *)
      Format.fprintf out_fmter "%a"
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments induced
         by the various lambda-lifting we previously performed.
         First, the species parameters carriers we used.
         Next, the extra arguments due to the species parameters methods we
         depends on and entity parameters. Here we will not use them to
         lambda-lift them this time, but to apply them !
         The name used for application is formed according to the same scheme
         we used at lambda-lifting time:
           "_p_" + species parameter name + "_" + called method name. *)
      Species_record_type_coq_generation.generate_method_lambda_lifted_arguments
        ~only_for_Self_meths: false out_fmter
        field_memory.Misc_common.cfm_used_species_parameter_tys
        field_memory.Misc_common.cfm_dependencies_from_parameters
        []  (* Methods of Self handled homogeneously just after. *)
     )
    else (
      (* It comes from a previous inheritance level. Then its name is the
         module where the species inhabits if not the same compilation unit
         than the current + "." + species name as module + "." + the method's
         name. *)
      let (defined_from_mod, defined_from_species) =
        from.Env.fh_initial_apparition in
      if Configuration.get_verbose () then
        Format.eprintf
          "Method '%a' inherited, from '%s#%a'.@."
          Sourcify.pp_vname field_memory.Misc_common.cfm_method_name
          defined_from_mod Sourcify.pp_vname defined_from_species ;
      if defined_from_mod <> ctx.Context.scc_current_unit then
        Format.fprintf out_fmter "%s." defined_from_mod ;
      Format.fprintf out_fmter "%a.%a"
        Parsetree_utils.pp_vname_with_operators_expanded defined_from_species
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments induced
         by the various lambda-lifting we previously did about
         collection / entity parameters in the species from which we inherit,
         i.e. where the method was defined.
         During the inheritance, parameters have been instanciated. We must
         track these instanciations to know to what apply the method
         generator. *)
      instanciate_parameters_through_inheritance ctx env field_memory
     );
    (* Now, continue applying the method generator to each of the extra
       arguments induced by the various lambda-lifting we previously performed.
       Here is the second part: methods of our inheritance tree we depend on
       and that were only declared.
       These methods leaded to "local" functions forced to be defined above
       because of well-ordering (in accordance to dependencies on methods of
       Self). Hence, for each method only declared of ourselves we depend on,
       its name is "local_" + the method's name. *)
    List.iter
      (fun n ->
        Format.fprintf out_fmter "@ local_%a"
          Parsetree_utils.pp_vname_with_operators_expanded n)
      field_memory.Misc_common.cfm_coq_min_typ_env_names;
    (* That's all for this field code generation. *)
    Format.fprintf out_fmter "@ in@]@\n";
    if Configuration.get_verbose () then
      Format.eprintf "End of Coq code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name in

  (* *********************** *)
  (* Now, let's really work. *)
  (* A little comment in the generated Coq code. *)
  Format.fprintf out_fmter
    "@\n(* Fully defined '%a' species's collection generator. *)@\n"
    Sourcify.pp_vname current_species_name;
  (* The generic name of the collection generator: the species' name +
     "_collection_create". *)
  Format.fprintf out_fmter "@[<2>Definition collection_create";
  (* The collection generator first arguments are those corresponding to the
     IS species parameters carriers, hence to the record type parameters.
     All of them are in the [collection_carrier_mapping] of the current
     compilation context. We remind the list of the parameters names by the
     way to later make them public in order to know what must be applied to
     the collection generator. *)
  let params_carriers_abstr_for_record =
    remind_collection_coq_generator_arguments_for_params_carriers ctx in
  (* Now, we dump them to make them parameters of the collection generator. *)
  dump_collection_coq_generator_arguments_for_params_carriers
    out_fmter params_carriers_abstr_for_record;
  (* Generate the parameters the collection generator needs to build each of
     the current species's local function (functions corresponding to the
     actual method stored in the collection record).
     These parameters of the generator come from the abstraction of methods
     coming from our collection parameters we depend on. By the way, recover the
     list of species parameters linked together with their methods we need to
     instanciate in order to apply the collection generator. *)
  let abstr_params_methods_in_coll_coq_gen =
    dump_collection_coq_generator_arguments_for_params_methods
      out_fmter compiled_species_fields in
  Format.fprintf out_fmter " :=@ ";
  (* Generate the local functions that will be used to fill the record value. *)
  List.iter
    (function
      | Misc_common.CSF_sig field_memory ->
          (* We handle "rep" apart. *)
          if field_memory.Misc_common.cfm_method_name =
             Parsetree.Vlident "rep" then (
            let sch =
              (match field_memory.Misc_common.cfm_method_scheme with
               | Env.MTK_computational s -> s | _ -> assert false) in
             let type_from_scheme = Types.specialize sch in
             let print_ctx = {
               Coq_pprint.cpc_current_unit = ctx.Context.scc_current_unit;
               Coq_pprint.cpc_current_species =
               Some
                 (Parsetree_utils.type_coll_from_qualified_species
                    ctx.Context.scc_current_species);
               Coq_pprint.cpc_collections_carrier_mapping =
                 (* Prefix all the "IS" mappings by "_p_" to use the parameters
                    declared in the collection generator's header. *)
                 make_carrier_mapping_using_lambda_lifts
                   ctx.Context.scc_collections_carrier_mapping } in
             Format.fprintf out_fmter "@[<2>let local_rep :=@ %a in@]@\n"
               (Coq_pprint.pp_type_simple_to_coq print_ctx) type_from_scheme
           )
          else process_one_field field_memory
      | Misc_common.CSF_property field_memory
      | Misc_common.CSF_theorem field_memory
      | Misc_common.CSF_let field_memory -> process_one_field field_memory
      | Misc_common.CSF_let_rec l ->
          List.iter (fun fm -> process_one_field fm) l)
    compiled_species_fields ;
  (* Now, apply the record type constructor. *)
  Format.fprintf out_fmter "mk_record" ;
  (* The "mk_record" first arguments are those corresponding to the IS species
     parameters carriers. They we already computed when we created the
     "mk_record". So we just now need to apply then since they are
     parameters (with the same names) of the collection generator we are
     building. *)
  dump_collection_coq_generator_arguments_for_params_carriers
    out_fmter params_carriers_abstr_for_record ;
  (* Now, print the same names than the parameters that must be provided when
     using the collection generator to represent methods of the collection
     parameters that are abstracted and used by the local "local_xxx" (these
     latter used to create the collection generator). *)
  build_collection_coq_generator_arguments_for_params_methods
    out_fmter abstracted_params_methods_in_record_type ;
  (* Then, always the "local_rep" since the first record field represents what
     is to be the "future collection" carrier (foo_T :> Set.). *)
  Format.fprintf out_fmter "@ local_rep";
  (* No need to generate the local functions that will be used to fill the
     record value since in Coq we always generate them. It's already done ! *)
  (* And now, the record value. *)
  List.iter
    (function
      | Misc_common.CSF_let field_memory
      | Misc_common.CSF_theorem field_memory ->
          Format.fprintf ctx.Context.scc_out_fmter "@ local_%a"
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.Misc_common.cfm_method_name
      | Misc_common.CSF_let_rec l ->
          List.iter
            (fun field_memory ->
              Format.fprintf ctx.Context.scc_out_fmter "@ local_%a"
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.Misc_common.cfm_method_name)
            l
      | Misc_common.CSF_sig field_memory ->
          (* In a fully defined species, no sig should remain. The only
             exception is "rep" that is **defined** when it appears as a
             "sig". *)
          if field_memory.Misc_common.cfm_method_name <>
            Parsetree.Vlident "rep" then assert false
      | Misc_common.CSF_property _ ->
          (* In a fully defined species, no property should remain. *)
          assert false)
    compiled_species_fields;
  (* Close the pretty-print box of the "Let collection_create ... :=". *)
  Format.fprintf ctx.Context.scc_out_fmter ".@]@\n";
  ((* Parameters induced by parameters carriers and used to instanciate the
      parameters of "mk_record"...*)
   params_carriers_abstr_for_record,
   (* Arguments of the collection generator that correspond the the species's
      parameters methods we depend on. *)
   abstr_params_methods_in_coll_coq_gen)
;;




let species_compile env ~current_unit out_fmter species_def species_descr
    dep_graph fields_abstraction_infos =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the chapter encapsulating the species representation. *)
  let module_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>Module %s.@\n" module_name;
  (* Insert in the environment the value bindings of the species methods and
     the species bindings for its parameters. This is needed since in Coq
     polymorphism is explicit, hence we need to know for each method the extra
     arguments it needs. *)
  let env' =
    extend_env_for_species_def
      ~current_species: (current_unit, species_name) env species_descr in
  (* Create the initial compilation context for this species. *)
  let ctxt_no_ccmap = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, species_name) ;
    Context.scc_dependency_graph_nodes = dep_graph;
    Context.scc_species_parameters_names =
      species_descr.Env.TypeInformation.spe_sig_params ;
    Context.scc_collections_carrier_mapping = [] ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* Now, establish the mapping between collections available and the names
     representing their carrier for the record type. *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping_for_record ~current_unit species_descr in
  let ctxt_ccmap =
    { ctxt_no_ccmap with
        Context.scc_collections_carrier_mapping =
          collections_carrier_mapping } in
  (* If the species is complete, the record type representing its "type".
     We get the parameters the record type has. If the species is not complete,
     then don't generate and anyway, we won't use
     [abstracted_params_methods_in_record_type] (its usages are guarded by the
     same test of species completness). *)
  let abstracted_params_methods_in_record_type =
    if species_descr.Env.TypeInformation.spe_is_closed then
      Species_record_type_coq_generation.generate_record_type
        ctxt_ccmap env' species_descr fields_abstraction_infos
    else [] in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Coq_pprint.cpc_current_unit = ctxt_no_ccmap.Context.scc_current_unit;
    Coq_pprint.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctxt_no_ccmap.Context.scc_current_species);
    Coq_pprint.cpc_collections_carrier_mapping = [] } in
  (* Because we sometimes need to bind function parameters to their types
     with the function
     [MiscHelpers.bind_parameters_to_types_from_type_scheme], we must
     beforehand know is [Self] is manifest or not. I.e. if there is a
     signature called "representation". *)
  let self_manifest =
    Misc_common.find_self_representation_if_some
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* Now, generate the Coq code of the methods. *)
  let compiled_fields =
    List.fold_left
      (fun accu field ->
        (* Pass the accu to be able to remind the already generated fields. *)
        let compiled_field =
          generate_methods
            ctxt_no_ccmap print_ctx env' ~self_manifest
            fields_abstraction_infos accu field in
        (* Not efficient, but required to keep the fields in the right order. *)
        accu @ [compiled_field])
      []
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* Now build the list of the species parameters names to make them public in
     the future ml generation environnment. *)
  let species_params_names_n_kinds =
    List.map
      (fun (pname, pkind) ->
        match pkind.Parsetree.ast_desc with
         | Parsetree.SPT_in _ -> (pname, Env.ScopeInformation.SPK_in)
         | Parsetree.SPT_is _ -> (pname, Env.ScopeInformation.SPK_is))
      species_def_desc.Parsetree.sd_params in
  (* Now check if the species supports a collection generator because fully
     defined and get the information about which arguments to pass in order to
     later call the collection generator. *)
  let extra_args_from_spe_params =
    if species_descr.Env.TypeInformation.spe_is_closed then (
      (* Explicitely use the context having the collection carrier mapping that
         contains the species parameters carriers !
         It will be used to remind the arguments to pass to the record type
         (i.e. to mk_record).
         The obtained list describes extra parameters due to species parameters.
         First, the parameters induced by parameters carriers abstracted the
         record type (useful to build a complete record type expression).
         Note that in this first stuff, the carriers of the "IN" parameters
         are here !
         Second, we get the abstracted methods from parameters we depend on.
         These methods parametrize the collection generator and will have to
         be provided when creating a collection. *)
      let (
        (* Parameters induced by parameters' carriers abstracted in the record
           type. They will be used to instanciate to use "mk_record" or create a
           valid record type expression. *)
        params_carriers_abstr_for_record,
        (* Only methods of the params required for the collection generator
           application. They correspond to the parameters methods that have
           been abstracted inside the collection generator. Inside, there are
           missing the carrier abstractions of the parameters. But they can be
           recovered with the above [params_carriers_abstr_for_record]. *)
        abstr_params_methods_in_coll_coq_gen) =
        generate_collection_coq_generator
          ctxt_ccmap env' compiled_fields
          abstracted_params_methods_in_record_type in
      (* From this, we must remove parameters whose methods list is empty.
         In fact, they correspond to entity parameters. Since to generate
         the code, we use the [print_methods_from_params_instanciations] (via
         [make_collection_effective_record] and since this function takes
         care of entity parameters (for the collection generator arguments),
         we don't want extra parameters added to the record accesses while
         building a collection (in effect, this info is used only for this). *)
      let abstracted_params_methods_in_record_type' =
        List.fold_right
          (fun (pname, Env.ODFP_methods_list m) accu ->
            if m = [] then accu else (pname, Env.ODFP_methods_list m) :: accu)
          abstracted_params_methods_in_record_type [] in
      let coll_coq_gen_params_info = {
        Env.CoqGenInformation.cgp_abstr_param_carriers_for_record =
        (* Just remove the "IN"/"IS" tag that is not needed to keep in the code
           generation environment. *)
          List.map snd params_carriers_abstr_for_record;
        Env.CoqGenInformation.cgp_abstr_param_methods_for_record =
          abstracted_params_methods_in_record_type';
        Env.CoqGenInformation.cgp_abstr_param_methods_for_coll_gen =
          abstr_params_methods_in_coll_coq_gen } in
      Some
        { Env.CoqGenInformation.cgi_implemented_species_params_names =
            species_params_names_n_kinds;
          Env.CoqGenInformation.cgi_generator_parameters =
            coll_coq_gen_params_info }
     )
    else None in
  (* The end of the module hosting the species. *)
  Format.fprintf out_fmter "@]\nEnd %s.@\n@\n" module_name;
  (* Now, extract the fields names to create the [species_binding_info]. *)
  let species_binding_info =
    List.flatten
      (List.map
         (function
           | Misc_common.CSF_sig compiled_field_memory
           | Misc_common.CSF_let compiled_field_memory
           | Misc_common.CSF_theorem compiled_field_memory ->
               [{ Env.mi_name =
                    compiled_field_memory.Misc_common.cfm_method_name;
                  Env.mi_history =
                    compiled_field_memory.Misc_common.cfm_from_species;
                  Env.mi_type_kind =
                    compiled_field_memory.Misc_common.cfm_method_scheme;
                  Env.mi_used_species_parameter_tys =
                    compiled_field_memory.Misc_common.
                      cfm_used_species_parameter_tys;
                  Env.mi_dependencies_from_parameters =
                    compiled_field_memory.Misc_common.
                      cfm_dependencies_from_parameters;
                  Env.mi_dependencies_from_parameters_in_type =
                    compiled_field_memory.Misc_common.
                      cfm_dependencies_from_parameters_in_type ;
                  Env.mi_abstracted_methods =
                    compiled_field_memory.Misc_common.
                      cfm_coq_min_typ_env_names }]
           | Misc_common.CSF_let_rec compiled_field_memories ->
               List.map
                 (fun cfm ->
                   { Env.mi_name = cfm.Misc_common.cfm_method_name;
                     Env.mi_history = cfm.Misc_common.cfm_from_species;
                     Env.mi_type_kind = cfm.Misc_common.cfm_method_scheme;
                     Env.mi_used_species_parameter_tys =
                       cfm.Misc_common.cfm_used_species_parameter_tys;
                     Env.mi_dependencies_from_parameters =
                       cfm.Misc_common.cfm_dependencies_from_parameters;
                     Env.mi_dependencies_from_parameters_in_type =
                       cfm.Misc_common.cfm_dependencies_from_parameters_in_type ;
                     Env.mi_abstracted_methods =
                       cfm.Misc_common.cfm_coq_min_typ_env_names })
                 compiled_field_memories
           | Misc_common.CSF_property compiled_field_memory ->
               [ { Env.mi_name =
                     compiled_field_memory.Misc_common.cfm_method_name;
                   Env.mi_history =
                     compiled_field_memory.Misc_common.cfm_from_species;
                   Env.mi_type_kind =
                     compiled_field_memory.Misc_common.cfm_method_scheme;
                   Env.mi_used_species_parameter_tys =
                     compiled_field_memory.Misc_common.
                       cfm_used_species_parameter_tys;
                   Env.mi_dependencies_from_parameters =
                     compiled_field_memory.Misc_common.
                       cfm_dependencies_from_parameters;
                   Env.mi_dependencies_from_parameters_in_type =
                     compiled_field_memory.Misc_common.
                       cfm_dependencies_from_parameters_in_type ;
                   (* For properties, this list should always be [] since we do
                      not compute the visible universe since it is never
                      used. *)
                   Env.mi_abstracted_methods =
                     compiled_field_memory.Misc_common.
                       cfm_coq_min_typ_env_names }])
         compiled_fields) in
  (species_descr.Env.TypeInformation.spe_sig_params,
   species_binding_info, extra_args_from_spe_params, Env.COS_species)
;;



(* ************************************************************************ *)
(** {b Descr} : Prints the list of effective arguments used to instanciate
    the formal representing species parameters carriers abstracted in a
    record type.
    This function is used twice: when creating the "__implemented" in a
    collection, and when generating each projection borrowing fields of the
    "implemented" record type to inject it into the collection record type.

    {b Rem}: Not exported outside this module.                              *)
(* ************************************************************************ *)
let print_implemented_species_for_coq ~current_unit out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another Coq "file-module". *)
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit, then
          again no need to find the species's module in another Coq
          "file-module" otherwise we explicitely prefix by the module name
          corresponding to the filename. *)
       if fname <> current_unit then
         Format.fprintf out_fmter "%s." fname;
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
;;



(* ************************************************************************** *)
(** {b Descr} Type used just to encode the result computed once for all of by
    what a species IS parameter's carrier or a IN parameter is instanciated.
    In effect, one not only need to remind a species/collection name, but
    also, for IN parameters, an expression.

    Remember that IN parameters are included in the record type parameters
    as coming from the parameters and not from the methods of parameters we
    depend on. That's the reason why we sometimes skip IN parameters
    dependencies when on lambda-lift stuff by walking the dependencies from
    species parameters' methods.

    Since we use the instanciation several time, instead of computing it each
    time we remind it via this structure and we print it each time we need
    with the function [print_record_type_carriers_args_instanciations] .

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
type record_type_arg_for_carrier_instanciation =
  | RTAI_by_is of ((Types.fname option) * Parsetree.vname)
  | RTAI_by_in of Parsetree.expr
;;



(** {b Descr} : Prints the instanciation of parameters' carriers that were
    abstracted. *)
let print_record_type_carriers_args_instanciations ctx env args_instanciations =
  let out_fmter = ctx.Context.scc_out_fmter in
  List.iter
    (function
      | RTAI_by_is (corresponding_effective_opt_fname,
                    corresponding_effective_vname) ->
          Format.fprintf out_fmter "@ ";
          (match corresponding_effective_opt_fname with
           | Some fname -> Format.fprintf out_fmter "%s." fname
           | None -> ()) ;
          Format.fprintf out_fmter "%a.me_as_carrier"
            Parsetree_utils.pp_vname_with_operators_expanded
            corresponding_effective_vname
      | RTAI_by_in expr ->
          Format.fprintf out_fmter "@ ";
          Species_record_type_coq_generation.generate_expr
            ctx ~local_idents: [] ~in_recursive_let_section_of: []
            ~self_methods_status: Species_record_type_coq_generation.SMS_from_record
            ~recursive_methods_status:
              Species_record_type_coq_generation.RMS_regular
            env expr)
    args_instanciations
;;




(** {b Descr} : Prints the instanciation of parameters' methods that were
    abstracted. *)
let print_methods_from_params_instanciations ctx env formal_to_effective_map l =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Species parameters we have dependencies on. *)
  List.iter
    (fun (formal_species_param_name, (Env.ODFP_methods_list method_names)) ->
      match List.assoc formal_species_param_name formal_to_effective_map with
       | Misc_common.CEA_collection_name_for_is corresponding_effective -> (
           let
               (corresponding_effective_opt_fname,
                corresponding_effective_vname) =
             match corresponding_effective with
             | Parsetree.Vname n -> (None, n)
             | Parsetree.Qualified (m, n) -> ((Some m), n) in
           List.iter
             (fun (meth_name, _) ->
               (* If needed, qualify the name of the species in the Coq code.
                  Don't print the type to prevent being too verbose. *)
               Format.fprintf out_fmter "@ ";
               (match corresponding_effective_opt_fname with
               | Some fname -> Format.fprintf out_fmter "%s." fname
               | None -> ()) ;
               (* Species name.method name. *)
               Format.fprintf out_fmter "%a.%a"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname
                 Parsetree_utils.pp_vname_with_operators_expanded meth_name)
             method_names
          )
       | Misc_common.CEA_value_expr_for_in expr -> (
           Format.fprintf out_fmter "@ (@[<1>";
           (* No local idents in the context because we just enter the scope
              of a species fields and so we are not under a core expression.
              For [~self_as], same thing, no relevant value since the
              application of the generator should not involve any other
              expressions than methods/theorems identifiers. *)
           Species_record_type_coq_generation.generate_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status:
               (* Or what you prefer. *)
               Species_record_type_coq_generation.SMS_abstracted
             ~recursive_methods_status:
               Species_record_type_coq_generation.RMS_regular
             env expr ;
           Format.fprintf out_fmter ")@]"
          ))
    l
;;



let apply_collection_coq_generator_to_parameters ctx env formal_to_effective_map
    col_coq_gen_info =
  let col_coq_gen_params_info =
    col_coq_gen_info.Env.CoqGenInformation.cgi_generator_parameters in
  (* Now, generate the argment identifier or expression for each expected
     collection generator parameter. *)
  (* First, start by generating identifiers for species parameters carriers.
     In fact, since this will be used again to make projections when the final
     collection record value, we compute the list of what need to be printed
     to keep is under the hand (we will return it for further usages). Then,
     we really print with a routine that will also be used later. *)
  let record_type_args_instanciations =
    List.map
      (fun param_name ->
       match List.assoc param_name formal_to_effective_map with
        | Misc_common.CEA_collection_name_for_is corresponding_effective -> (
            match corresponding_effective with
            | Parsetree.Vname n -> RTAI_by_is (None, n)
            | Parsetree.Qualified (m, n) -> RTAI_by_is ((Some m), n)
           )
        | Misc_common.CEA_value_expr_for_in expr -> RTAI_by_in expr)
      col_coq_gen_params_info.Env.CoqGenInformation.
        cgp_abstr_param_carriers_for_record in
  (* Since in the record type we always abstract first by the species
     parameters carriers, so it is in the collection generator. So we directly
     use this fact to apply the generator to the instanciations of the species
     parameters carriers. *)
  print_record_type_carriers_args_instanciations
    ctx env record_type_args_instanciations ;
  (* Now, we generate identifiers for methods of these species parameters we
     have dependencies on. *)
  print_methods_from_params_instanciations
    ctx env formal_to_effective_map
    col_coq_gen_params_info.Env.CoqGenInformation.
      cgp_abstr_param_methods_for_coll_gen ;
  record_type_args_instanciations
;;



(* ************************************************************************** *)
(* current_unit: Types.fname -> Format.formatter -> Parsetree.ident -> unit   *)
(** {b Descr} : Helper that prints a species name as a Coq module, with
    module qualification if needed.
    In other words, each time we need to refer to a module qualification
    induced by a species, this function prints the the name of the species,
    prefixed by its hosting file considered as an OCaml module if this
    species is not in the current compilation unit.
    For example, imagine we are in the "foo.foc" file and we need to speak
    of a record field of a species "S" that lives in the "bar.foc" file.
    Then because each FoCaL compilation unit is mapped onto an Coq file
    (hence an Coq module corresponding to the file-as-module), it will be
    printed like "bar.S". If the species "S" was in the same compilation unit
    (i.e. "foo.foc"), then it would be printed directly "S".

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let print_implemented_species_as_coq_module ~current_unit out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another Coq "file-module". *)
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit,  *)
       (* then again no need to find the species's module in another Coq *)
       (* "file-module" otherwise we explicitely prefix by the module    *)
       (* name corresponding to the filename.                            *)
       if fname <> current_unit then Format.fprintf out_fmter "%s." fname;
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
;;



(** Generate the definition representing the carrier of a collection. Must
    be the first "method" of the generated collection code. Similar to its
    OCaml counterpart. *)
let generate_rep_definition ctx fields =
  let rec rec_search = function
    | [] -> ()
    | h :: q -> (
        match h with
        | Env.TypeInformation.SF_sig (_, n, sch) ->
            (* Check if the sig is "rep". *)
            if (Parsetree_utils.name_of_vname n) = "rep" then (
              Format.fprintf ctx.Context.scc_out_fmter
                "(* Carrier's structure explicitly given by \"rep\". *)@\n" ;
              Format.fprintf ctx.Context.scc_out_fmter
                "@[<2>Definition me_as_carrier@ " ;
              (* Print the variables names... *)
              List.iter
                (function (_, (h, _)) ->
                  Format.fprintf ctx.Context.scc_out_fmter "(%s : Set) " h)
                ctx.Context.scc_collections_carrier_mapping ;
              let print_ctx = {
                Coq_pprint.cpc_current_unit = ctx.Context.scc_current_unit ;
                Coq_pprint.cpc_current_species =
                  Some
                    (Parsetree_utils.type_coll_from_qualified_species
                       ctx.Context.scc_current_species) ;
                Coq_pprint.cpc_collections_carrier_mapping =
                ctx.Context.scc_collections_carrier_mapping } in
              (* Now, output the type's name and body. *)
              let ty = Types.specialize sch in
              Format.fprintf ctx.Context.scc_out_fmter ":=@ %a.@]@\n"
                (Coq_pprint.pp_type_simple_to_coq print_ctx) ty
             )
            else rec_search q
        | _ -> rec_search q
       ) in
  rec_search fields
;;



(* ********************************************************************** *)
(** {b Descr} : Creates the effective methods of the collection's.
    The methods are borrowed from every fields from the temporary value
    ("__implemented") generated by the collection generator.
    In order to select a field of the "__implemented", i.e. to perform a
    projection on the "__implemented" record type, we must remember the
    effective types parametrising this type.
    Form instance:

      species Foo0 (A0 is Sp0) = ...
;;
      species Coll implements Foo0 (Csp0)
;;

    leads to the following Coq code:

    {[
      Record Foo0 (A0_T : Set) : Type :=
        mk_Foo0 {
        Foo0_T :> Set;
        (* From species collgen_for_coq#Foo0. *)
        Foo0_v : basics.int__t
        }.
      ...
      Module Coll =
        Definition me_as_carrier := ...
        (* From species collgen_for_coq#Foo0. *)
        Definition v := ...
       End.
     ]}

    To create the record value for Coll, we must borrow the field "v"
    from Foo0, but since the type Foo0 is parametrised (by A0_T),
    projections must be done applying each time the effective type used
    as argument in the "implements" clause.
    I.e.:

      Definition Coll_effective_collection :=
        mk_Coll
          self_T
          __implemented.(Foo0_v Csp0_effective_collection.(Csp0_T)).

    or shorter:

      Definition Coll_effective_collection :=
        mk_Coll
          self_T
          __implemented.(Foo0_v Csp0_effective_collection).

    OR EVEN SHORTER: Coq is smart enough to guess the types provided that
    we give the right number of implicit parameters (_). So, now, instead
    of making all this proessing, we generate some _'s.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let make_collection_effective_methods ctx implemented_species_name
    collection_descr formals_to_effectives record_type_args_instanciations
    record_type_args_instanciations2 =
  let out_fmter = ctx.Context.scc_out_fmter in
  let current_unit = ctx.Context.scc_current_unit in
  (* Local function to print as many _ as the record type has arguments.
     Before, we printed the exact expressions that were used as arguments
     of the record type to make projections. *)
  let print_underscores l =
    List.iter
      (fun (formal_species_param_name, (Env.ODFP_methods_list method_names)) ->
      match List.assoc formal_species_param_name formals_to_effectives with
       | Misc_common.CEA_collection_name_for_is _ ->
           List.iter
             (fun _ -> Format.fprintf ctx.Context.scc_out_fmter "@ _")
             method_names
       | Misc_common.CEA_value_expr_for_in _ ->
           Format.fprintf ctx.Context.scc_out_fmter "@ _")
    l in
  (* First, the definition of the carrier. *)
  generate_rep_definition
    ctx collection_descr.Env.TypeInformation.spe_sig_methods ;
  List.iter
    (function
      | Env.TypeInformation.SF_sig (_, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _, _) -> ()
      | Env.TypeInformation.SF_theorem (_, n, _, _, _, _)
      | Env.TypeInformation.SF_let (_, n, _, _, _, _, _, _) ->
          Format.fprintf out_fmter
            "@[<2>Definition %a :=@ effective_collection.@[<1>("
            Parsetree_utils.pp_vname_with_operators_expanded n ;
          print_implemented_species_as_coq_module
            ~current_unit out_fmter implemented_species_name;
          Format.fprintf out_fmter ".rf_%a"
            Parsetree_utils.pp_vname_with_operators_expanded n ;
          (* Apply to the instanciations of the parameters carriers. *)
          List.iter (fun _ -> Format.fprintf out_fmter "@ _")
            record_type_args_instanciations ;
          (* Apply to the instanciations of the parameters methods we depend
             on. *)
          print_underscores record_type_args_instanciations2 ;
          Format.fprintf out_fmter ").@]@]@\n"
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, n, _, _, _, _, _, _) ->
              Format.fprintf out_fmter
                "@[<2>Definition %a :=@ effective_collection.@[<1>("
                Parsetree_utils.pp_vname_with_operators_expanded n ;
              print_implemented_species_as_coq_module
                ~current_unit out_fmter implemented_species_name;
              Format.fprintf out_fmter ".rf_%a"
                Parsetree_utils.pp_vname_with_operators_expanded n ;
              (* Apply to the instanciations of the parameters carriers. *)
              List.iter (fun _ -> Format.fprintf out_fmter "@ _")
                record_type_args_instanciations ;
              (* Apply to the instanciations of the parameters methods we
                 depend on. *)
              print_underscores record_type_args_instanciations2 ;
              Format.fprintf out_fmter ").@]@]@\n")
            l)
    collection_descr.Env.TypeInformation.spe_sig_methods
;;



(* ************************************************************************ *)
(* current_unit: Parsetree.module_name ->                                   *)
(*   Misc_common.collection_effective_arguments list ->                     *)
(*     Env.CoqGenInformation.collection_coq_generator_info ->                   *)
(*      (Parsetree.vname * Misc_common.collection_effective_arguments) list *)
(* ************************************************************************ *)
let map_formal_to_effective_in_collection ~current_unit collection_body_params
    col_coq_gen_params_info =
  try
    (* Create the assoc list mapping the formal to the effective parameters. *)
    List.map2
      (fun formal_info effective_info ->
        match (formal_info, effective_info) with
         | ((formal, Env.ScopeInformation.SPK_is),
            Misc_common.CEA_collection_name_for_is qualified_vname) ->
              (begin
              (* "Is" parameter. Leads to collection name based stuff. *)
              match qualified_vname with
               | Parsetree.Vname _ ->
                   (* Assumed to be local to the current unit. *)
                   (formal,
                    Misc_common.CEA_collection_name_for_is qualified_vname)
               | Parsetree.Qualified (effective_fname, effective_vname) ->
                   (* If the species belongs to the current unit, then we don't
                      need to qualify it in the OCaml generated code. Then we
                      simply discard its explicit hosting information. *)
                   if effective_fname = current_unit then
                     (formal,
                      Misc_common.CEA_collection_name_for_is
                        (Parsetree.Vname effective_vname))
                   else
                     (formal,
                      Misc_common.CEA_collection_name_for_is
                        (Parsetree.Qualified
                           (effective_fname, effective_vname)))
              end)
         | ((formal, Env.ScopeInformation.SPK_in),
            (Misc_common.CEA_value_expr_for_in effective_expr)) ->
              (begin
              (* "In" parameter. Leads to direct value based stuff. *)
              (formal, (Misc_common.CEA_value_expr_for_in effective_expr))
              end)
         | (_, _) ->
             (* This would mean that we try to apply an effective stuff
                IN/IS-incompatible with the kind of the species parameter.
                This should have been caught before by the analyses ! *)
             assert false)
      col_coq_gen_params_info.Env.CoqGenInformation.
      cgi_implemented_species_params_names
      collection_body_params
  with Invalid_argument "List.map2" ->
    assert false  (* The lists length must be equal. *)
;;



(* ************************************************************************* *)
(* current_unit: Parsetree.module_name ->                                    *)
(*   Parsetree.ident_desc Parsetree.ast ->                                   *)
(*    (Parsetree.vname * Misc_common.collection_effective_arguments) list -> *)
(*      Env.generic_code_coq_gen_method_info list ->                             *)
(*        Env.generic_code_coq_gen_method_info list                              *)
(** {b Descr}: Replace in a collection methods the formal parameters
    occurences by the effective parameters provided to create the collection.
    Hence, future users of the collection will not see anymore occurrences
    of the parameters but effective collections used to instanciate them.
    This allows to fix the bug report #187.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
let substitute_formal_by_effective_in_coll_meths ~current_unit
    implemented_species_name form_to_effec meths =
  (* We must first find out in which file the implemented species is hosted.
     In effect, its formal parameters will have a [collection_type] with a
     module name that will be this file, not mandatorily the current
     compilation unit. *)
  let formal_parameters_mod_name =
    (match implemented_species_name.Parsetree.ast_desc with
     | Parsetree.I_local _ | Parsetree.I_global (Parsetree.Vname _) ->
         current_unit
     | Parsetree.I_global (Parsetree.Qualified (mod_name, _)) -> mod_name) in
  (* Now, in each method... *)
  List.map
    (fun meth_info ->
      (* ... apply the substitution of each formal parameter by the effective
         one. *)
      List.fold_left
        (fun accu_mi (param_vname, effective) ->
          match effective with
           | Misc_common.CEA_value_expr_for_in expr ->
               (begin
               match accu_mi.Env.mi_type_kind with
                | Env.MTK_computational _ -> accu_mi
                | Env.MTK_logical lexpr ->
                    (* [Unsure] *)
                    let lexpr' =
                      SubstExpr.subst_prop
                        ~param_unit: formal_parameters_mod_name param_vname
                        expr.Parsetree.ast_desc lexpr in
                    { accu_mi with Env.mi_type_kind = Env.MTK_logical lexpr' }
               end)
           | Misc_common.CEA_collection_name_for_is qcoll -> (
               let param_as_type_coll =
                 (formal_parameters_mod_name,
                  (Parsetree_utils.name_of_vname param_vname)) in
               let effective_as_type_coll =
                 (match qcoll with
                  | Parsetree.Vname vn ->
                      (current_unit, (Parsetree_utils.name_of_vname vn))
                  | Parsetree.Qualified (modname, vn) ->
                      (modname, (Parsetree_utils.name_of_vname vn))) in
               match accu_mi.Env.mi_type_kind with
                | Env.MTK_computational sch ->
                    let sch' =
                      Types.subst_type_scheme
                        param_as_type_coll
                        (Types.SBRCK_coll effective_as_type_coll) sch in
                    { accu_mi with Env.mi_type_kind =
                        Env.MTK_computational sch' }
                | Env.MTK_logical lexpr ->
                    let lexpr' =
                      SubstColl.subst_logical_expr
                        ~current_unit
                        (SubstColl.SRCK_coll param_as_type_coll)
                        (Types.SBRCK_coll effective_as_type_coll)
                        lexpr in
                    { accu_mi with Env.mi_type_kind = Env.MTK_logical lexpr' }
              ))
        meth_info
        form_to_effec)
    meths
;;



let collection_compile env ~current_unit out_fmter collection_def
    collection_descr dep_graph =
  let collection_name = collection_def.Parsetree.ast_desc.Parsetree.cd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for collection %a@."
      Sourcify.pp_vname collection_name ;
  (* Start the "Module" encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>Module %a.@\n"
    Sourcify.pp_vname collection_name;
  (* Now, establish the mapping between collections available and the names
     representing their carrier. *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping_for_record
      ~current_unit collection_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit;
    Context.scc_current_species = (current_unit, collection_name);
    Context.scc_dependency_graph_nodes = dep_graph;
    (* A collection never has parameter. *)
    Context.scc_species_parameters_names = [] ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* We do not want any collection generator. Instead, we will call the
     collection generator of the collection we implement and apply it to the
     functions it needs coming from the collection applied to its parameters
     if there are some. *)
  Format.fprintf out_fmter "@[<2>Let effective_collection :=@ ";
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    collection_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  (* We call the "implemented" collection generator, that is named by the
     implemented species name + ".collection_create". *)
  print_implemented_species_for_coq
    ~current_unit out_fmter implemented_species_name;
  Format.fprintf out_fmter ".collection_create";
  (* Finally, we must recover the arguments to apply to this collection
     generator. These arguments of course come from the species parameters the
     closed species we implement has (if it has some). We must make this
     application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE RIGHT ORDER ! *)
  (try
    let (_, implemented_species_methods, opt_params_info, _) =
      Env.CoqGenEnv.find_species
        ~loc: collection_def.Parsetree.ast_loc ~current_unit
        implemented_species_name env in
    let substituted_implemented_methods =
      (match opt_params_info with
       | None ->
           (* The species has no collection generator. Hence it is not a fully
              defined species. This should have be prevented before, by
              forbidding to make a collection from a non fully defined
              species ! *)
           assert false (* [Unsure] car je crois qu'on n'a pas fait la v�rif. *)
       | Some params_info ->
           (* Get the names of the collections or the value expressions
              effectively applied. *)
           let collection_body_params =
             Misc_common.get_implements_effectives
               collection_def.Parsetree.ast_desc.
                 Parsetree.cd_body.Parsetree.ast_desc.Parsetree.se_params
               params_info.Env.CoqGenInformation.
                 cgi_implemented_species_params_names in
           let formals_to_effectives =
             map_formal_to_effective_in_collection
               ~current_unit: ctx.Context.scc_current_unit
               collection_body_params params_info in
           let record_type_args_instanciations =
             apply_collection_coq_generator_to_parameters
               ctx env formals_to_effectives params_info in
           (* Close the pretty print box of the "effective_collection". *)
           Format.fprintf out_fmter ".@]@\n" ;
           (* And now, create the final value representing the effective
              instance of our collection, borrowing each field from the
              temporary value obtained above. This way, our collection will have
              ITS own record fields names, preventing the need to use those
              coming from the it implements. *)
           make_collection_effective_methods
             ctx implemented_species_name collection_descr formals_to_effectives
             record_type_args_instanciations
             params_info.Env.CoqGenInformation.cgi_generator_parameters.
               Env.CoqGenInformation.cgp_abstr_param_methods_for_record ;
           substitute_formal_by_effective_in_coll_meths
             ~current_unit: ctx.Context.scc_current_unit
             implemented_species_name
             formals_to_effectives implemented_species_methods) in
    (* End of the pretty print box of the Module embedding the collection. *)
    Format.fprintf out_fmter "@]\nEnd %a.@\n@\n"
      Sourcify.pp_vname collection_name;
    (* We now return the methods this collection has in order to put this
       information in the environment. The collections has the same methods
       than the species it implements. Note that in these methods, formal
       parameters have been replaced by effective arguments. *)
    substituted_implemented_methods
  with Not_found ->
    (* Don't see why the species could not be present in the environment. The
       only case would be to make a collection from a collection since
       collection are never entered in the environment because it's a non sense
       to make a collection "implementing" a collection ! *)
    assert false
  )
;;



let toplevel_theorem_compile ctx env theorem_def =
  let theorem_desc = theorem_def.Parsetree.ast_desc in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for toplevel theorem %a@."
      Sourcify.pp_vname theorem_desc.Parsetree.th_name ;
  (* Make a print context with an empty mapping since we are at toplevel. *)
  let print_ctx = {
    Coq_pprint.cpc_current_unit = ctx.Context.scc_current_unit ;
    Coq_pprint.cpc_current_species = None ;
    Coq_pprint.cpc_collections_carrier_mapping =
      ctx.Context.scc_collections_carrier_mapping } in
  (* No abstraction info to compute for toplevel theorems since there's no
     dependencies outside species !
     We create a fake [Env.from_history]. *)
  let from = {
    Env.fh_initial_apparition =
      (ctx.Context.scc_current_unit, (Parsetree.Vlident "*Toplevel*")) ;
     Env.fh_inherited_along = [] } in
  generate_defined_theorem
    ctx print_ctx env [(* Empty min coq env. *)]
    ~self_manifest: None [] [] []
    from theorem_desc.Parsetree.th_name theorem_desc.Parsetree.th_stmt
    theorem_desc.Parsetree.th_proof
;;

