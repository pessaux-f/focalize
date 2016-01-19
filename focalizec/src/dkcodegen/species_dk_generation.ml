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


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
    Dedukti of FoCaL's collections and species.                    *)
(* *************************************************************** *)


(* Print the type of a method *)
let print_method_type ctx print_ctx env param out_fmter meth_type_kind =
  match meth_type_kind with
  | Parsetree_utils.DETK_computational meth_ty ->
     Format.fprintf out_fmter "cc.eT (%a)"
       (Dk_pprint.pp_type_simple_to_dk print_ctx) meth_ty
  | Parsetree_utils.DETK_logical lexpr ->
     Format.fprintf out_fmter "dk_logic.eP (";
     Species_record_type_dk_generation.generate_logical_expr
       ctx ~in_recursive_let_section_of: [] ~local_idents: []
       ~self_methods_status:
       (Expr_dk_generation.SMS_from_param param)
       ~recursive_methods_status:
       Expr_dk_generation.RMS_regular
       env lexpr ;
     Format.fprintf out_fmter ")"
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
      - [env] : The current Dedukti code generation environment.

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
      Env.DkGenEnv.find_species
        ~loc: Location.none ~current_unit from_as_ident env in
    (* Now, find the method in the species information. *)
    let method_info =
      List.find
        (fun { Env.mi_name = n } -> n = method_name) species_meths_infos in
    method_info.Env.mi_abstracted_methods
  with
  | Env.No_available_Dk_code_generation_envt file ->
      (* Ok, re-raise it to catch it at toplevel. *)
      raise (Env.No_available_Dk_code_generation_envt file)
  | _ ->
    (* Because the generator is inherited, the species where it is hosted
       MUST be in the environment. Otherwise, something went wrong... *)
    assert false
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
      ctx (Abstrs.EK_dk env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  (match instancied_with with
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       let prefix =
         if coll_mod = current_unit then coll_name ^ "__"
         else coll_mod ^ "." ^ coll_name ^ "__" in
       List.iter
         (fun (meth, _) ->
           (* Don't print the type to prevent being too verbose. *)
           Format.fprintf out_fmter "@ %s%a"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Dedukti, species parameters are abstracted by "_p_species_xxx". *)
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
      ctx (Abstrs.EK_dk env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  match instancied_with with
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       Format.fprintf out_fmter "@ " ;
       if coll_mod <> current_unit then Format.fprintf out_fmter "%s." coll_mod;
       Format.fprintf out_fmter "%s__me_as_carrier" coll_name
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Dedukti, species parameters are abstracted by "_p_species_xxx". *)
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
    Env.DkGenEnv.find_species
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
  (* Since in Dedukti, types are explicit, now we apply to each extra parameter
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
                 ctx (Abstrs.EK_dk env) param_name
                 original_param_unit original_param_index
                 field_memory.Misc_common.cfm_from_species.
                   Env.fh_inherited_along in
             (* We must now generate the Dedukti code for this FoCaL expression. *)
             Format.fprintf out_fmter "@ @[<1>(";
             Expr_dk_generation.generate_expr
               ctx ~local_idents: [] ~in_recursive_let_section_of: []
               (* Or whatever, "Self" will never appear at this point. *)
               ~self_methods_status:
                 Expr_dk_generation.SMS_abstracted
               ~recursive_methods_status:
                 Expr_dk_generation.RMS_regular
               env instancied_expr ;
             Format.fprintf out_fmter ")@]"
            )
       | Env.TypeInformation.SPAR_is ((_, _), _, _, _, _) ->
           instanciate_IS_parameter_through_inheritance
             ctx env original_param_index field_memory meths_from_param)
    meth_info.Env.mi_dependencies_from_parameters
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

(** Génère l'expression à mettre derrière le :=  dans les arguments d'une
    méthode afin de forcer une equivalence due à une def-dépendance de cette
    methode vis à vis de la méthode [name] venant de [from].
    Ceci revient à trouver le générateur de méthode de [name] puis à
    l'appliquer à tous ses arguments. "Tous ses arguments" signifie les
    carriers de paramètres, les méthodes de paramètres et surtout les
    méthodes de l'espèce courante EN FONCTION DE SI ELLES SONT ENCORE
    ABSTRAITES OU EFFECTIVEMENT DÉFINIES. Celà signifie qu'il faut être capable
    de savoir si dans l'espèce courante une méthode dont on dépend est restée
    lambda-liftée dans la méthode courante ou est définie dans l'espèce.
    Dans le premier cas, il faudra utiliser le paramètre _p_xxx qui se
    trouvera (forcément, par construction) en argument de la méthode courante.
    Dans le second cas, il faudra utiliser directement le nom de la methode
    qui se trouvera (forcément, par construction) généré dans l'espèce
    courante ou un de ses parents (=> notation pointée).
    Donc pour savoir cela, il faut que l'on ait une trace parmis les champs
    déjà générés desquels sont effectivement définis et desquels sont encore
    abstraits dans l'espèce courante. Avant, on s'en foutait car on nommait
    les méthodes de Self par abst_xxx tout le temps et la seule différence
    était que si elles étaient définies on en faisait des Definition et si
    elles étaient abstraites, on en faisait des Variables ! *)
let generate_def_dependency_equivalence env ctx generated_fields from name =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the application of the method generator. First, its name. *)
  let defined_from = from.Env.fh_initial_apparition in
  if (fst defined_from) <> ctx.Context.scc_current_unit then
    Format.fprintf out_fmter "%s." (fst defined_from);
    Format.fprintf out_fmter "%a__"
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
  Species_record_type_dk_generation.generate_method_lambda_lifted_arguments
    ~only_for_Self_meths out_fmter
    memory.Misc_common.cfm_used_species_parameter_tys
    memory.Misc_common.cfm_dependencies_from_parameters
    memory.Misc_common.cfm_coq_min_typ_env_names
;;

(** Factorise la genération des abstrations pour un champ défini. Ca colle
    donc les abstractions dues aux types des paramètres d'espèce, puis
    aux méthodes des paramètres d'espèce dont on dépend, puis enfin aux
    méthodes de nous-mêmes dont on dépend.
    En cas de non-Section, on met l'espace de séparation AVANT.

  Args:
    - [~in_section] : Collect section variables instead of printing them
    - [~sep] : What to print between arguments
    - [~without_types] : Whether to print types after arguments and defined parameters
 *)
let print_field_definition_prelude ?sep ?without_types ctx print_ctx env min_dk_env
    used_species_parameter_tys dependencies_from_params generated_fields =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the parameters from the species parameters' types we use.
     By the way, we get the stuff to add to the current collection carrier
     mapping to make so the type expressions representing some species
     parameter carrier types, will be automatically be mapped onto our
     freshly created extra args. The trailing "_T" will be automatically added
     by the type printing routine. *)
  let print_arg arg_printing type_printing (out : Format.formatter) =
    (match sep with
     | None -> Format.fprintf out "@ ("
     | Some _ -> ());
    arg_printing out;
    (match without_types with
     | None -> Format.fprintf out " :@ ";
                type_printing out
     | Some _ -> ());
    (match sep with
     | None -> Format.fprintf out ")"
     | Some sep -> Format.fprintf out " %s@ " sep)
  in
  let print_defined_arg arg_printing val_printing out =
    match without_types with
    | None ->
       (match sep with
        | None -> Format.fprintf out "@ ("
        | Some _ -> ());
       arg_printing out;
       Format.fprintf out " :=@ ";
       val_printing out;
       (match sep with
        | None -> Format.fprintf out ")"
        | Some _ -> Format.fprintf out " =>@ " ) (* Force the "=>" separator (instead of "->") for Sukerujo let binding. *)
    | Some _ -> ()               (* Defined arguments are not printed
                                   when without_types is given because
                                   without_types is used for listing parameters
                                   when applying a function and we don't need them
                                   in that case*)
  in
  let cc_mapping_extension =
    List.map
      (fun species_param_type_name ->
        let as_string =
          Parsetree_utils.vname_as_string_with_operators_expanded
            species_param_type_name in
        let param_name =  "_p_" ^ as_string in
        print_arg (fun out -> Format.fprintf out "%s_T" param_name)
                  (fun out -> Format.fprintf out "cc.uT")
                  out_fmter;
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
      Dk_pprint.dpc_collections_carrier_mapping =
        cc_mapping_extension @
          print_ctx.Dk_pprint.dpc_collections_carrier_mapping } in
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
            match meth_ty_kind with
            | Parsetree_utils.DETK_computational meth_ty ->
                print_arg (fun out -> Format.fprintf out "%s%a"
                    prefix
                    Parsetree_utils.pp_vname_with_operators_expanded
                    meth)
                  (fun out -> Format.fprintf out "cc.eT (%a)"
                      (Dk_pprint.pp_type_simple_to_dk new_print_ctx)
                      meth_ty)
                  out_fmter;
            | Parsetree_utils.DETK_logical lexpr ->
                (* Inside the logical expression of the method of the
                   parameter "Self" must be printed as "_p_param_name_T". *)
                let self_map =
                  Species_record_type_dk_generation.
                    make_Self_cc_binding_species_param
                    ~current_species: ctx.Context.scc_current_species
                    species_param_name in
                let new_ctx' = { new_ctx with
                  Context.scc_collections_carrier_mapping =
                    self_map :: new_ctx.Context.scc_collections_carrier_mapping
                } in
                print_arg (fun out -> Format.fprintf out "%s%a"
                    prefix
                    Parsetree_utils.pp_vname_with_operators_expanded
                    meth)
                  (fun out -> Format.fprintf out "dk_logic.eP (";
                    Species_record_type_dk_generation.generate_logical_expr
                      new_ctx' ~in_recursive_let_section_of: []
                      ~local_idents: []
                      ~self_methods_status:
                        (Expr_dk_generation.SMS_from_param
                         species_param_name)
                      ~recursive_methods_status:
                        Expr_dk_generation.RMS_regular
                      env lexpr ;
                    Format.fprintf out_fmter ")")
                  out_fmter
        )
        meths_from_param)
    dependencies_from_params;
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  List.iter
         (function (_, meth_dep) ->
           (* Reason ignored since in Dedukti we take all the kinds of methods in
              account. *)
           match meth_dep with
           | Env.TypeInformation.MDEM_Defined_carrier sch ->
              let ty = Types.specialize sch in
              print_defined_arg (fun out -> Format.fprintf out "abst_T")
                                (fun out -> Format.fprintf out "%a"
                                           (Dk_pprint.pp_type_simple_to_dk new_print_ctx)
                                           ty)
                                out_fmter
           | Env.TypeInformation.MDEM_Defined_computational (fr, _, n, _, _, _) ->
              print_defined_arg (fun out -> Format.fprintf out "abst_%a"
                                           Parsetree_utils.pp_vname_with_operators_expanded
                                           n)
                                (fun _ -> generate_def_dependency_equivalence
                                         env new_ctx generated_fields fr n)
                                out_fmter
           | Env.TypeInformation.MDEM_Defined_logical (_, n, _) ->
              print_defined_arg
                (fun out -> Format.fprintf out "abst_%a"
                           Parsetree_utils.pp_vname_with_operators_expanded n)
                (fun _ -> ())
                out_fmter
           | Env.TypeInformation.MDEM_Declared_carrier ->
               (* Note that by construction, the carrier is first in the env. *)
              print_arg
                (fun out -> Format.fprintf out "abst_T")
                (fun out -> Format.fprintf out "cc.uT")
                out_fmter
           | Env.TypeInformation.MDEM_Declared_computational (n, sch) ->
               (* Due to a decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               print_arg
                 (fun out -> Format.fprintf out "abst_%a"
                            Parsetree_utils.pp_vname_with_operators_expanded n)
                 (fun out -> Format.fprintf out "cc.eT %a"
                            (Dk_pprint.pp_type_simple_to_dk new_print_ctx) ty)
                 out_fmter
           | Env.TypeInformation.MDEM_Declared_logical (n, b) ->
                   print_arg
                     (fun out ->
                       Format.fprintf out "abst_%a"
                         Parsetree_utils.pp_vname_with_operators_expanded n)
                     (fun out -> Format.fprintf out "dk_logic.eP (";
                       Species_record_type_dk_generation.generate_logical_expr
                         new_ctx ~local_idents: []
                         ~in_recursive_let_section_of: []
                         ~self_methods_status:
                         Expr_dk_generation.SMS_abstracted
                         env b
                         ~recursive_methods_status:
                           Expr_dk_generation.RMS_regular ;
                       Format.fprintf out ")")
                     out_fmter)
         min_dk_env;
;;

(** Factorise la genération des abstrations pour un champ défini. Ca colle
    donc les abstractions dues aux types des paramètres d'espèce, puis
    aux méthodes des paramètres d'espèce dont on dépend, puis enfin aux
    méthodes de nous-mêmes dont on dépend.
    En cas de non-Section, on met l'espace de séparation AVANT.

    Renvoie l'environnement mis à jour par le prelude, n'affiche rien.
 *)
let generate_field_definition_prelude ctx print_ctx min_dk_env
    used_species_parameter_tys =
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
      Dk_pprint.dpc_collections_carrier_mapping =
        cc_mapping_extension @
          print_ctx.Dk_pprint.dpc_collections_carrier_mapping } in
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  let abstracted_methods =
    List.flatten
      (List.map
         (function (_, meth_dep) ->
           (* Reason ignored since in Dedukti we take all the kinds of methods in
              account. *)
           match meth_dep with
           | Env.TypeInformation.MDEM_Defined_carrier _
           | Env.TypeInformation.MDEM_Defined_computational _
           | Env.TypeInformation.MDEM_Defined_logical _ -> []
           | Env.TypeInformation.MDEM_Declared_carrier -> [Parsetree.Vlident "rep"]
           | Env.TypeInformation.MDEM_Declared_computational (n, _)
           | Env.TypeInformation.MDEM_Declared_logical (n, _) -> [n])
         min_dk_env) in
  (abstracted_methods, new_ctx, new_print_ctx)
;;

(* ************************************************************************* *)
(* Context.species_compil_context ->                                         *)
(*  Dk_pprint.dk_print_context -> Env.DkGenEnv.t ->                            *)
(*    min_dk_env_element list -> Parsetree.vname list ->                    *)
(*      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ->             *)
(*        Parsetree.vname -> Parsetree.expr -> let_connect: let_connector -> *)
(*          Parsetree.vname list -> Types.type_scheme ->                     *)
(*            Parsetree.vname list                                           *)
(** {b Descr} Gererate the Dedukti code for a method defined in the current
    species (i.e. not inherited). In fact, this generates the method
    generator for this method in this species.
    It returns the list of methods of ourselves we depend on and that were
    lambda-lifted according to th minimal Dedukti environment.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let generate_defined_non_recursive_method ctx print_ctx env min_dk_env
    ~self_manifest used_species_parameter_tys dependencies_from_params
    generated_fields name body params scheme =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Dedukti code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Start the Dedukti function definition. *)
  Format.fprintf out_fmter "@[<2>def %a__%a"
    Sourcify.pp_vname (snd ctx.Context.scc_current_species)
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Generate the prelude of the method, i.e the sequence of parameters induced
     by the various lamda-liftings and their types .
     By the way, we get updated in the [new_print_ctx] the way "Self" must be
     printed. *)
  print_field_definition_prelude ctx print_ctx env min_dk_env
      used_species_parameter_tys dependencies_from_params generated_fields;
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_field_definition_prelude ctx print_ctx min_dk_env used_species_parameter_tys in
  (* We now generate the postlude of the method, i.e the sequence of real
     parameters of the method, not those induced by abstraction and finally
     the method's body. Inside, methods we depend on are abstracted by
     "abst_xxx". Since the method is non recursive, there is no decreasing
     argument to pass. *)
  Proof_dk_generation.generate_defined_method_proto_postlude
    new_ctx new_print_ctx env ~self_manifest params scheme (Some body) ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n" ;
  abstracted_methods
;;



(* ********************************************************************** *)
(** {b Descr} Generate the Dedukti code for a non-recursive method of the
    current species.
    If the method is defined in this species, then it generates the
    method generator. If the method is inherited, it recovers the
    methods abstracted in the generator without generating again the code.
    And in any case, it generate the local definition "self_..." by
    applying the generator to the local definitions.

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let generate_non_recursive_field_binding ctx print_ctx env min_dk_env
    ~self_manifest used_species_parameter_tys dependencies_from_params
    generated_fields (from, name, params, scheme, body) =
  (* First of all, only methods defined in the current species must be
     generated. Inherited methods ARE NOT generated again ! *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_non_recursive_method
        ctx print_ctx env min_dk_env ~self_manifest used_species_parameter_tys
        dependencies_from_params generated_fields name body params scheme
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
  (* Finally, return the methods we found abstracted in the minimal Dedukti typing
     environment and that leaded to extra parameters via lambda-lifting. *)
  abstracted_methods
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
(* Context.species_compil_context -> Dk_pprint.dk_print_context ->              *)
(*   Env.DkGenEnv.t -> min_dk_env_element list ->                          *)
(*     compiled_species_fields list -> Parsetree.qualified_species ->        *)
(*       Parsetree.vname -> Parsetree.logical_expr -> Parsetree.vname list   *)
(** {b Descr} Gererate the Dk code for a theorem defined in the current
    species (i.e. not inherited). In fact, this generates the theorem
    generator for this theorem in this species.
    It returns the list of methods of ourselves we depend on and that were
    abstracted by a "Variable abst_..." in the current Dk theorem's section
    according to the minimal dk environment.

    {b Rem}: Exported outside this module.                                *)
(* ************************************************************************* *)
let generate_defined_theorem ctx print_ctx env min_dk_env ~self_manifest
    used_species_parameter_tys dependencies_from_params generated_fields
    from name logical_expr proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Dk code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Put an extra newline before the theorem to make some air ! *)
  Format.fprintf out_fmter "@\n(; From species %a. ;)@\n"
    Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
  (* Now, generate the real theorem, using the temporarily created and applying
     the proof. *)
  Format.fprintf out_fmter "@[<2>def %a__%a@ "
    Sourcify.pp_vname (snd ctx.Context.scc_current_species)
    Parsetree_utils.pp_vname_with_operators_expanded name;
  (* Generate the prelude of the method, i.e the sequence of parameters and
     their types induced by the various lamda-liftings. *)
  print_field_definition_prelude
    ctx print_ctx env min_dk_env
    used_species_parameter_tys dependencies_from_params generated_fields;
  let (abstracted_methods, new_ctx, _) =
    generate_field_definition_prelude ctx print_ctx min_dk_env used_species_parameter_tys in
  Format.fprintf out_fmter " :@ dk_logic.eP@ (" ;
  (* Finally, the theorem itself. Inside, any method of "Self" is abstracted
     (i.e. is lambda-lifted), hence named "abst_xxx". That's why we use the
     mode [SMS_abstracted]. *)
  Species_record_type_dk_generation.generate_logical_expr
    ~local_idents: [] ~in_recursive_let_section_of: []
    ~self_methods_status: Expr_dk_generation.SMS_abstracted
    ~recursive_methods_status: Expr_dk_generation.RMS_regular
    new_ctx env logical_expr ;
  Format.fprintf out_fmter ")";
  Format.fprintf out_fmter " :=@\n";
  Format.fprintf out_fmter "@]@\n";
  Proof_dk_generation.generate_theorem_body new_ctx print_ctx env min_dk_env ~self_manifest
    used_species_parameter_tys dependencies_from_params generated_fields
    name logical_expr proof;
  abstracted_methods



let generate_theorem ctx print_ctx env min_dk_env used_species_parameter_tys
    ~self_manifest dependencies_from_params generated_fields
   (from, name, logical_expr) proof =
  (* A "theorem" defined in the species leads to a Dk "Theorem". *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_theorem
        ctx print_ctx env min_dk_env ~self_manifest used_species_parameter_tys
        dependencies_from_params generated_fields from name logical_expr
        proof
    else
      (begin
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
      end) in
  (* Return the names abstracted in the minimal typing environment. *)
  abstracted_methods



(** ***************************************************************************
    {b Descr}: Used to generate a recursive function that is not (assumed)
    structural.
    In this case, the function is generated using the Function construct of
    Dk.
    If experimental mode is activated, then it generates a termination order
    and a termination proof instead of using fake and generic ones.

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
(* TODO: split in several parts. Too big ! *)
let generate_defined_recursive_let_definition_With_Function ctx print_ctx env
    generated_fields from name params scheme body
    ai =
  let out_fmter = ctx.Context.scc_out_fmter in
  match body with
   | Parsetree.BB_logical _ ->
       failwith "recursive logical : TODO"  (* [Unsure] *)
   | Parsetree.BB_computational body_expr ->
       let species_name = snd (ctx.Context.scc_current_species) in
       (* Extend the context with the mapping between these recursive
          functions and their extra arguments. Since we are in Dk, we
          need to take care of the logical definitions and of the
          explicite types abstraction management. *)
       let ctx' = {
         ctx with
           Context.scc_lambda_lift_params_mapping =
             [(name,
               Misc_common.make_params_list_from_abstraction_info
                 ~care_logical: true ~care_types: true ai)] } in
       (* We get the function's parameters and their types. This will serve
          at various stage, each time we will need to speak about a
          parameter. *)
       (* For [bind_parameters_to_types_from_type_scheme], not that we do not
          have anymore information about "Self"'s structure... *)
       let (params_with_type, return_ty_opt, _) =
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

(* [Unsure] *)
       let (abstracted_methods, new_ctx, new_print_ctx) =
         ((* ---> Now, generate the prelude of the only method introduced by
              "let rec". *)
           generate_field_definition_prelude
             ctx' print_ctx ai.Env.TypeInformation.ad_min_dk_env ai.Env.TypeInformation.ad_used_species_parameter_tys) in


       (*
          A recursive method m is defined in Dedukti by two symbols m and rec_m:

          let rec m (arg1, arg2) = F(m, arg1, arg2)

          becomes

          def rec_m : T1 -> T2 -> T.
          def m : T1 -> T2 -> T.
          [arg1 : T1, arg2 : T2] rec_m arg1 arg2 --> F(m, arg1, arg2).
          [arg1 : T1, arg2 : T2] m arg1 arg2 --> call_by_value_T2 T (call_by_value_T1 (T2 -> T) rec_m arg1) arg2.

          call_by_value_Ti has been defined with Ti such that if v is a value of type Ti and f a function of type
          Ti -> T then
          (call_by_value_Ti T f v) rewrites to (f v).
        *)

       (* Define the type of both symbols *)
       Format.fprintf out_fmter
         "@[<2>def %a__%a_type@ : Type := ("
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;

       print_field_definition_prelude
         ~sep: "->" ctx' print_ctx env
         ai.Env.TypeInformation.ad_min_dk_env
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         generated_fields;

       List.iter
         (fun (_, ty) ->
          Format.fprintf out_fmter "cc.eT (%a) ->@ "
            (Dk_pprint.pp_type_simple_to_dk new_print_ctx) ty)
         params_with_type;

       Format.fprintf out_fmter "cc.eT (%a)).@]@\n"
         (Dk_pprint.pp_type_simple_to_dk new_print_ctx) return_ty ;


       (* Declare both symbols *)
       Format.fprintf out_fmter
         "@[<2>def %a__rec_%a@ : %a__%a_type.@]@\n"
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;

       Format.fprintf out_fmter
         "@[<2>def %a__%a@ : %a__%a_type.@]@\n"
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;


       let rec print_list_param sep out = function
         | [] -> ()
         | [(a, _)] ->
            Format.fprintf out "%a"
              Parsetree_utils.pp_vname_with_operators_expanded a
         | (a, _) :: l ->
            Format.fprintf out "%a%s@ %a"
              Parsetree_utils.pp_vname_with_operators_expanded a
              sep
              (print_list_param sep) l
       in

       (* Generate the recursive function. *)
       Format.fprintf out_fmter "@[<2>[";

       print_field_definition_prelude ~sep: "," ctx' print_ctx env
         ~without_types: true
         ai.Env.TypeInformation.ad_min_dk_env
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         generated_fields;

       print_list_param "," out_fmter params_with_type;

       Format.fprintf out_fmter "] %a__rec_%a"
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;

       print_field_definition_prelude ~without_types: true ctx' print_ctx env
         ai.Env.TypeInformation.ad_min_dk_env
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         generated_fields;

       List.iter
         (fun (a, _) ->
          Format.fprintf out_fmter "@ (%a)"
              Parsetree_utils.pp_vname_with_operators_expanded a)
         params_with_type;

       Format.fprintf out_fmter "@ -->@\n";

       (* Let-bind the unqualified version of the recursive method because it is how
          it appears in the recursive call. *)
       Format.fprintf out_fmter "(%a :=@ %a__%a =>@ "
         Parsetree_utils.pp_vname_with_operators_expanded name
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;

       (* In Dedukti, we do want the current recursive method to be applied to
          parameters coming from lambda-lifting. Hence ~in_recursive_let_section_of: []
          instead of ~in_recursive_let_section_of: [name] (as found in Coq translation) *)

       Expr_dk_generation.generate_expr
         new_ctx ~local_idents: [] ~in_recursive_let_section_of: []
         ~self_methods_status: Expr_dk_generation.SMS_abstracted
         ~recursive_methods_status: Expr_dk_generation.RMS_regular
         env body_expr ;
       Format.fprintf out_fmter ").@]@\n";

       (* Generate the CBV version. *)
       Format.fprintf out_fmter "@[<2>[] %a__%a -->@ ("
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name;

       print_field_definition_prelude ~sep: "=>" ctx' print_ctx env
         ai.Env.TypeInformation.ad_min_dk_env
         ai.Env.TypeInformation.ad_used_species_parameter_tys
         ai.Env.TypeInformation.ad_dependencies_from_parameters
         generated_fields;

       List.iter
         (fun (a, ty) ->
          Format.fprintf out_fmter "%a : cc.eT (%a) =>@ "
              Parsetree_utils.pp_vname_with_operators_expanded a
              (Dk_pprint.pp_type_simple_to_dk new_print_ctx) ty)
         params_with_type;

       let rec print_cbv_types_as_arrows out = function
         | [] -> Dk_pprint.pp_type_simple_to_dk new_print_ctx out return_ty
         | ty :: l ->
            Format.fprintf out "(@[cc.Arrow %a %a@])"
              (Dk_pprint.pp_type_simple_to_dk new_print_ctx) ty
              print_cbv_types_as_arrows l
       in

       let rec print_cbv accu out = function
         | [] ->
            Format.fprintf out "@[%a__rec_%a"
               Parsetree_utils.pp_vname_with_operators_expanded species_name
               Parsetree_utils.pp_vname_with_operators_expanded name;
            print_field_definition_prelude ~without_types: true ctx' print_ctx env
              ai.Env.TypeInformation.ad_min_dk_env
              ai.Env.TypeInformation.ad_used_species_parameter_tys
              ai.Env.TypeInformation.ad_dependencies_from_parameters
              generated_fields;
            Format.fprintf out "@]"
         | (a, ty) :: l when Dk_pprint.has_cbv ty ->
            Format.fprintf out "@[%a@ %a@ (%a)@ %a@]"
               (Dk_pprint.pp_for_cbv_type_simple_to_dk new_print_ctx) ty
               print_cbv_types_as_arrows accu
               (print_cbv (ty :: accu)) l
               Parsetree_utils.pp_vname_with_operators_expanded a
         | (a, ty) :: l ->
            Format.fprintf out "@[%a@ %a@]"
               (print_cbv (ty :: accu)) l
               Parsetree_utils.pp_vname_with_operators_expanded a
       in

       print_cbv [] out_fmter (List.rev params_with_type);

       (* Close the pretty print box. *)
       Format.fprintf out_fmter ").@]@\n" ;
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
   {b Descr}: Is in charge to generate the Dk code for recursive functions.
   It deals and differentiate structural and non-structural recursive
   functions.

    {b Visibility}: Not exported outside this module.
 *************************************************************************** *)
let generate_recursive_let_definition ctx print_ctx env
    fields_abstraction_infos generated_fields l =
  match l with
   | [] ->
       (* A "let", then a fortiori "let rec" construct *)
       (* must at least bind one identifier !          *)
       assert false
   | [(from, name, params, scheme, body, _, _, _)] ->
      let ai = List.assoc name fields_abstraction_infos in
       (* First of all, only methods defined in the current species must be
          generated. Inherited methods ARE NOT generated again ! *)
       if from.Env.fh_initial_apparition = ctx.Context.scc_current_species
       then (
         generate_defined_recursive_let_definition_With_Function
           ctx print_ctx env generated_fields from name
           params scheme body ai
            )
       else (
         (* Just a bit of debug/information if requested. *)
         if Configuration.get_verbose () then
           Format.eprintf
             "Field '%a' inherited from species '%a' but not \
             (re)-declared is not generated again. @."
             Parsetree_utils.pp_vname_with_operators_expanded name
             Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
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
   | (_, name1, _, _, _, _, _, _) ::
     (_, name2, _, _, _, _, _, _) :: _ ->
       raise (Recursion.MutualRecursion (name1, name2))
;;



(** generated_fields : The list of previous fields of the species that have
    already be generated. Used while generating theorems to know what to apply
        to the methods generators the theorem depends on. *)
let generate_methods ctx print_ctx env ~self_manifest fields_abstraction_infos generated_fields =
     function
  | Env.TypeInformation.SF_sig (from, name, sch) ->
      (* Only declared, hence, no code to generate yet ! *)
      let abstraction_info = List.assoc name fields_abstraction_infos in
      if Configuration.get_verbose () then
        Format.eprintf "Dedukti code for signature '%a' leads to void code.@."
          Parsetree_utils.pp_vname_with_operators_expanded name;
      (* Nothing very exciting to keep for the collection generator. *)
      let compiled_field = {
        Misc_common.cfm_is_logical = false;
        Misc_common.cfm_from_species = from;
        Misc_common.cfm_method_name = name;
        Misc_common.cfm_method_scheme = Env.MTK_computational sch;
        (* Since no code is generated for "sig", no need to get bored with
           species parameters carriers that may appear in the type of the
           "sig". *)
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Env.TypeInformation.ad_used_species_parameter_tys;
        (* Since the "sig " has no code, it can't refer to parameters'
           methods ! *)
        Misc_common.cfm_raw_dependencies_from_parameters = [];
        Misc_common.cfm_dependencies_from_parameters = [];
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
          ctx print_ctx env
          abstraction_info.Env.TypeInformation.ad_min_dk_env
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
        ctx print_ctx env fields_abstraction_infos generated_fields l
  | Env.TypeInformation.SF_theorem (from, name, _, logical_expr, pr, _) ->
      let abstraction_info = List.assoc name fields_abstraction_infos in
      let coq_min_typ_env_names =
        generate_theorem
          ctx print_ctx env abstraction_info.Env.TypeInformation.ad_min_dk_env
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
      let abstraction_info = List.assoc name fields_abstraction_infos in
      (* "Property"s are discarded. However we compute their dependencies. *)
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
    the Dk creation of the record type.
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
             Dk. *)
          let param_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't need
             any species expression to annotate this parameter in the Dk type
             expression annotating this parameter in the hosting species
             record type: it will simply be of the type [type_coll]. *)
          (type_coll, (param_name, (Types.CCMI_in provenance))))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(** Utiliser uniquement lorsque l'on génère la représentation de Self dans
    la création d'un générateur de collection. Ca permet de changer les noms
    vers lesquels on mappe une carrier en préfixant les "IS" par "_p_".
    En effet, le carrier mapping que l'on a à ce moment mappe directement
    sur "Bla_T". Or dans le contexte d'un générateur de collection, ben on a
    créé des paramètres pour abstraire les carriers des "IS" et ces paramètres
    sont la la forme "_p_Bla". Et comme on ne veut pas refaire un carrier
    mapping from scratch, on le patche comme ça. *)
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
(* Env.DkGenEnv.t -> Env.TypeInformation.species_description ->          *)
(*   Env.DkGenEnv.t                                                      *)
(** {b Descr} : This function extend the dk code generation envionnment
    for a species generation. Because in Dk we need information about
    the number of extra parameters to add to function idents due to the
    fact that in Dk polymorphism is explicit, we need to make methods of
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
        Env.DkGenEnv.add_value
          ~toplevel: None m_name Env.DkGenInformation.VB_non_toplevel
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
           Env.DkGenEnv.add_species
             ~loc: Location.none (Parsetree.Vuident param_name)
             ([], bound_methods, None, Env.COS_species) accu_env)
    env_with_methods_as_values
    species_descr.Env.TypeInformation.spe_sig_params
;;




(* ************************************************************************** *)
(* Format.formatter -> Misc_common.compiled_species_fields list ->
   (Parsetree.vname * Env.ordered_methods_from_params) list                   *)
(** {b Descr} : Dumps as Dk code the parameters required to the collection
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
    [dump_collection_generator_arguments_for_params_methods] of file
    "species_ml_generation.ml".

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let dump_collection_generator_arguments_for_params_methods
    ctx print_ctx env out_fmter compiled_species_fields =
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

      {b Rem} : Local to the enclosing [dump_collection_generator_arguments]
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
      let species_param_str_name =
        Parsetree_utils.name_of_vname species_param_name
      in
      let prefix = "_p_" ^ species_param_str_name ^ "_" in
      let coll_mapping =
        ((ctx.Context.scc_current_unit, "Self"),
         ("_p_" ^ species_param_str_name, Types.CCMI_is))
        ::
          ((ctx.Context.scc_current_unit, species_param_str_name),
         ("_p_" ^ species_param_str_name, Types.CCMI_is))
        :: print_ctx.Dk_pprint.dpc_collections_carrier_mapping
      in
      let new_print_ctx =
        {print_ctx
        with Dk_pprint.dpc_collections_carrier_mapping =
               coll_mapping}
      in
      let new_ctx =
        {ctx
        with Context.scc_collections_carrier_mapping =
               coll_mapping}
      in
      List.iter
        (fun (meth, meth_ty_kind) ->
          Format.fprintf out_fmter "@ (%s%a : %a)"
            prefix
            Parsetree_utils.pp_vname_with_operators_expanded meth
            (print_method_type new_ctx new_print_ctx env species_param_name) meth_ty_kind
        )
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
    [dump_collection_generator_arguments_for_params_methods] for the detail).
    Returns the list of the parameters names to later make them public in
    order to know what must be applied to the collection generator. This list

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let remind_collection_generator_arguments_for_params_carriers ctx =
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
    [remind_collection_generator_arguments_for_params_carriers]. *)
let dump_collection_generator_arguments_for_params_carriers out_fmter lst ~param =
  List.iter
    (fun (param_kind, param_name) ->
      match param_kind with
       | Env.ScopeInformation.SPK_is ->
           (* We need to enforce the type for Dk because I don't know what. *)
          if param then
            Format.fprintf out_fmter "@ (_p_%a_T : cc.uT)"
              Parsetree_utils.pp_vname_with_operators_expanded
              param_name
          else
            Format.fprintf out_fmter "@ _p_%a_T"
              Parsetree_utils.pp_vname_with_operators_expanded
              param_name
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






let build_collection_generator_arguments_for_params_methods out_fmter
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




let generate_collection_generator ctx print_ctx env compiled_species_fields
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
      [Theorem] definitions.

      {b Rem} : Local to the [generate_collection_generator] function.
               Not exported.                                                 *)
  (* *********************************************************************** *)
  let process_one_field field_memory =
    let from = field_memory.Misc_common.cfm_from_species in
    Format.fprintf out_fmter "(; From species %a. ;)@\n"
      Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
    Format.fprintf out_fmter "@[<2>(local_%a :=@ "
      Parsetree_utils.pp_vname_with_operators_expanded
      field_memory.Misc_common.cfm_method_name;
    if Configuration.get_verbose () then
      Format.eprintf "Generating Dk code for collection generator for '%a'.@."
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
      let (_, current_species_vname) = ctx.Context.scc_current_species in
      Format.fprintf out_fmter "%a__%a"
        Parsetree_utils.pp_vname_with_operators_expanded
        current_species_vname
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
      Species_record_type_dk_generation.generate_method_lambda_lifted_arguments
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
      Format.fprintf out_fmter "%a__%a"
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
      field_memory.Misc_common.cfm_coq_min_typ_env_names ;
    (* That's all for this field code generation. *)
    Format.fprintf out_fmter ")@]@\n" ;
    if Configuration.get_verbose () then
      Format.eprintf "End of Dk code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name in

  (* *********************** *)
  (* Now, let's really work. *)
  (* A little comment in the generated Dk code. *)
  Format.fprintf out_fmter
    "@\n(; Fully defined '%a' species's collection generator. ;)@\n"
    Sourcify.pp_vname current_species_name;
  (* The generic name of the collection generator: the species' name +
     "_collection_create". *)
  Format.fprintf out_fmter "@[<2>def %a__collection_create"
      Sourcify.pp_vname current_species_name;
  (* The collection generator first arguments are those corresponding to the
     IS species parameters carriers, hence to the record type parameters.
     All of them are in the [collection_carrier_mapping] of the current
     compilation context. We remind the list of the parameters names by the
     way to later make them public in order to know what must be applied to
     the collection generator. *)
  let params_carriers_abstr_for_record =
    remind_collection_generator_arguments_for_params_carriers ctx in
  (* Now, we dump them to make them parameters of the collection generator. *)
  dump_collection_generator_arguments_for_params_carriers
    out_fmter params_carriers_abstr_for_record ~param:true;
  (* Generate the parameters the collection generator needs to build each of
     the current species's local function (functions corresponding to the
     actual method stored in the collection record).
     These parameters of the generator come from the abstraction of methods
     coming from our collection parameters we depend on. By the way, recover the
     list of species parameters linked together with their methods we need to
     instanciate in order to apply the collection generator. *)
  let abstr_params_methods_in_coll_gen =
    dump_collection_generator_arguments_for_params_methods
      ctx print_ctx env out_fmter compiled_species_fields in
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
               Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit;
               Dk_pprint.dpc_current_species =
               Some
                 (Parsetree_utils.type_coll_from_qualified_species
                    ctx.Context.scc_current_species);
               Dk_pprint.dpc_collections_carrier_mapping =
                 (* Prefix all the "IS" mappings by "_p_" to use the parameters
                    declared in the collection generator's header. *)
                 make_carrier_mapping_using_lambda_lifts
                   ctx.Context.scc_collections_carrier_mapping } in
             Format.fprintf out_fmter "@[<2>(local_rep :=@ %a)@]@\n"
               (Dk_pprint.pp_type_simple_to_dk print_ctx) type_from_scheme
           )
          else process_one_field field_memory
      | Misc_common.CSF_property field_memory
      | Misc_common.CSF_theorem field_memory
      | Misc_common.CSF_let field_memory -> process_one_field field_memory
      | Misc_common.CSF_let_rec l ->
          List.iter (fun fm -> process_one_field fm) l)
    compiled_species_fields ;
  Format.fprintf out_fmter " :=@ ";
  (* Now, apply the record type constructor. *)
  Format.fprintf out_fmter "%a__mk_record"
    Sourcify.pp_vname current_species_name;
  (* The "mk_record" first arguments are those corresponding to the IS species
     parameters carriers. They we already computed when we created the
     "mk_record". So we just now need to apply then since they are
     parameters (with the same names) of the collection generator we are
     building. *)
  dump_collection_generator_arguments_for_params_carriers
    out_fmter params_carriers_abstr_for_record ~param:false;
  (* Now, print the same names than the parameters that must be provided when
     using the collection generator to represent methods of the collection
     parameters that are abstracted and used by the local "local_xxx" (these
     latter used to create the collection generator). *)
  build_collection_generator_arguments_for_params_methods
    out_fmter abstracted_params_methods_in_record_type ;
  (* Then, always the "local_rep" since the first record field represents what
     is to be the "future collection" carrier (foo_T :> Set.). *)
  Format.fprintf out_fmter "@ local_rep";
  (* No need to generate the local functions that will be used to fill the
     record value since in Dk we always generate them. It's already done ! *)
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
   abstr_params_methods_in_coll_gen)
;;




let species_compile env ~current_unit out_fmter species_def species_descr
    dep_graph fields_abstraction_infos =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Dk code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the chapter encapsulating the species representation. *)
  let module_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>(; Module %s. ;)@\n" module_name;
  (* Insert in the environment the value bindings of the species methods and
     the species bindings for its parameters. This is needed since in Dk
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
      Species_record_type_dk_generation.generate_record_type
        ctxt_ccmap env' species_descr fields_abstraction_infos
    else [] in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Dk_pprint.dpc_current_unit = ctxt_no_ccmap.Context.scc_current_unit;
    Dk_pprint.dpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctxt_no_ccmap.Context.scc_current_species);
    Dk_pprint.dpc_collections_carrier_mapping = [] } in
  (* Because we sometimes need to bind function parameters to their types
     with the function
     [MiscHelpers.bind_parameters_to_types_from_type_scheme], we must
     beforehand know is [Self] is manifest or not. I.e. if there is a
     signature called "representation". *)
  let self_manifest =
    Misc_common.find_self_representation_if_some
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* Now, generate the Dk code of the methods. *)
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
    if species_descr.Env.TypeInformation.spe_is_closed then
      (begin
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
        abstr_params_methods_in_coll_gen) =
        generate_collection_generator
          ctxt_ccmap print_ctx env' compiled_fields
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
      let coll_gen_params_info = {
        Env.DkGenInformation.cgp_abstr_param_carriers_for_record =
        (* Just remove the "IN"/"IS" tag that is not needed to keep in the code
           generation environment. *)
          List.map snd params_carriers_abstr_for_record;
        Env.DkGenInformation.cgp_abstr_param_methods_for_record =
          abstracted_params_methods_in_record_type';
        Env.DkGenInformation.cgp_abstr_param_methods_for_coll_gen =
          abstr_params_methods_in_coll_gen } in
      Some
        { Env.DkGenInformation.cgi_implemented_species_params_names =
            species_params_names_n_kinds;
          Env.DkGenInformation.cgi_generator_parameters =
            coll_gen_params_info }
      end)
    else None in
  (* The end of the module hosting the species. *)
  Format.fprintf out_fmter "@]\n(; End %s. ;)@\n@\n" module_name;
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
let print_implemented_species_for_dk ~current_unit out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another Dk "file-module". *)
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit, then
          again no need to find the species's module in another Dk
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
          Format.fprintf out_fmter "%a__me_as_carrier"
            Parsetree_utils.pp_vname_with_operators_expanded
            corresponding_effective_vname
      | RTAI_by_in expr ->
          Format.fprintf out_fmter "@ ";
          Expr_dk_generation.generate_expr
            ctx ~local_idents: [] ~in_recursive_let_section_of: []
            ~self_methods_status: Expr_dk_generation.SMS_from_record
            ~recursive_methods_status:
              Expr_dk_generation.RMS_regular
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
               (* If needed, qualify the name of the species in the Dk code.
                  Don't print the type to prevent being too verbose. *)
               Format.fprintf out_fmter "@ ";
               (match corresponding_effective_opt_fname with
               | Some fname -> Format.fprintf out_fmter "%s." fname
               | None -> ()) ;
               (* Species name.method name. *)
               Format.fprintf out_fmter "%a__%a"
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
           Expr_dk_generation.generate_expr
             ctx ~local_idents: [] ~in_recursive_let_section_of: []
             ~self_methods_status:
               (* Or what you prefer. *)
               Expr_dk_generation.SMS_abstracted
             ~recursive_methods_status:
               Expr_dk_generation.RMS_regular
             env expr ;
           Format.fprintf out_fmter ")@]"
          ))
    l
;;



let apply_collection_generator_to_parameters ctx env formal_to_effective_map
    col_gen_info =
  let col_gen_params_info =
    col_gen_info.Env.DkGenInformation.cgi_generator_parameters in
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
      col_gen_params_info.Env.DkGenInformation.
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
    col_gen_params_info.Env.DkGenInformation.
      cgp_abstr_param_methods_for_coll_gen ;
  record_type_args_instanciations
;;



(* ************************************************************************** *)
(* current_unit: Types.fname -> Format.formatter -> Parsetree.ident -> unit   *)
(** {b Descr} : Helper that prints a species name as a Dk module, with
    module qualification if needed.
    In other words, each time we need to refer to a module qualification
    induced by a species, this function prints the the name of the species,
    prefixed by its hosting file considered as an OCaml module if this
    species is not in the current compilation unit.
    For example, imagine we are in the "foo.foc" file and we need to speak
    of a record field of a species "S" that lives in the "bar.foc" file.
    Then because each FoCaL compilation unit is mapped onto an Dk file
    (hence an Dk module corresponding to the file-as-module), it will be
    printed like "bar.S". If the species "S" was in the same compilation unit
    (i.e. "foo.foc"), then it would be printed directly "S".

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let print_implemented_species_as_dk_module ~current_unit ?(prefix = "") out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another Dk "file-module". *)
       Format.fprintf out_fmter "%s%s" prefix (Parsetree_utils.name_of_vname vname)
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit,  *)
       (* then again no need to find the species's module in another Dk *)
       (* "file-module" otherwise we explicitely prefix by the module    *)
       (* name corresponding to the filename.                            *)
       if fname <> current_unit then Format.fprintf out_fmter "%s." fname;
       Format.fprintf out_fmter "%s%s" prefix (Parsetree_utils.name_of_vname vname)
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
                "(; Carrier's structure explicitly given by \"rep\". ;)@\n" ;
              Format.fprintf ctx.Context.scc_out_fmter
                "@[<2>def %a__me_as_carrier@ "
                Sourcify.pp_vname (snd ctx.Context.scc_current_species);
              (* Print the variables names... *)
              List.iter
                (function (_, (h, _)) ->
                  Format.fprintf ctx.Context.scc_out_fmter "(%s : cc.uT) " h)
                ctx.Context.scc_collections_carrier_mapping ;
              let print_ctx = {
                Dk_pprint.dpc_current_unit = ctx.Context.scc_current_unit ;
                Dk_pprint.dpc_current_species =
                  Some
                    (Parsetree_utils.type_coll_from_qualified_species
                       ctx.Context.scc_current_species) ;
                Dk_pprint.dpc_collections_carrier_mapping =
                ctx.Context.scc_collections_carrier_mapping } in
              (* Now, output the type's name and body. *)
              let ty = Types.specialize sch in
              Format.fprintf ctx.Context.scc_out_fmter ":=@ %a.@]@\n"
                (Dk_pprint.pp_type_simple_to_dk print_ctx) ty
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

    leads to the following Dk code:

    {[
      Record Foo0 (A0_T : Set) : Type :=
        mk_Foo0 {
        Foo0_T :> Set;
        (* From species collgen_for_dk#Foo0. *)
        Foo0_v : basics.int__t
        }.
      ...
      Module Coll =
        Definition me_as_carrier := ...
        (* From species collgen_for_dk#Foo0. *)
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

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let make_collection_effective_methods ctx env implemented_species_name
    collection_descr formals_to_effectives record_type_args_instanciations
    record_type_args_instanciations2 =
  let out_fmter = ctx.Context.scc_out_fmter in
  let current_unit = ctx.Context.scc_current_unit in
  let (_, species_name) = ctx.Context.scc_current_species in
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
            "@[<2>def %a__%a :=@ @[<1>("
            Sourcify.pp_vname species_name
            Parsetree_utils.pp_vname_with_operators_expanded n ;
          print_implemented_species_as_dk_module
            ~current_unit out_fmter implemented_species_name ~prefix:"proj_";
          Format.fprintf out_fmter "__rf_%a@ "
            Parsetree_utils.pp_vname_with_operators_expanded n;
          (* Apply to the instanciations of the parameters carriers. *)
          print_record_type_carriers_args_instanciations
            ctx env record_type_args_instanciations;
          (* Apply to the instanciations of the parameters methods we depend
              on. *)
          print_methods_from_params_instanciations
            ctx env formals_to_effectives
            record_type_args_instanciations2;
          Format.fprintf out_fmter "@ %a__effective_collection).@]@]@\n"
            Sourcify.pp_vname species_name;
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, n, _, _, _, _, _, _) ->
              Format.fprintf out_fmter
                "@[<2>def %a__%a :=@ @[<1>(proj_"
                Sourcify.pp_vname species_name
                Parsetree_utils.pp_vname_with_operators_expanded n;
              print_implemented_species_as_dk_module
                ~current_unit out_fmter implemented_species_name;
              Format.fprintf out_fmter "__rf_%a@ "
                Parsetree_utils.pp_vname_with_operators_expanded n ;
              (* Apply to the instanciations of the parameters carriers. *)
              print_record_type_carriers_args_instanciations
                ctx env record_type_args_instanciations;
              (* Apply to the instanciations of the parameters methods we
                 depend on. *)
              print_methods_from_params_instanciations
                 ctx env formals_to_effectives
                 record_type_args_instanciations2;
              Format.fprintf out_fmter "@ %a__effective_collection).@]@]@\n"
                Sourcify.pp_vname species_name;)
            l)
    collection_descr.Env.TypeInformation.spe_sig_methods
;;



(* ************************************************************************ *)
(* current_unit: Parsetree.module_name ->                                   *)
(*   Misc_common.collection_effective_arguments list ->                     *)
(*     Env.DkGenInformation.collection_generator_info ->                   *)
(*      (Parsetree.vname * Misc_common.collection_effective_arguments) list *)
(* ************************************************************************ *)
let map_formal_to_effective_in_collection ~current_unit collection_body_params
    col_gen_params_info =
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
      col_gen_params_info.Env.DkGenInformation.
      cgi_implemented_species_params_names
      collection_body_params
  with Invalid_argument "List.map2" ->
    assert false  (* The lists length must be equal. *)
;;



(* ************************************************************************* *)
(* current_unit: Parsetree.module_name ->                                    *)
(*   Parsetree.ident_desc Parsetree.ast ->                                   *)
(*    (Parsetree.vname * Misc_common.collection_effective_arguments) list -> *)
(*      Env.generic_code_gen_method_info list ->                             *)
(*        Env.generic_code_gen_method_info list                              *)
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
    Format.eprintf "Generating Dk code for collection %a@."
      Sourcify.pp_vname collection_name ;
  (* Start the "Module" encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>(; Module %a. ;)@\n"
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
  Format.fprintf out_fmter "@[<2>def %a__effective_collection :=@ "
    Sourcify.pp_vname collection_name;
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    collection_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  (* We call the "implemented" collection generator, that is named by the
     implemented species name + ".collection_create". *)
  print_implemented_species_for_dk
    ~current_unit out_fmter implemented_species_name;
  Format.fprintf out_fmter "__collection_create";
  (* Finally, we must recover the arguments to apply to this collection
     generator. These arguments of course come from the species parameters the
     closed species we implement has (if it has some). We must make this
     application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE RIGHT ORDER ! *)
  (try
    let (_, implemented_species_methods, opt_params_info, _) =
      Env.DkGenEnv.find_species
        ~loc: collection_def.Parsetree.ast_loc ~current_unit
        implemented_species_name env in
    let substituted_implemented_methods =
      (match opt_params_info with
       | None ->
           (* The species has no collection generator. Hence it is not a fully
              defined species. This should have be prevented before, by
              forbidding to make a collection from a non fully defined
              species ! *)
           assert false (* [Unsure] car je crois qu'on n'a pas fait la vérif. *)
       | Some params_info ->
           (* Get the names of the collections or the value expressions
              effectively applied. *)
           let collection_body_params =
             Misc_common.get_implements_effectives
               collection_def.Parsetree.ast_desc.
                 Parsetree.cd_body.Parsetree.ast_desc.Parsetree.se_params
               params_info.Env.DkGenInformation.
                 cgi_implemented_species_params_names in
           let formals_to_effectives =
             map_formal_to_effective_in_collection
               ~current_unit: ctx.Context.scc_current_unit
               collection_body_params params_info in
           let record_type_args_instanciations =
             apply_collection_generator_to_parameters
               ctx env formals_to_effectives params_info in
           (* Close the pretty print box of the "effective_collection". *)
           Format.fprintf out_fmter ".@]@\n" ;
           (* And now, create the final value representing the effective
              instance of our collection, borrowing each field from the
              temporary value obtained above. This way, our collection will have
              ITS own record fields names, preventing the need to use those
              coming from the it implements. *)
           make_collection_effective_methods
             ctx env implemented_species_name
             collection_descr formals_to_effectives
             record_type_args_instanciations
             params_info.Env.DkGenInformation.cgi_generator_parameters.
               Env.DkGenInformation.cgp_abstr_param_methods_for_record ;
           substitute_formal_by_effective_in_coll_meths
             ~current_unit: ctx.Context.scc_current_unit
             implemented_species_name
             formals_to_effectives implemented_species_methods) in
    (* End of the pretty print box of the Module embedding the collection. *)
    Format.fprintf out_fmter "@]\n(; End %a. ;)@\n@\n"
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
