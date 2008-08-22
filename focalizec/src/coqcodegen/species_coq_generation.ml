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

(* $Id: species_coq_generation.ml,v 1.92 2008-08-22 14:10:55 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
    Coq of FoCaL's collections and species.                        *)
(* *************************************************************** *)



exception Attempt_proof_by_def_of_species_param of
  (Location.t * Parsetree.expr_ident)
;;


exception Attempt_proof_by_def_of_declared_method_of_self of
  (Location.t * Parsetree.expr_ident)
;;


exception Attempt_proof_by_def_of_local_ident of
  (Location.t * Parsetree.expr_ident)
;;


exception Attempt_proof_by_prop_of_local_ident of
  (Location.t * Parsetree.expr_ident)
;;


exception Attempt_proof_by_unknown_hypothesis of
  (Location.t * Parsetree.vname)
;;


exception Attempt_proof_by_unknown_step of
  (Location.t * Parsetree.node_label)
;;


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

    {b Rem} : Not exported outside this module.                             *)
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

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let find_compiled_field_memory name fields =
  let rec find = function
    | [] -> assert false
    | h :: q ->
        (begin
        match h with
         | Misc_common.CSF_let field_memory
         | Misc_common.CSF_theorem field_memory ->
             if field_memory.Misc_common.cfm_method_name = name then
               field_memory
             else find q
         | Misc_common.CSF_let_rec fields_memories ->
             (begin
             try
               List.find
                 (fun fm -> fm.Misc_common.cfm_method_name = name)
                 fields_memories
             with Not_found -> find q
             end)
         | _ -> find q
        end) in
  find fields
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
let generate_def_dependency_equivalence ctx generated_fields from name =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the application of the method generator. First, its name. *)
  let defined_from = from.Env.fh_initial_apparition in
  if (fst defined_from) <> ctx.Context.scc_current_unit then
    Format.fprintf out_fmter "%s." (fst defined_from) ;
  (* If the generator comes from another species, qualify by its module. *)
  if defined_from <> ctx.Context.scc_current_species then
    Format.fprintf out_fmter "%a."
      Parsetree_utils.pp_vname_with_operators_expanded
      (snd defined_from) ;
  (* Now, print the generator's name. *)
  Format.fprintf out_fmter "%a"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Now, recover from the already generated fields, what to apply to this
     generator. *)
  let memory = find_compiled_field_memory name generated_fields in
  (* We first instanciate the parameters corresponding to the carriers types 
     of species parameters and appearing in the method's type. *)
  List.iter
    (fun species_param_type_name ->
      (* Because species parameters' carriers are mapped to "_p_" + the
         species parameter's name + "T", we must apply the definition to
         arguments folowing this naming scheme. *)
      Format.fprintf out_fmter "@ _p_%a_T"
        Parsetree_utils.pp_vname_with_operators_expanded
        species_param_type_name)
    memory.Misc_common.cfm_used_species_parameter_tys ;
  (* Now apply the abstracted methods from the species params we depend on. *)
  List.iter
    (fun (species_param, meths_from_param) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
             Parsetree.Vuident n in
      let prefix =
        Parsetree_utils.name_of_vname species_param_name in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          Format.fprintf out_fmter "@ _p_%s_%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded
            meth)
        meths_from_param)
    memory.Misc_common.cfm_dependencies_from_parameters ;
  (* And finally, apply to the methods from ourselves we depend on. *)
  List.iter
    (fun dep_name ->
      if dep_name = (Parsetree.Vlident "rep") then
        Format.fprintf out_fmter "@ abst_T"
      else
        Format.fprintf out_fmter "@ abst_%a"
          Parsetree_utils.pp_vname_with_operators_expanded
          dep_name)
    memory.Misc_common.cfm_coq_min_typ_env_names
;;



(** Factorise la gen�ration des abstrations pour un champ d�fini. Ca colle
    donc les abstractions dues aux types des param�tres d'esp�ce, puis
    aux m�thodes des param�tres d'esp�ce dont on d�pend, puis enfin aux
    m�thodes de nous-m�mes dont on d�pend.

  Args:
    - [~in_section] : True when this function is called to generate abstractions
    in a Section. Is is only the case when used to generate the code of a
    temporary theorem for Zenon purpose. In this case, instead of abstracting
    dependencies by adding extra arguments to the current definition, we
    generate Variable, Let and Hypothesis.*)
let generate_field_definifion_prelude ~in_section ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields =
  let out_fmter = ctx.Context.scc_out_fmter in
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
          Format.fprintf out_fmter "Variable %s_T :@ Set.@\n" param_name
        else Format.fprintf out_fmter "@ (%s_T :@ Set)" param_name ;
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
      Types.cpc_collections_carrier_mapping =
        cc_mapping_extension @
          print_ctx.Types.cpc_collections_carrier_mapping } in
  (* Abstract according to the species's parameters the current method depends
     on. *)
  List.iter
    (fun (species_param, meths) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
             Parsetree.Vuident n in
      (* Each abstracted method will be named like "_p_", followed by the
         species parameter name, followed by "_", followed by the method's
         name.
         We don't care here about whether the species parameters is "in" or
         "is". *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, meth_ty) ->
          if in_section then
            Format.fprintf out_fmter "Variable %s%a :@ %a.@\n"
              prefix Parsetree_utils.pp_vname_with_operators_expanded meth
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: false)
              meth_ty
          else
            Format.fprintf out_fmter "@ (%s%a :@ %a)"
              prefix Parsetree_utils.pp_vname_with_operators_expanded meth
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: false)
              meth_ty)
        meths)
    dependencies_from_params ;
  (* Generate the parameters denoting methods of ourselves we depend on
     according the the minimal typing environment. *)
  let abstracted_methods =
    List.flatten
      (List.map
         (function
           | MinEnv.MCEE_Defined_carrier sch ->
               (* Now, if "rep" is defined, then we generate an equivalence
                  between "abst_T" and it's representation using the
                  abstracted types passed as arguments to the Definition to
                  represent carriers of the species parameters we depend on. *)
               let ty = Types.specialize sch in
               if in_section then
                 Format.fprintf out_fmter "Variable abst_T := %a.@\n"
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty
               else
                 Format.fprintf out_fmter "@ (abst_T := %a)"
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty ;
               (* Anything defined is not abstracted. *)
               []
           | MinEnv.MCEE_Defined_computational (fr, n, _, _, _)
           | MinEnv.MCEE_Defined_logical (fr, n, _) ->
               (* We must add an equivalence to enforce de def-dependency. *)
               if in_section then
                 Format.fprintf out_fmter "Let abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n
               else
                 Format.fprintf out_fmter "@ (abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n ;
               (* We must recover the method generator of the method we
                  def-depend on. Then we must apply this generator to the
                  arguments it needs, parameters carriers, parameters methods
                  and methods of Self depending on wether these last ones are
                  really defined in the current species or still abstract. *)
               generate_def_dependency_equivalence
                 new_ctx generated_fields fr n ;
               if in_section then Format.fprintf out_fmter ".@\n"
               else Format.fprintf out_fmter ")" ;
               []                  (* Anything defined is not abstracted. *)
           | MinEnv.MCEE_Declared_carrier ->
               (* Note that by construction, the carrier is first in the env. *)
               if in_section then
                 Format.fprintf out_fmter "Variable abst_T : Set.@\n"
               else Format.fprintf out_fmter "@ (abst_T : Set)" ;
               [Parsetree.Vlident "rep"]
           | MinEnv.MCEE_Declared_computational (n, sch) ->
               (* Due to a decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               if in_section then
                 Format.fprintf out_fmter "Variable abst_%a : %a.@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty
               else
                 Format.fprintf out_fmter "@ (abst_%a : %a)"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty ;
               [n]
           | MinEnv.MCEE_Declared_logical (n, b) ->
               if in_section then
                 Format.fprintf out_fmter "Hypothesis abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n
               else
                 Format.fprintf out_fmter "@ (abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n ;
               (* Methods from Self are printed "abst_XXX" since dependencies
                  have leaded to extra parameters "abst_XXX". *)
               Species_record_type_generation.generate_logical_expr
                 new_ctx ~local_idents: []
                 ~self_methods_status:
                   Species_record_type_generation.SMS_abstracted env b ;
               if in_section then Format.fprintf out_fmter ".@\n"
               else Format.fprintf out_fmter ")" ;
               [n])
         min_coq_env) in
  (abstracted_methods, new_ctx, new_print_ctx)
;;




(* Prints args and body with methodes abstracted by "abst_xxx". *)
let generate_defined_non_recursive_method_postlude ctx print_ctx env params
    scheme body =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Add the parameters of the let-binding with their type. *)
  (* Ignore the result type of the "let" if it's a function because we never
     print the type constraint on the result of the "let". We only print them
     in the arguments of the let-bound ident. *)
  (* Because methods are not polymorphic, one should never have instanciate
     variables. We just check for this. *)
  let (params_with_type, ending_ty_opt, instanciated_vars) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
      (Some scheme) params in
  assert (instanciated_vars = []) ;
  let ending_ty =
    (match ending_ty_opt with
     | None ->
         (* Because we always provide a type scheme (a [Some ...]), one must
            always be returned a type, i.e, something [Some ...].  *)
         assert false
     | Some t -> t) in
  (* We are printing each parameter's type. These types in fact belong to a
     same type scheme. Hence, they may share variables together.
     For this reason, we first purge the printing variable mapping and after,
     activate its persistence between each parameter printing. *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  List.iter
    (fun (param_vname, opt_param_ty) ->
      match opt_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true)
             param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Now, we print the ending type of the method. *)
  Format.fprintf out_fmter " :@ %a :=@ "
    (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true)
    ending_ty ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should not
     be useful because the other guys usign printing should manage this
     themselves (as we did just above by cleaning before activating the
     sharing), but anyway, it is safer an not costly. So... *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  (* Generates the body's code of the method.
     No local idents in the context because we just enter the scope of a species
     fields and so we are not under a core expression. Since we are generating
     a "let", methods from Self are printed "abst_XXX" since dependencies have
     leaded to "Variables abst_XXX" before this new "Variable". *)
  (match body with
   | Parsetree.BB_computational e ->
       Species_record_type_generation.generate_expr
         ctx ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         env e
   | Parsetree.BB_logical p ->
       Species_record_type_generation.generate_logical_expr
         ctx ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         env p)
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

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
let generate_defined_non_recursive_method ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields name
    body params scheme =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Start the Coq function definition. *)
  Format.fprintf out_fmter "@[<2>Definition %a"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Generate the prelude of the method, i.e the sequence of parameters induced
     by the various lamda-liftings and their types .
     By the way, we get updated in the [new_print_ctx] the way "Self" must be
     printed. *)
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_field_definifion_prelude
      ~in_section: false ctx print_ctx env min_coq_env
      used_species_parameter_tys dependencies_from_params generated_fields in
  (* We now generate the postlude of the method, i.e the sequence of real
     parameters of the method, not those induced by abstraction and finally
     the method's body. Inside, methods we depend on are abstracted by
     "abst_xxx". *)
  generate_defined_non_recursive_method_postlude
    new_ctx new_print_ctx env params scheme body ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n" ;
  abstracted_methods
;;



(** Finish the job of [instanciate_parameter_through_inheritance] in the
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
      ctx (Abstractions.EK_coq env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  (match instancied_with with
   | Misc_common.IPI_by_toplevel_species (spec_mod, spec_name) ->
       (* We found that a toplevel species provides this method because this
          species is finally used as effective parameter. However, may be the
          method on which we have a dependency is not directly in this
          toplevel species.
          May be it is in one of its parents. We must search in its inheritance
          to determine exactly in which species each method is REALLY defined
          (not only inherited). *)
       Parsetree_utils.DepNameSet.iter
         (fun (meth, _) ->
           let (real_spec_mod, real_spec_name) =
             Misc_common.find_toplevel_spe_defining_meth_through_inheritance
               (Abstractions.EK_coq env)
               ~current_unit ~start_spec_mod: spec_mod
               ~start_spec_name: spec_name
               ~method_name: meth in
           (* We directly access the species's Module method since in a fully
              defined toplevel species their is no "effective_collection". *)
           let prefix =
             if real_spec_mod = current_unit then real_spec_name ^ "."
             else
               real_spec_mod ^ "." ^ real_spec_name ^ "." ^ real_spec_mod ^
               "." in
           Format.fprintf out_fmter "@ %s%a"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       let prefix =
         if coll_mod = current_unit then
           coll_name ^ ".effective_collection.(" ^ coll_name ^ "."
         else coll_mod ^ "." ^ coll_name ^
           ".effective_collection.(" ^ coll_mod ^ "." ^ coll_name ^ "." in
       Parsetree_utils.DepNameSet.iter
         (fun (meth, _) ->
           (* Don't print the type to prevent being too verbose. *)
           Format.fprintf out_fmter "@ %srf_%a)"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Coq, species parameters are abstracted by "_p_species_xxx". *)
       let species_param_name =
         match prm with
          | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
          | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
              Parsetree.Vuident n in
       let prefix = (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
       Parsetree_utils.DepNameSet.iter
         (fun (meth, _) ->
           (* Don't print the type to prevent being too verbose. *)
           Format.fprintf out_fmter "@ _p_%s%a"
             prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
         meths_from_param)
;;



(** Finish the job of [instanciate_parameter_through_inheritance] in the
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
      ctx (Abstractions.EK_coq env) original_param_index
      field_memory.Misc_common.cfm_from_species.Env.fh_inherited_along in
  (* Now really generate the code of by what to instanciate. *)
  match instancied_with with
   | Misc_common.IPI_by_toplevel_species (spec_mod, spec_name) ->
       Format.fprintf out_fmter "@ " ;
       if spec_mod <> current_unit then
         Format.fprintf out_fmter "%s." spec_mod ;
       (* [Unsure] Eh oui, dans une toplevel species, on n'a pas de 
          "effective_collection" via lequel acc�der au champ repr�sentant
          "rep". Faudrait-il dans ce cas g�n�rer un champ repr�sentant la
          "rep", au cas o� quelqu'un chercherait � instancier par cette
          esp�ce toplevel compl�te ? *)
       Format.fprintf out_fmter "%s.??????(" spec_name ;
       if spec_mod <> current_unit then
         Format.fprintf out_fmter "%s." spec_mod ;
       Format.fprintf out_fmter "%s.rf_T" spec_name
   | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
       Format.fprintf out_fmter "@ " ;
       if coll_mod <> current_unit then
         Format.fprintf out_fmter "%s." coll_mod ;
       Format.fprintf out_fmter "%s.effective_collection.(" coll_name ;
       if coll_mod <> current_unit then
         Format.fprintf out_fmter "%s." coll_mod ;
       Format.fprintf out_fmter "%s.rf_T)" coll_name
   | Misc_common.IPI_by_species_parameter prm ->
       (* In Coq, species parameters are abstracted by "_p_species_xxx". *)
       let species_param_name =
         match prm with
          | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
          | Env.TypeInformation.SPAR_is ((_, n), _, _, _) -> n in
       Format.fprintf out_fmter "@ _p_%s_T" species_param_name
;;



let instanciate_parameter_through_inheritance ctx env field_memory =
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
      Sourcify.pp_ident host_ident (List.length original_host_species_params) ;
  (* We search the dependencies the original method had on its species
     parameters' methods. *)
  let meth_info =
    List.find
      (fun inf -> inf.Env.mi_name = field_memory.Misc_common.cfm_method_name)
      host_method_infos in
  if Configuration.get_verbose () then
    (begin
    Format.eprintf "Method '%a' has the following dependencies on parameters:@."
      Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
    List.iter
      (fun (species_param, meths_from_param) ->
        let species_param_name =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
               Parsetree.Vuident n in
        Format.eprintf "\t From parameter '%a', dependencies on methods: "
          Sourcify.pp_vname species_param_name;
        Parsetree_utils.DepNameSet.iter
          (fun (meth, _) -> Format.eprintf "%a " Sourcify.pp_vname meth)
          meths_from_param ;
        Format.eprintf "@.")
      meth_info.Env.mi_dependencies_from_parameters
    end) ;
  (* Since in Coq, types are explicit, now we apply to each extra parameter
     coming from the lambda liftings that represent the types of the species
     parameters used in the method. The applied stuf is not always "_p_" +
     the species name + "_T" since the species may have no parameters the
     parent one (from where the method generator comes) may have. For this
     reason, we must instanciate the original species parameters (i.e. the
     ones of the species from where the method generator comes). *)
  List.iter
    (fun species_param_type_name ->
      (* Since we are dealing with carrier types, we are only interested by
         IS parameters. IN parameters have their type abstracted only if it
         is the one of a IS parameter (hence, this last one is a IS and is
         found just as said above). If the IN parameter has the type of a
         toplevel species/collection, then this type is not abstrated, hence
         do not need to be instanciated ! *)
      let as_string = Parsetree_utils.name_of_vname species_param_type_name in
      let original_param_index =
        Handy.list_first_index
          (function
            | Env.TypeInformation.SPAR_in (_, _, _) -> false
            | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
                n = as_string)
          original_host_species_params in
      instanciate_IS_parameter_carrier_through_inheritance
        ctx env original_param_index field_memory)
    meth_info.Env.mi_used_species_parameter_tys ;
  (* Now, we address the instanciation of the species parameters' methods.
     For each species parameter, we must trace by what it was instanciated. *)
  List.iter
    (fun (species_param, meths_from_param) ->
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
           let number_meth =
             Parsetree_utils.DepNameSet.cardinal meths_from_param in
           assert (number_meth <= 1) ;
           if number_meth = 1 then
             (begin
             (* For substitution, we technically need to know in which
                compilation unit the parameter, hence in fact the species,
                was. *)
             let (original_param_unit, _) =
               field_memory.Misc_common.cfm_from_species.
                 Env.fh_initial_apparition in
             (* We get the FoCaL expression once substitutions are done. *)
             let instancied_expr =
               Misc_common.follow_instanciations_for_in_param
                 ctx (Abstractions.EK_coq env) param_name
                 original_param_unit original_param_index
                 field_memory.Misc_common.cfm_from_species.
                   Env.fh_inherited_along in
             (* We must now generate the Coq code for this FoCaL expression. *)
             Format.fprintf out_fmter "@ @[<1>(" ;
             Species_record_type_generation.generate_expr
               ctx ~local_idents: []
               (* Or whatever, "Self" will never appear at this point. *)
               ~self_methods_status:
                 Species_record_type_generation.SMS_abstracted
               env instancied_expr ;
             Format.fprintf out_fmter ")@]"
             end)
       | Env.TypeInformation.SPAR_is ((_, _), _, _, _) ->
           instanciate_IS_parameter_through_inheritance
             ctx env original_param_index field_memory meths_from_param )
    meth_info.Env.mi_dependencies_from_parameters
;;



(* ********************************************************************** *)
(** {b Descr} Generate the coq code for a non-recursive method of the
    current species.
    If the method is defined in this species, then it generates the
    method generator. If the method is inherited, it recovers the
    methods abstracted in the generator without generating again the code.
    And in any case, it generate the local definition "self_..." by
    applying the generator to the local definitions.

    {b Rem} Not exported outside this module.                             *)
(* ********************************************************************** *)
let generate_non_recursive_field_binding ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params
    generated_fields (from, name, params, scheme, body) =
  (* First of all, only methods defined in the current species must be
     generated. Inherited methods ARE NOT generated again ! *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_non_recursive_method
        ctx print_ctx env min_coq_env used_species_parameter_tys
        dependencies_from_params generated_fields name body params scheme
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited from species '%a' but not (re)-declared is \
          not generated again. @."
          Parsetree_utils.pp_vname_with_operators_expanded name
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
      (* Recover the arguments for abstracted methods *)
      (* of self in the inherited generator.          *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit
        from.Env.fh_initial_apparition name env
      end) in
  (* Finally, return the methods we found abstracted in the minimal Coq typing
     environment and that leaded to extra parameters via lambda-lifting. *)
  abstracted_methods
;;



(* ************************************************************************* *)
(** {b Descr} : Helper to find, during a Zenon proof by steps, an hypothesis
    by its name among the list of available material to do the proof.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let find_hypothesis_by_name name l =
  let rec rec_find = function
    | [] -> raise Not_found
    | h :: q ->
        (begin
        match h.Parsetree.ast_desc with
         | Parsetree.H_hypothesis (n, body) ->
             if n = name then body else rec_find q
         | _ -> rec_find q
        end) in
  rec_find l
;;



(** {b Descr} : Helper. *)
let rec find_assumed_variables_and_lemmas_in_hyps = function
  | [] -> ([], [])
  | h :: q ->
      (begin
      let (found_vars, found_lemmas) =
        find_assumed_variables_and_lemmas_in_hyps q in
      match h.Parsetree.ast_desc with
       | Parsetree.H_variable (n, ty_expr) ->
           (((n, ty_expr) :: found_vars), found_lemmas)
       | Parsetree.H_hypothesis (_, body) ->
           (found_vars, (body :: found_lemmas))
       | Parsetree.H_notation (_, _) -> (found_vars, found_lemmas)
      end)
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



let zenonify_by_definition ctx print_ctx env min_coq_env by_def_expr_ident =
  let out_fmter = ctx.Context.scc_out_fmter in
  match by_def_expr_ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       raise
         (Attempt_proof_by_def_of_local_ident
            (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
   | Parsetree.EI_global _qvname ->
       (* The stuff is in fact a toplevel definition, not a species method. *)
       failwith "[Unsure] You are may be using a toplevel definition in proof."
   | Parsetree.EI_method (qcollname_opt, vname) ->
       (begin
       match qcollname_opt with
        | None ->
            (begin
            (* The method comes from ourselves (Self). So we will search it
               inside the coq minimal typing environment. *)
            let method_info =
              MinEnv.find_coq_env_element_by_name vname min_coq_env in
            match method_info with
             | MinEnv.MCEE_Declared_carrier
             | MinEnv.MCEE_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | MinEnv.MCEE_Declared_computational (_, _)
             | MinEnv.MCEE_Declared_logical (_, _) ->
                 (* We can't prove "by definition" of something only
                    declared ! *)
                 raise
                   (Attempt_proof_by_def_of_declared_method_of_self
                      (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
             | MinEnv.MCEE_Defined_computational (_, _, params, scheme, body) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by definition of \
                   %a\". *)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident ;
                 Format.fprintf out_fmter "@[<2>Definition abst_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded
                   vname ;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx". *)
                 generate_defined_non_recursive_method_postlude
                   ctx print_ctx env params scheme body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
             | MinEnv.MCEE_Defined_logical (_, _, body) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by definition of \
                   %a\". *)@\n"
                   Sourcify.pp_expr_ident by_def_expr_ident ;
                 Format.fprintf out_fmter "@[<2>Definition abst_%a :=@ "
                   Parsetree_utils.pp_vname_with_operators_expanded vname ;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx". *)
                 Species_record_type_generation.generate_logical_expr
                   ctx ~local_idents: []
                   ~self_methods_status:
                     Species_record_type_generation.SMS_abstracted env
                   body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
            end)
        | Some (Parsetree.Qualified (_, _)) ->
            (* The method comes from another module's species. Hence it is for
               sure from a toplevel species. And this is not correct since the
               methods used for proofs must only come from our methods or
               species parameters' ones. *)
            failwith "I think this is a toplevel species method (1)."
        | Some (Parsetree.Vname _) ->
            (begin
            (* The method belongs to a species parameters. Since they are
               always abstract, it is forbidden to prove "by definition" of a
               species parameter method. *)
            raise
              (Attempt_proof_by_def_of_species_param
                 (by_def_expr_ident.Parsetree.ast_loc, by_def_expr_ident))
            end)
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
   | Parsetree.EI_global _qvname ->
       (* The stuff is in fact a toplevel definition, not a species method. *)
       failwith "[Unsure] You are may be using a toplevel definition in proof."
   | Parsetree.EI_method (qcollname_opt, vname) ->
       (begin
       match qcollname_opt with
        | None ->
            (begin
            (* The method comes from ourselves (Self). So we will search it
               inside the coq minimal typing environment. *)
            let method_info =
              MinEnv.find_coq_env_element_by_name vname min_coq_env in
            match method_info with
             | MinEnv.MCEE_Declared_carrier
             | MinEnv.MCEE_Defined_carrier _ ->
                 (* Syntax does not allow to mention "Self" as a proof
                    element. *)
                 assert false
             | MinEnv.MCEE_Declared_computational (_, scheme)
             | MinEnv.MCEE_Defined_computational (_, _, _, scheme, _) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by property %a\". *)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident ;
                 (* We just need to print the type of the method. *)
                 let meth_ty = Types.specialize scheme in
                 Format.fprintf out_fmter "@[<2>Parameter abst_%a :@ %a.@]@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded vname
                   (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false)
                   meth_ty
             | MinEnv.MCEE_Declared_logical (_, body)
             | MinEnv.MCEE_Defined_logical (_, _, body) ->
                 (* A bit of comment. *)
                 Format.fprintf out_fmter
                   "(* For method of Self used via \"by property %a\". *)@\n"
                   Sourcify.pp_expr_ident by_prop_expr_ident ;
                 (* We need to print the logical expression of the method. *)
                 Format.fprintf out_fmter "@[<2>Parameter abst_%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded vname ;
                 (* We now generate the sequence of real parameters of the
                    method, not those induced by abstraction and finally the
                    method's body. Inside, methods we depend on are abstracted
                    by "abst_xxx". *)
                 Species_record_type_generation.generate_logical_expr
                   ctx ~local_idents: []
                   ~self_methods_status:
                     Species_record_type_generation.SMS_abstracted
                   env body ;
                 (* Done... Then, final carriage return. *)
                 Format.fprintf out_fmter ".@]@\n"
            end)
        | Some qcollname ->
            (begin
            (* There is a qualification on the method. If this qualification
               does not have a module name or have a module name that is the
               same than the current compilation unit, then may be the method
               comes from a species parameter. *)
             let param_to_search_opt =
               (match qcollname with
                | Parsetree.Vname param_name ->
                    (* Implicitely in the current compilation unit. *)
                    Some param_name
                | Parsetree.Qualified (mod_name, param_name) ->
                    if mod_name = ctx.Context.scc_current_unit then
                      Some param_name
                    else None) in
             match param_to_search_opt with
              | None ->
                  (* The method comes from another module's species. Hence it
                     is for sure from a toplevel species. And this is not
                     correct since the methods used for proofs must only come
                     from our methods or species parameters' ones. *)
                  failwith "I think this is a toplevel species method (2)."
              | Some param_name ->
                  (* The method belongs to a species parameters. We first get
                     the species parameter's bunch of methods. *)
                  let param_meths =
                    Handy.list_assoc_custom_eq 
                      (fun spe_param searched ->
                        match spe_param with
                         | Env.TypeInformation.SPAR_in (_, _, _) ->
                             (* Proofs never use methods of "IN" parameters. *)
                             false
                         | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
                             (Parsetree.Vuident n) = searched)
                      param_name
                      dependencies_from_params in
                  (* Now, get the type of the specified method. *)
                  let (_, meth_ty) =
                    Parsetree_utils.depnameset_find
                      (fun (n, _) -> n = vname) param_meths in
                  (* A bit of comment. *)
                  Format.fprintf out_fmter
                    "(* For species parameter method used via \"by \
                    property %a\". *)@\n"
                    Sourcify.pp_expr_ident by_prop_expr_ident ;
                  (* The method is name by "_p_" + the species parameter's name
                     + "_" + the method's name. *)
                  Format.fprintf out_fmter "@[<2>Parameter _p_%a_%a :@ %a.@]@\n"
                    Parsetree_utils.pp_vname_with_operators_expanded param_name
                    Parsetree_utils.pp_vname_with_operators_expanded vname
                    (Types.pp_type_simple_to_coq print_ctx
                       ~reuse_mapping: false)
                    meth_ty
            end)
       end)
;;



(*
type zenon_proof_context = {
  zpc_section_name_seed : string ;
  zpc_hypotheses : Parsetree.hyp list ;
  zpc_steps : (Parsetree.node_label * (Parsetree.vname * )) list
} ;;
*)


type proof_step_availability = {
  psa_node_label : Parsetree.node_label ;
  psa_lemma_name : Parsetree.vname ;
  psa_base_logical_expr : Parsetree.logical_expr ;
  psa_assumed_variables : (Parsetree.vname * Parsetree.type_expr) list ;
  psa_assumed_lemmas : Parsetree.logical_expr list
} ;;



let add_qualtifications_and_implications ctx print_ctx env avail_info =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* First, quantify all the assumed variables. *)
  List.iter
    (fun (var_vname, ty_expr) ->
      (match ty_expr.Parsetree.ast_type with
       | Parsetree.ANTI_type ty ->
           Format.fprintf out_fmter "forall %a :@ %a,@ "
             Parsetree_utils.pp_vname_with_operators_expanded var_vname
             (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false) ty
       | _ -> assert false))
    avail_info.psa_assumed_variables ;
  (* Now, make a string of implications with the assumed logical expressions. *)
  let rec print_implications_string = function
    | [] -> ()
    | log_expr :: q ->
        Format.fprintf out_fmter "@[<2>" ;
        Species_record_type_generation.generate_logical_expr
          ctx ~local_idents: []
          ~self_methods_status: Species_record_type_generation.SMS_abstracted
          env log_expr ;
        Format.fprintf out_fmter " ->@ " ;
        print_implications_string q ;
        Format.fprintf out_fmter "@]" in
  print_implications_string avail_info.psa_assumed_lemmas
;;



(* *********************************************************************** *)
(** {b Descr} : Generate the Definition and Parameter for Coq that Zenon
    needs to automatically prove the current theorem. Methods used by the
    the proof are present in the minimal typing environment or the 
    parameters dependencies since they induce either a def or a
    decl-dependency. Hence, to recover them, we will search inside these 2
    kinds of information.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let zenonify_fact ctx print_ctx env min_coq_env dependencies_from_params
    available_hyps available_steps fact =
  let out_fmter = ctx.Context.scc_out_fmter in
  match fact.Parsetree.ast_desc with
   | Parsetree.F_definition expr_idents ->
       (* Syntax: "by definition ...". This leads to a Coq Definition. *)
       List.iter
         (zenonify_by_definition ctx print_ctx env min_coq_env) expr_idents
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
             Sourcify.pp_vname vname ;
           Format.fprintf out_fmter "@[<2>Parameter abst_%a :@ "
             Parsetree_utils.pp_vname_with_operators_expanded vname ;
           Species_record_type_generation.generate_logical_expr
             ctx ~local_idents: []
             ~self_methods_status:
               Species_record_type_generation.SMS_abstracted
             env hyp_logical_expr ;
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
             (fst node_label) (snd node_label) ;
           Format.fprintf out_fmter "@[<2>Parameter %a :@ "
             Parsetree_utils.pp_vname_with_operators_expanded
             avail_info.psa_lemma_name ;
           add_qualtifications_and_implications ctx print_ctx env avail_info ;
           (* Now, print the lemma's body. *)
           Species_record_type_generation.generate_logical_expr
             ctx ~local_idents: []
             ~self_methods_status: Species_record_type_generation.SMS_abstracted
             env avail_info.psa_base_logical_expr ;
           (* Done... Then, final carriage return. *)
           Format.fprintf out_fmter ".@]@\n")
         node_labels
;;



let zenonify_hyp ctx print_ctx env hyp =
  let out_fmter = ctx.Context.scc_out_fmter in
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_variable  (vname, type_expr) ->
       (begin
       match type_expr.Parsetree.ast_type with
        | Parsetree.ANTI_type ty ->
            (* Notation "assume ... in ...". This leads to a Variable in the
               current Coq Section. *)
            Format.fprintf out_fmter "@[<2>Variable %a :@ %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded vname
              (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false) ty
        | _ -> assert false
       end)
   | Parsetree.H_hypothesis (vname, logical_expr) ->
       (* Notation "H: all blabla in Self, foo -> bar...". This leads to a
          Variable in the current Coq Section. *)
       Format.fprintf out_fmter "@[<2>Variable %a :@ "
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       Species_record_type_generation.generate_logical_expr
         ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ctx env logical_expr ;
       Format.fprintf out_fmter ".@]@\n"
   | Parsetree.H_notation (vname, expr) ->
       (* Leads to a Definition. *)
       Format.fprintf out_fmter "@[<2>Let %a :=@ "
         Parsetree_utils.pp_vname_with_operators_expanded vname ;
       Species_record_type_generation.generate_expr
         ctx ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         env expr ;
       Format.fprintf out_fmter ".@]@\n"
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
    dependencies_from_params available_hyps available_steps section_name_seed
    parent_proof_opt node default_aim_name aim =
  let out_fmter = ctx.Context.scc_out_fmter in
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub ((label_num, label_name), stmt, proof) ->
       (begin
       (* Start a new nested Section to prove the statement. *)
       let section_name = section_name_seed ^ "_" ^ label_name in
       Format.fprintf out_fmter "@[<2>Section __%s.@\n" section_name ;
       let stmt_desc = stmt.Parsetree.ast_desc in
       (* First, generate the hypotheses of the statement. We also recover the
          "Hypothesis" of the statement because we will need to print them
          again later if they are mentionned as used in a [F_hypothesis] .*)
       List.iter (zenonify_hyp ctx print_ctx env) stmt_desc.Parsetree.s_hyps ;
       (* We extend the context of available hypothesis with the new ones. *)
       let available_hyps' = available_hyps @ stmt_desc.Parsetree.s_hyps in
       (* Finally, we deal with the conclusion of the statement. *)
       let new_aim =
         (match stmt_desc.Parsetree.s_concl with
          | None -> aim
          | Some logical_expr -> logical_expr) in
       (* Now, handle the nested proof of the conclusion of the statement or
          the default one if there is no new aim provided. *)
       let lemma_name = Parsetree.Vlident ("__" ^ section_name ^ "_LEMMA") in
       let inner_available_steps =
         zenonify_proof
           ~in_nested_proof: true ctx print_ctx env min_coq_env
           dependencies_from_params available_hyps' available_steps section_name
           new_aim lemma_name parent_proof_opt proof in
       Format.fprintf out_fmter "End __%s.@]@\n" section_name ;
       (* Since we end a Section, all the lemma will have now to be explicitely
          quantified/implified by the assumed variables and hypotheses. *)
       let (assumed_variables, assumed_lemmas) =
         find_assumed_variables_and_lemmas_in_hyps stmt_desc.Parsetree.s_hyps in
       (* Moreover, lemmas comming from inner proofs will also have to. Hence,
          since they are deeper, we add the current variables in front since
          they are abstracted first. Same thing for the abstracted lemmas, we
          add them in front since they will be a left-er part of implication. *)
       let inner_available_steps' =
         List.map
           (fun avail_info ->
             { avail_info with
               psa_assumed_variables =
                 assumed_variables @ avail_info.psa_assumed_variables ;
               psa_assumed_lemmas =
                 assumed_lemmas @ avail_info.psa_assumed_lemmas })
           inner_available_steps in
       (* We return the extra steps known thanks to the current [PN_sub] and
          its nested children. These extra steps will be made available for
          the rest of the surrounding proof and sibling [PN_sub]/[PN_qed]. *)
       let extra_available_steps =
         { psa_node_label = (label_num, label_name) ;
           psa_lemma_name = lemma_name ;
           psa_base_logical_expr = new_aim ;
           psa_assumed_variables = assumed_variables ;
           psa_assumed_lemmas = assumed_lemmas } :: inner_available_steps' in
       extra_available_steps
       end)
   | Parsetree.PN_qed ((_label_num, _label_name), proof) ->
       let extra_available_steps =
         zenonify_proof ~in_nested_proof ctx print_ctx env min_coq_env
           dependencies_from_params available_hyps available_steps
           section_name_seed aim default_aim_name parent_proof_opt proof in
       extra_available_steps



and zenonify_proof ~in_nested_proof ctx print_ctx env min_coq_env
    dependencies_from_params available_hyps available_steps section_name_seed
    aim aim_name parent_proof_opt proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed reason ->
       (* Proof is assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter
         "(* Proof assumed because \"%s\". *)@\n" reason ;
       Format.fprintf out_fmter "apply basics.magic_prove.@\nQed.@\n" ;
       available_steps
   | Parsetree.Pf_coq script ->
       (* Dump verbatim the Coq code. *)
       Format.fprintf out_fmter "%s@\n" script ;
       available_steps
   | Parsetree.Pf_node nodes ->
       (* For each successive node, we remember the previously seen **extra**
          steps that will be available for the trailing Qed node. *)
       let rec rec_dump accu_extra_avail_steps = function
         | [] -> accu_extra_avail_steps
         | node :: q ->
             let extra_avail_steps =
               zenonify_proof_node
                 ~in_nested_proof ctx print_ctx env min_coq_env
                 dependencies_from_params available_hyps
                 (available_steps @ accu_extra_avail_steps)
                 section_name_seed (Some proof) node aim_name aim in
             rec_dump (accu_extra_avail_steps @ extra_avail_steps) q in
       let extra_available_steps = rec_dump available_steps nodes in
       (available_steps @ extra_available_steps)
   | Parsetree.Pf_auto facts ->
       (* Generate Zenon's header. *)
       Format.fprintf out_fmter "%%%%begin-auto-proof@\n" ;
       (* Location is not the while theorem, but its body instead. I think
          this is sufficient *)
       Format.fprintf out_fmter "%%%%location: [%a]@\n"
         Location.pp_location aim.Parsetree.ast_loc ;
       Format.fprintf out_fmter "%%%%name: for_zenon_%a@\n@\n"
         Parsetree_utils.pp_vname_with_operators_expanded aim_name ;
       (* Now, print the lemma body. Inside, any method of "Self" is
          abstracted (without lambda-lift) and named "abst_xxx". That's why we
          use the mode [SMS_abstracted]. *)
       Format.fprintf out_fmter "(* Theorem's body. *)@\n" ;
       Species_record_type_generation.generate_logical_expr
         ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ctx env aim ;
       Format.fprintf out_fmter
         "@\n@\n(* Methods to use for automated proof. *)@\n" ;
       (* Now, print Definition and Hypothesis mentionned in the "by" clause
          without using the method generator. This means that one must
          "inline" the Definitions' bodies (in fact, for Hypothesis, since we
          have no body, only declaration, the inlining has no effect since they
          don't have method generator). If the list of facts is empty, this
          implicitely means "by all the previous steps of THIS proof level",
          i.e. by the [PN_sub]s of the closest [Pf_node] hosting us. And the
          closest [Pf_node] hosting us is in our parent proof. *)
       let real_facts =
         (match facts with
          | [] ->
              let parent_proof_nodes =
                (match parent_proof_opt with
                 | None -> assert false
                 | Some p ->
                     match p.Parsetree.ast_desc with
                      | Parsetree.Pf_node ns -> ns
                      | _ -> assert false) in
              (* Make a pseudo list with all the encountered steps (node
                 labels). *)
              [{ Parsetree.ast_loc = Location.none ;
                 Parsetree.ast_desc =
                   Parsetree.F_node
                     (find_only_PN_subs_in_proof_nodes parent_proof_nodes) ;
                 Parsetree.ast_doc = [] ;
                 Parsetree.ast_type = Parsetree.ANTI_non_relevant }]
          | _ -> facts) in
       List.iter
         (zenonify_fact
            ctx print_ctx env min_coq_env dependencies_from_params
            available_hyps available_steps)
         real_facts ;
       (* End of Zenon stuff. *)
       Format.fprintf out_fmter "%%%%end-auto-proof@\n" ;
       (* Now, let's print the theorem/lemma and prove it unless we are at
          toplevel (i.e. not in a nested proof). In this last case, this will
          be done directly by [generate_defined_theorem]. *)
       if in_nested_proof then
         (begin
         Format.fprintf out_fmter "@[<2>Theorem %a :@ "
           Parsetree_utils.pp_vname_with_operators_expanded aim_name ;
         Species_record_type_generation.generate_logical_expr
           ~local_idents: []
           ~self_methods_status: Species_record_type_generation.SMS_abstracted
           ctx env aim ;
         Format.fprintf out_fmter ".@]@\n" ;
         (* Apply Zenon's result to prove the lemma... *)
         Format.fprintf out_fmter "apply for_zenon_%a ;@\nauto.@\nQed.@\n"
           Parsetree_utils.pp_vname_with_operators_expanded aim_name
         end) ;
       available_steps
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
    used_species_parameter_tys dependencies_from_params generated_fields
    name logical_expr proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Local function that prints the common stuff required by Zenon in case
     of proofs done by [Pf_auto] or [Pf_node]. It prints the opening of the
     main Section and the theorem. *)
  let print_common_prelude_for_zenon () =
    Format.fprintf out_fmter "(* Section for proof of theorem '%a'. *)@\n"
      Parsetree_utils.pp_vname_with_operators_expanded name ;
    (* Start the Section. *)
    Format.fprintf out_fmter "@[<2>Section Proof_of_%a.@\n"
      Parsetree_utils.pp_vname_with_operators_expanded name ;
    (* We must now dump Variables, Let and Hypothesis for all the things we
       usually lambda-lift in a regular definition of method. This is due to
       the fact that here we still use the Section mechanism. Hence, we do
       the same job than for regular field definition prelude but changing
       abstractions that are performed by extra parameters by Variable, Let
       or Hypothesis. *)
    ignore
      (generate_field_definifion_prelude
         ~in_section: true ctx print_ctx env min_coq_env
         used_species_parameter_tys dependencies_from_params
         generated_fields) in
  (* *********************** *)
  (* Start really the job... *)
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed _ | Parsetree.Pf_coq _ ->
       () (* No Section needed. *)
   | Parsetree.Pf_node _ | Parsetree.Pf_auto _ ->
       (begin
       (* Generate the common code for proofs done by Zenon either by [Pf_auto]
          of by [Pf_node]. *)
       print_common_prelude_for_zenon () ;
       (* Get the stuff to add to the current collection carrier mapping to
          make so the type expressions representing some species parameter
          carrier types, will be automatically be mapped onto our freshly
          created extra args. The trailing "_T" will be automatically added
          by the type printing routine.
          In fact, thsi process is already done by the function
          [generate_field_definifion_prelude] but we don't remind it. So we need
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
           Types.cpc_collections_carrier_mapping =
             cc_mapping_extension @
               print_ctx.Types.cpc_collections_carrier_mapping } in
       (* Create a unique name seed for Sections of this theorem. *)
       let section_name_seed =
         String.uppercase (Handy.int_to_base_26 (section_gen_sym ())) in
       (* Handle the proof, telling not to print the Theorem's body once the
          auto-proof is ended because this will be done directly by
          [generate_defined_theorem]. *)
       ignore
         (zenonify_proof ~in_nested_proof: false ctx print_ctx env min_coq_env
            dependencies_from_params
            [(* No available hypothesis at the beginning. *)]
            [(* No available steps at the beginning. *)] section_name_seed
            logical_expr name
            None (* No parent proof at the beginning. *)
            proof) ;
       (* End the Section. *)
       Format.fprintf out_fmter "End Proof_of_%a.@]@\n@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name
       end)
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
let generate_defined_theorem ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields
    from name logical_expr proof =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Put an extra newline before the theorem to make some air ! *)
  Format.fprintf out_fmter "@\n(* From species %a. *)@\n"
    Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
  (* Create the Section for Zenon in order to build the temporary theorem that
     will be used with its proof to build the REAL and proved theorem. This
     does the work only if the theorem must be proved by Zenon. Otherwise, it
     does nothing ! *)
  generate_theorem_section_if_by_zenon
    ctx print_ctx env min_coq_env used_species_parameter_tys
    dependencies_from_params generated_fields name logical_expr proof ;
  (* Now, generate the real theorem, using the temporarily created and applying
     the proof. *)
  Format.fprintf out_fmter "@[<2>Theorem " ;
  Format.fprintf out_fmter "%a "
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Generate the prelude of the method, i.e the sequence of parameters and
     their types induced by the various lamda-liftings. *)
  let (abstracted_methods, new_ctx, _) =
    generate_field_definifion_prelude
      ~in_section: false ctx print_ctx env min_coq_env
      used_species_parameter_tys dependencies_from_params generated_fields in
  Format.fprintf out_fmter "@ :@ " ;
  (* Finally, the theorem itself. Inside, any method of "Self" is abstracted
     (i.e. is lambda-lifted), hence named "abst_xxx". That's why we use the
     mode [SMS_abstracted]. *)
  Species_record_type_generation.generate_logical_expr
    ~local_idents: []
    ~self_methods_status: Species_record_type_generation.SMS_abstracted
    new_ctx env logical_expr ;
  Format.fprintf out_fmter ".@]@\n" ;
  (* End the proof matter. *)
  (match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed reason ->
       (* Proof assumed, then simply use "magic_prove". *)
       Format.fprintf out_fmter
         "(* Proof assumed because \"%s\". *)@\n" reason ;
       Format.fprintf out_fmter "apply basics.magic_prove.@\nQed.@\n"
   | Parsetree.Pf_auto _  | Parsetree.Pf_node _ ->
       (* Proof done by Zenon. Apply the temporary theorem. *)
       Format.fprintf out_fmter
         "apply for_zenon_%a ;@\nauto.@\nQed.@\n"
         Parsetree_utils.pp_vname_with_operators_expanded name
   | Parsetree.Pf_coq script ->
       (* Dump verbatim the Coq code. *)
       Format.fprintf out_fmter "%s@\n" script) ;
  abstracted_methods
;;




let generate_theorem ctx print_ctx env min_coq_env used_species_parameter_tys
    dependencies_from_params generated_fields (from, name, logical_expr) proof =
  (* A "theorem" defined in the species leads to a Coq "Theorem". *)
  let abstracted_methods =
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      generate_defined_theorem
        ctx print_ctx env min_coq_env used_species_parameter_tys
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
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
      (* Recover the arguments for abstracted methods of self in the inherited
         generator. *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit
        from.Env.fh_initial_apparition name env
      end) in
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
    | [(_, one)] -> 
        let ty = match one with None -> assert false | Some t -> t in
        Format.fprintf out_fmter "%a"
          (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty
    | (_, h) :: q ->
        let ty = match h with None -> assert false | Some t -> t in
        Format.fprintf out_fmter "@[<1>(prod %a@ "
          (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty ;
        rec_print q ;
        Format.fprintf out_fmter ")@]" in
  rec_print types
;;

 
 
let generate_termination_proof _ctx _print_ctx _env _name = function
  | None -> ()
(*     "Variable self_term_order_%a"
     "Variable self_term_obl_%a" *)
  | Some _termination_proof -> ()
     (* Abstraction par lambda *)
(*     "Definition %a__term_order_%a"
     "Let self_term_order_%a" *)

     (* Abstraction par Variable *)
(*     "Section"
     "Theorem %a__term_obl_%a %a"
     "End"

     "Let self_term_obl_%a " *)
;;



let generate_recursive_let_definition ctx print_ctx env generated_fields l =
  let out_fmter = ctx.Context.scc_out_fmter in
  match l with
   | [] ->
       (* A "let", then a fortiori "let rec" construct *)
       (* must at least bind one identifier !          *)
       assert false
   | [((from, name, params, scheme, body, _, _), ai)] ->
       (begin
       match body with
        | Parsetree.BB_logical _ ->
            (* [Unsure] *)
            failwith "recursive logical : TODO"
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
            (* Open the "Section" for the recursive definition. *)
            Format.fprintf out_fmter
              "@[<2>Section %a.@\n"
              Parsetree_utils.pp_vname_with_operators_expanded name ;
            (* Now, generate the prelude of the only method introduced by
               "let rec". *)
            let all_deps_from_params =
              Abstractions.merge_abstraction_infos
                ai.Abstractions.ai_dependencies_from_params_via_body
                (Abstractions.merge_abstraction_infos
                   ai.Abstractions.ai_dependencies_from_params_via_type
                   ai.Abstractions.ai_dependencies_from_params_via_completion)
            in
            let (abstracted_methods, new_ctx, new_print_ctx) =
              generate_field_definifion_prelude
                ~in_section: false ctx' print_ctx
                env ai.Abstractions.ai_min_coq_env
                ai.Abstractions.ai_used_species_parameter_tys
                all_deps_from_params generated_fields in
            (* We now generate the order. It always has 2 arguments having the
               same type. This type is a tuple if the method hase several
               arguments. *)
            Format.fprintf out_fmter
              "@\n@\n(* Abstracted termination order. *)@\n" ;
            Format.fprintf out_fmter "@[<2>Variable __term_order@ :@ " ;
            let (params_with_type, return_ty_opt, _) =
              MiscHelpers.bind_parameters_to_types_from_type_scheme
                (Some scheme) params in
            let return_ty =
              match return_ty_opt with None -> assert false | Some t -> t in
            (*  *)
            Types.purge_type_simple_to_coq_variable_mapping () ;
            (* Print the tuple that is the method's arguments' types. *)
            Format.fprintf out_fmter "%a -> %a -> Prop.@]@\n"
              (print_types_as_tuple_if_several new_print_ctx) params_with_type
              (print_types_as_tuple_if_several new_print_ctx) params_with_type ;






            (* We now prove that this order is well-founded. *)
            Types.purge_type_simple_to_coq_variable_mapping () ;
            (* Compute the recursive calls information to generate the
               termination proof obligation. *)
            let recursive_calls =
              Recursion.list_recursive_calls name params [] body_expr in
            (* The Variable representing the termination proof obligation... *)
            Format.fprintf out_fmter
              "@[<2>Variable __term_obl :" ;
            (* It's now time to generate the lemmas proving that each
               recursive call decreases. Each of then will be followed by
               a /\ to make the conjunction of all of them. And the latest one
               will be used to add the final "well_founded __term_order" to
               this big conjunction. *)
            Rec_let_gen.generate_termination_lemmas
              new_ctx new_print_ctx env recursive_calls ;
            (* Alway end by the obligation of well-formation of the order. *)
            Format.fprintf out_fmter "@ (well_founded __term_order).@]@\n@\n" ;
            (* Generate the recursive uncurryed function *)
            Format.fprintf out_fmter
              "@[<2>Function %a@ (__arg:@ %a)@ \
              {wf __term_order __arg}:@ %a@ :=@ @[<2>let (%a) :=@ __arg in@]@ "
              Parsetree_utils.pp_vname_with_operators_expanded name
              (print_types_as_tuple_if_several new_print_ctx) params_with_type
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: true)
              return_ty
              (Handy.pp_generic_separated_list ","
                Parsetree_utils.pp_vname_with_operators_expanded) params ;
            Species_record_type_generation.generate_expr
              new_ctx ~local_idents: []
              ~self_methods_status:
                Species_record_type_generation.SMS_abstracted
              env body_expr ;
            Format.fprintf out_fmter ".@]@\n" ;
            Format.fprintf out_fmter "@[<v 2>Proof.@ %a Qed.@]@\n"
              (Handy.pp_generic_n_times ((List.length recursive_calls) + 1)
                Format.fprintf)
              "coq_builtins.prove_term_obl __term_obl.@ " ;




            (* Generate the curryed version *)
            Format.fprintf out_fmter "@[Definition %a__%a %a :=@ %a (%a).@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded species_name
              Parsetree_utils.pp_vname_with_operators_expanded name
              (Handy.pp_generic_separated_list " "
                Parsetree_utils.pp_vname_with_operators_expanded) params
              Parsetree_utils.pp_vname_with_operators_expanded name
              (Handy.pp_generic_separated_list ","
                Parsetree_utils.pp_vname_with_operators_expanded) params ;
            (* Finally close the opened "Section". *)
            Format.fprintf out_fmter "End %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded name ;
            let compiled = {
              Misc_common.cfm_from_species = from ;
              Misc_common.cfm_method_name = name ;
              Misc_common.cfm_method_scheme = scheme ;
              Misc_common.cfm_used_species_parameter_tys =
                ai.Abstractions.ai_used_species_parameter_tys ;
              Misc_common.cfm_dependencies_from_parameters =
                all_deps_from_params ;
              Misc_common.cfm_coq_min_typ_env_names = abstracted_methods } in
            Misc_common.CSF_let_rec [compiled]
       end)
   | _ :: _ -> raise Recursion.MutualRecursion
;;



(** generated_fields : The list of previous fields of the species that have
    already be generated. Used while generating theorems to know what to apply
        to the methods generators the theorem depends on. *)
let generate_methods ctx print_ctx env generated_fields = function
  | Abstractions.FAI_sig (from, name, sch) ->
      (* Only declared, hence, no code to generate yet ! *)
      if Configuration.get_verbose () then
        Format.eprintf "Coq code for signature '%a' leads to void code.@."
          Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* Nothing very exciting to keep for the collection generator. *)
      let compiled_field = {
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = sch ;
        (* Since no code is generated for "sig", no need to get bored with
           species parameters carriers that may appear in the type of the
           "sig". *)
        Misc_common.cfm_used_species_parameter_tys = [] ;
        (* Since the "sig " has no code, it can't refer to parameters'
           methods ! *)
        Misc_common.cfm_dependencies_from_parameters = [] ;
        (* Since the "sig " has no code, it can't refer to some of our
           methods ! *)
        Misc_common.cfm_coq_min_typ_env_names = [] } in
      Misc_common.CSF_sig compiled_field
  | Abstractions.FAI_let ((from, name, params, scheme, body, _, _),
                          abstraction_info) ->
      let all_deps_from_params =
        Abstractions.merge_abstraction_infos
          abstraction_info.Abstractions.ai_dependencies_from_params_via_body
          (Abstractions.merge_abstraction_infos
             abstraction_info.Abstractions.ai_dependencies_from_params_via_type
             abstraction_info.Abstractions.
             ai_dependencies_from_params_via_completion) in
      (* No recursivity, then the method cannot call itself in its body then
         no need to set the [scc_lambda_lift_params_mapping] of the context. *)
      let coq_min_typ_env_names =
        generate_non_recursive_field_binding
          ctx print_ctx env abstraction_info.Abstractions.ai_min_coq_env
          abstraction_info.Abstractions.ai_used_species_parameter_tys
          all_deps_from_params generated_fields
          (from, name, params, scheme, body) in
      (* Now, build the [compiled_field_memory], even if the method was not
         really generated because it was inherited. *)
      let compiled_field = {
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = scheme ;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Abstractions.ai_used_species_parameter_tys ;
        Misc_common.cfm_dependencies_from_parameters = all_deps_from_params ;
          Misc_common.cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
      Misc_common.CSF_let compiled_field
  | Abstractions.FAI_let_rec l ->
      generate_recursive_let_definition ctx print_ctx env generated_fields l
  | Abstractions.FAI_theorem ((from, name, sch, logical_expr, pr, _),
                              abstraction_info) ->
      let all_deps_from_params =
        Abstractions.merge_abstraction_infos
          abstraction_info.Abstractions.ai_dependencies_from_params_via_body
          (Abstractions.merge_abstraction_infos
             abstraction_info.Abstractions.ai_dependencies_from_params_via_type
             abstraction_info.Abstractions.
             ai_dependencies_from_params_via_completion) in
      let coq_min_typ_env_names =
        generate_theorem
          ctx print_ctx env abstraction_info.Abstractions.ai_min_coq_env
          abstraction_info.Abstractions.ai_used_species_parameter_tys
          all_deps_from_params generated_fields (from, name, logical_expr)
          pr in
      let compiled_field = {
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = sch ;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Abstractions.ai_used_species_parameter_tys ;
        Misc_common.cfm_dependencies_from_parameters = all_deps_from_params ;
        Misc_common.cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
      Misc_common.CSF_theorem compiled_field
  | Abstractions.FAI_property ((from, name, sch, _, _), abstraction_info) ->
      (* "Property"s are discarded. However we compute their dependencies. *)
      let all_deps_from_params =
        Abstractions.merge_abstraction_infos
           abstraction_info.Abstractions.ai_dependencies_from_params_via_body
           (Abstractions.merge_abstraction_infos
              abstraction_info.Abstractions.ai_dependencies_from_params_via_type
              abstraction_info.Abstractions.
                ai_dependencies_from_params_via_completion) in
      let compiled_field = {
        Misc_common.cfm_from_species = from ;
        Misc_common.cfm_method_name = name ;
        Misc_common.cfm_method_scheme = sch ;
        Misc_common.cfm_used_species_parameter_tys =
          abstraction_info.Abstractions.ai_used_species_parameter_tys ;
        Misc_common.cfm_dependencies_from_parameters = all_deps_from_params ;
        Misc_common.cfm_coq_min_typ_env_names = [] } in
      Misc_common.CSF_property compiled_field
;;



(* ************************************************************************ *)
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
      | Env.TypeInformation.SPAR_is ((_, carrier_name), _, _, _) ->
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
      (fun accu_env (m_name, _) -> Env.CoqGenEnv.add_value m_name 0 accu_env)
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
       | Env.TypeInformation.SPAR_is ((_, param_name), _, param_methods, _) ->
           let methods_names =
             Dep_analysis.ordered_names_list_of_fields param_methods in
           (* A "IS" parameter is a collection. Hence it is fully instanciated
              and doesn't have anymore lifted extra parameters. Then the
              built [method_info] is trivially empty about
              [mi_dependencies_from_parameters] and [mi_abstracted_methods]. *)
           let bound_methods =
             List.map
               (fun (n, _) -> {
                 Env.mi_name = n ;
                 Env.mi_history = {
                   Env.fh_initial_apparition = current_species ;
                   Env.fh_inherited_along = [] } ;
                 Env.mi_used_species_parameter_tys = [] ;
                 Env.mi_dependencies_from_parameters = [] ;
                 Env.mi_abstracted_methods = [] })
               methods_names in
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



(* [Unsure] Comme en OCaml sauf que l'on traite aussi les
  theorem et property. *)
(* *********************************************************************** *)
(* Format.formatter -> compiled_species_fields list ->                     *)
(*  (Parsetree.vname * Parsetree_utils.DepNameSet.t) list                  *)
(** {b Descr} : Dumps as OCaml code the parameters required to the
         collection generator in order to make them bound in the
         collection generator's body. These parameters come from
         the methods of the species parameters that some of our methods
         depend on. This means that a closed species with no species
         parameters will have NO extra parameters in its collection
         generator.

         This function must UNIQUELY find the names of all the extra
         parameters the methods will need to make them arguments of the
         collection generator and record then in a precise order that must
         be made public for the guys who want to instanciate the collection.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let dump_collection_generator_arguments_for_params_methods out_fmter
    compiled_species_fields =
  (* Let's create an assoc list mapping for each species paramater name the
     set of methods names from it that needed to be lambda-lifted, hence that
     will lead to parameters of the collection generator. *)
  let species_param_names_and_methods =
    ref ([] : (Parsetree.vname * Parsetree_utils.DepNameSet.t ref) list) in
  (* ************************************************************************ *)
  (** {b Descr} :  Local function to process only one [compiled_field_memory].
         Handy to factorize the code in both the cases of [CSF_let] and
         [CSF_let_rec]. This function effectivly accumulates by side effect
         for each species parameter the set of methods we depend on.

      { b Rem} : Local to the enclosing [dump_collection_generator_arguments]
               function. Not exported.                                        *)
  (* ************************************************************************ *)
  let rec process_one_field_memory field_memory =
    List.iter
      (fun (spe_param, meths_set) ->
        match spe_param with
         | Env.TypeInformation.SPAR_in (_, _, _) ->
             (* Attention, as previously said, "IN" parameters are handled
                among the species parameters CARRIERS extra arguments. So we
                now skip them to prevent having them twice ! *)
             ()
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
             (* Recover the species parameter's name. *)
             let spe_param_name = Parsetree.Vuident n in
             (* Get or create for this species parameter name, the bucket
                recording all the methods someone depends on.
                We don't care here about whether the species parameters is
                "IN" or "IS". *)
             let spe_param_bucket =
               (try List.assoc spe_param_name !species_param_names_and_methods
               with Not_found ->
                 let bucket = ref Parsetree_utils.DepNameSet.empty in
                 species_param_names_and_methods :=
                   (spe_param_name, bucket) ::
                   !species_param_names_and_methods ;
                 bucket) in
             (* And now, union the current methods we depend on with the
                already previously recorded. *)
             spe_param_bucket :=
               Parsetree_utils.DepNameSet.union meths_set !spe_param_bucket)
      field_memory.Misc_common.cfm_dependencies_from_parameters in

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
  (* Now we get the assoc list complete, we can dump the parameters of the
     collection generator. To make them correct with their usage inside the
     local functions of the collection generator, we must give them a name
     shaped in the same way, i.e:
     "_p_" + species parameter name + "_" + called method name. *)
  List.iter
    (fun (species_param_name, meths_set) ->
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
        "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        !meths_set)
  !species_param_names_and_methods ;
  (* Finally, make this parameters information public by returning it. By the
     way, the ref on the inner set is not anymore needed, then remove it. *)
  List.map
    (fun (sp_par_name, meths_set) -> (sp_par_name, !meths_set))
    !species_param_names_and_methods
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
         | Types.CCMI_in _ ->
             (begin
             (* We generate the parameter (that's not its TYPE !). *)
             (Env.ScopeInformation.SPK_in, (Parsetree.Vlident param_name))
             end))
      ctx.Context.scc_collections_carrier_mapping in
  params_carriers_abstr_for_record
;;



(** {b Descr} : Realyl prints the code for arguments computed by
    [remind_collection_generator_arguments_for_params_carriers]. *)
let dump_collection_generator_arguments_for_params_carriers out_fmter lst =
  List.iter
    (fun (param_kind, param_name) ->
      match param_kind with
       | Env.ScopeInformation.SPK_is ->
           (* We need to enforce the type for Coq because I don't know what. *)
           Format.fprintf out_fmter "@ (_p_%a_T : Set)"
             Parsetree_utils.pp_vname_with_operators_expanded param_name ;
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
    (fun (species_param_name, meths) ->
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths)
    abstracted_params_methods_in_record_type
;;




let generate_collection_generator ctx env compiled_species_fields
    abstracted_params_methods_in_record_type =
  let current_species_name = snd ctx.Context.scc_current_species in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "@\nSpecies %a is fully defined. Generating its collection generator@."
      Sourcify.pp_vname current_species_name ;


  (* ******************************************************************* *)
  (** {b Descr} : A local function to process one field. This allows to
                factorize the processing for both [Let] and [Let_rec]
                [Property] and [Theorem]�definitions.

      {b Rem} : Local to the [generate_collection_generator] function.
               Not exported.                                             *)
  (* ******************************************************************* *)
  let process_one_field field_memory =
    let from = field_memory.Misc_common.cfm_from_species in
    Format.fprintf out_fmter "(* From species %a. *)@\n"
      Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
    Format.fprintf out_fmter "@[<2>let local_%a :=@ "
      Parsetree_utils.pp_vname_with_operators_expanded
      field_memory.Misc_common.cfm_method_name ;
    if Configuration.get_verbose () then
      Format.eprintf "Generating Coq code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this
       inheritance level or if it was inherited from another species. *)
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      (begin
      if Configuration.get_verbose () then
        Format.eprintf
          "Method '%a' not inherited, building method generator using \
          abstracted local species parameters as arguments.@."
          Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
      (* It comes from the current inheritance level. Then its name is simply
         the the method's name. *)
      Format.fprintf out_fmter "%a"
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments induced
         by the various lambda-lifting we previously performed.
         First, the species parameters carriers we used. *)
      List.iter
        (fun species_param_type_name ->
          Format.fprintf out_fmter "@ _p_%a_T"
            Parsetree_utils.pp_vname_with_operators_expanded
            species_param_type_name)
        field_memory.Misc_common.cfm_used_species_parameter_tys ;
      (* Next, the extra arguments due to the species parameters methods we
         depends on. Here we will not use them to lambda-lift them this time,
         but to apply them ! The name used for application is formed according
         to the same scheme we used at lambda-lifting time:
         "_p_" + species parameter name + "_" + called method name. *)
      List.iter
        (fun (species_param, meths_from_param) ->
          (* Recover the species parameter's name. *)
          let species_param_name =
            match species_param with
             | Env.TypeInformation.SPAR_in (n, _, _) -> n
             | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
                 Parsetree.Vuident n in
          (* We don't care here about whether the species parameters is "IN"
             or "IS". *)
          let prefix =
            "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
            "_" in
          Parsetree_utils.DepNameSet.iter
            (fun (meth, _) ->
              (* Don't print the type to prevent being too verbose. *)
              Format.fprintf out_fmter "@ %s%a"
                prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
            meths_from_param)
        field_memory.Misc_common.cfm_dependencies_from_parameters
(* [Unsure] Euh, tiens, avant on appliquait aussi aux trucs de
   Misc_common.cfm_coq_min_typ_env_names. Ce ne serait pas un oubli ici ?
En fait, non, je ne pense pas car on est dans le cas o� la m�thode n'est
pas inherit�e, donc dans la version courante de la m�thode, on a d�j�
trait� les methodes de nous dont on d�pend... *)
      end)
    else
      (begin
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
        Format.fprintf out_fmter "%s.@," defined_from_mod ;
      Format.fprintf out_fmter "%a.@,%a"
        Parsetree_utils.pp_vname_with_operators_expanded defined_from_species
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments induced
         by the various lambda-lifting we previously in the species from which
         we inherit, i.e. where the method was defined.
         During the inheritance, parameters have been instanciated. We must
         track these instanciations to know to what apply the method
         generator. *)
      instanciate_parameter_through_inheritance ctx env field_memory
      end) ;
    (* Now, apply the method generator to each of the extra arguments induced
       by the various lambda-lifting we previously performed. Second, the
       methods of our inheritance tree we depend on and that are only declared.
       These methods leaded to "local" functions defined above. Hence, for
       each  method only declared of ourselves we depend on, its name is
       "local_" + the method's name. *)
    List.iter
      (fun n ->
        Format.fprintf out_fmter "@ local_%a"
          Parsetree_utils.pp_vname_with_operators_expanded n)
      field_memory.Misc_common.cfm_coq_min_typ_env_names ;
    (* That's all for this field code generation. *)
    Format.fprintf out_fmter "@ in@]@\n" ;
    if Configuration.get_verbose () then
      Format.eprintf "End of Coq code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name in

  (* *********************** *)
  (* Now, let's really work. *)
  (* A little comment in the generated Coq code. *)
  Format.fprintf out_fmter
    "@\n(* Fully defined '%a' species's collection generator. *)@\n"
    Sourcify.pp_vname current_species_name ;
  (* The generic name of the collection generator: the species' name +
     "_collection_create". *)
  Format.fprintf out_fmter "@[<2>Definition collection_create" ;
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
    out_fmter params_carriers_abstr_for_record ;
  (* Generate the parameters the collection generator needs to build each of
     the current species's local function (functions corresponding to the
     actual method stored in the collection record).
     These parameters of the generator come from the abstraction of methods
     coming from our species parameters we depend on. By the way, recover the
     list of species parameters linked together with their methods we need to
     instanciate in order to apply the collection generator. *)
  let abstr_params_methods_in_coll_gen =
    dump_collection_generator_arguments_for_params_methods
      out_fmter compiled_species_fields in
  Format.fprintf out_fmter " :=@ " ;
  (* Generate the local functions that will be used to fill the record value. *)
  List.iter
    (function
      | Misc_common.CSF_sig field_memory ->
          (* We handle "rep" apart. *)
          if field_memory.Misc_common.cfm_method_name =
             Parsetree.Vlident "rep" then
            (begin
             let (type_from_scheme, generalized_instanciated_vars) =
               Types.specialize_n_show_instanciated_generalized_vars
                 field_memory.Misc_common.cfm_method_scheme in
             (* Because "rep" is never polymorphic, its type must never
                contain instanciated variables coming from the scheme. *)
             assert (generalized_instanciated_vars = []) ;
             let print_ctx = {
               Types.cpc_current_unit = ctx.Context.scc_current_unit ;
               Types.cpc_current_species =
               Some
                 (Parsetree_utils.type_coll_from_qualified_species
                    ctx.Context.scc_current_species) ;
               Types.cpc_collections_carrier_mapping =
                 (* Prefix all the "IS" mappings by "_p_" to use the parameters
                    declared in the collection generator's header. *)
                 make_carrier_mapping_using_lambda_lifts
                   ctx.Context.scc_collections_carrier_mapping } in
             Format.fprintf out_fmter "@[<2>let local_rep :=@ %a in@]@\n"
               (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false)
               type_from_scheme
            end)
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
     "mk_record". So we juste now need to apply then since they are
     parameters (with the same names) of the collection generator we are
     building. *)
  dump_collection_generator_arguments_for_params_carriers
    out_fmter params_carriers_abstr_for_record ;
  (* Now, print the names of parameters that must be provided when using the
     collection generator to represent methods of the species parameters that
     are abstracted and used by the local "local_xxx" used to create the
     collection generator. *)
  build_collection_generator_arguments_for_params_methods
    out_fmter abstracted_params_methods_in_record_type ;
  (* Then, always the "local_rep" since the first record field represents what
     is to be the species carrier (foo_T :> Set.). *)
  Format.fprintf out_fmter "@ local_rep" ;
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
    compiled_species_fields ;
  (* Close the pretty-print box of the "Let collection_create ... :=". *)
  Format.fprintf ctx.Context.scc_out_fmter ".@]@\n" ;
  ((* Parameters induced by parameters carriers and used to instanciate the
      parameters of "mk_record"...*)
   params_carriers_abstr_for_record,
   (* Arguments of the collection generator that correspond the the species's
      parameters methods we depend on. *)
   abstr_params_methods_in_coll_gen)
;;




let species_compile env ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the chapter encapsulating the species representation. *)
  let module_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>Module %s.@\n" module_name ;
  (* Now, establish the mapping between collections available and the names
     representing their carrier for the record type. *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping_for_record ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, species_name) ;
    Context.scc_dependency_graph_nodes = dep_graph ;
    Context.scc_species_parameters_names =
      species_descr.Env.TypeInformation.spe_sig_params ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* Insert in the environment the value bindings of the species methods and
     the species bindings for its parameters. This is needed since in Coq
     polymorphism is explicit, hence we need to know for each method the extra
     arguments it needs. *)
  let env' =
    extend_env_for_species_def
      ~current_species: (current_unit, species_name) env species_descr in
  (* The record type representing the species' type. We get the parameters the
     record type has. *)
  let abstracted_params_methods_in_record_type =
    Species_record_type_generation.generate_record_type
      ctx env' species_descr in
  (* [Unsure] pourquoi ne pas fouttre le 1er carrier mapping local � la
     fonction [generate_record_type] ? *)
  (* Now, we don't need anymore the collections_carrier_mapping since fields
     will do their own one. *)
  let ctx' = { ctx with Context.scc_collections_carrier_mapping = [] } in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx'.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx'.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping = [] } in
  (* Now, the methods of the species. *)
  let field_abstraction_infos =
    Abstractions.compute_abstractions_for_fields
      ~with_def_deps: true (Abstractions.EK_coq env')
      ctx' species_descr.Env.TypeInformation.spe_sig_methods in
  (* Now, generate the Coq code of the methods. *)
  let compiled_fields =
    List.fold_left
      (fun accu field ->
        (* Pass the accu to be able to remind the already generated fields. *)
        let compiled_field = generate_methods ctx' print_ctx env' accu field in
        (* Not efficient, but required to keep the fields in the right order. *)
        accu @ [compiled_field])
      []
      field_abstraction_infos in
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
         The obtained list of extra parameters due to species parameters.
         First, the parameters induced by parameters carriers abstracted the
         record type (useful to build a complete record type expression).
         Note that in this first stuff, the carriers of the "IN" parameters
         are here !
         Second, we get the abstracted methods from parameters we depend on.
         These methods parametrize the collection generator and will have to
         be provided when creatign a collection. *)
      let (
        (* Parameters induced by parameters' carriers abstracted in the record
           type. They will be to instanciate to use "mk_record" or create a
           valid record type expression. *)
        params_carriers_abstr_for_record,
        (* Only methods of the params required for the collection generator
           application. They correspond to the parameters methods that have
           been abstracted inside the collection generator. Inside, there are
           missing the carrier abstractions of the parameters. But they can be
           recovered with the above [params_carriers_abstr_for_record]. *)
        abstr_params_methods_in_coll_gen) =
        generate_collection_generator
          ctx env' compiled_fields abstracted_params_methods_in_record_type in
      let coll_gen_params_info = {
        Env.CoqGenInformation.cgp_abstr_param_carriers_for_record =
        (* Just remove the "IN"/"IS" tag that is not needed to keep in the code
           generation environment. *)
          List.map snd params_carriers_abstr_for_record ;
        Env.CoqGenInformation.cgp_abstr_param_methods_for_record =
          abstracted_params_methods_in_record_type ;
        Env.CoqGenInformation.cgp_abstr_param_methods_for_coll_gen =
          abstr_params_methods_in_coll_gen } in
      Some
        { Env.CoqGenInformation.cgi_implemented_species_params_names =
            species_params_names_n_kinds ;
          Env.CoqGenInformation.cgi_generator_parameters =
            coll_gen_params_info }
      end)
    else None in
  (* The end of the module hosting the species. *)
  Format.fprintf out_fmter "@]\nEnd %s.@\n@\n" module_name ;
  (* Now, extract the fields names to create the [species_binding_info]. *)
  let species_binding_info =
    List.flatten
      (List.map
         (function
           | Misc_common.CSF_sig compiled_field_memory
           | Misc_common.CSF_let compiled_field_memory
           | Misc_common.CSF_theorem compiled_field_memory ->
               [{ Env.mi_name =
                    compiled_field_memory.Misc_common.cfm_method_name ;
                  Env.mi_history =
                    compiled_field_memory.Misc_common.cfm_from_species ;
                  Env.mi_used_species_parameter_tys =
                    compiled_field_memory.Misc_common.
                      cfm_used_species_parameter_tys ;
                  Env.mi_dependencies_from_parameters =
                    compiled_field_memory.Misc_common.
                      cfm_dependencies_from_parameters ;
                  Env.mi_abstracted_methods =
                    compiled_field_memory.Misc_common.
                      cfm_coq_min_typ_env_names }]
           | Misc_common.CSF_let_rec compiled_field_memories ->
               List.map
                 (fun cfm ->
                   { Env.mi_name = cfm.Misc_common.cfm_method_name ;
                     Env.mi_history = cfm.Misc_common.cfm_from_species ;
                     Env.mi_used_species_parameter_tys =
                       cfm.Misc_common.cfm_used_species_parameter_tys ;
                     Env.mi_dependencies_from_parameters =
                       cfm.Misc_common.cfm_dependencies_from_parameters ;
                     Env.mi_abstracted_methods =
                       cfm.Misc_common.cfm_coq_min_typ_env_names })
                 compiled_field_memories
           | Misc_common.CSF_property compiled_field_memory ->
               [ { Env.mi_name =
                     compiled_field_memory.Misc_common.cfm_method_name ;
                   Env.mi_history =
                     compiled_field_memory.Misc_common.cfm_from_species ;
                   Env.mi_used_species_parameter_tys =
                     compiled_field_memory.Misc_common.
                       cfm_used_species_parameter_tys ;
                   Env.mi_dependencies_from_parameters =
                     compiled_field_memory.Misc_common.
                       cfm_dependencies_from_parameters ;
                   (* For properties, this list should always be [] since *)
                   (* we do not compute the visible universe since it is  *)
                   (* never used.                                         *)
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
         Format.fprintf out_fmter "%s." fname ;
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



(** {Descr} : Prints the instanciation of parameters' carriers that were
    abstracted. *)
let print_record_type_carriers_args_instanciations ctx env args_instanciations =
  let out_fmter = ctx.Context.scc_out_fmter in
  List.iter
    (function
      | RTAI_by_is (corresponding_effective_opt_fname,
                    corresponding_effective_vname) ->
          (match corresponding_effective_opt_fname with
           | Some fname -> Format.fprintf out_fmter "%s." fname
           | None -> ()) ;
          Format.fprintf out_fmter "@ %a.effective_collection.@[<1>("
            Parsetree_utils.pp_vname_with_operators_expanded
            corresponding_effective_vname ;
          (match corresponding_effective_opt_fname with
           | Some fname -> Format.fprintf out_fmter "%s." fname
           | None -> ()) ;
          Format.fprintf out_fmter "%a.rf_T)@]"
            Parsetree_utils.pp_vname_with_operators_expanded
            corresponding_effective_vname
      | RTAI_by_in expr ->
          Format.fprintf out_fmter "@ " ;
          Species_record_type_generation.generate_expr
            ctx ~local_idents: []
            ~self_methods_status: Species_record_type_generation.SMS_from_record
            env expr)
    args_instanciations
;;




(** {Descr} : Prints the instanciation of parameters' methods that were
    abstracted. *)
let print_methods_from_params_instanciations ctx env formal_to_effective_map l =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Species parameters we have dependencies on. *)
  List.iter
    (fun (formal_species_param_name, method_names) ->
      match List.assoc formal_species_param_name formal_to_effective_map with
       | Misc_common.CEA_collection_name_for_is corresponding_effective ->
           (begin
           let
               (corresponding_effective_opt_fname,
                corresponding_effective_vname) =
             match corresponding_effective with
              | Parsetree.Vname n -> (None, n)
              | Parsetree.Qualified (m, n) -> ((Some m), n) in
           Parsetree_utils.DepNameSet.iter
             (fun (meth_name, _) ->
               (* If needed, qualify the name of the species in the Coq code.
                  Don't print the type to prevent being too verbose. *)
               (match corresponding_effective_opt_fname with
                | Some fname -> Format.fprintf out_fmter "%s." fname
                | None -> ()) ;
               (* Species name + ".effective_collection.". *)
               Format.fprintf out_fmter "@ %a.effective_collection.(@[<1>"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname ;
               (* If needed, qualify the name of the species in the Coq code. *)
               (match corresponding_effective_opt_fname with
                | Some fname -> Format.fprintf out_fmter "%s." fname
                | None -> ()) ;
               (* Species name.rf_method name. *)
               Format.fprintf out_fmter "%a.rf_%a)@]"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname
                 Parsetree_utils.pp_vname_with_operators_expanded meth_name)
             method_names
           end)
       | Misc_common.CEA_value_expr_for_in expr ->
           (begin
           Format.fprintf out_fmter "(@[<1>" ;
           (* No local idents in the context because we just enter the scope
              of a species fields and so we are not under a core expression.
              For [~self_as], same thing, no relevant value since the
              application of the generator should not involve any other
              expressions than methods/theorems identifiers. *)
           Species_record_type_generation.generate_expr
             ctx ~local_idents: []
             ~self_methods_status:
               (* Or what you prefer. *)
               Species_record_type_generation.SMS_abstracted
             env expr ;
           Format.fprintf out_fmter ")@]" ;
           end))
    l
;;




let apply_collection_generator_to_parameters ctx env formal_to_effective_map
    col_gen_info =
  let col_gen_params_info =
    col_gen_info.Env.CoqGenInformation.cgi_generator_parameters in
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
        | Misc_common.CEA_collection_name_for_is corresponding_effective ->
            (begin
            match corresponding_effective with
             | Parsetree.Vname n -> RTAI_by_is (None, n)
             | Parsetree.Qualified (m, n) -> RTAI_by_is ((Some m), n)
            end)
        | Misc_common.CEA_value_expr_for_in expr -> RTAI_by_in expr)
      col_gen_params_info.Env.CoqGenInformation.
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
    col_gen_params_info.Env.CoqGenInformation.
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
       if fname <> current_unit then Format.fprintf out_fmter "%s." fname ;
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
;;



(* ******************************************************************** *)
(** {b Descr} : Creates the effective value of the collection's record.
    The record value borrows every fields from the temporary value
    ("__implemented") generated by the collection generator.
    In order to select a field of the "__implemented", i.e. to perform a
    projection on the "__implemented" record type, we must remember the
    effective types parametrising this type.
    Form instance:

      species Foo0 (A0 is Sp0) = ... ;;
      species Coll implements Foo0 (Csp0) ;;

    leads to the following Coq code:

      Record Foo0 (A0_T : Set) : Type :=
        mk_Foo0 {
        Foo0_T :> Set ;
        (* From species collgen_for_coq#Foo0. *)
        Foo0_v : basics.int__t
        }.
      ...
      Record Coll : Type :=
        mk_Coll {
        Coll_T :> Set ;
        (* From species collgen_for_coq#Foo0. *)
        Coll_v : basics.int__t
        }.

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
          mk_Coll self_T __implemented.(Foo0_v Csp0_effective_collection).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let make_collection_effective_record ctx env implemented_species_name
    collection_descr formals_to_effectives record_type_args_instanciations
    record_type_args_instanciations2 =
  let out_fmter = ctx.Context.scc_out_fmter in
  let current_unit = ctx.Context.scc_current_unit in
  (* The header of the record. *)
  Format.fprintf out_fmter "@[<2>mk_record" ;
  (* Now, always applying to the representation of "Self", i.e "rf_T". *)
  Format.fprintf out_fmter "@ t.@[<1>(" ;
  print_implemented_species_as_coq_module
    ~current_unit out_fmter implemented_species_name ;
  Format.fprintf out_fmter ".rf_T" ;
  (* Apply to the instanciations of the parameters carriers. *)
  print_record_type_carriers_args_instanciations
    ctx env record_type_args_instanciations ;
  (* Apply to the instanciations of the parameters methods we depend on. *)
  print_methods_from_params_instanciations
     ctx env formals_to_effectives record_type_args_instanciations2 ;
  Format.fprintf out_fmter ")@]" ;
  List.iter
    (function
      | Env.TypeInformation.SF_sig (_, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _, _) -> ()
      | Env.TypeInformation.SF_theorem (_, n, _, _, _, _)
      | Env.TypeInformation.SF_let (_, n, _, _, _, _, _) ->
          Format.fprintf out_fmter "@ t.@[<1>(" ;
          print_implemented_species_as_coq_module
            ~current_unit out_fmter implemented_species_name ;
          Format.fprintf out_fmter ".rf_%a"
            Parsetree_utils.pp_vname_with_operators_expanded n ;
          (* Apply to the instanciations of the parameters carriers. *)
          print_record_type_carriers_args_instanciations
            ctx env record_type_args_instanciations ;
          (* Apply to the instanciations of the parameters methods we depend
              on. *)
          print_methods_from_params_instanciations
            ctx env formals_to_effectives
            record_type_args_instanciations2 ;
          Format.fprintf out_fmter ")@]"
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, n, _, _, _, _, _) ->
              Format.fprintf out_fmter "@ t.@[<1>(" ;
              print_implemented_species_as_coq_module
                ~current_unit out_fmter implemented_species_name ;
              Format.fprintf out_fmter ".rf_%a"
                Parsetree_utils.pp_vname_with_operators_expanded n ;
              (* Apply to the instanciations of the parameters carriers. *)
              print_record_type_carriers_args_instanciations
                ctx env record_type_args_instanciations ;
              (* Apply to the instanciations of the parameters methods we
                 depend on. *)
              print_methods_from_params_instanciations
                 ctx env formals_to_effectives
                 record_type_args_instanciations2 ;
              Format.fprintf out_fmter ")@]")
            l)
    collection_descr.Env.TypeInformation.spe_sig_methods ;
  (* Close the pretty-print box of the "effective_collection". *)
  Format.fprintf out_fmter ".@]@\n"
;;



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
      col_gen_params_info.Env.CoqGenInformation.
      cgi_implemented_species_params_names
      collection_body_params
  with Invalid_argument "List.map2" ->
    assert false  (* The lists length must be equal. *)
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
    Sourcify.pp_vname collection_name ;
  (* Now, establish the mapping between collections available and the names
     representing their carrier. *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping_for_record
      ~current_unit collection_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, collection_name) ;
    Context.scc_dependency_graph_nodes = dep_graph ;
    (* A collection never has parameter. *)
    Context.scc_species_parameters_names = [] ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* The record type representing the collection's type. Ignore the parameters
     needed to make the record value, they will be recovered via the
     implemented species. *)
  ignore
    (Species_record_type_generation.generate_record_type
      ctx env collection_descr) ;
  (* We do not want any collection generator. Instead, we will call the
     collection generator of the collection we implement and apply it to the
     functions it needs coming from the collection applied to its parameters
     if there are some. *)
  Format.fprintf out_fmter "@[<2>Let effective_collection :=@ " ;
  (* The temporary value resulting from the application of the collection
     generator mentionned just above... *)
  Format.fprintf out_fmter "@[<2>let t :=@\n" ;
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    collection_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  (* We call the "implemented" collection generator, that is named by the
     implemented species name + ".collection_create". *)
  print_implemented_species_for_coq
    ~current_unit out_fmter implemented_species_name ;
  Format.fprintf out_fmter ".collection_create" ;
  (* Finally, we must recover the arguments to apply to this collection
     generator. These arguments of course come from the species parameters the
     closed species we implement has (if it has some). We must make this
     application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE  RIGHT ORDER ! *)
  (begin
  try
    let (_, _, opt_params_info, _) =
      Env.CoqGenEnv.find_species
        ~loc: collection_def.Parsetree.ast_loc ~current_unit
        implemented_species_name env in
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
             ~current_unit: ctx.Context.scc_current_unit collection_body_params
             params_info in
         let record_type_args_instanciations =
           apply_collection_generator_to_parameters
             ctx env formals_to_effectives params_info in
         (* Close the pretty print box of the "t". *)
         Format.fprintf out_fmter "@ in @]@\n" ;
         (* And now, create the final value representing the effective
            instance of our collection, borrowing each field from the
            temporary value obtained above. This way, our collection will have
            ITS own record fields names, preventing the need to use those
            coming from the it implements. *)
         make_collection_effective_record
           ctx env implemented_species_name
           collection_descr formals_to_effectives
           record_type_args_instanciations
           params_info.Env.CoqGenInformation.cgi_generator_parameters.
             Env.CoqGenInformation.cgp_abstr_param_methods_for_record) ;
    (* Close thre pretty print box of the "effective_collection". *)
    Format.fprintf out_fmter "@]" ;
    (* End of the pretty print box of the Module embedding the collection. *)
    Format.fprintf out_fmter "@]\nEnd %a.@\n@\n"
      Sourcify.pp_vname collection_name
  with Not_found ->
    (* Don't see why the species could not be present in the environment. The
       only case would be to make a collection from a collection since
       collection are never entered in the environment because it's a non sense
       to make a collection "implementing" a collection ! *)
    assert false
  end)
;;
