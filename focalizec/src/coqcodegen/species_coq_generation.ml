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

(* $Id: species_coq_generation.ml,v 1.10 2008-01-04 15:32:49 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
              Coq of FoCaL's collections and species.            *)
(* *************************************************************** *)




(* [Unsure] Comme pour OCaml ! Factoriser ! Nettoyer !!! *)
type let_connector =
  | LC_first_non_rec   (** The binding is the first of a non-recursive
                           definition. *)
  | LC_first_rec   (** The binding is the first of a recursive definition. *)
  | LC_following   (** The binding is not the first of a multiple definition
                       (don't matter if the definition is recursive or not). *)
;;



(* [Unsure] Comme pour OCaml ! Factoriser ! Nettoyer !!! *)
type compiled_field_memory = {
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_species ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's body. *)
  cfm_method_body  : Parsetree.expr ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and the second is the set of methods the current
      method depends on from this species parameter.*)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ;
  (** The methods of our inheritance tree the method depends on. *)
  cfm_decl_children :
    (Dep_analysis.name_node * Dep_analysis.dependency_kind) list ;
  (* Tell if the method's type involves "Self", hence if it has an extra
      Coq argument to represent the type of "Self". *)
  cfm_need_self_extra_arg : bool
} ;;



(* [Unsure] Quasiment comme pour OCaml ! Factoriser ! Nettoyer !!! En plus, incomplet ! *)
type compiled_species_fields =
  | CSF_sig of Parsetree.vname
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of Parsetree.vname
  | CSF_property of Parsetree.vname
;;




(* [Unsure] Quasiment comme pour OCaml ! Factoriser ! Nettoyer !!! *)
(* Return whether the method's type uses "Self", then whether it has an
   extra first argument that is "abst_speciesname : Set". This must be
   reminded because when applying the method generator, this extra argument
   will have to be provided. *)
let generate_one_field_binding ctx print_ctx env ~let_connect
    params_llifted _dependencies_from_params decl_children
    (from, name, params, scheme, body) =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* We need to check if at least one of these parameter has a type using *)
  (* "Self". In this case, "Self" being represented as "asbt_T", (i.e. a  *)
  (* type variable in OCaml), we need to make the polymorphism explicit   *)
  (* in Coq, that is, add an extra param "(asbt_T : Set)" in head of the  *)
  (* remaining parameters.                                                *)
  let need_self_extra_arg =
    List.exists (fun (_, t) -> Types.refers_to_self_p t) params_llifted in
  (* First of all, only methods defined in the current species must *)
  (* be generated. Inherited methods ARE NOT generated again !      *)
  if from = ctx.Species_gen_basics.scc_current_species then
    (begin
    (* Just a bit of debug. *)
    if Configuration.get_verbose () then
      Format.eprintf "Generating Coq code for field '%a'.@."
        Parsetree_utils.pp_vname_with_operators_expanded name ;
    let species_name = snd ctx.Species_gen_basics.scc_current_species in
    (* Start the Coq function definition. *)
    (match let_connect with
     | LC_first_non_rec ->
         (* Beware that the definition corresponding to the *)
         (* method outside the record type has 2 "_"'s !    *)
         Format.fprintf out_fmter "@[<2>Definition %a__%a"
           Parsetree_utils.pp_vname_with_operators_expanded species_name
           Parsetree_utils.pp_vname_with_operators_expanded name
     | LC_first_rec ->
         (* [Unsure] *)
         (*
         Format.fprintf out_fmter "@[<2>let rec %a"
           Parsetree_utils.pp_vname_with_operators_expanded name
         *)
         failwith "TODO 1"
     | LC_following ->
         (* [Unsure] *)
         (*
         Format.fprintf out_fmter "@[<2>and %a"
           Parsetree_utils.pp_vname_with_operators_expanded name) ;
         *)
         failwith "TODO 2") ;
    (* Check if an extra parameter is required to represent "Self"'s type. *)
    if need_self_extra_arg then Format.fprintf out_fmter "@ (abst_T : Set)" ;
    (* Now, output the extra parameters induced by the lambda liftings *)
    (* we did because of the species parameters and our dependencies.  *)
    List.iter
      (fun (param_name, param_ty) ->
        Format.fprintf out_fmter "@ (%s : %a)" param_name
          (Types.pp_type_simple_to_coq
             print_ctx ~reuse_mapping: false ~self_as: Types.CSR_abst)
          param_ty)
      params_llifted ;
    (* Add the parameters of the let-binding with their type.   *)
    (* Ignore the result type of the "let" if it's a function   *)
    (* because we never print the type constraint on the result *)
    (* of the "let". We only print them in the arguments of the *)
    (* let-bound ident.                                         *)
    (* Because methods are not polymorphic, one should never    *)
    (* have instanciate variables. We just check for this.      *)
    let (params_with_type, ending_ty_opt, instanciated_vars) =
      Misc_ml_generation.bind_parameters_to_types_from_type_scheme
        scheme params in
    assert (instanciated_vars = []) ;
    let ending_ty =
      (match ending_ty_opt with
       | None ->
           (* Because we always provide a type scheme (a [Some ...]), one *)
           (* must always be returned a type, i.e, something [Some ...].  *)
           assert false
       | Some t -> t) in
    (* We are printing each parameter's type. These types in fact belong *)
    (* to a same type scheme. Hence, they may share variables together.  *)
    (* For this reason, we first purge the printing variable mapping and *)
    (* after, activate its persistence between each parameter printing.  *)
    Types.purge_type_simple_to_coq_variable_mapping () ;
    List.iter
      (fun (param_vname, opt_param_ty) ->
        match opt_param_ty with
         | Some param_ty ->
             Format.fprintf out_fmter "@ (%a : %a)"
               Parsetree_utils.pp_vname_with_operators_expanded param_vname
               (Types.pp_type_simple_to_coq
                  print_ctx ~reuse_mapping: true ~self_as: Types.CSR_abst)
               param_ty
         | None ->
             Format.fprintf out_fmter "@ %a"
               Parsetree_utils.pp_vname_with_operators_expanded param_vname)
      params_with_type ;
    (* Now, we print the ending type of the method. *)
    Format.fprintf out_fmter " :@ %a :=@ "
      (Types.pp_type_simple_to_coq
         print_ctx ~reuse_mapping: true ~self_as: Types.CSR_abst)
      ending_ty ;
    (* Now we don't need anymore the sharing. Hence, clean it. This should *)
    (* not be useful because the other guys usign printing should manage   *)
    (* this themselves (as we did just above by cleaning before activating *)
    (* the sharing), but anyway, it is safer an not costly. So...          *)
    Types.purge_type_simple_to_coq_variable_mapping () ;
    (* Generates the body's code of the method.                       *)
    (* No local idents in the context because we just enter the scope *)
    (* of a species fields and so we are not under a core expression. *)
    Species_record_type_generation.generate_expr
      ctx ~local_idents: [] ~self_as: Types.CSR_abst env body ;
    (* Done... Then, final carriage return. *)
    Format.fprintf out_fmter ".@]@\n" ;
    (* Now, generate the "Let self_..." by applying this method generator.   *)
    (* It is required in case where a "property" uses the current method. In *)
    (* effect, because in "property"s we don't lambda-lift, to keep late     *)
    (* binding, "property"s always use the "self_..." Coq "Variable"         *)
    (* representing the method.                                              *)
    Format.fprintf out_fmter "@[<2>Let self_%a :=@ %a__%a"
      Parsetree_utils.pp_vname_with_operators_expanded name
      Parsetree_utils.pp_vname_with_operators_expanded species_name
      Parsetree_utils.pp_vname_with_operators_expanded name ;
    (* If required, apply the above method generator to the  *)
    (* extra argument that represents "Self" : i.e "self_T". *)
    if need_self_extra_arg then Format.fprintf out_fmter "@ self_T" ;
    (* Now, apply to each extra parameter coming from the lambda liftings. *)
    (* First, the extra arguments due to the species parameters methods we *)
    (* depends on. They are names parameter name + "_" + method name.      *)
(* [Unsure] TODO. En plus, il faut penser à rajouter 1 Variable par méthode
   des paramètres de l'espèce (dont 1 méthode au moins de l'espèce courante
   dépend serait certainement une bonne optim. *)
    (* Next, the extra arguments due to methods of ourselves we depend on. *)
    (* They are always present in the species under the name "self_...".   *)
    List.iter
      (fun ({ Dep_analysis.nn_name = dep_name }, _) ->
        Format.fprintf out_fmter "@ self_%a"
          Parsetree_utils.pp_vname_with_operators_expanded dep_name)
      decl_children ;
    Format.fprintf out_fmter ".@]@\n"
    end)
  else
    (begin
    (* Just a bit of debug/information if requested. *)
    if Configuration.get_verbose () then
      Format.eprintf
        "Field '%a' inherited but not (re)-declared is not generated again.@."
        Parsetree_utils.pp_vname_with_operators_expanded name
    end) ;
  need_self_extra_arg
;;



let generate_methods ctx print_ctx env species_parameters_names field =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  match field with
   | Env.TypeInformation.SF_sig (from, name, sch) ->
       (* "rep" is specially handled before, then ignore it now. *)
       if (Parsetree_utils.name_of_vname name) <> "rep" then
         (begin
         (* Because methods are not polymorphic, we take the shortcut not *)
         (* to verify if the need extra parameters to the type due to     *)
         (* polymorphism.                                                 *)
         let ty = Types.specialize sch in
         Format.fprintf out_fmter "(* From species %a. *)@\n"
           Sourcify.pp_qualified_species from ;
         (* Only declared method. Hence appears as a "Variable". In OCaml *)
         (* "sig"s are ignored and methods using them are lambda-lifted.  *)
         (* In Coq, we also lambda-lift this way methods, so the "sig"s   *)
         (* could seem to be ignored. However, it's impossible to lambda  *)
         (* lift in "property"s. So the variable is still needed. It will *)
         (* then be automatically abstracted by Coq in the "property".    *)
         (* Then, for methods generators where lambda-abstraction has     *)
         (* been done, we will apply these generators to this variable.   *)
         (* [Unsure] En fait, comme on n'utilise les générateurs de       *)
         (* méthodes dans les générateurs de collection et qu'à ce moment *)
         (* toutes les methodes sont définies, si ça se trouve on n'aura  *)
         (* même pas à utiliser la Variable, maisd directement la VRAIE   *)
         (* méthodes.                                                     *)
         Format.fprintf out_fmter
           "@[<2>Variable self_%a :@ %a.@]@\n"
           Parsetree_utils.pp_vname_with_operators_expanded name
           (Types.pp_type_simple_to_coq
              print_ctx ~reuse_mapping: false ~self_as: Types.CSR_self) ty
         end) ;
       (* Nothing to keep for the collection generator. *)
       CSF_sig name
   | Env.TypeInformation.SF_let (from, name, params, scheme, body) ->
       let (dependencies_from_params, decl_children, llift_params) =
         Misc_ml_generation.compute_lambda_liftings_for_field
           ~current_species: ctx.Species_gen_basics.scc_current_species
           species_parameters_names
           ctx.Species_gen_basics.scc_dependency_graph_nodes name body in
       (* No recursivity, then the method cannot call itself in its body *)
       (* then no need to set the [scc_lambda_lift_params_mapping] of    *)
       (* the context.                                                   *)
       let need_self_extra_arg =
         generate_one_field_binding
           ctx print_ctx env ~let_connect: LC_first_non_rec
           llift_params dependencies_from_params decl_children
           (from, name, params, (Some scheme), body) in
       (* Now, build the [compiled_field_memory], even if the method  *)
       (* was not really generated because it was inherited.          *)
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_method_body = body ;
         cfm_dependencies_from_parameters = dependencies_from_params ;
         cfm_decl_children = decl_children ;
         cfm_need_self_extra_arg = need_self_extra_arg } in
       CSF_let compiled_field
   | Env.TypeInformation.SF_let_rec _l ->
       (* [Unsure]. *)
       CSF_let_rec []
   | Env.TypeInformation.SF_theorem (_from, name, _, _, _) ->
       CSF_theorem name
   | Env.TypeInformation.SF_property (from, name, _, prop) ->
       (* "Property"s lead to a Coq "Hypothesis". *)
       Format.fprintf out_fmter "(* From species %a. *)@\n"
         Sourcify.pp_qualified_species from ;
       Format.fprintf out_fmter
         "@[<2>Hypothesis self_%a :@ "
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       Species_record_type_generation.generate_prop
         ~local_idents: [] ~self_as: Types.CSR_self ctx env prop ;
       Format.fprintf out_fmter ".@]@\n" ;
       CSF_property name
;;



(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the names to be used
              later during the Coq translation.
              For a species parameter [A is/in ... ], the name that will be
              used isthe name of the species parameter + "_T"
              No need like in OCaml to add a stamp because we don't
              lowercase names. Hence parameters will never wear the same
              name.
              This avoids the need to remind the stamp of a "is" parameter
              that is used to make a "in" parameter. In effect, for the
              "species Me (Naturals is IntModel, n in Naturals)" code,
              "Naturals" would be mapped on "Naturals0" and then
              everywhere "Natural" was used in the FoCaL code, one should
              replace by "Naturals0" in the Coq code !

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, carrier_name), _, param_expr) ->
          (* Now, build the "collection type" this name will be bound to. *)
          (* According to how the "collection type" of parameters are     *)
          (* built, this will be the couple of the current compilation    *)
          (* unit and the name of the parameter.                          *)
          let type_coll = (current_unit, carrier_name) in
          (* And now create the binding... Record that the parameter is a *)
          (* "is" parameter whose species expr is [param_expr] that will  *)
          (* be used to create the Coq type expression annotating this    *)
          (* parameter in the hosting species record type.                *)
          (type_coll,
           (carrier_name ^ "_T", (Species_gen_basics.CCMI_is param_expr)))
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name that will represent this parameter's *)
          (* carrier seen from Coq.                              *)
          let carrier_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't    *)
          (* need any species expression to annotate this parameter in the   *)
          (* Coq type expression annotating this parameter in the hosting    *)
          (* species record type: it will simply be of the type [type_coll]. *)
          (type_coll,
           (carrier_name ^"_T", Species_gen_basics.CCMI_in_or_not_param)))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ********************************************************************** *)
(* Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->          *)
(*   Env.CoqGenEnv.t                                                      *)
(** {b Descr} : This function extend the coq code generation envionnment
      for a species generation. Because in Coq we need information about
    the number of extra parameters to add to function idents due to the
    fact that in oq polymorphism is explicit, we need to make methods of
    a species known before generating its body. It's the same problem for
    the species's parameters that must be bound in the environment, in
    order to inductively known their methods.
    This function add all this information in the current environment and
    return the extended environment.
    Note that because in FoCaL methods are not polymorphic, the number
    of extra parameters due to polymorphism is trivially always 0.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let extend_env_for_species_def env species_descr =
  (* We first add the species methods. Because methods are not polymorphic,  *)
  (* we can safely bind them to 0 extra parameters-induced-by-polymorphism.  *)
  let species_methods_names =
    Dep_analysis.ordered_names_list_of_fields
      species_descr.Env.TypeInformation.spe_sig_methods in
  let env_with_methods_as_values =
    List.fold_left
      (fun accu_env (m_name, _) -> Env.CoqGenEnv.add_value m_name 0 accu_env)
      env
      species_methods_names in
  (* Now, add the species's parameters in the environment. And do not *)
  (* [fold_right] otherwise species will be inserted in reverse order *)
  (* in the environment !                                             *)
  List.fold_left
    (fun accu_env species_param ->
      match species_param with
       | Env.TypeInformation.SPAR_in _ -> 
           (* "In" parameters are not species. They are "values" of *)
           (* species, "instances". Hence they do not lead to any   *)
           (* species in the environment.                           *)
           accu_env
       | Env.TypeInformation.SPAR_is ((_, param_name), param_methods, _) ->
           let methods_names =
             Dep_analysis.ordered_names_list_of_fields param_methods in
           let bound_methods = List.map fst methods_names in
           (* Because species names are capitalized, we explicitely build *)
           (* a [Parsetree.Vuident] to wrap the species name string.      *)
           Env.CoqGenEnv.add_species
             ~loc: Location.none (Parsetree.Vuident param_name)
             bound_methods accu_env)
    env_with_methods_as_values
    species_descr.Env.TypeInformation.spe_sig_params
;;


let generate_self_representation out_fmter print_ctx species_fields =
  let rec rec_find = function
    | [] ->
        (* No explicit "rep" structure found. Then, generate a Coq variable. *)
        Format.fprintf out_fmter "@\n(* Carrier representation. *)@\n" ;
        Format.fprintf out_fmter "@[<2>Variable self_T : Set.@]@\n@\n"
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_sig (_, (Parsetree.Vlident "rep"), sch) ->
             (* We finally found an explicit representation of the carrier. *)
             let (type_from_scheme, generalized_instanciated_vars) =
               Types.specialize_n_show_instanciated_generalized_vars sch in
             (* Because "rep" is never polymorphic, its type must never *)
             (* contain instanciated variables coming from the scheme.  *)
             assert (generalized_instanciated_vars = []) ;
             Format.fprintf out_fmter "@\n(* Carrier representation. *)@\n" ;
             (* We print the "rep"'s type using the [CSR_self] mode but in    *)
             (* fact, because the carrier can't be recursive, it can't        *)
             (* appear in it's own structure, and the mode has no importance. *)
             Format.fprintf out_fmter "@[<2>Let self_T : Set :=@ %a.@]@\n@\n"
               (Types.pp_type_simple_to_coq
                  print_ctx ~reuse_mapping: false ~self_as: Types.CSR_self)
               type_from_scheme
         | _ -> rec_find q
        end) in
  rec_find species_fields
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
  let chapter_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>Chapter %s.@\n" chapter_name ;
  (* Now, establish the mapping between collections available *)
  (* and the names representing their carrier.                *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Compute the list of names of parameters of the species. This   *)
  (* will be use to compute for each method the set of methods from *)
  (* the parameters the method depends on.                          *)
  let species_parameters_names =
    List.map
      (function
        | Env.TypeInformation.SPAR_in (n, _) -> n
        | Env.TypeInformation.SPAR_is ((_, n), _, _) -> Parsetree. Vuident n)
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Species_gen_basics.scc_current_unit = current_unit ;
    Species_gen_basics.scc_current_species = (current_unit, species_name) ;
    Species_gen_basics.scc_dependency_graph_nodes = dep_graph ;
    Species_gen_basics.scc_species_parameters_names =
      species_parameters_names ;
    Species_gen_basics.scc_collections_carrier_mapping =
      collections_carrier_mapping ;
    Species_gen_basics.scc_lambda_lift_params_mapping = [] ;
    Species_gen_basics.scc_out_fmter = out_fmter } in
  (* Insert in the environment the value bindings of the species methods *)
  (* and the species bindings for its parameters.                        *)
  let env' = extend_env_for_species_def env species_descr in
  (* The record type representing the species' type. *)
  Species_record_type_generation.generate_record_type ctx env' species_descr ;
  (* We now extend the collections_carrier_mapping with ourselves known. *)
  (* Hence, if we refer to our "rep" we will be directly mapped onto the *)
  (* "self_T" without needing to re-construct this name each time.       *)
  let collections_carrier_mapping' =
    ((current_unit, (Parsetree_utils.name_of_vname species_name)),
     ("self_T", Species_gen_basics.CCMI_in_or_not_param)) ::
    ctx.Species_gen_basics.scc_collections_carrier_mapping in
  let ctx' = { ctx with
     Species_gen_basics.scc_collections_carrier_mapping =
       collections_carrier_mapping' } in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx'.Species_gen_basics.scc_collections_carrier_mapping } in
  (* Now we generate a "Variable" of type "Set" for each species's parameter *)
  (* with the same name used during the record type generation, i.e. the     *)
  (* parameter's name + "_T".                                                *)
  List.iter
    (fun p_vname ->
      let p_name = Parsetree_utils.name_of_vname p_vname in
      Format.fprintf out_fmter
        "(* Variable abstracting the species parameter [%s]. *)@\n" p_name ;
      Format.fprintf out_fmter "@[<2>Variable %s_T :@ Set.@]@\n" p_name)
    species_parameters_names ;
  (* Now, the methods of the species. We deal with "rep" first *)
  (* and then it will be ignore while generating the methods.  *)
  generate_self_representation
    out_fmter print_ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let compiled_fields =
    List.map
      (generate_methods ctx' print_ctx env' species_parameters_names)
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* The end of the chapter hosting the species. *)
  Format.fprintf out_fmter "@]End %s.@\n@." chapter_name ;
  (* Extract the fields names to create the [species_binding_info]. *)
  let species_binding_info =
    List.flatten
      (List.map
         (function
           | CSF_sig vname -> [vname]
           | CSF_let compiled_field_memory ->
               [compiled_field_memory.cfm_method_name]
           | CSF_let_rec compiled_field_memories ->
               List.map
                 (fun cfm -> cfm.cfm_method_name)
                 compiled_field_memories
           | CSF_theorem vname -> [vname]
           | CSF_property vname -> [vname])
         compiled_fields) in
  species_binding_info
;;
