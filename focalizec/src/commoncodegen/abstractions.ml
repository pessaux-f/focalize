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

(* $Id: abstractions.ml,v 1.14 2008-06-03 15:40:36 pessaux Exp $ *)


(* ******************************************************************** *)
(** {b Descr} : Describes if the argument passed to the function
      [compute_lambda_liftings_for_field] is the body of a "let" or of
      a "property/theorem". This allows the function to process at once
      both the case of the liftings computation for expressions and
      propositions.

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_logical_expr of Parsetree.logical_expr
;;



(* ************************************************************************* *)
(** {b Descr} Serves to pass to [complete_dependencies_from_params] the
    code generation environment of the current language backend. We need this
    since to apply rule [PRM], wee need to recover the dependencies on
    species parameter of some species. This information is present in all the
    code generation environments but these environments have different types.
    Hence, when we will need to access an environment, with this 2
    constructors, we will know its type and which primitives to use.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
type environment_kind =
  | EK_ml of Env.MlGenEnv.t
  | EK_coq of Env.CoqGenEnv.t
;;



(* ************************************************************************* *)
(* current_unit: Types.fname ->                                              *)
(*   current_species: Parsetree.qualified_species ->                         *)
(*     (Parsetree.vname * Context.species_param_kind) list ->                *)
(*       Dep_analysis.name_node list ->                                      *)
(*       Parsetree.vname -> field_body_kind -> Types.simple_types            *)
(*           ((Parsetree.vname list) *                                       *)
(*            (Parsetree.vname * Parsetree_utils.DepNameSet.t) list *        *)
(*            (Dep_analysis.name_node * Dep_analysis.dependency_kind) list * *)
(*            (Dep_analysis.name_node * Dep_analysis.dependency_kind) list)  *)
(** {b Descr} : Pre-process a field before its compilation to OCaml. We
        compute here the information related to the extra parameters
        a method will have by lambda-lifting due to the species parameters
        and the dependencies of the method.
        We extract the methods we decl-depend on, the methods we def-depend
        on, the methods of the species parameters we depend on via our
        "body" for lets and "type" for theorems and properties.
        ATTENTION, the set of  the methods of the species parameters we
        depend on is not complete: it must be completed to achieve the
        definition 72 page 153 in Virgile Prevosto's Phd. In fact, the
        present function only implements rule [BODY] !

    {b Rem} : Exported oustide this module.                                  *)
(* ************************************************************************* *)
let compute_lambda_liftings_for_field ~current_unit ~current_species
     species_parameters_names dependency_graph_nodes name body my_type =
  (* Get all the methods we directly decl-depend on. They will   *)
  (* lead each to an extra parameter of the final OCaml function *)
  (* (lambda-lifing). Get the methods we directly def-depend.    *)
  (* They will be ignored for OCaml but used for Coq.            *)
  let (decl_children, def_children) =
    (try
      let my_node =
        List.find
          (fun { Dep_analysis.nn_name = n } -> n = name)
          dependency_graph_nodes in
      (* Only keep "decl-dependencies" . *)
      List.partition
        (function
          | (_, Dep_analysis.DK_decl _) -> true
          | (_, Dep_analysis.DK_def) -> false)
        my_node.Dep_analysis.nn_children
    with Not_found -> ([], [])  (* No children at all. *)) in
  (* Get the list of the methods from the species parameters the current *)
  (* method depends on. Do not [fold_left] to keep the extra parameters  *)
  (* in the same order than the species parameters order. I.e. for a    *)
  (* species [Foo (A ..., B) ...] we want to have the extra parameters  *)
  (* due to lambda-lifting in the OCaml function ordered such as those  *)
  (* coming from [A] are first, then come those from [B].               *)
  let dependencies_from_params_in_bodies =
    List.fold_right
      (fun species_param accu ->
        (* Recover the species parameter's name. *)
        let species_param_name =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _) ->
               Parsetree.Vuident n in
        let meths_from_param =
          (match body with
           | FBK_expr e ->
               Param_dep_analysis.param_deps_expr
                 ~current_species species_param_name e
           | FBK_logical_expr p ->
               Param_dep_analysis.param_deps_logical_expr
                 ~current_species species_param_name p) in
        (* Return a couple binding the species parameter's name with the *)
        (* methods of it we found as required for the current method.    *)
        (species_param, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* By side effect, we remind the species types appearing in our type. *)
  let params_appearing_in_types =
    ref (Types.get_species_types_in_type my_type) in

  (* By side effect, we remind the species types appearing  *)
  (* in the species parameters methods' types we depend on. *)
  List.iter
    (fun (_, meths) ->
      Parsetree_utils.DepNameSet.iter
        (fun (_, meth_ty) ->
          let st_set = Types.get_species_types_in_type meth_ty in
          params_appearing_in_types :=
            Types.SpeciesCarrierTypeSet.union
              st_set !params_appearing_in_types)
        meths)
    dependencies_from_params_in_bodies ;
  (* Same thing for the methods of ourselves we decl-depend on except on *)
  (* rep that is processed apart.                                        *)
  List.iter
    (fun (node, _) ->
      if node.Dep_analysis.nn_name <> (Parsetree.Vlident "rep") then
        begin
        let st_set =
          Types.get_species_types_in_type node.Dep_analysis.nn_type in
        params_appearing_in_types :=
          Types.SpeciesCarrierTypeSet.union st_set !params_appearing_in_types
        end)
    decl_children ;
  (* Same thing for the methods of ourselves we def-depend on except on *)
  (* rep that is processed apart.                                       *)
  List.iter
    (fun (node, _) ->
      if node.Dep_analysis.nn_name <> (Parsetree.Vlident "rep") then
        begin
        let st_set =
          Types.get_species_types_in_type node.Dep_analysis.nn_type in
        params_appearing_in_types :=
          Types.SpeciesCarrierTypeSet.union st_set !params_appearing_in_types
        end)
    def_children ;
  (* Now compute the set of species parameters types used in the   *)
  (* types of the methods comming from the species parameters that *)
  (* the current field uses. This information is required for Coq  *)
  (* since they will lead to extra args of type "Set".             *)
(* [Unsure] Ne garder seulement les param�tres en "is" ? *)
  let species_param_names =
    List.map
      (fun (species_param, _) ->
        (* Recover the species parameter's name. *)
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _) -> Parsetree. Vuident n)
      dependencies_from_params_in_bodies in
  let used_species_parameter_tys =
    List.filter
      (fun species_param_name ->
        let as_string = Parsetree_utils.name_of_vname species_param_name in
        Types.SpeciesCarrierTypeSet.mem
          (current_unit, as_string) !params_appearing_in_types)
      species_param_names in
  (used_species_parameter_tys,
   dependencies_from_params_in_bodies,
   decl_children,
   def_children)
;;



type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  (** Dependencies found via [BODY] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  ai_dependencies_from_params_via_body :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.DepNameSet.t)     (** The set of methods we depend on. *)
  list ;
  (** Dependencies found via [TYPE] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  ai_dependencies_from_params_via_type :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.DepNameSet.t)     (** The set of methods we depend on. *)
  list ;
  (** Other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of definition
      72 page 153 of Virgile Prevosto's Phd. *)
  ai_dependencies_from_params_via_completion :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.DepNameSet.t)     (** The set of methods we depend on. *)
  list ;
  ai_min_coq_env : MinEnv.min_coq_env_element list
} ;;



type field_abstraction_info =
  | FAI_sig of Env.TypeInformation.sig_field_info
  | FAI_let of (Env.TypeInformation.let_field_info * abstraction_info)
  | FAI_let_rec of (Env.TypeInformation.let_field_info * abstraction_info) list
  | FAI_theorem of (Env.TypeInformation.theorem_field_info * abstraction_info)
  | FAI_property of (Env.TypeInformation.property_field_info * abstraction_info)
;;



(** Must never be called with the method "rep". It has no meaning.
    May return None is the searched field is a signature since a signature
  do not have any dependencies on methods. *)
let find_field_abstraction_by_name name abstractions =
  assert (name <> Parsetree.Vlident "rep") ;
  let rec rec_find = function
    | [] -> assert false             (* The search must succeed ! *)
    | h :: q ->
        match h with
         | FAI_sig (_, n, _) ->
             (* A signature never induce def-dependencies. *)
             if n = name then None
             else rec_find q  (* Not the good name, go on searching... *)
         | FAI_let ((_, n, _, _, _, _, _), abstraction_info)
         | FAI_theorem ((_, n, _, _, _, _) , abstraction_info)
         | FAI_property ((_, n, _, _, _), abstraction_info) ->
             if n = name then Some abstraction_info else rec_find q
         | FAI_let_rec l ->
             (begin
             try
               let (_, abstraction_info) =
                 List.find (fun ((_, n, _, _, _, _, _), _) -> n = name) l in
               Some abstraction_info
             with Not_found -> rec_find q
             end) in
  rec_find abstractions
;;



(* ********************************************************************** *)
(** {b Descr} Get the list of species parameters names used as effective
     arguments of the **parametrised** species expression [spe_expr] and
     their position according to the [spe_expr]'s parameters (i.e. first,
     second or whatever).
     ATTENTION: [spe_expr] is assumed to be parametrised !!!

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let get_user_of_parameters_with_position ~current_unit species_parameters
    spe_expr =

Format.eprintf "get_user_of_parameters_with_position: | species_parameters | = %d@."
  (List.length species_parameters) ;

  (* Do not [fold_right] otherwise, the counter will be reversed compared *)
  (* to the order of the elements of the list, i.e. otherwise when        *)
  (* processing the last element of the list, the counter will be 0 and   *)
  (* once on the first element of the list it will be length of the list  *)
  (* minus 1.                                                             *)
  let (params_with_pos, _) =
    List.fold_left
      (fun (accu, counter) effective_arg ->
        match effective_arg with
         | Env.TypeInformation.SPE_Self ->

Format.eprintf "Found effective arg Self@." ;

             (* "Self" is never a species parameter ! *)
             (accu, (counter + 1))
         | Env.TypeInformation.SPE_Species eff_arg_qual_vname ->
             (begin

Format.eprintf "Found effective arg: %a@."
  Sourcify.pp_qualified_vname eff_arg_qual_vname ;

             match eff_arg_qual_vname with
              | Parsetree.Qualified (modname, eff_arg_vname)
                when modname = current_unit ->
                  let eff_arg_name =
                    Parsetree_utils.name_of_vname eff_arg_vname in
                  (* We check if this [eff_arg_name] is a species parameter *)
                  (* of the current species. If so, we keep in the result ! *)
                  if List.exists
                      (function
                        | Env.TypeInformation.SPAR_in (_, _) ->
                            false      (* "In" parameters are never involved. *)
                        | Env.TypeInformation.SPAR_is ((_, n), _, _) ->

Format.eprintf "Species_parameter Vname: %s@." n ;

                            n = eff_arg_name)
                      species_parameters
                  then
                    (((effective_arg, counter) :: accu), (counter + 1))
                  else
                    (accu, (counter + 1))
              | Parsetree.Qualified (_, _) -> (accu, (counter + 1))
              | Parsetree.Vname _ ->
                  (* Scoping should have transformed it into a [Qualified]. *)
                  assert false
             end))
      ([], 0)
      spe_expr.Env.TypeInformation.sse_effective_args in
  (* Tells that species [sse_name] uses [params_with_pos] as arguments... *)
  (spe_expr.Env.TypeInformation.sse_name, params_with_pos)
;;



(** Implements rules [TYPE], [DEF-DEP], [UNIVERSE] and [PRM] of the
    definition 72 page 153 of Virgile Prevosto's Phd. *)
(* [Unsure] est-ce que les "used_parameters_ty" ne devraient pas �tre aussi
  "compl�t�s" ? *)
let complete_dependencies_from_params _env ~current_unit ~current_species
    seen_abstractions species_parameters def_children universe opt_proof =
  (* Rule [TYPE] possible only if a proof is provided. *)
  let dependencies_from_params_via_type =
    (match opt_proof with
     | None ->
         (* If no proof is given, then there is no dependency, but we must *)
         (* not simply return the [] because all our "union" functions on  *)
         (* dependencies rely on a list of sets with 1 set for each        *)
         (* species parameter name.                                        *)
         List.fold_right
           (fun species_param accu ->
             (species_param, Parsetree_utils.DepNameSet.empty) :: accu)
           species_parameters
           []
     | Some proof ->
         (* Same remark about [fold_right] than for the function *)
         (* [compute_lambda_liftings_for_field] when computing   *)
         (* [dependencies_from_params_in_bodies].                *)
         List.fold_right
           (fun species_param accu ->
             (* Recover the species parameter's name. *)
             let species_param_name =
               match species_param with
                | Env.TypeInformation.SPAR_in (n, _) -> n
                | Env.TypeInformation.SPAR_is ((_, n), _, _) ->
                    Parsetree. Vuident n in
             let meths_from_param =
               Param_dep_analysis.param_deps_proof
                 ~current_species species_param_name proof in
             (* Return a couple binding the species parameter's name with the *)
             (* methods of it we found as required for the current method.    *)
             (species_param, meths_from_param) :: accu)
           species_parameters
           []) in
  (* Rule [DEF-DEP]. Since "rep" is a method like the others, it may appear *)
  (* in the def-dependencies. However, since "rep" can never induce         *)
  (* dependencies on species parameters methods, we directly forget it.     *)
  let abstr_infos_from_all_def_children =
    List.fold_left
      (fun accu (def_child, _) ->
        if def_child.Dep_analysis.nn_name = (Parsetree.Vlident "rep") then accu
        else
          (* Get the abstraction info of the child we def-depend on. *)
          (find_field_abstraction_by_name def_child.Dep_analysis.nn_name
             seen_abstractions) :: accu)
      []
      def_children in
  (* The "empty" dependencies cannot simple be [] because all our "union" *)
  (* functions on dependencies rely on a list of sets with 1 set for each *)
  (* species parameter name. So we create our initial accumulator as the  *)
  (* list mapping each species parameter name onto the empty dependencies *)
  (* set.                                                                 *)
  let empty_initial_deps_accumulator =
    List.fold_right
      (fun species_param accu ->
        (species_param, Parsetree_utils.DepNameSet.empty) :: accu)
      species_parameters
      [] in
  (* Since methods on which we depend are from Self, all of them share the *)
  (* same species parameter names, and by construction, each of them have  *)
  (* the same structure of list (i.e. species parameter names at the same  *)
  (* place in the list) for their [ai_dependencies_from_params_via_body].  *)
  (* Hence, instead of making [List.map] on the [species_parameter_names]  *)
  (* to individually merge the methods from each children for a species    *)
  (* parameter, we simply make the union (without double) of all the       *)
  (* [ai_dependencies_from_params_via_body] of the def-children.           *)
  let dependencies_from_params_via_compl1 =
    List.fold_left
      (fun accu_deps_from_params abstr_infos_opt ->
        match abstr_infos_opt with
         | Some abstr_infos ->
             (* We merge the found abstraction info and *)
             (* the abstraction info accumulator.       *)
             List.map2
               (fun (prm1, deps1) (prm2, deps2) ->
                 (* A few asserts to ensure the compiler is fine. *)
                 assert (prm1 = prm2) ;
                 let deps = Parsetree_utils.DepNameSet.union deps1 deps2 in
                 (prm1, deps))
               abstr_infos.ai_dependencies_from_params_via_body
               accu_deps_from_params
         | None ->
             (* No abstr_infos found, so leave the accumulator as it was. *)
             accu_deps_from_params)
      empty_initial_deps_accumulator
      abstr_infos_from_all_def_children in
  (* Rule [UNIVERS]. We extend [dependencies_from_params_via_compl1]. *)
  let dependencies_from_params_via_compl2 =
    VisUniverse.Universe.fold
      (fun z_name_in_univ _ accu_deps_from_params ->
        (* For each z (c.f. notation in Virgile) in the visible universe,    *)
        (* we must add its [ai_dependencies_from_params_via_type].           *)
        (* So we must first search the abstraction info of [z_name_in_univ]. *)
        (* Since "rep" is a method like the others, it may appear in the     *)
        (* universe. However, since "rep" can never induce dependencies on   *)
        (* species parameters methods, we directly forget it.                *)
        if z_name_in_univ = (Parsetree.Vlident "rep") then
          accu_deps_from_params
        else
          (begin
          let abstr_info_opt =
            find_field_abstraction_by_name z_name_in_univ seen_abstractions in
          match abstr_info_opt with
           | Some abstr_info ->
               (* Now, add the [ai_dependencies_from_params_via_type] *)
               (* to the dependencies accumulator.                    *)
               List.map2
                 (fun (prm1, deps1) (prm2, deps2) ->
                   (* A few asserts to ensure the compiler is fine. *)
                   assert (prm1 = prm2) ;
                   let deps = Parsetree_utils.DepNameSet.union deps1 deps2 in
                   (prm1, deps))
                 abstr_info.ai_dependencies_from_params_via_type
                 accu_deps_from_params
           | None ->
               (* No abstr_infos found, so leave the accumulator as it was. *)
               accu_deps_from_params
          end))
      universe
      dependencies_from_params_via_compl1 in
  (* First, we look for "is" parameters themselves parametrised. We hunt in *)
  (* the [species_parameters], to get some [Env.TypeInformation.SPAR_is]    *)
  (* whose [simple_species_expr] has a non empty list [sse_effective_args]. *)
  let params_being_parametrised =
    List.filter
      (function
        | Env.TypeInformation.SPAR_is ((_, _), _, spe_expr) ->
            (* Keep it only if there are parameters in the expression. *)
            spe_expr.Env.TypeInformation.sse_effective_args <> []
        | Env.TypeInformation.SPAR_in (_, _) -> false)
      species_parameters in
  (* Now, get for each parametrised parameter of the species, which other  *)
  (* parameters it uses as effective argument in which species and at      *)
  (* which position.                                                       *)
  (* For example: species S (Cp is ..., Cp' is S'(Cp))                     *)
  (* We want to know that Cp' uses Cp as argument for the species S'. So   *)
  (* we want to get the pair (Cp', [(S', 1)]). If Cp' used another Cq' as  *)
  (* third argument, we would get the pair (Cp', [(Cp, 1); (Cq, 3)]).      *)
  let parametrised_params_with_their_effective_args_being_params =
    List.map
      (function
        | Env.TypeInformation.SPAR_in (_, _) ->
            (* "In" parameters are filtered just above ! *)
            assert false
        | Env.TypeInformation.SPAR_is ((_, n), _, spe_expr) ->
            (* In our example, [n] is Cp'. *)

Format.eprintf "Processing get_user_of_parameters_with_position for: %s@." n ;

            (n,
             (get_user_of_parameters_with_position
                ~current_unit species_parameters spe_expr)))
      params_being_parametrised in
  (* Now, we know that Cp' is a species parameter built from S' applying *)
  (* Cp at position 0. We must find the name of the formal parameter in  *)
  (* S' corresponding to the position where Cp is applied. Let's call it *)
  (* K. We have now to find all the dependencies (methods y) of K in S'  *)
  (* and we must add them to the dependencies of Cp.                     *)

List.iter
  (fun (species_param, (parametrised_species_using, parameters_used)) ->
    Format.eprintf "Species parameter %s built applying %a to:@."
      species_param Sourcify.pp_ident parametrised_species_using ;
    List.iter
      (fun (eff_arg, position) ->
        match eff_arg with
         | Env.TypeInformation.SPE_Self -> assert false
         | Env.TypeInformation.SPE_Species qualified_vname ->
             Format.eprintf "%a at position %d@."
               Sourcify.pp_qualified_vname qualified_vname position)
      parameters_used)
parametrised_params_with_their_effective_args_being_params ;

  (* Rule [PRM]. *)
  let dependencies_from_params_via_compl3 =  (* [Unsure] *)
    dependencies_from_params_via_compl2 in
 (dependencies_from_params_via_type, dependencies_from_params_via_compl3)
;;



(**
    To be usable for OCaml generation, the [with_def_deps] flag
    enables to forget the def-dependencies and their implied
    transitive decl-dependencies. In effect, in OCaml, only
    decl-dependencies are relevant.
*)
let compute_abstractions_for_fields ~with_def_deps env ctx fields =
  let reversed_abstractions =
    (* ATTENTION: do not [fold_right] ! We build the list in reverse order *)
    (* end finally reverse it at the end for sake of efficiency. We        *)
    (* explicitly [fold_left] to have in our accumulator, the list of      *)
    (* fields already processed, and in the right order (in their order of *)
    (* apparition) AND to process fields of the list [fields] in their     *)
    (* order of apparition. We need this in order to recover the already   *)
    (* computed dependencies from params on previous fields since this     *)
    (* info will possibly used to apply rules [DEF-DEP], [UNIVERSE] and    *)
    (* [PRM] of definition 72 page 153 from Virgile Prevosto's Phd.        *)
    List.fold_left
      (fun abstractions_accu current_field ->
        match current_field with
         | Env.TypeInformation.SF_sig si -> (FAI_sig si) :: abstractions_accu
         | Env.TypeInformation.SF_let ((_, name, _, sch, body, _, _) as li) ->
             (* ATTENTION, the [dependencies_from_params_in_body] is not  *)
             (* the complete set of dependencies. It must be completed to *)
             (* fully represent the definition 72 page 153 from Virgile   *)
             (* Prevosto's Phd.                                           *)
             let (used_species_parameter_tys,
                  dependencies_from_params_in_body,
                  decl_children, def_children) =
               let body_as_fbk =
                 match body with
                  | Parsetree.BB_logical p -> FBK_logical_expr p
                  | Parsetree.BB_computational e -> FBK_expr e in
               compute_lambda_liftings_for_field
                 ~current_unit: ctx.Context.scc_current_unit
                 ~current_species: ctx.Context.scc_current_species
                 ctx.Context.scc_species_parameters_names
                 ctx.Context.scc_dependency_graph_nodes name
                 body_as_fbk (Types.specialize sch) in
             (* Compute the visible universe of the method. *)
             let universe =
               VisUniverse.visible_universe
                 ~with_def_deps
                 ctx.Context.scc_dependency_graph_nodes decl_children
                 def_children in
             (* Complete the dependencies from species parameters info. *)
             let (dependencies_from_params_in_type,
                  dependencies_from_params_via_compl) =
               complete_dependencies_from_params
                 env ~current_unit: ctx.Context.scc_current_unit
                 ~current_species: ctx.Context.scc_current_species
                 abstractions_accu ctx.Context.scc_species_parameters_names
                 def_children universe None in
             (* Now, its minimal Coq typing environment. *)
             let min_coq_env =
               MinEnv.minimal_typing_environment universe fields in
             let abstr_info = {
               ai_used_species_parameter_tys = used_species_parameter_tys ;
               ai_dependencies_from_params_via_body =
                 dependencies_from_params_in_body ;
               ai_dependencies_from_params_via_type =
                 dependencies_from_params_in_type ;
               ai_dependencies_from_params_via_completion =
                 dependencies_from_params_via_compl ;
               ai_min_coq_env = min_coq_env } in
             (FAI_let (li, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let_rec l ->
             let deps_infos =
               List.map
                 (fun ((_, name, _, sch, body, _, _) as li) ->
                   let body_as_fbk =
                     match body with
                      | Parsetree.BB_logical p -> FBK_logical_expr p
                      | Parsetree.BB_computational e -> FBK_expr e in
                   (* ATTENTION, the [dependencies_from_params_in_bodies] is *)
                   (* not the complete set of dependencies. It must be  to   *)
                   (* completed fully represent the definition 72 page 153   *)
                   (* from Virgile Prevosto's Phd.                           *)
                   let (used_species_parameter_tys,
                        dependencies_from_params_in_bodies,
                        decl_children, def_children) =
                     compute_lambda_liftings_for_field
                       ~current_unit: ctx.Context.scc_current_unit
                       ~current_species: ctx.Context.scc_current_species
                       ctx.Context.scc_species_parameters_names
                       ctx.Context.scc_dependency_graph_nodes name
                       body_as_fbk (Types.specialize sch) in
                   (* Compute the visible universe of the method. *)
                   let universe =
                     VisUniverse.visible_universe
                       ~with_def_deps
                       ctx.Context.scc_dependency_graph_nodes
                       decl_children def_children in
                   (* Complete the dependencies from species parameters info. *)
                   let (dependencies_from_params_in_type,
                        dependencies_from_params_via_compl) =
                     complete_dependencies_from_params
                       env ~current_species: ctx.Context.scc_current_species
                       ~current_unit: ctx.Context.scc_current_unit
                       abstractions_accu ctx.Context.
                         scc_species_parameters_names
                       def_children universe None in
                   (* Now, its minimal Coq typing environment. *)
                   let min_coq_env =
                     MinEnv.minimal_typing_environment universe fields in
                   let abstr_info = {
                     ai_used_species_parameter_tys =
                       used_species_parameter_tys ;
                     ai_dependencies_from_params_via_body =
                       dependencies_from_params_in_bodies ;
                     ai_dependencies_from_params_via_type =
                       dependencies_from_params_in_type ;
                     ai_dependencies_from_params_via_completion =
                       dependencies_from_params_via_compl ;
                     ai_min_coq_env = min_coq_env } in
                   (li, abstr_info))
                 l in
             (FAI_let_rec deps_infos) :: abstractions_accu
         | Env.TypeInformation.SF_theorem
             ((_, name, sch, logical_expr, proof, _) as ti) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not *)
               (* the complete set of dependencies. It must be completed to  *)
               (* fully represent the definition 72 page 153 from Virgile    *)
               (* Prevosto's Phd.                                            *)
               let (used_species_parameter_tys,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.Context.scc_current_unit
                   ~current_species: ctx.Context.scc_current_species
                   ctx.Context.scc_species_parameters_names
                   ctx.Context.scc_dependency_graph_nodes name
                   (FBK_logical_expr logical_expr) (Types.specialize sch) in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps
                   ctx.Context.scc_dependency_graph_nodes decl_children
                   def_children in
               (* Now, its minimal Coq typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_typing_environment universe fields in
               (* Complete the dependencies from species parameters info. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_compl) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe (Some proof) in
               let abstr_info = {
                 ai_used_species_parameter_tys = used_species_parameter_tys ;
                 ai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 ai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 ai_dependencies_from_params_via_completion =
                   dependencies_from_params_via_compl ;
                 ai_min_coq_env = min_coq_env } in
               (FAI_theorem (ti, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_property
             ((_, name, sch, logical_expr, _) as pi) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not *)
               (* the complete set of dependencies. It must be completed to  *)
               (* fully represent the definition 72 page 153 from Virgile    *)
               (* Prevosto's Phd.                                            *)
               let (used_species_parameter_tys,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.Context.scc_current_unit
                   ~current_species: ctx.Context.scc_current_species
                   ctx.Context.scc_species_parameters_names
                   ctx.Context.scc_dependency_graph_nodes name
                   (FBK_logical_expr logical_expr) (Types.specialize sch) in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps
                   ctx.Context.scc_dependency_graph_nodes decl_children
                   def_children in
               (* Complete the dependencies from species parameters info. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_compl) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe None in
               (* Now, its minimal Coq typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_typing_environment universe fields in
               let abstr_info = {
                 ai_used_species_parameter_tys = used_species_parameter_tys ;
                 ai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 ai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 ai_dependencies_from_params_via_completion =
                   dependencies_from_params_via_compl ;
                 ai_min_coq_env = min_coq_env } in
               (FAI_property (pi, abstr_info)) :: abstractions_accu)
      []      (* Initial empty abstractions accumulator. *)
      fields in
  (* Finally, put the list of abstractions in the right order, i.e. *)
  (* in the order of apparition of the fields in the species.       *)
  List.rev reversed_abstractions
;;
