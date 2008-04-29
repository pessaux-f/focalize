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

(* $Id: abstractions.ml,v 1.12 2008-04-29 15:26:13 pessaux Exp $ *)


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
        We extract the methods we decl-depend on,the methods we def-depend
        on, the methods of the species parameters we depend on.

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
  let dependencies_from_params =
    List.fold_right
      (fun (species_param_name, species_param_kind) accu ->
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
        (species_param_name, species_param_kind, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* By side effect, we remind the species types appearing in our type. *)
  let params_appearing_in_types =
    ref (Types.get_species_types_in_type my_type) in

  (* By side effect, we remind the species types appearing  *)
  (* in the species parameters methods' types we depend on. *)
  List.iter
    (fun (_, _, meths) ->
      Parsetree_utils.DepNameSet.iter
        (fun (_, meth_ty) ->
          let st_set = Types.get_species_types_in_type meth_ty in
          params_appearing_in_types :=
            Types.SpeciesCarrierTypeSet.union
              st_set !params_appearing_in_types)
        meths)
    dependencies_from_params ;
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
(* [Unsure] Ne garder seulement les paramètres en "is" ? *)
  let species_param_names =
    List.map (fun (x, _, _) -> x) dependencies_from_params in
  let used_species_parameter_tys =
    List.filter
      (fun species_param_name ->
        let as_string = Parsetree_utils.name_of_vname species_param_name in
        Types.SpeciesCarrierTypeSet.mem
          (current_unit, as_string) !params_appearing_in_types)
      species_param_names in
  (used_species_parameter_tys,
   dependencies_from_params,
   decl_children,
   def_children)
;;



type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  ai_dependencies_from_params :
    (Parsetree.vname *                    (** The species parameter's name. *)
     Parsetree_utils.species_param_kind * (** The species parameter's kind. *)
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



(**
    To be usable for OCaml generation, the [with_def_deps] flag
    enables to forget the def-dependencies and their implied
    transitive decl-dependencies. In effect, in OCaml, only
    decl-dependencies are relevant.
*)
let compute_abstractions_for_fields ~with_def_deps ctx fields =
  List.map
    (function
      | Env.TypeInformation.SF_sig si -> FAI_sig si
      | Env.TypeInformation.SF_let ((_, name, _, sch, body, _, _) as li) ->
          let (used_species_parameter_tys, dependencies_from_params,
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
          (* Now, its minimal Coq typing environment. *)
          let min_coq_env =
            MinEnv.minimal_typing_environment universe fields in
          let abstr_info = {
            ai_used_species_parameter_tys = used_species_parameter_tys ;
            ai_dependencies_from_params = dependencies_from_params ;
            ai_min_coq_env = min_coq_env } in
          FAI_let (li, abstr_info)
      | Env.TypeInformation.SF_let_rec l ->
          let deps_infos =
            List.map
              (fun ((_, name, _, sch, body, _, _) as li) ->
                let body_as_fbk =
                  match body with
                   | Parsetree.BB_logical p -> FBK_logical_expr p
                   | Parsetree.BB_computational e -> FBK_expr e in
                let (used_species_parameter_tys, dependencies_from_params,
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
                (* Now, its minimal Coq typing environment. *)
                let min_coq_env =
                  MinEnv.minimal_typing_environment universe fields in
                let abstr_info = {
                  ai_used_species_parameter_tys = used_species_parameter_tys ;
                  ai_dependencies_from_params = dependencies_from_params ;
                  ai_min_coq_env = min_coq_env } in
                (li, abstr_info))
              l in
          FAI_let_rec deps_infos
      | Env.TypeInformation.SF_theorem
          ((_, name, sch, logical_expr, _, _) as ti) ->
          let (used_species_parameter_tys, dependencies_from_params,
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
          let min_coq_env = MinEnv.minimal_typing_environment universe fields in
          let abstr_info = {
            ai_used_species_parameter_tys = used_species_parameter_tys ;
            ai_dependencies_from_params = dependencies_from_params ;
            ai_min_coq_env = min_coq_env } in
          FAI_theorem (ti, abstr_info)
      | Env.TypeInformation.SF_property
          ((_, name, sch, logical_expr, _) as pi) ->
          let (used_species_parameter_tys, dependencies_from_params,
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
          let min_coq_env = MinEnv.minimal_typing_environment universe fields in
          let abstr_info = {
            ai_used_species_parameter_tys = used_species_parameter_tys ;
            ai_dependencies_from_params = dependencies_from_params ;
            ai_min_coq_env = min_coq_env } in
          FAI_property (pi, abstr_info))
    fields
;;
