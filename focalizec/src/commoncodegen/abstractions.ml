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

(* $Id: abstractions.ml,v 1.4 2008-03-07 10:55:32 pessaux Exp $ *)


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
  | FBK_prop of Parsetree.prop
;;



(* ************************************************************************ *)
(* current_unit: Types.fname ->                                             *)
(*   current_species: Parsetree.qualified_species ->                        *)
(*     Parsetree.vname list -> Dep_analysis.name_node list ->               *)
(*       Parsetree.vname -> field_body_kind ->                              *)
(*         ((Parsetree.vname list) *                                        *)
(*          (Parsetree.vname * Parsetree_utils.DepNameSet.t) list *         *)
(*          (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *  *)
(*          (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *  *)
(*          (string * Types.type_simple) list)                              *)
(** {b Descr} : Pre-process a field before its compilation to OCaml. We
        compute here the information related to the extra parameters
        a method will have by lambda-lifting due to the species parameters
        and the dependencies of the method.
        We extract the methods we decl-depend on,the methods we def-depend
        on, the methods of the species parameters we depend on, and finally
        the list of formal parameters (name and type) the method will have
        due to the decl and params dependencies infos we computed. This
        last list will be straight printed by the code generator, but will
        also be recorded in the context for the case we need to generate
        the code of a recursive method. This way, the recursive application
        of the method will have to and will be able to use these extra
        parameters in addition to those effectively passed in the FoCaL
        code.

    {b Rem} : Exported oustide this module.                                 *)
(* ************************************************************************ *)
let compute_lambda_liftings_for_field ~current_unit ~current_species
     species_parameters_names dependency_graph_nodes name body =
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
      (fun species_param_name accu ->
        let meths_from_param =
          (match body with
           | FBK_expr e ->
               Param_dep_analysis.param_deps_expr
                 ~current_species species_param_name e
           | FBK_prop p ->
               Param_dep_analysis.param_deps_prop
                 ~current_species species_param_name p) in
        (* Return a couple binding the species parameter's name with the *)
        (* methods of it we found as required for the current method.    *)
        (species_param_name, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* Build the list by side effect in reverse order for efficiency. *)
  let revd_lambda_lifts = ref [] in
  let params_appearing_in_types = ref Types.SpeciesCarrierTypeSet.empty in
  (* First, abstract according to the species's parameters the current  *)
  (* method depends on.                                                 *)
  List.iter
    (fun (species_param_name, meths) ->
      (* Each abstracted method will be named like "_p_", followed by *)
      (* the species parameter name, followed by "_", followed by the *)
      (* method's name.                                               *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, meth_ty) ->
          let llift_name =
            prefix ^
            (Parsetree_utils.vname_as_string_with_operators_expanded meth) in
          revd_lambda_lifts := (llift_name, meth_ty) :: !revd_lambda_lifts ;
          (* By the way and by side effect, we remind the   *)
          (* species types appearing the the method's type. *)
          let st_set = Types.get_species_types_in_type meth_ty in
          params_appearing_in_types :=
            Types.SpeciesCarrierTypeSet.union
              st_set !params_appearing_in_types)
        meths)
    dependencies_from_params ;
  (* Now, lambda-lift all the dependencies from our inheritance tree *)
  (* (i.e methods we depend on) that are only declared.              *)
  List.iter
    (fun ({ Dep_analysis.nn_name = dep_name ; Dep_analysis.nn_type = ty }, _) ->
      let llift_name =
        "abst_" ^
        (Parsetree_utils.vname_as_string_with_operators_expanded dep_name) in
      revd_lambda_lifts := (llift_name, ty) :: !revd_lambda_lifts)
    decl_children ;
  (* Now compute the set of species parameters types used in the   *)
  (* types of the methods comming from the species parameters that *)
  (* the current field uses. This information is required for Coq  *)
  (* since they will lead to extra args of type "Set".             *)
  let species_param_names = List.map fst dependencies_from_params in
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
   def_children,
   (List.rev !revd_lambda_lifts))
;;



type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  ai_dependencies_from_params :
    (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ;
  ai_min_coq_env : MinEnv.min_coq_env_element list
} ;;



type field_abstraction_info =
  | FAI_sig of Env.TypeInformation.sig_field_info
  | FAI_let of (Env.TypeInformation.let_field_info * abstraction_info)
  | FAI_let_rec of (Env.TypeInformation.let_field_info * abstraction_info) list
  | FAI_theorem of (Env.TypeInformation.theorem_field_info * abstraction_info)
  | FAI_property of Env.TypeInformation.property_field_info
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
      | Env.TypeInformation.SF_let ((_, name, _, _, body, _) as li) ->
          let (used_species_parameter_tys, dependencies_from_params,
               decl_children, def_children, _) =
            let body_as_fbk =
              match body with
               | Parsetree.BB_logical p -> FBK_prop p
               | Parsetree.BB_computational e -> FBK_expr e in
            compute_lambda_liftings_for_field
              ~current_unit: ctx.Context.scc_current_unit
              ~current_species: ctx.Context.scc_current_species
              ctx.Context.scc_species_parameters_names
              ctx.Context.scc_dependency_graph_nodes name
              body_as_fbk in
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
              (fun ((_, name, _, _, body, _) as li) ->
                let body_as_fbk =
                  match body with
                   | Parsetree.BB_logical p -> FBK_prop p
                   | Parsetree.BB_computational e -> FBK_expr e in
                let (used_species_parameter_tys, dependencies_from_params,
                     decl_children, def_children, _) =
                  compute_lambda_liftings_for_field
                    ~current_unit: ctx.Context.scc_current_unit
                    ~current_species: ctx.Context.scc_current_species
                    ctx.Context.scc_species_parameters_names
                    ctx.Context.scc_dependency_graph_nodes name
                    body_as_fbk in
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
      | Env.TypeInformation.SF_theorem ((_, name, _, prop, _, _) as ti) ->
          let (used_species_parameter_tys, dependencies_from_params,
               decl_children, def_children, _) =
            compute_lambda_liftings_for_field
              ~current_unit: ctx.Context.scc_current_unit
              ~current_species: ctx.Context.scc_current_species
              ctx.Context.scc_species_parameters_names
              ctx.Context.scc_dependency_graph_nodes name
              (FBK_prop prop) in
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
      | Env.TypeInformation.SF_property pi ->
          (* [Unsure] Pas besoin de connaitre les dépendances sur "rep" ?
             Pas besoin de notion d'univers ? De paramètres d'espèce ? *)
          FAI_property pi)
    fields
;;
