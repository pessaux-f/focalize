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

(* $Id: misc_ml_generation.ml,v 1.12 2007-12-17 14:31:05 pessaux Exp $ *)



let pp_to_ocaml_label_ident ppf lab_ident =
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.LI qual_name ->
       let vname =
         (match qual_name with
          | Parsetree.Vname n -> n
          | Parsetree.Qualified (modname, n) ->
              Format.fprintf ppf "%s." (String.capitalize modname) ;
              n) in
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
;;



(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the OCaml code for various constructs. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          uninteresting to understand the compilation process: it is more
          utilities.

    {b Rem} Exported outside this module.                                *)
(* ********************************************************************* *)
type reduced_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  rcc_current_unit : Types.fname ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. *)
  rcc_species_parameters_names : Parsetree.vname list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  rcc_collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the OCaml code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  rcc_lambda_lift_params_mapping :
   (Parsetree.vname * ((string * Types.type_simple )list)) list ;
  (** The current output formatter where to send the generated code. *)
  rcc_out_fmter : Format.formatter
} ;;



(* ************************************************************************ *)
(* Types.type_scheme option -> Parsetree.vname list ->                      *)
(*  (((Parsetree.vname * Types.type_simple option) list) *                  *)
(*   (Types.type_simple option) *                                           *)
(*   (Types.type_simple list))                                              *)
(** {b Descr} : Because methods parameters do not have their type with them
              in the [species_description]s, this function establish the
              mapping between the parameters names and their related type.
              It dissecates method's the type scheme (more accurately, an
              instance of it), "removing" arrows parameter after parameter.
              Because the typechecking pass is already done, the FoCaL
              program is well-typed, hence, the type of the method must
              have "as many arrows as" the method has parameters. If this
              is not the case, then we have a bug somewhere else in the
              previous processes in the compiler.
              This function hence returns the list giving for each
              parameter name its type and the "result" type of the method
              (i.e. the type remaining after having "removed all the
              arrows" induced by the parameters).
              It also returns the list of type variables that were used
              to instanciated the ones generalized in the type scheme.
              This is useful for Coq generation because polymorphism is
              explicit, leading for each polymorphic parameter to one
              extra parameter of type "Set" used to type the polymorphic
              parameter.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let bind_parameters_to_types_from_type_scheme opt_scheme params_names =
  match opt_scheme with
   | None ->
       (* Since we are not given any type information, the binding will *)
       (* be trivially void and no type constraint will be printed.     *)
       ((List.map (fun p_name -> (p_name, None)) params_names), None, [])
   | Some scheme ->
       (begin
       try
         let (type_from_scheme, generalized_instanciated_vars) =
           Types.specialize_n_show_instanciated_generalized_vars scheme in
         (* Be careful, the bindings list is built reversed ! We must finally *)
         (* reverse it again to keep the right order (i.e. first argument in  *)
         (* head of the list.                                                 *)
         let rec rec_bind accu_bindings ty = function
           | [] -> (accu_bindings, (Some ty))
           | h :: q ->
               (* We split the functionnal type. First, get argument type. *)
               let h_type = Types.extract_fun_ty_arg ty in
               (* Next, get the result type. *)
               let q_type = Types.extract_fun_ty_result ty in
               (* We bind the current parameter to the "head-type" *)
               (* and continue with the remaining parameters using *)
               (* the "tail-type".                                 *)
               rec_bind ((h, (Some h_type)) :: accu_bindings) q_type q in

         (* ********************** *)
         (* Now, let's do the job. *)
         let (revd_mapping, result_ty) =
           rec_bind [] type_from_scheme params_names in
         (* Put the resulting mapping in the right order. *)
         ((List.rev revd_mapping), result_ty, generalized_instanciated_vars)
       with _ ->
         (* Because the typechecking was done in the previous passes, the   *)
         (* program must be well-typed at this point. Then unification must *)
         (* always be successfull. If it fails, then there is a bug         *)
         (* somewhere else before !                                         *)
         assert false
       end)
;;




(* ************************************************************************ *)
(* current_species: Parsetree.qualified_species -> Parsetree.vname list ->  *)
(*   Dep_analysis.name_node list -> Parsetree.vname ->                      *)
(*     Parsetree.expr ->                                                    *)
(*       ((Parsetree.vname * Parsetree_utils.DepNameSet.t) list *           *)
(*        (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *    *)
(*        (string * Types.type_simple) list)                                *)
(** {b Descr} : Pre-process a field before its compilation to OCaml. We
        compute here the information related to the extra parameters
        a method will have by lambda-lifting due to the species parameters
        and the dependencies of the method.
        We extract the methods we decl-depend on, the methods of the
        species parameters we depend on, and finally the list of formal
        parameters (name and type) the method will have due to the 2
        dependency infos we computed. This last list will be straight
        printed by the code generator, but will also be recorded in the
        context for the case we need to generate the code of a recursive
        method. This way, the recursive application of the method will have
        to and will be able to use these extra parameters in addition to
        those effectively passed in the FoCaL code.

    {b Rem} : Not exported oustide this module.                             *)
(* ************************************************************************ *)
let compute_lambda_liftings_for_field ~current_species
     species_parameters_names dependency_graph_nodes name body =
  (* Get all the methods we directly decl-depend on. They will   *)
  (* lead each to an extra parameter of the final OCaml function *)
  (* (lambda-lifing).                                            *)
  let decl_children =
    (try
      let my_node =
        List.find
          (fun { Dep_analysis.nn_name = n } -> n = name)
          dependency_graph_nodes in
      (* Only keep "decl-dependencies" . *)
      List.filter
        (function
          | (_, Dep_analysis.DK_decl) -> true
          | (_, Dep_analysis.DK_def) -> false)
        my_node.Dep_analysis.nn_children
    with Not_found -> []  (* No children at all. *)) in
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
          Param_dep_analysis.param_deps_expr
            ~current_species species_param_name body in
        (* Return a couple binding the species parameter's name with the *)
        (* methods of it we found as required for the current method.    *)
        (species_param_name, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* Build the list by side effect in reverse order for efficiency. *)
  let revd_lambda_lifts = ref [] in
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
          revd_lambda_lifts := (llift_name, meth_ty) :: !revd_lambda_lifts)
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
  (dependencies_from_params, decl_children, (List.rev !revd_lambda_lifts))
;;
