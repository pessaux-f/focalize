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

(* $Id: miscHelpers.ml,v 1.2 2008-04-24 13:30:41 pessaux Exp $ *)


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


type collection_effective_arguments =
  | CEA_collection_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr
;;


type species_parameter_kind =
  | SPK_in
  | SPK_is
;;

(* ************************************************************************* *)
(* Parsetree.species_param list ->                                           *)
(*   (Parsetree.vname * Env.ScopeInformation.species_parameter_kind) list    *)
(*     collection_effective_arguments list                                   *)
(** {b Descr} : Extract the collections names used in an "implements" clause
       as arguments of the species that it used to make the collection.
       The parsetree encodes these parameters [Parsetree.expr]s but this
       is a too large structure for the actual legal parameters expressions.
       Then we extracts here just the names of effective collections hidden
       in these [Parsetree.expr]s.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let get_implements_effectives species_params_exprs species_formals_info =
  List.map2
    (fun param_expr (_, param_kind) ->
      let Parsetree.SP expr = param_expr.Parsetree.ast_desc in
      match param_kind with
       | SPK_is ->
           (begin
           match expr.Parsetree.ast_desc with
            | Parsetree.E_constr (cstr_ident, []) ->
                let Parsetree.CI effective_species_name =
                  cstr_ident.Parsetree.ast_desc in
                CEA_collection_name_for_is effective_species_name
            | _ ->
                (* Collections expressions used as parameters of an      *)
                (* "implements" clause should always be represented by   *)
                (* a sum-type value and because these collections are    *)
                (* not parametrized, this value should have no argument. *)
                (* If it's not the case here, then we missed something   *)
                (* before during the analyses !                          *)
                assert false
           end)
       | SPK_in ->
           (* For an entity parameter, all first-class expressions are legal. *)
           CEA_value_expr_for_in expr)
    species_params_exprs
    species_formals_info
;;
