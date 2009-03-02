(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: abstractions.mli,v 1.24 2009-03-02 10:19:30 weis Exp $ *)

type environment_kind =
  | EK_ml of Env.MlGenEnv.t
  | EK_coq of Env.CoqGenEnv.t

type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_logical_expr of Parsetree.logical_expr
  | FBK_proof of Parsetree.proof option

type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  ai_dependencies_from_params :
    (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list ;
  ai_min_coq_env : MinEnv.min_coq_env_element list
}

type field_abstraction_info =
  | FAI_sig of (Env.TypeInformation.sig_field_info * abstraction_info)
  | FAI_let of (Env.TypeInformation.let_field_info * abstraction_info)
  | FAI_let_rec of (Env.TypeInformation.let_field_info * abstraction_info) list
  | FAI_theorem of (Env.TypeInformation.theorem_field_info * abstraction_info)
  | FAI_property of (Env.TypeInformation.property_field_info * abstraction_info)

val compute_abstractions_for_fields :
  with_def_deps_n_term_pr : bool -> environment_kind ->
    Context.species_compil_context -> Env.TypeInformation.species_field list ->
      field_abstraction_info list

val compute_abstractions_for_toplevel_theorem :
  Context.species_compil_context -> Parsetree.theorem_def -> abstraction_info

(* For debugging purpose only.
val debug_print_dependencies_from_parameters :
  (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) list ->
    unit
val debug_print_dependencies_from_parameters2 :
  (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list ->
    unit
*)
