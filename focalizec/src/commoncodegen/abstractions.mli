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

(* $Id: abstractions.mli,v 1.2 2008-02-28 13:35:23 pessaux Exp $ *)

type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_prop of Parsetree.prop


(* {Unsure] Doit disparaitre de l'interface lorsque Caml et coq procéderont de
  la même façon ! *)
val compute_lambda_liftings_for_field :
  current_unit: Types.fname ->current_species: Parsetree.qualified_species ->
    Parsetree.vname list -> Dep_analysis.name_node list -> Parsetree.vname ->
      field_body_kind ->
        ((Parsetree.vname list) *
         (Parsetree.vname * Parsetree_utils.DepNameSet.t) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (string * Types.type_simple) list)

type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  ai_dependencies_from_params :
    (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ;
  ai_min_coq_env : MinEnv.min_coq_env_element list
}

type field_abstraction_info =
  | FAI_sig of Env.TypeInformation.sig_field_info
  | FAI_let of (Env.TypeInformation.let_field_info * abstraction_info)
  | FAI_let_rec of (Env.TypeInformation.let_field_info * abstraction_info) list
  | FAI_theorem of (Env.TypeInformation.theorem_field_info * abstraction_info)
  | FAI_property of Env.TypeInformation.property_field_info

val compute_abstractions_for_fields :
  Context.species_compil_context -> Env.TypeInformation.species_field list ->
    field_abstraction_info list
