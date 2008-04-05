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

(* $Id: abstractions.mli,v 1.6 2008-04-05 18:52:44 weis Exp $ *)

type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_logical_expr of Parsetree.logical_expr

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
  | FAI_property of (Env.TypeInformation.property_field_info * abstraction_info)

val compute_abstractions_for_fields :
  with_def_deps : bool -> Context.species_compil_context ->
    Env.TypeInformation.species_field list -> field_abstraction_info list
