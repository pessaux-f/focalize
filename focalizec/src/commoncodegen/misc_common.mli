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

(* $Id: misc_common.mli,v 1.13 2009-05-28 08:43:26 pessaux Exp $ *)

type compiled_field_memory = {
  cfm_is_logical : bool ;
  cfm_from_species : Env.from_history;
  cfm_method_name : Parsetree.vname;
  cfm_method_scheme : Env.method_type_kind;
  cfm_used_species_parameter_tys : Parsetree.vname list;
  cfm_dependencies_from_parameters :
    (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list;
  cfm_coq_min_typ_env_names : Parsetree.vname list;
}

type compiled_species_fields =
  | CSF_sig of compiled_field_memory
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of compiled_field_memory
  | CSF_property of compiled_field_memory

type collection_effective_arguments =
  | CEA_collection_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr

val get_implements_effectives :
  Parsetree.species_param_desc Parsetree.ast list ->
  ('a * Env.ScopeInformation.species_parameter_kind) list ->
  collection_effective_arguments list

val follow_instanciations_for_in_param :
  Context.species_compil_context -> Abstractions.environment_kind ->
  Parsetree.vname -> Parsetree.module_name -> int ->
  (Parsetree.qualified_species * Parsetree_utils.simple_species_expr * 'a)
      list ->
	Parsetree.expr

type is_parameter_instanciation =
  | IPI_by_toplevel_collection of Types.type_collection
  | IPI_by_toplevel_species of Types.type_collection
  | IPI_by_species_parameter of Env.TypeInformation.species_param

val follow_instanciations_for_is_param :
  Context.species_compil_context -> Abstractions.environment_kind -> int ->
  (Parsetree.qualified_species * Parsetree_utils.simple_species_expr * 'a)
      list ->
	is_parameter_instanciation

val find_toplevel_spe_defining_meth_through_inheritance :
  Abstractions.environment_kind -> current_unit: Parsetree.module_name ->
  start_spec_mod: Parsetree.module_name -> start_spec_name: string ->
  method_name: Parsetree.vname -> (Parsetree.module_name * string)

type let_connector =
  | LC_first_non_rec
  | LC_first_rec
  | LC_following


val make_params_list_from_abstraction_info :
  care_logical: bool -> care_types: bool -> Abstractions.abstraction_info ->
  string list

val find_self_representation_if_some :
  Abstractions.field_abstraction_info list -> Types.type_simple option
