(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

val make_Self_cc_binding_abst_T :
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))

val make_Self_cc_binding_rf_T :
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))

val make_Self_cc_binding_species_param :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))

val generate_method_lambda_lifted_arguments :
  only_for_Self_meths: bool -> Format.formatter -> Parsetree.vname list ->
    (Env.TypeInformation.species_param *
     Env.ordered_methods_from_params) list ->
       Parsetree.vname list -> unit

val generate_logical_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: Expr_dk_generation.self_methods_status ->
  recursive_methods_status: Expr_dk_generation.recursive_methods_status ->
  Env.DkGenEnv.t -> Parsetree.logical_expr ->
    unit

val generate_record_type :
  Context.species_compil_context ->
    Env.DkGenEnv.t -> Env.TypeInformation.species_description ->
      (Parsetree.vname * Env.TypeInformation.field_abstraction_info) list ->
        (Parsetree.vname * Env.ordered_methods_from_params) list
