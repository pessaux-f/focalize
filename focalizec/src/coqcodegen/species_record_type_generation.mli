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

(* $Id: species_record_type_generation.mli,v 1.31 2012-10-26 14:55:19 pessaux Exp $ *)

exception Wrong_decreasing_argument of
  (Location.t * Parsetree.qualified_species * Parsetree.vname *
   Parsetree.vname)

type self_methods_status =
  | SMS_from_param of Parsetree.vname
  | SMS_abstracted
  | SMS_from_record

type recursive_methods_status =
  | RMS_abstracted
  | RMS_regular

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

val generate_pattern :
  force_polymorphic_explicit_args: bool -> Context.species_compil_context ->
    Types.coq_print_context ->
    Env.CoqGenEnv.t -> Parsetree.pattern -> unit

val generate_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  Env.CoqGenEnv.t -> Parsetree.expr ->
    unit

val generate_logical_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  Env.CoqGenEnv.t -> Parsetree.logical_expr ->
    unit

type let_binding_pre_computation = {
  lbpc_value_body : Env.CoqGenInformation.value_body ;
  lbpc_params_names : Parsetree.vname list ;
  lbpc_nb_polymorphic_args : int ;
  lbpc_params_with_type : (Parsetree.vname * Types.type_simple option) list ;
  lbpc_result_ty : Types.type_simple option ;
  lbpc_generalized_vars : Types.type_variable list
}

val pre_compute_let_bindings_infos_for_rec :
  is_rec:bool -> toplevel:bool ->
  Env.CoqGenEnv.t -> Parsetree.binding_desc Parsetree.ast list ->
    (Env.CoqGenEnv.t * (let_binding_pre_computation list))

val let_binding_compile :
  Context.species_compil_context ->
  binder: string -> opt_term_proof:Parsetree.termination_proof option ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  is_rec: bool -> toplevel: bool ->
  Env.CoqGenEnv.t -> Parsetree.binding -> let_binding_pre_computation ->
    Env.CoqGenEnv.t

val generate_record_type :
  Context.species_compil_context ->
    Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->
      Abstractions.field_abstraction_info list ->
        (Parsetree.vname * Env.ordered_methods_from_params) list
