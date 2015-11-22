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

val generate_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  Env.DkGenEnv.t -> Parsetree.expr ->
    unit

val generate_logical_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  Env.DkGenEnv.t -> Parsetree.logical_expr ->
    unit

type let_binding_pre_computation = {
  lbpc_value_body : Env.DkGenInformation.value_body ;
  lbpc_nb_polymorphic_args : int ;
  lbpc_params_with_type : (Parsetree.vname * Types.type_simple option) list ;
  lbpc_result_ty : Types.type_simple option ;
  lbpc_generalized_vars : Types.type_variable list
}

val pre_compute_let_bindings_infos_for_rec :
  rec_status:Env.DkGenInformation.rec_status -> toplevel:bool ->
  Env.DkGenEnv.t -> Parsetree.binding_desc Parsetree.ast list ->
    (Env.DkGenEnv.t * (let_binding_pre_computation list))

val let_binding_compile :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  rec_status:Env.DkGenInformation.rec_status -> toplevel: bool ->
  Env.DkGenEnv.t -> Parsetree.binding -> let_binding_pre_computation ->
    Env.DkGenEnv.t

val generate_record_type :
  Context.species_compil_context ->
    Env.DkGenEnv.t -> Env.TypeInformation.species_description ->
      (Parsetree.vname * Env.TypeInformation.field_abstraction_info) list ->
        (Parsetree.vname * Env.ordered_methods_from_params) list
