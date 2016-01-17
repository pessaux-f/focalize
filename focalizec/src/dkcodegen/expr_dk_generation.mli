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

type self_methods_status =
  | SMS_from_param of Parsetree.vname
  | SMS_abstracted
  | SMS_from_record

type recursive_methods_status =
  | RMS_abstracted
  | RMS_regular

type let_binding_pre_computation = {
  lbpc_value_body : Env.DkGenInformation.value_body ;
  lbpc_params_with_type : (Parsetree.vname * Types.type_simple option) list ;
  lbpc_result_ty : Types.type_simple option ;
  lbpc_generalized_vars : Types.type_variable list
}

val generate_expr :
  Context.species_compil_context ->
  in_recursive_let_section_of: Parsetree.vname list ->
  local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status ->
  recursive_methods_status: recursive_methods_status ->
  Env.DkGenEnv.t -> Parsetree.expr ->
  unit

val pre_compute_let_bindings_infos_for_rec :
  rec_status:Env.DkGenInformation.rec_status -> toplevel:bool ->
  Env.DkGenEnv.t -> Parsetree.binding_desc Parsetree.ast list ->
    (Env.DkGenEnv.t * (let_binding_pre_computation list))
