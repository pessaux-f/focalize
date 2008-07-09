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

(* $Id: species_record_type_generation.mli,v 1.6 2008-07-09 14:52:28 pessaux Exp $ *)


type self_methods_status = SMS_abstracted | SMS_from_record

val make_Self_cc_binding_abst_T :
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))
val make_Self_cc_binding_rf_T : 
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))

val generate_expr :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    self_methods_status: self_methods_status -> Env.CoqGenEnv.t ->
      Parsetree.expr -> unit
val generate_logical_expr :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    self_methods_status: self_methods_status -> Env.CoqGenEnv.t ->
      Parsetree.logical_expr -> unit
val let_binding_compile :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status -> is_rec: bool ->
    Env.CoqGenEnv.t -> Parsetree.binding -> Env.CoqGenEnv.t
val generate_record_type :
  Context.species_compil_context ->
    Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->
      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list
