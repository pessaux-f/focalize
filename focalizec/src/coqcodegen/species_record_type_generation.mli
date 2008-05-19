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

(* $Id: species_record_type_generation.mli,v 1.3 2008-05-19 09:14:20 pessaux Exp $ *)


type self_methods_status =
  | SMS_abstracted     (** Must be called "abst_<meth>". *)
  | SMS_from_species   (** Must be called "hosting_species_<meth>". *)
  | SMS_from_self      (** Must be called "self_<meth>". *)
  | SMS_todo
;;

val make_Self_cc_binding_abst_T :
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))
val make_Self_cc_binding_self_T :
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))
val make_Self_cc_binding_current_species_T : 
  current_species: Parsetree.qualified_species ->
    (Types.type_collection * (string * Types.collection_carrier_mapping_info))

val generate_expr :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    self_methods_status: self_methods_status -> in_hyp: bool ->
      Env.CoqGenEnv.t -> Parsetree.expr -> unit
val generate_logical_expr :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    self_methods_status: self_methods_status -> in_hyp: bool ->
      Env.CoqGenEnv.t -> Parsetree.logical_expr -> unit
val let_binding_compile :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
  self_methods_status: self_methods_status -> in_hyp: bool -> is_rec: bool ->
    Env.CoqGenEnv.t -> Parsetree.binding -> Env.CoqGenEnv.t
val generate_record_type :
  Context.species_compil_context ->
    Env.CoqGenEnv.t -> Env.TypeInformation.species_description -> unit
