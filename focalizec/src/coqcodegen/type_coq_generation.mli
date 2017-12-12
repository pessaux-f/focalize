(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ...  LIP6 and INRIA                                      *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

exception Mutable_record_fields_not_in_coq of (Location.t * Parsetree.vname)

val type_def_compile :
  as_zenon_fact: bool -> Context.reduced_compil_context -> Env.CoqGenEnv.t ->
  is_first:bool -> is_last:bool -> Parsetree.vname ->
    Env.TypeInformation.type_description -> Env.CoqGenEnv.t

val type_defs_compile :
  Env.CoqGenEnv.t -> Format.formatter -> current_unit: Types.fname ->
  as_zenon_fact: bool ->
  (Parsetree.vname * Env.CoqGenInformation.type_info) list ->
    Env.CoqGenEnv.t
