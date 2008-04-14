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

(* $Id: species_record_type_generation.mli,v 1.1 2008-04-14 09:52:14 pessaux Exp $ *)


val generate_expr :
  Context.species_compil_context ->
  local_idents:Parsetree.vname list ->
  self_as:Types.coq_self_representation ->
  in_hyp:bool -> Env.CoqGenEnv.t -> Parsetree.expr -> unit
val generate_logical_expr :
  Context.species_compil_context ->
  local_idents:Parsetree.vname list ->
  self_as:Types.coq_self_representation ->
  in_hyp:bool -> Env.CoqGenEnv.t -> Parsetree.logical_expr -> unit
val generate_record_type :
  Context.species_compil_context ->
  Env.CoqGenEnv.t -> Env.TypeInformation.species_description -> unit
