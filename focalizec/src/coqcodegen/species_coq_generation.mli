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

(* $Id: species_coq_generation.mli,v 1.5 2008-08-13 15:55:17 pessaux Exp $ *)

exception Attempt_proof_by_def_of_species_param of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_def_of_declared_method_of_self of
  (Location.t * Parsetree.expr_ident)

exception Attempt_proof_by_def_of_local_ident of
  (Location.t * Parsetree.expr_ident)

val species_compile :
  Env.CoqGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.species_def_desc Parsetree.ast ->
      Env.TypeInformation.species_description ->
        Dep_analysis.name_node list ->
          (Env.TypeInformation.species_param list *
           Env.CoqGenInformation.method_info list *
           (Env.CoqGenInformation.collection_generator_info option) *
           Env.collection_or_species)

val collection_compile :
  Env.CoqGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.collection_def_desc Parsetree.ast ->
      Env.TypeInformation.species_description ->
        Dep_analysis.name_node list -> unit
