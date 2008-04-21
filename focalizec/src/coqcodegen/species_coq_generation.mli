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

(* $Id: species_coq_generation.mli,v 1.1 2008-04-21 11:51:18 pessaux Exp $ *)

val species_compile :
  Env.CoqGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.species_def_desc Parsetree.ast ->
      Env.TypeInformation.species_description ->
	Dep_analysis.name_node list ->
	  (Env.CoqGenInformation.method_info list *
	     (Env.CoqGenInformation.collection_generator_info option))
