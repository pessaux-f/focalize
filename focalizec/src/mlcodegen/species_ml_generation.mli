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

(* $Id: species_ml_generation.mli,v 1.8 2008-06-04 12:44:18 pessaux Exp $ *)


val species_compile :
   Env.MlGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.species_def -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list ->
        (Env.TypeInformation.species_param list *
         Env.MlGenInformation.method_info list *
         (Env.MlGenInformation.collection_generator_info option))

val collection_compile :
  Env.MlGenEnv.t -> current_unit: Types.fname -> Format.formatter ->
    Parsetree.collection_def -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list -> unit
