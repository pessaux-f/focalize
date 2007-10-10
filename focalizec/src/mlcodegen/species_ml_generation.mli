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

(* $Id: species_ml_generation.mli,v 1.2 2007-10-10 15:27:43 pessaux Exp $ *)


val species_compile :
  current_unit:Types.fname -> Format.formatter ->
    Parsetree.species_def -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list ->
	((Parsetree.vname list) *
	 ((Parsetree.vname * Parsetree_utils.VnameSet.t) list))
	option

val collection_compile :
  current_unit: Types.fname -> Format.formatter -> Env.MlGenEnv.t ->
    Parsetree.coll_def -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list -> unit
