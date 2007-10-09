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

(* $Id: species_ml_generation.mli,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


val species_compile :
  current_unit:Types.fname -> Format.formatter ->
    Parsetree.vname -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list -> unit
val collection_compile :
  current_unit: Types.fname -> Format.formatter ->
    Parsetree.coll_def -> Env.TypeInformation.species_description ->
      Dep_analysis.name_node list -> unit
