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


(* $Id: dep_analysis.mli,v 1.1 2007-08-31 13:45:52 pessaux Exp $ *)

exception Ill_formed_species of Types.species_name

val ensure_species_well_formed :
  current_species: Types.collection_name ->
    Env.TypeInformation.species_field list -> unit
