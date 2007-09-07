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


(* $Id: dep_analysis.mli,v 1.3 2007-09-07 10:43:31 pessaux Exp $ *)

exception Ill_formed_species of Types.species_name

val ensure_species_well_formed :
  current_species: Types.collection_name ->
    Env.TypeInformation.species_field list -> unit

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list -> Parsetree.vname list

val erase_fields_in_context :
  current_species: Types.collection_name -> Parsetree.vname list ->
    Env.TypeInformation.species_field list ->
      Env.TypeInformation.species_field list

val compute_fields_reordering :
  current_species: Types.collection_name ->
    Env.TypeInformation.species_field list -> Parsetree.vname list

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list -> Parsetree.vname list
