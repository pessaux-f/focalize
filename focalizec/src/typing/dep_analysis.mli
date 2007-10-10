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


(* $Id: dep_analysis.mli,v 1.9 2007-10-10 15:27:43 pessaux Exp $ *)

exception Ill_formed_species of Parsetree.qualified_vname

type dependency_kind = DK_decl | DK_def

type name_node = {
  nn_name : Parsetree.vname ;
  mutable nn_children : (name_node * dependency_kind) list
}

val ensure_species_well_formed :
  current_species: Parsetree.qualified_vname ->
    Env.TypeInformation.species_field list -> unit

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list -> Parsetree.vname list

val erase_fields_in_context :
  current_species: Parsetree.qualified_vname -> Parsetree.vname list ->
    Env.TypeInformation.species_field list ->
      Env.TypeInformation.species_field list

val compute_fields_reordering :
  current_species: Parsetree.qualified_vname ->
    Env.TypeInformation.species_field list -> Parsetree.vname list

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list -> Parsetree.vname list

val build_dependencies_graph_for_fields :
  current_species:Types.fname * Parsetree.vname ->
    Env.TypeInformation.species_field list -> name_node list

val dependencies_graph_to_dotty :
  dirname: string -> current_species: Parsetree.qualified_vname ->
    name_node list -> unit
