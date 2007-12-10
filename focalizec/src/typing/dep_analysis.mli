(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: dep_analysis.mli,v 1.11 2007-12-10 10:14:07 pessaux Exp $ *)

exception Ill_formed_species of Parsetree.qualified_vname

type dependency_kind = DK_decl | DK_def

type name_node = {
  nn_name : Parsetree.vname ;
  nn_type : Types.type_simple ;
  mutable nn_children : (name_node * dependency_kind) list
}

val ensure_species_well_formed :
  current_species: Parsetree.qualified_species ->
    Env.TypeInformation.species_field list -> unit

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list -> Parsetree_utils.DepNameSet.elt list

val erase_fields_in_context :
  current_species: Parsetree.qualified_species ->
    Parsetree_utils.DepNameSet.elt list ->
      Env.TypeInformation.species_field list ->
        Env.TypeInformation.species_field list

val compute_fields_reordering :
  current_species: Parsetree.qualified_species ->
    Env.TypeInformation.species_field list -> Parsetree.vname list

val build_dependencies_graph_for_fields :
  current_species:Types.fname * Parsetree.vname ->
    Env.TypeInformation.species_field list -> name_node list

val dependencies_graph_to_dotty :
  dirname: string -> current_species: Parsetree.qualified_species ->
    name_node list -> unit
