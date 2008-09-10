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


(* $Id: dep_analysis.mli,v 1.15 2008-09-10 15:12:27 pessaux Exp $ *)


type decl_dependency_kind = DDK_from_type | DDK_from_body

type dependency_kind = DK_decl of decl_dependency_kind | DK_def

type name_node = {
  nn_name : Parsetree.vname ;
  nn_type : Types.type_simple ;
  mutable nn_children : (name_node * dependency_kind) list
}

exception Ill_formed_species of
  (Parsetree.qualified_vname * name_node * name_node list)

val ensure_species_well_formed :
  current_species: Parsetree.qualified_species ->
    Env.TypeInformation.species_field list -> unit

val ordered_names_list_of_fields :
  Env.TypeInformation.species_field list ->
    (Parsetree.vname * Types.type_simple) list

val erase_fields_in_context :
  current_species: Parsetree.qualified_species ->
    Parsetree_utils.SelfDepSet.elt list ->
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
