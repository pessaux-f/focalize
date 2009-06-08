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


(* $Id: dep_analysis.mli,v 1.23 2009-06-08 15:35:39 pessaux Exp $ *)



exception Ill_formed_species of
  (Parsetree.qualified_vname * DepGraphData.name_node *
   DepGraphData.name_node list)

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
    Env.TypeInformation.species_field list -> DepGraphData.name_node list

val dependencies_graph_to_dotty :
  dirname: string -> current_species: Parsetree.qualified_species ->
    DepGraphData.name_node list -> unit

val order_species_params_methods :
  (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) list ->
    (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list

(* For debugging purpose only. *)
val debug_print_dependencies_from_parameters :
  (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) list ->
    unit
val debug_print_dependencies_from_parameters2 :
  (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list ->
    unit
val debug_print_dependencies_from_parameters3 :
  (Parsetree.vname * Env.ordered_methods_from_params) list ->
    unit

