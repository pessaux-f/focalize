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


(* $Id: dep_analysis.mli,v 1.5 2007-09-25 11:15:59 pessaux Exp $ *)

exception Ill_formed_species of Types.species_name

type dependency_kind = DK_decl | DK_def

module VnameSet :
  sig
    type elt = Parsetree.vname
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end


type name_node = {
  nn_name : Parsetree.vname ;
  mutable nn_children : (name_node * dependency_kind) list
}

val ensure_species_well_formed :
  current_species: Types.collection_name ->
    Env.TypeInformation.species_field list -> name_node list

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
