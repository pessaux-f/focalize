(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parsetree_utils.mli,v 1.23 2008-11-29 23:23:18 weis Exp $ *)

val name_of_vname : Parsetree.vname -> string
(** Extracts the inner string name of the [vname] variable name. *)
;;

module SelfDepSet :
  sig
    type elt = (Parsetree.vname * Types.type_simple)
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
;;

type dependency_elem_type_kind =
  | DETK_computational of Types.type_simple
  | DETK_logical of Parsetree.logical_expr
;;

module ParamDepSet :
  sig
    type elt = (Parsetree.vname * dependency_elem_type_kind)
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
;;

val list_to_param_dep_set : ParamDepSet.elt list -> ParamDepSet.t
;;

val param_dep_set_find :
  (ParamDepSet.elt -> bool) -> ParamDepSet.t -> ParamDepSet.elt
;;

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
;;

val get_local_idents_from_pattern : Parsetree.pattern -> Parsetree.vname list
;;

val get_local_idents_and_types_from_pattern :
  Parsetree.pattern ->
  (Parsetree.vname * Parsetree.ast_node_type_information) list
;;

val pp_vname_with_operators_expanded :
  Format.formatter -> Parsetree.vname -> unit
;;

val vname_as_string_with_operators_expanded : Parsetree.vname -> string
;;

val type_coll_from_qualified_species :
  Parsetree.qualified_species -> Types.type_collection
;;

type simple_species_expr_as_effective_parameter =
  | SPE_Self
  | SPE_Species of (Parsetree.qualified_vname * Types.species_collection_kind)
  | SPE_Expr_entity of Parsetree.expr
;;

type simple_species_expr = {
  sse_name : Parsetree.ident ;
  sse_effective_args : simple_species_expr_as_effective_parameter list;
}
;;

val make_pseudo_species_ident :
  current_unit: Parsetree.module_name -> Parsetree.qualified_species ->
  Parsetree.ident
;;

val make_concatenated_name_from_qualified_vname :
  Parsetree.qualified_vname -> string
;;

val make_concatenated_name_with_operators_expanded_from_qualified_vname :
  Parsetree.qualified_vname -> string
;;
