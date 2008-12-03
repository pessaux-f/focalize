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

(* $Id: visUniverse.mli,v 1.6 2008-12-03 09:07:26 pessaux Exp $ *)

type in_the_universe_because =
   | IU_only_decl
   | IU_trans_def
;;

module Universe :
  sig
    type key = Parsetree.vname
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
;;

val visible_universe :
  with_def_deps_n_term_pr: bool -> DepGraphData.name_node list ->
  (DepGraphData.name_node * DepGraphData.dependency_kind) list ->
  (DepGraphData.name_node * DepGraphData.dependency_kind) list ->
  in_the_universe_because Universe.t
;;
