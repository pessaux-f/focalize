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

(* $Id: depGraphData.mli,v 1.1 2008-09-15 09:24:29 pessaux Exp $ *)



(* ***************************************************************** *)
(** {b Descr} Describes for a "decl" dependency , the 2 cases of its
    origine. A  "decl" dependency can come from the type or the body
    of a method. In other words, this means wether it comes from the
    proposition of a theorem/property or if it comes from the proof
    of a theorem.
    For a "let", the dependency is always considered as coming from
    the "BODY".

    {b Rem} : Exported outside this module.                          *)
(* ***************************************************************** *)
type decl_dependency_kind = DDK_from_type | DDK_from_body ;;



(* ************************************************************** *)
(** {b Descr} : Describes the kind of dependency between 2 nodes.
    Can be either "def" or "decl" dependency. Note that "Let",
     "Sig", and "Let_rec" methods can't have "decl" dependencies.

    {b Rem} : Exported outside this module.                       *)
(* ************************************************************** *)
type dependency_kind =
  | DK_decl of decl_dependency_kind
  | DK_def
;;



(* ******************************************************************* *)
(** {b Descr} : Strutrure of a node in a dependency graph representing
    the fact that some names' bodies contain call to non-let-rec-bound
    othernames (relation \lbag n \rbag in Virgile Prevosto's Phd,
    section 3.5, definition 16, page 32.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
type name_node = {
  (** Name of the node, i.e. one name of a species fields. *)
  nn_name : Parsetree.vname ;
  (** The type of the field wearing this name. *)
  nn_type : Types.type_simple ;
  (** Means that the current names depends of the children nodes. I.e. the
      current name's body contains calls to the children names. *)
  mutable nn_children : (name_node * dependency_kind) list
} ;;

