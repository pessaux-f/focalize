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

(* $Id: depGraphData.mli,v 1.4 2009-05-06 20:06:49 pessaux Exp $ *)



(* ************************************************************************* *)
(** {b Descr} Describes for a "decl" dependency , the 3 cases of its origin.
    A  "decl" dependency can come from the type or the body or the
    termination proof of a method. In other words, this means wether it
    comes from the proposition of a theorem/property or if it comes from
    the proof of a theorem or if it comes from the termination proof of a
    recursive function.
    For a "let", the dependency is always considered as coming from
    the "BODY".

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
type decl_dependency_kind =
  | DcDK_from_type
  | DcDK_from_body
  | DcDK_from_term_proof
;;



type def_dependency_kind =
  | DfDK_not_from_term_proof (** The dependency doesn't come from a termination
				 proof. *)
  | DfDK_from_term_proof     (** The dependency comes from a termination
				 proof. *)
;;



(* ************************************************************** *)
(** {b Descr} : Describes the kind of dependency between 2 nodes.
    Can be either "def" or "decl" dependency. Note that "Let",
     "Sig", and "Let_rec" methods can't have "decl" dependencies.

    {b Rem} : Exported outside this module.                       *)
(* ************************************************************** *)
type dependency_kind =
  | DK_decl of decl_dependency_kind
  | DK_def of def_dependency_kind
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
  (** Means that the current names depends on the children nodes. I.e. the
      current name's body contains calls to the children names. *)
  mutable nn_children : (name_node * dependency_kind) list
} ;;

