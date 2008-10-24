(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            William Bartlett                                         *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: recursion.mli,v 1.4 2008-10-24 10:42:28 pessaux Exp $ *)

exception NestedRecursiveCalls of Parsetree.vname * Location.t
exception PartialRecursiveCall of Parsetree.vname * Location.t
exception MutualRecursion

type binding =
  | B_let of Parsetree.binding
  | B_match of (Parsetree.expr * Parsetree.pattern)
  | B_condition of (Parsetree.expr * bool)

val list_recursive_calls:
  Parsetree.vname -> (Parsetree.vname * Types.type_simple) list ->
    binding list -> Parsetree.expr ->
      (((Parsetree.vname * Types.type_simple) * Parsetree.expr) list *
         binding list)
        list

val is_structural:
  Parsetree.vname -> (Parsetree.vname * Types.type_simple) list ->
    Parsetree.vname -> Parsetree.expr -> bool
