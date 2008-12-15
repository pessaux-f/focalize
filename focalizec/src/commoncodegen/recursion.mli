(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*                                                                     *)
(*            William Bartlett                                         *)
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

(* $Id: recursion.mli,v 1.7 2008-12-15 17:09:45 pessaux Exp $ *)

exception NestedRecursiveCalls of Parsetree.vname * Location.t
exception PartialRecursiveCall of Parsetree.vname * Location.t
exception MutualRecursion of (Parsetree.vname * Parsetree.vname)

type binding =
  | B_let of Parsetree.binding
  | B_match of Parsetree.expr * Parsetree.pattern
  | B_condition of Parsetree.expr * bool

type typed_vname = Parsetree.vname * Types.type_simple

val list_recursive_calls:
  Parsetree.vname -> typed_vname list ->
    binding list -> Parsetree.expr ->
      ((typed_vname * Parsetree.expr) list * binding list) list

val is_structural:
  Parsetree.vname -> typed_vname list -> Parsetree.vname -> Parsetree.expr ->
    bool
