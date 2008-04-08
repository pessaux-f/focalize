(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            William Bartlett                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: recursion.mli,v 1.1 2008-04-08 15:26:02 bartlett Exp $ *)

exception NestedRecursiveCalls
exception PartialRecursiveCall

type binding =
  | B_let of Parsetree.binding
  | B_match of Parsetree.pattern * Parsetree.expr
  | B_condition of Parsetree.expr * bool

val list_recursive_calls:
  Parsetree.vname ->
  Parsetree.vname list ->
  binding list ->
  Parsetree.expr ->
  ((Parsetree.vname * Parsetree.expr) list * binding list) list

val is_structural:
  Parsetree.vname ->
  Parsetree.vname list ->
  Parsetree.vname ->
  Parsetree.expr ->
  bool
