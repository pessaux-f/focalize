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

(* $Id: recursion.mli,v 1.9 2012-01-30 13:21:25 pessaux Exp $ *)

exception NestedRecursiveCalls of Parsetree.vname * Location.t
exception PartialRecursiveCall of Parsetree.vname * Location.t
exception MutualRecursion of (Parsetree.vname * Parsetree.vname)

type binding =
  | B_let of Parsetree.binding
  | B_match of Parsetree.expr * Parsetree.pattern
  | B_condition of Parsetree.expr * bool

type typed_vname = (Parsetree.vname * Types.type_simple)

type recursive_calls_description =
  ((typed_vname * Parsetree.expr) list * binding list) list

val list_recursive_calls :
  current_unit: Parsetree.module_name -> Parsetree.vname -> typed_vname list ->
  binding list -> Parsetree.expr -> recursive_calls_description

(* [Unsure] Not yet used.
val is_structural :
  current_unit: Parsetree.module_name -> Parsetree.vname -> typed_vname list ->
  Parsetree.vname -> Parsetree.expr -> bool
*)
