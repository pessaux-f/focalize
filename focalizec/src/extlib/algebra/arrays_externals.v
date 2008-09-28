(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Renaud Rioboo                                            *)
(*            Virgile Prevosto                                         *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: arrays_externals.v,v 1.1 2008-09-28 15:06:48 rr Exp $ *)

Require basics.

(* ********************************* *)
(* Dummy mapping of arrays into Coq. *)

Inductive bi__array (a : Set) : Set := .

Parameter bi__array_set : forall a : Set,
  (bi__array a) -> basics.int__t -> a -> basics.unit__t.

Parameter bi__array_create : forall a : Set, basics.int__t -> a-> bi__array a.

Parameter bi__array_get : forall a: Set, (bi__array a) -> basics.int__t -> a.
