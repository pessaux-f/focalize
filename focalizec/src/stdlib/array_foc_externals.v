(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Renaud Rioboo                                            *)
(*            Virgile Prevosto                                         *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: array_foc_externals.v,v 1.1 2008-09-11 09:26:21 pessaux Exp $ *)

Require basics.

(* ********************************* *)
(* Dummy mapping of arrays into Coq. *)

Inductive bi__array (a : Set) : Set := .

Parameter bi__array_set : forall a : Set,
  (bi__array a) -> basics.int__t -> a -> basics.unit__t.

Parameter bi__array_create : forall a : Set, basics.int__t -> a-> bi__array a.

Parameter bi__array_get : forall a: Set, (bi__array a) -> basics.int__t -> a.
