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

(* $Id: randoms_externals.v,v 1.1 2008-09-28 15:06:48 rr Exp $ *)

Require basics.
Require ZArith.
Open Scope Z_scope.

(* ****************************************** *)
(* Dummy mapping of random integers into Coq. *)

Definition random_seed (seed : basics.int__t) := coq_builtins.Void.
Definition random_int (foo : basics.int__t) : basics.int__t := 42.
Definition random_self_init (x : basics.unit__t) := coq_builtins.Void.
