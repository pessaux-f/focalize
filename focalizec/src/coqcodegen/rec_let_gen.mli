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

(* $Id: rec_let_gen.mli,v 1.4 2008-07-16 13:24:19 pessaux Exp $ *)

val generate_termination_lemmas :
  'a -> 'b -> 'c ->
    ((Parsetree.vname * Parsetree.expr) list * Recursion.binding list) list ->
      unit
