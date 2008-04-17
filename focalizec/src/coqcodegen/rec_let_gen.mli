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

(* $Id: rec_let_gen.mli,v 1.1 2008-04-17 17:02:25 pessaux Exp $ *)

val generate_termination_lemmas :
  'a -> 'b -> 'c -> in_species: 'd -> 'e -> Parsetree.binding_body -> unit
