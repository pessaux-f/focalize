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

(* $Id: rec_let_gen.mli,v 1.6 2008-10-24 10:42:28 pessaux Exp $ *)

val generate_termination_lemmas :
  Context.species_compil_context -> Types.coq_print_context ->
    Env.CoqGenEnv.t ->
      (((Parsetree.vname * Types.type_simple) * Parsetree.expr) list *
         Recursion.binding list)
        list ->
          unit

val transform_recursive_calls_args_into_tuple :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    Parsetree.vname -> Parsetree.expr -> Parsetree.expr
