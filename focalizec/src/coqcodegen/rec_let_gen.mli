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

(* $Id: rec_let_gen.mli,v 1.9 2012-02-08 16:35:29 pessaux Exp $ *)

val generate_termination_lemmas :
  Context.species_compil_context -> Types.coq_print_context ->
  Env.CoqGenEnv.t ->
  explicit_order:
    (Parsetree.vname *
     (Parsetree.vname list) *
      ((Env.TypeInformation.species_param *
         Env.ordered_methods_from_params) list) *
      Parsetree.vname list)
   option ->
  Recursion.recursive_calls_description ->
    unit

val transform_recursive_calls_args_into_tuple :
  Context.species_compil_context -> local_idents: Parsetree.vname list ->
    Parsetree.vname -> Parsetree.expr -> Parsetree.expr
