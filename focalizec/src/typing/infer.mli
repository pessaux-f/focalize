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


(* $Id: infer.mli,v 1.5 2007-08-14 13:20:26 pessaux Exp $ *)

exception Method_multiply_defined of (Parsetree.vname * Types.species_name)
exception Unbound_type_variable of string
exception Bad_sum_type_constructor_arity of
  (Parsetree.ident * Env.TypeInformation.constructor_arity)
exception Bad_type_arity of (Parsetree.ident * int * int)

val typecheck_file : Parsetree.fname -> Parsetree.file -> Env.TypingEnv.t
