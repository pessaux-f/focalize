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


(* $Id: infer.mli,v 1.8 2007-08-23 15:18:42 pessaux Exp $ *)

exception Method_multiply_defined of (Parsetree.vname * Types.species_name)
exception Unbound_type_variable of string
exception Bad_sum_type_constructor_arity of
  (Parsetree.ident * Env.TypeInformation.constructor_arity)
exception Bad_type_arity of (Parsetree.ident * int * int)
exception Rep_multiply_defined of Location.t
exception Parameterized_species_arity_mismatch of string

exception Not_subspecies_conflicting_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_simple * Types.type_simple * Location.t)
exception Not_subspecies_circular_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_simple * Types.type_simple * Location.t)
exception Not_subspecies_arity_mismatch of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Types.type_name * int * int * Location.t)
exception Not_subspecies_missing_field of
  (Types.type_collection * Types.type_collection * Parsetree.vname *
   Location.t)


val typecheck_file : Types.fname -> Parsetree.file -> Env.TypingEnv.t
