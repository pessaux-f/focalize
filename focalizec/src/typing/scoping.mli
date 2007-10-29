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


(* $Id: scoping.mli,v 1.10 2007-10-29 08:18:36 pessaux Exp $ *) 

exception Multiply_used_module of Types.fname
exception Module_not_specified_as_used of Types.fname
exception Self_cant_parameterize_itself of Location.t
exception Is_parameter_only_coll_ident of Location.t
exception Parametrized_species_wrong_arity of (Location.t * int * int)

val scope_file :
  Types.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
