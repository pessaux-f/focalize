(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: scoping.mli,v 1.11 2007-10-30 13:27:59 pessaux Exp $ *) 

exception Multiply_used_module of Types.fname
exception Module_not_specified_as_used of Types.fname
exception Self_cant_parameterize_itself of Location.t
exception Is_parameter_only_coll_ident of Location.t
exception Parametrized_species_wrong_arity of (Location.t * int * int)
exception Invalid_external_binding_identifier of (Location.t * Parsetree.vname)

val scope_file :
  Types.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
