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


(* $Id: scoping.mli,v 1.14 2008-04-08 13:03:15 pessaux Exp $ *)

exception Non_logical_let_cant_define_logical_expr of (Parsetree.vname * Location.t)
exception Multiply_used_module of Types.fname
exception Module_not_specified_as_used of Types.fname
exception Self_cant_parameterize_itself of Location.t
exception Is_parameter_only_coll_ident of Location.t
exception Parametrized_species_wrong_arity of (Location.t * int * int)
exception Invalid_external_binding_identifier of (Location.t * Parsetree.vname)
exception Structural_termination_only_on_fun_arg of
  (Location.t * Parsetree.vname)
exception Termination_proof_delayed_only_on_self_meth of
  (Location.t *  Parsetree.vname)

val scope_file :
  Types.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
