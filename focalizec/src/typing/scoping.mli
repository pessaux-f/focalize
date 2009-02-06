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


(* $Id: scoping.mli,v 1.16 2009-02-06 15:28:27 pessaux Exp $ *)

exception Non_logical_let_cant_define_logical_expr of (Parsetree.vname * Location.t)
exception Module_not_specified_as_used of (Location.t * Types.fname)
exception Self_cant_parameterize_itself of Location.t
exception Is_parameter_only_coll_ident of Location.t
exception Parametrized_species_wrong_arity of (Location.t * int * int)
exception Invalid_external_binding_identifier of (Location.t * Parsetree.vname)
exception Structural_termination_only_on_fun_arg of
  (Location.t * Parsetree.vname)
exception Termination_proof_delayed_only_on_self_meth of
  (Location.t *  Parsetree.vname)
exception Ambiguous_logical_expression_or of (int * Location.t)
exception Ambiguous_logical_expression_and of (int * Location.t)

val scope_file :
  Types.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
