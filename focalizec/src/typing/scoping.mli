(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


exception Non_logical_let_cant_define_logical_expr of (Parsetree.vname * Location.t)
exception Module_not_specified_as_used of (Location.t * Types.fname)
exception Self_cant_parameterize_itself of Location.t
exception Is_parameter_only_coll_ident of Location.t
exception Parametrized_species_wrong_arity of (Location.t * int * int)
exception Invalid_external_binding_identifier of (Location.t * Parsetree.vname)
exception Termination_only_on_fun_arg of
  (Location.t * Parsetree.vname)
exception Termination_proof_delayed_only_on_self_meth of
  (Location.t *  Parsetree.vname)
exception Ambiguous_logical_expression_or of (int * Location.t)
exception Ambiguous_logical_expression_and of (int * Location.t)
exception Rebound_hyp_notation_or_var_in_proof of (Parsetree.vname * Location.t)
exception Proof_by_species_property of (Parsetree.expr_ident * Location.t)
exception Toplevel_species_as_effective_param of (Parsetree.ident * Location.t)

val scope_file :
  Types.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
