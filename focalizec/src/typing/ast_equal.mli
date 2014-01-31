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
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

val logical_expr_equal_p :
  Parsetree.logical_expr -> Parsetree.logical_expr -> bool

val binding_body_equal_p :
  params1: Parsetree.vname list -> params2: Parsetree.vname list ->
  body1: Parsetree.binding_body -> body2: Parsetree.binding_body -> bool
