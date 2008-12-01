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


(* $Id: substExpr.mli,v 1.3 2008-12-01 14:40:21 pessaux Exp $ *)
val subst_expr :
  param_unit: Parsetree.module_name -> Parsetree.vname ->
    by_expr: Parsetree.expr_desc -> in_expr: Parsetree.expr -> Parsetree.expr

val subst_prop :
  param_unit: Parsetree.module_name -> Parsetree.vname ->
    Parsetree.expr_desc -> Parsetree.logical_expr -> Parsetree.logical_expr

val subst_species_field :
  param_unit: Types.fname -> Parsetree.vname ->
    Parsetree.expr_desc -> Env.TypeInformation.species_field ->
      Env.TypeInformation.species_field
