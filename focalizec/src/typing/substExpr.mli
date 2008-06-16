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


(* $Id: substExpr.mli,v 1.2 2008-06-16 12:59:42 pessaux Exp $ *)
val subst_expr :
  param_unit: Parsetree.module_name -> Parsetree.vname ->
    by_expr: Parsetree.expr_desc -> in_expr: Parsetree.expr -> Parsetree.expr

val subst_species_field :
  param_unit: Types.fname -> Parsetree.vname ->
    Parsetree.expr_desc -> Env.TypeInformation.species_field ->
      Env.TypeInformation.species_field
