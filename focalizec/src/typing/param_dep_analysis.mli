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


(* $Id: param_dep_analysis.mli,v 1.7 2007-12-07 15:19:37 pessaux Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
     Parsetree.expr -> Parsetree_utils.VnameSet.t

val param_deps_prop :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
    Parsetree.prop -> Parsetree_utils.VnameSet.t
