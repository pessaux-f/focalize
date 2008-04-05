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


(* $Id: param_dep_analysis.mli,v 1.9 2008-04-05 18:48:16 weis Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
     Parsetree.expr -> Parsetree_utils.DepNameSet.t

val param_deps_logical_expr :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
    Parsetree.logical_expr -> Parsetree_utils.DepNameSet.t
;;
