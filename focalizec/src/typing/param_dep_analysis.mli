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


(* $Id: param_dep_analysis.mli,v 1.6 2007-10-30 21:15:07 weis Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_species -> Parsetree.vname ->
     Parsetree.expr -> Parsetree_utils.VnameSet.t
