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


(* $Id: param_dep_analysis.mli,v 1.4 2007-10-10 15:27:43 pessaux Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_vname -> Parsetree.vname ->
    Parsetree.expr -> Parsetree_utils.VnameSet.t
