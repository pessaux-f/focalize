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


(* $Id: param_dep_analysis.mli,v 1.3 2007-10-02 09:29:36 pessaux Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_vname -> Parsetree.vname ->
    Parsetree.expr -> Dep_analysis.VnameSet.t
