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


(* $Id: param_dep_analysis.mli,v 1.11 2008-09-10 08:14:47 pessaux Exp $ *)

val param_deps_expr :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.expr -> Parsetree_utils.ParamDepNameSet.t

val param_deps_logical_expr :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.logical_expr -> Parsetree_utils.ParamDepNameSet.t

val param_deps_proof :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.proof -> Parsetree_utils.ParamDepNameSet.t
