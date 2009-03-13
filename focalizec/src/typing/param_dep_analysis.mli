(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: param_dep_analysis.mli,v 1.17 2009-03-13 07:52:05 pessaux Exp $ *)


val guess_method_computational_or_logical :
  Parsetree.vname -> Types.type_simple option ->
    Env.TypeInformation.species_field list ->
      Parsetree_utils.dependency_elem_type_kind

val param_deps_expr :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.expr -> Parsetree_utils.ParamDepSet.t

val param_deps_logical_expr :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.logical_expr -> Parsetree_utils.ParamDepSet.t

val param_deps_proof :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.proof -> Parsetree_utils.ParamDepSet.t

val param_deps_termination_proof :
  current_species: Parsetree.qualified_species ->
    (Parsetree.vname * (Env.TypeInformation.species_field list)) ->
      Parsetree.termination_proof -> Parsetree_utils.ParamDepSet.t
