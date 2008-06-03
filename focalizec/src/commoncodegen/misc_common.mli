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

(* $Id: misc_common.mli,v 1.1 2008-06-03 15:40:36 pessaux Exp $ *)

type collection_effective_arguments =
  | CEA_collection_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr

val get_implements_effectives :
  Parsetree.species_param_desc Parsetree.ast list ->
    ('a * Env.ScopeInformation.species_parameter_kind) list ->
      collection_effective_arguments list
