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

(* $Id: abstractions.mli,v 1.1 2008-02-27 13:42:49 pessaux Exp $ *)

type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_prop of Parsetree.prop

val compute_lambda_liftings_for_field :
  current_unit: Types.fname ->current_species: Parsetree.qualified_species ->
    Parsetree.vname list -> Dep_analysis.name_node list -> Parsetree.vname ->
      field_body_kind ->
        ((Parsetree.vname list) *
         (Parsetree.vname * Parsetree_utils.DepNameSet.t) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (Dep_analysis.name_node * Dep_analysis.dependency_kind) list *
         (string * Types.type_simple) list)
