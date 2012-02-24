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


(* $Id: substColl.mli,v 1.6 2012-02-24 14:37:44 pessaux Exp $ *)

type substitution_replaced_collection_kind =
  | SRCK_coll of Types.type_collection
  | SRCK_self

val subst_logical_expr :
  current_unit: Types.fname -> substitution_replaced_collection_kind ->
    Types.substitution_by_replacement_collection_kind ->
      Parsetree.logical_expr -> Parsetree.logical_expr

val subst_species_field :
  current_unit: Types.fname -> substitution_replaced_collection_kind ->
    Types.substitution_by_replacement_collection_kind ->
      Env.TypeInformation.species_field -> Env.TypeInformation.species_field
