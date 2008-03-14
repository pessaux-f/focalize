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


(* $Id: substColl.mli,v 1.3 2008-03-14 14:44:00 pessaux Exp $ *)

type substitution_replaced_collection_kind =
    SRCK_coll of Types.type_collection
  | SRCK_self


val subst_prop :
  current_unit: Types.fname -> substitution_replaced_collection_kind ->
    Types.substitution_by_replacement_collection_kind -> Parsetree.prop ->
      Parsetree.prop

val subst_species_field :
  current_unit: Types.fname -> substitution_replaced_collection_kind ->
    Types.substitution_by_replacement_collection_kind ->
      Env.TypeInformation.species_field -> Env.TypeInformation.species_field
