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


(* $Id: substColl.mli,v 1.2 2007-09-05 14:13:18 pessaux Exp $ *)

type substitution_collection_kind =
    SCK_coll of Types.type_collection
  | SCK_self

val subst_prop :
  current_unit: Types.fname -> substitution_collection_kind ->
    Types.type_collection -> Parsetree.prop -> Parsetree.prop

val subst_species_field :
  current_unit: Types.fname -> substitution_collection_kind ->
    Types.type_collection -> Env.TypeInformation.species_field ->
      Env.TypeInformation.species_field
