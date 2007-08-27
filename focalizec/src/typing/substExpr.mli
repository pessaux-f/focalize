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


(* $Id: substExpr.mli,v 1.1 2007-08-27 11:33:23 pessaux Exp $ *)

val subst_species_field :
  param_unit: Types.fname -> Parsetree.vname ->
    Parsetree.expr_desc -> Env.TypeInformation.species_field ->
      Env.TypeInformation.species_field
