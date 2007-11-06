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

(* $Id: type_ml_generation.mli,v 1.2 2007-11-06 10:14:58 pessaux Exp $ *)


val type_def_compile :
  Misc_ml_generation.reduced_compil_context -> Env.MlGenEnv.t ->
  Parsetree.vname -> Env.TypeInformation.type_description -> Env.MlGenEnv.t
