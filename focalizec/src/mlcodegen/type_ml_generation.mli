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

(* $Id: type_ml_generation.mli,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


val type_def_compile :
  Misc_ml_generation.reduced_compil_context ->
  Parsetree.vname -> Env.TypeInformation.type_description -> unit
