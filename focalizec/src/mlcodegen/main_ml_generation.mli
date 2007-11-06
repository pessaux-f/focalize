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

(* $Id: main_ml_generation.mli,v 1.2 2007-11-06 10:14:58 pessaux Exp $ *)


val root_compile :
  current_unit: Types.fname ->  out_file_name: string ->
    Infer.please_compile_me list -> Env.MlGenEnv.t
