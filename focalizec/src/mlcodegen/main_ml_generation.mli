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

(* $Id: main_ml_generation.mli,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


val root_compile :
  current_unit: Types.fname ->  out_file_name: string ->
    Infer.please_compile_me list -> unit
