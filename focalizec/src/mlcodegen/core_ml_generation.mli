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


(* $Id: core_ml_generation.mli,v 1.2 2007-09-25 15:29:10 pessaux Exp $ *)

exception No_external_value_caml_def of (Parsetree.vname * Location.t)
exception No_external_type_caml_def of (Parsetree.vname * Location.t)

val root_compile :
  current_unit: Types.fname ->  out_file_name: string ->
    Infer.please_compile_me list -> unit
