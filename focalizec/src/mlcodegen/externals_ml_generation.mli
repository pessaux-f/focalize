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

(* $Id: externals_ml_generation.mli,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


exception No_external_value_caml_def of (Parsetree.vname * Location.t)
exception No_external_type_caml_def of (Parsetree.vname * Location.t)

val external_def_compile :
  Misc_ml_generation.reduced_compil_context ->
  (Parsetree.external_def_desc, 'a) Parsetree.generic_ast -> unit
