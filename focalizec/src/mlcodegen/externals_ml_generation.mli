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

(* $Id: externals_ml_generation.mli,v 1.2 2007-10-29 15:48:12 pessaux Exp $ *)


exception No_external_value_caml_def of (Parsetree.vname * Location.t)
exception No_external_type_caml_def of (Parsetree.vname * Location.t)
