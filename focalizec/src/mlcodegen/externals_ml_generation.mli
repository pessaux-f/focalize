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

(* $Id: externals_ml_generation.mli,v 1.3 2007-11-06 10:14:58 pessaux Exp $ *)


exception No_external_value_caml_def of (Parsetree.vname * Location.t)
exception No_external_type_caml_def of (Parsetree.vname * Location.t)
exception No_external_constructor_caml_def of Parsetree.constructor_ident
exception No_external_field_caml_def of Parsetree.label_ident
