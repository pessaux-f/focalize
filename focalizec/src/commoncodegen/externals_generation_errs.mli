(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: externals_generation_errs.mli,v 1.2 2008-11-29 23:41:13 weis Exp $ *)

exception No_external_value_def of (string * Parsetree.vname * Location.t);;
exception No_external_type_def of (string * Parsetree.vname * Location.t);;
exception No_external_constructor_def of (string * Parsetree.constructor_ident);;
exception No_external_field_def of (string * Parsetree.label_ident);;
