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

(* $Id: miscHelpers.mli,v 1.7 2012-02-08 16:35:29 pessaux Exp $ *)

val bind_parameters_to_types_from_type_scheme :
  self_manifest: Types.type_simple option ->
  gen_vars_in_scope:(Types.type_variable * Types.type_simple) list ->
  Types.type_scheme option ->
  Parsetree.vname list ->
  (((Parsetree.vname * Types.type_simple option) list) *
    (Types.type_simple option) *
    ((Types.type_variable * Types.type_simple) list))
