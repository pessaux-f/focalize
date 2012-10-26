(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: miscHelpers.mli,v 1.10 2012-10-26 14:55:19 pessaux Exp $ *)

val bind_parameters_to_types_from_type_scheme :
  self_manifest: Types.type_simple option ->
  Types.type_scheme option -> Parsetree.vname list ->
    (((Parsetree.vname * Types.type_simple option) list) *
       (Types.type_simple option) *
       (Types.type_variable list))
