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

(* $Id: miscHelpers.mli,v 1.4 2008-11-17 10:53:57 pessaux Exp $ *)

val bind_parameters_to_types_from_type_scheme :
  self_manifest: Types.type_simple option -> Types.type_scheme option ->
    Parsetree.vname list ->
      (((Parsetree.vname * Types.type_simple option) list) *
        (Types.type_simple option) *
        (Types.type_simple list))
