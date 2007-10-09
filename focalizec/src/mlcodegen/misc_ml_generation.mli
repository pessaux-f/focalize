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

(* $Id: misc_ml_generation.mli,v 1.1 2007-10-09 08:37:35 pessaux Exp $ *)


val pp_to_ocaml_vname : Format.formatter -> Parsetree.vname -> unit
type reduced_compil_context = {
  rcc_current_unit : Types.fname;
  rcc_out_fmter : Format.formatter;
}
val bind_parameters_to_types_from_type_scheme :
  Types.type_scheme option -> Parsetree.vname list ->
    (Parsetree.vname * Types.type_simple option) list
