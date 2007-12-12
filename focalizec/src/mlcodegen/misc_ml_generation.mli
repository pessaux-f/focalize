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

(* $Id: misc_ml_generation.mli,v 1.7 2007-12-12 16:45:15 pessaux Exp $ *)



val pp_to_ocaml_label_ident : Format.formatter -> Parsetree.label_ident -> unit

type reduced_compil_context = {
  rcc_current_unit : Types.fname ;
  rcc_species_parameters_names : Parsetree.vname list ;
  rcc_collections_carrier_mapping : (Types.type_collection * string) list ;
  rcc_lambda_lift_params_mapping : (Parsetree.vname * (string list)) list ;
  rcc_out_fmter : Format.formatter
}
val bind_parameters_to_types_from_type_scheme :
  Types.type_scheme option -> Parsetree.vname list ->
    (((Parsetree.vname * Types.type_simple option) list) *
      (Types.type_simple option))
