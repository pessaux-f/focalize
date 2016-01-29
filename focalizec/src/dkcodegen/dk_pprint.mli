(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(** Pretty_printing for types for the Dedukti translation. *)
type dk_print_context = {
  dpc_current_unit : Types.fname ;
  dpc_current_species : Types.type_collection option ;
  dpc_collections_carrier_mapping : Types.collection_carrier_mapping
}

exception Can_only_print_type_arguments_of_sum_types of Types.type_simple_view

val pp_type_simple_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> unit
val pp_type_scheme_to_dk :
  dk_print_context -> Format.formatter -> Types.type_scheme -> unit
val pp_type_variable_to_dk : Format.formatter -> Types.type_variable -> unit
val pp_type_simple_args_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> unit
val has_cbv : Types.type_simple -> bool
val pp_for_cbv_type_simple_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> unit

val purge_type_simple_to_dk_variable_mapping : unit -> unit
