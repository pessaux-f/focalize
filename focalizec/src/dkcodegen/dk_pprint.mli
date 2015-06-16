(** Pretty_printing for types for the Dedukti translation. *)
type dk_print_context = {
  dpc_current_unit : Types.fname ;
  dpc_current_species : Types.type_collection option ;
  dpc_collections_carrier_mapping : Types.collection_carrier_mapping
}


val pp_type_simple_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> unit
val pp_type_variable_to_dk : Format.formatter -> Types.type_variable -> unit
val pp_type_simple_args_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> int -> unit
val has_cbv : Types.type_simple -> bool
val pp_for_cbv_type_simple_to_dk :
  dk_print_context -> Format.formatter -> Types.type_simple -> unit

val purge_type_simple_to_dk_variable_mapping : unit -> unit
