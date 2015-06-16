(** Pretty_printing for types for the Coq translation. *)
type coq_print_context = {
  cpc_current_unit : Types.fname ;
  cpc_current_species : Types.type_collection option ;
  cpc_collections_carrier_mapping : Types.collection_carrier_mapping
}


val pp_type_simple_to_coq :
  coq_print_context -> Format.formatter -> Types.type_simple -> unit
val pp_type_variable_to_coq : Format.formatter -> Types.type_variable -> unit
val pp_type_simple_args_to_coq :
  coq_print_context -> Format.formatter -> Types.type_simple -> int -> unit

val purge_type_simple_to_coq_variable_mapping : unit -> unit
(* DEBUG
val debug_variable_mapping : unit -> unit *)
