
(** Pretty_printing for types for the OCaml translation. *)
val pp_type_simple_to_ml :
  current_unit: Types.fname -> Types.collection_carrier_mapping -> Format.formatter ->
  Types.type_simple ->
    unit

val purge_type_simple_to_ml_variable_mapping : unit -> unit
