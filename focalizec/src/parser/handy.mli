val pp_generic_separated_list :
  string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_generic_newlined_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_generic_explicit_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
val pp_generic_option :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter ->
    'a option -> unit
