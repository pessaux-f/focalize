val pretty_print_list :
  Format.formatter ->
  'a list ->
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b) -> unit
val pretty_print_list_comma :
  Format.formatter -> 'a list -> (Format.formatter -> 'a -> unit) -> unit
val pretty_print_list_double_newline :
  Format.formatter -> 'a list -> (Format.formatter -> 'a -> unit) -> unit
val priority_list : (string * int) list
val separator_prior : int
val is_infix : string -> bool
val print_prolog_term : int -> string list -> Format.formatter -> Own_prolog.prolog_term -> unit
val print_prolog_terms_comma : string  list ->
  Format.formatter -> Own_prolog.prolog_term list -> unit
val print_prolog_clause : Format.formatter -> Own_prolog.prolog_clause -> unit
val print_prolog_clause_list :
  Format.formatter -> Own_prolog.prolog_clause list -> unit
val print_prolog_pgm : Format.formatter -> Own_prolog.prolog_pgm -> unit
