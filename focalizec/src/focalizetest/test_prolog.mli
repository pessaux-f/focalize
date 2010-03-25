val prolog_args_of_constructor_typ :
  Own_types.typ list -> Own_prolog.prolog_term list
val prolog_type_of_constructor :
  Own_types.constructor -> Own_prolog.prolog_term
val prolog_type_of_typedef : Own_types.typ_definition -> Own_prolog.prolog_term
val import_all_types : unit -> Own_prolog.prolog_clause
