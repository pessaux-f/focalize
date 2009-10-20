(*
val and_of_list : Own_prolog.prolog_term list -> Own_prolog.prolog_term
val prolog_list_of_list : Own_prolog.prolog_term list -> Own_prolog.prolog_term
val add_elem : 'a -> 'a list -> 'a list
val merge_list : 'a list -> 'a list -> 'a list
val get_integers : Expr_prolog.minifoc_expr -> string list
val env_variable : string
val prolog_of_focarg : Expr_prolog.minifoc_arg -> Own_prolog.prolog_term
val prologs_of_focargs : Expr_prolog.minifoc_arg list -> Own_prolog.prolog_term list
val basics_noresult :
  (string * (Own_prolog.prolog_term list -> Own_prolog.prolog_term)) list
val basics_result :
  (string * (Own_prolog.prolog_term list -> string -> Own_prolog.prolog_term)) list
val get_dictionnary_entry :
  string -> Own_prolog.prolog_term list -> string option -> Own_prolog.prolog_term
val prolog_term_of_pattern :
  string ->
  string * Expr_prolog.minifoc_arg list * Expr_prolog.minifoc_expr ->
  string -> Own_prolog.prolog_term
val prolog_term_of_focexpr :
  Expr_prolog.minifoc_expr -> string -> Own_prolog.prolog_term list
val prolog_pgm_of_minifoc_function :
  Expr_prolog.minifoc_function -> Own_prolog.prolog_pgm
val create_goal_from_species_prop : string -> string -> Own_prolog.prolog_pgm list
*)

val create_prolog_file : Context_test.test_context -> string -> Own_prop.variables ->
  Own_prop.elementaire list -> Own_prolog.prolog_pgm * (Own_prolog.prolog_clause *
  Own_prop.elementaire) list
