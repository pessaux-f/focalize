type prolog_term =
  | Prolog_comment of string
  | Prolog_fun of string * prolog_term list
  | Prolog_var of string
  | Prolog_conjunction of prolog_term list
  | Prolog_list of prolog_term list
  | Prolog_int of int
val prolog_is_comment : prolog_term -> bool
type prolog_clause = prolog_term option * prolog_term list
type prolog_pgm = prolog_clause list
val prolog_list : prolog_term list -> prolog_term
val prolog_int : int -> prolog_term
val prolog_fun : string -> prolog_term list -> prolog_term
val prolog_fun_bin : string -> prolog_term -> prolog_term -> prolog_term
val prolog_var : string -> prolog_term
val prolog_fd_equal : prolog_term -> prolog_term -> prolog_term
val prolog_equal : prolog_term -> prolog_term -> prolog_term
val prolog_clause : prolog_term option -> prolog_term list -> prolog_clause
