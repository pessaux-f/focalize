
type prolog_term =
  | Prolog_comment of string (** prolog comment *)
  | Prolog_fun of string * prolog_term list (** functor call *)
  | Prolog_var of string (** A variable (the first character should be upcased) *)
  | Prolog_conjunction of prolog_term list (** A list of prolog term separated by commas *)
  | Prolog_list of prolog_term list (** a prolog list *)
  | Prolog_int of int (** An integer *)

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

val get_singleton : prolog_clause -> string list
