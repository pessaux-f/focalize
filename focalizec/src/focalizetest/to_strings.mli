
(** {6 Convert some types to string}
This module defined some type conversion to string. It is defined in for
debugging purpose. *)

val string_of_prop : Own_prop.proposition -> string
(** converts a proposition to string. *)

val print_prop : Own_prop.proposition -> unit
(** Prints on stdout a proposition. *)

val string_of_call : Own_expr.myexpr * bool -> string
(** converts a methods call to string, the boolean specify if the call is
negated or not. *)

val string_of_precond : Own_prop.pre_cond -> string
(** converts a pre-condition to string. *)

val print_elem_precond : Own_prop.elementaire -> unit
(** Prints on stdout a conclusion from a elementary value. *)

val string_of_conclusion : Own_prop.conclusion -> string
(** converts a conclusion to string. *)

val print_elem_conclusion : Own_prop.elementaire -> unit
(** Prints on stdout a pre-condition from a elementary value. *)

val print_elementaire : Own_prop.elementaire -> unit
(** Prints on stdout a elementary form. *)

val print_elementaires : Own_prop.elementaire list -> unit
(** Prints on stdout a list of elementary form. *)


