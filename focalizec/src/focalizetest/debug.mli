(** Module used to print focal definition *)

(**/**)

val string_of_foctyp : Parsetree.type_expr -> string
(** Transforms a focal typ to string.  *)

val print_foctyp : Parsetree.type_expr -> unit
(** Prints on stdout a focal typ *)

val string_of_expr : Parsetree.expr -> string
(** Transforms a focal expression to string.  *)

(*val string_of_texp_desc : Parsetree.expr -> string
(** Transforms a focal expression to string.  *)
*)

val print_expr : Parsetree.expr -> unit
(** Prints on stdout a focal expression *)

val string_of_prop : Parsetree.logical_expr_desc -> string
(** Transforms a focal proposition to string.  *)

val print_prop : Parsetree.logical_expr_desc -> unit
(** Prints on stdout a focal proposition. *)

(* val string_of_tdef : Typed_elt.tdef -> string *)
