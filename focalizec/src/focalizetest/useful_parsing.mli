
val parse_foc_topexpr : string -> Own_expr.toplevel_def 
(** [parse_foc_topexpr s] parses a string [s] as a light focal toplevel expression *)

val parse_test_context : string -> Context_test.test_context;;

val parse_foc_expr : string -> Own_expr.myexpr
(** [parse_foc_expr s] parses a string [s] as a light focal expression. *)

val parse_type : string -> Own_types.typ
(** [parse_type s] parses a strings [s] as a focal type. *)

val parse_foc_meth : string -> Own_expr.methods
(** [parse_foc_meth s] parses a string [s] as a focal method definition. *)

