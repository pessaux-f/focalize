
type minifoc_var =
  | FVInt of string (** An integer variable *)
  | FVHer of string * Own_types.typ;;  (** An Herbrand variable *)

type minifoc_arg =
  | FVar of minifoc_var (* A variable name *)
  | FInt of int    (** An integer constant *)
  | FConstruct of string * minifoc_arg list;; (** Appel à un constructeur *)

(* My expressions *)
type minifoc_expr =
  | FIfte of string * minifoc_expr * minifoc_expr
  | FMeth  of string * string * minifoc_arg list
            (** a function is applied to a list of string/integer name/value *)
  | FBasic of string * minifoc_arg list (** same as [FMeth] *)
  | FMatch of string * (string * minifoc_arg list * minifoc_expr) list
                               (** pattern matching is only on variable name *)
  | FVarloc of minifoc_var * minifoc_expr * minifoc_expr (** a [let] expression *)
  | FValue of minifoc_arg;; (** the value of a variable or an integer *)

type minifoc_function = string * string list * minifoc_expr;;

val dbg_string_minifoc_expr : minifoc_expr -> string;;

val minifoc_expr_of_myexpr : Own_expr.myexpr -> minifoc_expr;;
