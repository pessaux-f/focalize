
(** {6 Types type} *)

type typ =
(*   | TVar of string  (** Polymorphic types *) *)
(*   | TSelf   (** rep's type *)  *)
  | TAtom of string option * string           (** [int],[float] ... *)
  | TSpecPrm of string (** represents collection parameters *)
  | TFct of typ * typ         (** function type *)
  | TProd of typ * typ        (** product type *)
  | TPrm of string option * string * typ list (** parameterized type *)
(** Type [typ]. *)

type constructor = Own_basics.ident * typ list
(** Type of a constructor. *)

type typ_body =
  | Type of constructor list
  | TParam of string * typ_body;;
(** This is the definition of a the body of a type. It is a list of constructor
or a parameter followed by a body *)

val separate_args : typ_body -> string list * constructor list;;

type typ_definition = string * typ_body;;

(** Its name and the body of the definition *)

val dbg_string_constructor : constructor -> string

val dbg_string_typ_body : typ_body -> string

val dbg_string_typ_definition : typ_definition -> string

val string_of_typ : typ -> string
(** Convert a [typ] to [string]. *) 

val string_of_ttyp : typ -> string
(** Convert a [typ] to [string]. This function is injective *) 

val depends : typ -> typ list
(** [depends t] returns the list of types used in [t].

Example : depends (int * float) gives [\[int; float\]]. *)

val is_in : typ -> typ -> bool
(** [is_in t] returns true if [t] depends to itself. *)

val flatten_prod : typ -> typ list
(** [flatten_prod t] returns the list of typ used in the product type

flatten_prod (int * int * float) gives [\[int; int; float\]]. *)

val flatten_prod_right : typ -> typ list
(** [flatten_prod t] returns the list of typ used in tuple expansed if the right

flatten_prod (int * int * float) gives [\[int; int; float\]]. *)

