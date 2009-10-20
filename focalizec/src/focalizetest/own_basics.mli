(** {6 Basics definition} *)

(** {6 Focal's definitions  } *)

(** {7 Compiler/langage} *)

type ident =
  | Infix of string
  | Prefix of string option * string
;;

val ident_name : ident -> string;;

val prefix : string -> ident;;

val prefix_m : string -> string -> ident;;

val focself : string
(** The identifier used inside focal's compiler for representing [self]. *)

(* val focselfkey : string *)
(* (** The [self]'s type name (ie: ["self"]). *) *)

val focexception : string
(** Name of the exception possibly raised by focal methods *)

(** {7 Definitions in basics.foc} *)

val focbasics : string
(** The basics modules. *)

val focequal : ident
(** The focal structural equality function's name. *)

val focintequal : ident
(** The focal int equal function's name. *)

val focintgt : ident
(** The focal int greater than function's name. *)

val focintleq : ident
(** The focal int greater than function's name. *)
val foccrp : ident
(** The focal couple constructor. *)

val focfst : ident
(** The focal function fst. *)

val focsnd : ident
(** The focal function snd. *)

val focunit : ident
(** The constructor of the focal [unit] type. *)

val foctrue : ident
(** The focal [true] statement constructor. *)

val focfalse : ident
(** The focal [false] statement constructor. *)

val focnot : ident
(** The focal [not] function. *)

val focor : ident
(** The focal [or] function. *)

val focand : ident
(** The focal [and] function. *)

val focaddint : ident
(** The addition upon integers *)

val focpred : ident
(** The function subtracting one *)

val focsucc : ident
(** The successor function over integer *)

val focstringconcat : ident
(** The function concatenate two strings *)


(** {7 Focal types} *)

val foctint : string
(** The integer's type name *)

val foctbool : string
(** The boolean's type name *)

val foctself : string
(** The self's type name *)

val foctunit : string
(** The unit's type name *)

val foctstring : string
(** The string's type name *)

val foctchar : string
(** The character's type name *)

val foctfloat : string
(** The float numbers' type name *)

val foctoption : string
(** The focal [option] type *)

val foctlist : string
(** The focal [list] type *)

(** {7 Type manipulation } *)

val focnil : ident
(** The empty list constructor name *)

val foccons : ident
(** The cons list constructor name *)

val focunfailed : ident
(** The focal [Unfailed] constructor name *)

val focfailed : ident
(** The focal [Failed] constructor name *)

val focerror : ident
(** The focal [foc_error] function *)

val focnonfailed : ident
(** The focal [non_failed] function name *)

val focisfailed : ident
(** The focal [is_failed] function name *)

(** {7 Print out functions } *)

val focparseint : ident
(** The focal [parset_int] function name *)

val focprintint : ident
(** The focal [print_int] function name *)

val focprintstring : ident
(** The focal [print_string] function name *)

(** {6 Identifiers getters} *)

(** {7 Convenient name} *)

val param_name : int -> string
(** [param_name i] return the name of the [i]th parameters. *)

val coll_name : string -> string
(** [coll_name spec] returns the name of the collection corresponding to
[spec]. *)

val result_type : string
(** name of the result type in the .foc generated. *)

val result_ok : string * string list;;
(** The Ok constructor for type result. *)

val result_ko : string * string list;;
(** To Ko constructor for type result. :*)

val result_raise : string * string list;;
(** the raise constructor for type result. *)

val verdict_type : string
(** name of the verdict type in the .foc generated. *)

val verdict_precond_ok : string * string list

val verdict_precond_ko : string * string list

(** {7 New identifier generators} *)

(** The functions in the section are not determinists. Each call of these
functions returns a new string. *)

val spec_test_name : string * string -> string
(** [spec_test_name spec] returns a new identifier which is the name of the
species testing [spec]. *)

val spec_instru_name : string -> string list -> string
(** [spec_instru_name spec param] returns a new identifier corresponding to the
name of the species which instruments [spec] applied to [param]. *)

(** {6 Some convenient functions} *)

val string_of_list : ('a -> string) -> 'a list -> string
(** [string_of_list f l] converts the list [l] to string *)

val concat_no_doublon : 'a list -> 'a list -> 'a list
(** [concat_no_doublon l1 l2] returns a list constaining the elements of [l1]
and [l2] without doublon *) 

val (++) : 'a list -> 'a list -> 'a list
(** Same as [concat_no_doublon] but in infix way. *)


val flatten_no_doublon : 'a list list -> 'a list
(** [concat_no_doublon l1] flatten a list and delete all doublons *)

val add_string_args : 'a list -> string -> ('a -> string) -> string
(** [add_string_args [e1, ..., en] inter f] returns
[f e1 ^ s ^ f e2 ... ^ s ^ f en] *)

val to_args : ('a -> string) -> 'a list -> string
(** [to_args f [e1, ..., en]] returns
["(" ^ f e1 ^ "," ^ f e2 ... ^ "," ^ f en ^ ")"] *)

val int_list : int -> int -> int list;;
(** [int_list min max] returns the list
[\[min; min +1; ...; max - 1; max \]] *)


val merge_assoc_list : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
(** Takes to assoc list. Returns a list merging the two list. Remove all doublon
key in associations. *)

val ( @-@ ) : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
(** Infix definition of [merge_assoc_list]. *)

val list_create : (unit -> 'a) -> int -> 'a list
(** Create a list of length [n] *)

val string_assoc : string -> (string * string) list -> string

val string_of_bool : bool -> string

val ident_letter_name : ident -> string
(** get the ident symbol where all specal character (':', '(', etc) are replace
by sequence of letter/underscore.
*)

val string_of_option : ('a -> string) -> 'a option -> string
