(** Types of various identifiers in the abstract syntax tree. *)
type cname = string
     (** Collection name. *) ;;
type sname = string
     (** Species name. *) ;;
type tname = string
     (** Type name. *) ;;
type label_name = string
     (** Label name. *) ;;


type simple_type
type species_type
type types_scheme

exception Conflict of simple_type * simple_type
exception Circularity of simple_type * simple_type
exception Arity_mismatch of (string * int * int)

val begin_definition : unit -> unit
val end_definition : unit -> unit

val type_variable : unit -> simple_type
val type_basic : tname -> simple_type list -> simple_type
val type_int : unit -> simple_type
val type_float : unit -> simple_type
val type_bool : unit -> simple_type
val type_string : unit -> simple_type
val type_char : unit -> simple_type
val type_unit : unit -> simple_type
val type_arrow : simple_type -> simple_type -> simple_type
val type_tuple : simple_type list -> simple_type
val type_self : unit -> simple_type
val type_prop : unit -> simple_type

val specialize : types_scheme -> simple_type
val generalize : simple_type -> types_scheme
val trivial_scheme : simple_type -> types_scheme

val unify : simple_type -> simple_type -> unit
