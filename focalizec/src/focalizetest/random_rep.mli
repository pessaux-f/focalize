
(** {6 generating the methods for getting a random value} *)

exception Coinductive_type of string;;
(** Exception raised when a coinductive type is meet. *)

val ast_random :
 string list ->
   Own_types.typ list ->
     Own_types.typ ->
       Own_expr.methods list
(**
[ast_random l_p l_t rep] takes a list of collection's name [l_p], possibly
referenced in the types taken as others parameters, a list of types [l_t] and a
rep type [rep].
  
  It returns a set of method designing for generating randomly an instance for
  all types [t_l] and [rep], the list of function imported from caml and the
  list of method's name associating on each types. The method for generating
  [rep] is named {!Own_basics.meth_rnd}.
*)

(*
val ast_random : string list -> Own_types.typ -> (string * string) list * Own_expr.methods list
(** [ast_random prm t] takes a set of methods defining a instance of type [t]
applied to parameters [prm].
It returns the imports definitions and the list of methods. *)
 *)

(**/**)

(** {6 Basic type of focal} *)

val predefined_random_meth : (string * Own_expr.a_method) list
(** The list of all predefined random methods. For [int], [float], [unit], [big_int] *) 

(*
val default_random_name : (Own_types.typ * string) list
(** List of association. Associate a type with the predefined random methods name. *)
*)

(*
val predefined_random_fml : (string * (string * string)) list
(* The caml import association list for predefined random function type. *)
*)

val predefined_print_meth : (string * Own_expr.a_method) list
(** The list of all predefined print methods. For [int], [float], [unit], [big_int] *) 

(*
val default_print_name : (Own_types.typ * string) list
(** List of association. Associate a type with the predefined print methods name. *)
*)

(*
val predefined_print_fml : (string * (string * string)) list
(* The caml import association list for predefined print function type. *)
*)



(*
val recolte_fun : Own_expr.myexpr -> Own_types.typ list
(** Takes an expression and returns the list of types the expression expected as
argument.
*)

Example: [recolte_fun (fun x:int -> fun x:float)] returns [\[int; float\]]. *)

(** {6 Convenient ast generator} *)

val ast_random_cons :
  Own_types.constructor list -> string -> Own_expr.myexpr
(** [ast_random_cons cons_l t_name] returns the ast that choice equiprobably a
constructor in [cons_l] and construct a value over the constructor choiced.
[t_name] is the type name. *)

(** {6 Methods generator} *)

val ast_random_type :
  string list ->
  Own_types.typ ->
    Own_expr.a_method list
(** [ast_random_type l_param t] returns the list of method necessary to generate
a random value of type [t] and the list needed imported function and the name of
the method in the list that generate [t]. [l_param] is a list of collection [t]
can depends on.*) 

                                                                     
val ast_random_types :
  string list ->
  Own_types.typ list ->
  Own_expr.a_method list
(** Like [ast_random_types] but for a list of type *)

val meth_random_rep : Own_types.typ -> Own_expr.a_method
(** [meth_random_rep t] returns the method named {!Own_basics.meth_rnd}
generating a value of type [t]. It only calls a function generating the value.
The function should exists elsewhere. *)

