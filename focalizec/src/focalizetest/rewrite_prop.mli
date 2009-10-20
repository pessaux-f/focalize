
(** {6 Transform a property to a set of elementary property} *)

exception Not_good_form of string
(** Raised when attempt to normalize a proposition in bad form *) 

val normalise : Own_prop.name_and_prop -> Own_prop.normal_forms
(** [normalise np] returns the normals forms of the proposition [np].

@raise Not_good_form if the propropsition [np] is not in canonical form. *)

(**/**)

exception No_canon of Own_prop.proposition
(** Raised when attempting to normalize a bad formed property that been check
good formed by [is_canonique]. It should never be raised in the normal use of
the program. *)

exception Conclu of string
(** Raised when a conclusion is not in a disjunction of methods call.
It should never be raised in the normel use of the program. *)

val prefixe_pre_cond :
  Own_expr.myexpr -> Own_prop.elementaire list -> Own_prop.elementaire list
(** [prefixe_pre_cond expr elem_l] adds [expr] to all precondition of
elementary list [elem_l] *)

val prefixe_forall :
  string -> Own_prop.proposition list -> Own_prop.proposition list
(** [prefixe_forall v prop_l] adds [v] as universal quantified variable of all
element of the list [prop_l] *)

val aplati_conclu : Own_prop.proposition -> Own_prop.conclusion
(** Takes a disjunction of methods call and returns the list of call.

@raise Conclu if the parameters is not a disjunction of methods call. *)

val coupe : Own_prop.pre_cond -> Own_prop.proposition ->
               Own_prop.elementaire list
(** Takes a pre-condition and a proposition in conjunctive normal form and
returns the elementaries list corresponding to all differents possible
conclusion with the pre-condition.

@raise Conclu if the parameters is not a disjunction of methods call. *)

val normalise_canon : string -> Own_prop.proposition -> Own_prop.normal_forms
(** [normalise_canon name prop] returns the normal forms of the proposition
[prop] of name [name].

@raise No_canon if the proposition is not in canonical form. *)

val is_conjonctive_form : Own_prop.proposition -> bool
(** Checks if a proposition is in conjunctive form. *)

val is_canonique : Own_prop.proposition -> bool
(** Checks is a proposition is in canonical form. *)

