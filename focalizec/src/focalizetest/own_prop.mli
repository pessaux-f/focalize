
(** {6 Type of pre-condition} *)

type proposition =
    PUniv of string * Own_types.typ * proposition (** Universal quantifier. *)
  | PEx of string * Own_types.typ * proposition   (** Existential quantifier. *)
  | PAnd of proposition * proposition             (** Conjunction. *)
  | POr of proposition * proposition              (** Disjunction. *)
  | PImp of proposition * proposition             (** Implication. *)
  | PEq of proposition * proposition              (** Equivalence. *)
  | PNot of proposition                           (** Negation. *)
  | PCall of Own_expr.myexpr                      (** call of [myexpr] value. *)
(** A value of type [proposition] describe a proposition. *)

type name_and_prop = string * proposition
(** [name_and_prop] is the name of a property with its definition. *)


val puniv : string -> proposition -> proposition
(** [puniv nv prop] returns the proposition [prop] with the variable named [nv]
quantified universally. *)

val pex : string -> proposition -> proposition
(** [pex nv prop] returns the proposition [prop] with the variable named [nv]
quantified existentially. *)

val pand : proposition -> proposition -> proposition
(** [pand prop1 prop2] returns the conjunction [prop1] and [prop2]. *)

val por : proposition -> proposition -> proposition
(** [por prop1 prop2] returns the disjunction [prop1] and [prop2]. *)

val pimp : proposition -> proposition -> proposition
(** [pimp prop1 prop2] returns the proposition [prop1] imply [prop2]. *)

val peq : proposition -> proposition -> proposition
(** [peq prop1 prop2] returns the proposition [prop1] equivalent [prop2]. *)

val pnot : proposition -> proposition
(** [pnot prop] returns the negation of [prop]. *)

val pcall : Own_expr.myexpr -> proposition
(** [pcall expr] returns the proposition consisting of the call of predicate
 * [expr]. *)

val get_forall_types : proposition -> Own_types.typ list
(**
  Takes a proposition in prenex form. Returns the type, without doublons of all
  universally quantified variables of the proposition. The behaviour of thes
  function is undefined if the proposition is not in prenex form and contains
  some existentials quantifiers.
*)

(** {6 Type of pre-condition} *)

type pre_cond = Own_expr.myexpr list
(** A pre-condition is conjunction of several expression. *)

val create_precond : Own_expr.myexpr -> pre_cond
(** [create_precond expr] returns the pre-condition with unique expression
[expr]. *)

val precond_null : pre_cond
(** The empty pre-condition value. *)

val list_of_precond : pre_cond -> Own_expr.myexpr list
(** [list_of_precond p] converts the precondition [p] to a list of expression *)

(** {6 Type of conclusion} *)


type conclusion = Own_expr.myexpr list
(** The type of conclusion for of the elementary forms *)

val conclusion_null : conclusion
(** The nil conclusion *)

val create_conclusion : Own_expr.myexpr -> conclusion
(** [create_conclusion e] creates the conclusion from the only expression [e]. *)

val add_conclusion : Own_expr.myexpr -> conclusion -> conclusion
(** [add_conclusion e c] adds the expression [e] to the conclusion [c]. *)

val list_of_conclusion : conclusion -> Own_expr.myexpr list
(** [list_of_conclusion c] converts the conclusion to a list of expression *)

(** {6 Type of simple variable} *)

type variable
(** The type of variables. A variable has a name and a type. *)

val create_variable : string -> Own_types.typ -> variable
(** [create_variable n t] create a new variable value with name [n] and type
[t]. *)

val get_variable_name : variable -> string 
(** [get_variable_name v] get the name of variable [v]. *)

val get_variable_type : variable -> Own_types.typ 
(** [get_variable_type v] returns the type of variable [v]. *)

val variable_ren : (string -> string) ->  variable -> variable
(** [variable_ren f v] renames the variable [v] according to [f]. *)

val foc_string_of_variable : variable -> string
(** [foc_string_of_variable v] converts a variable to a string of the form
["x in int"].
*)

(** {6 Type of collection variables} *)

type variables
(** Type of ordered collection variables. *)

val string_of_variables : variables -> string
(** [string_of_variables vs] converts [vs] to a [string] in the format
["(v1 v2 : t1) (v3 : t2) ..."]
The variable of same type are grouped in the same parenthesis.
*)

val variables_nb : variables -> int
(** [variables_nb vs] returns the number of variable in [vs]. *)

val variables_null : variables
(** [variables_null] is an empty collection variables. *)

val variables_is_null : variables -> bool 
(** [variables_is_null vs] return [true] if [vs] is empty, [false] otherwise. *)

val variables_mem : string -> variables -> bool
(** [variables_mem s vs] tests if variable named [s] is member of [vs]. *)

val variables_get_type : variables -> string  -> Own_types.typ
(** [variables_mem s vs] get the type of the variable [v] in [vs]. *)

val variables_map_esc : (variable -> 'a) -> variables -> 'a list
(** [variables_map f vs] take a function [f] transforming a variables value to a
 * value of another type and a variables value [vs]. It returns the list of
 * values created from variables in [vs] where the order of the value are the
 * order of all variables in [vs] apply to [f]. *)

val variables_map : (variable -> variable) -> variables -> variables
(** [variables_map f vs] returns [vs] where all variables have been apply to
[f]. *)  

val create_variables : variable -> variables
(** [create_variables v] create an collection variables which contain only [v]. *)

val add_variable : variable -> variables -> variables
(** [add_variables v vs] add a variable [v] in the collection variables [vs]. *)

val variables_to_list : variables -> (string * Own_types.typ) list
(** [variables_to_list vs] transforms [vs] in a list of associations variables
name to types. *)

val variables_to_tuple_type : variables -> Own_types.typ
(** [variables_to_tuple_type vs] transforms [vs] in a type in the form of a
n-tuple where n is the number of variable and the elements of the tuple is the
type of each variables. The tuple is represents as a composition of 2-tuple
oriented form right to left. A value returned by this function is for example
[(t_1,(t_2, ...(t_{n-1},t_n)...)]. 
The last element of the tyuple is the unit type. *)

val variables_to_tuple : variables -> Own_expr.myexpr
(** [variables_to_tuple_type vs] transforms [vs] in a value in the form of a
n-tuple where n is the number of variable and the elements of the tuple is the
name each variables. The tuple is represents as a composition of 2-tuple
oriented form right to left. A value returned by this function is for example
[(t_1,(t_2, ...(t_{n-1},t_n)...)].
The last element of the tyuple is unit. *)

val foc_argsdef_of_variables : variables -> string
(** [foc_argsdef_of_variables vs] converts a variables [vs] containing some
variables [v1], ..., [vn] of types [t1], ..., [tn] to the string
["(v1 in t1, v2 in t2, ..., vn in tn)"]. If variables is empty then it returns the
string ["(unused_var in @UNIT")]
*)

val foc_argscall_of_variables : variables -> string
(** [foc_argscall_of_variables vs] converts a variables [vs] containing some
variables [v1], ..., [vn] of types [t1], ..., [tn] to the string
["(v1, v2, ..., vn)"]. If variables is empty then it returns the
string ["(@VUNIT")]
*)

(** {6 Type of elementary form} *)

type elementaire
(** The type of elementary form. A elementary form contains a pred-condition and
a conclusion. *)

val create_elementaire : pre_cond -> conclusion -> elementaire
(** [create_elementaire p c] create a elementary form with pre-condtion [p] and
conclusion [s]. *)

val elementaire_null : elementaire
(** [elemntaire_null] is the zero precondition and zero conclusion property *)

val get_precond : elementaire -> pre_cond
(** [get_precond e] returns the pre-condition of [e]. *)

val get_conclusion : elementaire -> conclusion
(** [get_conclusion e] returns the conclusion of [e]. *)

val add_elem_precond : Own_expr.myexpr -> elementaire -> elementaire
(** [add_elem_precond expr e] add a precondition [expr] to [e]. *)

val add_elem_conclusion : Own_expr.myexpr -> elementaire -> elementaire
(** [add_elem_conclusion expr e] add a conclusion [expr] to [e]. *)

val map_elementaire_esc : (Own_expr.myexpr -> 'a) -> elementaire -> 'a list * 'a list
(** [map_elementaire f e] apply function [f] to all expression appearing in the
pre-conditions and conclusions of [e]. *)

val map_elementaire : (Own_expr.myexpr -> Own_expr.myexpr) -> elementaire -> elementaire
(** [map_elementaire f e] apply function [f] to all expression appearing in the
pre-conditions and conclusions of [e]. *)

val map_elementaire_ren : (string -> string) -> variables -> elementaire -> elementaire
(** [map_elementaire_ren f vs e] returns [e] where all variables [vs] are renamed
using function [f]. *)

val string_of_elem : variables -> elementaire -> string
(** [string_of_elem elem] convert an elementary form in string. *)

(** {6 Type of normal_form} *)

type normal_forms
(** The type of normals forms. *)

val create_normal_forms : string -> variables -> elementaire list -> normal_forms
(** [create_normal_forms vs elems] create a new normal form from a collection
of variables [vs] and a list of elementary property [elems]. *)

val get_norm_variables : normal_forms -> variables
(** [get_norm_variables nfs] returns the collections variables of used in the
normals forms [nfs]. *)

val get_norm_name : normal_forms -> string
(** [get_norm_name nfs] returns the name of the property behind the normal
forms. *)

val get_norm_elems : normal_forms -> elementaire list
(** [get_norm_elems nfs] returns the elementaries forms used in the
normals forms [nfs]. *)

val get_norm_nbvar : normal_forms -> int 
(** [get_norm_nbvar nfs] returns the number of variables present in the normal
forms [nfs]. *)

val get_norm_num_elems : normal_forms -> int
(** [get_norm_num_elems nfs] returns the number of elementaries forms present in the
normal forms [nfs]. *)

val map_norm_ren : (string -> string) -> normal_forms -> normal_forms
(** [map_norm_elementaire f nfs] apply the function [f] on all variables appearing
in the normal form [nfs]. The function [f] can be side-effect and returns
differents value on several call with the same arguments. *)

val map_norm_ren_assoc : (string * string) list -> normal_forms -> normal_forms
(** List [map_norm_elementaire], but with a list of association instead. *)

val map_norm : (variables -> elementaire -> 'a) -> normal_forms -> 'a list
(** [map_norm f nf] apply [f] to all elementaries form of [nf] and returns the
list corresponding. *)

