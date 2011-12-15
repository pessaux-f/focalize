
(** {6 Create the test generator} *)

val ast_testprop :
     (string * Own_prop.variables * (Own_prolog.prolog_clause *
     Own_prop.elementaire) list) -> string -> Own_expr.species_name * Own_expr.parameters
     list * Own_expr.species_name list ->
     string * Own_expr.toplevel_def list
(** [ast_testprop nfs stat (spec_n,prm_l,coll_l) typ_meth] takes some elementary
forms [nfs], the statement of the original property [stat], a species name
[spec_n], a list of parameters [prm_l] the species [spec_n] expect, a list of
collection [coll_l] corresponding to the instanciation of the parameters [prm_l]
and a list of association [typ_meth] associating a type with the name of the
 method generating it.

It returns, the name of the collection implementing a test generator for testing
the property [np] and a list of toplevel definition implementing this
generator. *)

(*
val ast_testprop_reinject :
  Own_xml.report_property -> string * Own_expr.parameters list * string list ->
    string * Own_expr.toplevel_def list
(** [ast_testprop_reinject nfs stat (spec_n,prm_l,coll_l) typ_meth] same as
ast_testprop but with a test report. *)
*)

val ast_testprop_list :
     (Own_prop.normal_forms * string) list -> 
       Context_test.test_context -> 
         Own_expr.species_name *
         Own_expr.parameters list *
         Own_expr.species_name list -> 
           string list * Own_expr.toplevel_def list
(** Same as [ast_test_prop] with a list of normal forms *)


val ast_testprop_reinject_list :
  Own_xml.report_property list ->
    Own_expr.species_name *
    Own_expr.parameters list *
    Own_expr.species_name list ->
      string list * Own_expr.toplevel_def list
(** Same as [ast_testprop_reinject_list] but with a list of report *)



(**/**)

(** {6 Functions' name} *)

val pre_cond_string : int -> string
(** Takes a pre-condition number and returns the name of the method testing the
validity of this pre-condition. *)

val conclu_string : int -> string
(** Takes a conclusion number and returns the name of the method testing the
validity of the conclusion. *)

val rapport_test : int -> string
(** Takes a elementary property number and returns the name of the method
creating the test report of it. *)

val test_elem : int -> string
(** Takes a elementary property number and returns the name of the method
testing the validity of this elementary property. *)

(** {6 Convenient abstract syntax tree generator} *)

val conjonction_call : Own_expr.myexpr list -> Own_expr.myexpr
(** [conjonction_call [e1; ...; en]] returns the focal expression calculating
the conjunction of all [ei]. *)

val disjonction_call : Own_expr.myexpr list -> Own_expr.myexpr
(** [disjonction_call [e1; ...; en]] returns the focal expression calculating
the disjunction of all [ei]. *)
(*
val ast_value_var : (string * Own_types.typ) list ->
                    Own_expr.gen_print_typ list ->
                    Own_expr.myexpr -> Own_expr.myexpr
(** [ast_value_var var_l expr] returns the ast printing on xml file the value of
variable in [var_l] with [expr] in sequence. *) 
*)
val ast_random_var : (string * Own_types.typ) list ->
                       string
(** Like [ast_value_var], but bound variables with random value instead of
printing and takes in plus a association list associating a type with the name
of the method generating a value. *) 

val ast_call_test_elems : int -> Own_expr.myexpr list -> Own_expr.myexpr
(** [ast_call_test_elems i args] returns the ast calculating the conjunction of
all [rapport_test x] applied to [args] with x in \[0;[i]\]. *)

(** {6 Methods} *)

val ast_test_pre_cond :
  Own_prop.elementaire -> Own_prop.variables -> int -> Own_expr.methods
(** [ast_test_pre_cond elem var_l i] takes a elementary property [elem], a list
of variable name associated with their types [var_l] and a number [i]. It
returns the method with name [pre_cond_string i] which takes as arguments
all variable in [l_var]. It calculate the value of [elem]'s pre-condition. *)

val ast_test_conclu :
  Own_prop.elementaire -> Own_prop.variables -> int -> Own_expr.methods
(** Like [ast_test_pre_cond], but with conclusion. *) 

val ast_test_elem_i :
  Own_prop.elementaire -> Own_prop.variables -> int ->
    Own_expr.methods
(** [ast_test_elem_i elem var_l i typ_meth] returns the method testing the
elementary property [elem] with variable [var_l], [i] is the number of [elem].
[typ_meth] associate a type with a method name.
*)

val ast_rapport_test_elem :
  Own_expr.species_name ->
  Own_prop.elementaire -> Own_prop.variables -> string -> int -> Own_expr.methods
(** [ast_rapport_test_elem e vs i] returns the method [rapport_test i] creating
the test report for the elementary property [e]. *)

(** {6 Creation of the collection} *)                                                     

val ast_collection_test : 
  string -> 
    string -> 
      Own_expr.species_name list ->
        Own_expr.collection
(** [ast_collection_test coll spec coll_l] returns the collection named [coll]
which implements [spec(coll_l)]. *)

