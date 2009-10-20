
val top_xml_def : unit -> Own_expr.toplevel_def list 
(* val top_xml_spec_name : string *)
val top_xml_coll_name : string

(** [ast_seq expr1 expr2] returns the ast call [expr1] and [expr2] in sequence
  *)

val xml_string_of_typ : Own_types.typ -> string;;
(** [xml_string_of_typ t] converts a type to a string in xml format. *)

val xml_string_of_expr : Own_expr.myexpr -> string;;
(** [xml_string_of_expr e] converts an expression to a string in xml format. *)

val xml_string_of_parameters_instance : Own_expr.parameters_instance -> string;;
(** [xml_string_of_parameters_instance p] converts a parameters instance [p] to
a string in xml format, for the collection parameters it consists in the name
of the collection, for a entity parameters it converts to a xml tree value. *)

(*
(** {6 xml printing} *)

(** This module handle the generation of the xml file. All functions defined here
returns an abstract syntax tree that print on the file defined by
{!Whattodo.get_file_output_xml}. *)


val ast_print_file : Own_expr.myexpr -> Own_expr.myexpr
(** [ast_print_file expr] returns the ast printing on xml file the expression
[expr]. [expr] should be of type [string]. *)

val ast_print_file_string : string -> Own_expr.myexpr
(** Like [ast_print_file] with [string]. *)

val ast_print_newline : unit -> Own_expr.myexpr
(** The ast which prints a new line on stdout. *)

val ast_print_file_newline : unit -> Own_expr.myexpr
(** Prints a new line on the xml file. *)

val xml_print_header : Own_expr.myexpr
(** The ast printing on the xml file the xml header. *)

(** {6 Markup printing} *)                                       

val xml_print_property : string -> string -> Own_expr.myexpr
(** [xml_print_property n p] prints to xml the open mark {e property} for property
name [n] with body [p]. *)

val xml_close_property : Own_expr.myexpr
(** The ast printing on the xml file the close markup {e property}. *)

val xml_print_elementaire : string -> Own_expr.myexpr
(** [xml_print_elementaire form] prints to xml file the open markup {e
elemntaire} of the elementary property with the form [form] (ie: forall x y,
... ). *)

val xml_close_elementaire : Own_expr.myexpr
(** The ast printing to xml file the close markup {e elementaire}. *)

val xml_open_report : string -> Own_expr.myexpr
(** The ast printing to xml file the open markup {e rapport}. *)

val xml_close_report : unit -> Own_expr.myexpr
(** The ast printing to xml file the close markup {e rapport}. *)

val xml_open_test : Own_expr.myexpr
(** The ast printing to xml file the open markup {e test}. *)

val xml_close_test : Own_expr.myexpr
(** The ast printing to xml file the close markup {e test}. *)

val xml_print_variables : string list -> Own_expr.myexpr
(** [xml_print_variables v_l] prints to xml file, the complete markup
{e variables} with variables names [v_l]. *)

val xml_print_value : Own_expr.myexpr -> Own_expr.myexpr
(** [xml_print_value v] returns the ast printing to xml file the complete
markup {e value} which print the value of variable [v]. *)

val xml_result_test : Own_expr.myexpr -> Own_expr.myexpr
(** [xml_result_test cond] prints on xml file the complete markup {e resultat}. *) 
*)

val safe_replace : string -> string;;
