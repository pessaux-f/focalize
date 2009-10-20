
type species_context;;

val sc_get_name : species_context -> Own_expr.species_name;;
val sc_get_parameters : species_context -> string list;;

val create_sc :
  Own_expr.species_name -> string list -> species_context;;

type binding_context_aux

val bca_c : species_context -> binding_context_aux;;

val bca_e : Own_expr.myexpr -> binding_context_aux;;

type binding_context;;
val create_bc :
  string -> binding_context_aux -> binding_context;;

val bc_get_name : binding_context -> string;;

val bc_get_parameters : binding_context -> string list;;

val bc_get_name2 : binding_context -> Own_expr.species_name;;

(* val bc_get_species : binding_context -> species_context;; *)

type test_context;;

val string_of_tc : test_context -> string;;

val create_tc : binding_context list -> species_context -> test_context;;

val tc_add_bc : test_context -> binding_context ->
  test_context;;

val tc_get_end_sc : test_context -> species_context;;

(* val tc_get_assoc_spec : test_context -> string -> string;; *)

val tc_get_parameters : test_context -> binding_context list;;

type context_path;;

val string_of_cp : context_path -> string;;

val letter_string_of_cp : context_path -> string;;

val empty_path : context_path;;

val species_of_cp : test_context -> context_path -> Own_expr.species_name;;

val cp_add : 
  context_path -> string -> context_path;;

val wf_tc : test_context -> bool


type parameters =
  | For_harness of string * int list
  | From_user of string * int list

val string_of_parameters : parameters -> string;;
val parameters_get_name : parameters -> string;;

type species_context_harness;;
(*   string * parameters list *)

val sch_get_species : species_context_harness -> Own_expr.species_name;;
val sch_get_parameters : species_context_harness -> parameters list;;

type binding_context_harness_aux =
  BCH_Coll of species_context_harness
| BCH_Ent of Own_expr.myexpr;;


type binding_context_harness =
  string * binding_context_harness_aux;;

val bch_get_name : binding_context_harness -> string;;
val bch_get_sch :
  binding_context_harness -> binding_context_harness_aux;;

type test_context_harness =
  binding_context_harness list * species_context_harness;;

val tch_get_binding : test_context_harness -> 
  binding_context_harness list 
val tch_get_sc : test_context_harness ->
  species_context_harness;;

val print_tch : test_context_harness -> unit;;

val tch_of_tc : test_context -> test_context_harness 

