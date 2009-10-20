
val get_from_existing : string -> string 
val new_prolog_var : unit -> string
val new_capitalize : unit -> string
val new_uncapitalize : unit -> string
val get_from_existing_prefix : string -> string -> string
val new_var_prefix : string -> string


(* for methods name *)

val random_meth : string -> string;;
(* val of_external_meth : unit -> string;; *)
val print_meth : string -> string;;
val parse_meth : string -> string;;
val import_meth : string -> string;;
val print_xml_meth : string -> string;;
val reinject_value_meth : string -> string;;

(* val prolog_file : string -> string;; *)
val prolog_pgm_name : string -> int -> string;;
val prolog_pgm_state_name : string -> int -> string;;
(* val prolog_pgm_create_file : string -> int -> string;; *)
val prolog_pgm_res : string -> int -> string;;
val prolog_pgm_get_res : string -> string;;
