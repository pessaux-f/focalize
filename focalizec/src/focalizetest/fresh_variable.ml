
let var_assoc = ref [];;
let index = ref 0;;

let get_newindex () =
  index := !index + 1; !index;;


let add_var v n =
  var_assoc := (v,n)::!var_assoc;
  n;;

let new_seed () = string_of_int (get_newindex());;

let new_var_prefix p =
  p ^ new_seed ();;

(* ************************************************************************* *)
(*                                                                           *)
(*                           Convenient functions                            *)
(*                                                                           *)
(* ************************************************************************* *)

let get_from_existing s =
  try List.assoc s !var_assoc with
  | Not_found ->
      let name = "Aux_" ^ s ^ "__" ^ new_seed () in 
      add_var s name;;

let get_from_existing_prefix s p =
  p ^ (try List.assoc s !var_assoc with
       | Not_found ->
           let name = new_seed () in
             add_var s name);;

let new_capitalize () =
  new_var_prefix "Aux";;

let new_uncapitalize () =
  new_var_prefix "aux";;

(* ************************************************************************* *)
(*                                                                           *)
(*                           For prolog                                      *)
(*                                                                           *)
(* ************************************************************************* *)

let new_prolog_var () =
  "Aux" ^ new_seed ();;

let prolog_var _ = 
  get_from_existing_prefix "Aux";;


(* ************************************************************************* *)
(*                                                                           *)
(*                           The name of the added methods                   *)
(*                                                                           *)
(* ************************************************************************* *)

let random_meth s =
  get_from_existing_prefix s "random_meth_";;

let print_meth s =
  get_from_existing_prefix s "print_meth_";;

let import_meth s =
  get_from_existing_prefix s "import_meth_";;

let parse_meth s =
  get_from_existing_prefix s "parse_meth_";;

let print_xml_meth s =
  get_from_existing_prefix s "print_xml_meth_";;

let reinject_value_meth s =
  get_from_existing_prefix s "reinject_value_meth_";;

(* ************************************************************************* *)
(*                                                                           *)
(*                         The name for the prolog part                      *)
(*                                                                           *)
(* ************************************************************************* *)


(* Name of the prolog program file *)
(*
let prolog_file n =
  get_from_existing_prefix n "prolog_pgm";;
*)

(* Name of the predicate generating the convenient test data *)
let prolog_pgm_name n i =
  let n = n ^ (string_of_int i) in
  get_from_existing_prefix n "prolog_";;

(* Name of the predicate generating the convenient test data *)
let prolog_pgm_state_name n i =
  let n = n ^ (string_of_int i) in
  get_from_existing_prefix n "prolog_state";;

(* Name of the prolog result file *)
let prolog_pgm_res n i =
  prolog_pgm_name n i ^ "_res";;

(* Name of the Focal function that parse the results *)
let prolog_pgm_get_res _ =
  get_from_existing_prefix "Parse my result" "prolog_read" ^ "_get_res";;

(* Name of the Focal function that create the prolog file *)
(*
let prolog_pgm_create_file n i =
  let n = n ^ (string_of_int i) in
  get_from_existing_prefix n "prolog_create_file_";;
*)

(*
let of_external_meth () =
  get_from_existing_prefix "external" "of_external_";;
*)

