(* Fichier qui définit ce qu'il faut faire *)
(* Les valeurs sont mise à jour par Arg.parse dans run.ml *)

type typefun = Toplevel of string | Species of string | Nothing;;

exception Species_not_defined;;
exception Property_not_defined;;

(* let species_test : Own_expr.species_test option ref = ref None;; *)
let property_test : string list ref = ref [];;
let file_output = ref "a.out";;
let size_value_test = ref 10;;
let number_of_test = ref 10;;
let use_seq_function = ref false;;
let use_report : string option ref = ref None;;

let externfun = ref Nothing;; (*
                               fonction de génération automatique (fonction
                               importe de caml ou fonction fournit par l'espece
                               sous test) *)
(* accesseurs *)

let set_size_value_test s = size_value_test := s;;

let get_size_value_test () = !size_value_test;;

(*
let set_species_test s = species_test := Some s;;
let get_species_test () = 
  match !species_test with
  | None -> raise Species_not_defined
  | Some s -> s;;
*)

(*
let get_species_string () = 
  match !species_test with
  | None -> raise Species_not_defined
  | Some (s,prm_l) ->
      if prm_l = [] then
        s
      else (* TODO : Ajouter les truc is *)
        s ^ Own_basics.to_args Own_expr.string_of_parameters_instance
                               prm_l;;
*)

(* ********************************* *)

let set_property_test lp = property_test := lp;;
let get_property_test () = 
  match !property_test with
  | [] -> raise Property_not_defined
  | _ -> !property_test;;

(* ********************************* *)

let externfun opt =
  match opt with
    | None -> !externfun
    | Some s -> externfun := s;!externfun;;

(* ********************************* *)

let get_file_output_foc () = (!file_output) ^ ".fcl" ;;

let get_file_output_fml () = (!file_output) ^ ".fml" ;;

let get_file_output_xml () = (!file_output) ^ ".xml" ;;

let get_file_output_prolog n = (!file_output) ^ ".prolog_" ^ n ;;

let get_output_module () = !file_output ;;

let set_file_output s =
  let l = String.length s in
  if l > 4 && Filename.check_suffix s ".fcl" then
    file_output := Filename.chop_suffix s ".fcl"
  else file_output := s
;;

(* ********************************* *)

let set_number_of_test = (:=) number_of_test ;;

let get_number_of_test () = !number_of_test ;;

(* ********************************* *)

let set_seq () = use_seq_function := true ;;

let set_let () = use_seq_function := false ;;

let use_seq_function () = !use_seq_function ;;

(* ********************************* *)

let input_lexbuf = ref None ;;

let set_input_lexbuf lb = input_lexbuf := lb ;;

let get_input_lexbuf () = !input_lexbuf ;;

(* ********************************* *)

let int_size = ref 65536 ;;

let set_int_size n = int_size := n ;;

let get_int_size () = !int_size ;;

(* ********************************* *)

let get_use_report () = !use_report ;;

let set_use_report s = use_report := Some s ;;

(* ********************************* *)

let use_prolog = ref false ;;

let set_use_prolog b = use_prolog := b ;;

let get_use_prolog () = !use_prolog ;;

(* ********************************* *)

let prolog_path = ref "sicstus" ;;

let set_prolog_path s = prolog_path := s ;;

let get_prolog_path () = !prolog_path ;;

(* ********************************* *)

let prolog_opt = ref "meta" ;;

let set_prolog_opt s = prolog_opt := s ;;

let get_prolog_opt () = !prolog_opt ;;

let mcdc_number = ref 0 ;;

let set_mcdc_number n = mcdc_number := n ;;

let get_mcdc_number () = !mcdc_number ;;

(* ********************************* *)

(* ********************************* *)

let verbose_mode = ref false;;

let set_verbose_mode b = verbose_mode := b;;

let print_verbose s =
  if !verbose_mode then
    print_string s
  else
   ();;

(* ***** *)

let prolog_stat_file : string option ref = ref None;;

let set_prolog_stat_file o = prolog_stat_file := o;;

let get_prolog_stat_file () = !prolog_stat_file;;


let globalstk = ref 0;;

let set_globalstk t =
  globalstk := t;;
let get_globalstk () = !globalstk;;

let localstk = ref 0;;

let set_localstk t =
  localstk := t;;
let get_localstk () = !localstk;;

let choicestk = ref 0;;

let set_choicestk t =
  choicestk := t;;
let get_choicestk () = !choicestk;;

let trailstk = ref 0;;

let set_trailstk t =
  trailstk := t;;
let get_trailstk () = !trailstk;;

let prologmax = ref 0;;

let set_prologmax t =
  prologmax := t;;
let get_prologmax () = !prologmax;;

let opens = (ref [] : string list ref);;

let add_open m = (opens := m :: !opens);;
let get_open () = !opens;;

