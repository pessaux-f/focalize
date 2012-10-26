open Own_basics;;
(*

Nouvelle syntaxe :

cs := nom d'une espèce complète
c := nom de collection

let c = cs in
let c = cs(c, ..., c) in
let c = cs(c, ..., c) in
let c = cs(c, ..., c) in
let c = cs(c, ..., c) in
cs(c, ..., c)

*)

type species_context =
  Own_expr.species_name * string list;;
(* cs(c, ... , c) *)

let sc_get_name (e : species_context) = fst e;;
let sc_get_parameters e = snd e;;

let string_of_sc ((s,args) : species_context) =
  let s = Own_expr.string_of_species_name s in
  match args with
  | [] -> s
  | e::[] -> s ^ "(" ^ e ^ ")"
  | e::r -> List.fold_left (fun s e -> s ^ ", " ^ e) (s ^ "(" ^ e ) r ^")";;


let create_sc species args : species_context =
  species, args;;

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

(*
type binding_element =
  Collection of string
| Entity of 
*)


(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

type binding_context_aux =
  BC_Coll of species_context
| BC_Ent of Own_expr.myexpr;;

let bca_c s = BC_Coll s;;
let bca_e e = BC_Ent e;;

type binding_context = string * binding_context_aux;;

(* let c = cs(c, ... , c) in *)
(* let e = expr in *)
(** A tuple of species name * the list of all collection under test. *)

(*
let bc_get_args (bindings : binding_context list) arg_name =
  snd (List.assoc arg_name bindings);;
*)

let bc_get_name bc = fst bc;;
let bc_get_parameters bc =
  match snd bc with
  | BC_Coll(sc) -> sc_get_parameters sc
  | BC_Ent(_) -> []
;;
let bc_get_name2 (bc : binding_context) =
  match snd bc with
  | BC_Coll(sc) -> sc_get_name sc
  | BC_Ent(_e) -> failwith "bc_get_name2 : Own_expr.string_of_myexpr e"
;;


(* let bc_get_species bc = snd bc;; *)

(*
let rec bc_list_get_species (b_l : binding_context list) arg_name : string =
  match b_l with
  | [] -> raise Not_found
  | (arg_name',(s,_))::r -> 
      if arg_name = arg_name' then
        s
      else
        bc_list_get_species r arg_name;;
*)

let string_of_bc ((s, sc) : binding_context) =
  match sc with
  | BC_Coll(sc) ->
      "let " ^ s ^ " = " ^ string_of_sc sc
  | BC_Ent(e) ->
      "let " ^ s ^ " = " ^ Own_expr.string_of_myexpr e;;


let create_bc name spec_context : binding_context =
  name, spec_context;;

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

type test_context =
  binding_context list * species_context;;
(** The list of parameters * the species under test. *)

let string_of_tc ((bcs, sc) : test_context) =
  List.fold_right (fun e s -> string_of_bc e ^ " in\n" ^ s)
                  bcs (string_of_sc sc);;

let create_tc collections species : test_context =
  collections, species;;

let tc_add_bc ((bl,c): test_context) (bc : binding_context) :
  test_context =
    bc::bl, c;;

let tc_get_end_sc (tc : test_context) = snd tc;;

(*
let tc_get_assoc_spec tc n =
  fst (List.assoc n (fst tc));;
*)

let tc_get_parameters (tc : test_context) : binding_context list = fst tc;;

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

type context_path = string list;;

let empty_path : context_path = [];;

(** Remove all element of tc which are after the last occurence of c in tc *)
let goto_coll (tc : binding_context list) (c : string) =
  let rec aux l =
    match l with
    | [] -> raise Not_found
    | (n,bca)::r ->
        if n = c then 
            bca, List.rev r
        else
          aux r in
  aux (List.rev tc);;

let string_of_cp (cp : context_path) : string =
  string_of_list (fun x -> x) cp;;

let letter_string_of_cp (cp : context_path) : string =
  "f_" ^ add_string_args cp "_foctest_" (fun s -> s);;

let species_of_cp (tc : test_context) (cp : context_path) =
  let rec aux (cp : context_path) (l : binding_context list) (spec, args) =
    match cp with
    | [] -> spec
    | e::r ->
        let n = Focalize_inter.get_parameters_number spec e in
        let coll = try List.nth args n with | Not_found -> "Internal Error :p" in
        let n_e, n_l = try goto_coll l coll with | Not_found -> failwith "Internal error :D" in
        match n_e with
        | BC_Coll(n_e) -> aux r n_l n_e
        | BC_Ent(_) -> failwith "species_of_cp: context-path poins to a entity" in
  aux cp (fst tc) (snd tc);;

let cp_add (cp : context_path) a : context_path =
  let new_path = cp @ [a] in
  new_path;;

let rec all_diff l = 
  match l with
  | [] -> true
  | e::r -> if List.mem e r then
               false
            else
              all_diff r;;

let rec free_variables_tc tc = 
  match tc with
  | (n,bca)::r, sc ->
      List.filter (fun e -> n <> e) (free_variables_tc (r, sc)) @
      (match bca with
      | BC_Coll((_n,args)) -> args
      | _ -> []
      )
  | [], (_n,args) -> args;;

let wf_tc ((c_l,c) : test_context) =
  free_variables_tc (c_l,c) = [] &&
  all_diff (List.map bc_get_name (c_l)) ;;

(** The test context harness is calculated from a well-formed test_context.
It add to each binding of a context test the parameters needed for
instanciating. *)

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

type parameters =
  | For_harness of string * int list
  | From_user of string * int list

let string_of_parameters = 
  function
    | For_harness(s, i) -> s ^ to_args string_of_int i 
    | From_user(s, i) -> s ^ to_args string_of_int i 
;;

let parameters_get_name =
  function
    | For_harness(s, _i) -> s
    | From_user(s, _i) -> s
;;


let string_of_parameters_debug = 
  function
    | For_harness(s, i) ->
        "For_harness(" ^ s ^ ", " ^ string_of_list string_of_int i ^ ")"
    | From_user(s, i) ->
        "From_user(" ^ s ^ ", " ^ string_of_list string_of_int i ^ ")"
;;

type species_context_harness = 
  Own_expr.species_name * parameters list

let sch_get_species e = fst e;;
let sch_get_parameters (e : species_context_harness) = snd e;;

let string_of_sch ((s,args) : species_context_harness) =
  let s = Own_expr.string_of_species_name s in
  List.fold_left (fun s e -> s ^ ", " ^ string_of_parameters e) (s ^ "(") args ^")";;

type binding_context_harness_aux =
  BCH_Coll of species_context_harness
| BCH_Ent of Own_expr.myexpr;;

type binding_context_harness = string * binding_context_harness_aux;;

let bch_get_name (bch : binding_context_harness) = fst bch;;
let bch_get_sch (bch : binding_context_harness) = snd bch;;

let bch_get_args (bindings : binding_context_harness list) arg_name =
  try
    match List.assoc arg_name bindings with
    | BCH_Coll(sc) -> List.map parameters_get_name (snd sc)
    | BCH_Ent(_) -> failwith "bch_get_args : no args"
  with
  | Not_found -> failwith ("bch_get_args: " ^ arg_name);;

let string_of_bch ((s, sch) : binding_context_harness) =
  match sch with
  | BCH_Coll sch -> "let " ^ s ^ " = " ^ string_of_sch sch ^ "\n"
  | BCH_Ent e -> "let " ^ s ^ " = " ^ Own_expr.string_of_myexpr e ^ "\n" ;;

type test_context_harness =
  binding_context_harness list * species_context_harness;;

let tch_get_binding (tc : test_context_harness) =
  fst tc;;
let tch_get_sc (tc : test_context_harness) =
  snd tc;;

let string_of_tch ((bch_l, sch) : test_context_harness) =
   List.fold_left (fun s e -> s ^ string_of_bch e)
                  "" bch_l ^
   string_of_sch sch;;

let print_tch (c : test_context_harness) =
  print_string (string_of_tch c);;

let list_assoc_range l e =
  let rec aux l n =
    match l with
    | [] -> raise Not_found
    | e'::r -> 
        match e' with
        | From_user(e',_)
        | For_harness(e',_) ->
            if e' = e then
              n
            else
              aux r (n + 1) in
  aux l 1;;


(**
[add_arg arg_l arg_name]

Add the element [arg_name] in the end of the list of argument [arg_l] and
returns it associated number.
If [arg_name] is present in [l], it doesn't change [l] .

*)
let rec add_arg args_l (arg_name : string) (bindings : binding_context_harness list) =
  try 
  let n = list_assoc_range args_l arg_name in
  args_l, n
  with
  | Not_found ->
      let args = bch_get_args bindings arg_name in
      let args_l, n_args = add_args args_l args bindings in 
      args_l @ [For_harness(arg_name, n_args)], List.length args_l + 1

(**
[add_args arg_l arg_name_l]

Do the same thing of add_args but for a list of argument.

*)
and
add_args args_l arg_names (bindings : binding_context_harness list) =
  List.fold_left (fun (args_l, n_l) e -> 
                    let args_l,n = add_arg args_l e bindings in
                      args_l, n_l @ [n]
                 )
                 (args_l, [])
                 arg_names;;

let rec add_existing_args n_args args (bindings : binding_context_harness list) =
  match args with
  | [] -> n_args
  | bind_name::r -> 
      try 
      match List.assoc bind_name bindings with
      | BCH_Coll(_, exp_args) ->
        let exp_args = List.map parameters_get_name exp_args in
(*        let exp_args = List.map (binding_list_get_species bindings) exp_args in *)
(*        print_string (string_of_list (fun x -> x) exp_args); *)
        let n_args, n_exp_args = add_args n_args exp_args bindings in
        add_existing_args (n_args @ [From_user(bind_name, n_exp_args)]) r bindings
      | BCH_Ent(_e) ->
          add_existing_args (n_args @ [From_user(bind_name, [])]) r bindings
      with
      | Not_found -> failwith ("add_existing_args: " ^ bind_name)
;;


let tch_of_tc (ct : test_context) : test_context_harness =
  let rec aux (bc : binding_context_aux)
              (others : binding_context list)
                       : (binding_context_harness list * binding_context_harness_aux) =
       (* spec, args <- binding_context *)
    match others with
    | [] ->
        (match bc with
         | BC_Coll(spec, args) ->
             let tch = [], BCH_Coll(spec, add_existing_args [] args []) in
(*             print_string (string_of_tch tch); *)
(*             print_newline (); print_newline (); *)
             tch
         | BC_Ent(e) ->
             let tch = [], BCH_Ent(e) in
             tch
        )
    | (n,bc2)::r ->
        let bindings, last_bch = aux bc2 r in
        (match bc with
        | BC_Coll(spec,args) ->
(*           print_string n; print_newline (); *)
(*           print_string (to_args (fun x -> x) args); print_newline (); *)
           let n_args = add_existing_args [] args ((n,last_bch)::bindings) in
           let tch = bindings @ [n, last_bch], BCH_Coll(spec, n_args) in
(*           print_string (string_of_tch tch); *)
(*           print_newline (); print_newline (); *)
           tch
        | BC_Ent(e) -> bindings @ [n, last_bch], BCH_Ent(e)
        ) in
    let spec_n = fst(snd ct) and args = snd(snd ct) in
    let b, r = aux (BC_Coll(spec_n, args)) (List.rev (fst ct)) in
    match r with
    | BCH_Ent(_) -> failwith("tch_of_tc : internal error")
    | BCH_Coll(s,a) -> b, (s,a);;

(*
let tch_get_fst tch =
*)
