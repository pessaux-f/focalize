open Own_types;;
open Own_prolog;;
open Own_basics;;

(* A type in prolog is represented as a tuple of [type name] * [constructor list]
*)

let rec prolog_args_of_constructor_typ =
  let rec aux t =
    match t with
    | TAtom(_m, s) -> prolog_fun s []  
    | TSpecPrm _ -> failwith "houlala"
    | TProd(t1,t2) -> prolog_fun "couple" [aux t1; aux t2]
    | TPrm(_m, s,t_l) -> prolog_fun s (List.map aux t_l)
    | TFct(_e1, _e2) -> failwith "prolog_args_of_constructor_typ : Fct" in
  fun t -> 
    List.map aux t;;
(*
    match t with
    | TFct(e1, e2) -> aux e1 :: prolog_args_of_constructor_typ e2
    | _ -> [aux t];;
*)


let prolog_type_of_constructor ((n,t) : constructor) =
      prolog_fun (atom_of_cons_name (ident_name n)) (prolog_args_of_constructor_typ t);;


let prolog_type_of_typedef ((n,b_l) : typ_definition) =
  let args, c_l = separate_args b_l in
  prolog_fun "def" [prolog_fun n (List.map (fun e -> prolog_fun e []) args);
                    prolog_list
                    (List.map prolog_type_of_constructor c_l)];;

let protect_cons_name (t : typ_definition list) =
  let protect_ident n =
    Prefix(None, ident_letter_name n) in
  let rec protect_cons c =
    match c with
    | Type c_l -> Type (List.map (fun ((n, t_l) : constructor) -> protect_ident n, t_l) c_l) 
    | TParam(s,t) -> TParam(s, protect_cons t) in
  List.map (fun (n,c) -> n, protect_cons c) t;;

(** Construct the prolog predicate that imports all type currently defined to
the prolog interpreter *)
let import_all_types () =
  let all_types = Focalize_inter.get_all_types () in
  let all_types = protect_cons_name all_types in
  let l_types = 
    prolog_list (
    prolog_fun "def" [prolog_fun "pair" [prolog_fun "a" []; prolog_fun "b" []];
                      prolog_list [prolog_fun "pair" [prolog_fun "a" []; prolog_fun "b" []]]]::
   (prolog_fun "def" [prolog_fun "fun" [prolog_int 1; prolog_int 1; prolog_int 1; prolog_int 1]; prolog_list [] ])::
                 List.map prolog_type_of_typedef all_types) in
  prolog_clause None [(prolog_fun "set_type_env" [l_types; prolog_var "_rien"])];;
