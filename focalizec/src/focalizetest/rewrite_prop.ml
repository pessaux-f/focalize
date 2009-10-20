open To_strings;;
open Own_prop;;
open Own_types;;
open Own_expr;;
(***************)

(** Takes a property in caninical form and transform it to normal form *)

(* Add s on all precondition of each elementary form *)
let prefixe_pre_cond s : elementaire list -> elementaire list =
        List.map (add_elem_precond s);;


(* Link s as universal quantified variable on each element of a list of
proposition *)
let prefixe_forall s l = List.map (fun e -> puniv s e) l;;

exception No_canon of proposition;;
exception Conclu of string;;

(* Split a conclusion (receive as a disjunction of methods call) to a
conclusion. Raise Conclu if bad form *)
let rec aplati_conclu  p : conclusion =
  match p with
    | POr(a,b) -> (aplati_conclu a) @ (aplati_conclu b)
    | PCall(m) -> [positif m]
    | PNot(PCall(m)) -> [negatif m]
    | _ -> raise (Conclu (string_of_prop p));;


(* Takes a proposition in disjunctive normal form and returns the elementaries
list corresponding to all differents conclusion*) 
let coupe pre conclu : elementaire list =
  let rec aux_coupe concl =
    match concl with
    | PAnd(a,b) -> (aux_coupe a) @ (aux_coupe b)
    | _ -> [create_elementaire pre (aplati_conclu concl)] in
       aux_coupe conclu;;

let rec get_variables p =
  match p with
  | PUniv(v,t,p) ->
      let p, vs = get_variables p in
      p, add_variable (create_variable v t) vs
  | _ -> p, variables_null;;

(* Take a property in canonical form, returns the normals forms. *)
let rec normalise_canon n p : normal_forms =
  let rec aux_norm x =
    try 
      match x with
      | PImp(POr(a,b),c) ->
           aux_norm (PImp(a,c)) @ aux_norm (PImp(b,c))
      | PImp(PAnd(a,b),c) ->
           aux_norm (PImp(a,PImp(b,c)))
      | PImp(PCall s,(PImp(_a,_b) as c)) ->
           prefixe_pre_cond (positif s) (aux_norm c)
      | PImp(PNot (PCall s),(PImp(_a,_b) as c)) ->
           prefixe_pre_cond (negatif s) (aux_norm c)
      | PImp(PCall s,conclu) ->
           coupe [positif s] conclu
      | PImp(PNot (PCall s),conclu) ->
           coupe [negatif s] conclu
      | PAnd(a,b) ->
           (coupe [] a) @ (coupe [] b)
      | POr(_,_) as x  ->
           [create_elementaire [] (aplati_conclu x)]
      | PCall x ->
           [create_elementaire [] ([positif x])]
      | PNot (PCall x) ->
           [create_elementaire precond_null (create_conclusion (negatif x))]
      | _ -> raise (No_canon x)
    with
    | Conclu _ -> raise (No_canon x) in
  let p,vs = get_variables p in
    create_normal_forms n vs (aux_norm p)


(* Checks if a proposition is in conjunctive form *)
let rec is_conjonctive_form =
        let rec is_disjonction p =
                match p with
                | POr(a,b) -> is_disjonction a && (is_disjonction b)
                | PCall _ | PNot (PCall _) -> true
                | _ -> false in
        fun p ->
                match p with
                | POr(a,b) -> is_disjonction a && (is_disjonction b) 
                | PAnd(a,b) -> (is_conjonctive_form a) && (is_conjonctive_form b)
                | PCall _ | PNot (PCall _) -> true
                | _ -> false;;

let rec is_canonique p =
        let rec is_or_and_form p =
                match p with 
                | POr(a,b) | PAnd(a,b) ->
                                is_or_and_form a && (is_or_and_form b)
                | PCall _ | PNot (PCall _) -> true
                | _ -> false in
        let rec is_imply_form p =
                match p with
                | PImp(a,(PImp(_,_) as b)) ->  is_or_and_form a && (is_imply_form b)
                | PImp(a,b) -> (is_or_and_form a)  && (is_conjonctive_form b)
                | PNot (PCall _) | PCall _ -> true
                | PNot _ -> false
                | PAnd (_,_) | POr(_,_) -> is_conjonctive_form p
                | PEx (_,_,_) | PUniv (_,_,_)-> false
                | PEq (_,_) -> false in 
        match p with
        | PUniv (_v,_,a) -> is_canonique a
        | _ -> is_imply_form p;;

exception Not_good_form of string;;

(* Transforme une proposition en forme élémentaire *)
let normalise (n,p) = if is_canonique p then
                      normalise_canon n p
                  else
                      raise (Not_good_form n);;

