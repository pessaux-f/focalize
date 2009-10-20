open Own_prop;;
open Own_basics;;
open Own_types;;
open Own_expr;;

(*
let rec meth_to_string m =
        let args_to_string arg = match arg with
        | [] -> ""
        | e::r -> "(" ^ (List.fold_left (fun seed e -> seed ^
                                                      "," ^
                                                      (meth_to_string e) )
                                       (meth_to_string e)
                                       r) ^ ")" in
        match m with
        | SpecCall (s,m,arg) ->
                        if s = "Self" then
                                "!" ^ m ^  (args_to_string arg)
                        else  
                                s ^ "!" ^ m ^  (args_to_string arg)
        | Basic (m,arg) -> "#" ^ (args_to_string arg)
        | (LVar x) -> x;;

*)

(** Obtain a string corresponding to a proposition *)
let rec string_of_prop =
  let open_paren prec op_prec =
    if prec > op_prec then "(" else "" in
  let close_paren prec op_prec =
    if prec > op_prec then ")" else "" in
  let rec pts prec = function
     | PUniv(s,t,(PUniv(_,_,_) as p)) ->
         (if prec != (-1) then "all " else " ") ^
         "(" ^ s ^ " : " ^ string_of_typ t ^ ")" ^ (pts (-1) p)
     | PUniv(s,t,p) ->
         (if prec != (-1) then "all " else " ") ^
         "(" ^ s ^ " : " ^ string_of_typ t ^ ")" ^ (pts (-1) p)
     | PEx(s,_,(PEx(_,_,_) as p)) ->
         (if prec != (-2) then "Exists " else " ") ^
         s ^ ", " ^ (pts (-2) p)
     | PEx (s,_,p) ->
         (if prec != (-2) then "Exists " else " ") ^
         s ^ ", " ^ (pts (-1) p)
     | PAnd (p1,p2) ->
        (open_paren prec 4) ^
        (pts 4 p1) ^ " and " ^ (pts 4 p2) ^ 
        (close_paren prec 4)
     | POr (p1,p2) ->
        (open_paren prec 3) ^
        (pts 3 p1) ^ " or " ^ (pts 3 p2) ^
        (close_paren prec 3)
     | PImp ((PImp(_,_) as p1),p2) ->
        (open_paren prec 2) ^
        (pts 18 p1) ^ " -> " ^ (pts 2 p2) ^
        (close_paren prec 2)
     | PImp (p1,p2) ->
        (open_paren prec 2) ^
        (pts 2 p1) ^ " -> " ^ (pts 2 p2) ^
        (close_paren prec 2)
     | PEq (p1,p2) ->
        (open_paren prec 1) ^
        (pts 1 p1) ^ " <-> " ^ (pts 1 p2) ^
        (close_paren prec 1) 
     | PNot p -> "not(" ^ (pts 0 p) ^ ")"
     | PCall e -> string_of_myexpr e in
     pts 0;;


let print_prop p = print_string (string_of_prop p);;

(** conversion on elementary form *)

(** Transform a positive or negative call to a string *)
let string_of_call (e,s) =
   if s then
	    string_of_myexpr e
	else
	    "#not_b(" ^ string_of_myexpr e ^ ")";;


(** convert a pre_cond *)
let string_of_precond : pre_cond -> string = function
        | [] -> "#True"
        | e::r -> List.fold_left (fun seed e -> seed ^ " and " ^
                                                (string_of_myexpr e))
                                 (string_of_myexpr e)
                                 r;;

(** convert a conclusion *)
let string_of_conclusion : conclusion -> string = function
        | [] -> "#False" (* Impossible case *)
        | e::r -> List.fold_left (fun seed e -> seed ^ " or " ^
                                                (string_of_myexpr e))
                                 (string_of_myexpr e)
                                 r;;

let print_elem_precond p =
   print_string (string_of_precond (get_precond p));;

let print_elem_conclusion c =
   print_string (string_of_conclusion (get_conclusion c));;

let print_elementaire : elementaire -> unit =  fun x -> 
        print_string "pre-condition : ";
        print_elem_precond x;
        print_string "conclusion : ";
        print_elem_conclusion x;
        print_newline ();;
        
let print_elementaires : elementaire list -> unit =
       List.iter print_elementaire;;

