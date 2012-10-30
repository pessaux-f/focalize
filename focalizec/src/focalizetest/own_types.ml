(*open Debug;; *)
open Own_basics;;

(* Types' type :  *)
type typ =
  | TAtom of string option * string (** represents : int, float, string ... *)
  | TSpecPrm of string (** represents collection parameters *)
  | TFct of typ * typ (** for a type : typ -> typ *)
  | TProd of typ * typ  (** product type *)
  | TPrm of string option * string * (typ list);;  (** Parameterized type : list(int), option(float) *)

type constructor = ident * (typ list);;

type typ_body =
  | Type of constructor list
  | TParam of string * typ_body;;

let rec separate_args m =
  match m with
  | Type(c_l) -> [], c_l
  | TParam(s,t) -> 
      let args, c_l = separate_args t in
      s::args, c_l;;

type typ_definition = string * typ_body;;

(* Convert Own_types.typ term to string *) 
let rec string_of_typ =
        let open_paren prec op_prec =
                if prec > op_prec then "(" else "" in
        let close_paren prec op_prec =
                if prec > op_prec then ")" else "" in
        let rec print prec exp = 
                match exp with
(*                 | TVar s -> "'" ^ s (* 'a, 'b ... *) *)
                | TAtom(_, s) -> s (* int, float, Self, ... *)
                | TSpecPrm(s) -> s
(*                 | TSelf -> "Self" *)
                | TFct (t1,t2) ->
                                open_paren prec 0 ^
                                print 1 t1 ^ " -> " ^ print 0 t2 ^
                                close_paren prec 0
                | TProd (t1,t2) ->
                             (*   open_paren prec 1 ^
                                print 2 t1 ^ " * " ^ print 1 t2 ^
                                close_paren prec 1 *)
                                "(" ^
                                print 2 t1 ^ " * " ^ print 1 t2 ^
                                ")"
                | TPrm (_, s, l) -> s ^ if l = [] then "" else to_args string_of_typ l
        in
         fun t -> 
            print 0 t;;

(* Types' type :  *)
let rec string_of_ttyp t =
  match t with
  | TAtom(None, s) -> let s = if s = "Self" then focself else s in "TAtom(None, " ^ s ^ ")"
  | TAtom(Some m, s) -> let s = if s = "Self" then focself else s in "TAtom("^ m ^ ", " ^ s ^ ")"
  | TSpecPrm(s) -> "TSpecPrm(" ^ s ^ ")"
  | TFct(t1,t2) -> "TFct(" ^ string_of_ttyp t1 ^ "," ^ string_of_ttyp t2 ^ ")"
  | TProd(t1,t2) -> "TProd(" ^ string_of_ttyp t1 ^ "," ^ string_of_ttyp t2 ^ ")"
  | TPrm(None, s, t_l) ->
      if t_l = [] then
        string_of_ttyp (TAtom(None, s))
      else
        "TPrm(" ^ s ^ "(" ^ to_args string_of_ttyp t_l ^ "))"
  | TPrm(Some m, s, t_l) ->
      if t_l = [] then
        string_of_ttyp (TAtom(Some m, s))
      else
        "TPrm(" ^ m ^ ", " ^ s ^ "(" ^ to_args string_of_ttyp t_l ^ "))";;

(** Transforms a constructor to a string in OCaml syntax like.
For debugging purpose only.
*)
let dbg_string_constructor ((n, t) : constructor) =
  if t = [] then ident_name n
  else (ident_name n) ^ " of " ^ add_string_args t "," string_of_typ
;;

let rec dbg_string_typ_body =
  function
    | Type l ->
        let add_one s e = s ^ " | " ^ dbg_string_constructor e in
        List.fold_left add_one "" l
    | TParam(s, d) ->
        "fun " ^ s ^ " -> " ^
          dbg_string_typ_body d;;

(** Transforms a type body to a string in OCaml syntax like.
For debugging purpose only.
*)
let dbg_string_typ_definition (name, body) =
  "type " ^ name ^ " = " ^ dbg_string_typ_body body;;

(* depends_type: typ -> typ list  *)
(* depends_type t
   return all types t depends on
   Ex :  list(int) depends on type int
         option(list(int * int)) depends on int, list(int * int)
  *)
let depends =
  let rec aux typ =
    match typ with
    | TAtom _ -> [ typ ]
    | TSpecPrm _ -> [ typ ]
    | TProd (t1, t2)
    | TFct (t1, t2) ->  aux t1 @ aux t2
    | TPrm (_, _,l) ->
        typ :: List.fold_left (fun seed e -> aux e @ seed) [] l in
  function
    | TAtom _ -> []
    | TPrm (_, _, l) -> List.fold_left (fun seed e -> aux e @ seed) [] l 
    | typ -> aux typ
;;


let is_in tt t =
  let l = depends t in
  List.mem tt (t::l);;

(* flatten_prod: typ -> typ list *)
(** flatten_type t
    flatten t over products constructor *)
let rec flatten_prod  = function
  | TProd (e1, e2) -> (flatten_prod e1) @ (flatten_prod e2)
  | typ -> [typ]
;;

let flatten_prod_right = function
  | TProd (e1, e2) -> e1::flatten_prod e2
  | typ -> [typ]
;;


let rec get_arity typ =
  match typ with
  | TAtom(_) -> 0
  | TSpecPrm(_) -> 0
  | TFct(_, t2) -> get_arity t2 + 1
  | TProd(_, _) -> 0
  | TPrm(_, _, _) -> 0;;



