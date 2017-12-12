(* keyword of focal compiler/langage *)

type ident =
  | Infix of string
  | Prefix of string option * string
;;

let ident_name s =
  match s with
  | Infix s -> s
  | Prefix(_,s) -> s ;;

let prefix s = Prefix(None, s);;
let prefix_m m s = Prefix(Some m, s);;

let focself = (* Keywords.sobj;; *) (* TODO : focalize *) "Self" ;;
(* let focselfkey = "Self" ;; *)
let focexception = "Ml_builtins.Focalize_error" ;; (* defined by focalizec *)
let focbasics = "basics" ;;

let focequal = Infix "=" ;;
let focintequal = Infix "=0x" ;;
let focintgt = Infix ">" ;;
let focintgeq = Infix ">=" ;;
let focintleq = Infix "<=" ;;
(* let focintmod = Prefix "int_mod" ;; *)

let focaddint = Infix "+" ;;
let focstringconcat = Infix "^" ;;
let focpred = Prefix(Some "basics", "int_pred");;
let focsucc = Prefix(Some "basics", "int_succ");;
let focunit = Prefix(Some "basics", "()");;
let foctrue = Prefix(Some "basics", "True");;
let focfalse = Prefix(Some "basics", "False");;
let foccrp = Prefix(Some "basics", "pair");;
let focfst = Prefix(Some "basics", "fst");;
let focsnd = Prefix(Some "basics", "snd");;
let focnot = Prefix(None, "~~");;
let focor = Infix "||" ;;
let focand = Infix "&&" ;;

let foctself = "Self" ;;
let focnil = Prefix(Some "basics", "[]");;
let foccons = Infix "::" ;;
let focnonfailed = Prefix(Some "basics", "non_failed");;
let focisfailed = Prefix(Some "basics", "is_failed");;
let focunfailed = Prefix(Some "basics", "Unfailed");;
let focfailed = Prefix(Some "basics", "Failed");;
let focerror = Prefix(Some "basics", "focalize_error");;

(* The types *)
let foctoption = "partiel" ;;
let foctint = "int" ;;
let foctbool = "bool" ;;
let foctunit = "unit" ;;
let foctstring = "string" ;;
let foctchar = "char" ;;
let foctfloat = "float" ;;
let foctlist = "list" ;;

let focparseint = Prefix (Some "basics", "int_of_string") ;;
let focprintint = Prefix (Some "basics", "string_of_int") ;;
let focprintstring = Prefix (Some "basics", "print_string") ;;

(* Methods to get the name of *)

let suffixe_id =
  let i = ref 0 in
  fun s -> i := !i + 1; s ^ "__" ^ string_of_int (!i)
;;

let spec_instru_name s l =
  suffixe_id (s ^ "_instru" ^ List.fold_left (fun s e -> s ^ "_" ^ e) "" l) ;;

let spec_test_name s = suffixe_id (snd s ^ "_test") ;;

let coll_name s = "coll_" ^ s ;;

let param_name i  = "p" ^ (string_of_int i) ;;

let result_type = "result" ;;
let result_ok = ("Ok", []) ;;
let result_ko = ("Ko", []) ;;
let result_raise = "Raise",[foctstring] ;;

let verdict_type = "verdict" ;;
let verdict_precond_ok = "PreOk", ["result * list(result)"] ;;
let verdict_precond_ko = "PreKo", ["result * list(result)"] ;;


(* Convenient functions *)

(* calculate the union of two lists *)

let concat_no_doublon l1 l2 =
  let add_elem el l = if List.mem el l then l else el :: l in
  List.fold_right add_elem l1 (List.fold_right add_elem [] l2)
;;


let (++) = concat_no_doublon ;;


(* flatten a list and eliminate all doublon *)
let rec flatten_no_doublon = function
  | []   -> []
  | e :: r -> e ++ (flatten_no_doublon r)
;;


(* transforme la liste d'elements en chaine de caractere :
 *    [e1;e2;e3;... en] donne
 *          f e1 ^ s ^ f e2 ^ ... ^ s ^ f en *)
let add_string_args l s f =
  let rec aux l =
    match l with
    | [] -> ""
    | [ e ] -> f e
    | e :: r -> f e ^ s ^ (aux r) in
  aux l
;;


(* prend une liste de variables et met sous la forme
 *    (var1,var2,var3 ... )          *)
let to_args f l = "(" ^ (add_string_args l ", " f)  ^ ")" ;;


let rec int_list min max =
  if min > max then [] else min::int_list (min+1) max
;;


(* Returns l1@l2 where duplicate associations have been deleted.
The order is preserved. *)
let merge_assoc_list l1 l2 =
  let rec aux cumul l =
    match l with
    | [] -> cumul
    | ((e, _) as e') :: r ->
        if List.mem_assoc e cumul then aux cumul r
        else aux (e'::cumul) r in
  List.rev (aux (aux [] l1) l2)
;;


let ( @-@ ) = merge_assoc_list ;;


(** string_of_list f l *)
let string_of_list f l =
  let rec aux = function
    | [] -> "]"
    | e::r -> ";" ^ f e ^ aux r in
  match l with
  | [] -> "[]"
  | e::r -> "[" ^ f e ^ aux r
;;


let list_create f n =
  let rec aux n l =
    if n <= 0 then [] else aux (n - 1) (f ():: l) in
  aux n []
;;


let replace pat dest s =
  let tmp = ref "" in
  String.iter
    (fun e ->
      if e = pat then tmp := !tmp ^ dest
      else tmp := !tmp ^ String.make 1 e)
    s ;
  !tmp
;;

  (*
  let rec comp pat ip lp s i ls =
    if ip >= lp then
      true
    else
      if i >= ls then
        false
      else
        if pat.[ip] = s.[i] then
          comp pat (ip + 1) lp s (i + 1) ls
        else
          false in
  let l1 = String.length pat in
  let l2 = String.length s in
  let rec aux s i =
    if i >= l2 then
      ""
    else
      if comp pat 0 l1 s i l2 then
        dest ^ aux s (i + l1)
      else
        String.make 1 (s.[i]) ^ aux s (i+1) in
  aux s 0;;
*)

let rec string_assoc s l =
  match l with
  | [] -> s
  | (p, e) :: r -> string_assoc (replace p e s) r
;;


let string_of_bool b = if b then "true" else "false" ;;

let to_prolog_assoc = [
  ':', "foctest_colon" ;
  '[', "foctest_lbracket" ;
  ']', "foctest_rbracket" ;
  ')', "foctest_rparen" ;
  '(', "foctest_lparen" ]
;;


let string_letter_name s = string_assoc s to_prolog_assoc ;;

let ident_letter_name s =
  let s = ident_name s in
  string_assoc s to_prolog_assoc
;;

let atom_of_cons_name n =
  let n1 = (string_letter_name n) in
  if n1 = "" then failwith "Un constructeur a pour nom une chaine vide ???"
  else n1.[0] <- Char.lowercase_ascii (n1.[0]) ;
  n1
;;

let string_of_option f t =
  match t with
  | None -> "none"
  | Some t -> "some (" ^ f t ^ ")"
;;
