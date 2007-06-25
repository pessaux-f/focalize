(*  Copyright 2006 INRIA  *)
(*  $Id: invoke_cime.ml,v 1.3 2007-06-25 12:58:56 pessaux Exp $  *)


let cime_nb = ref 0 ;;
let file_nb = ref 0 ;;


let copy_file name oc =
  let ic = open_in_bin name in
  let buflen = 8192 in
  let buf = String.create buflen in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n = 0 then ()
    else
      (begin
      output oc buf 0 n ;
      loop ()
      end) in
  loop () ;
  close_in ic
;;


let file_size name =
  let ic = open_in_bin name in
  let result = in_channel_length ic in
  close_in ic ;
  result
;;


let ehead s = String.sub s 1 (String.length s -1)
;;


(* La chaîne de caractères s1 commence-t-elle par s2 ? *)

let rec bw s1 s2 =
  match String.length s2 with
  | 0 -> true
  | _ ->
      if (String.get s1 0 == String.get s2 0) then
	bw (ehead s1)  (ehead s2)
      else false
;;


(* Préfixe une chaîne de caractères par c. *)
let pref c s = String.concat c [String.create (0) ; s] ;;


(* Préfixe une chaîne de caractères par "v" *)
(* et la suffixe par un entier n donné.     *)
let createNewVar s n =
  let s1 = String.concat "v" [ String.create (0) ; s] in
  String.concat s1 [String.create (0) ; string_of_int (n)]
;;


(* Une expression peut elle se réécrire sous forme d'égalité ? *)
let rec isRewritable exp =
  match exp with
  | Expr.Enot (e1) -> isNotRewritable e1
  | Expr.Eand (e1, e2) -> isRewritable e1 && isRewritable e2
  | Expr.Eor (e1, e2) -> false
  | Expr.Eimply (e1, e2) -> false
  | Expr.Eequiv (e1, e2) -> false
  | Expr.Eall (sl, e) -> isRewritable e
  | Expr.Eex (sl, e) -> isRewritable e
  | Expr.Eapp ("Is_true", [t])-> isRewritable t
  | Expr.Eapp ("abst_egal", el)-> true
  | Expr.Eapp ("equal", el) -> true
  | _ -> false


and isNotRewritable exp =
  match exp with
  | Expr.Enot (e1) -> isRewritable e1
  | Expr.Eand (e1, e2) -> false
  | Expr.Eor (e1, e2) -> isNotRewritable e1 || isNotRewritable e2
  | Expr.Eimply (e1, e2) -> isRewritable e1 && isNotRewritable e2
  | Expr.Eequiv (e1, e2) -> false
  | Expr.Eall (sl, e) -> isNotRewritable e
  | Expr.Eex (sl, e) -> isNotRewritable e
  | Expr.Eapp ("equal", el) -> false
  | _ -> false
;;


(* Une hyp peut elle se réécrire sous forme d'égalités ?*)
let rec isHypRewritable hyp =
  match hyp with
  | Expr.Hyp (expr) -> isRewritable expr
  | Expr.Def (s, sl, expr) -> isRewritable expr
;;


(* Toutes les hypothèses d'une liste peuvent *)
(* elles se réécrire sous forme  d'égalité ? *)
let rec isHypListRewritable hl =
  match hl with
  | [] -> true
  | h :: t ->  (isHypRewritable h) && (isHypListRewritable t)
;;


(* Le but peut-il se réécrire sous forme d'égalité ? *)
let isButRewritable (hyps, goal) =
  (isRewritable goal) && (isHypListRewritable hyps)
;;


(* Remplace dans e les variables liées par des variables préfixées par "v", *)
(* les variables libres par des constantes, et les constantes par des       *)
(* constantes préfixées par C.                                              *)
let rec modifyH l expr =
  match expr with
  | Expr.Enot e1 -> Expr.Enot (modifyH l e1)
  | Expr.Eand (e1, e2) -> Expr.Eand (modifyH l e1, modifyH l e2)
  | Expr.Eor (e1, e2) -> Expr.Eor (modifyH l e1, modifyH l e2)
  | Expr.Eimply (e1, e2) -> Expr.Eimply (modifyH l e1, modifyH l e2)
  | Expr.Eequiv (e1, e2) -> Expr.Eequiv (modifyH l e1, modifyH l e2)
  | Expr.Eall (sl, e) -> modifyH (List.append l sl) e
  | Expr.Eex (sl, e) -> modifyH (List.append l sl) e
  | Expr.Evar s ->
      if List.mem s l then Expr.Evar (pref "V" s)
      else Expr.Eapp (pref "c" s, [])
  | Expr.Eapp (s, [])->  Expr.Eapp (pref "c" s, [])
  | Expr.Eapp (s, el) -> Expr.Eapp (s, List.map (modifyH l) el)
  | Expr.Etrue -> Expr.Etrue
  | Expr.Efalse -> Expr.Efalse
;;


(* Remplace dans expr les variables par des constantes, *)
(* et préfixe le nom des constantes par c.              *)
let rec modifyG expr =
  match expr with
  | Expr.Enot e1 -> Expr.Enot (modifyG e1)
  | Expr.Eand (e1, e2) -> Expr.Eand (modifyG e1, modifyG e2)
  | Expr.Eor (e1, e2) -> Expr.Eor (modifyG e1, modifyG e2)
  | Expr.Eimply (e1, e2) -> Expr.Eimply (modifyG  e1, modifyG e2)
  | Expr.Eequiv (e1, e2) -> Expr.Eequiv (modifyG e1, modifyG e2)
  | Expr.Eall (sl, e) -> modifyG  e
  | Expr.Eex (sl, e) -> modifyG  e
  | Expr.Evar s -> Expr.Eapp (pref "c" s, [])
  | Expr.Eapp (s, [])->  Expr.Eapp (pref "c" s, [])
  | Expr.Eapp (s, el) -> Expr.Eapp (s, List.map modifyG el)
  | Expr.Etrue -> Expr.Etrue
  | Expr.Efalse -> Expr.Efalse
;;


(* Modifie les hypothèses, quelle que soit leur forme. *)
let modifyHyps hyps =
  match hyps with
  | Expr.Hyp e -> modifyH [] e
  | Expr.Def (s, sl, e) -> modifyH [] e
;;


let rec printExpr o e =
  match e with
  | Expr.Evar s ->  Format.fprintf o "@[%s@]" s
  | Expr.Eapp (s, []) -> Format.fprintf o "@[%s@]" s
  | Expr.Eapp ("Is_true", el)-> Format.fprintf o "@[%a@]" printExprList el
  | Expr.Eapp ("abst_egal", h :: el)->
      Format.fprintf o "@[%a = %a @]" printExpr h printExprList el
  | Expr.Eapp (s, el) -> Format.fprintf o "@[%s(%a)@]" s printExprList el
  | Expr.Enot e -> Format.fprintf o "~(@[@;%a@])" printExpr e
  | Expr.Eand (e1, e2)->
      Format.fprintf o "@[%a &@;<1 2>%a@]" printExpr e1 printExpr e2
  |_ ->  assert false


and printExprList o el=
  match el with
  | [] -> Format.print_string ""
  | [h] -> printExpr o h
  | h :: t ->
      printExpr o h ;
      Format.fprintf o "@[, @]" ;
      printExprList o t
;;


let printNegatedConjecture o e =
  match e with
  | Expr.Eapp ("Is_true", [Expr.Eapp("abst_egal", h :: [t])]) ->
      Format.fprintf o "@[%a != %a @]" printExpr h  printExpr t
  |_ -> assert false
;;


let printHyp o hypName hyp =
  Format.fprintf o
    "@[%s(%s, %s, %a). \n @]"
    "cnf" ("h" ^ hypName) "hypothesis" printExpr hyp
;;


let rec printHypList o hypName hypList =
  match hypList with
  | [] -> Format.print_string ""
  | Expr.Eand (e1, e2) :: t ->
      printHyp o (hypName ^ (string_of_int !cime_nb)) e1 ;
      incr cime_nb ;
      printHyp o (hypName ^ (string_of_int !cime_nb)) e2 ;
      incr cime_nb ;
      printHypList o hypName t
  | [h] ->
      printHyp o (hypName ^ (string_of_int !cime_nb)) h ;
      incr cime_nb
  | h :: t ->
      printHyp o (hypName ^ (string_of_int !cime_nb)) h ;
      incr cime_nb ;
      Format.fprintf o "@[%s @]" "\n" ;
      incr cime_nb; printHypList o hypName t
;;


let printGoal o goalName goal =
  Format.fprintf o
    "@[%s(%s, %s, %a). \n@]" "cnf"
    ("g" ^ goalName) "negated_conjecture" printNegatedConjecture goal
;;


let printPhrase o ph =
  match ph with
  | Expr.Hyp e -> printExpr o e
  | Expr.Def (s, sl, e) -> printExpr o e
;;


(* Trouver le mot "unsatisfiable" dans un fichier. *)
(* inf : fileName (string) *)
let find inf =
  try
    let ic = open_in_bin inf in
    let lexbuf = Lexing.from_channel ic in
    Parser_sat.cimefile Lexer_sat.cimetoken lexbuf
  with _ -> false
;;


(* Fonction principale. *)
let cime filename data loc statement name oc=
  let lexbuf = Lexing.from_string data in
  let (goal, hyps) = Parser_coq.coqfile Lexer_coq.coqtoken lexbuf in
  let (goal2, hyps2)= (modifyG goal, List.map modifyHyps hyps) in
  let b = (isRewritable goal) && (isHypListRewritable hyps) in
  if not b then
    Invoke.zenon_loc filename (statement, name) data loc oc
  else
    (begin
    let (tmpname, f) = Filename.open_temp_file "zvtov" ".p" in
    let resname = Filename.temp_file "zvtov" ".res" in
    let fmt = Format.formatter_of_out_channel f in
    printHypList fmt name hyps2 ;
    printGoal fmt name goal2 ;
    Format.pp_print_flush fmt () ;
    close_out f ;
    (* Puis passer ce fichier à cime3. *)
    let cmd = "cime3 -tptp " ^ tmpname ^ " > " ^ resname in
    let rc = Sys.command cmd in
    (* Trouver unsatisfiable dans le fichier. *)
    let unsatisfiable = (rc = 0) && find resname in
    (try Sys.remove resname with _ -> ()) ;
    (try Sys.remove tmpname with _ -> ()) ;
    (* Si la réponse cime ne convient pas appeler zenon. *)
    (begin
    match unsatisfiable with
      | false -> Invoke.atp filename (statement, name) data loc oc ;
      | true ->
          (begin
	  (* Sinon, modifier le .v. *)
          Printf.fprintf oc
	    "Theorem %s : %s.\n Admitted. (* proved by Cime *)\n \n"
	    name statement ;
          end)
    end) ;
    incr file_nb ;
    incr cime_nb
    end)
;;
