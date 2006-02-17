(*  Copyright 2006 INRIA  *)
(*  $Id: invoke_cime.ml,v 1.2 2006-02-17 15:55:12 doligez Exp $  *)

open Expr;;

let cime_nb = ref 0;;
let file_nb = ref 0;;


let copy_file name oc =
  let ic = open_in_bin name in
  let buflen = 8192 in
  let buf = String.create buflen in
  let rec loop () =
    let n = input ic buf 0 buflen in
    if n = 0 then () else begin
      output oc buf 0 n;
      loop ();
    end;
  in
  loop ();
  close_in ic;
;;


let file_size name =
  let ic = open_in_bin name in
  let result = in_channel_length ic in
  close_in ic;
  result
;;


let ehead s =
String.sub s 1 (String.length s -1)
;;



(* la string s1 commence-t-elle par s2 ? *)

let rec bw s1 s2 =
 match String.length s2 with
 |0 -> true
 |_ ->
     if (String.get s1 0 == String.get s2 0)
     then bw (ehead s1)  (ehead s2)
     else false
;;


(* prefixe une chaine de caractere par c *)
let pref c s =
  String.concat c [String.create(0);s]
;;


(* prefixe une chaine de caractere par "v" *)
(* et la suffixe par un entier n donne *)
let createNewVar s n =
   let s1 = String.concat "v" [String.create(0);s]
   in String.concat s1 [String.create(0); string_of_int(n)]
;;


(* une expression peut elle se reecrire sous forme d'egalite ? *)
let rec isRewritable exp =
 match exp with
 |Enot (e1) -> isNotRewritable e1
 |Eand (e1, e2) -> isRewritable e1 && isRewritable e2
 |Eor (e1, e2) -> false
 |Eimply (e1, e2) -> false
 |Eequiv (e1, e2) -> false
 |Eall (sl, e) -> isRewritable e
 |Eex (sl, e) -> isRewritable e
 |Eapp("Is_true", [t])-> isRewritable t
 |Eapp("abst_egal", el)-> true
 |Eapp("equal", el) -> true
 |_ -> false
and isNotRewritable exp =
  match exp with
  |Enot (e1) -> isRewritable e1
  |Eand (e1, e2) -> false
  |Eor (e1, e2) -> isNotRewritable e1 || isNotRewritable e2
  |Eimply(e1, e2) -> isRewritable e1 && isNotRewritable e2
  |Eequiv (e1, e2) -> false
  |Eall (sl, e) -> isNotRewritable e
  |Eex (sl, e) -> isNotRewritable e
  |Eapp("equal", el) -> false
  |_ -> false
;;

(* une hyp peut elle se reecrire sous forme d'egalites ?*)
let rec isHypRewritable hyp =
  match hyp with
  |Hyp (expr) -> isRewritable expr
  |Def (s, sl, expr) -> isRewritable expr
;;

(* toutes les hypotheses d'une liste peuvent elles se reecrire sous forme d'egalite ? *)
let rec isHypListRewritable hl =
  match hl with
  |[] -> true
  |h::t ->  (isHypRewritable h) && (isHypListRewritable t)
;;


(* le but peut-il se reecrire sous forme d'egalite ? *)
let isButRewritable (hyps, goal) =
  (isRewritable goal) && (isHypListRewritable hyps)
;;

(* remplace dans e les variables liees par des variables prefixees par "v", *)
(* les variables libres par des constantes, *)
(* et les constantes par des constantes par des constantes prefixees par C  *)
let rec modifyH l expr =
 match expr with
 |Enot (e1) -> Enot (modifyH l e1)
 |Eand (e1, e2) -> Eand (modifyH l e1, modifyH l e2)
 |Eor (e1, e2) -> Eor(modifyH l e1, modifyH l e2)
 |Eimply(e1, e2) -> Eimply(modifyH l e1, modifyH l e2)
 |Eequiv (e1, e2) -> Eequiv(modifyH l e1, modifyH l e2)
 |Eall (sl, e) -> modifyH (List.append l sl) e
 |Eex (sl, e) -> modifyH (List.append l sl) e
 |Evar(s) -> if List.mem s l then Evar(pref "V" s) else Eapp(pref "c" s, [])
 |Eapp(s, [])->  Eapp(pref "c" s, [])
 |Eapp(s, el) -> Eapp(s, List.map (modifyH l) el)
 |Etrue -> Etrue
 |Efalse -> Efalse
;;


(* remplace dans expr les variables par des constantes, *)
(* et prefixe le nom des constantes par c *)
let rec modifyG expr =
 match expr with
 |Enot (e1) -> Enot (modifyG e1)
 |Eand (e1, e2) -> Eand (modifyG e1, modifyG e2)
 |Eor (e1, e2) -> Eor(modifyG e1, modifyG e2)
 |Eimply(e1, e2) -> Eimply(modifyG  e1, modifyG e2)
 |Eequiv (e1, e2) -> Eequiv(modifyG e1, modifyG e2)
 |Eall (sl, e) -> modifyG  e
 |Eex (sl, e) -> modifyG  e
 |Evar(s) -> Eapp(pref "c" s, [])
 |Eapp(s, [])->  Eapp(pref "c" s, [])
 |Eapp(s, el) -> Eapp(s, List.map modifyG el)
 |Etrue -> Etrue
 |Efalse -> Efalse
;;

(* modifie les hypotheses, quelle que soit leur forme *)
let modifyHyps hyps =
  match hyps with
  |Hyp(e)-> modifyH [] e
  |Def(s, sl, e) -> modifyH [] e
;;

let rec printExpr o e =
  match e with
  | Evar(s) ->  Format.fprintf o "@[%s@]" s
  | Eapp(s, [])-> Format.fprintf o "@[%s@]" s
  | Eapp("Is_true", el)-> Format.fprintf o "@[%a@]" printExprList el
  | Eapp("abst_egal", h::el)-> Format.fprintf o "@[%a = %a @]" printExpr h printExprList el

  | Eapp(s, el)-> Format.fprintf o "@[%s(%a)@]" s printExprList el
  | Enot(e)-> Format.fprintf o "~(@[@;%a@])" printExpr e
  | Eand(e1, e2)-> Format.fprintf o "@[%a &@;<1 2>%a@]" printExpr e1 printExpr e2
  |_ ->  assert false

and printExprList o el=
  match el with
  |[] -> Format.print_string ""
  |h::[] -> printExpr o h
  |h::t -> printExpr o h; Format.fprintf o "@[, @]" ; printExprList o t

;;

let printNegatedConjecture o e =
  match e with
  | Eapp("Is_true", [Eapp("abst_egal", h::[t])])-> Format.fprintf o "@[%a != %a @]" printExpr h  printExpr t
  |_ -> assert false
;;

let printHyp o hypName hyp =

  Format.fprintf o "@[%s(%s, %s, %a). \n @]" "cnf" ("h"^hypName) "hypothesis" printExpr hyp;;

let rec printHypList o hypName hypList  =
  match hypList with
  |[] ->
      Format.print_string ""
  |Eand(e1, e2)::t  ->
      printHyp o (hypName^(string_of_int !cime_nb)) e1; incr cime_nb;
      printHyp o (hypName^(string_of_int !cime_nb)) e2; incr cime_nb;
      printHypList o hypName t
  |h::[] ->
      printHyp o (hypName^(string_of_int !cime_nb)) h; incr cime_nb
  |h::t ->
      printHyp o (hypName^(string_of_int !cime_nb)) h; incr cime_nb;
      Format.fprintf o "@[%s @]" "\n"  ; incr cime_nb; printHypList o hypName t ;
;;

let printGoal o goalName goal =
  Format.fprintf o "@[%s(%s, %s, %a). \n@]" "cnf" ("g"^goalName) "negated_conjecture" printNegatedConjecture goal;;

let printPhrase o ph =
  match ph with
  |Hyp e -> printExpr o e
  |Def (s, sl, e) -> printExpr o e
;;

(* trouver le mot "unsatisfiable" dans un fichier *)
(* inf : fileName (string) *)
let find inf =
  try
    let ic = open_in_bin inf in
    let lexbuf = Lexing.from_channel ic in
    Parser_sat.cimefile Lexer_sat.cimetoken lexbuf
  with _ -> false
;;

(*fonction principale *)

let cime filename data loc statement name oc=
  let lexbuf = Lexing.from_string data in
  let (goal, hyps) = Parser_coq.coqfile Lexer_coq.coqtoken lexbuf in

  let (goal2, hyps2)= (modifyG goal, List.map modifyHyps hyps)
  in
  let b=(isRewritable goal)&&(isHypListRewritable hyps) in

  if (not b)
  then
     Invoke.zenon_loc filename (statement, name) data loc oc

  else
    begin

      let (tmpname, f) = Filename.open_temp_file "zvtov" ".p" in
      let resname = Filename.temp_file "zvtov" ".res" in

      let fmt = Format.formatter_of_out_channel f in

      printHypList fmt name hyps2 ;
      printGoal fmt name goal2 ;

      Format.pp_print_flush fmt ();
      close_out f;
      (* puis passer ce fichier a cime3*)

      let cmd =
        "cime3 -tptp "^tmpname^" > "^resname in
      let rc = Sys.command cmd in
      (* trouver unsatisfiable dans le fichier *)
      let unsatisfiable = (rc = 0) && find resname in
      (try Sys.remove resname with _ -> ());
      (try Sys.remove tmpname with _ -> ());

(* si la reponse cime ne convient pas appeler zenon *)
           begin
             match unsatisfiable with
             |false -> Invoke.atp filename (statement, name) data loc oc;
(*Sinon, modifier le .v *)
             |true ->
                 begin
                   Printf.fprintf oc "Theorem %s : %s.\n Admitted. (* proved by Cime *)\n \n" name statement ;
                 end;
           end;

      incr file_nb;
      incr cime_nb;
     end
;;
