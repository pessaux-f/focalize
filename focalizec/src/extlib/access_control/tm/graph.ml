(* $Id: graph.ml,v 1.2 2009-05-16 19:41:35 weis Exp $ *)

type node_bullet = {
  b_subject : string;
  b_depth : int;
  mutable b_children : node_bullet list;
}
;;

(* Type of root nodes. *)
type node_star = {
  n_subject : string;
  n_object : string;
  n_access : string;
  n_depth : int;
  mutable n_children : node_bullet list;
}
;;

type graph = node_star list;;

let pierre2 = {
  b_subject = "Pierre";
  b_depth = 6;
  b_children = [];
}
;;

let marie = {
  b_subject = "Marie";
  b_depth = 3;
  b_children = [];
}
;;

let bob2 = {
  b_subject = "bob";
  b_depth = 7;
  b_children = [pierre2; marie];
}
;;

let pierre1 = {
  n_subject = "Pierre";
  n_object = "f1";
  n_access = "x";
  n_depth = 8;
  n_children = [marie; bob2];
}
;;

let bob1 = {
  n_subject = "Bob";
  n_object = "f1";
  n_access = "x";
  n_depth = 5;
  n_children = [marie];
}
;;


let listA = [("Pierre","f1","x",8);("Bob","f1","x",5)];;
let listD1 = [("Pierre","f1","x","Marie",3);("Pierre","f1","x","Bob",7)];;
let listD2 = [("Bob","f1","x","Marie",3);("Bob","f1","x","Pierre",6)];;

let equal_star n s d = n.n_subject = s && n.n_depth = d;;

let rec isInList2 list s d =
  match list with
  | [] -> false
  | hd :: tl ->
    if hd.b_subject = s && hd.b_depth = d then true
    else isInList2 tl s d
;;

let rec isInList list s o a d =
  match list with
  | [] -> false
  | hd :: tl ->
    if hd.n_object = o && hd.n_access = a then
      if equal_star hd s d then true else
      if isInList2 hd.n_children s d then true else
      isInList tl s o a d else
    isInList tl s o a d
;;

(*A(o,a) rend {(s,o,a,d)} : avec o et a constant dans l'ens*)
(*D(s',o,a) rend {(s',o,a,s,d)} : avec s', o et a constant dans l'ens*)
let rec delegate_a a =
  match a with
  | [] -> []
  | (s, o, a, d) :: tl ->
    let ns = {
      n_subject = s;
      n_object = o;
      n_access = a;
      n_depth = d;
      n_children = [];
    } in
    ns :: delegate_a tl
;;

let rec nouveau_noeud2 list s d =
  match list with
  | [] ->
    let vide = {
      b_subject = "empty_vide";
      b_depth = 0;
      b_children = [];
     } in
     vide
   | hd :: tl ->
     if hd.b_depth >= d then
       (begin
          if hd.b_subject = s then hd
          else nouveau_noeud2 tl s d
        end)
     else
       let vide = {
         b_subject = "empty_vide";
         b_depth = 0;
         b_children = [];
       } in
       vide
;;

(*Cree le sommet si il n'existe pas deja, sinon renvoie le sommet en question*)
let rec nouveau_noeud graph s o a d =
  match graph with
  | [] ->
    let r = { b_subject = s; b_depth = d; b_children = [] } in
    r
  | hd :: tl ->
    if hd.n_depth >= d && hd.n_object = o && hd.n_access = a then
      match hd.n_children with
      | [] -> nouveau_noeud tl s o a d
      | hd2 :: tl2 ->
        (begin
           if hd2.b_subject = s && hd2.b_depth = d then hd2 else
           let n_aux = nouveau_noeud2 tl2 s d in
           if n_aux.b_subject = "empty_vide" && n_aux.b_depth = 0 then
             nouveau_noeud tl s o a d
           else n_aux
         end)
    else nouveau_noeud tl s o a d
;;

let rec ajouter_fils s1 s2 d list =
  match list with
  | []-> false
  | hd :: tl ->
    if hd.b_depth >= d then
      (begin
         if hd.b_subject = s1 then
           (begin
              let n_aux = nouveau_noeud2 list s2 d in
              hd.b_children <- n_aux :: hd.b_children;
              true
            end)
         else ajouter_fils s1 s2 d tl
       end)
    else false
;;

(*Nous posons que la delegation est de la forme sujet_d, objet, access, sujet_r, depth*)
let rec ajouter s1 o a s2 d graph =
  match graph with
  | [] -> failwith "Graphe vide"
  | hd :: tl ->
    if hd.n_depth >= d && hd.n_object = o && hd.n_access = a then
      (if hd.n_subject = s1 then
         (let n_aux = (nouveau_noeud graph s2 o a d) in
          hd.n_children <- n_aux::hd.n_children)
       else
         (if not (ajouter_fils s1 s2 d hd.n_children) then
            ajouter s1 o a s2 d tl))
    else ajouter s1 o a s2 d tl
;;

let rec delegate_d list_a d =
  match d with
  | [] -> list_a
  | (s1, o, a, s2, d) :: tl ->
    ajouter s1 o a s2 d list_a;
    delegate_d list_a tl
;;

let rec trier d =
  match d with
  | [] -> []
  | (s', o, a, s, d) :: t ->
    let is_less x =
      match x with
      | (s2',o2,a2,s2,d2) -> d2 > d in
    let left, right = List.partition is_less t in
    trier left @ [(s', o, a, s, d)] @ trier right
;;

let create_g a d =
  match a with
  | [] -> []
  | hd :: tl ->
    let list_a = delegate_a a in
    let list_d = trier d in
    delegate_d list_a list_d
;;

(*
let rec create_f_a graph =
  match graph with
  | [] -> []
  | hd :: tl ->
    let list_a =
      (hd.n_subject, hd.n_object, hd.n_access, hd.n_depth) in
    list_a :: create_f_a tl
;;
*)
(*Version compilable*)
(*
let rec create_f_d2 s o a list =
   match list with
   | [] -> []
   | (s_aux, d) :: tl ->
     let list_aux = (s, o, a, s_aux, d) in
     list_aux :: create_f_d2 s o a tl
;;
*)
(*Essais de modif du comportement*)
(*
let rec create_f_d2 s o a list_read list_write =
  match list_read with
  | [] -> []
  | (s_aux, d) :: tl ->
    list_write <- (s, o, a, s_aux, d) :: create_f_d2 s o a tl list_write
;;
*)

listD1
;;
trier listD1
;;
let list_a = delegate_a listA in
delegate_d list_a listD1
;;
create_g listA listD1
;;
let list_a = create_g listA listD1 in
isInList list_a "Marie" "f1" "x" 3
;;
