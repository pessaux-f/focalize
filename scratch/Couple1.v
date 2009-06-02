Require Import Bool.
Require Export Arith.

(** Global stuff foe cheating. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let myfun2_order
  (* Takes 2 arguments to compare. Each argument is the tuple of our function
     arguments. *)
  (foo : (prod nat nat))
  (bar : (prod nat nat)) :=
  magic_order foo bar.


(* Verbatim. *)
Let well_founded_myfun2_order: (well_founded myfun2_order).
apply magic_prove.
Defined.


Let decrease1 :
  forall (x y : nat),  (* As many arguments that the recursive function. *)
  x <> 0 -> x = 1 ->   (* Condition string coming from the tests. *)
  myfun2_order
   ((x - 1), y)     (* Tuple if all the recursive call. *)
   (x, y).          (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.


Let decrease2 :
  forall (c d : nat),  (* As many arguments that the recursive function. *)
  c <> 0 -> c <> 1 ->  (* Condition string coming from the tests. *)
  myfun2_order
    ((c - 2), d)       (* Tuple if all the recursive call. *)
    (c, d).            (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.


Let myfun2_aux :
  forall x : nat * nat,    (* Tuple of all the function arguments. *)
  (* Type du truc qui va servir à faire la fermeture. *)
  (forall foo : nat * nat,  (* Tuple des args de la fct récursive. *)
     myfun2_order foo x -> nat (* Type de retour de la fct rec. *)) ->
    nat (* Type de retour de la fct rec. *) :=
  fun (bla : nat * nat) =>  (* Tuple de tous les args de la fct. *)
  (* On matche le tuple des args initiaux de la fct pour retrouver chacun
     de ses arguments non-tuplifié. *)
  match bla
  return
   (* C'est le meme truc que 8 lignes au dessus. *)
   (forall y : nat * nat, myfun2_order y bla -> nat) -> nat with
  | (a, b) =>
    (* En quelque sorte, la définition de notre truc rec qui correspond à
       l'appel récursif. *)
    fun (H : (forall gee : nat * nat, myfun2_order gee (a, b) -> nat)) =>
    match eq_nat_dec a 0 with
    | left _ => b
    | right n =>
      match eq_nat_dec a 1 with
      | left e =>
         H
          (a - 1, b) (decrease1 a b n e)  (* Le lemme ne prend plus un tuple. *)
      | right n0 =>
         H
          (a - 2, b) (decrease2 a b n n0)
      end
    end
  end.

Let myfun2 := Fix well_founded_myfun2_order
  (fun (args : (prod nat nat)) => nat) myfun2_aux.




(* ************************************************************* *)
(* Alternative ................................................. *)
(* ************************************************************* *)

Let decrease11 :
  forall (x y : nat),  (* As many arguments that the recursive function. *)
  myfun2_order
   ((x - 1), y)     (* Tuple of all the recursive call. *)
   (x, y).          (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.


Let decrease22 :
  forall (x y : nat),  (* As many arguments that the recursive function. *)
  myfun2_order
    ((x - 2), y)       (* Tuple if all the recursive call. *)
    (x, y).            (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.



Let __T t := (forall a : nat * nat, myfun2_order a t -> nat).

Let myfun3_aux :
  forall args : nat * nat,    (* Tuple of all the function arguments. *)
  (* Type du truc qui va servir à faire la fermeture. *)
  __T args ->
    nat (* Type de retour de la fct rec. *) :=
  fun (args : nat * nat) =>  (* Tuple de tous les args de la fct. *)
  (* On matche le tuple des args initiaux de la fct pour retrouver chacun
     de ses arguments non-tuplifié. *)
  match args return (__T args) -> nat with
   | (a, b) =>
     (* En quelque sorte, la définition de notre truc rec qui correspond à
	l'appel récursif. *)
       fun (myfun3_rec : (__T (a, b))) =>
	 if eq_nat_dec a 0 then b
	 else
	   if eq_nat_dec a 1 then
	     myfun3_rec (a - 1, b) (decrease11 a b)
	   else
	     myfun3_rec (a - 2, b) (decrease22 a b)
   end.


Let myfun3 := Fix well_founded_myfun2_order
  (fun (args : (prod nat nat)) => nat) myfun3_aux.
