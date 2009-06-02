(*
let rec myfun x :=
  if x = 0 then 0
  else myfun (x -1)
;;
*)

Require Import Bool.
Require Export Arith.


(** Global stuff for cheating. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let myfun_order (foo : nat) (bar : nat) := magic_order foo bar.


(** Verbatim. *)
Let well_founded_myfun_order: (well_founded myfun_order).
apply magic_prove.
Qed.



Let myfun_decrease1_lemma : forall y : nat, myfun_order (y - 1) y.
apply magic_prove.
Qed.


Let __T t := (forall a : nat, (myfun_order a t) -> nat).

Let myfun_aux : forall args : nat, (__T args) -> nat  :=
  fun (args : nat) =>
    match args return (__T args) -> nat with
     | x =>
	if eq_nat_dec x 0 then (fun myfun_rec : __T x => 0)
	else
	fun (myfun_rec : __T x) =>
         myfun_rec (x - 1) (myfun_decrease1_lemma x)
  end.


Let myfun :=
  Fix well_founded_myfun_order
     (fun (x : nat) => nat) myfun_aux.
