(*
  let rec length (l) =
    match l with
     | Nil -> 0
     | Cons (h, q) -> 1 + length (q)
*)

Require Import Bool.
Require Export Arith.


Inductive list (alpha : Set)  : Set :=
  | Nil : list alpha
  | Cons : alpha -> (list alpha) -> (list alpha).


(** Global stuff for cheating. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let myfun_order (alpha : Set) (foo : (list alpha)) (bar : (list alpha)) :=
  magic_order foo bar.


(** Verbatim. *)
Let well_founded_myfun_order: forall alpha : Set,
  (well_founded (myfun_order alpha)).
apply magic_prove.
Qed.


Let decrease : forall alpha : Set, forall h : alpha, forall q : (list alpha),
   myfun_order alpha q (Cons alpha h q).
Proof.
  apply magic_prove.
Defined.


Let __T (alpha : Set) t := (forall a : (list alpha),
  myfun_order alpha a t -> nat).


Let length_aux : forall alpha : Set, forall args : (list alpha), 
  (__T alpha args) -> nat :=
  fun (alpha : Set) (args : (list alpha)) =>
    match args return (__T alpha args) -> nat with
     | Nil =>
        fun _ (* : (__T alpha (Nil alpha)) *) => 0
     | Cons h q =>
        fun (length_rec : __T alpha (Cons alpha h q)) =>
          1 + (length_rec alpha q (decrease alpha q))
    end.
