(*
Fixpoint fib (n :nat) :=
  match n with
   | 0 => 1
   | S m =>
      match m with
        | 0 => 1
        | S p => fib p + fib m
     end
end.
*)


Require Import Bool.
Require Export Arith.

(** Global stuff for cheating. Verbatim. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).



Let myfib_order (foo : nat) (bar : nat) := magic_order foo bar.



(* Verbatim. *)
Let well_founded_myfib_order: (well_founded myfib_order).
apply magic_prove.
Defined.



Let decrease1 : forall n : nat, myfib_order n (S n).
Proof.
  apply magic_prove.
Defined.



Let decrease2 : forall n : nat, myfib_order n (S (S n)).
Proof.
  apply magic_prove.
Defined.



Let __T t := (forall a : nat, myfib_order a t -> nat).

Let myfun2_aux : forall args : nat, (__T args) -> nat :=
  fun (args : nat) =>
    match args return (__T args) -> nat with
    | n =>
       match n return (__T n) -> nat with
       | 0 => fun (myfun2_rec0 : __T 0) => 1
       | S m =>
	  fun (myfun2_rec0 : __T (S m)) =>
	   (match m return __T (S m) -> nat with
	     | 0 => fun (myfun2_rec1 : __T (S 0)) => 1
	     | S p =>
		fun (myfun2_rec1 : __T (S (S p))) =>
		  (myfun2_rec0 m (decrease1 m)) + (myfun2_rec1 p (decrease2 p))
	      end) myfun2_rec0
       end
    end.


Let myfib := Fix well_founded_myfib_order (fun x : nat => nat) myfun2_aux.
