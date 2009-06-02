(*
let rec myfun (a, b) =
  if a = 0 then b
  else
    if a = 1 then myfun ((a - 1), b)
    else myfun ((a - 2), b)
*)

Require Import Bool.
Require Export Arith.

(** Global stuff for cheating. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let myfun_order
  (* Takes 2 arguments to compare. Each argument is the tuple of our function
     arguments. *)
  (foo : (prod nat nat))
  (bar : (prod nat nat)) :=
  magic_order foo bar.


(* Verbatim. *)
Let well_founded_myfun_order: (well_founded myfun_order).
apply magic_prove.
Defined.



Let decrease1 :
  forall (x y : nat),  (* As many arguments that the recursive function. *)
  myfun_order
   ((x - 1), y)     (* Tuple of all the recursive call. *)
   (x, y).          (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.


Let decrease2 :
  forall (x y : nat),  (* As many arguments that the recursive function. *)
  myfun_order
    ((x - 2), y)       (* Tuple if all the recursive call. *)
    (x, y).            (* Tuple of all the initial function arguments. *)
Proof.
  apply magic_prove.
Defined.



Let __T t := (forall a : nat * nat, myfun_order a t -> nat).

Let myfun_aux : forall args : nat * nat, (__T args) -> nat :=
  fun (args : nat * nat) =>
    match args return (__T args) -> nat with
     | (a, b) =>
        let __T x := __T x in
        fun (myfun_rec : __T (a, b)) =>
	  if eq_nat_dec a 0 then b
	  else
	  if eq_nat_dec a 1 then
	    myfun_rec (a - 1, b) (decrease1 a b)
	  else
	     myfun_rec (a - 2, b) (decrease2 a b)
    end.


Let myfun := Fix well_founded_myfun_order
  (fun (args : (prod nat nat)) => nat) myfun_aux.
