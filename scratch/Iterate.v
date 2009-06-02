(*
  let choose (x in bool, y in bool) = x = y ;;

  let rec iterate (x in bool, n in int) in bool =
    if n = 0 then true
    else choose (x, iterate (x, (n -1)))
;;
*)


Require Import Bool.
Require Export Arith.

(** Global stuff for cheating. Verbatim. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let choose (x : bool) (y : bool) := 
  match x with
   | true =>
       match y with
        | true => true
        | false => false
       end
   | false =>
       match y with
        | true => false
        | false => true
       end
  end.



Let myfun_order (foo : (bool * nat)) (bar : (bool * nat)) :=
  magic_order foo bar.



(* Verbatim. *)
Let well_founded_myfun_order: (well_founded myfun_order).
apply magic_prove.
Defined.


Let decrease1 : forall x : bool, forall n : nat,
  myfun_order (x, (n - 1)) (x, n).
Proof.
  apply magic_prove.
Defined.


Let __T t := (forall a : bool * nat, myfun_order a t -> bool).


Let myfun_aux : forall args : bool * nat, (__T args) -> bool :=
  fun (args : bool * nat) =>
    match args return (__T args) -> bool with
     | (x, y) =>
        fun (myfun_rec : __T (x, y)) =>
          if eq_nat_dec 0 0 then true
          else
            choose x ((myfun_rec (x, (y - 1))) (decrease1 x y))
    end.

Let myfun := Fix well_founded_myfun_order
  (fun (args : (prod bool nat)) => bool) myfun_aux.
