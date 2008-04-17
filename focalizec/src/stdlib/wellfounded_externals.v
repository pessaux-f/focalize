Require Import Bool.
Require coq_builtins.

Definition wellfounded (A:Set) := fun (r: A -> A -> bool) =>
  well_founded (fun (x y: A) => Is_true (r x y)).
(* Implicit Arguments wellfounded [A]. *)

Require Import ZArith.
Require Import Zwf.
Open Scope Z_scope.

Section ZwfLtCompat.
  Open Scope Z_scope.
  Variable A:Type.
  Variable f:A->Z.
  Variable R : A -> A -> Prop.
  Variable c:Z.

  Hypothesis H_compat : forall (x y:A), R x y -> Zwf c (f x) (f y).

  Let g (z:Z) := Zabs_nat (z-c).

  Theorem well_founded_Zwf_compat: well_founded R.
  Proof.
    red in |- *; intros.
    assert (forall (n:nat) (a:A), (g (f a) < n)%nat \/ f a < c -> Acc R a).
    clear a; simple induction n; intros.
  (*n= 0*)
    case H; intros.
    case (lt_n_O (g (f a))); auto.
    apply Acc_intro; intros.
    assert (H2:=H_compat _ _ H1);
    unfold Zwf in H2;
    assert False;
    omega || contradiction.
  (*inductive case*)
    case H0; clear H0; intro; auto.
    apply Acc_intro; intros.
    apply H.
    assert (H2:=H_compat _ _ H1);
    unfold Zwf in H2.
    case (Zle_or_lt c (f y)); intro; auto with zarith.
    left.
    red in H0.
    apply lt_le_trans with (g (f a)); auto with arith.
    unfold g in |- *.
    apply Zabs.Zabs_nat_lt; omega.
    apply (H (S (g(f a)))); auto.
  Qed.

End ZwfLtCompat.
