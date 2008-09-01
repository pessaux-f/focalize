(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Virgile Prevosto                                         *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: generic_proof_cases.v,v 1.1 2008-09-01 10:30:12 pessaux Exp $ *)


Inductive list_Prop : Type :=
  | Null : list_Prop
  | Chce : Prop -> list_Prop -> list_Prop.

Inductive In_list_Prop : Prop -> list_Prop -> Prop :=
  | Here : forall (P : Prop) (l : list_Prop), In_list_Prop P (Chce P l)
  | Deeper :
      forall (P0 P : Prop) (l : list_Prop),
      In_list_Prop P l -> In_list_Prop P (Chce P0 l). 

Hint Immediate Here Deeper: in_list.

(* Note: Null can not be exhaustive *)
Inductive exhaustive : list_Prop -> Prop :=
  | Chce_left : forall (P : Prop) (l : list_Prop), P -> exhaustive (Chce P l)
  | Chce_right :
      forall (P : Prop) (l : list_Prop),
      exhaustive l -> exhaustive (Chce P l). 

Hint Immediate Chce_left Chce_right: lst_Prop_dec.

Lemma proof_lst_prop :
 forall (P : Prop) (l : list_Prop),
 (forall case_i : Prop, In_list_Prop case_i l -> case_i -> P) ->
 exhaustive l -> P.
intros P l.
induction  l as [| P0 l Hrecl].
intros H Habs; inversion Habs.

intros Hcases Hexh; inversion Hexh.
apply (Hcases P0).
auto with in_list.
exact H0.
apply Hrecl.
intros case_i Hin Hcase.
apply (Hcases case_i).
auto with in_list.
trivial.
trivial.
Qed.

Lemma lst2or :
 forall (P : Prop) (l : list_Prop),
 exhaustive (Chce P l) -> P \/ exhaustive l.
Proof.
  intros P l Hexh. 
  inversion Hexh; auto.
Qed.

Lemma or2lst :
 forall (P : Prop) (l : list_Prop),
 P \/ exhaustive l -> exhaustive (Chce P l).
Proof.
  intros P l Hor.
  inversion Hor; auto with lst_Prop_dec.
Qed.

Lemma Null_not_exh : ~ exhaustive Null.
unfold not in |- *; intros Hexh; inversion Hexh.
Qed.

Fixpoint lst2Prop (l : list_Prop) : Prop :=
  match l with
  | Null => False
  | Chce P l => P \/ lst2Prop l
  end.


Lemma or2exh : forall l : list_Prop, lst2Prop l -> exhaustive l.
intros l; induction  l as [| P l Hrecl].
compute in |- *.
simple induction 1.

intros Hor.
apply or2lst.
unfold lst2Prop in Hor.
compute in |- *.
case Hor.
auto.
fold (lst2Prop l) in |- *.
auto.
Qed.

Theorem proof_by_cases :
 forall (P : Prop) (l : list_Prop),
 (forall case_i : Prop, In_list_Prop case_i l -> case_i -> P) ->
 lst2Prop l -> P.

intros P l Hcases Hexh.
apply (proof_lst_prop P l); trivial.
apply or2exh; trivial.
Qed.


Inductive list_imp_Prop (P : Prop) : list_Prop -> Prop :=
  | N : list_imp_Prop P Null
  | Case :
      forall (H : Prop) (l : list_Prop),
      (H -> P) -> list_imp_Prop P l -> list_imp_Prop P (Chce H l).

Lemma implication :
 forall (P : Prop) (l : list_Prop),
 list_imp_Prop P l ->
 forall case_i : Prop, In_list_Prop case_i l -> case_i -> P.

intros P l; induction  l as [| P0 l Hrecl].
intros H case_i Habs; inversion Habs.

intros Hl case_i Hc H. 

inversion Hc.

inversion_clear Hl.
rewrite <- H2 in H4.
auto.

inversion_clear Hl.
eauto.
Qed.

Theorem generic_cases :
 forall (P : Prop) (l : list_Prop), list_imp_Prop P l -> lst2Prop l -> P.
intros P l Himp Hexh.
apply (proof_by_cases P l).
exact (implication _ _ Himp).
trivial.
Qed.