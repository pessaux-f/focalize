(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Virgile Prevosto                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: coq_builtins.v,v 1.6 2008-09-05 14:10:07 pessaux Exp $ *)

Require Import Bool.
Require Export ZArith.
Require Export String.




(* *********************************************************************** *)
                 (* (Previously in Dec_equality.v) *)
(* *********************************************************************** *)
(* *********************************************************************** *)
(** Equality.                                                              *)

(* Beware we use Coq [bool] type because we map FoCaL's [bool] type on it. *)
Section eq.
Variable beq___A : Set.

Parameter beq_ : beq___A -> beq___A -> bool.

Axiom base_eq_refl : forall x : beq___A, Is_true (beq_ x x).

Axiom base_eq_sym :
  forall x y : beq___A, Is_true (beq_ x y) -> Is_true (beq_ y x).

Axiom
  base_eq_trans :
    forall x y z : beq___A,
    Is_true (beq_ x y) -> Is_true (beq_ y z) -> Is_true (beq_ x z).
End eq.
Notation beq := (beq_ _) (only parsing).

Theorem EQ_base_eq :
 forall (A : Set) (x y : A), x = y -> Is_true (beq_ _ x y).
Proof.
  intros A x y Heq; rewrite Heq; exact (base_eq_refl A y).
Qed.

Definition __g_base_eq_ (__var_a : Set) (x : __var_a) (y : __var_a) :=
   beq_ __var_a x y.
Notation  bi__base_eq := __g_base_eq_.

(* In case the equality is decidable, it is extensionaly equal to  beq *)
Axiom
  decidable :
    forall (A : Set) (x y : A),
    {x = y} + {x <> y} -> Is_true (beq_ _ x y) -> x = y.

Hint Resolve base_eq_refl base_eq_sym base_eq_trans EQ_base_eq decidable:
  FoCeq.

(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
             (* (Previously in generic_proof_cases.v) *)
(* *********************************************************************** *)
(* *********************************************************************** *)
Inductive list_Prop : Type :=
  | Null : list_Prop
  | Chce : Prop -> list_Prop -> list_Prop.

Inductive In_list_Prop : Prop -> list_Prop -> Prop :=
  | Here : forall (P : Prop) (l : list_Prop), In_list_Prop P (Chce P l)
  | Deeper :
      forall (P0 P : Prop) (l : list_Prop),
      In_list_Prop P l -> In_list_Prop P (Chce P0 l). 

Hint Immediate Here Deeper: in_list.

(* Note: Null can not be exhaustive. *)
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

(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
                   (* (Previously in Foc_init.v) *)
(* *********************************************************************** *)
(* *********************************************************************** *)

(* ********** Begining of verbatim copy from the old compiler. ************ *)
Definition app_Prop (A B : Prop) (f : A -> B) (x : A) := f x.
(* app_Prop is used in the proof trees, since type inference in the Refine *
 * tactic does not work well with terms such as ((?::A->B)(?::A))          *)
 
Definition all_elim (A : Set) (P : A -> Prop) (H : forall x : A, P x)
  (x : A) := H x.
(* same thing with the dependant case of 'all' *)

(* wonder why this is not standard ?*)
Lemma IsTrue_eq_false : forall x : bool, ~ Is_true x -> x = false.
Proof.
intros x; case x; simpl in |- *; trivial.
intros Habs; absurd False; auto.
Defined.

Lemma IsTrue_eq_false2 : forall x : bool, x = false -> ~ Is_true x.
Proof.
intros x; case x; simpl in |- *; auto with bool.
Defined.

Ltac Replacetrue :=
  match goal with
  | id:(?X1 = true :>bool) |- _ =>
      generalize (Is_true_eq_true2 X1 id); clear id; intros id
  | id:(true = ?X1 :>bool) |- _ =>
      generalize (Is_true_eq_true2 X1 (sym_eq id)); clear id; intros id
  end.

Ltac Replacefalse :=
  match goal with
  | id:(?X1 = false :>bool) |- _ =>
      generalize (IsTrue_eq_false2 X1 id); clear id; intros id
  | id:(false = ?X1 :>bool) |- _ =>
      generalize (IsTrue_eq_false2 X1 (sym_eq id)); clear id; intros id
  end.

Ltac BoolElimNamed b HBtrue HBfalse :=
  elim (Sumbool.sumbool_of_bool b);
   [ intros HBtrue; rewrite HBtrue; Replacetrue
   | intros HBfalse; rewrite HBfalse; Replacefalse ].

(* NdV: pb avec v8. *)
(* Tactic Definition BoolElim b := (BoolElimNamed b HBtrue HBfalse). *)

Ltac IsTrueRewHyp HIs_true H := rewrite (Is_true_eq_true _ HIs_true) in H.

Ltac NotIsTrueRewHyp HIs_false H :=
  rewrite (IsTrue_eq_false _ HIs_false) in H.

Ltac IsTrueRew H := rewrite (Is_true_eq_true _ H).

Ltac NotIsTrueRew H := rewrite (IsTrue_eq_false _ H).

Ltac ReplaceIn e1 e2 H :=
  generalize H; clear H; replace e1 with e2; [ intros H | idtac ].

Lemma simpl_contr : Is_true false -> False.
Proof.
auto with bool.
Defined.

Hint Resolve simpl_contr: my_bool.

Lemma ContrIstrueFalse : forall A : Prop, Is_true false -> A.
Proof.
intros A.
cut (False -> A).
intros H H0.
apply H.
apply simpl_contr.
apply H0.

intros H.
elim H.

Defined.

Lemma IstrueTrue : Is_true true.
Proof.
exact I.
Defined.

Hint Resolve IstrueTrue: my_bool.

(* a few lemmas very useful when dealing with bool *)

Lemma andb_if :
 forall a b : bool, Is_true (a && b) -> Is_true (if a then b else false).
Proof.
intros a b.
case a; case b; auto with bool.
Defined.

Lemma if_andb :
 forall a b : bool, Is_true (if a then b else false) -> Is_true (a && b).
Proof.
intros a b.
case a; case b; auto with bool.
Defined.

Hint Resolve if_andb andb_if: my_bool.

(* Lemmas about andb *)

Lemma andb_elim1 : forall a b : bool, Is_true (a && b) -> Is_true a.
Proof.
intros a b.
case a; simpl in |- *; trivial.
Defined.

Lemma andb_elim2 : forall a b : bool, Is_true (a && b) -> Is_true b.
Proof.
intros a b.
case b; simpl in |- *; trivial.
case a; simpl in |- *; trivial.
Defined.

Lemma andb_intro :
 forall a b : bool, Is_true a -> Is_true b -> Is_true (a && b).
Proof.
intros a.
case a; simpl in |- *; trivial.
Defined.

Hint Resolve andb_elim1 andb_elim2 andb_intro: my_bool.

Lemma notb_intro : forall a : bool, Is_true (negb a) -> ~ Is_true a.

intros a; case a; simpl in |- *; auto.
Qed.

Lemma notb_elim : forall a : bool, ~ Is_true a -> Is_true (negb a).
intros a; case a; simpl in |- *; auto.
Qed.

Hint Resolve notb_intro notb_elim: my_bool.
(* Since a lot of lemmas begins with Is_true, it should be declared as opaque *
 * so that Auto can unify goals and theorems *)
(* Rq: not so sure this is still needed if we construct explicit terms as
proof *)
Opaque Is_true.

(* conversion of decidable functions into booleans * 
 * (for instance eq. on setoids). A is implicit *)
Set Implicit Arguments.
Unset Strict Implicit.
Definition dec_to_bool (A : Prop) (dec : {A} + {~ A}) :=
  if dec return bool then true else false.

Theorem IsTrue_dec :
 forall (A : Prop) (dec : {A} + {~ A}), Is_true (dec_to_bool dec) -> A.
intros A dec; elim dec.
Transparent Is_true.
compute in |- *.
intros H; trivial.
compute in |- *.
intros H H1; contradiction.
Qed.

Theorem dec_IsTrue :
 forall (A : Prop) (dec : {A} + {~ A}), A -> Is_true (dec_to_bool dec).
intros A dec Hal.
elim dec; compute in |- *; auto.
Qed.

Set Strict Implicit.
Unset Implicit Arguments.

(* A simple wf relation on Z *)
Require Import ZArith.
Definition Z_lt_wf (x y : Z) := Zabs_nat x < Zabs_nat y.
Lemma wf_Z_lt_wf : well_founded Z_lt_wf.
Require Import Wf_nat.
unfold Z_lt_wf in |- *.
cut (well_founded (ltof _ Zabs_nat)).
intros H.
simpl in H.
unfold ltof in H.
trivial.
exact (well_founded_ltof Z Zabs_nat).
Qed.

(* *********** End of verbatim copy from the old compiler. ************** *)



(** The weak proof !!! Give it a Prop, and abracadabra ... it's proved ! *)
Axiom magic_prove : forall A : Prop, A.

Axiom __magic_order__ : forall A : Set, A -> A -> Prop.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation magic_order := (__magic_order__ _) (only parsing).

(** Exceptions have all the properties we can imagine. That's assumed ! *)
Axiom __foc_bottom__ : forall A : Set, A.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation bottom := (__foc_bottom__ _) (only parsing).

(* Junky code to have one representative of the string__t type. *)
Variable ___a_string : string.



(** Definition of the "raise" function.
    Note that we use type [string]. This is right since in basics.foc, the
    mapping of FoCaL's string type will be internally done on Coq's [string]. *)
Definition __g_foc_error_ (a : Set) (s : string) : a := __foc_bottom__ _.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation foc_error := (__g_foc_error_ _) (only parsing).

Ltac prove_term_obl term_obl := intuition.

(* "bi" for "built-in". Don't search anymore... *)
Inductive bi__unit : Set :=
  | Void: bi__unit.


(** Alias the type of propositions [prop__t] to Coq [Prop]. It is impossible
    to make this alias in basics.foc since "Prop" is a keyword of FoCaLize.
    Hence trying to define it in FoCaLize leads to a crude syntax error.
    However, since "Prop" is not available for OCaml programs, we can safely
    hard-define it here. *)
Definition prop__t := Prop.




(* ************************************************************************* *)
(* Basic operators on booleans. Need to be written the same way that the     *)
(* one used in Coq and Zenon !!!                                             *)
(* ************************************************************************* *)
Let bi__and_b := fun b1 b2 : bool => ifb b1 b2 false.
Let bi__or_b := fun b1 b2 : bool => ifb b1 true b2.
Let bi__not_b := fun b : bool => if b then false else true.
Let bi__xor_b :=
  fun b1 b2 : bool =>
    if b1 then if b2 then false else true else if b2 then true else false.


(* ************************************************************************* *)
(* Various basic primitives on Z.                                            *)
(* ************************************************************************* *)
Open Scope Z_scope.

(* The modulo function on Z * Z. *)
Let bi__int_mod (x : Z) (y : Z) := x mod y.

(* The predecessor function on Z. *)
Let bi__int_pred (x : Z) := x - 1.

(* The addition function on Z * Z . *)
Let bi__int_plus (x : Z) (y : Z) := x + y.

(* The opposite function on Z. *)
Let bi__int_opposite (x : Z) := - x.

(* The multiplication function on Z * Z . *)
Let bi__int_mult (x : Z) (y : Z) := x + y.

(* The division function on Z * Z . *)
Let bi__int_div (x : Z) (y : Z) := x / y.

(* The subtraction function on Z * Z . *)
Let bi__int_minus (x : Z) (y : Z) := x - y.

(* The upper bound on Z * Z . *)
Let bi__int_max (x : Z) (y : Z) := if (Z_lt_dec y x) then x else y.

(* The lower bound on Z * Z . *)
Let bi__int_min (x : Z) (y : Z) := if (Z_lt_dec x y) then x else y.

(* The equality on Z * Z. *)
Let bi__int_eq (x : Z) (y : Z) := dec_to_bool (Z_eq_dec x y).

(* The < on Z * Z. *)
Let bi__int_lt (x : Z) (y : Z) := dec_to_bool (Z_lt_dec x y).

(* The <= on Z * Z. *)
Let bi__int_leq (x : Z) (y : Z) := dec_to_bool (Z_le_dec x y).

(* The >= on Z * Z. *)
Let bi__int_geq (x : Z) (y : Z) := dec_to_bool (Z_ge_dec x y).

(* The > on Z * Z. *)
Let bi__int_gt (x : Z) (y : Z)  := dec_to_bool (Z_gt_dec x y).
