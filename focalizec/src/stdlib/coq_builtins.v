(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: coq_builtins.v,v 1.4 2008-09-02 13:17:03 pessaux Exp $ *)


Require Export ZArith.
Require Export String.
Open Scope Z_scope.


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



(** Equalities. *)

(* Beware we use Coq [bool] type because we map FoCaL's [bool] type on it. *)
Section eq.
Variable beq___A : Set.
Parameter beq_ : beq___A -> beq___A -> bool.
End eq.
Notation beq := (beq_ _) (only parsing).

Definition __g_base_eq_ (__var_a : Set) (x : __var_a) (y : __var_a) :=
   beq_ __var_a x y.
Notation  bi__base_eq := __g_base_eq_.



(* Conversion of decidable functions into booleans. *)
Set Implicit Arguments.  (* [Unsure] Je ne comprends pas ce que ça fait. *)
Unset Strict Implicit.   (* [Unsure] Je ne comprends pas ce que ça fait. *)
Let dec_to_bool (A : Prop) (dec : {A} + {~ A}) :=
  if dec return bool then true else false.

Ltac prove_term_obl term_obl :=
  intuition.

(* "bi" for "built-in". Don't search anymore... *)
Inductive bi__unit : Set :=
  | Null: bi__unit.


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
