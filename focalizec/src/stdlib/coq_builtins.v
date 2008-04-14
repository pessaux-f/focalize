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

(* $Id: coq_builtins.v,v 1.2 2008-04-14 16:07:32 pessaux Exp $ *)


Require Export ZArith.
Open Scope Z_scope.

(* Conversion of decidable functions into booleans. *)
Set Implicit Arguments.  (* [Unsure] Je ne comprends pas ce que ça fait. *)
Unset Strict Implicit.   (* [Unsure] Je ne comprends pas ce que ça fait. *)
Let dec_to_bool (A : Prop) (dec : {A} + {~ A}) :=
  if dec return bool then true else false.

(* "bi" for "built-in". Don't search anymore... *)
Inductive bi__unit : Set :=
  | Null: bi__unit.

(* The predecessor function on Z. *)
Let bi__int_pred (x : Z) := x - 1.

(* The addition function on Z * Z . *)
Let bi__int_plus (x : Z) (y : Z) := x + y.

(* The opposite function on Z. *)
Let bi__int_opposite (x : Z) := - x.

(* The multiplication function on Z * Z . *)
Let bi__int_mult (x : Z) (y : Z) := x + y.

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
