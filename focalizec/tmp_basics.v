Require Export Bool.
Require Export ZArith.
Require Export Reals.
Require Export Ascii.

(** The weak proof !!! Give it a Prop, and abracadabra ... it's proved ! *)
Axiom magic_prove : forall A : Prop, A.


(** The type [unit__t] only contains 1 value. *)
Inductive unit__t : Set :=
  Void : unit__t.


(** Alias the type of integers [int__t] to Coq [Z]. *)
Definition int__t := Z.


(** Alias the type of booleans [bool__t] to Coq [bool]. *)
Definition bool__t := bool.


(** Alias the type of propositions [prop__t] to Coq [Prop]. *)
Definition prop__t := Prop.


(** Alias the type of floatting [float__t] to Coq [R]. *)
Definition float__t := R.


(** Alias the type of chars [char__t] to Coq [ascii]. *)
Definition char__t := ascii.


(** Alias the type of strings [string__t] to Coq [string]. *)
Definition string__t := ascii.


(** Exceptions have all the properties we can imagine. That's assumed ! *)
Axiom __foc_bottom__ : forall A : Set, A.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation bottom := (__foc_bottom__ _) (only parsing).


(** Definition of the "raise" function. *)
Definition __g_foc_error_ (a : Set) (s : string__t) : a := __foc_bottom__ _.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation __g_foc_error := (__g_foc_error_ _) (only parsing).

Definition or_b (x : bool__t) (y : bool__t) : bool__t :=
  if x then true else y.

Definition not_b (x : bool__t) : bool__t := if x then false else true.

Definition and_b (x : bool__t) (y : bool__t) : bool__t :=
  if x then y else false.

Definition xor_b (x : bool__t) (y : bool__t) : bool__t :=
  (or_b  (and_b  x (not_b  y)) (and_b  (not_b  x) y)).

Require Export ZArith.

(* [Unsure]. *)
Definition int_eq (x : int__t) (y : int__t) : bool__t := true.
Definition int_leq (x : int__t) (y : int__t) : bool__t := true.


(* Junky code to have one representative of the string__t type. *)
Variable ___a_string : string__t.


(* Currently partially hand-generated. *)
Chapter Basic_object.
  Record Basic_object : Type :=
    mk_Basic_object {
    Basic_object_T :> Set ;
    (* From species basics#Basic_object. *)
    parse : string__t -> Basic_object_T ;
    (* From species basics#Basic_object. *)
    print : Basic_object_T -> string__t ;
    (* From species basics#basic_object. *)
    Basic_object_print : Basic_object_T -> string__t
    }.

  (* Carrier representation. *)
  Variable self_T : Set.
  
  Definition Basic_object__parse (abst_T : Set) (x : string__t) :
    abst_T := (__g_foc_error ___a_string).
  Let self_parse := Basic_object__parse self_T.
  Definition Basic_object__print (abst_T : Set) (x : abst_T) :
    basics.string__t := ___a_string.
  Let self_print := Basic_object__print self_T.
End Basic_object.

Inductive partiel__t (a : Set) : Set :=
  | Failed_ : (partiel__t (a))
  | Unfailed_ : (a) -> (partiel__t (a)).

Notation Failed := (Failed_ _ ).
Notation Unfailed := (Unfailed_ _).

Definition __is_failed (a : Set) (x : partiel__t a) : bool__t :=
  match x with
   | Failed_ => true
   | Unfailed_ _ => false
  end.
Notation is_failed := (__is_failed _).

Definition __non_failed (a : Set) (x : partiel__t a) : a :=
  match x with
   | Failed_ => __g_foc_error ___a_string
   | Unfailed_ v => v
  end.
Notation non_failed := (__non_failed _).
