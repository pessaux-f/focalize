Require Export Bool.
Require Export ZArith.


(** The type of strings named [string__t]. *)
Inductive string : Set :=
  string_make : string.
Definition string__t := string.


(** The type [unit__t] only contains 1 value. *)
Inductive unit__t : Set :=
  Void : unit__t.

(** Alias the type of integers [int__t] to Coq [Z]. *)
Definition int__t := Z.


(** Alias the type of booleans [bool__t] to Coq [bool]. *)
Definition bool__t := bool.


(** Alias the type of propositions [prop__t] to Coq [Prop]. *)
Definition prop__t := Prop.


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

Chapter Basic_object.
  Record Basic_object : Type :=
    mk_Basic_object {
    Basic_object_T :> Set ;
    (* From species basics#Basic_object. *)
    parse : string__t -> Basic_object_T ;
    (* From species basics#Basic_object. *)
    print : Basic_object_T -> string__t
    }.
  End Basic_object.

