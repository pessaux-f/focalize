Require Export Bool.
Require Export ZArith.
Require Export Reals.
Require Export Ascii.
Require Export String.
Require Export coq_builtins.


(** The weak proof !!! Give it a Prop, and abracadabra ... it's proved ! *)
Axiom magic_prove : forall A : Prop, A.

Axiom __magic_order__ : forall A : Set, A -> A -> Prop.
(* Notation made available only in Coq parser, not in Coq output / feedback. *)
Notation magic_order := (__magic_order__ _) (only parsing).


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
Definition string__t := string.


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


(* Junky code to have one representative of the string__t type. *)
Variable ___a_string : string__t.


(* Miscellaneous basic functions. *)
Let int_eq : int__t -> int__t -> bool__t := coq_builtins.bi__int_eq.
Let int_lt : int__t -> int__t -> bool__t := coq_builtins.bi__int_lt.
Let int_leq : int__t -> int__t -> bool__t := coq_builtins.bi__int_leq.
Let int_geq : int__t -> int__t -> bool__t := coq_builtins.bi__int_geq.
Let int_gt : int__t -> int__t -> bool__t := coq_builtins.bi__int_gt.
Let pred : int__t -> int__t := coq_builtins.bi__int_pred.
Let int_opp : int__t -> int__t := coq_builtins.bi__int_opposite.
Let int_plus : int__t -> int__t -> int__t := coq_builtins.bi__int_plus.
Let succ (x : int__t) : int__t := (int_plus x 1).
Let int_mult : int__t -> int__t -> int__t := coq_builtins.bi__int_mult.
Let int_minus : int__t -> int__t -> int__t := coq_builtins.bi__int_minus.
Let int_max : int__t -> int__t -> int__t := coq_builtins.bi__int_max.
Let int_min : int__t -> int__t -> int__t := coq_builtins.bi__int_min.



Section eq.
Variable beq___A : Set.
Parameter beq_ : beq___A -> beq___A -> bool.
End eq.
Notation beq := (beq_ _) (only parsing).

Definition __g_base_eq_: forall a: Set,  (a)-> (a)-> bool__t :=
  fun a : Set=>  
    (fun (x : a) => (fun (y : a) => (beq x y) )).
Notation  base_eq:= __g_base_eq_.




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

Inductive partiel__t (__var_a : Set) : Set := 
  | Failed : (partiel__t __var_a)
  | Unfailed : (__var_a -> partiel__t __var_a).

Let is_failed (__var_a : Set) (x : partiel__t __var_a) : bool__t :=
  match x with
   | Failed =>
       true
   | Unfailed (_) =>
       false
   end.

Let non_failed (__var_a : Set) (x : partiel__t __var_a) : __var_a :=
  match x with
   | Failed => __g_foc_error ___a_string
   | Unfailed v => v
  end.
