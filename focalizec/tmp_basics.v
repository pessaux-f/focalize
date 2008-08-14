Require Import zenon.
Require Import zenon_coqbool.
Require Export Bool.
Require Export ZArith.
Open Scope Z_scope.
Require Export Reals.
Require Export Ascii.
Require Export String.
Require Export List.
Require Export Recdef.
Require Export coq_builtins.

Definition int__t :=  Z .

Definition unit__t :=  coq_builtins.bi__unit .

Definition float__t :=  R .

Definition char__t :=  ascii .

Definition string__t :=  string .

Definition bool__t :=  bool .

Definition list__t (__var_a : Set) :=  (list __var_a) .

Let foc_error (__var_a : Set) : string__t -> __var_a :=  foc_error .

Let and_b (x : bool__t) (y : bool__t) : bool__t :=
  match (x, y) with
   | (true, true) =>
       true
   | (false, false) =>
       false
   | (_, _) =>
       false
   end.

Let or_b (x : bool__t) (y : bool__t) : bool__t :=
  match (x, y) with
   | (true, _) =>
       true
   | (_, true) =>
       true
   | (_, _) =>
       false
   end.

Let not_b (x : bool__t) : bool__t :=
  match x with
   | true =>
       false
   | false =>
       true
   end.

Let xor_b (x : bool__t) (y : bool__t) : bool__t :=
  (or_b (and_b x (not_b y)) (and_b (not_b x) y)).

Let crp (__var_a : Set) (__var_b : Set) (x : __var_b) (y : __var_a) :
  (prod __var_b __var_a) := (x, y).

Let first (__var_a : Set) (__var_b : Set) (x : (prod __var_b __var_a)) :
  __var_b := match x with
              | (v, _) =>
                  v
              end.

Let scnd (__var_a : Set) (__var_b : Set) (x : (prod __var_a __var_b)) :
  __var_b := match x with
              | (_, v) =>
                  v
              end.

Let string_of_int : int__t -> string__t :=  [Unsure] .

Let int_of_string : string__t -> int__t :=  [Unsure] .

Let print_int : int__t -> unit__t :=  [Unsure] .

Let int_mod : int__t -> int__t -> int__t :=  [Unsure] .

Let int_eq : int__t -> int__t -> bool__t :=  [Unsure] .

Let int_div : int__t -> int__t -> int__t :=  [Unsure] .

Let int_lt : int__t -> int__t -> bool__t :=  int_lt .

Let int_leq : int__t -> int__t -> bool__t :=  int_leq .

Let int_geq : int__t -> int__t -> bool__t :=  int_geq .

Let int_gt : int__t -> int__t -> bool__t :=  int_gt .

Let phys_eq (__var_a : Set) : __var_a -> __var_a -> bool__t :=  [Unsure] .

Let base_eq (__var_a : Set) : __var_a -> __var_a -> bool__t :=  [Unsure] .

Let _focop_eq_ (__var_a : Set) (x : __var_a) (y : __var_a) : bool__t :=
  (base_eq _ x y).

Let _focop_ampers__focop_ampers_ (x : bool__t) (y : bool__t) : bool__t :=
  (and_b x y).

Let _focop_pipe__focop_pipe_ (x : bool__t) (y : bool__t) : bool__t :=
  (or_b x y).

Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Let pred : int__t -> int__t :=  coq_builtins.bi__int_pred .

Let int_opp : int__t -> int__t :=  coq_builtins.bi__int_opposite .

Let int_plus : int__t -> int__t -> int__t :=  coq_builtins.bi__int_plus .

Let _focop_plus_ (x : int__t) (y : int__t) : int__t := (int_plus x y).

Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Let succ (x : int__t) : int__t := (int_plus x 1).

Let int_mult : int__t -> int__t -> int__t :=  coq_builtins.bi__int_mult .

Let int_minus : int__t -> int__t -> int__t :=  coq_builtins.bi__int_minus .

Let int_max : int__t -> int__t -> int__t :=  coq_builtins.bi__int_max .

Let int_min : int__t -> int__t -> int__t :=  coq_builtins.bi__int_min .

Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Let sc : string__t -> string__t -> string__t :=  [Unsure] .

Let _focop_hat_ (x : string__t) (y : string__t) : string__t := (sc x y).

Let str_lt : string__t -> string__t -> bool__t :=  [Unsure] .

Let print_string : string__t -> unit__t :=  [Unsure] .

Let print_newline (x : unit__t) : unit__t :=
  (print_string basics.___a_string).

Module Basic_object.
  Record Basic_object : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species basics#Basic_object. *)
    rf_parse : string__t -> rf_T ;
    (* From species basics#Basic_object. *)
    rf_print : rf_T -> string__t
    }.
  
  Definition parse (abst_T : Set) (x : string__t) : abst_T :=
    (foc_error _ basics.___a_string).
  Definition print (abst_T : Set) (x : abst_T) : string__t :=
    basics.___a_string.
  
End Basic_object.

Inductive partiel__t (__var_a : Set) : Set := 
  | Failed : ((partiel__t __var_a))
  | Unfailed : (__var_a -> (partiel__t __var_a)).

Let is_failed (__var_a : Set) (x : (partiel__t __var_a)) : bool__t :=
  match x with
   | Failed =>
       true
   | Unfailed (_) =>
       false
   end.

Let non_failed (__var_a : Set) (x : (partiel__t __var_a)) : __var_a :=
  match x with
   | Failed =>
       (foc_error _ basics.___a_string)
   | Unfailed (a) =>
       a
   end.

