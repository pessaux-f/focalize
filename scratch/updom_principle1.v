Global Unset Automatic Introduction.
Require Import zenon.
Require Import zenon_induct.
Require Import zenon_focal.
Require Export Bool.
Require Export ZArith.
Open Scope Z_scope.
Require Export Reals.
Require Export Ascii.
Require Export String.
Require Export List.
Require Export Recdef.
Require Export coq_builtins.

Require basics.
Inductive list_t__t (__var_a : Set) : Set := 
  | Nil_list : ((list_t__t __var_a))
  | Cons_list : (__var_a -> (list_t__t __var_a) -> (list_t__t __var_a)).

Inductive tree_t__t (__var_a : Set) : Set := 
  | Nil_tree : ((tree_t__t __var_a))
  | Cons_tree :
  (basics.int__t -> (list_t__t (tree_t__t __var_a)) -> (tree_t__t __var_a)).

Fixpoint pr_list (__var_a : Set) (pr_elem_fct : __var_a -> basics.string__t)
  (l : (list_t__t __var_a)) {struct pr_elem_fct} :
  basics.string__t :=
  match l with
   | Nil_list  =>
       "0"%string
   | (Cons_list h
       q) =>
       (basics._hat_ (pr_elem_fct h) (pr_list _ pr_elem_fct q))
   end.

Fixpoint pr_tree (__var_a : Set) (l : (list_t__t (tree_t__t __var_a)))
  {struct l} :
  basics.string__t :=
  match l with
   | Nil_tree  =>
       ""%string
   | (Cons_tree n l) =>
       (basics._hat_ (basics.string_of_int n) (lp _ l))
   end
with lp (__var_b : Set) (l : (tree_t__t __var_a)) {struct l} :
  basics.string__t :=
  (pr_list _ pr_tree _ l).

