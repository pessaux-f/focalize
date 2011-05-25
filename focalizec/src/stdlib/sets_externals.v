Require Import Bool.
Require coq_builtins.
Require Export Ascii.
Require Export String.
  
Let i_failed (A : Set) (r : string) (elt : A) : A :=
  elt.