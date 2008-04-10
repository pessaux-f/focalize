Require Export Bool.
Require Export ZArith.
Open Scope Z_scope.
Require Export Reals.
Require Export Ascii.
Require Export String.
Require Export List.
Require Export coq_builtins.

Definition int__t := Z.

Definition unit__t := coq_builtins.bi__unit.

Definition float__t := R.

Definition char__t := ascii.

Definition string__t := string.

Definition bool__t := bool.

Definition list__t (__var_a : Set) := (list __var_a).

Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
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
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_theorem TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
Chapter Basic_object.
  Record Basic_object : Type :=
    mk_Basic_object {
    Basic_object_T :> Set ;
    (* From species basics#Basic_object. *)
    Basic_object_parse : string__t -> Basic_object_T ;
    (* From species basics#Basic_object. *)
    Basic_object_print : Basic_object_T -> string__t
    }.
  
  
  (* Carrier representation. *)
  Variable self_T : Set.
  
  Definition Basic_object__parse (abst_T : Set) (x : string__t) : abst_T :=
    (foc_error C_string).
  Let self_parse := Basic_object__parse self_T.
  Definition Basic_object__print (abst_T : Set) (x : abst_T) : string__t :=
    C_string.
  Let self_print := Basic_object__print self_T.
  End Basic_object.

Inductive partiel__t (__var_a : Set) : Set := 
  | Failed : (partiel__t __var_a)
  | Unfailed : (__var_a -> partiel__t __var_a).

Infer.PCM_let_def TODO
Infer.PCM_let_def TODO
