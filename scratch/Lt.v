(*
Fixpoint lt (x : peano__t) (y : peano__t) :=
  match x with
   | P_zero => 
       match y with 
          | P_zero => false
          | _y => true
       end
    | P_succ(x_1) =>
        match y with 
          | P_zero => false
          | P_succ(y_1) => lt x_1 y_1
        end
    end.
*)

Require Import Bool.
Require Export Arith.


Inductive peano__t : Set := 
  | P_zero : (peano__t)
  | P_succ : (peano__t -> peano__t).


(** Global stuff for cheating. *)
Axiom magic_prove : forall A : Prop, A.
Parameter magic_order_ : forall A : Set, A -> A -> Prop.
Notation magic_order := (magic_order_ _) (only parsing).


Let myfun_order (foo : peano__t * peano__t) (bar : peano__t * peano__t) :=
  magic_order foo bar.


(** Verbatim. *)
Let well_founded_myfun_order: (well_founded myfun_order).
apply magic_prove.
Qed.


(* No tuplification. *)
Let decrease1 : forall x_1 : peano__t, forall y_1 : peano__t,
  myfun_order (x_1, y_1) ((P_succ x_1), (P_succ y_1)).
Proof.
  apply magic_prove.
Defined.


Let __T t := (forall a : peano__t * peano__t, myfun_order a t -> bool).


Let lt_aux : forall args : (peano__t * peano__t), (__T args) -> bool :=
  fun (args : peano__t * peano__t) =>
    match args return (__T args) -> bool with
     | (x, y) =>
        match x return (__T (x, y)) -> bool with
          | P_zero =>
              match y return (__T (P_zero, y)) -> bool with
                | P_zero => fun _ => false
                | _ => fun _ => true
              end
          | P_succ x_1 =>
             match y return (__T ((P_succ x_1), y)) -> bool with
               | P_zero => fun _ => false
               | P_succ y_1 =>
                   fun (lt_rec : __T ((P_succ x_1), (P_succ y_1))) =>
                     lt_rec (x_1, y_1) (decrease1 x_1 y_1)
             end
        end
     end.


Let lt := Fix well_founded_myfun_order
  (fun (args : peano__t * peano__t) => bool) lt_aux.


Eval compute in lt (P_zero, (P_succ P_zero)).
