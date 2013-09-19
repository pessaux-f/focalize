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

(* Below: to prevent Function to apply heuristics that would
the expected aim in recursive functions termination proofs. *)

Set Function_raw_tcc.

Require basics.
Module Identity.
  Record Identity : Type :=
    mk_record {
    rf_T :> Set ;
    (* From species recurs#Identity. *)
    rf_id : basics.int__t -> basics.int__t
    }.
  
  Module Termination_id_namespace.
    Definition id_wforder (__x __y : (basics.int__t)) : Prop :=
      coq_builtins.magic_order __x __y.
    Theorem id_termination
      : (forall x : basics.int__t, forall n : basics.int__t, (n = x) ->
       (id_wforder) (basics._dash_ n 1) x)
       /\
        (well_founded (id_wforder)).
      
      apply coq_builtins.magic_prove.
      Qed.
    Section id.
      
      
      (* Abstracted termination order. *)
      Variable __term_order : (basics.int__t) -> (basics.int__t) -> Prop.
      Variable __term_obl :(forall x : basics.int__t, forall n :
        basics.int__t, (n = x) -> __term_order (basics._dash_ n 1) x)
        /\
         (well_founded __term_order).
      
      Function id (__arg: (basics.int__t)) {wf __term_order __arg}:
        basics.int__t
        :=
        match __arg with
          | (x) =>
          match x with
           | 0 =>
               0
           | n =>
               (basics._plus_ 1 (id (basics._dash_ n 1)))
           end
          end.
        Proof.
          apply coq_builtins.magic_prove.
          Qed.
        Definition Identity__id x := id (x).
        End id.
      End Termination_id_namespace.
    Definition id := Termination_id_namespace.Identity__id
      coq_builtins.magic_order.
    
End Identity.
  
  Inductive t__t : Set := 
    | D : (t__t)
    | C : (t__t -> t__t).
  
  Module A.
    Record A : Type :=
      mk_record {
      rf_T :> Set ;
      (* From species recurs#A. *)
      rf_toto : basics.int__t ;
      (* From species recurs#A. *)
      rf_foo : t__t -> basics.int__t -> basics.int__t
      }.
    
    Module Termination_foo_namespace.
      Definition foo_wforder (abst_toto : basics.int__t) (__x __y :
        (((t__t) * (basics.int__t))%type)) : Prop := coq_builtins.magic_order
        __x __y.
      Theorem foo_termination (abst_toto : basics.int__t)
        : (forall x : t__t, forall y : basics.int__t, (@D   = x) -> ~
         Is_true ((basics.syntactic_equal _ y 0)) -> (foo_wforder abst_toto)
         ((@D ), ((basics._plus_ y 1))) (x, y))
         /\
         (forall x : t__t, forall y : basics.int__t, forall a : t__t, (@C 
         a = x) -> (foo_wforder abst_toto) (a, ((basics._dash_ y abst_toto)))
         (x, y))
         /\
          (well_founded (foo_wforder abst_toto)).
        
        apply coq_builtins.magic_prove.
        Qed.
      Section foo.
        Variable abst_toto : basics.int__t.
        
        
        (* Abstracted termination order. *)
        Variable __term_order :
          (((t__t) * (basics.int__t))%type) -> (((t__t) *
                                                 (basics.int__t))%type) -> Prop.
        Variable __term_obl :(forall x : t__t, forall y : basics.int__t, (@D 
           = x) -> ~ Is_true ((basics.syntactic_equal _ y 0)) -> __term_order
          ((@D ), ((basics._plus_ y 1))) (x, y))
          /\
          (forall x : t__t, forall y : basics.int__t, forall a : t__t, (@C 
          a = x) -> __term_order (a, ((basics._dash_ y abst_toto))) (x, y))
          /\
           (well_founded __term_order).
        
        Function foo (__arg: (((t__t) * (basics.int__t))%type))
          {wf __term_order __arg}: basics.int__t
          :=
          match __arg with
            | (x,
            y) =>
            match x with
             | D
                  =>
                 (if (basics.syntactic_equal _ y 0) then 1
                   else (basics._plus_ 1
                          (foo ((@D ), ((basics._plus_ y 1))))))
             | C D  =>
                 2
             | C a =>
                 (foo (a, ((basics._dash_ y abst_toto))))
             end
            end.
          Proof.
            assert (__force_use_abst_toto := abst_toto).
            apply coq_builtins.magic_prove.
            Qed.
          Definition A__foo x  y := foo (x, y).
          End foo.
        End Termination_foo_namespace.
      Definition foo (abst_toto : basics.int__t) :=
        Termination_foo_namespace.A__foo abst_toto coq_builtins.magic_order.
      
End A.
    
    Module B.
      Record B : Type :=
        mk_record {
        rf_T :> Set ;
        (* From species recurs#B. *)
        rf_bar : t__t -> basics.int__t -> basics.int__t ;
        (* From species recurs#B. *)
        rf_bar0 : t__t -> basics.int__t ;
        (* From species recurs#B. *)
        rf_bar2 : basics.int__t -> t__t -> basics.int__t
        }.
      
      Definition bar (a : t__t) (b : basics.int__t) : basics.int__t :=
        let fix rec_bar (__var_a : Set) (x : t__t) (y : __var_a) {struct x} :
          basics.int__t :=
          match x with
           | D  =>
               0
           | C z =>
               (basics._plus_ 1 (rec_bar _ z y))
           end
        in
        (rec_bar _ a b).
      Fixpoint bar0  (x : t__t) { struct x }  : basics.int__t :=
        match x with
         | D  =>
             0
         | C y =>
             (basics._plus_ 1 (bar0 y))
         end.
      
      Definition bar2 (a : basics.int__t) (b : t__t) : basics.int__t :=
        let fix rec_bar2 (__var_b : Set) (x : __var_b) (y : t__t) {struct y}
          :
          basics.int__t :=
          match y with
           | D  =>
               0
           | C z =>
               (basics._plus_ 1 (rec_bar2 _ x z))
           end
        in
        (rec_bar2 _ a b).
      
End B.
    
    Fixpoint gee (x : basics.int__t) (y : basics.int__t) {struct x} :
      basics.int__t :=
      (if (basics.syntactic_equal _ x 0) then 1
        else (basics._plus_ 1
               (gee ((basics._dash_ x 1)) ((basics._plus_ y 1))))).
    
    Fixpoint buzz (x : t__t) (y : basics.int__t) {struct x} :
      basics.int__t :=
      match x with
       | D  =>
           0
       | C y =>
           (basics._plus_ 1 (buzz y 0))
       end.
    
    Fixpoint buzz2 (x : t__t) (y : basics.int__t) {struct x} :
      basics.int__t :=
      match x with
       | D  =>
           0
       | C y =>
           (basics._plus_ 1 (buzz2 y 0))
       end.
    
    Let toto (__var_a : Set) (x : __var_a) : __var_a :=
      x.
    
    Fixpoint buzz3 (x : t__t) (y : basics.int__t) {struct x} :
      basics.int__t :=
      match x with
       | D  =>
           0
       | C y =>
           (basics._plus_ 1 (buzz3 y 0))
       end.
    
    Inductive tt__t : Set := 
      | Z : (tt__t)
      | S : (tt__t -> tt__t).
    
    Fixpoint f (__var_a : Set) (x : tt__t) (y : __var_a) {struct x} :
      basics.int__t :=
      match x with
       | Z  =>
           0
       | S n =>
           (g _ n y)
       end
    with g (__var_b : Set) (x : tt__t) (y : __var_b) {struct x} :
      basics.int__t :=
      match x with
       | Z  =>
           0
       | S n =>
           (f _ n y)
       end.
    
    