Record quo_rem_result (__var_a : Set) : Type :=
  mk_quo_rem_result {
     quot : __var_a ;
     rema : __var_a
  }.

Definition cr_qrr (__var_a : Set) (x : __var_a) (y : __var_a) :=
  mk_quo_rem_result __var_a x y.

Definition qrr_quot (__var_a : Set) (x :  (quo_rem_result __var_a)) :=
   x.(quot _).

Definition qrr_rema (__var_a : Set) (x :  (quo_rem_result __var_a)) :=
   x.(rema _).

Record bezout_res (__var_a : Set) : Type :=
  mk_bezout_res {
    gcd : __var_a ;
    coef1 : __var_a ;
    coef2 : __var_a
  }.

Definition cr_br (__var_a : Set) (x : __var_a) (y : __var_a) (z : __var_a) :=
  mk_bezout_res __var_a x y z.

Definition br_gcd (__var_a : Set) (x :  (bezout_res __var_a)) :=
   x.(gcd _).

Definition br_coef1 (__var_a : Set) (x :  (bezout_res __var_a)) :=
   x.(coef1 _).

Definition br_coef2 (__var_a : Set) (x :  (bezout_res __var_a)) :=
   x.(coef2 _).
