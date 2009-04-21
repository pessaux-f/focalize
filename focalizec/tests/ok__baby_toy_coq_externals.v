Require Export ZArith. (* To have type Z. *)

Record foc_record (param_a : Set) (param_b : Set) : Type :=
  mk_foc_record {
    hc : Z;
    conts : ((param_a * param_b)%type)
}.

