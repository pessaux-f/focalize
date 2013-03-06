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
Module Data.
  Definition id : basics.string__t := "default"%string.
  
End Data.

Module Comparable.
  Definition toto (abst_id : basics.string__t) : basics.string__t := abst_id.
  Definition gt (abst_T : Set) (abst_eq : abst_T -> abst_T -> basics.bool__t)
    (abst_lt : abst_T -> abst_T -> basics.bool__t) (x : abst_T)
    (y : abst_T) : basics.bool__t :=
    (basics._amper__amper_ (basics._tilda__tilda_ ((abst_lt x y)))
      (basics._tilda__tilda_ ((abst_eq x y)))).
  
End Comparable.

Module NativeInt.
  Record me_as_species (rf_T : Set) : Type :=
    mk_record {
    (* From species trash#NativeInt. *)
    rf_id : basics.string__t ;
    (* From species trash#Comparable. *)
    rf_toto : basics.string__t ;
    (* From species trash#NativeInt. *)
    rf_eq : rf_T -> rf_T -> basics.bool__t ;
    (* From species trash#NativeInt. *)
    rf_from_int : basics.int__t -> rf_T ;
    (* From species trash#NativeInt. *)
    rf_lt : rf_T -> rf_T -> basics.bool__t ;
    (* From species trash#Comparable. *)
    rf_gt : rf_T -> rf_T -> basics.bool__t ;
    (* From species trash#NativeInt. *)
    rf_lt_not_gt :
      forall x  y : rf_T, Is_true ((rf_lt x y)) -> ~Is_true ((rf_gt x y))
    }.
  
  Definition id : basics.string__t := "native int"%string.
  Definition eq (abst_T := basics.int__t) (x : abst_T) (y : abst_T) :
    basics.bool__t := (basics._equal_0x x y).
  Definition from_int (abst_T := basics.int__t) (x : basics.int__t) :
    abst_T := x.
  Definition lt (abst_T := basics.int__t) (x : abst_T) (y : abst_T) :
    basics.bool__t := (basics._lt_0x x y).
  
  (* From species trash#NativeInt. *)
  (* Section for proof of theorem 'lt_not_gt'. *)
  Section Proof_of_lt_not_gt.
    Let abst_T := basics.int__t.
    Let abst_eq := eq.
    Let abst_lt := lt.
    Let abst_gt := Comparable.gt abst_T abst_eq
    abst_lt.
(* File "trash.fcl", line 25, characters 4-54 *)
Theorem for_zenon_lt_not_gt:(forall x:abst_T,(forall y:abst_T,((Is_true
(abst_lt x y))->(~(Is_true (abst_gt x y)))))).
Proof.
exact(
(NNPP _ (fun zenon_G=>(zenon_notallex (fun x:abst_T=>(forall y:abst_T,((
Is_true (abst_lt x y))->(~(Is_true (abst_gt x y)))))) (fun zenon_H11=>(
zenon_ex abst_T (fun x:abst_T=>(~(forall y:abst_T,((Is_true (abst_lt x
y))->(~(Is_true (abst_gt x y))))))) (fun(zenon_Tx_c:abst_T) zenon_H10=>(
zenon_notallex (fun y:abst_T=>((Is_true (abst_lt zenon_Tx_c y))->(~(
Is_true (abst_gt zenon_Tx_c y))))) (fun zenon_Hf=>(zenon_ex abst_T (fun
y:abst_T=>(~((Is_true (abst_lt zenon_Tx_c y))->(~(Is_true (abst_gt
zenon_Tx_c y)))))) (fun(zenon_Ty_d:abst_T) zenon_He=>(zenon_notimply _
_ (fun zenon_H4 zenon_Hd=>(zenon_Hd (fun zenon_Hc=>(let zenon_Hb
:=zenon_Hc in (let zenon_Ha:=zenon_Hb in (zenon_focal_and (
basics._tilda__tilda_ (abst_lt zenon_Tx_c zenon_Ty_d)) (
basics._tilda__tilda_ (abst_eq zenon_Tx_c zenon_Ty_d)) (fun zenon_H9=>(
zenon_and _ _ (fun zenon_H7 zenon_H8=>(let zenon_H6:=zenon_H7 in (
zenon_focal_not (abst_lt zenon_Tx_c zenon_Ty_d) (fun zenon_H5=>(
zenon_H5 zenon_H4)) zenon_H6))) zenon_H9)) zenon_Ha)))))) zenon_He))
zenon_Hf)) zenon_H10)) zenon_H11)) zenon_G)))).
Qed.

    (* Dummy theorem to enforce Coq abstractions. *)
    Theorem for_zenon_abstracted_lt_not_gt :
      forall x  y : abst_T,
        Is_true ((abst_lt x y)) -> ~Is_true ((abst_gt x y)).
    assert (__force_use_abst_T := abst_T).
    assert (__force_use_abst_eq := abst_eq).
    assert (__force_use_abst_lt := abst_lt).
    assert (__force_use_abst_gt := abst_gt).
    apply for_zenon_lt_not_gt;
    auto.
    Qed.
    End Proof_of_lt_not_gt.
  
  Theorem lt_not_gt  (abst_T := basics.int__t) (abst_eq := eq) (abst_lt :=
    lt) (abst_gt := Comparable.gt abst_T abst_eq abst_lt):
    forall x  y : abst_T, Is_true ((abst_lt x y)) -> ~Is_true ((abst_gt x y)).
  apply for_zenon_abstracted_lt_not_gt;
  auto.
  Qed.
  
  (* Fully defined 'NativeInt' species's collection generator. *)
  Definition collection_create :=
    let local_rep := basics.int__t in
    (* From species trash#NativeInt. *)
    let local_id := id in
    (* From species trash#Comparable. *)
    let local_toto := Comparable.toto local_id in
    (* From species trash#NativeInt. *)
    let local_eq := eq in
    (* From species trash#NativeInt. *)
    let local_from_int := from_int in
    (* From species trash#NativeInt. *)
    let local_lt := lt in
    (* From species trash#Comparable. *)
    let local_gt := Comparable.gt local_rep local_eq local_lt in
    (* From species trash#NativeInt. *)
    let local_lt_not_gt := lt_not_gt in
    mk_record local_rep local_id local_toto local_eq local_from_int local_lt
    local_gt local_lt_not_gt.
  
End NativeInt.

Inductive statut_t__t : Set := 
  | In_range : (statut_t__t)
  | Too_low : (statut_t__t)
  | Too_high : (statut_t__t).

Module Thresholder.
  Record me_as_species (V_T : Set) (_p_minv_minv : V_T) (_p_maxv_maxv : V_T) (_p_V_gt : 
    V_T -> V_T -> basics.bool__t)  (rf_T : Set) : Type :=
    mk_record {
    (* From species trash#Thresholder. *)
    rf_filter : V_T -> rf_T ;
    (* From species trash#Thresholder. *)
    rf_get_status : rf_T -> statut_t__t ;
    (* From species trash#Thresholder. *)
    rf_get_value : rf_T -> V_T ;
    (* From species trash#Thresholder. *)
    rf_too_low_not_gt_min :
      forall x : V_T,
        Is_true ((basics._equal_ _ (rf_get_status (rf_filter x)) (@Too_low ))) ->
          ~Is_true ((_p_V_gt x _p_minv_minv)) ;
    (* From species trash#Thresholder. *)
    rf_junk : rf_T -> basics.int__t
    }.
  
  Definition filter (_p_V_T : Set) (_p_V_lt :
    _p_V_T -> _p_V_T -> basics.bool__t) (_p_V_gt :
    _p_V_T -> _p_V_T -> basics.bool__t) (_p_minv_minv : _p_V_T)
    (_p_maxv_maxv : _p_V_T) (abst_T := ((_p_V_T * statut_t__t)%type))
    (x : _p_V_T) : abst_T :=
    (if (_p_V_lt x _p_minv_minv) then (_p_minv_minv, (@Too_low ))
      else (if (_p_V_gt x _p_maxv_maxv) then (_p_maxv_maxv, (@Too_high ))
             else (x, (@In_range )))).
  Definition get_status (_p_V_T : Set)
    (abst_T := ((_p_V_T * statut_t__t)%type)) (x : abst_T) : statut_t__t :=
    (basics.snd _ _ x).
  Definition get_value (_p_V_T : Set)
    (abst_T := ((_p_V_T * statut_t__t)%type)) (x : abst_T) : _p_V_T :=
    (basics.fst _ _ x).
  
  (* From species trash#Thresholder. *)
  (* Section for proof of theorem 'too_low_not_gt_min'. *)
  Section Proof_of_too_low_not_gt_min.
    Variable _p_V_T : Set.
    Variable _p_V_lt : _p_V_T -> _p_V_T -> basics.bool__t.
    Variable _p_V_gt : _p_V_T -> _p_V_T -> basics.bool__t.
    Variable _p_V_lt_not_gt :
      forall x  y : _p_V_T,
        Is_true ((_p_V_lt x y)) -> ~Is_true ((_p_V_gt x y)).
    Variable _p_minv_minv : _p_V_T.
    Variable _p_maxv_maxv : _p_V_T.
    Let abst_T := ((_p_V_T * statut_t__t)%type).
    Let abst_filter := filter _p_V_T _p_V_lt _p_V_gt _p_minv_minv
    _p_maxv_maxv.
    Let abst_get_status := get_status
    _p_V_T.
    Section __B_1.
      Variable x : _p_V_T.
      Variable H :
        Is_true ((basics._equal_ _ (basics.snd _ _ (abst_filter x))
                   (@Too_low ))).
      Section __B_1_1.
(* File "trash.fcl", line 53, characters 13-63 *)
Theorem for_zenon___B_1_1_LEMMA:(Is_true (_p_V_lt x _p_minv_minv)).
Proof.
exact(
let zenon_L1_:(forall zenon_T_c,(forall zenon_T_d,(((@ Too_high) =
zenon_T_d)->(((abst_filter x) = (@ Datatypes.pair _ _ zenon_T_c
zenon_T_d))->((match (abst_filter x) with|Datatypes.pair zenon_Vb
zenon_Va=>zenon_Va end = (@ Too_low))->((~((@ Too_low) = (@ Too_high)))
->False)))))):=
(fun zenon_T_c zenon_T_d(zenon_H6:((@ Too_high) = zenon_T_d))(zenon_H5:(
(abst_filter x) = (@ Datatypes.pair _ _ zenon_T_c zenon_T_d)))(zenon_H7
:(match (abst_filter x) with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va
end = (@ Too_low)))(zenon_H4:(~((@ Too_low) = (@ Too_high))))=>(let
zenon_H8:=(fun zenon_H9=>(zenon_subst _ (fun zenon_Vma=>(zenon_Vma = (@
Too_high))) (@ Too_high) (@ Too_low) (fun zenon_Ha=>(zenon_subst _ (fun
zenon_Vna=>(zenon_Vna = (@ Too_low))) match (abst_filter x) with|
Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end (@ Too_high) (fun
zenon_Hb=>(let zenon_H8:=(fun zenon_H9=>(zenon_subst _ (fun zenon_Voa=>(
zenon_Voa = (@ Too_high))) (@ Too_high) match (abst_filter x) with|
Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end (fun zenon_He=>(
zenon_congruence_lr _ (fun zenon_Vz=>(~((@ Too_high) = match zenon_Vz
with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end))) (abst_filter x) (
@ Datatypes.pair _ _ zenon_T_c zenon_T_d) (fun zenon_Hd=>(
zenon_induct_match_redex (~((@ Too_high) = match (@ Datatypes.pair _ _
zenon_T_c zenon_T_d) with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va
end)) (fun zenon_Hc=>(zenon_Hc zenon_H6)) zenon_Hd)) zenon_He zenon_H5))
 zenon_Hb zenon_H9)) in (zenon_noteq _ (@ Too_high) zenon_H8)))
zenon_Ha zenon_H7)) zenon_H4 zenon_H9)) in (zenon_noteq _ (@ Too_high)
zenon_H8)))in
let zenon_L2_:(forall zenon_T_c,(forall zenon_T_d,(((@ In_range) =
zenon_T_d)->(((abst_filter x) = (@ Datatypes.pair _ _ zenon_T_c
zenon_T_d))->((match (abst_filter x) with|Datatypes.pair zenon_Vb
zenon_Va=>zenon_Va end = (@ Too_low))->((~((@ Too_low) = (@ In_range)))
->False)))))):=
(fun zenon_T_c zenon_T_d(zenon_H10:((@ In_range) = zenon_T_d))(zenon_H5
:((abst_filter x) = (@ Datatypes.pair _ _ zenon_T_c zenon_T_d)))(
zenon_H7:(match (abst_filter x) with|Datatypes.pair zenon_Vb zenon_Va=>
zenon_Va end = (@ Too_low)))(zenon_Hf:(~((@ Too_low) = (@ In_range))))=>
(let zenon_H11:=(fun zenon_H12=>(zenon_subst _ (fun zenon_Vpa=>(
zenon_Vpa = (@ In_range))) (@ In_range) (@ Too_low) (fun zenon_H13=>(
zenon_subst _ (fun zenon_Vqa=>(zenon_Vqa = (@ Too_low))) match (
abst_filter x) with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end (@
In_range) (fun zenon_H14=>(let zenon_H11:=(fun zenon_H12=>(zenon_subst
_ (fun zenon_Vra=>(zenon_Vra = (@ In_range))) (@ In_range) match (
abst_filter x) with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end (fun
zenon_H17=>(zenon_congruence_lr _ (fun zenon_Vp=>(~((@ In_range) =
match zenon_Vp with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end))) (
abst_filter x) (@ Datatypes.pair _ _ zenon_T_c zenon_T_d) (fun
zenon_H16=>(zenon_induct_match_redex (~((@ In_range) = match (@
Datatypes.pair _ _ zenon_T_c zenon_T_d) with|Datatypes.pair zenon_Vb
zenon_Va=>zenon_Va end)) (fun zenon_H15=>(zenon_H15 zenon_H10))
zenon_H16)) zenon_H17 zenon_H5)) zenon_H14 zenon_H12)) in (zenon_noteq
_ (@ In_range) zenon_H11))) zenon_H13 zenon_H7)) zenon_Hf zenon_H12))
in (zenon_noteq _ (@ In_range) zenon_H11)))in
(NNPP _ (fun zenon_G=>(coq_builtins.zenon_syntactic_equal
zenon_focal_eqdec _ (basics.snd _ _ (abst_filter x)) (@ Too_low) (fun
zenon_H25=>(let zenon_H7:=zenon_H25 in (@Datatypes.prod_ind _ _ (fun
zenon_Vua=>(~((abst_filter x) = zenon_Vua))) (fun zenon_Vsa zenon_Vta=>(
NNPP _ (fun zenon_H24=>(zenon_notallex (fun zenon_Vf=>(forall zenon_Vg,(
~((abst_filter x) = (@ Datatypes.pair _ _ zenon_Vf zenon_Vg))))) (fun
zenon_H23=>(zenon_ex _ (fun zenon_Vf=>(~(forall zenon_Vg,(~((
abst_filter x) = (@ Datatypes.pair _ _ zenon_Vf zenon_Vg)))))) (fun
zenon_T_c zenon_H22=>(zenon_notallex (fun zenon_Vg=>(~((abst_filter x)
= (@ Datatypes.pair _ _ zenon_T_c zenon_Vg)))) (fun zenon_H21=>(
zenon_ex _ (fun zenon_Vg=>(~(~((abst_filter x) = (@ Datatypes.pair _ _
zenon_T_c zenon_Vg))))) (fun zenon_T_d zenon_H20=>(zenon_H20 (fun
zenon_H5=>(let zenon_H1f:=zenon_H5 in (zenon_focal_ite_rel_l (fun
zenon_Via zenon_Vja=>(zenon_Via = zenon_Vja)) (_p_V_lt x _p_minv_minv) (
@ Datatypes.pair _ _ _p_minv_minv (@ Too_low)) (if (_p_V_gt x
_p_maxv_maxv) then (@ Datatypes.pair _ _ _p_maxv_maxv (@ Too_high))
else (@ Datatypes.pair _ _ x (@ In_range))) (@ Datatypes.pair _ _
zenon_T_c zenon_T_d) (fun zenon_H18 zenon_H1e=>(zenon_G zenon_H18)) (
fun zenon_G zenon_H1d=>(zenon_focal_ite_rel_l (fun zenon_Via zenon_Vja=>
(zenon_Via = zenon_Vja)) (_p_V_gt x _p_maxv_maxv) (@ Datatypes.pair _ _
_p_maxv_maxv (@ Too_high)) (@ Datatypes.pair _ _ x (@ In_range)) (@
Datatypes.pair _ _ zenon_T_c zenon_T_d) (fun zenon_H1b zenon_H19=>(
zenon_induct_f_equal (@ Datatypes.pair _ _ _p_maxv_maxv (@ Too_high)) (
@ Datatypes.pair _ _ zenon_T_c zenon_T_d) (fun zenon_Vea=>match
zenon_Vea with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end) (fun
zenon_H6=>(let zenon_H4:=(eq_ind (@ Too_low) (fun zenon_Vj=>match
zenon_Vj with|Too_low=>True|_=>False end) I (@ Too_high)) in (zenon_L1_
zenon_T_c zenon_T_d zenon_H6 zenon_H5 zenon_H7 zenon_H4))) zenon_H19)) (
fun zenon_H1c zenon_H1a=>(zenon_induct_f_equal (@ Datatypes.pair _ _ x (
@ In_range)) (@ Datatypes.pair _ _ zenon_T_c zenon_T_d) (fun zenon_Vea=>
match zenon_Vea with|Datatypes.pair zenon_Vb zenon_Va=>zenon_Va end) (
fun zenon_H10=>(let zenon_Hf:=(eq_ind (@ Too_low) (fun zenon_Vj=>match
zenon_Vj with|Too_low=>True|_=>False end) I (@ In_range)) in (zenon_L2_
zenon_T_c zenon_T_d zenon_H10 zenon_H5 zenon_H7 zenon_Hf))) zenon_H1a))
zenon_H1d)) zenon_H1f))))) zenon_H21)) zenon_H22)) zenon_H23))
zenon_H24)) zenon_Vsa zenon_Vta)) _ (refl_equal _)))) H)))).
Qed.

        Theorem __B_1_1_LEMMA : Is_true ((_p_V_lt x _p_minv_minv)).
        assert (__force_use_H := H).
        apply for_zenon___B_1_1_LEMMA;
        auto.
        Qed.
        End __B_1_1.
(* File "trash.fcl", line 54, characters 17-50 *)
Theorem for_zenon___B_1_LEMMA:(~(Is_true (_p_V_gt x _p_minv_minv))).
Proof.
exact(
(NNPP _ (fun zenon_G=>(zenon_G (fun zenon_H3=>(zenon_all _p_V_T (fun x
:_p_V_T=>(forall y:_p_V_T,((Is_true (_p_V_lt x y))->(~(Is_true (_p_V_gt
x y)))))) x (fun zenon_H7=>(zenon_all _p_V_T (fun y:_p_V_T=>((Is_true (
_p_V_lt x y))->(~(Is_true (_p_V_gt x y))))) _p_minv_minv (fun zenon_H6=>
(zenon_imply _ _ (fun zenon_H5=>(zenon_H5 __B_1_1_LEMMA)) (fun
zenon_H4=>(zenon_H4 zenon_H3)) zenon_H6)) zenon_H7)) _p_V_lt_not_gt)))))
).
Qed.

      Theorem __B_1_LEMMA : ~Is_true ((_p_V_gt x _p_minv_minv)).
      assert (__force_use_H := H).
      apply for_zenon___B_1_LEMMA;
      auto.
      Qed.
      End __B_1.
(* File "trash.fcl", line 55, characters 12-49 *)
Theorem for_zenon_too_low_not_gt_min:(forall x:_p_V_T,((Is_true (
basics._equal_ _ (abst_get_status (abst_filter x)) (@ Too_low)))->(~(
Is_true (_p_V_gt x _p_minv_minv))))).
Proof.
exact(
(NNPP _ (fun zenon_G=>(zenon_notallex (fun x:_p_V_T=>((Is_true (
basics._equal_ _ (abst_get_status (abst_filter x)) (@ Too_low)))->(~(
Is_true (_p_V_gt x _p_minv_minv))))) (fun zenon_Hd=>(zenon_ex _p_V_T (
fun x:_p_V_T=>(~((Is_true (basics._equal_ _ (abst_get_status (
abst_filter x)) (@ Too_low)))->(~(Is_true (_p_V_gt x _p_minv_minv))))))
(fun(zenon_Tx_c:_p_V_T) zenon_Hc=>(zenon_notimply _ _ (fun zenon_Ha
zenon_Hb=>(zenon_Hb (fun zenon_H3=>(coq_builtins.zenon_syntactic_equal
zenon_focal_eqdec _ (abst_get_status (abst_filter zenon_Tx_c)) (@
Too_low) (fun zenon_H9=>(let zenon_H5:=zenon_H9 in (zenon_all _p_V_T (
fun x:_p_V_T=>((Is_true (basics._equal_ _ (basics.snd _ _ (abst_filter
x)) (@ Too_low)))->(~(Is_true (_p_V_gt x _p_minv_minv))))) zenon_Tx_c (
fun zenon_H8=>(zenon_imply _ _ (fun zenon_H7=>(
coq_builtins.zenon_not_syntactic_equal _ (basics.snd _ _ (abst_filter
zenon_Tx_c)) (@ Too_low) (fun zenon_H6=>(zenon_H6 zenon_H5)) zenon_H7))
(fun zenon_H4=>(zenon_H4 zenon_H3)) zenon_H8)) __B_1_LEMMA))) zenon_Ha))
)) zenon_Hc)) zenon_Hd)) zenon_G)))).
Qed.

    (* Dummy theorem to enforce Coq abstractions. *)
    Theorem for_zenon_abstracted_too_low_not_gt_min :
      forall x : _p_V_T,
        Is_true ((basics._equal_ _ (abst_get_status (abst_filter x))
                   (@Too_low ))) ->
          ~Is_true ((_p_V_gt x _p_minv_minv)).
    assert (__force_use_p_V_T := _p_V_T).
    assert (__force_use__p_V_lt := _p_V_lt).
    assert (__force_use__p_V_gt := _p_V_gt).
    assert (__force_use__p_V_lt_not_gt := _p_V_lt_not_gt).
    assert (__force_use__p_minv_minv := _p_minv_minv).
    assert (__force_use__p_maxv_maxv := _p_maxv_maxv).
    assert (__force_use_abst_T := abst_T).
    assert (__force_use_abst_filter := abst_filter).
    assert (__force_use_abst_get_status := abst_get_status).
    apply for_zenon_too_low_not_gt_min;
    auto.
    Qed.
    End Proof_of_too_low_not_gt_min.
  
  Theorem too_low_not_gt_min  (_p_V_T : Set) (_p_V_lt :
    _p_V_T -> _p_V_T -> basics.bool__t) (_p_V_gt :
    _p_V_T -> _p_V_T -> basics.bool__t) (_p_V_lt_not_gt :
    forall x  y : _p_V_T, Is_true ((_p_V_lt x y)) -> ~Is_true ((_p_V_gt x y)))
    (_p_minv_minv : _p_V_T) (_p_maxv_maxv : _p_V_T)
    (abst_T := ((_p_V_T * statut_t__t)%type)) (abst_filter := filter _p_V_T
    _p_V_lt _p_V_gt _p_minv_minv _p_maxv_maxv) (abst_get_status := get_status
    _p_V_T):
    forall x : _p_V_T,
      Is_true ((basics._equal_ _ (abst_get_status (abst_filter x))
                 (@Too_low ))) ->
        ~Is_true ((_p_V_gt x _p_minv_minv)).
  apply for_zenon_abstracted_too_low_not_gt_min;
  auto.
  Qed.
  Definition junk (_p_V_T : Set) (abst_T : Set)
    (abst_get_status : abst_T -> statut_t__t)
    (abst_get_value : abst_T -> _p_V_T) (x : abst_T) : basics.int__t :=
    let _a : _p_V_T :=
      (abst_get_value x)
    in
    let _b : statut_t__t :=
      (abst_get_status x)
    in
    3.
  
  (* Fully defined 'Thresholder' species's collection generator. *)
  Definition collection_create (_p_V_T : Set) _p_minv_minv _p_maxv_maxv
    _p_V_lt _p_V_gt _p_V_lt_not_gt :=
    let local_rep := ((_p_V_T * statut_t__t)%type) in
    (* From species trash#Thresholder. *)
    let local_filter := filter _p_V_T _p_V_lt _p_V_gt _p_minv_minv
      _p_maxv_maxv in
    (* From species trash#Thresholder. *)
    let local_get_status := get_status _p_V_T in
    (* From species trash#Thresholder. *)
    let local_get_value := get_value _p_V_T in
    (* From species trash#Thresholder. *)
    let local_too_low_not_gt_min := too_low_not_gt_min _p_V_T _p_V_lt _p_V_gt
      _p_V_lt_not_gt _p_minv_minv _p_maxv_maxv in
    (* From species trash#Thresholder. *)
    let local_junk := junk _p_V_T local_rep local_get_status local_get_value
      in
    mk_record (_p_V_T : Set) _p_minv_minv _p_maxv_maxv _p_V_gt local_rep
    local_filter local_get_status local_get_value local_too_low_not_gt_min
    local_junk.
  
End Thresholder.

Module IntColl.
  Let effective_collection := NativeInt.collection_create.
  (* Carrier's structure explicitly given by "rep". *)
  Definition me_as_carrier := basics.int__t.
  (* Added the argument representing the carrier, but also replaced all the
     arguments by _'s since Coq already knows them. *)
  Definition id := effective_collection.(NativeInt.rf_id _).
  Definition toto := effective_collection.(NativeInt.rf_toto _).
  Definition eq := effective_collection.(NativeInt.rf_eq _).
  Definition from_int := effective_collection.(NativeInt.rf_from_int _).
  Definition lt := effective_collection.(NativeInt.rf_lt _).
  Definition gt := effective_collection.(NativeInt.rf_gt _).
  Definition lt_not_gt := effective_collection.(NativeInt.rf_lt_not_gt _).
  
End IntColl.

Module Thresh_5_10.
  Let effective_collection := Thresholder.collection_create
    IntColl.me_as_carrier (IntColl.from_int 5) (IntColl.from_int 10)
    IntColl.lt IntColl.gt IntColl.lt_not_gt.
  (* Carrier's structure explicitly given by "rep". *)
  Definition me_as_carrier := ((IntColl.me_as_carrier * statut_t__t)%type).
  Definition filter :=
    (* Added the argument representing the carrier, but also replaced all the
       arguments by _'s since Coq already knows them. *)
    effective_collection.(Thresholder.rf_filter _ _ _ _ _ ).
  Definition get_status :=
    effective_collection.(Thresholder.rf_get_status _ _ _ _ _).
  Definition get_value :=
    effective_collection.(Thresholder.rf_get_value _ _ _ _ _).
  Definition too_low_not_gt_min :=
    effective_collection.(Thresholder.rf_too_low_not_gt_min _ _ _ _ _).
  Definition junk :=
    effective_collection.(Thresholder.rf_junk _ _ _ _ _).
  
End Thresh_5_10.

