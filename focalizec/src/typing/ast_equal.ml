(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: ast_equal.ml,v 1.3 2007-11-06 10:14:58 pessaux Exp $ *)

(* ********************************************************************** *)
(** {b Descr} : This module performs test equality of the AST expression.
              This feature is mostly needed to compare [prop]s when
              making the fields fusion of properties and theorems fields.
              It is unclear whether the physical equality would be
              sufficient. Hence we implement here a custom structural
              equality to determine if 2 expressions have the same
              form.
              In particular, we are only interested in comparing only
              the [Parsetree.ast_desc] field.
              Currently, the comparison is done on the skeleton of the
              expression WITHOUT alpha-conversion equivalence.            *)
(* ********************************************************************** *)



(* ****************************************************** *)
(* val ident : Parsetree.ident -> Parsetree.ident -> bool *)
(** {b Descr} : Tests the equality of 2 [ident].

    {b Rem} : Not exported outside this module.           *)
(* ****************************************************** *)
let ident (ident1 : Parsetree.ident) (ident2 : Parsetree.ident) =
  (* Because scoping has been done, we can safely perform a simple structural *)
  (* equality on the [Parsetree.ast_desc] field of the [ident] .              *)
  ident1.Parsetree.ast_desc = ident2.Parsetree.ast_desc
;;



(* **************************************************************** *)
(* val ident : Parsetree.expr_ident -> Parsetree.expr_ident -> bool *)
(** {b Descr} : Tests the equality of 2 [ident].

    {b Rem} : Not exported outside this module.                     *)
(* **************************************************************** *)
let expr_ident (ident1 : Parsetree.expr_ident) (ident2 : Parsetree.expr_ident) =
  (* Because scoping has been done, we can safely perform a simple structural *)
  (* equality on the [Parsetree.ast_desc] field of the [ident] .              *)
  ident1.Parsetree.ast_desc = ident2.Parsetree.ast_desc
;;



(* ****************************************************************** *)
(* Parsetree.constructor_ident -> Parsetree.constructor_ident -> bool *)
(** {b Descr} : Tests the equality of 2 [constructor_expr]s.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let constructor_ident
    (cexpr1 : Parsetree.constructor_ident)
    (cexpr2  : Parsetree.constructor_ident) =
  (* Constructor expressions only contain string and sum types. Nothing *)
  (* requiring sharing or physical stuff. It is then safe to apply a    *)
  (* simple structural equality.                                        *)
  cexpr1.Parsetree.ast_desc = cexpr2.Parsetree.ast_desc
;;



(* ********************************************************** *)
(* Parsetree.external_expr -> Parsetree.external_expr -> bool *)
(** {b Descr} : Tests the equality of 2 [external_expr]s.

    {b Rem} : Not exported outside this module.               *)
(* ********************************************************** *)
let external_expr
    (extern_expr1 : Parsetree.external_expr)
    (extern_expr2 : Parsetree.external_expr) =
  (* External expressions only contain string and sum types. Nothing *)
  (* requiring sharing or physical stuff. It is then safe to apply a *)
  (* simple structural equality.                                     *)
  extern_expr1.Parsetree.ast_desc = extern_expr2.Parsetree.ast_desc
;;



(* ************************************************ *)
(* Parsetree.constant -> Parsetree.constant -> bool *)
(** {b Descr} : Tests the equality of 2 [constant]s.

    {b Rem} : Not exported outside this module.     *)
(* ************************************************ *)
let constant (cst1 : Parsetree.constant) (cst2 : Parsetree.constant) =
  (* Constants only contain string and sum types. Nothing requiring *)
  (* sharing or physical stuff. It is then safe to apply a simple   *)
  (* structural equality.                                           *)
  cst1.Parsetree.ast_desc = cst2.Parsetree.ast_desc
;;



(* *********************************************** *)
(* Parsetree.pattern -> Parsetree.pattern -> bool  *)
(** {b Descr} : Tests the equality of 2 [pattern]s.

    {b Rem} : Not exported outside this module.    *)
(* *********************************************** *)
let rec pattern pattern1 pattern2 =
  match (pattern1.Parsetree.ast_desc, pattern2.Parsetree.ast_desc) with
   | ((Parsetree.P_const c1), (Parsetree.P_const c2)) -> constant c1 c2
   | ((Parsetree.P_var n1), (Parsetree.P_var n2)) -> n1 = n2
   | ((Parsetree.P_as (p1, n1)), (Parsetree.P_as (p2, n2))) ->
       (pattern p1 p2) && (n1 = n2)
   | (Parsetree.P_wild, Parsetree.P_wild) -> true
   | ((Parsetree.P_constr (id1, pats1)), (Parsetree.P_constr (id2, pats2))) ->
       (constructor_ident id1 id2) && (List.for_all2 pattern pats1 pats2)
   | ((Parsetree.P_record labs_pats1), (Parsetree.P_record labs_pats2)) ->
       List.for_all2
         (fun (lab1, pat1) (lab2, pat2) -> (lab1 = lab2) && (pattern pat1 pat2))
         labs_pats1 labs_pats2
   | ((Parsetree.P_tuple pats1), (Parsetree.P_tuple pats2)) ->
       List.for_all2 pattern pats1 pats2
   | ((Parsetree.P_paren p1), (Parsetree.P_paren p2)) -> pattern p1 p2
   | (_, (Parsetree.P_paren p2)) ->
       (* Consider that parentheses are non-significant. *)
       pattern pattern1 p2
   | ((Parsetree.P_paren p1), _) ->
       (* Consider that parentheses are non-significant. *)
       pattern p1 pattern2
   | (_, _) -> false
;;



(* ************************************************** *)
(* Parsetree.type_expr -> Parsetree.type_expr -> bool *)
(** {b Descr} : Tests the equality of 2 [type_expr]s.

    {b Rem} : Not exported outside this module.       *)
(* ************************************************** *)
let rec type_expr ty_expr1 ty_expr2 =
  match (ty_expr1.Parsetree.ast_desc, ty_expr2.Parsetree.ast_desc) with
   | ((Parsetree.TE_ident id1), (Parsetree.TE_ident id2)) -> ident id1 id2
   | ((Parsetree.TE_fun (te1, te1')), (Parsetree.TE_fun (te2, te2'))) ->
       (type_expr te1 te2) && (type_expr te1' te2')
   | ((Parsetree.TE_app (id1, tes1)), (Parsetree.TE_app (id2, tes2))) ->
       (ident id1 id2) && (List.for_all2 type_expr tes1 tes2)
   | ((Parsetree.TE_prod tes1), (Parsetree.TE_prod tes2)) ->
       List.for_all2 type_expr tes1 tes2
   | (Parsetree.TE_self, Parsetree.TE_self)
   | (Parsetree.TE_prop, Parsetree.TE_prop) -> true
   | ((Parsetree.TE_paren te1), (Parsetree.TE_paren te2)) -> type_expr te1 te2
   | (_, (Parsetree.TE_paren te2)) ->
       (* Consider that parentheses are non-significant. *)
       type_expr ty_expr1 te2
   | ((Parsetree.TE_paren te1), _) ->
       (* Consider that parentheses are non-significant. *)
       type_expr te1 ty_expr2
   | (_, _) -> false
;;



(* ******************************************** *)
(* Parsetree.expr -> Parsetree.expr -> bool     *)
(** {b Descr} : Tests the equality of 2 [expr]s.

    {b Rem} : Not exported outside this module. *)
(* ******************************************** *)
let rec expr expression1 expression2 =
  match (expression1.Parsetree.ast_desc, expression2.Parsetree.ast_desc) with
   | (Parsetree.E_self, Parsetree.E_self) -> true
   | ((Parsetree.E_const c1), (Parsetree.E_const c2)) -> constant c1 c2
   | ((Parsetree.E_fun (vnames1, e1)), (Parsetree.E_fun (vnames2, e2))) ->
       (vnames1 = vnames2) && (expr e1 e2)
   | ((Parsetree.E_var id1), (Parsetree.E_var id2)) -> expr_ident id1 id2
   | ((Parsetree.E_app (e1, es1)), (Parsetree.E_app (e2, es2))) ->
       (expr e1 e2) && (List.for_all2 expr es1 es2)
   | ((Parsetree.E_constr (ce1, es1)), (Parsetree.E_constr (ce2, es2))) ->
       (constructor_ident ce1 ce2) && (List.for_all2 expr es1 es2)
   | ((Parsetree.E_match (e1, pats_es1)), (Parsetree.E_match (e2, pats_es2))) ->
       (expr e1 e2) &&
       (List.for_all2
          (fun (pat1, e1) (pat2, e2) -> (pattern pat1 pat2) && (expr e1 e2))
          pats_es1 pats_es2)
   | ((Parsetree.E_if (e1, e1', e1'')), (Parsetree.E_if (e2, e2', e2''))) ->
       (expr e1 e2) && (expr e1' e2') && (expr e1'' e2'')
   | ((Parsetree.E_let (ldef1, e1)), (Parsetree.E_let (ldef2, e2))) ->
       (let_def ldef1 ldef2) && (expr e1 e2)
   | ((Parsetree.E_record labs_es1), (Parsetree.E_record labs_es2)) ->
       List.for_all2
         (fun (lab1, e1) (lab2, e2) -> (lab1 = lab2) && (expr e1 e2))
         labs_es1 labs_es2
   | ((Parsetree.E_record_access (e1, lab1)),
      (Parsetree.E_record_access (e2, lab2))) ->
        (expr e1 e2) && (lab1 = lab2)
   | ((Parsetree.E_record_with (e1, labs_es1)),
      (Parsetree.E_record_with (e2, labs_es2))) ->
        (expr e1 e2) &&
        (List.for_all2
           (fun (lab1, e1) (lab2, e2) -> (lab1 = lab2) && (expr e1 e2))
           labs_es1 labs_es2)
   | ((Parsetree.E_tuple es1), (Parsetree.E_tuple es2)) ->
       List.for_all2 expr es1 es2
   | ((Parsetree.E_external ee1), (Parsetree.E_external ee2)) ->
       external_expr ee1 ee2
   | ((Parsetree.E_paren e1), (Parsetree.E_paren e2)) ->
       expr e1 e2
   | (_, (Parsetree.E_paren e2)) ->
       (* Consider that parentheses are non-significant. *)
       expr expression1 e2
   | ((Parsetree.E_paren e1), _) ->
       (* Consider that parentheses are non-significant. *)
       expr e1 expression2
   | (_, _) -> false



(* ************************************************ *)
(* Parsetree.let_def -> Parsetree.let_def -> bool   *)
(** {b Descr} : Tests the equality of 2 [let_def]s.

    {b Rem} : Not exported outside this module.     *)
(* ************************************************ *)
and let_def let_definition1 let_definition2 =
  let let_definition1_desc = let_definition1.Parsetree.ast_desc in
  let let_definition2_desc = let_definition2.Parsetree.ast_desc in
  (let_definition1_desc.Parsetree.ld_rec =
   let_definition2_desc.Parsetree.ld_rec)
  &&
  (let_definition1_desc.Parsetree.ld_logical =
   let_definition2_desc.Parsetree.ld_logical)
  &&
  (let_definition1_desc.Parsetree.ld_local =
   let_definition2_desc.Parsetree.ld_local)
  &&
  (List.for_all2
     bindind let_definition1_desc.Parsetree.ld_bindings
     let_definition2_desc.Parsetree.ld_bindings)



(* ************************************************ *)
(* Parsetree.binding -> Parsetree.binding -> bool   *)
(** {b Descr} : Tests the equality of 2 [binding]s.

    {b Rem} : Not exported outside this module.     *)
(* ************************************************ *)
and bindind bnd1 bnd2 =
  let bnd1_desc = bnd1.Parsetree.ast_desc in
  let bnd2_desc = bnd2.Parsetree.ast_desc in
  (bnd1_desc.Parsetree.b_name = bnd2_desc.Parsetree.b_name)
  &&
  (List.for_all2
     (fun (n1, te_opt1) (n2, te_opt2) ->
       (n1 = n2)
       &&
       (match (te_opt1, te_opt2) with
        | (None, None) -> true
        | ((Some te1), (Some te2)) -> type_expr te1 te2
        | (_, _) -> false))
     bnd1_desc.Parsetree.b_params bnd2_desc.Parsetree.b_params)
  &&
  (expr bnd1_desc.Parsetree.b_body bnd2_desc.Parsetree.b_body)
;;



(* ********************************************* *)
(* Parsetree.prop -> Parsetree.prop -> bool      *)
(** {b Descr} : Tests the equality of 2 [prop]s.

    {b Rem} : Exported outside this module.      *)
(* ********************************************* *)
let rec prop prop1 prop2 =
  match (prop1.Parsetree.ast_desc, prop2.Parsetree.ast_desc) with
   | ((Parsetree.Pr_forall (vnames1, ty_expr1, p1)),
      (Parsetree.Pr_forall (vnames2, ty_expr2, p2)))
   | ((Parsetree.Pr_exists (vnames1, ty_expr1, p1)),
      (Parsetree.Pr_exists (vnames2, ty_expr2, p2))) ->
        vnames1 = vnames2 && (type_expr ty_expr1 ty_expr2) && (prop p1 p2)
   | ((Parsetree.Pr_imply (p1, p1')), (Parsetree.Pr_imply (p2, p2')))
   | ((Parsetree.Pr_or (p1, p1')), (Parsetree.Pr_or (p2, p2')))
   | ((Parsetree.Pr_and (p1, p1')), (Parsetree.Pr_and (p2, p2')))
   | ((Parsetree.Pr_equiv (p1, p1')), (Parsetree.Pr_equiv (p2, p2'))) ->
       (prop p1 p2) && (prop p1' p2')
   | ((Parsetree.Pr_not p1), (Parsetree.Pr_not p2)) -> prop p1 p2
   | ((Parsetree.Pr_expr e1), (Parsetree.Pr_expr e2)) -> expr e1 e2
   | ((Parsetree.Pr_paren p1), (Parsetree.Pr_paren p2)) -> prop p1 p2
   | ((Parsetree.Pr_paren p1), _) ->
       (* Consider that parentheses are non-significant. *)
       prop p1 prop2
   | (_, (Parsetree.Pr_paren p2)) ->
       (* Consider that parentheses are non-significant. *)
       prop prop1 p2
   | (_, _) -> false
;;
