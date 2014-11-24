(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre-Nicolas Tollitte                                         *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)



(** {b Descr}: Exceptions for pattern matching analysis. *)
exception Match_not_exhaustive of Location.t ;;
exception Match_useless_case of Location.t ;;

(* ************************************************************************** *)
(** {b Descr}: Search for pattern matching expressions in the AST.

    {b Visibility}: Expored outside this module.                              *)
(* ************************************************************************** *)
let rec verify_matchings ~current_unit typing_env stuff_to_compile =
  match stuff_to_compile with
  | Infer.PCM_species (species_def, _, _, _) ->
      verify_matchings_species ~current_unit typing_env species_def
  | Infer.PCM_let_def (let_def, _) ->
      verify_matchings_let ~current_unit typing_env let_def
  | Infer.PCM_expr expr -> verify_matchings_expr ~current_unit typing_env expr
  | _ -> ()



and verify_matchings_species ~current_unit typing_env species_def =
  let species_def_desc = species_def.Parsetree.ast_desc in
  if Configuration.get_verbose () then
    Format.eprintf
      "Checking pattern-matching exhaustivity in species '%a'.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name ;
  List.iter
    (fun spe_field ->
      match spe_field.Parsetree.ast_desc with
      | Parsetree.SF_let let_def ->
          verify_matchings_let ~current_unit typing_env let_def
      | _ -> ())
    species_def_desc.Parsetree.sd_fields



and verify_matchings_let ~current_unit typing_env let_def =
  List.iter
    (fun binding ->
      match binding.Parsetree.ast_desc.Parsetree.b_body with
      | Parsetree.BB_logical logical_expr ->
          verify_matchings_logexpr ~current_unit typing_env logical_expr
      | Parsetree.BB_computational expr ->
          verify_matchings_expr ~current_unit typing_env expr)
    let_def.Parsetree.ast_desc.Parsetree.ld_bindings



and verify_matchings_logexpr ~current_unit typing_env logical_expr =
  match logical_expr.Parsetree.ast_desc with
    | Parsetree.Pr_forall (_ ,_ , logical_expr)
    | Parsetree.Pr_exists (_ , _, logical_expr)
    | Parsetree.Pr_not logical_expr | Parsetree.Pr_paren logical_expr ->
        verify_matchings_logexpr ~current_unit typing_env logical_expr
    | Parsetree.Pr_imply (le1, le2) | Parsetree.Pr_or (le1, le2)
    | Parsetree.Pr_and (le1, le2) | Parsetree.Pr_equiv (le1, le2) ->
        verify_matchings_logexpr ~current_unit typing_env le1 ;
        verify_matchings_logexpr ~current_unit typing_env le2
  | Parsetree.Pr_expr expr ->
      verify_matchings_expr ~current_unit typing_env expr



and verify_matchings_expr ~current_unit typing_env expr =
  match expr.Parsetree.ast_desc with
  | Parsetree.E_paren expr | Parsetree.E_record_access (expr, _)
  | Parsetree.E_fun (_, expr) ->
      verify_matchings_expr ~current_unit typing_env expr
  | Parsetree.E_app (expr, expr_list) ->
      verify_matchings_expr ~current_unit typing_env expr ;
      List.iter (verify_matchings_expr ~current_unit typing_env) expr_list
  | Parsetree.E_tuple expr_list
  | Parsetree.E_sequence expr_list
  | Parsetree.E_constr (_, expr_list) ->
      List.iter (verify_matchings_expr ~current_unit typing_env) expr_list
  | Parsetree.E_if (expr1, expr2, expr3) ->
      verify_matchings_expr ~current_unit typing_env expr1 ;
      verify_matchings_expr ~current_unit typing_env expr2 ;
      verify_matchings_expr ~current_unit typing_env expr3
  | Parsetree.E_let (let_def, expr) ->
      verify_matchings_let ~current_unit typing_env let_def ;
      verify_matchings_expr ~current_unit typing_env expr
  | Parsetree.E_record label_expr_list ->
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        label_expr_list
  | Parsetree.E_record_with (expr, label_expr_list) ->
      verify_matchings_expr ~current_unit typing_env expr ;
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        label_expr_list
  | Parsetree.E_match (expr2, pattern_expr_list) ->
      (* verify ~current_unit typing_env expr ;*)  (* Disabled for release. *)
      verify_matchings_expr ~current_unit typing_env expr2 ;
      List.iter
        (fun (_, e) -> verify_matchings_expr ~current_unit typing_env e)
        pattern_expr_list
  | _ -> ()
;;
