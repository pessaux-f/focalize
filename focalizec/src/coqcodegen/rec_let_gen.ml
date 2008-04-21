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

(* $Id: rec_let_gen.ml,v 1.2 2008-04-21 14:46:33 pessaux Exp $ *)



(** - Recursion will be detected only on idents that are [EI_local] idents. *)
let find_recursion_expr_ident name local_idents ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local n ->
       (n = name) && (not (List.mem name local_idents))
   | Parsetree.EI_global _ -> false
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
        | None
        | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
            (begin
            (* Method call from the current species. *)
            vname = name
            end)
        | Some _ ->
            (* Method call from a species that is not the current. Hence *)
            (* no possible recursion accross species frontiers.          *)
            false
       end)
;;



(** - Recursion will be detected only on idents that are [EI_local] idents.
    - [current_cond_string] : The list of conditionnal expressions we are
    currently under.
    - [local_idents] : The list of local identifiers defined in the scope.
    They are recorded to ensure that if one of them wears the same name
    than the recursive function, and hence masks it, then we won't confuse
    them and won't think that there is a recursive call. *)
let rec find_recursion_expr _name _current_cond_string _local_idents
    _expression =
(*
  match expression.Parsetree.ast_desc with
   | Parsetree.E_self -> []
   | Parsetree.E_const _ -> []
   | Parsetree.E_fun (vnames, body) ->
       (* Record the newly appeared local identifiers. *)
       let local_idents' = vnames @ local_idents in
       find_recursion_expr name current_cond_string local_idents' body
   | Parsetree.E_var ident ->
       if find_recursion_expr_ident name local_idents ident then
         ...
       else []
   | Parsetree.E_app (fun_expr, args_exprs) ->
   | Parsetree.E_constr (cstr_ident, args_exprs) ->
   | Parsetree.E_match (e, pats_exprs) ->
   | Parsetree.E_if (if_expr, then_expr, else_expr) ->
   | Parsetree.E_let (let_def, in_expr) ->
   | Parsetree.E_record labels_exprs ->
   | Parsetree.E_record_access (e, label) ->
   | Parsetree.E_record_with (e, labels_exprs) ->
   | Parsetree.E_tuple exprs ->
   | Parsetree.E_external _ ->
   | Parsetree.E_paren e ->
*) []
;;


let rec find_recursion_prop _name _current_cond_string
    _local_idents _log_expr =
  [] ;;


let generate_termination_lemmas _ctx _print_ctx _env name body =
  (* We first recovers the strings of conditionnals under which a    *)
  (* recursive call appears, with it effective argument expressions. *)
  let _cond_strings =
    match body with
     | Parsetree.BB_logical log_expr ->
         find_recursion_prop name [] [] log_expr
     | Parsetree.BB_computational expr ->
         find_recursion_expr name [] [] expr in
  ()
;;

      
