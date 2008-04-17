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

(* $Id: rec_let_gen.ml,v 1.1 2008-04-17 17:02:25 pessaux Exp $ *)



(** - [~in_species] : Option telling if we are in a species or not
    (i.e. otherwise, in a toplevel function). If we are in a species
    then recursion will be detected only on idents that are [EI_method].
    Otherwise, recursion will be detected only on [EI_local] idents. *)
let find_recursion_expr_ident name local_idents ~in_species ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local n ->
       (* [Unsure] A optimiser car quand on est dans une fct toplevel,
          comme on ne peut récurser que sur un local ident, c'est lorsque
          l'on étend les local_idents qu'ils suffit de vérifier que ce que
          l'on rajoute en étendant ne vient pas masquer [name]. Et dans
          le cas où on est dans une espèce, ben on ne peut récurser que par
          un [EI_method]. *)
       (in_species = None) && (n = name) && (not (List.mem name local_idents))
   | Parsetree.EI_global _ -> false
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       if in_species = None then false
       else
         (begin
         match coll_specifier_opt with
          | None
          | Some (Parsetree.Vname (Parsetree.Vuident "Self")) ->
              (begin
              (* Method call from the current species. Note that the fact *)
              (* that [in_species <> None] is already tested above.       *)
              vname = name
              end)
          | Some _ ->
              (* Method call from a species that is not the current. Hence *)
              (* no possible recursion accross species frontiers.          *)
              false
         end)
;;



(** - [~in_species] : Optiontelling if we are in a species or not
    (i.e. otherwise, in a toplevel function). If we are in a species
    then recursion will be detected only on idents that are [EI_method].
    Otherwise, recursion will be detected only on [EI_local] idents.
    - [current_cond_string] : The list of conditionnal expressions we are
    currently under.
    - [local_idents] : The list of local identifiers defined in the scope.
    They are recorded to ensure that if one of them wears the same name
    than the recursive function, and hence masks it, then we won't confuse
    them and won't think that there is a recursive call. *)
let rec find_recursion_expr _name ~in_species: _ _current_cond_string _local_idents
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
       if find_recursion_expr_ident name local_idents ~in_species ident then
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


let rec find_recursion_prop _name ~in_species: _ _current_cond_string
    _local_idents _log_expr =
  [] ;;


let generate_termination_lemmas _ctx _print_ctx _env ~in_species name body =
  (* We first recovers the strings of conditionnals under which a    *)
  (* recursive call appears, with it effective argument expressions. *)
  let _cond_strings =
    match body with
     | Parsetree.BB_logical log_expr ->
         find_recursion_prop name ~in_species [] [] log_expr
     | Parsetree.BB_computational expr ->
         find_recursion_expr name ~in_species [] [] expr in
  ()
;;

      
