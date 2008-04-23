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

(* $Id: param_dep_analysis.ml,v 1.12 2008-04-23 13:19:28 pessaux Exp $ *)

(* ******************************************************************** *)
(** {b Descr} : This module deals with the computation of which methods
              of a collection parameter an expression "needs" (i.e.
              depends on).
              This information is required in order to be able to
              generate the Coq/OCaml code.                              *)
(* ******************************************************************** *)

open Parsetree

(* ********************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->      *)
(*  Parsetree.expr_ident -> Parsetree_utils.DepNameSet.t                 *)
(** {b Descr} : Computes the set of methods names the identifier [ident]
              represents as involving a dependency with the
              [param_coll_name] collection name.
              In fact, either the identifier is a method call from the
              the same species as [param_coll_name] and is counted as a
              dependency. Or it is not and then the returned set of
              dependencies is empty.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let param_deps_ident ~current_species param_coll_name local_idents ident =
  (* Recover the ident's type. *)
  let ident_ty =
    (match ident.Parsetree.ast_type with
     | Parsetree.ANTI_none
     | Parsetree.ANTI_non_relevant
     | Parsetree.ANTI_scheme  _ -> assert false
     | Parsetree.ANTI_type t -> t) in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local n ->
       (* Be careful. Because "in" parameters smell like regular local *)
       (* identifiers, we must check here if the current identifier is *)
       (* in fact a "in-parameter" of the species. To check this, one  *)
       (* must be careful to possible masking that could exist if a    *)
       (* really local identifier wearing the same name than a "in"    *)
       (* parameter was bound since the "in"-parameter was bound.      *)
       (* Because local bound idents can only appear in a "let"        *)
       (* EXPRESSION (not species fields because it would not be used  *)
       (* directly by its name but by "!it's name") and can't escape   *)
       (* this "let" EXPRESSION, and because they only can be more     *)
       (* recent that the species "in"-paramater definition, to know   *)
       (* is a more recent ident is wearing the same name that a       *)
       (* "in"-parameter (hence, masks it) , we just need to check if  *)
       (* it exists in the list of locally-bound idents.               *)
       if param_coll_name = n && not (List.mem n local_idents)
       then Parsetree_utils.DepNameSet.singleton (n, ident_ty)
       else Parsetree_utils.DepNameSet.empty
   | Parsetree.EI_global _ ->
       (* These are not a method call, then they induce no dependency. *)
       Parsetree_utils.DepNameSet.empty
   | Parsetree.EI_method (None, _) ->
       (* A method of self, then induces no dependency *)
       (* like those were are looking for.             *)
       Parsetree_utils.DepNameSet.empty
   | Parsetree.EI_method (Some coll_specifier, vname) ->
       (begin
        match coll_specifier with
        | Vname coll_name ->
          (* Check it this method call is from the species parameter *)
          (* we are working with. Should never happen because the    *)
          (* scoping pass should make explicit the hosting module.   *)
          if coll_name = param_coll_name then
            Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
          else Parsetree_utils.DepNameSet.empty
        | Qualified (module_name, coll_name) ->
          (* If the module specification matches the one of the *)
          (* current_species and if the collection name matches *)
          (* species parameter then we have a dependency.       *)
          if module_name = fst current_species &&
             coll_name = param_coll_name
          then Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
          else Parsetree_utils.DepNameSet.empty
       end)
;;



(* ************************************************************************ *)
(** {b Descr} : Basically really does the job of [param_deps_expr] but
      has the extra parameter [start_local_idents] allowing to start
      with a non-empty list of identifiers considered as local.
      This is needed for [__param_deps_logical_expr] that needs to call
      ourselves with its already accumulated list of local identifiers.
      However, outside this module, the exported function [param_deps_expr]
      does not have any local identifier list as parameter because it
      must never be called in a context where there would already exists
      local identifiers.

  {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************ *)
let rec __param_deps_expr ~current_species param_coll_name start_local_idents
    expression =
  let rec rec_deps local_idents expr =
    match expr.Parsetree.ast_desc with
    | Parsetree.E_self
    | Parsetree.E_const _
    | Parsetree.E_external _ -> Parsetree_utils.DepNameSet.empty
    | Parsetree.E_fun (bound_name, e_body) ->
        (* Here, the function parameter name may mask a "in"-parameter. *)
        rec_deps (bound_name @ local_idents) e_body
    | Parsetree.E_var ident ->
        param_deps_ident ~current_species param_coll_name local_idents ident
    | Parsetree.E_app (functional_expr, args_exprs) ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.DepNameSet.union
              accu_deps (rec_deps local_idents e))
          (rec_deps local_idents functional_expr)
          args_exprs
    | Parsetree.E_match (matched_expr, bindings) ->
        List.fold_left
          (fun accu_deps (pat, e) ->
            (* Here, each name of the pattern may mask a "in"-parameter. *)
            let local_idents' =
              (Parsetree_utils.get_local_idents_from_pattern pat) @
              local_idents in
            Parsetree_utils.DepNameSet.union
              accu_deps (rec_deps local_idents' e))
          (rec_deps local_idents matched_expr)
          bindings
    | Parsetree.E_if (e_cond, e_then, e_else) ->
        let deps1 = rec_deps local_idents e_cond in
        let deps2 = rec_deps local_idents e_then in
        let deps3 = rec_deps local_idents e_else in
        Parsetree_utils.DepNameSet.union
          (Parsetree_utils.DepNameSet.union deps1 deps2) deps3
    | Parsetree.E_let (let_def, in_expr) ->
        List.fold_left
          (fun accu_deps binding ->
            (* Here, each parameter name of the   *)
            (* binding may mask a "in"-parameter. *)
            let local_idents' =
              (List.map fst binding.Parsetree.ast_desc.Parsetree.b_params) @
              local_idents in
            let deps =
              (match binding.Parsetree.ast_desc.Parsetree.b_body with
               | Parsetree.BB_logical p ->
                   __param_deps_logical_expr
                     ~current_species param_coll_name local_idents' p
               | Parsetree.BB_computational e -> rec_deps local_idents' e) in
            Parsetree_utils.DepNameSet.union accu_deps deps)
          (rec_deps local_idents in_expr)
          let_def.Parsetree.ast_desc.Parsetree.ld_bindings
    | Parsetree.E_record fields ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.DepNameSet.union
              accu_deps (rec_deps local_idents e))
          Parsetree_utils.DepNameSet.empty
          fields
    | Parsetree.E_record_access (e, _) -> rec_deps local_idents e
    | Parsetree.E_record_with (e, labs_exprs) ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.DepNameSet.union
              accu_deps (rec_deps local_idents e))
          (rec_deps local_idents e)
          labs_exprs
    | Parsetree.E_constr (_, exprs)
    | Parsetree.E_tuple exprs ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.DepNameSet.union
              accu_deps (rec_deps local_idents e))
          Parsetree_utils.DepNameSet.empty
          exprs
    | Parsetree.E_paren e -> rec_deps local_idents e in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps start_local_idents expression



and __param_deps_logical_expr ~current_species param_coll_name
    start_local_idents proposition =
  let rec rec_deps local_idents logical_expr =
    match logical_expr.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, _, logical_expr')
     | Parsetree.Pr_exists (vnames, _, logical_expr') ->
         (* Here, the quantifid names may mask a "in"-parameter. *)
        rec_deps (vnames @ local_idents) logical_expr'
     | Parsetree.Pr_imply (logical_expr1, logical_expr2)
     | Parsetree.Pr_or (logical_expr1, logical_expr2)
     | Parsetree.Pr_and (logical_expr1, logical_expr2)
     | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
         let deps1 = rec_deps local_idents logical_expr1 in
         let deps2 = rec_deps local_idents logical_expr2 in
         Parsetree_utils.DepNameSet.union deps1 deps2
     | Parsetree.Pr_not logical_expr' -> rec_deps local_idents logical_expr'
     | Parsetree.Pr_expr expr ->
         __param_deps_expr ~current_species param_coll_name local_idents expr
     | Parsetree.Pr_paren logical_expr' ->
         rec_deps local_idents logical_expr' in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps start_local_idents proposition
 ;;



(* ************************************************************************* *)
(* current_species: Parsetree.qualified_species -> Parsetree.vname ->        *)
(*   Parsetree.expr -> Parsetree_utils.DepNameSet.t                          *)
(** {b Descr} : Computes the dependencies of an expression on the collection
              parameter name [param_coll_name]. In other words, detects
              which methods of [param_coll_name] (that is considered as
              a collection (i.e "is") parameter), the current expression
              needs.

    {b Args}:
      - ~current_species : The name (module + effective name) of the
          currently analyzed species, i.e. the species where we asked for
          the dependencies to be computed.
      - param_coll_name : The name of the species parameter we want to
          to detect dependencies to in the expression.
      - expression : The expression in which we want to detect possible
          dependencies on the species parameter whose name is
          [param_coll_name].

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let param_deps_expr ~current_species param_coll_name expression =
  __param_deps_expr ~current_species param_coll_name [] expression
;;



(* current_species: Parsetree.qualified_species -> Parsetree.vname -> *)
(*   Parsetree.logical_expr -> Parsetree_utils.DepNameSet.t                   *)
let param_deps_logical_expr ~current_species param_coll_name proposition =
  __param_deps_logical_expr ~current_species param_coll_name [] proposition
;;


