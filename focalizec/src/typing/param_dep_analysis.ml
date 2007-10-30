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

(* $Id: param_dep_analysis.ml,v 1.6 2007-10-30 21:15:07 weis Exp $ *)

(* ******************************************************************** *)
(** {b Descr} : This module deals with the computation of which methods
              of a collection parameter an expression "needs" (i.e.
              depends on).
              This information is required in order to be able to
              generate the Coq/OCaml code.                              *)
(* ******************************************************************** *)

open Parsetree;;

(* ********************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->      *)
(*  Parsetree.expr_ident -> Parsetree_utils.VnameSet.t                   *)
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
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local n ->
       (* Be careful. Because "is" parameters smell like regular local *)
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
       then Parsetree_utils.VnameSet.singleton n
       else Parsetree_utils.VnameSet.empty
   | Parsetree.EI_global _ ->
       (* These are not a method call, then they induce no dependency. *)
       Parsetree_utils.VnameSet.empty
   | Parsetree.EI_method (None, _) ->
       (* A method of self, then induces no dependency *)
       (* like those were are looking for.             *)
       Parsetree_utils.VnameSet.empty
   | Parsetree.EI_method (Some coll_specifier, vname) ->
       (begin
        match coll_specifier with
        | Vname coll_name ->
          (* Check it this method call is from the species parameter *)
          (* we are working with. Should never happen because the    *)
          (* scoping pass should make explicit the hosting module.   *)
          if coll_name = param_coll_name then
            Parsetree_utils.VnameSet.singleton vname
          else Parsetree_utils.VnameSet.empty
        | Qualified (module_name, coll_name) ->
          (* If the module specification matches the one of the *)
          (* current_species and if the collection name matches *)
          (* species parameter then we have a dependency.       *)
          if module_name = fst current_species &&
             coll_name = param_coll_name
          then Parsetree_utils.VnameSet.singleton vname
          else Parsetree_utils.VnameSet.empty
       end)
;;



(* ************************************************************************* *)
(* current_species: Parsetree.qualified_vname ->                             *)
(*   Parsetree.qualified_vname -> Parsetree.expr ->                          *)
(*   Parsetree_utils.VnameSet.t                                              *)
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
  let rec rec_deps local_idents expr =
    match expr.Parsetree.ast_desc with
    | Parsetree.E_self
    | Parsetree.E_const _
    | Parsetree.E_external _ -> Parsetree_utils.VnameSet.empty
    | Parsetree.E_fun (bound_name, e_body) ->
        (* Here, the function parameter name may mask a "in"-parameter. *)
        rec_deps (bound_name @ local_idents) e_body
    | Parsetree.E_var ident ->
        param_deps_ident ~current_species param_coll_name local_idents ident
    | Parsetree.E_app (functional_expr, args_exprs) ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.VnameSet.union accu_deps (rec_deps local_idents e))
          (rec_deps local_idents functional_expr)
          args_exprs
    | Parsetree.E_match (matched_expr, bindings) ->
        List.fold_left
          (fun accu_deps (pat, e) ->
            (* Here, each name of the pattern may mask a "in"-parameter. *)
            let local_idents' =
              (Parsetree_utils.get_local_idents_from_pattern pat) @
              local_idents in
            Parsetree_utils.VnameSet.union
              accu_deps (rec_deps local_idents' e))
          (rec_deps local_idents matched_expr)
          bindings
    | Parsetree.E_if (e_cond, e_then, e_else) ->
        let deps1 = rec_deps local_idents e_cond in
        let deps2 = rec_deps local_idents e_then in
        let deps3 = rec_deps local_idents e_else in
        Parsetree_utils.VnameSet.union
          (Parsetree_utils.VnameSet.union deps1 deps2) deps3
    | Parsetree.E_let (let_def, in_expr) ->
        List.fold_left
          (fun accu_deps binding ->
            (* Here, each parameter name of the   *)
            (* binding may mask a "in"-parameter. *)
            let local_idents' =
              (List.map fst binding.Parsetree.ast_desc.Parsetree.b_params) @
              local_idents in
            let body = binding.Parsetree.ast_desc.Parsetree.b_body in
            Parsetree_utils.VnameSet.union
              accu_deps (rec_deps local_idents' body))
          (rec_deps local_idents in_expr)
          let_def.Parsetree.ast_desc.Parsetree.ld_bindings
    | Parsetree.E_record fields ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.VnameSet.union accu_deps (rec_deps local_idents e))
          Parsetree_utils.VnameSet.empty
          fields
    | Parsetree.E_record_access (e, _) -> rec_deps local_idents e
    | Parsetree.E_record_with (e, labs_exprs) ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.VnameSet.union accu_deps (rec_deps local_idents e))
          (rec_deps local_idents e)
          labs_exprs
    | Parsetree.E_constr (_, exprs)
    | Parsetree.E_tuple exprs ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.VnameSet.union accu_deps (rec_deps local_idents e))
          Parsetree_utils.VnameSet.empty
          exprs
    | Parsetree.E_paren e -> rec_deps local_idents e in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps [] expression
;;

