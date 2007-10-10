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


(* $Id: param_dep_analysis.ml,v 1.4 2007-10-10 15:27:43 pessaux Exp $ *)


(* ******************************************************************** *)
(** {b Descr} : This module deals with the computation of which methods
              of a collection parameter an expression "needs" (i.e.
              depends on).
              This information is required in order to be able to
              generate the Coq/OCaml code.                              *)
(* ******************************************************************** *)



(* ********************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->      *)
(*  Parsetree.expr_ident -> Parsetree_utils.VnameSet.t                   *)
(** {b Descr} : Computes the set of methods names the identifier [ident]
              represents as invloving a dependency with the
              [param_coll_name] collection name.
              In fact, either the identifier is a method call from the
              the same species as [param_coll_name] and is counted as a
              dependency. Or it is not and then the returned set of
              dependencies is empty.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let param_deps_ident ~current_species param_coll_name ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local _
   | Parsetree.EI_global (_, _) ->
       (* These are not a method call, then they induce no dependency. *)
       Parsetree_utils.VnameSet.empty
   | Parsetree.EI_method (coll_specifier_opt, vname) ->
       (begin
       match coll_specifier_opt with
	| None ->
	    (* A method of self, then induces no dependency *)
	    (* like those were are looking for.             *)
	    Parsetree_utils.VnameSet.empty
	| Some coll_specifier ->
	    (begin
	    match coll_specifier with
	     | (None, coll_name) ->
		 (* Check it this method call is from the species parameter *)
		 (* we are working with. Should never happen because the    *)
		 (* scoping pass should make explicit the hosting module.   *)
		 if coll_name = param_coll_name then
		   Parsetree_utils.VnameSet.singleton vname
		 else Parsetree_utils.VnameSet.empty
	     | ((Some module_name), coll_name) ->
		 (begin
		 (* If the module specification matches the one of the *)
		 (* current_species and if the collection name matches *)
                 (* species parameter then we have a dependency.       *)
                 if module_name = (fst current_species) &&
                    coll_name = param_coll_name then
		   Parsetree_utils.VnameSet.singleton vname
		 else Parsetree_utils.VnameSet.empty
		 end)
	    end)
       end)
;;



(* ************************************************************************* *)
(* current_species: Parsetree.qualified_vname ->                             *)
(*   Parsetree.may_be_qualified_vname -> Parsetree.expr ->                   *)
(*   Parsetree_utils.VnameSet.t                                              *)
(** {b Descr} : Computes the dependencies of an expression on the collection
              parameter name [param_coll_name]. In other words, detects
              which methods of [param_coll_name] (that is considered as
              a collection (i.e "is") parameter), the current expression
              needs.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let param_deps_expr ~current_species param_coll_name expression =
  let rec rec_deps expr =
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self
     | Parsetree.E_const _
     | Parsetree.E_external _ -> Parsetree_utils.VnameSet.empty
     | Parsetree.E_fun (_, e_body) -> rec_deps e_body
     | Parsetree.E_var ident ->
	 param_deps_ident ~current_species param_coll_name ident
     | Parsetree.E_app (functional_expr, args_exprs) ->
	 List.fold_left
	   (fun accu_deps e ->
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps e))
	   ( rec_deps functional_expr)
	   args_exprs
     | Parsetree.E_match (matched_expr, bindings) ->
	 List.fold_left
	   (fun accu_deps (_, e) ->
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps e))
	   (rec_deps matched_expr)
	   bindings
     | Parsetree.E_if (e_cond, e_then, e_else) ->
	 let deps1 = rec_deps e_cond in
	 let deps2 = rec_deps e_then in
	 let deps3 = rec_deps e_else in
	 Parsetree_utils.VnameSet.union
	   (Parsetree_utils.VnameSet.union deps1 deps2) deps3
     | Parsetree.E_let (let_def, in_expr) ->
	 List.fold_left
	   (fun accu_deps binding ->
	     let body = binding.Parsetree.ast_desc.Parsetree.b_body in
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps body))
	   (rec_deps in_expr)
	   let_def.Parsetree.ast_desc.Parsetree.ld_bindings
     | Parsetree.E_record fields ->
	 List.fold_left
	   (fun accu_deps (_, e) ->
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps e))
	   Parsetree_utils.VnameSet.empty
	   fields
     | Parsetree.E_record_access (e, _) -> rec_deps e
     | Parsetree.E_record_with (e, labs_exprs) ->
	 List.fold_left
	   (fun accu_deps (_, e) ->
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps e))
	   (rec_deps e)
	   labs_exprs
     | Parsetree.E_constr (_, exprs)
     | Parsetree.E_tuple exprs ->
	 List.fold_left
	   (fun accu_deps e ->
	     Parsetree_utils.VnameSet.union accu_deps (rec_deps e))
	   Parsetree_utils.VnameSet.empty
	   exprs
     | Parsetree.E_paren e -> rec_deps e in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps expression
;;

