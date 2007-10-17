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


(* $Id: scoping.ml,v 1.32 2007-10-17 11:45:28 pessaux Exp $ *)

(* *********************************************************************** *)
(** {b Desc} : Scoping phase is intended to disambiguate identifiers.
             Hence, this means that only [I_local] [ident]s will be
             affected by the scoping transformation.
             Local [ident]s will be looked-up to determine whether they
             are really local or are method names or toplevel (of a file)
             names.

             For [idents] already disamguated, there are 2 cases:
              #-ed idents
              !-ed idents

	       - For #-ed idents, the lookup is performed and they are
	       always explicitly replaced with the name of the hosting
	       file where they are bound. Hence in a compilation unit
               "Kikoo", then [#test ()] will be replaced by [Kikoo#test ()]
               if the function [test] was really found inside this unit.
               If not found, then an exception is raised.

	       - For !-ed idents, the lookup is performed but no change
	       is done. If it is like [!test()], then it is NOT changed
               to [Self!test] !!! Only a verification is done that the
               method exists in [Self]. If it is like [Coll!test], then
               also only a verification is done that the method exists in
              [Coll].

             The transformation is not performed in place. Instead, we
             return a fresh AST (still possibly having sharing with the
             original one) that will be suitable for the typechecking
	     phase.                                                        *)
(* *********************************************************************** *)



(* ************************************************************************ *)
(** {b Descr} : Exception raised when a module is "use"-d (i.e. with the
              [use] FoCaL directive) several times in the same source file.
              Although this is not a damn, it reveals a useless idom, then
              we prefer to reject it.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception Multiply_used_module of
  Types.fname  (** The name of the module used several times. *)
;;



(* ************************************************************************* *)
(** {b Descr} : Exception raised when a module is "open"-ed (i.e. with the
              [open] FoCaL directive) or directly invoked with the #-notation
              without having be "use"-d (i.e. with the [use] FoCaL directive)
              before.

    {b Rem} : Exported outside this module.
              Not yet implemented for the #-se notation indeed !             *)
(* ************************************************************************* *)
exception Module_not_specified_as_used of
  Types.fname  (** The name of the module not mentioned as "use". *)
;;



(* *********************************************************************** *)
(** {b Descr} : Exception raised when Self appears as a species identifier
              used in a [species_expr] that is a parameter of the current
	      defined species.

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
exception Self_cant_parameterize_itself of Location.t ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an expression used to represent the
              value of a "is" or "in" species parameter is not a collection
	      identifier.
	      This is detected by the fact that the [expr] found in the
              AST is not an [E_constr]. In effect, because the parser uses
              the rule [expr], collection names being capitalized, they
              must be parsed as sum types constructors with no argument.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception Is_parameter_only_coll_ident of Location.t ;;



exception Parametrized_species_wrong_arity of Location.t ;;


(* ************************************************************************* *)
(** {b Descr} : Datastructure recording various the information required
              and propagated during the scoping pass. It is much more
              convenient to group the various flags and stuff needed than
              passing them all the time as arguments of each recursive call.
              This datastructure serves especially this purpose.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
type scoping_context = {
  (** The name of the currently analysed compilation unit. *)
  current_unit : Types.fname ;
  (** The list of "use"-d modules. Not file with paths and extension : just
      module name (ex: "Basics"). *)
  used_modules : Types.fname list ;
} ;;



(* *********************************************************************** *)
(* Parsetree.ident -> Parsetree.vname                                      *)
(** {b Descr} : Extracts the [vname] from an [ident], hence providing the
              name denoted by this identifier without any
              qualification/scoping.
              For example, "bar", "foo#bar" or "foo!bar" will lead to the
              [vname] "bar".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let unqualified_vname_of_ident ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (_, vname) -> vname
;;



(* *********************************************************************** *)
(* Parsetree.constructor_ident -> Parsetree.vname                          *)
(** {b Descr} : Extracts the [vname] from a [constructor_ident], hence
              providing the name denoted by this identifier without any
              qualification/scoping.
              For example, "bar", "foo#Bar" will lead to the [vname] "Bar".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let unqualified_vname_of_constructor_ident ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.CI (_, vname) -> vname
;;



(* *********************************************************************** *)
(* Parsetree.expr_ident -> Parsetree.vname                                 *)
(** {b Descr} : Extracts the [vname] from an [expt_ident], hence providing
              the name denoted by this identifier without any
              qualification/scoping.
              For example, "bar", "foo#bar" or "foo!bar" will lead to the
              [vname] "bar".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let unqualified_vname_of_expr_ident ident =
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname
   | Parsetree.EI_global (_, vname)
   | Parsetree.EI_method (_, vname) -> vname
;;



(* ************************************************************************* *)
(* basic_vname:Parsetree.vname -> Env.ScopeInformation.value_binding_info -> *)
(*   Parsetree.expr_ident_desc                                               *)
(* {b Descr} : Build a [Parsetree.ident] from both the [vname] initialy
             contained in an [ident] and the value scoping information
             found for this [ident].
             Basically, this function dissecates the scoping information
             and differentiate the case of "method" : of Self or of
             another collection.

   {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************* *)
let scoped_expr_ident_desc_from_value_binding_info ~basic_vname = function
  | Env.ScopeInformation.SBI_file fname ->
      Parsetree.EI_global ((Some fname), basic_vname)
  | Env.ScopeInformation.SBI_method_of_self ->
      Parsetree.EI_method (None, basic_vname)
  | Env.ScopeInformation.SBI_method_of_coll (coll_module, coll_vname) ->
      (* We nown know the module of the collection *)
      (* and the collection the method belongs to. *)
      Parsetree.EI_method
	((Some ((Some coll_module), coll_vname)), basic_vname)
  | Env.ScopeInformation.SBI_local ->
      Parsetree.EI_local basic_vname
;;



(* ************************************************************* *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.type_expr -> *)
(*   Parsetree.type_expr                                         *)
(** {b Descr} : Scopes a type expression. This resolves the
              constructors, labels and type names.

    {b Rem} : Not exported outside this module.                  *)
(* ************************************************************* *)
let rec scope_type_expr ctx env ty_expr =
  let new_desc =
    (match ty_expr.Parsetree.ast_desc with
     | Parsetree.TE_ident ident ->
	 (begin
	 let basic_vname = unqualified_vname_of_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_type
	     ~loc: ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.TBI_builtin_or_var ->
		Parsetree.I_local basic_vname
	    | Env.ScopeInformation.TBI_defined_in hosting_file ->
		Parsetree.I_global ((Some hosting_file), basic_vname)) in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 Parsetree.TE_ident scoped_ident
	 end)
     | Parsetree.TE_fun (te1, te2) ->
	 let scoped_te1 = scope_type_expr ctx env te1 in
	 let scoped_te2 = scope_type_expr ctx env te2 in
	 Parsetree.TE_fun (scoped_te1, scoped_te2)
     | Parsetree.TE_app (ident, tes) ->
	 (begin
	 let basic_vname = unqualified_vname_of_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_type
	     ~loc: ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.TBI_builtin_or_var ->
		Parsetree.I_local basic_vname
	    | Env.ScopeInformation.TBI_defined_in hosting_file ->
		Parsetree.I_global ((Some hosting_file), basic_vname)) in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 let scoped_tes = List.map (scope_type_expr ctx env) tes in
	 Parsetree.TE_app (scoped_ident, scoped_tes)
	 end)
     | Parsetree.TE_prod tes ->
	 Parsetree.TE_prod (List.map (scope_type_expr ctx env) tes)
     | Parsetree.TE_self
     | Parsetree.TE_prop -> ty_expr.Parsetree.ast_desc
     | Parsetree.TE_paren te ->
	 Parsetree.TE_paren (scope_type_expr ctx env te)) in
  { ty_expr with Parsetree.ast_desc = new_desc }
;;



(* ********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.type_body             *)
(*   (Parsetree.type_body * Env.ScopingEnv.t)  *)
(** {b Descr} : Scopes a the body of a type definition by scoping its
              internal type expressions. Returns the extended environment
              with bindings for the possible sum type constructors or
              record type fields label.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let scope_type_def_body ctx env ty_def_body =
  let (scoped_desc, new_env) =
    (match ty_def_body.Parsetree.ast_desc with
     | Parsetree.TD_alias ty_expr ->
	 let descr = Parsetree.TD_alias (scope_type_expr ctx env ty_expr) in
	 (descr, env)
     | Parsetree.TD_union constructors ->
	 (begin
	 (* This will extend the scoping environment with the sum type  *)
	 (* constructors. Do not fold_left otherwise you'll reverse the *)
	 (* order of the constructors !                                 *)
	 let (env', scoped_constructors) =
	   List.fold_right
	     (fun (constr_name, args) (env_accu, cstrs_accu) ->
	       (* Scope the constructor's arguments. *)
	       let scoped_args = List.map (scope_type_expr ctx env) args in
	       let scoped_constructor = (constr_name, scoped_args) in
	       (* Extend the environment with the constructor's name. *)
	       let ext_env =
		 Env.ScopingEnv.add_constructor
		   constr_name ctx.current_unit env_accu in
	       (* And return the whole stuff... *)
	       (ext_env, (scoped_constructor :: cstrs_accu)))
	     constructors
	     (env, []) in
	 ((Parsetree.TD_union scoped_constructors), env')
	 end)
     | Parsetree.TD_record fields ->
	 (begin
	 (* This will extend the scoping environment with the record type *)
	 (* fields labels. Do not fold_left otherwise you'll reverse the  *)
	 (* order of the fields !                                         *)
	 let (env', scoped_fields) =
	   List.fold_right
	     (fun (label_name, field_tye) (env_accu, fields_accu) ->
	       let scoped_field_tye = scope_type_expr ctx env field_tye in
	       let scoped_field = (label_name, scoped_field_tye) in
	       (* Extend the environment with the field's name. *)
	       let ext_env =
		 Env.ScopingEnv.add_label
		   label_name ctx.current_unit env_accu in
	       (* And return the whole stuff... *)
	       (ext_env, (scoped_field :: fields_accu)))
	     fields
	     (env, []) in
	 ((Parsetree.TD_record scoped_fields), env')
	 end)) in
  (* Now finish to reconstruct the whole definition's body. *)
  let scoped_ty_def_body = {
    ty_def_body with Parsetree.ast_desc = scoped_desc } in
  (scoped_ty_def_body, new_env)
;;



(* ***************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.rep_type_def ->  *)
(*   Parsetree.rep_type_def                                          *)
(* {b Descr} : Scopes a [scope_rep_type_def] and returns this scoped
             [scope_rep_type_def].

   {b Rem} : Not exported outside this module.                       *)
(* ***************************************************************** *)
let rec scope_rep_type_def ctx env rep_type_def =
  let new_desc =
    (match rep_type_def.Parsetree.ast_desc with
     | Parsetree.RTE_ident ident ->
	 (begin
	 let basic_vname = unqualified_vname_of_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_type
	     ~loc: ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.TBI_builtin_or_var ->
		Parsetree.I_local basic_vname
	    | Env.ScopeInformation.TBI_defined_in hosting_file ->
		Parsetree.I_global ((Some hosting_file), basic_vname)) in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 Parsetree.RTE_ident scoped_ident
	 end)
     | Parsetree.RTE_fun (rtd1, rtd2) ->
	 let scoped_rtd1 = scope_rep_type_def ctx env rtd1 in
	 let scoped_rtd2 = scope_rep_type_def ctx env rtd2 in
	 Parsetree.RTE_fun (scoped_rtd1, scoped_rtd2)
     | Parsetree.RTE_app (ident, rtds) ->
	 (begin
	 let basic_vname = unqualified_vname_of_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_type
	     ~loc: ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.TBI_builtin_or_var ->
		Parsetree.I_local basic_vname
	    | Env.ScopeInformation.TBI_defined_in hosting_file ->
		Parsetree.I_global ((Some hosting_file), basic_vname)) in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 let scoped_rtds = List.map (scope_rep_type_def ctx env) rtds in
	 Parsetree.RTE_app (scoped_ident, scoped_rtds)
	 end)
     | Parsetree.RTE_prod rtds ->
	 let scoped_rtds = List.map (scope_rep_type_def ctx env) rtds in
	 Parsetree.RTE_prod scoped_rtds
     | Parsetree.RTE_paren rtd ->
	 Parsetree.RTE_paren (scope_rep_type_def ctx env rtd)) in
  { rep_type_def with Parsetree.ast_desc = new_desc }
;;



(* ************************************************************************* *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.type_def ->              *)
(*   (Parsetree.type_def * Env.ScopingEnv.t)                                 *)
(** {b Descr} : Scopes a type definition by scoping its internal body.
              Return the extended environment with bindings for the possible
              sum type constructors or record type fields label and a
              binding to this type name with the current compilation unit.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let scope_type_def ctx env ty_def =
  (* We must first extend the environment with the type parameters. *)
  let ty_def_descr = ty_def.Parsetree.ast_desc in
  let env_with_params =
    List.fold_left
      (fun accu_env param_vname ->
	Env.ScopingEnv.add_type
	  param_vname Env.ScopeInformation.TBI_builtin_or_var accu_env)
      env
      ty_def_descr.Parsetree.td_params in
  (* Now scope de definition's body. *)
  let (scoped_body, env_from_def) =
    scope_type_def_body ctx env_with_params ty_def_descr.Parsetree.td_body in
  (* Reconstruct the completely scoped definition. *)
  let scoped_ty_def_descr = {
    ty_def_descr with Parsetree.td_body = scoped_body } in
  let scoped_ty_def = {
    ty_def with Parsetree.ast_desc = scoped_ty_def_descr } in
  (* Extend the initial environment with a binding to *)
  (* this type name to the current compilation unit.  *)
  let final_env =
    Env.ScopingEnv.add_type
      ty_def_descr.Parsetree.td_name
      (Env.ScopeInformation.TBI_defined_in ctx.current_unit)
      env_from_def in
  (scoped_ty_def, final_env)
;;



(* *********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.expr -> Parsetree.expr *)
(* {b Descr} : Scopes an [expr] and returns this scoped [expr].

   {b Rem} : Not exported outside this module.                             *)
(* *********************************************************************** *)
let rec scope_expr ctx env expr =
  let new_desc =
    (match expr.Parsetree.ast_desc with
     | Parsetree.E_self
     | Parsetree.E_const _ ->
	 expr.Parsetree.ast_desc  (* Nothing to scope. *)
     | Parsetree.E_fun (vnames, body) ->
	 (* Add each parameter name as a local binding. *)
	 let env' =
	   List.fold_left
	     (fun accu_env vname ->
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env)
	     env
	     vnames in
	 let scoped_body = scope_expr ctx env' body in
	 Parsetree.E_fun (vnames, scoped_body)
     | Parsetree.E_var ident ->
	 (begin
	 (* Here, we will finally use our environment in order  *)
	 (* to determine the effective scope of the [ident].    *)
         let basic_vname = unqualified_vname_of_expr_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_value
	     ~loc: ident.Parsetree.ast_loc
             ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   scoped_expr_ident_desc_from_value_binding_info
             ~basic_vname hosting_info in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 Parsetree.E_var scoped_ident
	 end)
     | Parsetree.E_app (fun_expr, args_exprs) ->
	 let scoped_fun_expr = scope_expr ctx env fun_expr in
	 let scoped_args = List.map (scope_expr ctx env) args_exprs in
	 Parsetree.E_app (scoped_fun_expr, scoped_args)
     | Parsetree.E_constr (cstr_ident, args_exprs) ->
	 (begin
	 (* First, re-construct a fake [ident] to be able *)
	 (* to look-up inside the values environment.     *)
         let basic_vname =
	   (match cstr_ident.Parsetree.ast_desc with
	      Parsetree.CI (_, n) -> n) in
         let pseudo_ident =
           let Parsetree.CI (fname_opt, vname) =
	     cstr_ident.Parsetree.ast_desc in
	   { Parsetree.ast_loc = cstr_ident.Parsetree.ast_loc ;
             Parsetree.ast_desc = Parsetree.CI (fname_opt, vname) ;
	     Parsetree.ast_doc = cstr_ident.Parsetree.ast_doc ;
	     Parsetree.ast_type = None } in
         let cstr_hosting_info =
	   Env.ScopingEnv.find_constructor
	     ~loc: cstr_ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit pseudo_ident env in
         let scoped_cstr = { cstr_ident with
	   Parsetree.ast_desc =
	     Parsetree.CI ((Some cstr_hosting_info), basic_vname) } in
	 (* Now, scopes the arguments. *)
	 let scoped_args = List.map (scope_expr ctx env) args_exprs in
	 Parsetree.E_constr (scoped_cstr, scoped_args)
	 end)
     | Parsetree.E_match (e, pats_exprs) ->
	 let scoped_e = scope_expr ctx env e in
	 (* No scoping environment extention because bindings  *)
	 (* are local to each branch of the "match" construct. *)
	 let scoped_pats_exprs =
	   List.map
	     (fun (pat, expr) ->
	       let (scoped_pat, extended_env) = scope_pattern ctx env pat in
	       let scoped_expr = scope_expr ctx extended_env expr in
	       (scoped_pat, scoped_expr))
	     pats_exprs in
	 Parsetree.E_match (scoped_e, scoped_pats_exprs)
     | Parsetree.E_if (if_expr, then_expr, else_expr) ->
	 let if_expr' = scope_expr ctx env if_expr in
	 let then_expr' = scope_expr ctx env then_expr in
	 let else_expr' = scope_expr ctx env else_expr in
	 Parsetree.E_if (if_expr', then_expr', else_expr')
     | Parsetree.E_let (let_def, in_expr) ->
	 (* The list of bound names does not interest us here. *)
	 let (scoped_let_def, env', _) =
	   scope_let_definition ~toplevel_let: false ctx env let_def in
	 let scoped_in_expr = scope_expr ctx env' in_expr in
	 Parsetree.E_let (scoped_let_def, scoped_in_expr)
     | Parsetree.E_record labels_exprs ->
	 let scoped_labels_exprs =
	   List.map
	     (fun (label, expr) -> (label, (scope_expr ctx env expr)))
	     labels_exprs in
	 Parsetree.E_record scoped_labels_exprs
     | Parsetree.E_record_access (e, label) ->
	 let e' = scope_expr ctx env e in
	 Parsetree.E_record_access (e', label)
     | Parsetree.E_record_with (e, labels_exprs) ->
	 let scoped_e = scope_expr ctx env e in
	 let scoped_labels_exprs =
	   List.map
	     (fun (label, expr) -> (label, (scope_expr ctx env expr)))
	     labels_exprs in
	 Parsetree.E_record_with (scoped_e, scoped_labels_exprs)
     | Parsetree.E_tuple exprs ->
	 let exprs' = List.map (scope_expr ctx env) exprs in
	 let scoped_expr = Parsetree.E_tuple exprs' in
	 scoped_expr
     | Parsetree.E_external _ ->
	 (* Nothing to scope since it's only strings. *)
	 expr.Parsetree.ast_desc
     | Parsetree.E_paren e ->
	 let scoped_e = scope_expr ctx env e in
	 Parsetree.E_paren scoped_e) in
  { expr with Parsetree.ast_desc = new_desc }



(* ************************************************************************* *)
(* scoping_context -> Env.ScopeInformation.t -> Parsetree.pattern ->         *)
(*   (Parsetree.pattern * Env.ScopeInformation.t)                            *)
(** {b Descr} : Scopes a pattern and return the extended scoping environment
              that can be used to scope the expression part of the branch.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
and scope_pattern ctx env pattern =
  let (new_desc, new_env) =
    (match pattern.Parsetree.ast_desc with
     | Parsetree.P_const _
     | Parsetree.P_wild -> (pattern.Parsetree.ast_desc, env)
     | Parsetree.P_var vname ->
	 let env' =
	   Env.ScopingEnv.add_value vname Env.ScopeInformation.SBI_local env in
	 (pattern.Parsetree.ast_desc, env')
     | Parsetree.P_as  (p, vname) ->
	 let (scoped_p, env') = scope_pattern ctx env p in
	 let env'' =
	   Env.ScopingEnv.add_value vname Env.ScopeInformation.SBI_local env' in
	 ((Parsetree.P_as (scoped_p, vname)), env'')
     | Parsetree.P_constr  (cstr, pats) ->
         let cstr_vname = unqualified_vname_of_constructor_ident cstr in
	 let cstr_host_module =
	   Env.ScopingEnv.find_constructor
	     ~loc: cstr.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit cstr env in
	 (* Let's build tne complete extended environment by accumulating   *)
	 (* the new bindings directly inside an environment accumulator.    *)
	 (* DO NOT fold_left otherwise one gets the reverses patters list ! *)
	 let (scoped_pats, env') =
	   List.fold_right
	     (fun pat (accu_pats, accu_env) ->
	       let (scoped_pat, accu_env') = scope_pattern ctx accu_env pat in
	       ((scoped_pat :: accu_pats) ,accu_env'))
	     pats
	     ([], env) in
	 (* Reconstruct a completely scopped constructor. *)
	 let scoped_cstr =
	   { cstr with Parsetree.ast_desc =
	       Parsetree.CI ((Some cstr_host_module), cstr_vname) } in
	 ((Parsetree.P_constr (scoped_cstr, scoped_pats)), env')
     | Parsetree.P_record labs_n_pats ->
	 (* Same remark than in the case of the arguments of [P_app]. *)
	 let (scoped_labs_n_pats, env') =
	   List.fold_right
	     (fun (lab, pat) (accu_lab_pats, accu_env) ->
	       let (scoped_pat, accu_env') = scope_pattern ctx accu_env pat in
	       (((lab, scoped_pat) :: accu_lab_pats), accu_env'))
	     labs_n_pats
	     ([], env) in
	 ((Parsetree.P_record scoped_labs_n_pats), env')
     | Parsetree.P_tuple pats ->
	 (* Same remark than in the case of the arguments of [P_app]. *)
	 let (scoped_pats, env') =
	   List.fold_right
	     (fun pat (accu_pats, accu_env) ->
	       let (scoped_pat, accu_env') = scope_pattern ctx accu_env pat in
	       ((scoped_pat :: accu_pats) ,accu_env'))
	     pats
	     ([], env) in
	 ((Parsetree.P_tuple scoped_pats), env')
     | Parsetree.P_paren p ->
	 let (scoped_p, env') = scope_pattern ctx env p in
	 ((Parsetree.P_paren scoped_p), env')) in
  (* Finally, return the whole scoped pattern and the initial     *)
  (* environment extended by the bindings induced by the pattern. *)
  ({ pattern with Parsetree.ast_desc = new_desc }, new_env)



(* **************************************************************** *)
(* toplevel_let: bool -> scoping_context -> Env.ScopingEnv.t ->     *)
(*   Parsetree.let_def -> (Parsetree.let_def * Env.ScopingEnv.t *   *)
(*                         Parsetree.vname list)                    *)
(** {b Descr} : Scopes a let-definition. This returns the scoped
              bindings, the initial scoping environment extended
              with the information bound to the identifiers defined
              in the let-definition and the list of [vnames] bound
              by this let-definition. This last result is only
              interesting while scoping species fields because in
              this case the environment is not valuable, just the
              methods names are !
              This function can either operate on let-definitions
              found at toplevel (let "NOT in") and "local"
              let-definitions (let ... in ...). In the first case,
              the bool argument [toplevel_let] must be [true]. In
              the second, it must be [false].

    {b Rem} : Not exported outside this module.                     *)
(* **************************************************************** *)
and scope_let_definition ~toplevel_let ctx env let_def =
  let let_def_descr = let_def.Parsetree.ast_desc in
  (* If the let-definition is at toplevel, then we will scope the idents *)
  (* as SBI_file to represent the fact they are ... at toplevel of       *)
  (* the current compilation unit. Otherwise, the let-definition is not  *)
  (* at toplevel and hence is local.                                     *)
  let bind_locality =
    if toplevel_let then Env.ScopeInformation.SBI_file ctx.current_unit
    else Env.ScopeInformation.SBI_local in
  (* We create the extended environment with the identifiers bound by the *)
  (* definition. This environment will always be the one returned if the  *)
  (* scoping process succeeds.                                            *)
  let final_env =
    List.fold_left
      (fun accu_env let_binding ->
	Env.ScopingEnv.add_value
	  let_binding.Parsetree.ast_desc.Parsetree.b_name
	  bind_locality
	  accu_env)
      env
      let_def_descr.Parsetree.ld_bindings in
  (* However, we extend the scoping environment with the let-bound *)
  (* variables only if the definition is recursive.                *)
  let env' =
    if let_def_descr.Parsetree.ld_rec = Parsetree.RF_no_rec then env
    else final_env in
  (* Now, scope the bodies, hence the bindings... *)
  let scoped_bindings =
    List.map
      (fun let_binding ->
	let let_binding_descr = let_binding.Parsetree.ast_desc in
	(* Extend the local environment with the possible arguments *)
	(* if the bound identifier denotes a function...            *)
	let env_with_params =
	  List.fold_left
	    (fun accu_env (param_vname, _) ->
	      Env.ScopingEnv.add_value
		param_vname Env.ScopeInformation.SBI_local accu_env)
	    env'
	    let_binding_descr.Parsetree.b_params in
	(* Scope the params type exprs. *)
	let scoped_b_params =
	  List.map
	    (fun (param_vname, tye_opt) ->
	      let scoped_tye_opt =
		(match tye_opt with
		 | None -> None
		 | Some tye ->
		     Some (scope_type_expr ctx env_with_params tye)) in
	      (param_vname, scoped_tye_opt))
	    let_binding_descr.Parsetree.b_params in
	(* Now scope the body. *)
	let scoped_body =
	  scope_expr ctx env_with_params let_binding_descr.Parsetree.b_body in
	(* Now scope the optionnal body's type. *)
	let scoped_b_type =
	  (match let_binding_descr.Parsetree.b_type with
	   | None -> None
	   | Some tye -> Some (scope_type_expr ctx env_with_params tye)) in
	let new_binding_desc =
	  { let_binding_descr with
	      Parsetree.b_params = scoped_b_params ;
	      Parsetree.b_type = scoped_b_type ;
	      Parsetree.b_body = scoped_body } in
	{ let_binding with Parsetree.ast_desc = new_binding_desc })
      let_def_descr.Parsetree.ld_bindings in
  (* An finally be return the scoped let-definition *)
  (* and the extended environment.                  *)
  let scoped_let_def_desc = {
    let_def_descr with Parsetree.ld_bindings = scoped_bindings } in
  let scoped_let_def = {
    let_def with Parsetree.ast_desc = scoped_let_def_desc } in
  (* We finally get le list of names bound by this let-definition. *)
  let bound_names =
    List.map (fun b -> b.Parsetree.ast_desc.Parsetree.b_name)
      let_def_descr.Parsetree.ld_bindings in
  (scoped_let_def, final_env, bound_names)
;;



(* *********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.prop -> Parsetree.prop *)
(* {b Descr} : Scopes a [prop] and returns this scoped [prop].

   {b Rem} : Not exported outside this module.                             *)
(* *********************************************************************** *)
let rec scope_prop ctx env prop =
  let new_desc =
    (match prop.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, ty_expr, p) ->
	 (begin
	 let scoped_ty_expr = scope_type_expr ctx env ty_expr in
	 (* Entend the environment with the variables bound to local values. *)
	 let env' =
	   List.fold_left
	     (fun accu_env vname ->
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env)
	     env
	     vnames in
	 let scoped_p = scope_prop ctx env' p in
	 Parsetree.Pr_forall (vnames, scoped_ty_expr, scoped_p)
	 end)
     | Parsetree.Pr_exists (vnames, ty_expr, p) ->
	 (begin
	 let scoped_ty_expr = scope_type_expr ctx env ty_expr in
	 (* Entend the environment with the variables bound to local values. *)
	 let env' =
	   List.fold_left
	     (fun accu_env vname ->
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env)
	     env
	     vnames in
	 let scoped_p = scope_prop ctx env' p in
	 Parsetree.Pr_exists (vnames, scoped_ty_expr, scoped_p)
	 end)
     | Parsetree.Pr_imply (p1, p2) ->
	 let scoped_p1 = scope_prop ctx env p1 in
	 let scoped_p2 = scope_prop ctx env p2 in
	 Parsetree.Pr_imply (scoped_p1, scoped_p2)
     | Parsetree.Pr_or (p1, p2) ->
	 let scoped_p1 = scope_prop ctx env p1 in
	 let scoped_p2 = scope_prop ctx env p2 in
	 Parsetree.Pr_or (scoped_p1, scoped_p2)
     | Parsetree.Pr_and (p1, p2) ->
	 let scoped_p1 = scope_prop ctx env p1 in
	 let scoped_p2 = scope_prop ctx env p2 in
	 Parsetree.Pr_and (scoped_p1, scoped_p2)
     | Parsetree.Pr_equiv (p1, p2) ->
	 let scoped_p1 = scope_prop ctx env p1 in
	 let scoped_p2 = scope_prop ctx env p2 in
	 Parsetree.Pr_equiv (scoped_p1, scoped_p2)
     | Parsetree.Pr_not p -> Parsetree.Pr_not (scope_prop ctx env p)
     | Parsetree.Pr_expr expr -> Parsetree.Pr_expr (scope_expr ctx env expr)
     | Parsetree.Pr_paren p -> Parsetree.Pr_paren (scope_prop ctx env p)) in
  { prop with Parsetree.ast_desc = new_desc }
;;



(* ************************************************************************* *)
(* Parsetree.type_expr -> Env.ScopingEnv.t -> Env.ScopingEnv.t               *)
(** {b Descr} : Insert in the scoping environment the variables present in
              a type expression, considering they are implicitely
              generalized. This is used when one creates a
              type structure from an external value's type expression.
              In effect, in such a context, variables in the type are
              implicitely considered as generalized because the type
              constraint annotating the external value does not show
              explicitly "forall-bound-variables".
              Hence, in an external value definition like:
              [external value foc_error : string -> 'a = ...]
              the ['a] must be considered as generalized, then when
              typechecking this definition the context must have a variable
              mapping where ['a] is known. Using the present function, one
              can build such a mapping.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let extend_env_with_implicit_gen_vars_from_type_expr env type_expression =
  let rec rec_extend texpr accu_env =
    match texpr.Parsetree.ast_desc with
     | Parsetree.TE_ident ident ->
	 (begin
	 match ident.Parsetree.ast_desc with
	  | Parsetree.I_local ((Parsetree.Vqident _) as variable_qname) ->
	      (begin
              (* Just handle the special where the ident is a type variable. *)
	      Env.ScopingEnv.add_type
		variable_qname Env.ScopeInformation.TBI_builtin_or_var accu_env
	      end)
	  | _ -> accu_env
	 end)
     | Parsetree.TE_fun (ty_expr1, ty_expr2) ->
	 let accu_env1 = rec_extend ty_expr1 accu_env in
	 rec_extend ty_expr2 accu_env1
     | Parsetree.TE_app (_, ty_exprs)
     | Parsetree.TE_prod ty_exprs ->
	 List.fold_left
	   (fun local_accu_env ty -> rec_extend ty local_accu_env)
	   accu_env
	   ty_exprs
     | Parsetree.TE_self
     | Parsetree.TE_prop -> accu_env
     | Parsetree.TE_paren inner -> rec_extend inner accu_env in
  rec_extend env type_expression
;;



(* ************************************************************************* *)
(** {b Descr} : Insert in the scoping environment the variables present in
              a type parts of a [prop], considering they are implicitely
              generalized. This is used when one creates a type structure
              from a theorem expression.
              In effect, in such a context, variables in the type are
              implicitely considered as generalized because the type
              constraint annotating the theorem does not show explicitly
              "forall-bound-variables".
              Hence, in an theorem definition like:
              [theorem beq_refl : all x in 'a, ...]
              the ['a] must be considered as generalized, then when
              typechecking this definitionn the context must have a variable
              mapping where ['a] is known. Using the present function, one
              can build such a mapping.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let extend_env_with_implicit_gen_vars_from_prop env prop_expression =
  let rec rec_extend pexpr accu_env =
    match pexpr.Parsetree.ast_desc with
     | Parsetree.Pr_forall (_, ty, prop)
     | Parsetree.Pr_exists (_, ty, prop) ->
	 (* First recover the mapping induced byt the type expression. *)
	 let env_from_ty =
	   extend_env_with_implicit_gen_vars_from_type_expr ty accu_env in
	 rec_extend prop env_from_ty
     | Parsetree.Pr_imply (prop1, prop2)
     | Parsetree.Pr_or (prop1, prop2)
     | Parsetree.Pr_and (prop1, prop2)
     | Parsetree.Pr_equiv (prop1, prop2) ->
	 let env_from_prop1 = rec_extend prop1 accu_env in
	 rec_extend prop2 env_from_prop1
     | Parsetree.Pr_not prop
     | Parsetree.Pr_paren prop -> rec_extend prop accu_env
     | Parsetree.Pr_expr _ ->
	 (* Inside expressions type variable must be bound by the previous *)
         (* parts of the prop ! Hence, do not continue searching inside.   *)
	 accu_env in
  rec_extend prop_expression env
;;



let scope_sig_def ctx env sig_def =
  let sig_def_descr = sig_def.Parsetree.ast_desc in
  let scoped_type = scope_type_expr ctx env sig_def_descr.Parsetree.sig_type in
  let scoped_sig_def = {
    sig_def with
      Parsetree.ast_desc = {
        Parsetree.sig_name = sig_def_descr.Parsetree.sig_name ;
	Parsetree.sig_type = scoped_type
      } } in
  (scoped_sig_def, sig_def_descr.Parsetree.sig_name)
;;



(* *********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.fact -> Parsetree.fact *)
(* {b Descr} : Scopes a [fact] and return this scoped fact].

   {b Rem} : Not exported outside this module.                             *)
(* *********************************************************************** *)
let scope_fact ctx env fact =
  let new_desc =
    (match fact.Parsetree.ast_desc with
     | Parsetree.F_def idents ->
	 (begin
	 let scoped_idents =
	   List.map
	     (fun ident ->
	       let basic_vname = unqualified_vname_of_expr_ident ident in
	       let scope_info =
		 Env.ScopingEnv.find_value
		   ~loc: ident.Parsetree.ast_loc
		   ~current_unit: ctx.current_unit ident env in
	       let tmp =
		 scoped_expr_ident_desc_from_value_binding_info
		   ~basic_vname scope_info in
	       { ident with Parsetree.ast_desc = tmp })
	     idents in
	 Parsetree.F_def scoped_idents
	 end)
     | Parsetree.F_property idents ->
	 (begin
	 let scoped_idents =
	   List.map
	     (fun ident ->
	       let basic_vname = unqualified_vname_of_expr_ident ident in
	       let scope_info =
		 Env.ScopingEnv.find_value
		   ~loc: ident.Parsetree.ast_loc
		   ~current_unit: ctx.current_unit ident env in
	       let tmp =
		 scoped_expr_ident_desc_from_value_binding_info
		   ~basic_vname scope_info in
	     { ident with Parsetree.ast_desc = tmp })
	     idents in
	 Parsetree.F_property scoped_idents
	 end)
     | Parsetree.F_hypothesis _  (* Only vnames, so nothing to scope... *)
     | Parsetree.F_node _ -> fact.Parsetree.ast_desc) in
  { fact with Parsetree.ast_desc = new_desc }
;;



(* ********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.hyp ->                *)
(*   (Env.ScopingEnv.t * (Parsetree.hyp list))                            *)
(** {b Descr} : Scopes a list of [hyp]s and insert them in the current
              environment. Returns the list of scoped [hyp]s and the
              initial environment extended with all these hypotheses.
              This environment will then be suitable to be used by
              [scope_statement] in order to scope the body of a statement
              under these hypotheses.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let scope_hyps ctx env hyps =
  List.fold_left
    (fun (accu_env, accu_scoped_hyps) hyp ->
      let hyp_desc = hyp.Parsetree.ast_desc in
      let (new_desc, env') =
	(begin
	match hyp_desc with
	 | Parsetree.H_var (vname, type_expr) ->
	     let scoped_type_expr = scope_type_expr ctx env type_expr in
	     let env' =
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env in
	     ((Parsetree.H_var (vname, scoped_type_expr)), env')
	 | Parsetree.H_hyp (vname, prop) ->
	     let scoped_prop = scope_prop ctx env prop in
	     let env' =
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env in
	     ((Parsetree.H_hyp (vname, scoped_prop)), env')
	 | Parsetree.H_not (vname, expr) ->
	     let scoped_expr = scope_expr ctx env expr in
	     let env' =
	       Env.ScopingEnv.add_value
		 vname Env.ScopeInformation.SBI_local accu_env in
	     ((Parsetree.H_not (vname, scoped_expr)), env')
	end) in
      (env', { hyp with Parsetree.ast_desc = new_desc } :: accu_scoped_hyps))
    (env, [])
    hyps
;;



let scope_statement ctx env stmt =
  let stmt_desc = stmt.Parsetree.ast_desc in
  let (env', scoped_hyps) = scope_hyps ctx env stmt_desc.Parsetree.s_hyps in
  let scoped_concl =
    (match stmt_desc.Parsetree.s_concl with
     | None -> None
     | Some prop -> Some (scope_prop ctx env' prop)) in
  let scoped_stmt_desc = {
    Parsetree.s_hyps = scoped_hyps ; Parsetree.s_concl = scoped_concl } in
  { stmt with Parsetree.ast_desc = scoped_stmt_desc }
;;



(* ********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.proof_node ->         *)
(*   Parsetree.proof_node                                                 *)
(** {b Descr} : Scopes a [proof_node] and returns a scoped version of it.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let rec scope_proof_node ctx env node =
  let new_desc =
    (match node.Parsetree.ast_desc with
     | Parsetree.PN_sub (node_label, stmt, proof) ->
	 let scoped_stmt = scope_statement ctx env stmt in
	 let scoped_proof = scope_proof ctx env proof in
	 Parsetree.PN_sub (node_label, scoped_stmt, scoped_proof)
     | Parsetree.PN_qed (node_label, proof) ->
	 let scoped_proof = scope_proof ctx env proof in
	 Parsetree.PN_qed (node_label, scoped_proof)) in
  { node with Parsetree.ast_desc = new_desc }



and scope_proof ctx env proof =
  let new_desc =
    (match proof.Parsetree.ast_desc with
     | Parsetree.Pf_assumed
     | Parsetree.Pf_coq _ -> proof.Parsetree.ast_desc  (* Nothing to scope. *)
     | Parsetree.Pf_auto facts ->
	 Parsetree.Pf_auto (List.map (scope_fact ctx env) facts)
     | Parsetree.Pf_node proof_nodes ->
	 Parsetree.Pf_node (List.map (scope_proof_node ctx env) proof_nodes)) in
  { proof with Parsetree.ast_desc = new_desc }
;;



let scope_theorem_def ctx env theo_def =
  let theo_def_desc = theo_def.Parsetree.ast_desc in
  let env' =
    extend_env_with_implicit_gen_vars_from_prop
      env theo_def_desc.Parsetree.th_stmt in
  let scoped_stmt = scope_prop ctx env' theo_def_desc.Parsetree.th_stmt in
  let scoped_proof = scope_proof ctx env' theo_def_desc.Parsetree.th_proof in
  let scoped_theo_def_desc = {
    Parsetree.th_name = theo_def_desc.Parsetree.th_name ;
    Parsetree.th_local = theo_def_desc.Parsetree.th_local ;
    Parsetree.th_stmt = scoped_stmt ;
    Parsetree.th_proof = scoped_proof } in
  ({ theo_def with Parsetree.ast_desc = scoped_theo_def_desc },
   theo_def_desc.Parsetree.th_name)
;;



let scope_proof_def ctx env proof_def =
  let proof_def_desc = proof_def.Parsetree.ast_desc in
  let scoped_proof = scope_proof ctx env proof_def_desc.Parsetree.pd_proof in
  let scoped_proof_def_desc = {
    Parsetree.pd_name = proof_def_desc.Parsetree.pd_name ;
    Parsetree.pd_proof = scoped_proof } in
  ({ proof_def with Parsetree.ast_desc = scoped_proof_def_desc },
   proof_def_desc.Parsetree.pd_name)
;;



(* ******************************************************************* *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.property_def ->    *)
(*   (Parsetree.property_def * Parsetree.vname)                        *)
(* {b Descr} : Scopes a property definition and return both the scoped
             roperty definition and its name as a [vname].

   {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************* *)
let scope_property_def ctx env property_def =
  let property_def_desc = property_def.Parsetree.ast_desc in
  let scoped_prop = scope_prop ctx env property_def_desc.Parsetree.prd_prop in
  let scoped_property_def = {
    property_def with
      Parsetree.ast_desc = {
        Parsetree.prd_name = property_def_desc.Parsetree.prd_name ;
	Parsetree.prd_prop = scoped_prop
      } } in
  (scoped_property_def, property_def_desc.Parsetree.prd_name)
;;



(* ***************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.species_field -> *)
(*   (Parsetree.species_field * (Parsetree.vname list))              *)
(* {b Descr} : Scopes a species field and return both the scoped
             field and its names as a [vname list] they must be
             inserted later in the scoping environment as a values.
             Especially, [rep] and [proof] are not methods hence are
             not values to bind in the environment.

   {b Rem} : Not exported outside this module.                       *)
(* ***************************************************************** *)
let scope_species_field ctx env field =
  let (new_desc, method_name_opt) =
    (match field.Parsetree.ast_desc with
     | Parsetree.SF_rep rep_type_def ->
         let scoped_rep_type_def = scope_rep_type_def ctx env rep_type_def in
         ((Parsetree.SF_rep scoped_rep_type_def), [])
     | Parsetree.SF_sig sig_def ->
	 let (scoped_sig, name) = scope_sig_def ctx env sig_def in
	 ((Parsetree.SF_sig scoped_sig), [name])
     | Parsetree.SF_let let_def ->
	let (scoped_let_def, _, names) =
	  scope_let_definition ~toplevel_let: false ctx env let_def in
	((Parsetree.SF_let scoped_let_def), names)
     | Parsetree.SF_property prop_def ->
	 let (scoped_prop, name) = scope_property_def ctx env prop_def in
	 ((Parsetree.SF_property scoped_prop), [name])
     | Parsetree.SF_theorem theo_def ->
	 let (scoped_theo, name) = scope_theorem_def ctx env theo_def in
	 ((Parsetree.SF_theorem scoped_theo), [name])
     | Parsetree.SF_proof proof_def ->
	 let (scoped_proof_def, name) = scope_proof_def ctx env proof_def in
	 ((Parsetree.SF_proof scoped_proof_def), [name])) in
  ({ field with Parsetree.ast_desc = new_desc }, method_name_opt)
;;



let rec scope_species_fields ctx env = function
  | [] -> ([], [])
  | field :: rem ->
      let (scoped_field, names) = scope_species_field ctx env field in
      (* All the methods a always inserted as [SBI_method_of_self], the *)
      (* [find_value] taking care of changing to [SBI_method_of_coll]   *)
      (* when required.                                                 *)
      let env' =
	List.fold_left
	  (fun accu_env method_name ->
	    Env.ScopingEnv.add_value
	      method_name (Env.ScopeInformation.SBI_method_of_self)
	      accu_env)
	  env names in
      let (rem_scoped_fields, rem_method_names) =
	scope_species_fields ctx env' rem in
      ((scoped_field :: rem_scoped_fields), (names @ rem_method_names))
;;


let rec scope_expr_collection_cstr_for_is_param ctx env initial_expr =
  match initial_expr.Parsetree.ast_desc with
   | Parsetree.E_self ->
       raise (Self_cant_parameterize_itself initial_expr.Parsetree.ast_loc)
   | Parsetree.E_constr (cstr_expr, []) ->
       (* We re-construct a fake ident from the constructor expression *)
       (* just to be able to lookup inside the environment.            *)
       let Parsetree.CI (id_opt_fname, id_vname) =
	 cstr_expr.Parsetree.ast_desc in
       let pseudo_ident = { cstr_expr with
         Parsetree.ast_desc = Parsetree.I_global (id_opt_fname, id_vname) } in
       let species_info =
	 Env.ScopingEnv.find_species
	   ~loc: pseudo_ident.Parsetree.ast_loc
	   ~current_unit: ctx.current_unit pseudo_ident env in
       (* Recover the hosting file of the species. If the species is     *)
       (* local, then the hosting ffile is the current compilation unit. *)
       let hosting_file =
	 (match species_info.Env.ScopeInformation.spbi_scope with
	  | Env.ScopeInformation.SPBI_file n -> n
	  | Env.ScopeInformation.SPBI_local -> ctx.current_unit) in
       let scoped_cstr_expr = {
	 cstr_expr with
           Parsetree.ast_desc = Parsetree.CI (Some hosting_file, id_vname) } in
       { initial_expr with
           Parsetree.ast_desc = Parsetree.E_constr (scoped_cstr_expr, []) }
   | Parsetree.E_paren expr ->
       scope_expr_collection_cstr_for_is_param ctx env expr
   | _ -> raise (Is_parameter_only_coll_ident initial_expr.Parsetree.ast_loc)
;;



let scope_species_param ctx env param param_kind =
  let new_desc =
    (match param.Parsetree.ast_desc with
     | Parsetree.SP expr ->
	 (begin
	 match param_kind with
	  | Env.ScopeInformation.SPK_is ->
	      (* Note that to be well-typed this expression must ONLY be an *)
	      (* [E_constr] (because species names are capitalized, parsed  *)
              (* as sum type constructors) that should be considered as a   *)
              (* species name.                                              *)
	      Parsetree.SP
		(scope_expr_collection_cstr_for_is_param ctx env expr)
	  | Env.ScopeInformation.SPK_in ->
	      Parsetree.SP (scope_expr ctx env expr)
	 end)) in
  { param with Parsetree.ast_desc = new_desc }
;;



(* ******************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.species_expr ->     *)
(*   (Parsetree.species_expr * Parsetree.vname list)                    *)
(** {b Descr} : Scopes a species expression. Returns the scoped
              expression, the list of methods names this expression
              has. This may be used when binding a new collection name
              (a parameter indeed) to a species expression, in order
              to "transfer" the methods of the expression to the bound
              identifier.
              Hence, in [species ... (C is Spec) = ... C#m], one will
              be able to check that C really has a [m] method providing
              that [Spec] has also it.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let rec scope_species_expr ctx env species_expr =
  let species_expr_descr = species_expr.Parsetree.ast_desc in
  (* We first scope the "applied" ident. *)
  let se_name_ident = species_expr_descr.Parsetree.se_name in
  let ident_scope_info =
    Env.ScopingEnv.find_species
      ~loc: se_name_ident.Parsetree.ast_loc
      ~current_unit: ctx.current_unit se_name_ident env in
  let basic_vname = unqualified_vname_of_ident se_name_ident in
  let scoped_ident_descr =
    (match ident_scope_info.Env.ScopeInformation.spbi_scope with
     | Env.ScopeInformation.SPBI_local -> Parsetree.I_local basic_vname
     | Env.ScopeInformation.SPBI_file hosting_file ->
	 Parsetree.I_global ((Some hosting_file), basic_vname)) in
  let scoped_ident = {
    se_name_ident with Parsetree.ast_desc = scoped_ident_descr } in
  (* Scopes the effective parameters. *)
  let scoped_params =
    (try
      List.map2
	(scope_species_param ctx env)
	species_expr_descr.Parsetree.se_params
	ident_scope_info.Env.ScopeInformation.spbi_params_kind
    with Failure _ ->
      (* If List.map2 fails, that's because the 2 lists are of different     *)
      (* length, then the species is applied to a wrong number of arguments. *)
      raise
	(Parametrized_species_wrong_arity species_expr.Parsetree.ast_loc)) in
  let scoped_species_expr = {
    species_expr with
      Parsetree.ast_desc = {
	Parsetree.se_name = scoped_ident ;
	Parsetree.se_params = scoped_params } } in
  (scoped_species_expr, ident_scope_info.Env.ScopeInformation.spbi_methods)
;;



(* *********************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t ->                                  *)
(*   (Parsetree.species_expr list) Parsetree.ast_doc ->                    *)
(*     ((Parsetree.species_expr list) ast_doc * Parsetree.vname list)      *)
(** {b Descr} : Scopes the inheritance specification of a species. This
              specification  is a list of species expressions. By the way,
              returns the list of methods inherited from this tree.
              This list of methods contains those of the first inherited
              species in head, then the laters...

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let scope_inheritance ctx env spec_exprs =
  let spec_exprs_descr = spec_exprs.Parsetree.ast_desc in
  let (scoped_exprs_descs, methods) =
    List.fold_right
      (fun spec_expr (accu_exprs, accu_methods) ->
	let (scoped_expr, meths) = scope_species_expr ctx env spec_expr in
	((scoped_expr :: accu_exprs), (meths @ accu_methods)))
      spec_exprs_descr
      ([], []) in
  let scoped_spec_exprs = {
    spec_exprs with Parsetree.ast_desc = scoped_exprs_descs } in
  (scoped_spec_exprs, methods)
 ;;



(* ******************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t ->                               *)
(*   (Parsetree.vname * Parsetree.species_param_type) list ->           *)
(*     ((Parsetree.vname * Parsetree.species_param_type) list           *)
(*      *                                                               *)
(*      Env.ScopingEnv.t)                                               *)
(** {b Descr} : Scopes the species parameters of a specied definition
              and return the scoping environment extended with bindings
              for theses parameters.
              This extended environment will be later used to scope
              the body of the species definition.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let scope_species_params_types ctx env params =
  (* Do not fold_right otherwise params will be inserted in reverse order ! *)
  (* in the environment. Do not fold_left otherwise le list of scoped       *)
  (* parameters will be reversed ! So what to do ????                       *)
  (* Eh guy, just fold_left to get the correct environment, and then we     *)
  (* just reverse the list of the scoped parameters, that's it !            *)
  let (scoped_params_revd, env') =
    List.fold_left
      (fun (accu_params_revd, accu_env) (param_name, param_kind) ->
	(* We scope parameters in the environment incrementally extended. *)
	(* This means that following parameters know previous ones.       *)
	match param_kind.Parsetree.ast_desc with
	 | Parsetree.SPT_in ident ->
	     (begin
	     (* Let's first scope the ident representing a collection name. *)
	     let ident_scope_info =
	       Env.ScopingEnv.find_species
		 ~loc: ident.Parsetree.ast_loc
		 ~current_unit: ctx.current_unit ident accu_env in
	     let basic_vname = unqualified_vname_of_ident ident in
	     let scoped_ident_descr =
	       (match ident_scope_info.Env.ScopeInformation.spbi_scope with
		| Env.ScopeInformation.SPBI_local ->
		    Parsetree.I_local basic_vname
		| Env.ScopeInformation.SPBI_file hosting_file ->
		    Parsetree.I_global ((Some hosting_file), basic_vname)) in
	     let scoped_ident = {
	       ident with Parsetree.ast_desc = scoped_ident_descr } in
	     (* The parameter is a value belonging to a collection. It's *)
	     (* type is the "repr" of the collection. Then this leads to *)
	     (* a simple value that will get scoped as local.            *)
	     let accu_env' =
	       Env.ScopingEnv.add_value
		 param_name Env.ScopeInformation.SBI_local accu_env in
	     let scoped_param_kind = {
	       param_kind with
	         Parsetree.ast_desc = Parsetree.SPT_in scoped_ident } in
	     let scoped_param = (param_name, scoped_param_kind) in
	     ((scoped_param :: accu_params_revd), accu_env')
	     end)
	 | Parsetree.SPT_is spec_expr ->
	     (begin
	     (* The parameter is a collection (indeed, that's its *)
	     (* type). Because collections and species are not    *)
	     (* first-class-value, the environment extention will *)
	     (* Not be done at the "values" level.                *)
	     let (scoped_species_expr, species_methods) =
	       scope_species_expr ctx env spec_expr in
	     (* Extend the environment with a "locally defined" collection *)
	     (* having the same methods than those coming from the         *)
	     (* expression.                                                *)
	     let accu_env' =
	       Env.ScopingEnv.add_species
                 param_name
		 { Env.ScopeInformation.spbi_scope =
		     Env.ScopeInformation.SPBI_local ;
		   (* Because parameters are indeed COLLECTION parameters *)
		   (* (i.e. ar eintended to be finally instanciated to a  *)
		   (* collection), they have no ... parameters, them.     *)
		   Env.ScopeInformation.spbi_params_kind = [] ;
		   Env.ScopeInformation.spbi_methods = species_methods }
		 accu_env in
	     (* Now, extend the environment with the name *)
	     (* of the carrier type for this species.     *)
	     let accu_env'' =
	       Env.ScopingEnv.add_type
		 param_name
		 (Env.ScopeInformation.TBI_defined_in ctx.current_unit)
		 accu_env' in
	     (* Now, scope the kind of the parameter. *)
	     let scoped_param_kind = {
	       param_kind with
	         Parsetree.ast_desc = Parsetree.SPT_is scoped_species_expr } in
	     let scoped_param = (param_name, scoped_param_kind) in
	     ((scoped_param :: accu_params_revd), accu_env'')
	     end))
      ([], env)
      params in
  ((List.rev scoped_params_revd), env')
;;



(* *********************************************************************** *)
(** {b Descr} : Scopes a species and returns the environment extended with
              the bindings induced by the species.

    {b Rem} : Not exported outside this module.
            Environment search must proceed in the following order:
	     1) try to find the identifier in local environment
	     2) check if it's a parameter of entity ("in") or
                collection ("is")
	     4) try to find the ident throughout the hierarchy
	     5) try to find the ident is a global identifier
	     else) not found
            So the order in which the idents are inserted into the
            environment used to scope the species must respect extentions
            in the reverse order in order to find the most recently added
            bindings first !                                               *)
(* *********************************************************************** *)
let scope_species_def ctx env species_def =
  let species_def_descr = species_def.Parsetree.ast_desc in
  (* A species is not recursive, so no need to     *)
  (* insert it inside the environment to scope it. *)
  (* According to the search order, we must first add the parameters    *)
  (* of collection and entity in their order of apparition then import  *)
  (* the bindings from the inheritance tree, and finally local bindings *)
  (* will be automagically be added while scoping the species' body     *)
  (* (fields).                                                          *)
  (* So ... the parameters... *)
  let (scoped_params, env_with_params) =
    scope_species_params_types ctx env species_def_descr.Parsetree.sd_params in
  (* Next ... the inheritance... By the way we get the methods coming *)
  (* from our ancestors. *)
  let (scoped_inherits, inherited_methods) =
    scope_inheritance
      ctx env_with_params species_def_descr.Parsetree.sd_inherits in
  (* We now must extend the environment with the inherited methods. Because *)
  (* the list of methods contains the olders in head and the youngers in    *)
  (* tail, we need to [fold_left] Env.ScopingEnv.add_value on it.           *)
  let full_env =
    List.fold_left
      (fun accu_env meth_vname ->
	(* Because these methods are inherited, they now are methods of   *)
        (* ourselves, hence methodes of Self. Anyway, as previously told, *)
        (* all the methods a always inserted as [SBI_method_of_self], the *)
        (* [find_value] taking care of changing to [SBI_method_of_coll]   *)
        (* when required.                                                 *)
	Env.ScopingEnv.add_value
	  meth_vname Env.ScopeInformation.SBI_method_of_self accu_env)
      env_with_params
      inherited_methods in
  (* And now the fields in the environment we just built. *)
  let (scoped_fields, method_names) =
    scope_species_fields ctx full_env species_def_descr.Parsetree.sd_fields in
  (* We must extend the initial environment with our name *)
  (* bound to a species with the got methods names.       *)
  let our_info = {
    Env.ScopeInformation.spbi_methods = inherited_methods @ method_names ;
    Env.ScopeInformation.spbi_params_kind =
      List.map
        (fun (_, pkind) ->
	  match pkind.Parsetree.ast_desc with
	   | Parsetree.SPT_in _ -> Env.ScopeInformation.SPK_in
	   | Parsetree.SPT_is _ -> Env.ScopeInformation.SPK_is)
        species_def_descr.Parsetree.sd_params ;
    Env.ScopeInformation.spbi_scope =
      Env.ScopeInformation.SPBI_file ctx.current_unit } in
  let final_env =
    Env.ScopingEnv.add_species
      species_def_descr.Parsetree.sd_name our_info env in
  let scoped_def_descr = {
    Parsetree.sd_name = species_def_descr.Parsetree.sd_name ;
    Parsetree.sd_params = scoped_params ;
    Parsetree.sd_inherits = scoped_inherits ;
    Parsetree.sd_fields = scoped_fields } in
  let scoped_def = { species_def with Parsetree.ast_desc = scoped_def_descr } in
  (scoped_def, final_env)
;;



(* *************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.coll_def ->    *)
(*   (Parsetree.coll_def_desc * Env.ScopingEnv.t)                  *)
(** {b Descr} : Scopes a collection definition. Returns the scoped
              definition and the initial environment extended with
              a binding for this collection inside.

    {b Rem} : Not exported outside this module.                    *)
(* *************************************************************** *)
let scope_collection_def ctx env coll_def =
  let coll_def_desc = coll_def.Parsetree.ast_desc in
  let (scoped_body, methods_names) =
    scope_species_expr ctx env coll_def_desc.Parsetree.cd_body in
  let scoped_desc = {
    Parsetree.cd_name = coll_def_desc.Parsetree.cd_name ;
    Parsetree.cd_body = scoped_body } in
  let scoped_coll_def = { coll_def with Parsetree.ast_desc = scoped_desc } in
  (* Now add ourselves as a collection in the environment. *)
  let our_info = {
    Env.ScopeInformation.spbi_methods = methods_names ;
    (* A collection never have remaining parameters ! *)
    Env.ScopeInformation.spbi_params_kind = [] ;
    Env.ScopeInformation.spbi_scope =
      Env.ScopeInformation.SPBI_file ctx.current_unit } in
  let final_env =
    Env.ScopingEnv.add_species
      coll_def_desc.Parsetree.cd_name our_info env in
  (scoped_coll_def, final_env)
;;



(* **************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.external_def -> *)
(*   (Parsetree.external_def * Env.ScopingEnv.t)                    *)
(* {Descr} : Returns the environment extended with the name of the
           external definition. This name is bound into the "types"
           bucket or into the "values" bucket according to the kind
           of external definition.
           Also return a "scoped" version of the definition.

   {b Rem} : Not exported outside this module.                      *)
(* **************************************************************** *)
let scope_external_def ctx env external_def =
  match external_def.Parsetree.ast_desc with
   | Parsetree.ED_type e_def_body ->
       (begin
       let env' =
	 Env.ScopingEnv.add_type
	   e_def_body.Parsetree.ast_desc.Parsetree.etd_name
	   (Env.ScopeInformation.TBI_defined_in ctx.current_unit) env in
       (* Because external type definition do not structurally contain *)
       (* elements that can be scoped, we directly return the initial  *)
       (* expression.                                                  *)
       (external_def, env')
       end)
   | Parsetree.ED_value e_def_body ->
       (begin
       (* First, scope the type expression annotating the external value. *)
       let env' =
	 extend_env_with_implicit_gen_vars_from_type_expr
	   e_def_body.Parsetree.ast_desc.Parsetree.evd_type env in
       let scoped_ty =
	 scope_type_expr
	   ctx env' e_def_body.Parsetree.ast_desc.Parsetree.evd_type in
       let bound_name = e_def_body.Parsetree.ast_desc.Parsetree.evd_name in
       (* Build the extended scoping environment. *)
       let env'' =
	 Env.ScopingEnv.add_value
	   bound_name (Env.ScopeInformation.SBI_file ctx.current_unit) env in
       (* Now, reconstruct a scoped external definition. *)
       let scoped_e_def_body_desc = { e_def_body.Parsetree.ast_desc with
         Parsetree.evd_type = scoped_ty } in
       let scoped_e_def_body = { e_def_body with
         Parsetree.ast_desc = scoped_e_def_body_desc } in
       let scoped_external_def = { external_def with
         Parsetree.ast_desc = Parsetree.ED_value scoped_e_def_body } in
       (* And finally, return both the extended environment ans scoped def. *)
       (scoped_external_def, env'')
       end)
;;



let scope_phrase ctx env phrase =
  let (new_desc, new_env, new_ctx) =
    (match phrase.Parsetree.ast_desc with
     | Parsetree.Ph_external external_def ->
	 (* Nothing to scope here. External definition structurally do not *)
	 (* contain any  [ident]. However, the scoping environment must be *)
	 (* extended.                                                      *)
	 let (scoped_external_def, env') =
	   scope_external_def ctx env external_def in
	 ((Parsetree.Ph_external scoped_external_def), env', ctx)
     | Parsetree.Ph_use fname ->
	 (* Check if the "module" was not already "use"-d. *)
	 if List.mem fname ctx.used_modules then
	   raise (Multiply_used_module fname) ;
	 (* Allow this module to be used. *)
	 let ctx' = { ctx with used_modules = fname :: ctx.used_modules } in
	 (phrase.Parsetree.ast_desc, env, ctx')
     | Parsetree.Ph_open fname ->
	 (* Load this module interface to extend the current environment *)
	 (* only if this "module" was previously "use"-d.                *)
	 if not (List.mem fname ctx.used_modules) then
	   raise (Module_not_specified_as_used fname) ;
	 let env' =
	   Env.scope_open_module ~loc: phrase.Parsetree.ast_loc fname env in
	 (phrase.Parsetree.ast_desc, env', ctx)
     | Parsetree.Ph_species species_def ->
	 let (scoped_species_def, env') =
	   scope_species_def ctx env species_def in
	 ((Parsetree.Ph_species scoped_species_def), env', ctx)
     | Parsetree.Ph_coll coll_def ->
	 let (scoped_coll_def, env') =
	   scope_collection_def ctx env coll_def in
	 ((Parsetree.Ph_coll scoped_coll_def), env', ctx)
     | Parsetree.Ph_type type_def ->
         let (scoped_ty_def, env') = scope_type_def ctx env type_def in
	 ((Parsetree.Ph_type scoped_ty_def), env', ctx)
     | Parsetree.Ph_let let_def ->
	 (* This one can extend the global scoping environment. *)
	 (* The list of bound names does not interest us here.  *)
	 let (scoped_let_def, env', _) =
	   scope_let_definition ~toplevel_let: true ctx env let_def in
	 ((Parsetree.Ph_let scoped_let_def), env', ctx)
     | Parsetree.Ph_theorem theo_def ->
	 let (scoped_theo, name) = scope_theorem_def ctx env theo_def in
	 let env' =
	   Env.ScopingEnv.add_value
	     name Env.ScopeInformation.SBI_method_of_self env in
	 ((Parsetree.Ph_theorem scoped_theo), env', ctx)
     | Parsetree.Ph_expr expr_def ->
	 (* No scoping environment extention here. *)
	 let scoped_expr = Parsetree.Ph_expr (scope_expr ctx env expr_def) in
	 (scoped_expr, env, ctx)) in
  ({ phrase with Parsetree.ast_desc = new_desc }, new_env, new_ctx)
;;



(* ******************************************************************* *)
(* scope_file : Types.fname -> Parsetree.file ->                       *)
(*   (Parsetree.file * Env.ScopingEnv.t)                               *)
(** {b Descr} : Starts the scoping phase on a whole file. The initial
              scoping environment is totally empty. Returns the scoped
              AST structure and the complete toplevel scoping
              environment obtained after the scoping is finished.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
let scope_file current_unit file =
  match file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
       (* Initial context with no "use"-d modules. *)
       let global_ctx =
	 ref { current_unit = current_unit ; used_modules = [] } in
       (* Scoping of a file starts with the empty scoping environment. *)
       let global_env = ref (Env.ScopingEnv.pervasives ()) in
       let scoped_phrases =
	 List.map
	   (fun phrase ->
	     let (phrase', env', ctx') =
	       scope_phrase !global_ctx !global_env phrase in
	     global_env := env' ;
	     global_ctx := ctx' ;
	     (* Return the scoped phrase. *)
	     phrase')
	   phrases in
       ({ file with Parsetree.ast_desc = Parsetree.File scoped_phrases },
	!global_env)
;;
