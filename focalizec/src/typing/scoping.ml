(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: scoping.ml,v 1.5 2007-08-09 12:21:11 pessaux Exp $ *)

(** {b Desc} : Scoping phase is intended to disambiguate
		- variables identifiers
		- types identifiers, ??????????????
		- constructors (of sum types) identifiers. ??????????????
             Hence, this means that only [I_local] [ident]s will be
             affected by the scoping transformation.
             Local [ident]s will be looked-up to determine whether they
             are really local or are method names or toplevel (of a file)
             names.
             If no more precise binding than I_local is found, then NO
             error is raised, leaving the original I_local. Errors will
             then be reported during the tyechecking phase.
             The transformation is not performed in place. Instead, we
             return a fresh AST (still possibly having sharing with the
             original one) that will be suitable for the typechecking
	     phase.                                                       *)


type scoping_context = {
  (** The name of the currently analysed compilation unit. *)
  current_unit : Parsetree.fname
} ;;



(* *********************************************************************** *)
(* Parsetree.ident -> Parsetree.vname *)
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
   | Parsetree.I_global (_, vname)
   | Parsetree.I_method (_, vname) -> vname
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
	   Env.ScopingEnv.find_type ~current_unit: ctx.current_unit ident env in
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
	   Env.ScopingEnv.find_type ~current_unit: ctx.current_unit ident env in
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
      (fun accu_env param_name ->
	Env.ScopingEnv.add_type
	  param_name Env.ScopeInformation.TBI_builtin_or_var accu_env)
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
         let basic_vname = unqualified_vname_of_ident ident in
	 let hosting_info =
	   Env.ScopingEnv.find_value
             ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.SBI_file fname ->
		Parsetree.I_global ((Some fname), basic_vname)
	    | Env.ScopeInformation.SBI_method_of_self ->
		Parsetree.I_method (None, basic_vname)
	    | Env.ScopeInformation.SBI_method_of_coll coll_name ->
		Parsetree.I_method ((Some coll_name), basic_vname)
	    | Env.ScopeInformation.SBI_local ->
		Parsetree.I_local basic_vname) in
	 let scoped_ident =
	   { ident with Parsetree.ast_desc = scoped_ident_descr } in
	 Parsetree.E_var scoped_ident
	 end)
     | Parsetree.E_app (fun_expr, args_exprs) ->
	 let scoped_fun_expr = scope_expr ctx env fun_expr in
	 let scoped_args = List.map (scope_expr ctx env) args_exprs in
	 Parsetree.E_app (scoped_fun_expr, scoped_args)
     | Parsetree.E_constr (cstr_expr, args_exprs) ->
	 let scoped_args = List.map (scope_expr ctx env) args_exprs in
	 Parsetree.E_constr (cstr_expr, scoped_args)
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
	 let (scoped_let_def, env') =
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
	 expr.Parsetree.ast_desc  (* Nothing to scope. *)
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
     | Parsetree.P_wild
     | Parsetree.P_var _ -> (pattern.Parsetree.ast_desc, env)
     | Parsetree.P_as  (p, vname) ->
	 let (scoped_p, env') = scope_pattern ctx env p in
	 let env'' =
	   Env.ScopingEnv.add_value vname Env.ScopeInformation.SBI_local env' in
	 ((Parsetree.P_as (scoped_p, vname)), env'')
     | Parsetree.P_app  (cstr, pats) ->
         let cstr_vname = unqualified_vname_of_ident cstr in
	 let cstr_host_module =
	   Env.ScopingEnv.find_constructor
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
	       Parsetree.I_global ((Some cstr_host_module), cstr_vname) } in
	 ((Parsetree.P_app (scoped_cstr, scoped_pats)), env')
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
	 ((Parsetree.P_paren p), env')) in
  (* Finally, return the whole scoped pattern and the initial     *)
  (* environment extended by the bindings induced by the pattern. *)
  ({ pattern with Parsetree.ast_desc = new_desc }, new_env)



(* **************************************************************** *)
(* toplevel_let: bool -> scoping_context -> Env.ScopingEnv.t ->     *)
(*   Parsetree.let_def -> (Parsetree.let_def * Env.ScopingEnv.t)    *)
(** {b Descr} : Scopes a let-definition. This returns the scoped
              bindings and the initial scoping environment extended
              with the information bound to the identifiers defined
              in the let-definition.
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
  (scoped_let_def, final_env)
;;



let scope_species_field ctx env field =
  let (new_desc, method_name) =
    (match field.Parsetree.ast_desc with
     | Parsetree.SF_rep rep_type_def -> failwith "S6"
     | Parsetree.SF_sig sig_def -> failwith "S7"
     | Parsetree.SF_let let_def -> failwith "S8"
     | Parsetree.SF_property prop_def -> failwith "S9"
     | Parsetree.SF_theorem theo_def -> failwith "S10"
     | Parsetree.SF_proof proof_def -> failwith "S11") in
  ({ field with Parsetree.ast_desc = new_desc }, method_name)
;;



let rec scope_species_fields ctx env = function
  | [] -> ([], [])
  | field :: rem ->
      let (scoped_field, method_name) = scope_species_field ctx env field in
      (* All the methods a always inserted as [SBI_method_of_self], the *)
      (* [find_value] taking care of changing to [SBI_method_of_coll]   *)
      (* when required.                                                 *)
      let env' =
	Env.ScopingEnv.add_value
	  method_name (Env.ScopeInformation.SBI_method_of_self) env in
      let (rem_scoped_fields, rem_method_names) =
	scope_species_fields ctx env' rem in
      ((scoped_field :: rem_scoped_fields), (method_name :: rem_method_names))
;;



let scope_species_param ctx env param =
  let new_desc =
    (match param.Parsetree.ast_desc with
     | Parsetree.SP expr -> Parsetree.SP (scope_expr ctx env expr)) in
  { param with Parsetree.ast_desc = new_desc }
;;



(* ******************************************************************** *)
(* scoping_context -> Env.ScopingEnv.t -> Parsetree.species_expr ->     *)
(*   (Parsetree.species_expr * Parsetree.vname list)                    *)
(** {b Descr} : Scopes a species expression. Returns the scoped
              expression and the list of methods names this expression
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
      ~current_unit: ctx.current_unit se_name_ident  env in
  let basic_vname = unqualified_vname_of_ident se_name_ident in
  let scoped_ident_descr =
    (match ident_scope_info.Env.ScopeInformation.spbi_scope with
     | Env.ScopeInformation.SPBI_file hosting_file ->
	 Parsetree.I_global ((Some hosting_file), basic_vname)) in
  let scoped_ident = {
    se_name_ident with Parsetree.ast_desc = scoped_ident_descr } in
  (* Scopes the parameters. *)
  let scoped_params =
    List.map
      (scope_species_param ctx env)
      species_expr_descr.Parsetree.se_params in
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
(** {b Descr} : Scopes the species parameters and return the scoping
              environment extended with bindings for theses parameters.
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
	       Env.ScopingEnv.find_collection
		 ~current_unit: ctx.current_unit ident accu_env in
	     let basic_vname = unqualified_vname_of_ident ident in
	     let scoped_ident_descr =
	       (match ident_scope_info.Env.ScopeInformation.cbi_scope with
		| Env.ScopeInformation.CBI_local ->
		    Parsetree.I_local basic_vname
		| Env.ScopeInformation.CBI_file hosting_file ->
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
	     (* not be done at the "values" level.                *)
	     let (scoped_species_expr, species_methods) =
	       scope_species_expr ctx env spec_expr in
	     (* Extend the environment with a "locally defined" collection *)
	     (* having the same methods than those coming from the         *)
	     (* expression.                                                *)
	     let accu_env' = 
	       Env.ScopingEnv.add_collection
                 (Parsetree_utils.name_of_vname param_name)
		 { Env.ScopeInformation.cbi_scope =
		     Env.ScopeInformation.CBI_local ;
		   Env.ScopeInformation.cbi_methods = species_methods }
		 accu_env in
	     let scoped_param_kind = {
	       param_kind with
	         Parsetree.ast_desc = Parsetree.SPT_is scoped_species_expr } in
	     let scoped_param = (param_name, scoped_param_kind) in
	     ((scoped_param :: accu_params_revd), accu_env')
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



let scope_phrase ctx env phrase =
  let (new_desc, new_env) =
    (match phrase.Parsetree.ast_desc with
     | Parsetree.Ph_external _
	 (* Nothing to scope here. External definition structurally *)
	 (* do not contain any  [ident]. The scoping environment is *)
	 (* not affected.                                           *)
     | Parsetree.Ph_use _
     | Parsetree.Ph_open _ -> (phrase.Parsetree.ast_desc, env)
     | Parsetree.Ph_species species_def ->
	 let (scoped_species_def, env') =
	   scope_species_def ctx env species_def in
	 ((Parsetree.Ph_species scoped_species_def), env')
     | Parsetree.Ph_coll coll_def -> failwith "todo S3"
     | Parsetree.Ph_type type_def ->
         let (scoped_ty_def, env') = scope_type_def ctx env type_def in
	 ((Parsetree.Ph_type scoped_ty_def), env')
     | Parsetree.Ph_let let_def ->
	 (* This one can extend the global scoping environment. *)
	 let (scoped_let_def, env') =
	   scope_let_definition ~toplevel_let: true ctx env let_def in
	 ((Parsetree.Ph_let scoped_let_def), env')
     | Parsetree.Ph_theorem theo_def -> failwith "todo S5"
     | Parsetree.Ph_expr expr_def ->
	 (* No scoping environment extention here. *)
	 let scoped_expr = Parsetree.Ph_expr (scope_expr ctx env expr_def) in
	 (scoped_expr, env)) in
  ({ phrase with Parsetree.ast_desc = new_desc }, new_env)
;;



(* ****************************************************************** *)
(* scope_file : Parsetree.fname -> Parsetree.file -> Parsetree.file   *)
(** {b Descr} : Starts the scoping phase on a whole file. The initial
              scoping environment is totally empty.

    {b Rem} : Exported outside this module.                           *)
(* ****************************************************************** *)
let scope_file current_unit file =
  match file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
       let ctx = { current_unit = current_unit } in
       (* Scoping of a file starts with the empty scoping environment. *)
       let global_env = ref (Env.ScopingEnv.pervasives ()) in
       let scoped_phrases =
	 List.map
	   (fun phrase ->
	     let (phrase', env') = scope_phrase ctx !global_env phrase in
	     global_env := env' ;
	     (* Return the scoped phrase. *)
	     phrase')
	   phrases in
       { file with Parsetree.ast_desc = Parsetree.File scoped_phrases }
;;
