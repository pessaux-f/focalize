(* $Id: scoping.ml,v 1.1 2007-08-04 10:17:17 pessaux Exp $ *) 

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



let rec scope_expr ctx env expr =
  let new_desc =
    (match expr.Parsetree.ast_desc with
     | Parsetree.E_self ->
	 expr.Parsetree.ast_desc  (* Nothing to scope. *)
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
	 let (hosting_info, basic_vname) =
	   Env.ScopingEnv.find_value
	     ~current_unit: ctx.current_unit ident env in
	 (* Let's re-construct a completely scoped identifier. *)
	 let scoped_ident_descr =
	   (match hosting_info with
	    | Env.ScopeInformation.SBI_file fname_opt ->
		Parsetree.I_global (fname_opt, basic_vname)
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
     | Parsetree.E_let (let_def, in_expr) -> failwith "todo"
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
	 let (cstr_host_module, cstr_vname) =
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
;;



let scope_phrase ctx env phrase =
  let new_desc =
    (match phrase.Parsetree.ast_desc with
     | Parsetree.Ph_external exter_def ->
	 (* Nothing to scope here. External definition structurally *)
	 (* do not contain any  [ident]. The scoping environment is *)
	 (* not affected.                                           *)
	 Parsetree.Ph_external exter_def
     | Parsetree.Ph_use fname -> Parsetree.Ph_use fname
     | Parsetree.Ph_open fname -> Parsetree.Ph_open fname
     | Parsetree.Ph_species species_def -> failwith "todo"
     | Parsetree.Ph_coll coll_def -> failwith "todo"
     | Parsetree.Ph_type type_def -> failwith "todo"
     | Parsetree.Ph_let let_def ->
	 (* This one can extend the global scoping environment. *)
	 failwith "todo"
     | Parsetree.Ph_theorem theorem_def -> failwith "todo"
     | Parsetree.Ph_expr expr_def ->
	 (* No scoping environment extention there. *)
	 Parsetree.Ph_expr (scope_expr ctx env expr_def)) in
  { phrase with Parsetree.ast_desc = new_desc }
;;



(* ****************************************************************** *)
(* scope_file : Parsetree.file -> Parsetree.file                      *)
(** {b Descr} : Starts the scoping phase on a whole file. The initial
              scoping environment is totally empty.

    {b Rem} : Exported outside this module.                           *)
(* ****************************************************************** *)
let scope_file file =
  match file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
       (* Scoping of a file starts with the empty scoping environment. *)
       let global_env = ref (Env.ScopingEnv.empty ()) in
       let scoped_phrases =
	 List.map
	   (fun phrase ->
	     let ctx = { current_unit = failwith "todo" } in
	     let phrase' = scope_phrase ctx !global_env phrase in
	     (* Return the scoped phrase. *)
	     phrase')
	   phrases in
       { file with Parsetree.ast_desc = Parsetree.File scoped_phrases }
;;
