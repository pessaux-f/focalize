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

(* $Id: substExpr.ml,v 1.24 2012-02-24 17:38:08 pessaux Exp $ *)

(* *********************************************************************** *)
(** {b Descr} : This module performs substitution of a value name [name_x]
            by an expression [by_expr]. This means that [name_x] will be
            replaced by [by_expr].
            The substitution operates in "values" expressions and is
            trigered by a "in" parameter application. Hence it only
            affects the I_local bindings or the I_global bindings whose
            "module" field is the same that the one passed in the
            argument [param_unit] to the substitution function (i.e. the
            module name of where the parmaterized species was defined.     *)
(* *********************************************************************** *)


let subst_E_var ~param_unit ~bound_variables name_x by_expr ident =
  (* Substitute in the AST node description. *)
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local vname ->
       if vname = name_x && not (List.mem vname bound_variables) then
         by_expr
       else Parsetree.E_var ident
   | Parsetree.EI_global (Parsetree.Qualified (modname, vname)) ->
       if modname = param_unit && vname = name_x &&
          not (List.mem vname bound_variables) then
         by_expr
       else Parsetree.E_var ident
   | Parsetree.EI_method (_, _) -> Parsetree.E_var ident
   | Parsetree.EI_global (Parsetree.Vname _) ->
       (* In this case, may be there is some scoping process missing. *)
       assert false
;;



(* ************************************************************************ *)
(* Parsetree.vname list -> Parsetree.pattern -> Parsetree.vname list        *)
(** {b Descr} : Returns an extended list of bound identifiers that must not
              be affected by a substitution. Especially, variables bound by
              a pattern are NOT subject to substitutions !

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let rec extend_bound_name_from_pattern bound_variables pattern =
  match pattern.Parsetree.ast_desc with
   | Parsetree.P_const _ | Parsetree.P_wild -> bound_variables
   | Parsetree.P_var vname -> vname :: bound_variables
   | Parsetree.P_as (pat', vname) ->
       extend_bound_name_from_pattern (vname :: bound_variables) pat'
   | Parsetree.P_constr (_, pats) ->
       (* Because the [ident] here is a sum type constructor, *)
       (* there is no risk of capture here.                   *)
       List.fold_left
         (fun accu_bounds p ->
           extend_bound_name_from_pattern accu_bounds p)
         bound_variables
         pats
   | Parsetree.P_record fields ->
       List.fold_left
         (fun accu_bounds (_, p) ->
           extend_bound_name_from_pattern accu_bounds p)
         bound_variables
         fields
   | Parsetree.P_tuple pats ->
       List.fold_left
         (fun accu_bounds p ->
           extend_bound_name_from_pattern accu_bounds p)
         bound_variables
         pats
   | Parsetree.P_paren pat' ->
       extend_bound_name_from_pattern bound_variables pat'
;;



(* ******************************************************************* *)
(* param_unit: Types.fname -> bound_variables: Parsetree.vname list -> *)
(*   Parsetree.vname -> Parsetree.expr_desc -> Parsetree.expr ->       *)
(*     Parsetree.expr                                                  *)
(** {b Descr} : Performs the substitution of a value name [name_x] by
              an expression [by_expr] inside an expression. Takes care
              not to substitute non free variables whose names are in
              the list [bound_variables].

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let rec __subst_expr ~param_unit ~bound_variables name_x by_expr expression =
  (* Let's just make a local recursive function to save the stack, avoiding *)
  (* passing each time the 3 arguments [~param_unit], [bound_variables]     *)
  (* [name_x] and [by_expr].                                                *)
  let rec rec_subst rec_bound_vars initial_expr =
    (* Substitute in the AST node description. *)
    let new_desc =
      (match initial_expr.Parsetree.ast_desc with
       | Parsetree.E_self
       | Parsetree.E_const _ ->
           (* Structurally, no possible change in expressions or types below. *)
           initial_expr.Parsetree.ast_desc
       | Parsetree.E_fun (arg_vnames, e_body) ->
           Parsetree.E_fun (arg_vnames, (rec_subst rec_bound_vars e_body))
       | Parsetree.E_var ident ->
           subst_E_var
             ~param_unit ~bound_variables: rec_bound_vars name_x by_expr ident
       | Parsetree.E_app (functional_expr, args_exprs) ->
           let functional_expr' = rec_subst rec_bound_vars functional_expr in
           let args_exprs' = List.map (rec_subst rec_bound_vars) args_exprs in
           Parsetree.E_app (functional_expr', args_exprs')
       | Parsetree.E_constr (cstr_expr, exprs) ->
           (* The constructor expression can not be substituted. *)
           let exprs' = List.map (rec_subst rec_bound_vars) exprs in
           Parsetree.E_constr (cstr_expr, exprs')
       | Parsetree.E_match (matched_expr, bindings) ->
           let matched_expr' = rec_subst rec_bound_vars matched_expr in
           let bindings' =
             List.map
               (fun (pat, expr) ->
                 (* Because patterns bind variables the substitution does *)
                 (* not affect the pattern's structure. However patterns  *)
                 (* bind variables and then make them insensitive to      *)
                 (* substitutions. So, first extend the list of bound     *)
                 (* variables with those introduced by the pattern.       *)
                 let bound_variables' =
                   extend_bound_name_from_pattern bound_variables pat in
                 let expr' = rec_subst bound_variables' expr in
                 (pat, expr'))
               bindings in
           Parsetree.E_match (matched_expr', bindings')
       | Parsetree.E_if (e_cond, e_then, e_else) ->
           let e_cond' = rec_subst rec_bound_vars e_cond in
           let e_then' = rec_subst rec_bound_vars e_then in
           let e_else' = rec_subst rec_bound_vars e_else in
           Parsetree.E_if (e_cond', e_then', e_else')
       | Parsetree.E_let (let_def, in_expr) ->
           let (let_def', bound_variables') =
             subst_let_definition
               ~param_unit ~bound_variables: rec_bound_vars name_x by_expr
               let_def in
           let in_expr' = rec_subst bound_variables' in_expr in
           Parsetree.E_let (let_def', in_expr')
       | Parsetree.E_record fields ->
           let fields' =
             List.map
               (fun (name, expr) -> (name, (rec_subst rec_bound_vars expr)))
               fields in
           Parsetree.E_record fields'
       | Parsetree.E_record_access (expr, label) ->
           Parsetree.E_record_access ((rec_subst rec_bound_vars expr), label)
       | Parsetree.E_record_with (with_expr, fields) ->
           let with_expr' = rec_subst rec_bound_vars with_expr in
           let fields' =
             List.map
               (fun (name, expr) -> (name, (rec_subst rec_bound_vars expr)))
               fields in
           Parsetree.E_record_with (with_expr', fields')
       | Parsetree.E_tuple exprs ->
           Parsetree.E_tuple (List.map (rec_subst rec_bound_vars) exprs)
       | Parsetree.E_sequence exprs ->
           Parsetree.E_sequence (List.map (rec_subst rec_bound_vars) exprs)
       | Parsetree.E_external _ ->
           (* Because this is in fact just a string, nowhere to substitute . *)
           initial_expr.Parsetree.ast_desc
       | Parsetree.E_paren expr ->
           Parsetree.E_paren (rec_subst rec_bound_vars expr)) in
    (* An finally, make a new AST node. *)
    { initial_expr with Parsetree.ast_desc = new_desc } in
  (* Do je job now. *)
  rec_subst bound_variables expression



and subst_let_binding ~param_unit ~bound_variables name_x by_expr binding =
  (* Substitute in the AST node description. *)
  let binding_desc = binding.Parsetree.ast_desc in
  (* We must first extend the bound variables list with the *)
  (* parameters of the currently defined identifier.        *)
  let bound_variables' =
    (List.map fst binding_desc.Parsetree.b_params) @ bound_variables in
  let b_body' =
    (match binding_desc.Parsetree.b_body with
     | Parsetree.BB_computational expr ->
         Parsetree.BB_computational
           (__subst_expr
              ~param_unit ~bound_variables: bound_variables' name_x by_expr
              expr)
     | Parsetree.BB_logical prop ->
         Parsetree.BB_logical
           (__subst_prop ~param_unit ~bound_variables: bound_variables'
              name_x by_expr prop)) in
  let desc' = { binding_desc with Parsetree.b_body = b_body' } in
  ({ binding with Parsetree.ast_desc = desc' }, binding_desc.Parsetree.b_name)



(* ************************************************************************** *)
(* param_unit: Types.fname -> bound_variables: Parsetree.vname list ->        *)
(*   Parsetree.vname -> Parsetree.expr_desc -> Parsetree.let_def ->           *)
(*     (Parsetree.let_def * Parsetree.vname list)                             *)
(** {b Descr} : Performs the substitution of a value name [name_x] by an
              expression [by_expr] inside a let-definition and returns the
              list of variables bound by this definition. These variable
              must then never be affected by a substitution because they are
              not free anymore.

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
and subst_let_definition ~param_unit ~bound_variables name_x by_expr let_def =
  let let_def_desc = let_def.Parsetree.ast_desc in
  (* Substitute in the AST node description. *)
  match let_def_desc.Parsetree.ld_rec with
   | Parsetree.RF_no_rec -> (
       (* In case of non recursive let-definition, the set of variables    *)
       (* insensitive to substitutions is incrementally extended. Do       *)
       (* not [fold_left] otherwise the list of bindings will be reversed. *)
       let (bound_variables', bindings') =
         List.fold_right
           (fun binding (accu_bvars, accu_bindings) ->
             let (binding', bound_var) =
               subst_let_binding
                 ~param_unit ~bound_variables: accu_bvars name_x by_expr
                 binding in
             ((bound_var :: accu_bvars), (binding' :: accu_bindings)))
           let_def_desc.Parsetree.ld_bindings
           (bound_variables, []) in
       let desc' = { let_def_desc with Parsetree.ld_bindings = bindings' } in
       ({ let_def with Parsetree.ast_desc = desc' }, bound_variables')
      )
   | Parsetree.RF_rec -> (
       (* First get all the bound variables in the recursive let-definition. *)
       let bound_variables' =
         List.map
           (fun binding -> binding.Parsetree.ast_desc.Parsetree.b_name) 
           let_def_desc.Parsetree.ld_bindings in
       (* An now, substitute inside the definitions with all these *)
       (* insensitive variables known. Note that we can ignore the *)
       (* returned bound variable because it is still in our       *)
       (* "recursive" bound vars list.                             *)
       let (bindings', _) =
         List.split
           (List.map
              (subst_let_binding
                 ~param_unit ~bound_variables: bound_variables' name_x by_expr)
              let_def_desc.Parsetree.ld_bindings) in
       let desc' = { let_def_desc with Parsetree.ld_bindings = bindings' } in
       ({ let_def with Parsetree.ast_desc = desc' }, bound_variables')
      )




and __subst_prop ~param_unit ~bound_variables name_x by_expr initial_prop_expr =
  (* Just  local recursive function to save the stack. *)
  let rec rec_subst rec_bound_vars prop_expr =
    (* Substitute in the AST node description. *)
    let new_desc =
      (match prop_expr.Parsetree.ast_desc with
       |  Parsetree.Pr_forall (vnames, type_expr, prop) ->
           let bound_variables' = vnames @ bound_variables in
           let body' = rec_subst bound_variables' prop in
           Parsetree.Pr_forall (vnames, type_expr, body')
       | Parsetree.Pr_exists (vnames, type_expr, prop) ->
           let bound_variables' = vnames @ bound_variables in
           let body' = rec_subst bound_variables' prop in
           Parsetree.Pr_exists (vnames, type_expr, body')
       | Parsetree.Pr_imply (prop1, prop2) ->
           let prop1' = rec_subst rec_bound_vars prop1 in
           let prop2' = rec_subst rec_bound_vars prop2 in
           Parsetree.Pr_imply (prop1', prop2')
       | Parsetree.Pr_or (prop1, prop2) ->
           let prop1' = rec_subst rec_bound_vars prop1 in
           let prop2' = rec_subst rec_bound_vars prop2 in
           Parsetree.Pr_or (prop1', prop2')
       | Parsetree.Pr_and (prop1, prop2) ->
           let prop1' = rec_subst rec_bound_vars prop1 in
           let prop2' = rec_subst rec_bound_vars prop2 in
           Parsetree.Pr_and (prop1', prop2')
       | Parsetree.Pr_equiv (prop1, prop2) ->
           let prop1' = rec_subst rec_bound_vars prop1 in
           let prop2' = rec_subst rec_bound_vars prop2 in
           Parsetree.Pr_equiv (prop1', prop2')
       | Parsetree.Pr_not prop ->
           Parsetree.Pr_not (rec_subst rec_bound_vars prop)
     | Parsetree.Pr_expr expr ->
         let expr' =
           __subst_expr ~param_unit ~bound_variables name_x by_expr expr in
         Parsetree.Pr_expr expr'
     | Parsetree.Pr_paren prop ->
         Parsetree.Pr_paren (rec_subst rec_bound_vars prop)) in
    { prop_expr with Parsetree.ast_desc = new_desc } in
  (* Now do the job. *)
  rec_subst bound_variables initial_prop_expr
;;



let subst_binding_body ~param_unit ~bound_variables name_x by_expr = function
  | Parsetree.BB_computational e ->
      Parsetree.BB_computational
        (__subst_expr ~param_unit ~bound_variables name_x by_expr e)
  | Parsetree.BB_logical p ->
      Parsetree.BB_logical
        (__subst_prop ~param_unit ~bound_variables name_x by_expr p)
;;



(* ********************************************************************* *)
(** {b Descr} : Hides the initially empty [~bound_variables] for calling
    outside the context of a recursion on the AST. Useful for "in"
    parameters instanciations while generating collection generators.

   {b Rem} : Exported outside this module.                                 *)
(* ********************************************************************* *)
let subst_expr ~param_unit name_x ~by_expr ~in_expr =
  __subst_expr ~param_unit ~bound_variables: [] name_x by_expr in_expr
;;



(* ********************************************************************* *)
(** {b Descr} : Hides the initially empty [~bound_variables] for calling
    outside the context of a recursion on the AST. Useful for "in"
    parameters instanciations while generating collection generators.

   {b Rem} : Exported outside this module.                                 *)
(* ********************************************************************* *)
let subst_prop ~param_unit name_x by_expr in_prop =
  __subst_prop ~param_unit ~bound_variables: [] name_x by_expr in_prop
;;



(** {b Rem} : Exported outside this module. *)
let subst_species_field ~param_unit name_x by_expr field =
  match field with
  | Env.TypeInformation.SF_sig (_, _, _) -> field   (* Nowhere to substitute. *)
  | Env.TypeInformation.SF_let
      (from, vname, params_names, scheme, body, opt_proof, dep, log_flag) ->
      (begin
      let bound_variables = [vname] in
      let body' =
        subst_binding_body ~param_unit ~bound_variables name_x by_expr body in
      Env.TypeInformation.SF_let
        (from, vname, params_names, scheme, body', opt_proof, dep, log_flag)
      end)
  | Env.TypeInformation.SF_let_rec l ->
      (* First get all the recursive bound variables. *)
      let bound_variables =
        List.map (fun (_, vname, _, _, _, _, _, _) -> vname) l in
      let l' =
        List.map
          (fun (from, vname, params_names, scheme, body, opt_proof,
                dep, log_flag) ->
            let body' =
              subst_binding_body
                ~param_unit ~bound_variables name_x by_expr body in
            (from, vname, params_names, scheme, body', opt_proof,
             dep, log_flag))
          l in
      Env.TypeInformation.SF_let_rec l'
  | Env.TypeInformation.SF_theorem
      (from, vname, scheme, body, proof, deps_rep) ->
      (begin
      (* No substitution inside the proof. *)
      let bound_variables = [vname] in
      let body' =
        __subst_prop ~param_unit ~bound_variables name_x by_expr body in
      Env.TypeInformation.SF_theorem
        (from, vname, scheme, body', proof, deps_rep)
      end)
  | Env.TypeInformation.SF_property (from, vname, scheme, body, deps_rep) ->
      (begin
      let bound_variables = [vname] in
      let body' =
        __subst_prop ~param_unit ~bound_variables name_x by_expr body in
      Env.TypeInformation.SF_property (from, vname, scheme, body', deps_rep)
      end)

;;
