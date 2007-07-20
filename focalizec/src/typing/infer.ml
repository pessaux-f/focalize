(* $Id: infer.ml,v 1.5 2007-07-20 08:14:47 pessaux Exp $ *)

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

(* *********************************************************************** *)
(** {bL Descr} : Exception used to inform that a sum type constructor was
               used with an incorrect arity. The correct expected arity is
              stored in the second argument of the exception constructor.  *)
exception Bad_constructor_arity of (Parsetree.ident * Env.constructor_arity) ;;
(* *********************************************************************** *)


exception Unbound_type_variable of string ;;
(* Expected arity, used with arity. *)
exception Bad_type_arity of (Parsetree.ident * int * int) ;;


(** To become the context recording various information propagated for the
    type inference. May be could also contain the environment. *)
type typing_context = {
  (** Optional type Self is known to be equal to. *)
  self_manifest : Types.simple_type option ;
  (** Mapping between 'variables and the [simpletype] they are bound to.
      Used when creating a polymorphic type definition. *)
  tyvars_mapping : (string * Types.simple_type) list
} ;;

(*
type rep_type_def = rep_type_def_desc ast_doc
and rep_type_def_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_def * rep_type_def
  | RTE_app of ident * rep_type_def list
  | RTE_prod of rep_type_def * rep_type_def
  | RTE_paren of rep_type_def
;;
*)



(* ******************************************************************* *)
(* typing_context -> Env.t -> Parsetree.type_expr -> Types.simple_type *)
(** {b Descr} : Translates a type expression into a [simple_type].
                Variable are translated according to the mapping found
                inside the current context. Hence, in case this
                function is used to create the body type of a type
                definition, if the definition is polymorphic, the
                parameter-variables must already exist inside  the
                mapping.

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let rec typecheck_type_expr ctx env ty_expr =
  let final_ty =
    (match ty_expr.Parsetree.ast_desc with
     | Parsetree.TE_ident ident ->
	 (begin
	 match ident.Parsetree.ast_desc with
	  | Parsetree.I_local (Parsetree.Vqident quote_name) ->
	      (begin
	      (* Just handle the special where the ident is a type variable. *)
	      try List.assoc quote_name ctx.tyvars_mapping
	      with Not_found -> raise (Unbound_type_variable quote_name)
	      end)
	  | _ ->
	      (* Case of all 0-ary other user-defined type constructors. *)
	      let ty_descr = Env.find_type ident env in
	      if ty_descr.Env.type_arity <> 0 then
		raise (Bad_type_arity (ident, ty_descr.Env.type_arity, 0))
	      else Types.specialize ty_descr.Env.type_identity
	 end)
     | Parsetree.TE_fun (ty_expr1, ty_expr2) ->
	 Types.type_arrow
	   (typecheck_type_expr ctx env ty_expr1)
	   (typecheck_type_expr ctx env ty_expr2)
     | Parsetree.TE_app (ty_cstr_ident, args_ty_exprs) ->
	 (begin
	 let ty_descr = Env.find_type ty_cstr_ident env in
	 (* Check the type constructor's arity. *)
	 let args_ty_len = List.length args_ty_exprs in
	 if args_ty_len <> ty_descr.Env.type_arity then
	   raise
	     (Bad_type_arity
		(ty_cstr_ident, ty_descr.Env.type_arity, args_ty_len)) ;
	 (* Synthetise the types for the arguments. *)
	 let args_ty = List.map (typecheck_type_expr ctx env) args_ty_exprs in
	 (* Now get a fresh instance of the type's type scheme, directly  *)
	 (* mapping its gener&lised variables onto the arguments. This    *)
	 (* saves the need to know which variables have been instanciated *)
	 (* in order to unify them afterwards with the corresponding type *)
	 (* of the corresponding argument.                                *)
	 Types.specialize_and_instanciate ty_descr.Env.type_identity args_ty
	 end)
     | Parsetree.TE_prod (ty_expr1, ty_expr2) ->
	 failwith "Plonger a*b dans une liste ?"
     | Parsetree.TE_self -> Types.type_self ()
     | Parsetree.TE_prop -> Types.type_prop ()
     | Parsetree.TE_paren inner -> typecheck_type_expr ctx env inner) in
  (* Store the type information in the expression's node. *)
  ty_expr.Parsetree.ast_type <- Some final_ty ;
  final_ty
;;



 (* If returns true then one can generalise. *)
let rec is_non_expansive env expr =
  match expr.Parsetree.ast_desc with
   | Parsetree.E_const _ -> true
   | Parsetree.E_var _ -> true
   | Parsetree.E_let (let_def, body) ->
       (begin
       (List.for_all
          (fun binding ->
	    let bound_expr = binding.Parsetree.ast_desc.Parsetree.b_body in
	    (* Be careful. Consider the comment in the function    *)
            (* [typecheck_let_definition] dealing with body hiding *)
	    (* the functional aspect of the whole definition.      *)
	    binding.Parsetree.ast_desc.Parsetree.b_params <> [] ||
	    is_non_expansive env bound_expr)
          let_def.Parsetree.ast_desc.Parsetree.ld_bindings)
	 &&
       (is_non_expansive env body)
       end)
   | Parsetree.E_fun _ -> true
   | Parsetree.E_tuple exprs -> List.for_all (is_non_expansive env) exprs
   | Parsetree.E_record lbl_exp_list ->
       (begin
       List.for_all
         (fun (lbl, e) ->
           let lbl_descr = Env.find_label lbl env in
           (lbl_descr.Env.field_mut = Env.FM_immutable) &&
	   (is_non_expansive env e))
         lbl_exp_list
       end)
   | Parsetree.E_record_access (e, _) -> is_non_expansive env e
   | Parsetree.E_constr (_, exprs) ->
       List.for_all (is_non_expansive env) exprs
   | Parsetree.E_paren e -> is_non_expansive env e
   | _ -> false
;;



let typecheck_constant constant_desc =
  let ty =
    match constant_desc.Parsetree.ast_desc with
     | Parsetree.C_int _ -> Types.type_int ()
     | Parsetree.C_float _ -> Types.type_float ()
     | Parsetree.C_bool _ -> Types.type_bool ()
     | Parsetree.C_string _ -> Types.type_string ()
     | Parsetree.C_char _ -> Types.type_char () in
  constant_desc.Parsetree.ast_type <- Some ty ;
  ty
;;



let rec typecheck_pattern ctx env pat_desc =
  (* First, get the pattern's type and induced bindings. *)
  let (final_ty, bindings) =
    (match pat_desc.Parsetree.ast_desc with
     | Parsetree.P_const cst -> ((typecheck_constant cst), [])
     | Parsetree.P_var var ->
	 let var_ty = Types.type_variable () in
	 (var_ty, [ (var, (Types.trivial_scheme var_ty)) ])
     | Parsetree.P_as (pat, alias) ->
	 let (ty, bindings) = typecheck_pattern ctx env pat in
	 (ty, ((alias, (Types.trivial_scheme ty)) :: bindings))
     | Parsetree.P_wild ->
	 let var_ty = Types.type_variable () in
	 (var_ty, [])
     | Parsetree.P_app (cstr_name, pats) ->
	 (begin
	 (* Find a specialization of the constructor's type scheme. *)
	 let cstr_decl = Env.find_constructor cstr_name env in
	 match (pats, cstr_decl.Env.cstr_arity) with
	  | ([], Env.CA_zero) ->
	      let cstr_ty = Types.specialize cstr_decl.Env.cstr_scheme in
	      (cstr_ty, [])
	  | (nempty_pats, Env.CA_one) ->
	      let cstr_ty = Types.specialize cstr_decl.Env.cstr_scheme in
	      (* Recover the type of the sub-patterns by typechecking an  *)
	      (* artificial tuple argument compound of the sub-patterns.  *)
	      (* Proceed this way EVEN if there is ONE argument. We then  *)
	      (* in effect have degenerated tuples with only 1 component. *)
              let (cstr_arg_ty, sub_bindings) =
		typecheck_pattern ctx env
		  { pat_desc with
		     Parsetree.ast_desc = Parsetree.P_tuple nempty_pats } in
              (* Constructeurs being functions, we will unify [cstr_type]   *)
	      (* with an arrow type to ensure that it is really one and     *)
	      (* to ensure the arguments types and extract the result type. *)
              let cstr_res_ty = (Types.type_variable ()) in
              Types.unify
		~self_manifest: ctx.self_manifest
		(Types.type_arrow cstr_arg_ty cstr_res_ty) cstr_ty ;
	      (cstr_res_ty, sub_bindings)
	  | (_, _) ->
	      (* Just raise the exception with the right expected arity. *)
	      raise
		(Bad_constructor_arity (cstr_name, cstr_decl.Env.cstr_arity))
	 end)
     | Parsetree.P_record label_n_patterns ->
	 (begin
	 (* Type for this pattern. Will be instanciated by side effect. *)
	 let whole_pat_ty = Types.type_variable () in
	 (* Infer type of eack sub-pattern. *)
         let bindings =
	   List.flatten
             (List.map
		(fun (lbl, pat) ->
		  (* Get bindings and sub-pattern type. *)
		  let (sub_pat_ty, bnds) = typecheck_pattern ctx env pat in
		  let lbl_desc = Env.find_label lbl env in
		  (* Get the related label type. *)
		  let lbl_ty = Types.specialize lbl_desc.Env.field_scheme in
		  (* Now, ensure the 2 sub-pattners types are compatible and *)
		  (* that the resulting record type for the whole pattern is *)
		  (* consistent.                                             *)
		  Types.unify
		    ~self_manifest: ctx.self_manifest
		    lbl_ty (Types.type_arrow sub_pat_ty whole_pat_ty) ;
		  (* Just returns the bindings. *)
		  bnds)
		label_n_patterns) in
	 (whole_pat_ty, bindings)
	 end)
     | Parsetree.P_tuple pats ->
         let (tys, bindings) =
	   List.split (List.map (typecheck_pattern ctx env) pats) in
	 let ty = Types.type_tuple tys in
	 (ty, (List.flatten bindings))
     | Parsetree.P_paren pat -> typecheck_pattern ctx env pat) in
  (* And now, store the type information inside the node. *)
  pat_desc.Parsetree.ast_type <- Some final_ty ;
  (* Return both infered type and bindings. *)
  (final_ty, bindings)
;;


(*
type external_language =
  | EL_Caml
  | EL_Coq
  | EL_external of string
;;

type external_def = external_def_desc ast
and external_def_desc =
  | ED_type of external_def_body
  | ED_value of external_def_body

and external_def_body = external_def_body_desc ast
and external_def_body_desc = {
  ed_name : vname ;
  ed_body : external_expr
}
*)



(* Does not make any assumption. Crudely returns a fresh type variable. *)
let typecheck_external_expr ext_expr =
  let ty = Types.type_variable () in (* A somewhat of magic obj... *)
  ext_expr.Parsetree.ast_type <- Some ty ;
  ty
;;



(*
let rec typecheck_species_def env species_def_desc =
  (* Ignore params for the moment. *)
  if species_def_desc
  (* One must infer the type of the current field's bodies *)
  (* assuming all the method's types in the environment.   *)

  (* Then one must ensure that each method has a   *)
  (* same type everywhere in the inheritance tree. *)
;;
*)
(*
and species_def_desc = {
  sd_name : Types.sname ;
  sd_params : (vname * species_param_type) list ;
  sd_inherits : (species_expr list) ast_doc ;
  sd_fields : species_field list
}
*)
(*
and species_param_type = species_param_type_desc ast
and species_param_type_desc =
  | SPT_in of ident
  | SPT_is of species_expr

and species_expr = species_expr_desc ast
and species_expr_desc = {
  se_name : ident ;
  se_params : species_param list
}

and species_param = species_param_desc ast
and species_param_desc =
  | SP of expr

and sig_def = sig_def_desc ast_doc
and sig_def_desc = {
  sig_name : ident ;
  sig_type: type_expr
}

and proof_def = proof_def_desc ast_doc
and proof_def_desc = {
  pd_name : ident;
  pd_proof: proof
}

and property_def = property_def_desc ast_doc
and property_def_desc = {
  prd_name : ident ;
  prd_prop: prop
}

and species_field = species_field_desc ast
and species_field_desc =
  | SF_rep of rep_type_def
  | SF_sig of sig_def
  | SF_let of let_def
  | SF_property of property_def
  | SF_theorem of theorem_def
  | SF_proof of proof_def

and theorem_def = theorem_def_desc ast_doc
and theorem_def_desc = {
  th_name : ident ;
  th_loc : loc_flag ;
  th_stmt : prop ;
  th_proof : proof
}

and fact = fact_desc ast
and fact_desc =
  | F_def of ident list
  | F_property of ident list
  | F_hypothesis of vname list
  | F_node of node_label list

and proof = proof_desc ast
and proof_desc =
  | Pf_assumed
  | Pf_auto of fact list
  | Pf_coq of string
  | Pf_node of proof_node list

and proof_node = proof_node_desc ast
and proof_node_desc =
  | PN_sub of node_label * statement * proof
  | PN_qed of node_label * proof

and statement = statement_desc ast
and statement_desc = {
  s_hyps : hyp list ;
  s_concl : prop option
}

and hyp = hyp_desc ast
and hyp_desc =
  | H_var of vname * type_expr
  | H_hyp of vname * prop
  | H_not of vname * expr

and prop = prop_desc ast
and prop_desc =
  | Pr_forall of vname list * type_expr option * prop
  | Pr_exists of vname list * type_expr option * prop
  | Pr_imply of prop * prop
  | Pr_or of prop * prop
  | Pr_and of prop * prop
  | Pr_equiv of prop * prop
  | Pr_not of prop
  | Pr_expr of expr
  | Pr_paren of prop
*)



let rec typecheck_expr ctx env initial_expr =
  let final_ty =
    (match initial_expr.Parsetree.ast_desc with
     | Parsetree.E_const cst -> typecheck_constant cst
     | Parsetree.E_fun (arg_vnames, e_body) ->
	 (begin
	 (* Create the type for each argument .*)
	 let args_ty = List.map (fun _ -> Types.type_variable ()) arg_vnames in
	 (* Build the environment extended by these arguments and types. *)
	 (* Preferably use a fold_left instead of a fold_right, this way *)
	 (* arguments are "cons-ed" in the environment in their order of *)
	 (* declaration. Because arguments can't depend on each other,   *)
	 (* that's not a big matter, but that's cleaner...               *)
         let extended_env =
           List.fold_left2
	     (fun accu_env arg_name arg_ty ->
	       (* Do not generalize argument types ! No Mu-rule yet ! *)
	       Env.add_ident arg_name (Types.trivial_scheme arg_ty) accu_env)
	     env arg_vnames args_ty in
	 (* Now, typecheck the body i nthe new environment. *)
	 let ty_body = typecheck_expr ctx extended_env e_body in
	 (* Remains to build the final functional type. And do not *)
	 (* fold_left, otherwise you'll get a mirrored type !      *)
	 List.fold_right
	   (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
	   args_ty ty_body
	 end)
     | Parsetree.E_var ident ->
	 (* E_var is never "self" because "self" is a dedicated case. *)
	 (* Now, don't bother with the search order, this has already *)
	 (* be done by both the scoping and the environment build     *)
	 (* process. As reminder, lookup will naturally find the      *)
         (* ident among local identifiers, in-params, is-params,      *)
         (* inheritance and finally global identifiers.               *)
         Types.specialize (Env.find_ident ident env)
     | Parsetree.E_app (functional_expr, args_exprs) ->
	 (begin
	 let fun_ty = typecheck_expr ctx env functional_expr in
	 let ty_exprs = List.map (typecheck_expr ctx env) args_exprs in
	 List.fold_left
           (fun accu_fun_ty arg_ty ->
	     (* Type returned after this application step. *)
	     let result_ty = Types.type_variable () in
	     (* Temporary functionnal type to unify with *)
	     (* the type of the current applicator.      *)
	     let tmp_fun_ty = Types.type_arrow arg_ty result_ty in
	     Types.unify
	       ~self_manifest: ctx.self_manifest tmp_fun_ty accu_fun_ty ;
	     (* The result is the positive part of the arrow. *)
	     result_ty)
           fun_ty
	   ty_exprs
	 end)
     | Parsetree.E_constr (constr, exprs) ->
	 (begin
	 (* Because the environment maps [idents] onto types scheme and *)
         (* because the constructor's name is not a "full" ident, we    *)
         (* just wrap the the constructor's name into a global [ident]  *)
         (* to be able to lookup inside the environment.                *)
	 let pseudo_ident = {
	   Parsetree.ast_loc = constr.Parsetree.ast_loc ;
	   Parsetree.ast_desc = Parsetree.I_global constr.Parsetree.ast_desc ;
	   Parsetree.ast_doc = constr.Parsetree.ast_doc ;
	   Parsetree.ast_type = None } in
	 let cstr_decl = Env.find_constructor pseudo_ident env in
	 match (exprs, cstr_decl.Env.cstr_arity) with
	  | ([], Env.CA_zero) ->
	      (* Just get an instance of the constructor's type scheme. *)
	      Types.specialize cstr_decl.Env.cstr_scheme
          | (_, Env.CA_one) ->
	      (begin
	      (* The constructor must be viewed as a function. *)
	      let tys = List.map (typecheck_expr ctx env) exprs in
	      (* Get an instance of the constructor's type scheme. *)
	      let cstr_ty = Types.specialize cstr_decl.Env.cstr_scheme in
	      (* Build the shadow tuple type as the *)
	      (* real argument of the constructor.  *)
	      let cstr_arg_ty = Types.type_tuple tys in
	      (* Get a hand on what will be our final type result... *)
	      let result_ty = Types.type_variable () in
	      (* And simulate an application. *)
	      Types.unify
		~self_manifest: ctx.self_manifest
		cstr_ty (Types.type_arrow cstr_arg_ty result_ty) ;
	      result_ty
	      end)
       | (_, _) ->
	   raise
	     (Bad_constructor_arity (pseudo_ident, cstr_decl.Env.cstr_arity))
	 end)
     | Parsetree.E_match (matched_expr, bindings) ->
	 (begin
	 let matched_expr_ty = typecheck_expr ctx env matched_expr in
	 (* Let's get a fresh type accumulator  *)
	 (* to unify the clauses' bodies types. *)
	 let result_ty = Types.type_variable () in
	 (* Process each clause of the match. *)
	 List.iter
	   (fun (pat, expr) ->
	     (* Infer the type and bindings induced by the pattern. *)
	     let (pat_ty, bnds) = typecheck_pattern ctx env pat in
	     (* Ensure the matched expression and *)
	     (* the pattern have the same type.   *)
	     Types.unify
	       ~self_manifest: ctx.self_manifest matched_expr_ty pat_ty ;
	     (* Extend the environment with the pattern type bindings. *)
	     let env' =
	       List.fold_left
		 (fun accu_env (id, ty_scheme) ->
		   Env.add_ident id ty_scheme accu_env)
		 env bnds in
	     (* Infer the type of the match clause's body. *)
	     let clause_ty = typecheck_expr ctx env' expr in
	     (* Force every bodies to have the same result type. *)
	     Types.unify
	       ~self_manifest: ctx.self_manifest result_ty clause_ty)
	   bindings ;
	 (* Return the type of the bodies' clauses. *)
	 result_ty
	 end)
     | Parsetree.E_if (e_cond, e_then, e_else) ->
	 (begin
	 let ty_cond = typecheck_expr ctx env e_cond in
	 (* Ensure the condition is a boolean. *)
	 Types.unify
	   ~self_manifest: ctx.self_manifest ty_cond (Types.type_bool ()) ;
	 (* Typecheck the "then" expression. *)
	 let ty_then = typecheck_expr ctx env e_then in
	 let ty_else = typecheck_expr ctx env e_else in
	 (* Enforce both branches to have the same type. *)
	 Types.unify ~self_manifest: ctx.self_manifest ty_then ty_else ;
	 (* And return any of them as result type. *)
	 ty_then
	 end)
     | Parsetree.E_let (let_def, in_expr) ->
	 (* Don't increase level, this will be done in the let inference. *)
	 let bindings = typecheck_let_definition ctx env let_def in
	 (* Let's build the environment for typing the ending expression. *)
	 let env' =
	   List.fold_left
	     (fun accu_env (id, ty_scheme) ->
	       Env.add_ident id ty_scheme accu_env)
             env bindings in
	 typecheck_expr ctx env' in_expr
     | Parsetree.E_record fields -> typeckeck_record_expr ctx env fields None
     | Parsetree.E_record_access (expr, label) ->
	 (begin
	 let ty_expr = typecheck_expr ctx env expr in
	 let label_desc = Env.find_label label env in
	 (* Just remind that labels are types as functions of type     *)
	 (* "type of the field as seen by user -> type od the record". *)
	 let label_ty = Types.specialize label_desc.Env.field_scheme in
	 (* Get a holder to extract the "arg type of the function", *)
	 (* i.e. the type of the field as seen by the user.         *)
	 let result_ty = Types.type_variable () in
	 Types.unify
	   ~self_manifest: ctx.self_manifest
	   (Types.type_arrow result_ty ty_expr) label_ty ;
	 result_ty
	 end)
     | Parsetree.E_record_with (with_expr, fields) ->
         typeckeck_record_expr ctx env fields (Some with_expr)
     | Parsetree.E_tuple exprs ->
	 (begin
          assert (exprs <> []) ;  (* Just in case. O-ary tuple is non-sense ! *)
          let tys = List.map (typecheck_expr ctx env) exprs in
          Types.type_tuple tys
	 end)
     | Parsetree.E_external ext_expr -> typecheck_external_expr ext_expr
     | Parsetree.E_paren expr -> typecheck_expr ctx env expr) in
  (* Store the type information in the expression's node. *)
  initial_expr.Parsetree.ast_type <- Some final_ty ;
  final_ty



(* ****************************************************************** *)
(* typeckeck_record_expr :                                            *)
(*          Env.t -> (Types.label_name * Parsetree.expr) list ->      *)
(*            Parsetree.expr option -> Types.simple_type              *)
(** {b Descr} : Performs type inference on record expressions with or
              without "with" clause. Currently, the labels
              exhaustivity is not checked. It has to be done when
              there is no "with" clause.

    {b Args} :
      - env : The current typing environment.
      - fields : The list of fields values of the record expression.
      - opt_with_expr : The optional "with" clause.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
and typeckeck_record_expr ctx env fields opt_with_expr =
  (* At then end, must be the type of the host of all these labels. *)
  let result_ty = Types.type_variable () in
  (* Typecheck the "with" construct if any. *)
  (match opt_with_expr with
   | None ->
       (* To disapear once implemented ! *)
       Printf.eprintf "Labels exhaustivity not checked on record expression.\n"
   | Some expr ->
       let expr_ty = typecheck_expr ctx env expr in
       Types.unify ~self_manifest: ctx.self_manifest expr_ty result_ty) ;
  (* Now proceed with the labels.                               *)
  (* Just remind that labels are types as functions of type     *)
  (* "type of the field as seen by user -> type od the record". *)
  List.iter
    (fun (label, expr) ->
      let expr_ty = typecheck_expr ctx env expr in
      let lbl_descr = Env.find_label label env in
      (* Get the functionnal type of this field. *)
      let field_ty = Types.specialize lbl_descr.Env.field_scheme in
      (* Unify the result type by side effect. *)
      Types.unify
	~self_manifest: ctx.self_manifest
	(Types.type_arrow expr_ty result_ty) field_ty)
    fields ;
  result_ty



and typecheck_let_definition ctx env let_def =
  let let_def_descr = let_def.Parsetree.ast_desc in
  (* Get information to possibly build the pre-environment  *)
  (* , i.e. the induced environment bindings between idents *)
  (* and types and if they can be generalised or not.       *)
  let pre_env_info =
    List.map
      (fun { Parsetree.ast_desc = binding } ->
        (* Just use a hack telling that if the expression won't be *)
        (* generalisable, to typecheck it, we don't change the     *)
	(* generalisation level, then we won't generalise it.      *)
	(* Becareful, functions are non_expansive. Because the     *)
	(* structure of the let-def includes the parameters of the *)
	(* bound ident, a fun like [let f (x) = body] will not be  *)
        (* considered as non_expansive if [body] is not because    *)
        (* [body] hides the function because the arguments are     *)
        (* recorded in the [b_params] field. Hence, if the list    *)
        (* [b_params] is not empty, then the bound expression is a *)
        (* a function and is non_expansive whatever the body is.   *)
	if binding.Parsetree.b_params <> [] ||
           is_non_expansive env binding.Parsetree.b_body then
	  (begin
	  (* The body expression will be authorised to be generalised. *)
	  Types.begin_definition () ;
	  let ty = Types.type_variable () in
          Types.end_definition () ;
	  (* Say "true" to mean "generalisable" (implies that a  *)
	  (* begin/end_definition have been performed) .         *)
	  (binding.Parsetree.b_name, ty, true)
	  end)
	else
	  (begin
	  (* The body expression won't be authorised to be generalised. *)
	  let ty = Types.type_variable () in
	  (* Say "false" to mean "NON-generalisable" (implies that *)
	  (* no begin/end_definition have been performed) .        *)
	  (binding.Parsetree.b_name, ty, false)
	  end))
      let_def_descr.Parsetree.ld_bindings in
  (* Now let's address the bindings bodies ! If the definition is recursive *)
  (* then we extend the current environment with the assumed types.         *)
  let env' =
    (if let_def_descr.Parsetree.ld_rec = Parsetree.RF_no_rec then env
    else
      List.fold_left
	(fun accu_env (vname, ty, _) ->
	  (* No generalisation (polymorphism) of the function *)
	  (* inside its body (that's would be Mu-rule).       *)
	  let scheme = Types.trivial_scheme ty in
	  Env.add_ident vname scheme accu_env)
	env pre_env_info) in
  (* Now typecheck each def's body. *)
  let env_bindings =
    List.map2
      (fun { Parsetree.ast_desc = binding } (_, assumed_ty, non_expansive) ->
        (* Build a type for the arguments of the bound identier if there are *)
        (* some. If they have type constraints, then use it as primary type  *)
        (* instead of using a type variable that we should unify afterwards. *)
	if non_expansive then Types.begin_definition () ;
	let args_tys =
	  List.map
	    (fun (_, opt_arg_ty_expr) ->
	      match opt_arg_ty_expr with
	       | None -> Types.type_variable ()
	       | Some ty_expr -> typecheck_type_expr ctx env ty_expr)
	    binding.Parsetree.b_params in
	if non_expansive then Types.end_definition () ;
	(* Extend the current environment with the arguments *)
	(* of the bound identier if there are some.          *)
	let local_env =
	  List.fold_left2
	    (fun accu_env (arg_name, _) arg_ty ->
	      Env.add_ident arg_name (Types.trivial_scheme arg_ty) accu_env)
	    env'
	    binding.Parsetree.b_params
	    args_tys in
        (* Same hack thant above for the variables *)
        (* that must not be generalised.           *)
        if non_expansive then Types.begin_definition () ;
	(* Guess the body's type. *)
	let infered_body_ty =
	  typecheck_expr ctx local_env binding.Parsetree.b_body in
	(* If there is some constraint on this type, then unify with it. *)
	(match binding.Parsetree.b_type with
	 | None -> ()
	 | Some ty_expr ->
	     let constraint_ty = typecheck_type_expr ctx env ty_expr in
	     Types.unify
	       ~self_manifest: ctx.self_manifest
	       constraint_ty infered_body_ty) ;
        if non_expansive then Types.end_definition () ;
	(* Now, reconstruct the functional type from the body's and args' *)
        (* types. DO NOT fold_left, otherwise the fun type gets mirored ! *)
        let complete_ty =
	  List.fold_right
	    (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
	    args_tys
	    infered_body_ty in
        (* Unify the found type with the type that was temporarily assumed. *)
        Types.begin_definition () ;
        Types.unify ~self_manifest: ctx.self_manifest assumed_ty complete_ty ;
        Types.end_definition () ;
	(* And finally returns the type binding induced by this definition. *)
	let ty_scheme =
          (if non_expansive then Types.generalize complete_ty
          else Types.trivial_scheme complete_ty) in
        (binding.Parsetree.b_name, ty_scheme))
      let_def_descr.Parsetree.ld_bindings
      pre_env_info in
  (* Finally, returns the induced bindings. Note that [Parsetree.binding] *)
  (* and [Parsetree.let_def have an [ast_type] but in this case it has no *)
  (* relevance, so we just leave them [None].                             *)
  env_bindings
;;



(*
type coll_def = coll_def_desc ast_doc
and coll_def_desc = {
  cd_name : Types.cname ;
  cd_body : species_expr
} ;;
*)



let typecheck_type_def ctx env type_def =
  let type_def_descr = type_def.Parsetree.ast_desc in
  (* First, extend the [tyvars_mapping] of the current *)
  (* context with parameters of the type definition.   *)
  let vmapp_extention =
    List.map
      (fun var_name -> (var_name, Types.type_variable ()))
      type_def_descr.Parsetree.td_params in
  let new_ctx = { ctx with
    tyvars_mapping = vmapp_extention @ ctx.tyvars_mapping } in
  (* Get the type constructor's arity. One could avoid a second iteration *)
  (* on the list by incrementing a reference while building the extention *)
  (* of the context, but that would be pretty uggly... And usually, there *)
  (* are no tons of parameters in types definitions !                     *)
  let nb_params = List.length type_def_descr.Parsetree.td_params in
  (* Process the body of the type definition. *)
  match type_def_descr.Parsetree.td_body.Parsetree.ast_desc with
   | Parsetree.TD_alias ty ->
       (* We do not insert the defined name itself  *)
       (* to reject recursive type abbreviations.   *)
       let identity_type = typecheck_type_expr new_ctx env ty in
       (* Generalize the scheme to get the real identity. *)
       let ty_descr = {
	 Env.type_kind = Env.TK_abstract ;
	 Env.type_identity = Types.generalize identity_type ;
	 Env.type_arity = nb_params } in
       (* Just returns the environment extended by the type itself. *)
       Env.add_type type_def_descr.Parsetree.td_name ty_descr env
   | Parsetree.TD_union constructors ->
       failwith "todo 15"
   | Parsetree.TD_record labels ->
       (* We do not insert the defined record name *)
       (* itself to reject recursive record types. *)
       failwith "todo 15"
;;



(* ****************************************************************** *)
(* typecheck_phrase : Env.t -> Parsetree.phrase -> Env.t              *)
(** {b Descr} : Performs type inference on a [phrase] and returns the
                initial environment extended with the possible type
		bindings induced by the [phrase].

    {b Rem} : Not exported outside this module                        *)
(* ****************************************************************** *)
let typecheck_phrase env phrase =
  (* A phrase is always typed in an empty context. *)
  let ctx = { self_manifest = None ; tyvars_mapping = [] } in
  let (final_ty, new_env) =
    (match phrase.Parsetree.ast_desc with
     | Parsetree.Ph_external exter_def -> failwith "todo2"
     | Parsetree.Ph_use _ -> failwith "todo3"
     | Parsetree.Ph_open _ -> failwith "todo4"
     | Parsetree.Ph_species species_def -> failwith "todo5"
     | Parsetree.Ph_coll coll_def -> failwith "todo6"
     | Parsetree.Ph_type type_def ->
	 let env' = typecheck_type_def ctx env type_def in
	 Format.fprintf Format.err_formatter "Type defined.@\n" ;
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_let let_def  ->
	 let envt_bindings = typecheck_let_definition ctx env let_def in
	 (* Extend the current environment with the *)
	 (* bindings induced the let-definition.    *)
	 let env' =
	   List.fold_left
	     (fun accu_env (id, ty_scheme) ->
	       (* Just a bit of debug. *)
	       Format.fprintf Format.err_formatter "%s : %a@\n"
		 (Parsetree_utils.string_of_vname id)
		 Types.pp_types_scheme ty_scheme ;
	       Env.add_ident id ty_scheme accu_env)
	 env envt_bindings in
	 (* Return unit and the extended environment. *)
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_theorem theorem_def  -> failwith "todo"
     | Parsetree.Ph_expr expr ->
	 let expr_ty = typecheck_expr ctx env expr in
	 (* Just a bit of debug. *)
	 Format.fprintf Format.err_formatter "- : %a@\n"
	   Types.pp_simple_type expr_ty ;
	 (expr_ty, env)) in
  (* Store the type information in the phrase's node. *)
  phrase.Parsetree.ast_type <- Some final_ty ;
  (* Return the environment extended with the bindings induced by the phrase. *)
  new_env
;;



let typecheck_file file =
  match file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
       let global_env = ref (Env.pervasives ()) in
       List.iter
	 (fun phrase -> global_env := typecheck_phrase !global_env phrase)
	 phrases
;;
