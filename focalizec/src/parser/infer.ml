(* $Id: infer.ml,v 1.2 2007-07-13 15:16:38 pessaux Exp $ *)
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

(** [Descr] : Exception used to inform that a sum type constructor was used
              with an incorrect arity. The correct expected arity is
              stored in the second argument of the exception constructor.   *)
exception Bad_constructor_arity of (Parsetree.ident * Env.constructor_arity) ;;


(*
type vname =
   | Vlident of string  (* Lowercase ident. *)
   | Vuident of string  (* Capitalized ident. *)
   | Vpident of string  (* Prefix operator ident. *)
   | Viident of string  (* Infix operator ident. *)
   | Vqident of string
     (** Variable names are classified with respect of their lexical class,
       which can be regular. infix or prefix. *);;

type ident = ident_desc ast
and ident_desc =
  | I_local of vname
  | I_global of fname option * vname
  | I_method of Types.cname option * vname (* If vname is self, then the real *)
	                             (* name should be considered as only     *)
                                     (* [cname]. If [cname] is None and       *)
                                     (* [vname] is self, then it's bugged !   *)
;;

type rep_type_def = rep_type_def_desc ast_doc
and rep_type_def_desc =
  | RTE_ident of ident
  | RTE_fun of rep_type_def * rep_type_def
  | RTE_app of ident * rep_type_def list
  | RTE_prod of rep_type_def * rep_type_def
  | RTE_paren of rep_type_def
;;

type type_expr = type_expr_desc ast
and type_expr_desc =
  | TE_ident of ident
  | TE_fun of type_expr * type_expr
  | TE_app of ident * type_expr list
  | TE_prod of type_expr * type_expr
  | TE_self
  | TE_prop
  | TE_paren of type_expr
;;
*)



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



let rec typecheck_pattern env pat_desc =
  (* First, get the pattern's type and induced bindings. *)
  let (final_ty, bindings) =
    (match pat_desc.Parsetree.ast_desc with
     | Parsetree.P_const cst -> ((typecheck_constant cst), [])
     | Parsetree.P_var var ->
	 let var_ty = Types.type_variable () in
	 (var_ty, [ (var, (Types.trivial_scheme var_ty)) ])
     | Parsetree.P_as (pat, alias) ->
	 let (ty, bindings) = typecheck_pattern env pat in
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
		typecheck_pattern env
		  { pat_desc with
		     Parsetree.ast_desc = Parsetree.P_tuple nempty_pats } in
              (* Constructeurs being functions, we will unify [cstr_type]   *)
	      (* with an arrow type to ensure that it is really one and     *)
	      (* to ensure the arguments types and extract the result type. *)
              let cstr_res_ty = (Types.type_variable ()) in
              Types.unify (Types.type_arrow cstr_arg_ty cstr_res_ty) cstr_ty ;
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
		  let (sub_pat_ty, bnds) = typecheck_pattern env pat in
		  let lbl_desc = Env.find_label lbl env in
		  (* Get the related label type. *)
		  let lbl_ty = Types.specialize lbl_desc.Env.field_scheme in
		  (* Now, ensure the 2 sub-pattners types are compatible and *)
		  (* that the resulting record type for the whole pattern is *)
		  (* consistent.                                             *)
		  Types.unify
		    lbl_ty (Types.type_arrow sub_pat_ty whole_pat_ty) ;
		  (* Just returns the bindings. *)
		  bnds)
		label_n_patterns) in
	 (whole_pat_ty, bindings)
	 end)
     | Parsetree.P_tuple pats ->
         let (tys, bindings) =
	   List.split (List.map (typecheck_pattern env) pats) in
	 let ty = Types.type_tuple tys in
	 (ty, (List.flatten bindings))
     | Parsetree.P_paren pat -> typecheck_pattern env pat) in
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

and external_expr = external_expr_desc ast
and external_expr_desc =
    (external_language * external_expression) list

and external_expression = string ;;
*)

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

and let_def = let_def_desc ast_doc
and let_def_desc = {
  ld_rec : rec_flag ;
  ld_log : log_flag ;
  ld_loc : loc_flag ;
  ld_bindings : binding list
}
and binding = binding_desc ast
and binding_desc = {
  b_name : ident ;
  b_params : (ident * type_expr option) list ;
  b_type : type_expr option ;
  b_body : expr
}

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


(*
let rec typecheck_expr env expr_desc =
  let final_ty =
    (match expr_desc.Parsetree.ast_desc with
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
	 let ty_body = typecheck_expr extended_env e_body in
	 (* Remains to build the final functional type. And do not *)
	 (* fold_left, otherwise you'll get a mirrored type !      *)
	 List.fold_right
	   (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
	   args_ty ty_body
	 end)
(*
     | Parsetree.E_var ident ->
	 (* One must first check whether the ident is "self". *)
	 if ... then Types.type_self ()
	 else ... Dans quel ordre faire la recherche. Est-elle directement induite par l'environnement ?... Ancien ordre: local env, in-param, is-param, inheritance, global.
	   Types.specialize (Env.find_ident ident env)
     | Parsetree.E_app of expr * expr list
*)
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
	      let tys = List.map (typecheck_expr env) exprs in
	      (* Get an instance of the constructor's type scheme. *)
	      let cstr_ty = Types.specialize cstr_decl.Env.cstr_scheme in
	      (* Build the shadow tuple type as the *)
	      (* real argument of the constructor.  *)
	      let cstr_arg_ty = Types.type_tuple tys in
	      (* Get a hand on what will be our final type result... *)
	      let result_ty = Types.type_variable () in
	      (* And simulate an application. *)
	      Types.unify cstr_ty (Types.type_arrow cstr_arg_ty result_ty) ;
	      result_ty
	      end)
       | (_, _) ->
	   raise
	     (Bad_constructor_arity (pseudo_ident, cstr_decl.Env.cstr_arity))
	 end)
(*
     | Parsetree.E_match of expr * (pattern * expr) list
*)
     | Parsetree.E_if (e_cond, e_then, e_else) ->
	 let ty_cond = typecheck_expr env e_cond in
	 (* Ensure the condition is a boolean. *)
	 Types.unify ty_cond (Types.type_bool ()) ;
	 (* Typecheck the "then" expression. *)
	 let ty_then = typecheck_expr env e_then in
	 let ty_else = typecheck_expr env e_else in
	 (* Enforce both branches to have the same type. *)
	 Types.unify ty_then ty_else ;
	 (* And return any of them as result type. *)
	 ty_then
(*
     | Parsetree.E_let of let_def * expr
     | Parsetree.E_record of (Types.label_name * expr) list
     | Parsetree.E_record_access of expr * Types.label_name
     | Parsetree.E_record_with of expr * (Types.label_name * expr) list
*)
     | Parsetree.E_tuple exprs ->
          assert (exprs <> []) ;  (* Just in case. O-ary tuple is non-sense ! *)
          let tys = List.map (typecheck_expr env) exprs in
          Types.type_tuple tys
(*
     | Parsetree.E_external of external_expr
*)
     | Parsetree.E_paren expr -> typecheck_expr env expr) in
  (* Store the type information in the expression's node. *)
  expr_desc.Parsetree.ast_type <- Some final_ty ;
  final_ty
;;
*)


(*
type coll_def = coll_def_desc ast_doc
and coll_def_desc = {
  cd_name : Types.cname ;
  cd_body : species_expr
} ;;

type type_def = type_def_desc ast
and type_def_desc = {
  td_name : Types.tname ;
  td_params : string list ;
  td_body : type_body
}

and type_body = type_body_desc ast
and type_body_desc =
  | TD_alias of type_expr
  | TD_union of (constr_name * (type_expr list)) list 
  | TD_record of (Types.label_name * type_expr) list
;;

(** Toplevel expressions. *)
type expr_def = expr ;;

type phrase = phrase_desc ast
and phrase_desc =
  | Ph_external of external_def
  | Ph_use of fname
  | Ph_open of fname
  | Ph_species of species_def
  | Ph_coll of coll_def
  | Ph_type of type_def
  | Ph_let of let_def
  | Ph_theorem of theorem_def
  | Ph_expr of expr_def
;;

type file = file_desc ast_doc
and file_desc =
  | File of phrase list
;;
*)
