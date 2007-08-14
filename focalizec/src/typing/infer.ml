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


(* $Id: infer.ml,v 1.14 2007-08-14 13:20:26 pessaux Exp $ *)

(* *********************************************************************** *)
(** {bL Descr} : Exception used to inform that a sum type constructor was
               used with an incorrect arity. The correct expected arity is
              stored in the second argument of the exception constructor.  *)
exception Bad_sum_type_constructor_arity of
  (Parsetree.ident * Env.TypeInformation.constructor_arity) ;;

(* *********************************************************************** *)


exception Unbound_type_variable of string ;;

(* Method name, species name. *)
exception Method_multiply_defined of (Parsetree.vname * Types.species_name) ;;

(* Expected arity, used with arity. *)
exception Bad_type_arity of (Parsetree.ident * int * int) ;;



(** To become the context recording various information propagated for the
    type inference. May be could also contain the environment. *)
type typing_context = {
  (** The name of the currently analysed compilation unit. *)  
  current_unit : Parsetree.fname ;
  (** The name of the current species if relevant. *)
  current_species : Types.species_name option ;
  (** Optional type Self is known to be equal to. *)
  self_manifest : Types.type_simple option ;
  (** Mapping between 'variables and the [simpletype] they are bound to.
      Used when creating a polymorphic type definition. *)
  tyvars_mapping : (string * Types.type_simple) list
} ;;



(* ******************************************************************* *)
(* typing_context -> Env.Env.ScopeInformation.t ->                     *)
(*   Parsetree.type_expr -> Types.type_simple                          *)
(** {b Descr} : Translates a type expression into a [type_simple].
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
         (match ident.Parsetree.ast_desc with
	  | Parsetree.I_local (Parsetree.Vqident quote_name) ->
	      (begin
	      (* Just handle the special where the ident is a type variable. *)
	      try List.assoc quote_name ctx.tyvars_mapping
	      with Not_found -> raise (Unbound_type_variable quote_name)
	      end)
	  | _ ->
	      (* Case of all 0-ary other user-defined type constructors. *)
	      let ty_descr =
		Env.TypingEnv.find_type
		  ~current_unit: ctx.current_unit ident env in
	      if ty_descr.Env.TypeInformation.type_arity <> 0 then
		raise
		  (Bad_type_arity
		     (ident, ty_descr.Env.TypeInformation.type_arity, 0))
	      else Types.specialize ty_descr.Env.TypeInformation.type_identity)
     | Parsetree.TE_fun (ty_expr1, ty_expr2) ->
	 Types.type_arrow
	   (typecheck_type_expr ctx env ty_expr1)
	   (typecheck_type_expr ctx env ty_expr2)
     | Parsetree.TE_app (ty_cstr_ident, args_ty_exprs) ->
	 (begin
	 let ty_descr =
	   Env.TypingEnv.find_type
	     ~current_unit: ctx.current_unit ty_cstr_ident env in
	 (* Check the type constructor's arity. *)
	 let args_ty_len = List.length args_ty_exprs in
	 if args_ty_len <> ty_descr.Env.TypeInformation.type_arity then
	   raise
	     (Bad_type_arity
		(ty_cstr_ident, ty_descr.Env.TypeInformation.type_arity,
		 args_ty_len)) ;
	 (* Synthetise the types for the arguments. *)
	 let args_ty = List.map (typecheck_type_expr ctx env) args_ty_exprs in
	 (* Now get a fresh instance of the type's type scheme, directly  *)
	 (* mapping its gener&lised variables onto the arguments. This    *)
	 (* saves the need to know which variables have been instanciated *)
	 (* in order to unify them afterwards with the corresponding type *)
	 (* of the corresponding argument.                                *)
	 Types.instanciate_parameters
	   ty_descr.Env.TypeInformation.type_identity args_ty
	 end)
     | Parsetree.TE_prod (ty_exprs) ->
	 let tys = List.map (typecheck_type_expr ctx env) ty_exprs in
	 Types.type_tuple tys
     | Parsetree.TE_self -> Types.type_self ()
     | Parsetree.TE_prop -> Types.type_prop ()
     | Parsetree.TE_paren inner -> typecheck_type_expr ctx env inner) in
  (* Store the type information in the expression's node. *)
  ty_expr.Parsetree.ast_type <- Some final_ty;
  final_ty
;;



(* ******************************************************************* *)
(* typing_context -> Env.Env.ScopeInformation.t ->                     *)
(*   Parsetree.rep_type_def -> Types.type_simple                       *)
(** {b Descr} : Translates a rep type expression into a [type_simple].
                Variable are translated according to the mapping found
                inside the current context. Hence, in case this
                function is used to create the body type of a type
                definition, if the definition is polymorphic, the
                parameter-variables must already exist inside  the
                mapping.

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let rec typecheck_rep_type_def ctx env rep_type_def =
  let final_ty =
    (match rep_type_def.Parsetree.ast_desc with
     | Parsetree.RTE_ident ident ->
         (match ident.Parsetree.ast_desc with
	  | Parsetree.I_local (Parsetree.Vqident quote_name) ->
	      (begin
	      (* Just handle the special where the ident is a type variable. *)
	      try List.assoc quote_name ctx.tyvars_mapping
	      with Not_found -> raise (Unbound_type_variable quote_name)
	      end)
	  | _ ->
	      (* Case of all 0-ary other user-defined type constructors. *)
	      let ty_descr =
		Env.TypingEnv.find_type
		  ~current_unit: ctx.current_unit ident env in
	      if ty_descr.Env.TypeInformation.type_arity <> 0 then
		raise
		  (Bad_type_arity
		     (ident, ty_descr.Env.TypeInformation.type_arity, 0))
	      else Types.specialize ty_descr.Env.TypeInformation.type_identity)
     | Parsetree.RTE_fun (ty_expr1, ty_expr2) ->
	 Types.type_arrow
	   (typecheck_rep_type_def ctx env ty_expr1)
	   (typecheck_rep_type_def ctx env ty_expr2)
     | Parsetree.RTE_app (ty_cstr_ident, args_ty_exprs) ->
	 (begin
	 let ty_descr =
	   Env.TypingEnv.find_type
	     ~current_unit: ctx.current_unit ty_cstr_ident env in
	 (* Check the type constructor's arity. *)
	 let args_ty_len = List.length args_ty_exprs in
	 if args_ty_len <> ty_descr.Env.TypeInformation.type_arity then
	   raise
	     (Bad_type_arity
		(ty_cstr_ident, ty_descr.Env.TypeInformation.type_arity,
		 args_ty_len)) ;
	 (* Synthetise the types for the arguments. *)
	 let args_ty =
	   List.map (typecheck_rep_type_def ctx env) args_ty_exprs in
	 (* Now get a fresh instance of the type's type scheme, directly  *)
	 (* mapping its gener&lised variables onto the arguments. This    *)
	 (* saves the need to know which variables have been instanciated *)
	 (* in order to unify them afterwards with the corresponding type *)
	 (* of the corresponding argument.                                *)
	 Types.instanciate_parameters
	   ty_descr.Env.TypeInformation.type_identity args_ty
	 end)
     | Parsetree.RTE_prod (ty_exprs) ->
	 let tys = List.map (typecheck_rep_type_def ctx env) ty_exprs in
	 Types.type_tuple tys
     | Parsetree.RTE_paren inner -> typecheck_rep_type_def ctx env inner) in
  (* Store the type information in the expression's node. *)
  rep_type_def.Parsetree.ast_type <- Some final_ty;
  final_ty
;;



 (* If returns true then one can generalise. *)
let rec is_non_expansive env expr =
  match expr.Parsetree.ast_desc with
  | Parsetree.E_const _ -> true
  | Parsetree.E_var _ -> true
  | Parsetree.E_let (let_def, body) ->
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
  | Parsetree.E_fun _ -> true
  | Parsetree.E_tuple exprs -> List.for_all (is_non_expansive env) exprs
  | Parsetree.E_record lbl_exp_list ->
      List.for_all
        (fun (lbl, e) ->
          let lbl_descr = Env.TypingEnv.find_label lbl env in
          (lbl_descr.Env.TypeInformation.field_mut =
	    Env.TypeInformation.FM_immutable) && (is_non_expansive env e))
        lbl_exp_list
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
	 (var_ty, [ (var, (Types.closed_scheme var_ty)) ])
     | Parsetree.P_as (pat, alias) ->
	 let (ty, bindings) = typecheck_pattern ctx env pat in
	 (ty, ((alias, (Types.closed_scheme ty)) :: bindings))
     | Parsetree.P_wild ->
	 let var_ty = Types.type_variable () in
	 (var_ty, [])
     | Parsetree.P_app (cstr_name, pats) ->
	 (* Find a specialization of the constructor's type scheme. *)
	 let cstr_decl =
	   Env.TypingEnv.find_constructor
	     ~current_unit: ctx.current_unit cstr_name env in
	 (match (pats, cstr_decl.Env.TypeInformation.cstr_arity) with
	  | ([], Env.TypeInformation.CA_zero) ->
	      let cstr_ty =
		Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
	      (cstr_ty, [])
	  | (nempty_pats, Env.TypeInformation.CA_one) ->
	      let cstr_ty =
		Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
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
		(Bad_sum_type_constructor_arity
		   (cstr_name, cstr_decl.Env.TypeInformation.cstr_arity)))
     | Parsetree.P_record label_n_patterns ->
	 (* Type for this pattern. Will be instanciated by side effect. *)
	 let whole_pat_ty = Types.type_variable () in
	 (* Infer type of eack sub-pattern. *)
         let bindings =
	   List.flatten
             (List.map
		(fun (lbl, pat) ->
		  (* Get bindings and sub-pattern type. *)
		  let (sub_pat_ty, bnds) = typecheck_pattern ctx env pat in
		  let lbl_desc = Env.TypingEnv.find_label lbl env in
		  (* Get the related label type. *)
		  let lbl_ty =
		    Types.specialize
		      lbl_desc.Env.TypeInformation.field_scheme in
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



(* *********************************************************************** *)
(* Env.TypingEnv.t -> Parsetree.external_def -> Env.TypingEnv.t            *)
(** {b Desc} : Synthetise the type of an external definition. In fact,
             because such definitions do not contain any type information,
             the only solution is to make a stupid type variable...
             If the definition is a value definition, then the binding
             get added to the [ident]s environment.
             If the definition is a type definition, then the binding
             gets added to the [type]'s environment.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let typecheck_external_def env e_def =
  (* Anyway, just make a fresh type variable because  *)
  (* we don't have any type information here.         *)
  Types.begin_definition () ;
  let ty = Types.type_variable () in
  Types.end_definition () ;
  match e_def.Parsetree.ast_desc with
  | Parsetree.ED_type body ->
      (* The type definition will be considered as fully polymorphic *)
      (* because we generalize it. It's a kind of forall 'a.'a.      *)
      let ty_descr = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
        Env.TypeInformation.type_identity = Types.generalize ty ;
        Env.TypeInformation.type_arity = 0 } in
      let name_as_str =
        Parsetree_utils.name_of_vname
          body.Parsetree.ast_desc.Parsetree.ed_name in
      Env.TypingEnv.add_type name_as_str ty_descr env
  | Parsetree.ED_value body ->
      (* Record the type inside the node. I think it's useless, but... *)
      body.Parsetree.ast_type <- Some ty ;
      let scheme = Types.generalize ty in
      Env.TypingEnv.add_value
	body.Parsetree.ast_desc.Parsetree.ed_name scheme env
;;



(* Does not make any assumption. Crudely returns a fresh type variable. *)
let typecheck_external_expr ext_expr =
  let ty = Types.type_variable () in (* A somewhat of magic obj... *)
  ext_expr.Parsetree.ast_type <- Some ty;
  ty
;;



(* **************************************************************** *)
(* Parsetree.vname -> ('a * 'b * 'c) list -> ('a * 'd * 'e) list -> *)
(*   unit                                                           *)
(* {Descr} : Checks if the 2 lists of methods names overlaps.
           If so then raises en exception [Method_multiply_defined],
           else silently returns.

   {Rem} : Not exported outside this module.                        *)
(* **************************************************************** *)
let ensure_methods_uniquely_defined current_species l1 l2 =
  List.iter
    (fun (name1, _, _) ->
      List.iter
	(fun (name2, _, _) ->
	  if name1 = name2 then
	    raise (Method_multiply_defined (name1, current_species)))
	l2)
    l1
;;



(* ************************************************************************ *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.expr -> Types.type_simple *)
(** {b Descr} : Infers the type o an [expr] and assign it by side effect in
              the [ast_type] field of the [expr] node.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let rec typecheck_expr ctx env initial_expr =
  (let final_ty =
    (match initial_expr.Parsetree.ast_desc with
     | Parsetree.E_self -> Types.type_self ()
     | Parsetree.E_const cst -> typecheck_constant cst
     | Parsetree.E_fun (arg_vnames, e_body) ->
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
	       Env.TypingEnv.add_value
		 arg_name (Types.closed_scheme arg_ty) accu_env)
	     env arg_vnames args_ty in
	 (* Now, typecheck the body i nthe new environment. *)
	 let ty_body = typecheck_expr ctx extended_env e_body in
	 (* Remains to build the final functional type. And do not *)
	 (* fold_left, otherwise you'll get a mirrored type !      *)
	 List.fold_right
	   (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
	   args_ty ty_body
     | Parsetree.E_var ident ->
	 (* E_var is never "self" because "self" is a dedicated case. *)
	 (* Now, don't bother with the search order, this has already *)
	 (* be done by both the scoping and the environment build     *)
	 (* process. As reminder, lookup will naturally find the      *)
         (* ident among local identifiers, in-params, is-params,      *)
         (* inheritance and finally global identifiers.               *)
	 let var_scheme =
	   Env.TypingEnv.find_value ~current_unit: ctx.current_unit ident env in
         Types.specialize var_scheme
     | Parsetree.E_app (functional_expr, args_exprs) ->
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
	       ~self_manifest: ctx.self_manifest tmp_fun_ty accu_fun_ty;
	     (* The result is the positive part of the arrow. *)
	     result_ty)
           fun_ty
	   ty_exprs
     | Parsetree.E_constr (cstr_expr, exprs) ->
	 (* Because the environment maps [idents] onto type schemes and
            because the constructor's name is not a "full" ident, we
            just wrap the constructor's name into a global [ident]
            to be able to lookup inside the environment. *)
	 let pseudo_ident =
           let Parsetree.CE (fname_opt, vname) = cstr_expr.Parsetree.ast_desc in
	   {
            Parsetree.ast_loc = cstr_expr.Parsetree.ast_loc;
            Parsetree.ast_desc =
              Parsetree.I_global (fname_opt, vname);
	    Parsetree.ast_doc = cstr_expr.Parsetree.ast_doc;
	    Parsetree.ast_type = None;
           } in
	 let cstr_decl =
	   Env.TypingEnv.find_constructor
	     ~current_unit: ctx.current_unit pseudo_ident env in
	 (match (exprs, cstr_decl.Env.TypeInformation.cstr_arity) with
	  | ([], Env.TypeInformation.CA_zero) ->
	      (* Just get an instance of the constructor's type scheme. *)
	      Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme
          | (_, Env.TypeInformation.CA_one) ->
	      (* The constructor must be viewed as a function. *)
	      let tys = List.map (typecheck_expr ctx env) exprs in
	      (* Get an instance of the constructor's type scheme. *)
	      let cstr_ty =
		Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
	      (* Build the shadow tuple type as the *)
	      (* real argument of the constructor.  *)
	      let cstr_arg_ty = Types.type_tuple tys in
	      (* Get a hand on what will be our final type result... *)
	      let result_ty = Types.type_variable () in
	      (* And simulate an application. *)
	      Types.unify
		~self_manifest: ctx.self_manifest
		cstr_ty (Types.type_arrow cstr_arg_ty result_ty);
	      result_ty
       | (_, _) ->
	   raise
	     (Bad_sum_type_constructor_arity
		(pseudo_ident, cstr_decl.Env.TypeInformation.cstr_arity)))
     | Parsetree.E_match (matched_expr, bindings) ->
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
	       ~self_manifest: ctx.self_manifest matched_expr_ty pat_ty;
	     (* Extend the environment with the pattern type bindings. *)
	     let env' =
	       List.fold_left
		 (fun accu_env (id, ty_scheme) ->
		   Env.TypingEnv.add_value id ty_scheme accu_env)
		 env bnds in
	     (* Infer the type of the match clause's body. *)
	     let clause_ty = typecheck_expr ctx env' expr in
	     (* Force every bodies to have the same result type. *)
	     Types.unify
	       ~self_manifest: ctx.self_manifest result_ty clause_ty)
	   bindings;
	 (* Return the type of the bodies' clauses. *)
	 result_ty
     | Parsetree.E_if (e_cond, e_then, e_else) ->
	 let ty_cond = typecheck_expr ctx env e_cond in
	 (* Ensure the condition is a boolean. *)
	 Types.unify
	   ~self_manifest: ctx.self_manifest ty_cond (Types.type_bool ());
	 (* Typecheck the "then" expression. *)
	 let ty_then = typecheck_expr ctx env e_then in
	 let ty_else = typecheck_expr ctx env e_else in
	 (* Enforce both branches to have the same type. *)
	 Types.unify ~self_manifest: ctx.self_manifest ty_then ty_else;
	 (* And return any of them as result type. *)
	 ty_then
     | Parsetree.E_let (let_def, in_expr) ->
	 (* Don't increase level, this will be done in the let inference. *)
	 let bindings = typecheck_let_definition ctx env let_def in
	 (* Let's build the environment for typing the ending expression. *)
	 let env' =
	   List.fold_left
	     (fun accu_env (id, ty_scheme) ->
	       Env.TypingEnv.add_value id ty_scheme accu_env)
             env bindings in
	 typecheck_expr ctx env' in_expr
     | Parsetree.E_record fields -> typeckeck_record_expr ctx env fields None
     | Parsetree.E_record_access (expr, label) ->
	 let ty_expr = typecheck_expr ctx env expr in
	 let label_desc = Env.TypingEnv.find_label label env in
	 (* Just remind that labels are types as functions of type     *)
	 (* "type of the field as seen by user -> type od the record". *)
	 let label_ty =
	   Types.specialize label_desc.Env.TypeInformation.field_scheme in
	 (* Get a holder to extract the "arg type of the function", *)
	 (* i.e. the type of the field as seen by the user.         *)
	 let result_ty = Types.type_variable () in
	 Types.unify
	   ~self_manifest: ctx.self_manifest
	   (Types.type_arrow result_ty ty_expr) label_ty;
	 result_ty
     | Parsetree.E_record_with (with_expr, fields) ->
         typeckeck_record_expr ctx env fields (Some with_expr)
     | Parsetree.E_tuple exprs ->
          assert (exprs <> []);  (* Just in case. O-ary tuple is non-sense ! *)
          let tys = List.map (typecheck_expr ctx env) exprs in
          Types.type_tuple tys
     | Parsetree.E_external ext_expr -> typecheck_external_expr ext_expr
     | Parsetree.E_paren expr -> typecheck_expr ctx env expr) in
  (* Store the type information in the expression's node. *)
  initial_expr.Parsetree.ast_type <- Some final_ty;
  final_ty)



(* ****************************************************************** *)
(* Env.TypingEnv.t -> (Types.label_name * Parsetree.expr) list ->     *)
(*   Parsetree.expr option -> Types.type_simple                       *)
(** {b Descr} : Performs type inference on record expressions with or
              without "with" clause. Currently, the labels
              exhaustivity is not checked. It has to be done when
              there is no "with" clause.

    {b Args} :
      - env : The current typing environment.
      - fields : The list of fields values of the record expression.
      - opt_with_expr : The optional "with" clause.

    {b Rem} : Not exported outside this module.                      *)
(* ***************************************************************** *)
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
       Types.unify ~self_manifest: ctx.self_manifest expr_ty result_ty);
  (* Now proceed with the labels.                               *)
  (* Just remind that labels are types as functions of type     *)
  (* "type of the field as seen by user -> type od the record". *)
  List.iter
    (fun (label, expr) ->
      let expr_ty = typecheck_expr ctx env expr in
      let lbl_descr = Env.TypingEnv.find_label label env in
      (* Get the functionnal type of this field. *)
      let field_ty =
	Types.specialize lbl_descr.Env.TypeInformation.field_scheme in
      (* Unify the result type by side effect. *)
      Types.unify
	~self_manifest: ctx.self_manifest
	(Types.type_arrow expr_ty result_ty) field_ty)
    fields ;
  result_ty



(* ************************************************************************ *)
(* typing_context ->                                                        *)
(*  Env.TypingEnv.t -> Parsetree.let_def ->                                 *)
(*   (Parsetree.vname * Types.type_scheme) list                             *)
(** {b Descr} : Infers the list of bindings induced by the let-def and that
                will extend the current typing environment.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
and typecheck_let_definition ctx env let_def =
  let let_def_descr = let_def.Parsetree.ast_desc in
  (* Get information to possibly build the pre-environment, *)
  (* i.e. the induced environment bindings between idents   *)
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
	  let scheme = Types.closed_scheme ty in
	  Env.TypingEnv.add_value vname scheme accu_env)
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
	(* of the bound identifier if there are some.        *)
	let local_env =
	  List.fold_left2
	    (fun accu_env (arg_name, _) arg_ty ->
	      Env.TypingEnv.add_value
		arg_name (Types.closed_scheme arg_ty) accu_env)
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
          else Types.closed_scheme complete_ty) in
        (binding.Parsetree.b_name, ty_scheme))
      let_def_descr.Parsetree.ld_bindings
      pre_env_info in
  (* Finally, returns the induced bindings. Note that [Parsetree.binding] *)
  (* and [Parsetree.let_def have an [ast_type] but in this case it has no *)
  (* relevance, so we just leave them [None].                             *)
  env_bindings



(* ************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.prop ->        *)
(*   Types.type_simple                                           *)
(** {b Descr} : Infers the type of a [prop]. This type is always
              expected to be [Prop], hence this inference moslty
              verifies the right types usages inside a property
              and ensures that the final type is really [Prop].
              It finally assign the type by side effect in the
              [ast_type] field of the [prop] node.

    {b Rem} : Not exported outside this module.                  *)
(* ************************************************************* *)
and typecheck_prop ctx env prop =
  let final_ty =
    (match prop.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, t_expr, pr)
     | Parsetree.Pr_exists (vnames, t_expr, pr) ->
         (Types.begin_definition ();
	 (* Get the couple (name, type) for each defined variable. *)
	 let bound_variables =
	   (let ty = typecheck_type_expr ctx env t_expr in
	   List.map (fun vname -> (vname, ty)) vnames) in
	 (* Now typecheck the theorem's body in the extended environment.  *)
	 (* Note that as often, th order bindings are inserted in the      *)
	 (* environment does not matter since parameters can never depends *)
	 (* on each other.                                                 *)
	 let env' =
	   List.fold_left
	     (fun accu_env (th_name, th_type) ->
	       let scheme = Types.generalize th_type in
	       Env.TypingEnv.add_value th_name scheme accu_env)
	     env bound_variables in
	 typecheck_prop ctx env' pr)
     | Parsetree.Pr_imply (pr1, pr2)
     | Parsetree.Pr_or (pr1, pr2)
     | Parsetree.Pr_and (pr1, pr2)
     | Parsetree.Pr_equiv (pr1, pr2) ->
	 let ty1 = typecheck_prop ctx env pr1 in
	 let ty2 = typecheck_prop ctx env pr2 in
	 Types.unify ~self_manifest: ctx.self_manifest ty1 ty2;
	 (* Enforce the type to be [prop]. *)
	 Types.unify
	   ~self_manifest: ctx.self_manifest ty1 (Types.type_prop ());
	 ty1
     | Parsetree.Pr_not pr ->
         let ty = typecheck_prop ctx env pr in
	 (* Enforce the type to be [prop]. *)
	 Types.unify ~self_manifest: ctx.self_manifest ty (Types.type_prop ());
         ty
     | Parsetree.Pr_expr expr ->
	 (* Expressions must be typed as [bool]. If *)
         (* so, then the returned  type is [prop].  *)
	 let ty = typecheck_expr ctx env expr in
         Types.unify ~self_manifest: ctx.self_manifest ty (Types.type_bool ());
         Types.type_prop ()
     | Parsetree.Pr_paren pr -> typecheck_prop ctx env pr) in
  prop.Parsetree.ast_type <- Some final_ty;
  final_ty



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.proof -> unit              *)
(** {b Descr} : Typechecks a [proof]. Because the type of a proof is not
              relevant in FoCaL, this function noes not returns any type.
	      Its only goal is to verify the type of the AST-sub-expressions
	      where it is relevant and to screw this type in the [ast_type]
              field of these AST-nodes (i.e especially throug [statement]s.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
and typecheck_proof ctx env proof =
  (* No type information inserted in the AST node because not relevant. *)
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed -> ()
   | Parsetree.Pf_auto facts -> ()
   | Parsetree.Pf_coq _ -> ()
   | Parsetree.Pf_node nodes ->
       List.iter (typecheck_node ctx env) nodes



and typecheck_node ctx env node =
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub (_, statement, _) ->
       typecheck_statement ctx env statement
   | Parsetree.PN_qed (_, proof) -> typecheck_proof ctx env proof



(* ******************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.statement -> unit     *)
(** {b Descr} : Typechecks a [statement]. Hypotheses are entered in the
              current environment before typechecking the optional
              [s_concl] proposition.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
and typecheck_statement ctx env statement =
  (begin
  let env' =
    List.fold_left
      (fun accu_env hyp ->
	let (name, ty) =
	  (match hyp.Parsetree.ast_desc with
	   | Parsetree.H_var (vname, type_expr) ->
	       (vname, (typecheck_type_expr ctx accu_env type_expr))
	   | Parsetree.H_hyp (vname, prop) ->
	       (vname, (typecheck_prop ctx accu_env prop))
	   | Parsetree.H_not (vname, expr) ->
	       (vname, (typecheck_expr ctx accu_env expr))) in
	(* Record the type information in the AST node. *)
	hyp.Parsetree.ast_type <- Some ty ;
	(* Extend the environment for the next hypotheses and finally *)
	(* to build the complete environment that will be used to     *)
	(* typecheck the conclusion of the statement.                 *)
	let scheme = Types.generalize ty in
	Env.TypingEnv.add_value name scheme accu_env)
      env
      statement.Parsetree.ast_desc.Parsetree.s_hyps in
  (* Now, typecheck the conclusion, if some, in the extended environment. *)
  match statement.Parsetree.ast_desc.Parsetree.s_concl with
   | None -> ()
   | Some prop -> ignore (typecheck_prop ctx env' prop)
  end)



and typecheck_theorem_def ctx env theorem_def =
  let ty =
    typecheck_prop
      ctx env theorem_def.Parsetree.ast_desc.Parsetree.th_stmt in
  (* Record the type information in the AST node. *)
  theorem_def.Parsetree.ast_type <- Some ty ;
  (* Now, typecheck the proof to fix types inside by side effet. *)
  typecheck_proof ctx env theorem_def.Parsetree.ast_desc.Parsetree.th_proof ;
  (* And return the type pf the stamement as type of the theorem.*)
  ty



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_field ->      *)
(*   (Parsetree.vname * Types.type_scheme * Parsetree.expr option) list *)
(** {b Descr} : Infers the types of the species fields contained in the
              list. The typing environment is incrementally extended
              with the found methods and used to typecheck the next
              methods.
              The function returns a triplet suitable to be inserted in
              the structure of a species's type.

    {b Return} : A list of triplets, one for each method found with
      - Parsetree.vname : The name of the method.
      - Types.type_scheme : The type scheme of the method.
      - The body of the method is this method is "defined" (except for "rep"
        that never has body even if it is really defined).

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
and typecheck_species_fields ctx env = function
  | [] -> []
  | field :: rem_fields ->
      let (fields_tys, new_ctx, new_env) =
	(begin
	match field.Parsetree.ast_desc with
	 | Parsetree.SF_rep rep_type_def ->
	     (begin
	     let rep_vname = Parsetree.Vlident "rep" in
	     (* On must not defined several rep inside a species. *)
	     if ctx.self_manifest <> None then
	       (begin
	       let current_species =
		 (match ctx.current_species with
		  | None ->assert false
		  | Some n -> n) in
	       raise (Method_multiply_defined (rep_vname, current_species))
	       end) ;
	     let ty = typecheck_rep_type_def ctx env rep_type_def in
	     let ctx' = { ctx with self_manifest = Some ty } in
	     (* Record the type information in the AST node. *)
	     field.Parsetree.ast_type <- Some ty ;
	     let field_info = (rep_vname, (Types.generalize ty), None) in
	     ([field_info], ctx', env)
	     end)
	 | Parsetree.SF_sig sig_def ->
	     (begin
	     let sig_def_descr = sig_def.Parsetree.ast_desc in
	     let ty =
	       typecheck_type_expr ctx env sig_def_descr.Parsetree.sig_type in
	     (* Record the type information in the AST nodes. *)
	     sig_def.Parsetree.ast_type <- Some ty ;
	     field.Parsetree.ast_type <- Some ty ;
	     (* Extend the environment with this new method of Self. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 sig_def_descr.Parsetree.sig_name scheme env in
	     let field_info =
	       (sig_def_descr.Parsetree.sig_name, scheme, None) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_let let_def ->
	     (begin
	     (* Don't increase level, this will be done in the let inference. *)
	     let bindings = typecheck_let_definition ctx env let_def in
	     (* Let's build the environment with the bindings for this let. *)
	     let env' =
	       List.fold_left
		 (fun accu_env (id, ty_scheme) ->
		   Env.TypingEnv.add_value id ty_scheme accu_env)
		 env bindings in
	     (* We now collect the type information of these methods   *)
	     (* in order to make them suitable for a "type of method". *)
	     let field_infos =
	       List.map2
		 (fun (id, ty_scheme) binding ->
		   let expr = binding.Parsetree.ast_desc.Parsetree.b_body in
		   (id, ty_scheme, (Some expr)))
		 bindings
		 let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
	     (field_infos, ctx, env')
	     end)
	 | Parsetree.SF_property property_def ->
	     (begin
	     let ty =
	       typecheck_prop
		 ctx env property_def.Parsetree.ast_desc.Parsetree.prd_prop in
	     (* Record the type information in the AST node. *)
	     property_def.Parsetree.ast_type <- Some ty ;
	     (* Extend the environment. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 property_def.Parsetree.ast_desc.Parsetree.prd_name
		 scheme env in
	     let field_info =
	       (property_def.Parsetree.ast_desc.Parsetree.prd_name,
		scheme, None) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_theorem theorem_def ->
	     (begin
	     let ty = typecheck_theorem_def ctx env theorem_def in
	     (* Extend the environment. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 theorem_def.Parsetree.ast_desc.Parsetree.th_name scheme env in
	     let field_info =
	       (theorem_def.Parsetree.ast_desc.Parsetree.th_name,
		scheme, None) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_proof proof_def ->
	     (begin
	     failwith "T9"
	     end)
	end) in
      let rem_fields_tys =
	typecheck_species_fields new_ctx new_env rem_fields in
      (* Make sure that method names are not *)
      (* bound several times in the species. *)
      let current_species =
	(match ctx.current_species with
	 | None ->assert false
	 | Some n -> n) in
      ensure_methods_uniquely_defined
	current_species fields_tys rem_fields_tys ;
      fields_tys @ rem_fields_tys
;;



(** Also performs the interface printing stuff of a species. *)
let typecheck_species_def ctx env species_def =
  let species_def_desc = species_def.Parsetree.ast_desc in
  (* First of all, we are in a species !!! *)
  let ctx = { ctx with
    current_species = Some species_def_desc.Parsetree.sd_name } in
  (* Ignore params and inherit for the moment. *)
  if species_def_desc.Parsetree.sd_params <> [] then failwith "TODO Params." ;
  if species_def_desc.Parsetree.sd_inherits.Parsetree.ast_desc <> [] then
    failwith "TODO inherit" ;
  (* Infer the types of the current field's.*)
  let methods_info =
    typecheck_species_fields ctx env species_def_desc.Parsetree.sd_fields in
  
  (* Then one must ensure that each method has a   *)
  (* same type everywhere in the inheritance tree. *)

  let species_description = {
    Env.TypeInformation.spe_sig_params = [] ;
    Env.TypeInformation.spe_sig_inher = [] ;
    Env.TypeInformation.spe_sig_methods = methods_info } in
  (* Extend the environment with the species. *)
  let env_with_species =
    Env.TypingEnv.add_species
      species_def_desc.Parsetree.sd_name species_description env in
  (* Now, extend the environment with a type that is the spefcies. *)
  let species_as_type =
    Types.type_rep_species species_def_desc.Parsetree.sd_name in
  let species_as_type_description = {
    Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
    Env.TypeInformation.type_identity = Types.generalize species_as_type ;
    Env.TypeInformation.type_arity = 0 ; (* Wrong !!! *) } in
  let full_env =
    Env.TypingEnv.add_type
      species_def_desc.Parsetree.sd_name species_as_type_description
      env_with_species in
  (* Interface printing stuff. *)
  if Configuration.get_do_interface_output () then
    (begin
    Format.printf "@[<2>species %s%a ;;@]@\n"
      species_def_desc.Parsetree.sd_name
      Env.TypeInformation.pp_species_description species_description
    end) ;
  (species_as_type, full_env)
;;



(* ************************************************************************ *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.type_def ->               *)
(*   Env.TypingEnv.t                                                        *)
(** {b Descr} : Transforms a type definition into a somewhat that can be
             inserted inside the environment. Also generates type
             constructors in case of a sum type definition and field
             labels in case of a record type definition.
             Both field labels and constructors with arguments are assigned
             a type scheme like a function taking as argument the field's
             type (or a tuple with type constructor's arguments types) and
             returning a ST_construct embedding the record/sum type's name.
             For instance:
               type t = A of int
             will create a constructor A : (int) -> t
             where one must note that (int) stands for a degenerated tuple
             type with only 1 component.
             For other instance:
               type u = { junk : string }
             will create a field label junk : string -> u
             Sum type constructors with no argument are typed as constants
             of this type.
	     Also performs the interface printing stuff is needed.
   {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************ *)
let typecheck_type_def ctx env type_def =
  let type_def_descr = type_def.Parsetree.ast_desc in
  (* First, extend the [tyvars_mapping] of the current *)
  (* context with parameters of the type definition.   *)
  let vmapp_extention =
    List.map
      (fun var_name -> (var_name, Types.type_variable ()))
      type_def_descr.Parsetree.td_params in
  Types.end_definition ();
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
      (begin
      (* We do not insert the defined name itself  *)
      (* to reject recursive type abbreviations.   *)
      Types.begin_definition ();
      (* This definition will only add a type name, no new type constructor. *)
      let identity_type = typecheck_type_expr new_ctx env ty in
      Types.end_definition ();
      (* Generalize the scheme to get the real identity. *)
      let ty_descr = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
        Env.TypeInformation.type_identity = Types.generalize identity_type ;
        Env.TypeInformation.type_arity = nb_params } in
      (* Just returns the environment extended by the type itself. *)
      Env.TypingEnv.add_type type_def_descr.Parsetree.td_name ty_descr env
      end)
  | Parsetree.TD_union constructors ->
      (* Sum types are allowed to be recursive. So make a proto     *)
      (* definition that will be used to infer the type declaration *)
      (* if it is recursive.                                        *)
     (let futur_type_type =
        Types.type_basic
          type_def_descr.Parsetree.td_name (List.map snd vmapp_extention) in
      let proto_descrip = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_variant [] ;
        Env.TypeInformation.type_identity =
	  Types.closed_scheme futur_type_type ;
        Env.TypeInformation.type_arity = nb_params } in
      (* Extend the environment with ourselves. *)
      let new_env =
        Env.TypingEnv.add_type
	  type_def_descr.Parsetree.td_name proto_descrip env in
      (* Now process the constructors of the type. Create the  *)
      (* list of couples : (constructor name * type_simple).   *)
      let cstr_bindings =
        List.map
          (fun (cstr_name, cstr_args) ->
            match cstr_args with
             | [] ->
                 (* No argument for the constructor. So it's a constant. *)
                 let cstr_descr = {
                   Env.TypeInformation.cstr_arity =
		     Env.TypeInformation.CA_zero ;
                   Env.TypeInformation.cstr_scheme =
		     Types.generalize futur_type_type } in
                 (cstr_name, cstr_descr)
             | _ ->
                 (* There are some argument(s). So the constructor is *)
                 (* types as a function taking a tuple of argument(s) *)
                 (* and returning the type of the current definition. *)
                 Types.begin_definition () ;
                 let args_ty =
                   List.map
                     (typecheck_type_expr new_ctx new_env) cstr_args in
                 (* Make a tuple of the arguments. *)
                 let as_tuple = Types.type_tuple args_ty in
                 let arrow = Types.type_arrow as_tuple futur_type_type in
                 Types.end_definition () ;
                 let cstr_descr = {
                   Env.TypeInformation.cstr_arity = Env.TypeInformation.CA_one ;
                   Env.TypeInformation.cstr_scheme = Types.generalize arrow } in
                 (cstr_name, cstr_descr))
          constructors in
      (* And finally, extends the environment with the constructors. *)
      let env_with_constructors =
        List.fold_left
          (fun accu_env (cstr_name, cstr_descr) ->
            Env.TypingEnv.add_constructor cstr_name cstr_descr accu_env)
          env
          cstr_bindings in
      (* Now add the type itself. *)
      let final_type_descr = {
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_variant
            (List.map
               (fun (n, descr) -> (n, descr.Env.TypeInformation.cstr_scheme))
               cstr_bindings) ;
        Env.TypeInformation.type_identity = Types.generalize futur_type_type ;
        Env.TypeInformation.type_arity = nb_params } in
      (* And return the fully extended environment. *)
      Env.TypingEnv.add_type
        type_def_descr.Parsetree.td_name
        final_type_descr env_with_constructors)
  | Parsetree.TD_record labels ->
      (* We do not insert the defined record name *)
      (* itself to reject recursive record types. *)
      (* First, we sort the label list in order to get a canonical *)
      (* representation of a record.                               *)
      let labels = Sort.list (fun (n1, _) (n2, _) -> n1 <= n2) labels in
      (* Let's create the [ST_construct] that will    *)
      (* represent the type of values of this record. *)
      let futur_type_type =
        Types.type_basic
          type_def_descr.Parsetree.td_name (List.map snd vmapp_extention) in
      (* Now typecheck the fields of the record. *)
      let fields_descriptions =
        List.map
          (fun (lbl_name, lbl_ty_expr) ->
            Types.begin_definition ();
            let lbl_ty = typecheck_type_expr new_ctx env lbl_ty_expr in
            let arrow = Types.type_arrow lbl_ty futur_type_type in	     
            Types.end_definition ();
            let lbl_scheme = Types.generalize arrow in
            (* Currently, fields do not support the "mutable" tag. *)
            (lbl_name, { Env.TypeInformation.field_mut =
			   Env.TypeInformation.FM_immutable;
                         Env.TypeInformation.field_scheme = lbl_scheme }))
          labels in
      (* And finally, extends the environment with the labels. *)
      let env_with_labels =
        List.fold_left
          (fun accu_env (lbl_name, lbl_descr) ->
            Env.TypingEnv.add_label lbl_name lbl_descr accu_env)
          env
          fields_descriptions in
      (* Now add the type itself. *)
      let final_type_descr = {
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_record
            (List.map
               (fun (lbl_name, lbl_descr) ->
                 (lbl_name,
                  lbl_descr.Env.TypeInformation.field_mut,
                  lbl_descr.Env.TypeInformation.field_scheme))
               fields_descriptions);
        Env.TypeInformation.type_identity = Types.generalize futur_type_type ;
        Env.TypeInformation.type_arity = nb_params } in
      (* And return the fully extended environment. *)
      Env.TypingEnv.add_type
        type_def_descr.Parsetree.td_name
        final_type_descr env_with_labels
;;




(* ****************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.phrase ->           *)
(*   Env.TypingEnv.t                                                  *)
(** {b Descr} : Performs type inference on a [phrase] and returns the
                initial environment extended with the possible type
		bindings induced by the [phrase].
                Also assign the infered type in the [ast_type] field
                of the [phrase] node.

    {b Rem} : Not exported outside this module                        *)
(* ****************************************************************** *)
let typecheck_phrase ctx env phrase =
  let (final_ty, new_env) =
    (match phrase.Parsetree.ast_desc with
     | Parsetree.Ph_external exter_def ->
	 let env' = typecheck_external_def env exter_def in
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_use _ -> failwith "todo T1"
     | Parsetree.Ph_open _ -> failwith "todo T2"
     | Parsetree.Ph_species species_def ->
	 (* Interface printing stuff is done inside. *)
	 typecheck_species_def ctx env species_def
     | Parsetree.Ph_coll coll_def -> failwith "todo T4"
     | Parsetree.Ph_type type_def ->
	 let env' = typecheck_type_def ctx env type_def in
         (* Interface printing stuff must be bone inside. *)
	 if Configuration.get_do_interface_output () then
	   Format.printf "type ...@\n" ;
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_let let_def  ->
	 let envt_bindings = typecheck_let_definition ctx env let_def in
	 (* Extend the current environment with the *)
	 (* bindings induced the let-definition.    *)
	 let env' =
	   List.fold_left
	     (fun accu_env (id, ty_scheme) ->
	       (* Interface printing stuff. *)
	       if Configuration.get_do_interface_output () then
		 Format.printf "val %a in %a@\n"
		   Sourcify.pp_vname id Types.pp_type_scheme ty_scheme ;
	       (* Extend the environment with the current binding. *)
	       Env.TypingEnv.add_value id ty_scheme accu_env)
	 env envt_bindings in
	 (* Return unit and the extended environment. *)
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_theorem theorem_def ->
	 let ty = typecheck_theorem_def ctx env theorem_def in
	 let scheme = Types.generalize ty in
	 let env' =
	   Env.TypingEnv.add_value
	     theorem_def.Parsetree.ast_desc.Parsetree.th_name scheme env in
	 (* Interface printing stuff. *)
	 if Configuration.get_do_interface_output () then
	   Format.printf "theorem %a in %a@\n"
	     Sourcify.pp_vname theorem_def.Parsetree.ast_desc.Parsetree.th_name
	     Types.pp_type_simple ty ;
	 (ty, env')
     | Parsetree.Ph_expr expr ->
	 let expr_ty = typecheck_expr ctx env expr in
	 (* No interface printing stuff because the expression is not bound. *)
	 (expr_ty, env)) in
  (* Store the type information in the phrase's node. *)
  phrase.Parsetree.ast_type <- Some final_ty ;
  (* Return the environment extended with the bindings induced by the phrase. *)
  new_env
;;



let typecheck_file current_unit ast_file =
  match ast_file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
       (* A phrase is always typed in an empty context. *)
       let ctx = {
	 current_unit = current_unit ;
	 current_species = None ;
	 self_manifest = None ;
	 tyvars_mapping = [] } in
       let global_env = ref (Env.TypingEnv.pervasives ()) in
       List.iter
	 (fun phrase ->
	   global_env := typecheck_phrase ctx !global_env phrase)
	 phrases ;
       !global_env
;;
