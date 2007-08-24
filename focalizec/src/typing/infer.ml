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


(* $Id: infer.ml,v 1.39 2007-08-24 10:51:20 pessaux Exp $ *)

(* *********************************************************************** *)
(** {b Descr} : Exception used to inform that a sum type constructor was
               used with an incorrect arity. The correct expected arity is
              stored in the second argument of the exception constructor.

    {b Rem} : Exported outside this module.                                *)
(* *********************************************************************** *)
exception Bad_sum_type_constructor_arity of
  (Parsetree.ident * (** The name of the misused sum type constructor. *)
   Env.TypeInformation.constructor_arity (** The correct arity it should have
					     be used. *)
  ) ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when a type variable was not found inside
              the [tyvars_mapping] of current [typing_context]. This means
              that while typechecking a type definition, its body contains
              a type variable not specified in the type parameters list.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception Unbound_type_variable of
  string    (** The name of the unbound variable. *)
;;



(* ************************************************************************* *)
(** {b Descr} : Exception raised when a method is defined several times in
              the same species.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
exception Method_multiply_defined of
  (Parsetree.vname *     (** The method's name. *)
   Types.species_name)   (** The species's name where the method is defined. *)
;;



(* ********************************************************************** *)
(** {b Descr} : Exception raised when a type constructor is applied to an
              incorrect number of type arguments.

    {b Rem} : Exported outside this module.                               *)
(* ********************************************************************** *)
exception Bad_type_arity of
  (Parsetree.ident *   (** The name of the misused type constructor. *)
   int *               (** The expected arity. *)
   int)                (** The arity the constructor was used with. *)
;;



(* ********************************************************************* *)
(** {b Descr} : Exception raised when "rep" is defined several times in
              the same species or when it is defined several time during
	      the inheritance.

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
exception Rep_multiply_defined of Location.t ;;



(* ******************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was found
              with 2 incompatible types.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
exception Not_subspecies_conflicting_field of
  ((** The collection name that should be a subspecies of the one below. *)
   Types.type_collection *
   (** The collection name that should be an overspecies of the one above. *)
   Types.type_collection *
   Parsetree.vname *     (** Name of the field. *)
   Types.type_simple *   (** First subpart of type found for this field. *)
   Types.type_simple *   (** Second subpart of type found for this field. *)
   Location.t)           (** Related location when the error occured. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was found
              with 2 types that can't be unified because one of them
              occurs in the other.

    {b Rem} : Exported outside this module.
              Note that I can't really see how this coudl happen...    *)
(* ******************************************************************* *)
exception Not_subspecies_circular_field of
  ((** The collection name that should be a subspecies of the one below. *)
   Types.type_collection *
   (** The collection name that should be an overspecies of the one above. *)
   Types.type_collection *
   Parsetree.vname *     (** Name of the field. *)
   Types.type_simple *   (** First subpart of type found for this field. *)
   Types.type_simple *   (** Second subpart of type found for this field. *)
   Location.t)           (** Related location when the error occured. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was found
              with 2 types that can't be unified because they do not
              respect the correct type constructor arity.

    {b Rem} : Exported outside this module.
              Note that I can't really see how this coudl happen...    *)
(* ******************************************************************* *)
exception Not_subspecies_arity_mismatch of
  ((** The collection name that should be a subspecies of the one below. *)
   Types.type_collection *
   (** The collection name that should be an overspecies of the one above. *)
   Types.type_collection *
   Parsetree.vname *    (** Name of the field. *)
   Types.type_name *    (** Name of the type constructor. *)
   int *                (** First arity the type constructor was used with. *)
   int *                (** Second arity the type constructor was used with. *)
    Location.t)         (** Related location when the error occured. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was not
              found in the species signature that is considered as the
              subspecies.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
exception Not_subspecies_missing_field of
  ((** The collection name that should be a subspecies of the one below. *)
   Types.type_collection *
   (** The collection name that should be an overspecies of the one above. *)
   Types.type_collection *
   Parsetree.vname *     (** Field name that was not found. *)
   Location.t)           (** Related location when the error occured. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During a parameterized species application, the number
              of provided arguments is incorrect compared to the
	      number of expected arguments.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
exception Parameterized_species_arity_mismatch of
  string    (** The message to insert in the error message: "many" or "few". *)
;;


(* ************************************************************************* *)
(** {b Descr} : Datastructure recording various the information required
              and propagated during the type inference. It is much more
              convenient to group the various flags and stuff needed than
              passing them all the time as arguments of each recursive call.
              This datastructure serves especially this purpose.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
type typing_context = {
  (** The name of the currently analysed compilation unit. *)  
  current_unit : Types.fname ;
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
		  ~loc: ident.Parsetree.ast_loc
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
	     ~loc: ty_cstr_ident.Parsetree.ast_loc
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
	 (* Now get a fresh instance of the type's type scheme being   *)
	 (* cautious to preserve sharing between the scheme parameters *)
	 (* during the instanciation.                                  *)
	 let (ty_constr_type, ty_constr_params) =
	   Types.specialize2
	     ty_descr.Env.TypeInformation.type_identity args_ty in
	 (* We now must unify the parameters with the effective *)
	 (* arguments types provided in the application.        *)
	 List.iter2
	   (Types.unify
	      ~loc: ty_expr.Parsetree.ast_loc ~self_manifest: ctx.self_manifest)
	   ty_constr_params args_ty ;
	 ty_constr_type
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
		  ~loc: ident.Parsetree.ast_loc
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
	     ~loc: ty_cstr_ident.Parsetree.ast_loc
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
	 (* Now get a fresh instance of the type's type scheme being   *)
	 (* cautious to preserve sharing between the scheme parameters *)
	 (* during the instanciation.                                  *)
	 let (ty_constr_type, ty_constr_params) =
	   Types.specialize2
	     ty_descr.Env.TypeInformation.type_identity args_ty in
	 (* We now must unify the parameters with the effective *)
	 (* arguments types provided in the application.        *)
	 List.iter2
	   (Types.unify
	      ~loc: rep_type_def.Parsetree.ast_loc
	      ~self_manifest: ctx.self_manifest)
	   ty_constr_params args_ty ;
	 ty_constr_type
	 end)
     | Parsetree.RTE_prod (ty_exprs) ->
	 let tys = List.map (typecheck_rep_type_def ctx env) ty_exprs in
	 Types.type_tuple tys
     | Parsetree.RTE_paren inner -> typecheck_rep_type_def ctx env inner) in
  (* Store the type information in the expression's node. *)
  rep_type_def.Parsetree.ast_type <- Some final_ty ;
  final_ty
;;



(* ********************************************************************* *)
(* Env.TypingEnv.t -> Parsetree.expr -> bool *)
(** {b Descr} : If returns [true] then one can generalise the expression
             passed as argument.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
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
          let lbl_descr =
	    Env.TypingEnv.find_label ~loc: expr.Parsetree.ast_loc lbl env in
          (lbl_descr.Env.TypeInformation.field_mut =
	    Env.TypeInformation.FM_immutable) && (is_non_expansive env e))
        lbl_exp_list
  | Parsetree.E_record_access (e, _) -> is_non_expansive env e
  | Parsetree.E_constr (_, exprs) ->
      List.for_all (is_non_expansive env) exprs
  | Parsetree.E_paren e -> is_non_expansive env e
  | _ -> false
;;



(* ******************************************************************** *)
(* Parsetree.constant -> Types.type_simple                              *)
(** {b Descr} : Infers the type of a constant. Records this type in the
              AST node and return the type.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
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



(* ********************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.pattern ->             *)
(*   Types.type_simple * (Parsetree.vname * Types.type_scheme) list      *)
(** {b Descr} : Infers the type of a [pattern]. Records this type in the
              AST node and return the type and a list of bindings
              that map the variables found in the pattern and their
              types.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
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
	 (* Find a specialization of the constructor's type scheme. *)
	 let cstr_decl =
	   Env.TypingEnv.find_constructor
	     ~loc: cstr_name.Parsetree.ast_loc
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
		~loc: pat_desc.Parsetree.ast_loc
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
		  let lbl_desc =
		    Env.TypingEnv.find_label
		      ~loc: pat.Parsetree.ast_loc lbl env in
		  (* Get the related label type. *)
		  let lbl_ty =
		    Types.specialize
		      lbl_desc.Env.TypeInformation.field_scheme in
		  (* Now, ensure the 2 sub-pattners types are compatible and *)
		  (* that the resulting record type for the whole pattern is *)
		  (* consistent.                                             *)
		  Types.unify
		    ~loc: pat.Parsetree.ast_loc
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
  let e_def_desc = e_def.Parsetree.ast_desc in
  match e_def_desc with
  | Parsetree.ED_type body ->
      (* Get the name of the defined type as a simple string. *)
      let name_as_str =
        Parsetree_utils.name_of_vname
          body.Parsetree.ast_desc.Parsetree.ed_name in
      (* We will build an abstract type of this name with as many *)
      (* parameters we find in the [ed_params] list.              *)
      Types.begin_definition () ;
      (* Make the parameters... *)
      let params =
        List.map
	  (fun _ -> Types.type_variable ())
	  body.Parsetree.ast_desc.Parsetree.ed_params in
      (* Make the constructor... *)
      let ty = Types.type_basic name_as_str params in
      Types.end_definition () ;
      let (identity, params) = Types.generalize2 ty params in
      (* And now make the type's description to insert in the environment. *)
      let ty_descr = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
        Env.TypeInformation.type_identity = identity ;
	Env.TypeInformation.type_params = params ;
        Env.TypeInformation.type_arity = List.length params } in
      Env.TypingEnv.add_type name_as_str ty_descr env
  | Parsetree.ED_value body ->
      (* [Unsure] *)
      (* Anyway, just make a fresh type variable because *)
      (* we don't have any type information here yet.    *)
      Types.begin_definition () ;
      let ty = Types.type_variable () in
      Types.end_definition () ;
      (* Record the type inside the node. I think it's useless, but... *)
      body.Parsetree.ast_type <- Some ty ;
      let scheme = Types.generalize ty in
      Env.TypingEnv.add_value
	body.Parsetree.ast_desc.Parsetree.ed_name scheme env
;;



(* Does not make any assumption. Crudely returns a fresh type variable. *)
let typecheck_external_expr ext_expr =
  let ty = Types.type_variable () in (* A somewhat of magic obj... *)
  ext_expr.Parsetree.ast_type <- Some ty ;
  ty
;;



(* **************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->     *)
(*   Env.TypeInformation.species_field list -> unit                 *)
(* {Descr} : Checks if the 2 lists of fields contain methods names
           that overlap. If so then raises en exception
           [Method_multiply_defined], else silently returns.

   {Rem} : Not exported outside this module.                        *)
(* **************************************************************** *)
let ensure_methods_uniquely_defined current_species l1 l2 =
  (* Just a local flattening function... *)
  let local_flat_fields fields =
    List.fold_right
      (fun field accu ->
	match field with
	 | Env.TypeInformation.SF_sig (v, _)
	 | Env.TypeInformation.SF_let (v, _, _) -> v :: accu
	 | Env.TypeInformation.SF_let_rec l ->
	     let l' = List.map (fun (v, _, _) -> v) l in
	     l' @ accu)
      fields [] in
  (* Now get the flat list of all the methods names. *)
  let flat_l1 = local_flat_fields l1 in
  let flat_l2 = local_flat_fields l2 in
  (* And check for no overlap. *)
  List.iter
    (fun name1 ->
      List.iter
	(fun name2 ->
	  if name1 = name2 then
	    raise (Method_multiply_defined (name1, current_species)))
	flat_l2)
    flat_l1
;;



(* ************************************************************************ *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.expr -> Types.type_simple *)
(** {b Descr} : Infers the type o an [expr] and assign it by side effect in
              the [ast_type] field of the [expr] node.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let rec typecheck_expr ctx env initial_expr =
  (begin
  let final_ty =
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
		 arg_name (Types.trivial_scheme arg_ty) accu_env)
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
	   Env.TypingEnv.find_value
	     ~loc: ident.Parsetree.ast_loc
	     ~current_unit: ctx.current_unit ident env in
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
	       ~loc: initial_expr.Parsetree.ast_loc
	       ~self_manifest: ctx.self_manifest tmp_fun_ty accu_fun_ty ;
	     (* The result is the positive part of the arrow. *)
	     result_ty)
           fun_ty
	   ty_exprs
     | Parsetree.E_constr (cstr_expr, exprs) ->
	 (* Because the environment maps [idents] onto type schemes and *)
         (* because the constructor's name is not a "full" ident, we    *)
         (* just wrap the constructor's name into a global [ident]      *)
         (* to be able to lookup inside the environment.                *)
	 let pseudo_ident =
           let Parsetree.CE (fname_opt, vname) = cstr_expr.Parsetree.ast_desc in
	   { Parsetree.ast_loc = cstr_expr.Parsetree.ast_loc ;
             Parsetree.ast_desc = Parsetree.I_global (fname_opt, vname) ;
	     Parsetree.ast_doc = cstr_expr.Parsetree.ast_doc ;
	     Parsetree.ast_type = None } in
	 let cstr_decl =
	   Env.TypingEnv.find_constructor
	     ~loc: cstr_expr.Parsetree.ast_loc
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
		~loc: initial_expr.Parsetree.ast_loc
		~self_manifest: ctx.self_manifest
		cstr_ty (Types.type_arrow cstr_arg_ty result_ty) ;
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
	       ~loc: initial_expr.Parsetree.ast_loc
	       ~self_manifest: ctx.self_manifest matched_expr_ty pat_ty ;
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
	       ~loc: initial_expr.Parsetree.ast_loc
	       ~self_manifest: ctx.self_manifest result_ty clause_ty)
	   bindings;
	 (* Return the type of the bodies' clauses. *)
	 result_ty
     | Parsetree.E_if (e_cond, e_then, e_else) ->
	 let ty_cond = typecheck_expr ctx env e_cond in
	 (* Ensure the condition is a boolean. *)
	 Types.unify
	   ~loc: initial_expr.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest ty_cond (Types.type_bool ()) ;
	 (* Typecheck the "then" expression. *)
	 let ty_then = typecheck_expr ctx env e_then in
	 let ty_else = typecheck_expr ctx env e_else in
	 (* Enforce both branches to have the same type. *)
	 Types.unify
	   ~loc: initial_expr.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest ty_then ty_else ;
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
	 let label_desc =
	   Env.TypingEnv.find_label
	     ~loc: initial_expr.Parsetree.ast_loc label env in
	 (* Just remind that labels are types as functions of type     *)
	 (* "type of the field as seen by user -> type od the record". *)
	 let label_ty =
	   Types.specialize label_desc.Env.TypeInformation.field_scheme in
	 (* Get a holder to extract the "arg type of the function", *)
	 (* i.e. the type of the field as seen by the user.         *)
	 let result_ty = Types.type_variable () in
	 Types.unify
	   ~loc: initial_expr.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest
	   (Types.type_arrow result_ty ty_expr) label_ty ;
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
  initial_expr.Parsetree.ast_type <- Some final_ty ;
  final_ty
  end)



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
       Format.eprintf "Labels exhaustivity not checked on record expression.@\n"
   | Some expr ->
       let expr_ty = typecheck_expr ctx env expr in
       Types.unify
	 ~loc: expr.Parsetree.ast_loc
	 ~self_manifest: ctx.self_manifest expr_ty result_ty) ;
  (* Now proceed with the labels.                               *)
  (* Just remind that labels are types as functions of type     *)
  (* "type of the field as seen by user -> type od the record". *)
  List.iter
    (fun (label, expr) ->
      let expr_ty = typecheck_expr ctx env expr in
      let lbl_descr =
	Env.TypingEnv.find_label ~loc: expr.Parsetree.ast_loc label env in
      (* Get the functionnal type of this field. *)
      let field_ty =
	Types.specialize lbl_descr.Env.TypeInformation.field_scheme in
      (* Unify the result type by side effect. *)
      Types.unify
	~loc: expr.Parsetree.ast_loc
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
	  let scheme = Types.trivial_scheme ty in
	  Env.TypingEnv.add_value vname scheme accu_env)
	env pre_env_info) in
  (* Now typecheck each def's body. *)
  let env_bindings =
    List.map2
      (fun { Parsetree.ast_desc = binding ; Parsetree.ast_loc = binding_loc }
	   (_, assumed_ty, non_expansive) ->
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
		arg_name (Types.trivial_scheme arg_ty) accu_env)
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
	       ~loc: ty_expr.Parsetree.ast_loc
	       ~self_manifest: ctx.self_manifest
	       constraint_ty infered_body_ty) ;
	(* Now, reconstruct the functional type from the body's and args' *)
        (* types. DO NOT fold_left, otherwise the fun type gets mirored ! *)
        (* By the way, be careful to create the type arrow with the right *)
	(* binding level, and especially, not outside the binding level   *)
	(* used to infer the body !!! That's why the end_definition is    *)
	(* done AFTER having created the [complete_ty].                   *)
        let complete_ty =
	  List.fold_right
	    (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
	    args_tys
	    infered_body_ty in
        if non_expansive then Types.end_definition () ;
        (* Unify the found type with the type that was temporarily assumed. *)
        Types.begin_definition () ;
        Types.unify
	  ~loc: binding_loc
	  ~self_manifest: ctx.self_manifest assumed_ty complete_ty ;
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
         (Types.begin_definition () ;
	 (* Get the couple (name, type) for each defined variable. *)
	 let bound_variables =
	   (let ty = typecheck_type_expr ctx env t_expr in
	   List.map (fun vname -> (vname, ty)) vnames) in
	 (* Now typecheck the prop's body in the extended environment.     *)
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
	 Types.unify
	   ~loc: prop.Parsetree.ast_loc ~self_manifest: ctx.self_manifest
	   ty1 ty2 ;
	 (* Enforce the type to be [prop]. *)
	 Types.unify
	   ~loc: prop.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest ty1 (Types.type_prop ()) ;
	 ty1
     | Parsetree.Pr_not pr ->
         let ty = typecheck_prop ctx env pr in
	 (* Enforce the type to be [prop]. *)
	 Types.unify
	   ~loc: prop.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest ty (Types.type_prop ()) ;
         ty
     | Parsetree.Pr_expr expr ->
	 (* Expressions must be typed as [bool]. If *)
         (* so, then the returned  type is [prop].  *)
	 let ty = typecheck_expr ctx env expr in
         Types.unify
	   ~loc: prop.Parsetree.ast_loc
	   ~self_manifest: ctx.self_manifest ty (Types.type_bool ()) ;
         Types.type_prop ()
     | Parsetree.Pr_paren pr -> typecheck_prop ctx env pr) in
  prop.Parsetree.ast_type <- Some final_ty ;
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
   | Parsetree.Pf_node nodes -> List.iter (typecheck_node ctx env) nodes



and typecheck_node ctx env node =
  match node.Parsetree.ast_desc with
   | Parsetree.PN_sub (_, statement, _) -> typecheck_statement ctx env statement
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



(* ********************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.theorem_def ->          *)
(*  Types.type_simple                                                     *)
(** {b Descr } : Typechecks a theorem definition, records its type inside
               the AST node and returns this type.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
and typecheck_theorem_def ctx env theorem_def =
  let ty =
    typecheck_prop ctx env theorem_def.Parsetree.ast_desc.Parsetree.th_stmt in
  (* Record the type information in the AST node. *)
  theorem_def.Parsetree.ast_type <- Some ty ;
  (* Now, typecheck the proof to fix types inside by side effet. *)
  typecheck_proof ctx env theorem_def.Parsetree.ast_desc.Parsetree.th_proof ;
  (* And return the type pf the stamement as type of the theorem.*)
  ty



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_field ->           *)
(*   Env.TypeInformation.species_field list                                  *)
(** {b Descr} : Infers the types of the species fields contained in the
              list. The typing environment is incrementally extended
              with the found methods and used to typecheck the next
              methods.
              The function returns a triplet suitable to be inserted in
              the structure of a species's type.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
and typecheck_species_fields ctx env = function
  | [] -> ([], ctx)
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
	     Types.begin_definition () ;
	     let ty = typecheck_rep_type_def ctx env rep_type_def in
	     Types.end_definition () ;
	     (* Before modifying the context, just check that no "rep" *)
             (* was previously identified. If some, then fails.        *)
	     if ctx.self_manifest <> None then
	       raise (Rep_multiply_defined field.Parsetree.ast_loc) ;
	     let ctx' = { ctx with self_manifest = Some ty } in
	     (* Record the type information in the AST node. *)
	     field.Parsetree.ast_type <- Some ty ;
	     let field_info =
	       Env.TypeInformation.SF_sig (rep_vname, (Types.generalize ty)) in
	     ([field_info], ctx', env)
	     end)
	 | Parsetree.SF_sig sig_def ->
	     (begin
	     let sig_def_descr = sig_def.Parsetree.ast_desc in
Format.eprintf "Champ sig: %a@." Sourcify.pp_vname sig_def_descr.Parsetree.sig_name ;
	     Types.begin_definition () ;
	     let ty =
	       typecheck_type_expr ctx env sig_def_descr.Parsetree.sig_type in
	     Types.end_definition () ;
	     (* Record the type information in the AST nodes. *)
	     sig_def.Parsetree.ast_type <- Some ty ;
	     field.Parsetree.ast_type <- Some ty ;
	     (* Extend the environment with this new method of Self. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 sig_def_descr.Parsetree.sig_name scheme env in
	     let field_info =
	       Env.TypeInformation.SF_sig
		 (sig_def_descr.Parsetree.sig_name, scheme) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_let let_def ->
	     (begin
Format.eprintf "Champ let: @." ;
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
	     match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
	      | Parsetree.RF_rec ->
		  (begin
		  let field_infos =
		    List.map2
		      (fun (id, ty_scheme) binding ->
			let expr =
			  binding.Parsetree.ast_desc.Parsetree.b_body in
			(* Note that [expr] below is already typed here. *)
			(id, ty_scheme, expr))
		      bindings
		      let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
		  (* Recursive, so just 1 field with several names. *)
		  ([(Env.TypeInformation.SF_let_rec field_infos)], ctx, env')
		  end)
	      | Parsetree.RF_no_rec ->
		  (begin
		  (* Not recursive, then the list should be only 1 long.  *)
		  (* Anyway, if that not the case, this does not annoy.   *)
		  (* So we return a list of n fields with 1 name in each. *)
		  let field_infos =
		    List.map2
		      (fun (id, ty_scheme) binding ->
			let expr =
			  binding.Parsetree.ast_desc.Parsetree.b_body in
			(* Note that [expr] below is already typed here. *)
			Env.TypeInformation.SF_let (id, ty_scheme, expr))
		      bindings
		      let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
		  (field_infos, ctx, env')
		  end)
	     end)
	 | Parsetree.SF_property property_def ->
	     (begin
	     Types.begin_definition () ;
	     let ty =
	       typecheck_prop
		 ctx env property_def.Parsetree.ast_desc.Parsetree.prd_prop in
	     Types.end_definition () ;
	     (* Record the type information in the AST node. *)
	     property_def.Parsetree.ast_type <- Some ty ;
	     (* Extend the environment. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 property_def.Parsetree.ast_desc.Parsetree.prd_name
		 scheme env in
	     let field_info =
	       Env.TypeInformation.SF_sig
		 (property_def.Parsetree.ast_desc.Parsetree.prd_name, scheme) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_theorem theorem_def ->
	     (begin
	     Types.begin_definition () ;
	     let ty = typecheck_theorem_def ctx env theorem_def in
	     Types.end_definition () ;
	     (* Extend the environment. *)
	     let scheme = Types.generalize ty in
	     let env' =
	       Env.TypingEnv.add_value
		 theorem_def.Parsetree.ast_desc.Parsetree.th_name scheme env in
	     let field_info =
	       Env.TypeInformation.SF_sig
		 (theorem_def.Parsetree.ast_desc.Parsetree.th_name, scheme) in
	     ([field_info], ctx, env')
	     end)
	 | Parsetree.SF_proof proof_def ->
	     (begin
	     let proof_def_desc = proof_def.Parsetree.ast_desc in
	     typecheck_proof ctx env proof_def_desc.Parsetree.pd_proof ;
	     (* No extension there. *)
	     ([], ctx, env)
	     end)
	end) in
      let (rem_fields_tys, final_ctx) =
	typecheck_species_fields new_ctx new_env rem_fields in
      (* Make sure that method names are not *)
      (* bound several times in the species. *)
      let current_species =
	(match ctx.current_species with
	 | None ->assert false
	 | Some n -> n) in
      ensure_methods_uniquely_defined
	current_species fields_tys rem_fields_tys ;
      ((fields_tys @ rem_fields_tys), final_ctx)
;;



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.expr ->                    *)
(*   (Types.collection_name * Env.TypeInformation.species_description)       *)
(** {b Descr} : Typechecks an expression in the restricted case where is it
              used as a "is" parameter effective argument. In this particular
              case, the rule [COLL-INST] expects a collection identifier and
              nothing else. For this reason, the identifier is looked-up in
              the species environment.
              Because the AST structure cannot know a priori (i.e at parsing
              stage) is the expression used as argument will be the one of a
              "is" or a "in" argument, the [expr] rule is sufficiently
              general to absorbe any possible expression, ... but is also
              too large. Hence we perfom this check afterward, during the
              typing stage.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let rec typecheck_expr_collection_cstr_for_is_param ctx env initial_expr =
  match initial_expr.Parsetree.ast_desc with
   | Parsetree.E_self ->
       (* Should be always caught before, at scoping phase. *)
       raise
	 (Scoping.Self_cant_parameterize_itself initial_expr.Parsetree.ast_loc)
   | Parsetree.E_constr (cstr_expr, []) ->
       (* We re-construct a fake ident from the constructor expression *)
       (* just to be able to lookup inside the environment.            *)
       let Parsetree.CE (id_opt_fname, id_vname) =
	 cstr_expr.Parsetree.ast_desc in
       let pseudo_ident = { cstr_expr with
         Parsetree.ast_desc = Parsetree.I_global (id_opt_fname, id_vname) } in
       let descr =
	 Env.TypingEnv.find_species
	   ~loc: pseudo_ident.Parsetree.ast_loc
	   ~current_unit: ctx.current_unit pseudo_ident env in
       let id_effective_name =
	 (match id_opt_fname with None -> ctx.current_unit | Some n -> n) in
       (* We return the "collection type", and the collection's description. *)
       ((id_effective_name, (Parsetree_utils.name_of_vname id_vname)), descr)
   | Parsetree.E_paren expr ->
       typecheck_expr_collection_cstr_for_is_param ctx env expr
   | _ ->
       (* Should be always caught before, at scoping phase. *)
       raise
	 (Scoping.Is_parameter_only_coll_ident initial_expr.Parsetree.ast_loc)
;;



(* ************************************************************************ *)
(* (Types.fname * Types.collection_name) ->                                 *)
(*   Env.TypeInformation.species_field list ->                              *)
(*     Env.TypeInformation.species_field list                               *)
(** {b Descr} : Perform the abstraction of species methods. This implements
              the "A" function described in Virgile Prevosto's Phd, section
              3.8, page 41.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let abstraction cname fields =
  let rec rec_abstract = function
    | [] -> []
    | h :: q ->
	let h' =
	  (match h with
	   | Env.TypeInformation.SF_sig (vname, scheme)
	   | Env.TypeInformation.SF_let (vname, scheme, _) ->
	       Types.begin_definition () ;
	       let ty = Types.specialize scheme in
	       let ty' = Types.abstract_copy cname ty in
	       Types.end_definition () ;
	       [Env.TypeInformation.SF_sig (vname, (Types.generalize ty'))]
	   | Env.TypeInformation.SF_let_rec l ->
	       List.map
		 (fun (vname, scheme, _) ->
		   Types.begin_definition () ;
		   let ty = Types.specialize scheme in
		   let ty' = Types.abstract_copy cname ty in
		   Types.end_definition () ;
		   Env.TypeInformation.SF_sig (vname, (Types.generalize ty')))
		 l) in
	h' @ (rec_abstract q) in
  (* Do je job now... *)
  rec_abstract fields
;;



(* ********************************************************************* *)
(* loc: Location.t -> typing_context ->                                  *)
(*   name_should_be_sub_spe: Types.type_collection ->                    *)
(*     Env.TypeInformation.species_field list ->                         *)
(*       name_should_be_over_spe: Types.type_collection ->               *)
(*         Env.TypeInformation.species_field list -> unit                *)
(** {b Descr} : Check that [s1] is a subspecies of [s2]. This means that
              for all (v, sc) in s2,
                ex (v, sc') in s1 and sc = sc'.
              Hence, signature [s1] is "richer" than [s2].
              The names of the 2 engaged species are provided for error
              reporting purposes.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let is_sub_species_of ~loc ctx ~name_should_be_sub_spe s1
    ~name_should_be_over_spe s2 =
  let local_flat_fields fields =
    List.fold_right
      (fun field accu ->
	match field with
	 | Env.TypeInformation.SF_sig (v, sc)
	 | Env.TypeInformation.SF_let (v, sc, _) -> (v, sc) :: accu
	 | Env.TypeInformation.SF_let_rec l ->
	     let l' = List.map (fun (v, sc, _) -> (v, sc)) l in
	     l' @ accu)
      fields [] in
  let flat_s1 = local_flat_fields s1 in
  let flat_s2 = local_flat_fields s2 in
  (* Check that for all (v, sc) in s2, ex (v, sc') in s1 and sc = sc'. *)
  List.iter
    (fun (v2, sc2) ->
      let found =
	List.exists
	  (fun (v1, sc1) ->
	    if v1 = v2 then
	      (begin
	      Types.begin_definition () ;
	      let ty1 = Types.specialize sc1 in
	      let ty2 = Types.specialize sc2 in
	      (begin
	      (* We try to translate type errors into *)
	      (* more significant error messages.     *)
	      try Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2
	      with
	      | Types.Conflict (ty1, ty2, _) ->
		  raise
		    (Not_subspecies_conflicting_field
		       (name_should_be_sub_spe, name_should_be_over_spe, v1,
			ty1, ty2, loc))
	      | Types.Circularity (ty1, ty2, _) ->
		  (* Mostly improbable ! *)
		  raise
		    (Not_subspecies_circular_field
		       (name_should_be_sub_spe, name_should_be_over_spe, v1,
			ty1, ty2, loc))
	      | Types.Arity_mismatch (ty_name, ar1, ar2, _) ->
		  raise
		    (Not_subspecies_arity_mismatch
		       (name_should_be_sub_spe, name_should_be_over_spe, v1,
			ty_name, ar1, ar2,loc))
	      end) ;
		  Types.end_definition () ;
	      true
	      end)
	    else false)
	  flat_s1 in
      (* Check if we found the same method name than v2 amoung the v1's . *)
      (* Note that if 2 fields with the same name were found, then either *)
      (* they can be unified, hence [found = true], or the unification    *)
      (* failed, hence we can't be here, since an exception will abort    *)
      (* the computation !                                                *)
      if not found then
	raise
	  (Not_subspecies_missing_field
	     (name_should_be_sub_spe, name_should_be_over_spe, v2, loc)))
      flat_s2
;;



(* *********************************************************************** *)
(* typing_context -> Env.TypingEnv.t ->                                    *)
(*   Env.TypeInformation.species_description -> Parsetree.species_param -> *)
(*     Env.TypeInformation.species_field list                              *)
(** {b Descr} : Function managing application of arguments during species
              applications in species expressions.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let apply_species_arguments ctx env base_spe_descr params =
  let rec rec_apply accu_meths = function
    | ([], []) -> accu_meths
    | ((f_param :: rem_f_params), (e_param :: rem_e_params)) ->
	let new_meths =
	  (begin
	  let (Parsetree.SP e_param_expr) = e_param.Parsetree.ast_desc in
	  match f_param with
	   | Env.TypeInformation.SPAR_in (f_name, f_ty) ->
	       (* First, get the argument expression's type. *)
	       (*let expr_ty = typecheck_expr ctx env e_param_expr in *)
	       failwith "Et la suite ?.."
	   | Env.TypeInformation.SPAR_is (f_name, c1_ty) ->
	       let c1 =
		 (ctx.current_unit, (Parsetree_utils.name_of_vname f_name)) in
Format.eprintf "Trouvé un param formel IS nommé %a@." Sourcify.pp_vname f_name ;
Format.eprintf "Ses méthodes sont attendues comme:@." ;
Format.eprintf "%a@." Env.TypeInformation.pp_species_description
  { Env.TypeInformation.spe_sig_methods = c1_ty ;
    Env.TypeInformation.spe_is_collection = false ;
    Env.TypeInformation.spe_sig_params = [] } ;
Format.eprintf "Fin@." ;
	       (* Get the argument species expression signature and methods. *)
	       (* Note that to be well-typed this expression must ONLY be    *)
	       (* an [E_constr] (because species names are capitalized,      *)
               (* parsed as sum type constructors) that should be considered *)
               (* as a species name. C.f. Virgile Prevosto's Phd, section    *)
               (* 3.8, page 43.                                              *)
               (* Rule [COLL-INST].                                          *)
	       let (c2, expr_sp_description) = (* The c2 of Virgile's Phd. *)
		 typecheck_expr_collection_cstr_for_is_param
                   ctx env e_param_expr in
Format.eprintf "Il est appliqué à l'effectif %s#%s@." (fst c2) (snd c2) ;
Format.eprintf "dont les méthodes sont:@." ;
Format.eprintf "%a@."
  Env.TypeInformation.pp_species_description expr_sp_description ;
Format.eprintf "Fin@." ;
	       let big_A_i1_c2 = abstraction c2 c1_ty in
Format.eprintf "Une fois abstraites ses méthodes sont:@." ;
Format.eprintf "%a@." Env.TypeInformation.pp_species_description
  { Env.TypeInformation.spe_sig_methods = big_A_i1_c2 ;
    Env.TypeInformation.spe_is_collection = false ;
    Env.TypeInformation.spe_sig_params = [] } ;
Format.eprintf "Fin@." ;
	       (* Ensure that i2 <= A(i1, c2). *)
	       is_sub_species_of
		 ~loc: e_param.Parsetree.ast_loc ctx
		 ~name_should_be_sub_spe: c2
		 expr_sp_description.Env.TypeInformation.spe_sig_methods
                 ~name_should_be_over_spe: c1
		 big_A_i1_c2 ;
	       (* And now, the new methods where c1 <- c2. *)
	       let substd_meths =
		 List.map
		   (SubstColl.subst_species_field
		      ~current_unit: ctx.current_unit c1 c2)
		   accu_meths in
	       substd_meths
	  end) in
	rec_apply new_meths (rem_f_params, rem_e_params)
    | (rem_formals, _) ->
	(begin
	let rem_formals_len = List.length rem_formals in
	(* To be able to tell "... is applied to too many/to few arguments". *)
	let msg = (if rem_formals_len = 0 then "many" else "few") in
	raise (Parameterized_species_arity_mismatch msg)
	end) in
  (* Do the job now. *)
  rec_apply
    base_spe_descr.Env.TypeInformation.spe_sig_methods
    (base_spe_descr.Env.TypeInformation.spe_sig_params, params)
;;



(* **************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_expr ->   *)
(*   Env.TypeInformation.species_field list                         *)
(** {b Descr} : Typechecks a species expression, record its type in
              the AST node and return the list of its methods names
              type schemes and possible bodies (the list of fields
              in fact).

    {b Rem} :Not exported outside this module.                      *)
(* **************************************************************** *)
let typecheck_species_expr ctx env species_expr =
  let species_expr_desc = species_expr.Parsetree.ast_desc in
  (* Recover the information about the species. *)
  let species_species_description =
    Env.TypingEnv.find_species
      ~loc: species_expr.Parsetree.ast_loc
      ~current_unit: ctx.current_unit species_expr_desc.Parsetree.se_name env in
  (* Create the type of this species. *)
  let (species_module, species_name) =
    (match species_expr_desc.Parsetree.se_name.Parsetree.ast_desc with
     | Parsetree.I_local vname
     | Parsetree.I_global (None, vname) ->
	 (ctx.current_unit, (Parsetree_utils.name_of_vname vname))
     | Parsetree.I_global ((Some fname), vname) ->
	 (fname, (Parsetree_utils.name_of_vname vname))
     | Parsetree.I_method (_, _) ->
	 (* Species are not first class value, then  *)
	 (* they can't be returned by a method call. *)
	 assert false) in
  let species_carrier_type =
    Types.type_rep_species ~species_module ~species_name in
  (* Now, create the "species type" (a somewhat of signature). *)
  let species_methods =
    apply_species_arguments      ctx env species_species_description
      species_expr_desc.Parsetree.se_params in
  (* Record the type in the AST node. *)
  species_expr.Parsetree.ast_type <- Some species_carrier_type ;
  species_methods
;;



(* *********************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Types.species_name ->              *)
(*   (Parsetree.vname * Parsetree.species_param_type) list ->              *)
(*     (Env.TypingEnv.t * Env.TypeInformation.species_param list *         *)
(*      Types.type_species)                                                *)
(** {b Descr} : Performs the typechecking of a species definition
              parameters.
              It build the species type of the species owning these
	      parameters and return it.
	      It also build the list of [Env.TypeInformation.species_param]
              that will appear in the hosting species [spe_sig_params]
              field.
              It also extend the environment with the species
              induced by the parameters and the carrier types of
              these species induced by the parameters.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let typecheck_species_def_params ctx env species_name species_params =
  let rec rec_typecheck_params accu_env = function
    | [] -> (accu_env, [])
    | (vname, param_kind) :: rem ->
	(begin
	let param_name_as_string = Parsetree_utils.name_of_vname vname in
	match param_kind.Parsetree.ast_desc with
	 | Parsetree.SPT_in ident ->
	     (begin
	     (* Recover the module and the species name that the parameter *)
             (* must be of.                                                *)
	     let (param_sp_module, param_sp_name) =
	       (match ident.Parsetree.ast_desc with
		| Parsetree.I_local vname
		| Parsetree.I_global (None, vname) ->
		    (ctx.current_unit, (Parsetree_utils.name_of_vname vname))
		| Parsetree.I_global ((Some fname), vname) ->
		    (fname, (Parsetree_utils.name_of_vname vname))
		| Parsetree.I_method (_, _) ->
		    (* Species are not first class value, then  *)
		    (* they can't be returned by a method call. *)
		    assert false) in
	     (* Just check that the species exists to avoid raising an  *)
	     (* error at application-time if the species doesn't exist. *)
	     ignore
	       (Env.TypingEnv.find_species
		  ~loc: ident.Parsetree.ast_loc ~current_unit: ctx.current_unit
		  ident env) ;
	     (* Create the carrier type of the parameter and extend the *)
             (* current environment with this parameter as a value of   *)
             (* the carrier type.                                       *)
	     Types.begin_definition () ;
	     let param_carrier_ty =
	       Types.type_rep_species ~species_module: param_sp_module
		 ~species_name: param_sp_name in
	     Types.end_definition () ;
	     let accu_env' =
	       Env.TypingEnv.add_value
		 vname (Types.generalize param_carrier_ty) accu_env in
	     (* And now, build the species type of the application. *)
	     let (accu_env'', rem_spe_params) =
	       rec_typecheck_params accu_env' rem in
	     let current_spe_param =
	       Env.TypeInformation.SPAR_in
		 (vname, (param_sp_module, param_sp_name)) in
	     (* Finally, we return the fully extended environment and *)
	     (* the type of the species application we just built.    *)
	     (accu_env'', (current_spe_param:: rem_spe_params))
	     end)
	 | Parsetree.SPT_is species_expr ->
	     (begin
Format.eprintf "Trouvé un paramètre is dans la decl: %a@." Sourcify.pp_vname vname ;
	     (* First, typecheck the species expression .*)
	     let species_expr_fields =
	       typecheck_species_expr ctx env species_expr in
	     (* Create the [species_description] of the parameter *)
             (* and extend the current environment. Because the   *)
	     (* obtained species is not a declaration, it cannot  *)
	     (* have parameters or inheritance.                   *)
             (* This leads to a somewhat of "local" species, and  *)
	     (* we will bind it in the species environment under  *)
	     (* an internal name to be able to denote it in the   *)
	     (* type of the application.                          *)
	     (* This internal name is the name of the parameter.  *)
Format.eprintf "Méthodes de ce paramètre après analyse de l'expression d'espèces:@." ;
Format.eprintf "%a@." Env.TypeInformation.pp_species_description
  { Env.TypeInformation.spe_sig_methods = species_expr_fields ;
    Env.TypeInformation.spe_is_collection = false ;
    Env.TypeInformation.spe_sig_params = [] } ;
Format.eprintf "Fin@." ;
	     let abstracted_methods =
	       abstraction
		 (ctx.current_unit, param_name_as_string) species_expr_fields in
Format.eprintf "Méthodes de ce paramètre après abstraction:@." ;
Format.eprintf "%a@." Env.TypeInformation.pp_species_description
  { Env.TypeInformation.spe_sig_methods = abstracted_methods ;
    Env.TypeInformation.spe_is_collection = false ;
    Env.TypeInformation.spe_sig_params = [] } ;
Format.eprintf "Fin@." ;
	     let param_description = {
	       Env.TypeInformation.spe_is_collection = false ;
	       Env.TypeInformation.spe_sig_params = [] ;
	       Env.TypeInformation.spe_sig_methods = abstracted_methods } in
	     let accu_env' =
	       Env.TypingEnv.add_species
		 param_name_as_string param_description accu_env in
	     (* Create the carrier type of the parameter *)
             (* and extend the current environment.      *)
	     Types.begin_definition () ;
	     let param_carrier_ty =
	       Types.type_rep_species ~species_module: ctx.current_unit
		 ~species_name: param_name_as_string in
	     Types.end_definition () ;
	     let param_carrier_ty_description = {
	       Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
	       Env.TypeInformation.type_identity =
	         Types.generalize param_carrier_ty ;
	       (* No param because the species is fully applied. *)
	       Env.TypeInformation.type_params = [] ;
	       Env.TypeInformation.type_arity = 0 } in
	     let accu_env'' =
	       Env.TypingEnv.add_type
		 param_name_as_string param_carrier_ty_description accu_env' in
	     (* And now, build the species type of the application. *)
	     let (accu_env''', rem_spe_params) =
	       rec_typecheck_params accu_env'' rem in
	     let current_spe_param =
	       Env.TypeInformation.SPAR_is  (vname, abstracted_methods) in
	     (* Finally, we return the fully extended environment and *)
	     (* the type of the species application we just built.    *)
	     (accu_env''', (current_spe_param:: rem_spe_params))
	     end)
	end) in
  (* And now do the job... *)
  rec_typecheck_params env species_params
;;



(* ********************************************************************** *)
(* loc: Location.t -> typing_context -> Env.TypingEnv.t ->                *)
(* Parsetree.species_expr list ->                                         *)
(*   (Env.TypeInformation.species_field list *                            *)
(*    Env.TypingEnv.t * typing_context)                                   *)
(** {b Descr} : Extends an environment as value bindings with the methods
              of the inherited species provided in argument. Methods are
              added in the same order than their hosting species comes
              in the inheritance list. This means that the methods of the
              first inherited species will be deeper in the resulting
              environment.
              Extends the typing_context if among the inherited methods
              "rep" is found. In this case, this means that the carrier
              is manifest and changes the typing_context with its
              representation.

    {b Rem} :Not exported outside this module.                            *)
(* ********************************************************************** *)
let extend_env_with_inherits ~loc ctx env spe_exprs =
  let rec rec_extend current_ctx current_env revd_accu_found_methods = function
    | [] -> (revd_accu_found_methods, current_env, current_ctx)
    | inh :: rem_inhs ->
	(* First typecheck the species expression in the initial   *)
        (* (non extended) and recover its methods names and types. *)
	let inh_species_methods =
	  typecheck_species_expr current_ctx env inh in
	let (env', current_ctx')  =
	  List.fold_left
	    (fun (accu_env, accu_ctx) field ->
	      match field with
	       | Env.TypeInformation.SF_sig (meth_name, meth_scheme)
	       | Env.TypeInformation.SF_let (meth_name, meth_scheme, _) ->
		   let e =
		     Env.TypingEnv.add_value meth_name meth_scheme accu_env in
		   (* Now check if we inherited a [rep]. *)
		   let m_name_as_str =
		     Parsetree_utils.name_of_vname meth_name in
		   let manifest =
		     (if m_name_as_str = "rep" then
		       (begin
		       (* Before modifying the context, just check that no *)
		       (* "rep" was previously identified. If some, then   *)
		       (* fail.                                            *)
		       if accu_ctx.self_manifest <> None then
                         raise (Rep_multiply_defined loc) ;
		       Some (Types.specialize meth_scheme)
                       end)
		     else accu_ctx.self_manifest) in (* Else, keep unchanged. *)
		   let c = { accu_ctx with self_manifest = manifest } in
		   (e, c)
	       | Env.TypeInformation.SF_let_rec l ->
		   let e =
		     List.fold_left
		       (fun internal_accu_env (meth_name, meth_scheme, _) ->
			 Env.TypingEnv.add_value
			   meth_name meth_scheme internal_accu_env)
		       accu_env
		       l in
		   (e, accu_ctx))
	    (current_env, current_ctx)
	    inh_species_methods in
	let new_accu_found_methods =
	  inh_species_methods @ revd_accu_found_methods in
	rec_extend current_ctx' env' new_accu_found_methods rem_inhs in
  (* Now, let's work... The list of the found methods is built reversed *)
  (* for efficiency reason. So reverse it finally before returning so   *)
  (* that deeper inherited methods are in head of the list.             *)
  let (revd_found_methods, env', ctx') = rec_extend ctx env [] spe_exprs in
  ((List.rev revd_found_methods), env', ctx')
;;



(* ********************************************************************* *)
(* loc:Location.t -> typing_context -> Parsetree.vname ->                *)
(*   Types.type_scheme ->                                                *)
(*     (Parsetree.vname * Types.type_scheme * Parsetree.expr) list ->    *)
(*       Env.TypeInformation.species_field                               *)
(** {b Descr} : Implements the "fusion" algorithm (c.f [fields_fusion])
              in the particular case of fusionning 1 field Sig and 1
              field Let_rec.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let fusion_fields_let_rec_sig ~loc ctx sig_name sig_scheme rec_meths =
  let rec_meths' =
    List.map
      (fun ((n, sc, body) as rec_meth) ->
	if n = sig_name then
	  begin
	  Types.begin_definition () ;
	  let sig_ty = Types.specialize sig_scheme in
	  let ty = Types.specialize sc in
	  Types.unify ~loc ~self_manifest: ctx.self_manifest sig_ty ty ;
	  Types.end_definition () ;
	  (n, (Types.generalize ty), body)
	  end
	else rec_meth)
      rec_meths in
  Env.TypeInformation.SF_let_rec rec_meths'
;;



(* ******************************************************************** *)
(* 'a -> ('a * 'b * 'c) list -> ('a * 'b * 'c) * ('a * 'b * 'c) list *)
(* {b Descr} : Searches in the list the first element whose first
             component is equal to [name], then returns it and the list
             minus this element.
             If the searched name is not found in the list, then the
             exception [Not_found] is raised.

   {b Rem} : Not exported outside this module.                          *)
(* ******************************************************************** *)
let find_and_remain name meths =
  let rec rec_find = function
    | [] -> raise Not_found
    | ((n, _, _) as meth) :: rem ->
	if name = n then (meth, rem)
	else
	  let (found, tail) = rec_find rem in
	  (found, (meth :: tail)) in
  rec_find meths
;;



(* ******************************************************************** *)
(* loc: Location.t -> typing_context ->                                 *)
(*   (Parsetree.vname * Types.type_scheme * Parsetree.expr) list ->     *)
(*     (Parsetree.vname * Types.type_scheme * Parsetree.expr) list ->   *)
(*       Env.TypeInformation.species_field                              *)
(** {b Descr} : Implements the "fusion" algorithm (c.f [fields_fusion])
              in the particular case of fusionning 2 fields Let_rec.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let fusion_fields_let_rec_let_rec ~loc ctx rec_meths1 rec_meths2 =
  let rec rec_fusion l1 l2 =
    match l1 with
     | [] -> l2
     | ((n1, sc1, _) as meth) :: rem1 ->
	 let (fused_meth, new_l2) =
	   (try
	     let (m2, rem_of_l2) = find_and_remain n1 l2 in
	     let (_, sc2, _) = m2 in
	     let ty1 = Types.specialize sc1 in
	     let ty2 = Types.specialize sc2 in
	     (* Ensure that the 2 versions of the method are type-compatible. *)
	     Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2 ;
	     (* And return the seconde one (late binding). *)
	     (m2, rem_of_l2)
	   with Not_found ->
	     (* The method doesn't belog to l2, then keep this one. *)
	     (meth, l2)) in
	 (* Now make the fusion of the remaining of l1 and the remaining *)
	 (* of l2 (this las one being possibly l2 if the search failed). *)
	 let rem_fused_methods = rec_fusion rem1 new_l2 in
	 fused_meth :: rem_fused_methods in
  (* Go... *)
  Env.TypeInformation.SF_let_rec (rec_fusion rec_meths1 rec_meths2)
;;



(* ************************************************************************* *)
(* loc: Location.t -> typing_context -> Env.TypeInformation.species_field -> *)
(*   Env.TypeInformation.species_field -> Env.TypeInformation.species_field  *)
(** {b Descr} : Implements the "fusion" algorithm described in Virgile
              Prevosto's Phd, Section 3.6, page 35.
              This basically ensure that 2 fields with at leat 1 common
              name are type-compatible and select the new field information
              that summarizes these 2 original fields (implementing the late
              binding feature by the way).

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let fields_fusion ~loc ctx phi1 phi2 =
  match (phi1, phi2) with
   (* *** *)
   | (Env.TypeInformation.SF_sig (n1, sc1),
      Env.TypeInformation.SF_sig (n2, sc2)) when n1 = n2 ->
        (* sig / sig. *)
	Types.begin_definition () ;
	let ty1 = Types.specialize sc1 in
	let ty2 = Types.specialize sc2 in
	Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2 ;
	Types.end_definition () ;
	Env.TypeInformation.SF_sig (n1, (Types.generalize ty1))
   | (Env.TypeInformation.SF_sig (n1, sc1),
      Env.TypeInformation.SF_let (n2, sc2, body)) when n1 = n2 ->
        (* sig / let. *)
	Types.begin_definition () ;
	let ty1 = Types.specialize sc1 in
	let ty2 = Types.specialize sc2 in
	Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2 ;
	Types.end_definition () ;
	Env.TypeInformation.SF_let (n2, (Types.generalize ty2), body)
   | (Env.TypeInformation.SF_sig (n1, sc1),
      Env.TypeInformation.SF_let_rec rec_meths) ->
        (* sig / let rec. *)
	fusion_fields_let_rec_sig ~loc ctx n1 sc1 rec_meths
   (* *** *)
   | (Env.TypeInformation.SF_let (n1, sc1, body),
      Env.TypeInformation.SF_sig (n2, sc2)) when n1 = n2 ->
        (* let / sig. *)
	Types.begin_definition () ;
	let ty1 = Types.specialize sc1 in
	let ty2 = Types.specialize sc2 in
	Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2 ;
	Types.end_definition () ;
	Env.TypeInformation.SF_let (n1, (Types.generalize ty1), body)
   | (Env.TypeInformation.SF_let (n1, sc1, _),
      Env.TypeInformation.SF_let (n2, sc2, body)) when n1 = n2 ->
        (* let / let. *)
	(* Late binding : keep the second body ! *)
	Types.begin_definition () ;
	let ty1 = Types.specialize sc1 in
	let ty2 = Types.specialize sc2 in
	Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2 ;
	Types.end_definition () ;
	Env.TypeInformation.SF_let (n2, (Types.generalize ty2), body)
   | (Env.TypeInformation.SF_let (n1, sc1, _),
      Env.TypeInformation.SF_let_rec rec_meths) ->
	failwith "fields_fusion let / let rec"
   (* *** *)
   | (Env.TypeInformation.SF_let_rec rec_meths,
      Env.TypeInformation.SF_sig (n2, sc2)) ->
        (* let rec / sig. *)
	(* Symetric case than for sig / let_rec. *)
	fusion_fields_let_rec_sig ~loc ctx n2 sc2 rec_meths
   | (Env.TypeInformation.SF_let_rec rec_meths1,
      Env.TypeInformation.SF_let (n2, sc2, _)) ->
        failwith "fields_fusion let rec / let"
   | (Env.TypeInformation.SF_let_rec rec_meths1,
      Env.TypeInformation.SF_let_rec rec_meths2) ->
        fusion_fields_let_rec_let_rec ~loc ctx rec_meths1 rec_meths2
   | _ -> assert false  (* From Virgile's thesis Lemma 8 p 37 *)
;;



let oldest_inter_n_field_n_fields phi fields =
  let flat_phi_names =
    (match phi with
     | Env.TypeInformation.SF_sig (v, _)
     | Env.TypeInformation.SF_let (v, _, _) -> [v]
     | Env.TypeInformation.SF_let_rec l -> List.map (fun (v, _, _) -> v) l) in
  (* We will now check for an intersection between the list of names *)
  (* from phi and the names of one field of the argument [fields].   *)
  let rec rec_hunt = function
    | [] -> (None, [])
    | f :: rem_f ->
	(begin
	match f with
	 | Env.TypeInformation.SF_sig (v, _)
	 | Env.TypeInformation.SF_let (v, _, _) ->
	     if List.mem v flat_phi_names then ((Some f), rem_f)
	     else
	       let (found, rem_list) = rec_hunt rem_f in
	       (found, (f :: rem_list))
	 | Env.TypeInformation.SF_let_rec l ->
	     let names_in_l = List.map (fun (v, _, _) -> v) l in
	     if Handy.list_intersect_p flat_phi_names names_in_l then
	       ((Some f), rem_f)
	     else
	       let (found, rem_list) = rec_hunt rem_f in
	       (found, (f :: rem_list))
	end) in
  rec_hunt fields
;;



(* **************************************************************** *)
(** {b Descr} : Implements the normalization algorithm described in
              Virgile Prevosto's Phd, Section 3.7.1, page 36.

    {b Rem}: Not exported outside this module.                      *)
(* **************************************************************** *)
let normalize_species ~loc ctx methods_info inherited_methods_infos =
  let w1 = ref (inherited_methods_infos @ methods_info) in
  let w2 = ref ([] : Env.TypeInformation.species_field list) in
  let continue = ref true in
  while !continue do
    match !w1 with
     | [] -> continue := false
     | phi :: bigX ->
	 (begin
	 match oldest_inter_n_field_n_fields phi !w2 with
	  | (None, _) ->
	      w1 := bigX ;
	      w2 := !w2 @ [phi]
	  | ((Some psi_i0), sniped_w2) ->
	      w1 :=  (fields_fusion ~loc ctx phi psi_i0) :: bigX ;
	      w2 := sniped_w2
	 end)
  done ;
  !w2
;;



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_def ->             *)
(*  (Types.type_simple * Env.TypingEnv.t)                                    *)
(** {b Descr} : Typechecks a species definition. Il infers its signature and
              bind it to the species name in the environment. Finally, adds
              a type binding representing the species's carrier type.
              Also performs the interface printing stuff of a species.
              It returns both the extended environment and the species's
              carrier type.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let typecheck_species_def ctx env species_def =
  let species_def_desc = species_def.Parsetree.ast_desc in
Format.eprintf "Espèce: %s@." species_def_desc.Parsetree.sd_name ;
  (* First of all, we are in a species !!! *)
  let ctx = { ctx with
    current_species = Some species_def_desc.Parsetree.sd_name } in
  (* Extend the environment with the species param and   *)
  (* synthetize the species type of the current species. *)
  let (env_with_species_params, sig_params) =
    typecheck_species_def_params
      ctx env species_def_desc.Parsetree.sd_name
      species_def_desc.Parsetree.sd_params in
  (* We first load the inherited methods in the environment and  *)
  (* get their signatures and methods information by the way.    *)
  (* We also get a possibly new context where the fact that Self *)
  (* is now manifest is unpdated, in case we inherited a [repr]. *)
  let (inherited_methods_infos,
       env_with_inherited_methods,
       ctx_with_inherited_repr) = 
    extend_env_with_inherits
      ~loc: species_def.Parsetree.ast_loc ctx env_with_species_params
      species_def_desc.Parsetree.sd_inherits.Parsetree.ast_desc in
  (* Now infer the types of the current field's and recover *)
  (* the context  where we may know the shape of [repr].    *)
  let (methods_info, ctx') =
    typecheck_species_fields
      ctx_with_inherited_repr env_with_inherited_methods
      species_def_desc.Parsetree.sd_fields in
  (* Then one must ensure that each method has the same type everywhere *)
  (* in the inheritance tree and more generaly create the normalised    *)
  (* form of the species.                                               *)
  let normalized_methods =
    normalize_species
      ~loc: species_def.Parsetree.ast_loc ctx' methods_info
      inherited_methods_infos in
  (* Let's build our "type" information. Since we are managing a species *)
  (* and NOT a collection, we must set [spe_is_collection] to [false].   *)
  let species_description = {
    Env.TypeInformation.spe_is_collection = false ;
    Env.TypeInformation.spe_sig_params = sig_params ;
    Env.TypeInformation.spe_sig_methods = normalized_methods } in
  (* Extend the initial environment with the species. Not the environment *)
  (* used to typecheck the internal definitions of the species !!!        *)
  let env_with_species =
    Env.TypingEnv.add_species
      species_def_desc.Parsetree.sd_name species_description env in
  (* Now, extend the environment with a type that is the species. *)
  Types.begin_definition () ;
  let species_carrier_type =
    Types.type_rep_species
      ~species_module: ctx'.current_unit
      ~species_name: species_def_desc.Parsetree.sd_name in
  Types.end_definition () ;
  let species_as_type_description = {
    Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
    Env.TypeInformation.type_identity = Types.generalize species_carrier_type ;
    (* Nevers parameters for a species's carrier type ! *)
    Env.TypeInformation.type_params = [] ;
    Env.TypeInformation.type_arity = 0 } in
  let full_env =
    Env.TypingEnv.add_type
      species_def_desc.Parsetree.sd_name species_as_type_description
      env_with_species in
  (* Record the type in the AST node. *)
  species_def.Parsetree.ast_type <- Some species_carrier_type ;
  (* Interface printing stuff. *)
  if Configuration.get_do_interface_output () then
    (begin
    Format.printf "@[<2>species %s%a@]@\n"
      species_def_desc.Parsetree.sd_name
      Env.TypeInformation.pp_species_description species_description
    end) ;
  (species_carrier_type, full_env)
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
               [type t = A of int]
             will create a constructor [A : (int) -> t]
             where one must note that (int) stands for a degenerated tuple
             type with only 1 component.
             For other instance:
               [type u = { junk : string }]
             will create a field label [junk : string -> u]
             Sum type constructors with no argument are typed as constants
             of this type.
	     Also performs the interface printing stuff is needed.
   {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************ *)
let typecheck_type_def ctx env type_def =
  let type_def_desc = type_def.Parsetree.ast_desc in
  (* First, extend the [tyvars_mapping] of the current *)
  (* context with parameters of the type definition.   *)
  Types.begin_definition () ;
  let vmapp =
    List.map
      (fun var_name -> (var_name, Types.type_variable ()))
      type_def_desc.Parsetree.td_params in
  Types.end_definition () ;
  let vars_of_mapping = List.map snd vmapp in
  let new_ctx = { ctx with tyvars_mapping = vmapp } in
  (* Get the type constructor's arity. One could avoid a second iteration *)
  (* on the list by incrementing a reference while building the extention *)
  (* of the context, but that would be pretty uggly... And usually, there *)
  (* are no tons of parameters in types definitions !                     *)
  let nb_params = List.length type_def_desc.Parsetree.td_params in
  (* Process the body of the type definition. *)
  match type_def_desc.Parsetree.td_body.Parsetree.ast_desc with
  | Parsetree.TD_alias ty ->
      (begin
      (* We do not insert the defined name itself  *)
      (* to reject recursive type abbreviations.   *)
      Types.begin_definition () ;
      (* This definition will only add a type name, no new type constructor. *)
      let identity_type = typecheck_type_expr new_ctx env ty in
      Types.end_definition () ;
      (* Generalize the got type to get the real identity. *)
      let (identity_scheme, identity_params) =
	Types.generalize2 identity_type vars_of_mapping in
      let ty_descr = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
        Env.TypeInformation.type_identity = identity_scheme ;
        Env.TypeInformation.type_params = identity_params ;
        Env.TypeInformation.type_arity = nb_params } in
      (* Just returns the environment extended by the type itself. *)
      Env.TypingEnv.add_type type_def_desc.Parsetree.td_name ty_descr env
      end)
  | Parsetree.TD_union constructors ->
      (* Sum types are allowed to be recursive. So make a proto     *)
      (* definition that will be used to infer the type declaration *)
      (* if it is recursive.                                        *)
      (begin
      Types.begin_definition () ;
      let futur_type_type =
        Types.type_basic type_def_desc.Parsetree.td_name vars_of_mapping in
      Types.end_definition () ;
      let proto_descrip = {
        Env.TypeInformation.type_kind = Env.TypeInformation.TK_variant [] ;
        Env.TypeInformation.type_identity =
	  Types.trivial_scheme futur_type_type ;
        (* Because a trivial scheme, there is no parameter ! *)
        Env.TypeInformation.type_params = [] ;
        Env.TypeInformation.type_arity = nb_params } in
      (* Extend the environment with ourselves. *)
      let new_env =
        Env.TypingEnv.add_type
	  type_def_desc.Parsetree.td_name proto_descrip env in
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
      let (type_identity, type_params) =
	Types.generalize2 futur_type_type vars_of_mapping in
      let final_type_descr = {
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_variant
            (List.map
               (fun (n, descr) -> (n, descr.Env.TypeInformation.cstr_scheme))
               cstr_bindings) ;
        Env.TypeInformation.type_identity = type_identity ;
	Env.TypeInformation.type_params = type_params ;
        Env.TypeInformation.type_arity = nb_params } in
      (* And return the fully extended environment. *)
      Env.TypingEnv.add_type
        type_def_desc.Parsetree.td_name
        final_type_descr env_with_constructors
      end)
  | Parsetree.TD_record labels ->
      (* We do not insert the defined record name *)
      (* itself to reject recursive record types. *)
      (* First, we sort the label list in order to get a canonical *)
      (* representation of a record.                               *)
      let labels = Sort.list (fun (n1, _) (n2, _) -> n1 <= n2) labels in
      (* Let's create the [ST_construct] that will    *)
      (* represent the type of values of this record. *)
      Types.begin_definition () ;
      let futur_type_type =
        Types.type_basic
          type_def_desc.Parsetree.td_name vars_of_mapping in
      Types.end_definition () ;
      (* Now typecheck the fields of the record. *)
      let fields_descriptions =
        List.map
          (fun (lbl_name, lbl_ty_expr) ->
            Types.begin_definition () ;
            let lbl_ty = typecheck_type_expr new_ctx env lbl_ty_expr in
            let arrow = Types.type_arrow lbl_ty futur_type_type in	     
            Types.end_definition () ;
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
      let (type_identity, type_params) =
	Types.generalize2 futur_type_type vars_of_mapping in
      let final_type_descr = {
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_record
            (List.map
               (fun (lbl_name, lbl_descr) ->
                 (lbl_name,
                  lbl_descr.Env.TypeInformation.field_mut,
                  lbl_descr.Env.TypeInformation.field_scheme))
               fields_descriptions);
        Env.TypeInformation.type_identity = type_identity ;
	Env.TypeInformation.type_params = type_params ;
        Env.TypeInformation.type_arity = nb_params } in
      (* And return the fully extended environment. *)
      Env.TypingEnv.add_type
        type_def_desc.Parsetree.td_name
        final_type_descr env_with_labels
;;



let rec ensure_collection_completely_defined = function
  | [] -> ()
  | field :: rem_fields ->
      Format.eprintf "Todo : collection is fully defined@." ;
      ensure_collection_completely_defined rem_fields 
;;



let typecheck_collection_def ctx env coll_def =
  let coll_def_desc = coll_def.Parsetree.ast_desc in
  (* First of all, we are in a species !!! *)
  let ctx = { ctx with
    current_species = Some coll_def_desc.Parsetree.cd_name } in
 (* Typecheck the body's species expression .*)
  let species_expr_fields =
    typecheck_species_expr ctx env coll_def_desc.Parsetree.cd_body in
  (* One must ensure that the collection is *)
  (* really a completely defined species.   *)
  ensure_collection_completely_defined species_expr_fields ;
  (* Let's build our "type" information. Since we are managing a species *)
  (* and NOT a collection, we must set [spe_is_collection] to [false].   *)
  let collec_description = {
    Env.TypeInformation.spe_is_collection = true ;
    Env.TypeInformation.spe_sig_params = [] ;
    Env.TypeInformation.spe_sig_methods = species_expr_fields } in
  (* Add this collection in the environment. *)
  let env_with_collection =
    Env.TypingEnv.add_species
      coll_def_desc.Parsetree.cd_name collec_description env in
  (* Now, extend the environment with a type that is this collection. *)
  Types.begin_definition () ;
  let collec_carrier_type =
    Types.type_rep_species
      ~species_module: ctx.current_unit
      ~species_name: coll_def_desc.Parsetree.cd_name in
  Types.end_definition () ;
  let collec_as_type_description = {
    Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract ;
    Env.TypeInformation.type_identity = Types.generalize collec_carrier_type ;
    (* Nevers parameters for a species's carrier type ! *)
    Env.TypeInformation.type_params = [] ;
    Env.TypeInformation.type_arity = 0 } in
  let full_env =
    Env.TypingEnv.add_type
      coll_def_desc.Parsetree.cd_name collec_as_type_description
      env_with_collection in
  (* Record the type in the AST node. *)
  coll_def.Parsetree.ast_type <- Some collec_carrier_type ;
  (* Interface printing stuff. *)
  if Configuration.get_do_interface_output () then
    (begin
    Format.printf "@[<2>collection %s%a@]@\n"
      coll_def_desc.Parsetree.cd_name
      Env.TypeInformation.pp_species_description collec_description
    end) ;
  (collec_carrier_type, full_env)
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
     | Parsetree.Ph_use _ ->
	 (* Nothing to do, the scoping pass already ensured that *)
         (* "modules" opened or used were previously "use"-d.    *)
	 ((Types.type_unit ()), env)
     | Parsetree.Ph_open fname ->
	 (* Load this module interface to extend the current environment. *)
	 let env' =
	   Env.type_open_module ~loc: phrase.Parsetree.ast_loc fname env in
	 ((Types.type_unit ()), env')
     | Parsetree.Ph_species species_def ->
	 (* Interface printing stuff is done inside. *)
	 typecheck_species_def ctx env species_def
     | Parsetree.Ph_coll coll_def ->
	 (* Interface printing stuff is done inside. *)
	 typecheck_collection_def ctx env coll_def
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
	 Types.begin_definition () ;
	 let ty = typecheck_theorem_def ctx env theorem_def in
	 Types.end_definition () ;
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
