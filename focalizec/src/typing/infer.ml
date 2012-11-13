(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*                               LIP6  --  INRIA Rocquencourt                 *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(* $Id: infer.ml,v 1.210 2012-11-13 14:02:05 pessaux Exp $ *)



(* ********************************************************************* *)
(** {b Descr} : Exception used when the fusion algorithm (leading to the
    normal form of a species) find 2 computational fields with the same
    names but not the same ML type.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
exception Wrong_type_by_inheritance of
  (Location.t *         (** Location of the whole hosting species. *)
   Parsetree.vname *    (** Name of the faulty method. *)
   Types.type_simple *  (** First type found for the method. *)
   Types.type_simple *  (** Second type found for the method. *)
   Env.from_history *   (** History of the first occurrence of the method. *)
   Env.from_history)    (** History of the second occurrence of the method. *)
;;



(* ******************************************************************* *)
(** {b Descr} : Exception used when two properties or theorems must be
    merged because they have the same names but it appears that their
    statement could not be seen as the same. In effect, the meaning of
    "being the same" for types of computational methods, i.e. for ML
    types is clear, but for types of logical methods, i.e. for logical
    expressions, it is not clear and undecidable in general.

    Currently, the compiler compares the syntax trees, forgetting
    locations and modulo alpha-conversion to check if 2 logical
    statements are "equal".

    {b Exported} : Yes.                                                *)
(* ******************************************************************* *)
exception Logical_statements_mismatch of
  (Parsetree.vname *     (** Name of the involved theorem/property. *)
   Parsetree.qualified_species *  (** First hosting species. *)
   Location.t *   (** Source location of the first apparition. *)
   Parsetree.qualified_species *  (** Second hosting species. *)
   Location.t)  (** Source location of the second apparition. *)
;;



(* **************************************************************** *)
(** {b Descr} : Exception used to inform that a delayed proof for a
    property was found several time in the same species.

    {b Exported} : Yes.                                             *)
(* **************************************************************** *)
exception Proof_of_multiply_defined of
  (Location.t *   (** Location of one of the occurrence of "proof of". *)
   Parsetree.vname *   (** Name of the several time proved property. *)
   Location.t)   (** Location of the other occurrence of "proof of". *)
;;



(* ******************************************************************** *)
(** {b Descr} : Exception used to inform that a delayed proof was found
    but no property for this proof exist. Since, this a a proof of a
    non-existing property.

    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
exception Proof_of_unknown_property of
  (Location.t *   (** Location of the detected error. *)
   Parsetree.qualified_species *   (** Hosting species of the error. *)
   Parsetree.vname)            (** Name of the proved inexisting property. *)
;;



(* ********************************************************************** *)
(** {b Descr} : Exception used to inform that a sum type constructor was
    used with an incorrect arity. The correct expected arity is stored in
    the second argument of the exception constructor.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception Bad_sum_type_constructor_arity of
  (Parsetree.constructor_ident *     (** The name of the misused sum type
         constructor. *)
   Env.TypeInformation.constructor_arity (** The correct arity it should have
             be used. *)
  )
;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when a type variable was not found inside
    the [tyvars_mapping] of current [typing_context]. This means that while
    typechecking a type definition, its body contains a type variable not
    specified in the type parameters list.

    {b Exported} : Yes.                                                     *)
(* ************************************************************************ *)
exception Unbound_type_variable of
  (Location.t *        (* The location where the variable was found unbound. *)
   Parsetree.vname)    (** The name of the unbound variable. *)
;;



(* *********************************************************************** *)
(** {b Descr} : Exception raised when a method is defined several times in
    the same species.

    {b Exported} : Yes.                                                    *)
(* *********************************************************************** *)
exception Method_multiply_defined of
  (Parsetree.vname *                (** The method's name. *)
   Parsetree.qualified_species)    (** The species's name and module
       where the method is defined. *)
;;



(* ********************************************************************** *)
(** {b Descr} : Exception raised when a type constructor is applied to an
    incorrect number of type arguments.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception Bad_type_arity of
  (Location.t *
   Parsetree.ident *   (** The name of the misused type constructor. *)
   int *               (** The expected arity. *)
   int)                (** The arity the constructor was used with. *)
;;



(* *********************************************************************** *)
(** {b Descr} : Exception raised when "rep" is defined several times in
    the same species or when it is defined several time during the
    inheritance along a single inheritance path (do not deal with
    multiple "rep" due to multipel inheritance which is taken into account
    by the exception [Rep_multiply_defined_by_multiple_inheritance]).


    {b Exported} : Yes.                                                    *)
(* *********************************************************************** *)
exception Rep_multiply_defined of Location.t;;



(* ******************************************************************** *)
(** {b Descr} : Exception raised when "rep" is defined several times in
    the same species due to multiple inheritance
    by the exception [Rep_multiply_defined_by_multiple_inheritance]).


    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
exception Rep_multiply_defined_by_multiple_inheritance of
  (Types.type_simple *   (** Former type found for "rep". *)
   Types.type_simple *   (** Newer and incompatible type found for "rep". *)
   Location.t)  (** Location of the whole species where the "rep" was found
                    inconsistent. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was found
              with 2 incompatible types.

    {b Exported} : Yes.                                                *)
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
    with 2 types that can't be unified because one of them occurs in
    the other.

    {b Rem} : Note that I can't really see how this coudl happen...

    {b Exported} : Yes.                                                *)
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
    with 2 types that can't be unified because they do not respect the
    correct type constructor arity.

    {b Rem} : Note that I can't really see how this coudl happen...

    {b Exported} : Yes.                                                *)
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



(* ********************************************************************* *)
(** {b Descr} : During subspecies relation checking, a field was not
    found in the species signature that is considered as the subspecies.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
exception Not_subspecies_missing_field of
  ((** The collection name that should be a subspecies of the one below. *)
   Types.type_collection *
   (** The collection name that should be an overspecies of the one above. *)
   Types.type_collection *
   Parsetree.vname *     (** Field name that was not found. *)
   Env.from_history *    (** The hostory of the missing field in the species
                             that imposes the signature. *)
   Location.t)           (** Related location when the error occured. *)
;;



(* ******************************************************************* *)
(** {b Descr} : During a parameterized species application, the number
    of provided arguments is incorrect compared to the number of
    expected arguments.

    {b Exported} : Yes.                                                *)
(* ******************************************************************* *)
exception Parameterized_species_arity_mismatch of
  string    (** The message to insert in the error message: "many" or "few". *)
;;



(* ************************************************************************* *)
(** {b Descr} : Describes if a species can't be turned into a collection
    because a method is non-defined since it is a signature/property/missing
    "rep" or if it it due to a missing termination proof.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
type non_defined_method =
  | NDMK_prototype of Parsetree.vname
  | NDMK_termination_proof of Parsetree.vname
;;



(* *********************************************************************** *)
(** {b Descr} : During collection creation, it appears that the collection
    can't be created because at leat one field is only declared and not
    defined.

    {b Exported} : Yes.                                                    *)
(* *********************************************************************** *)
exception Collection_not_fully_defined of
  (Parsetree.qualified_species *  (** The incompletely defined species. *)
   (non_defined_method list))      (** The names and reasons of the fields
                                       missing an implementation. *)
;;


(* ************************************************************************** *)
(** {b Descr} : After generalization, a type scheme contains type variables.
    (either generalized or non generalized). Because in FoCaL methods are not
    even polymorphic, we of course also reject any scheme where some
    polymorphic variable would remain. But because we also don't want to
    have non instanciated variables in a scheme (those like OCaml's "'_a") we
    also reject schemes with non-generalized variables. Hence, more globally,
    we reject schemes containing variables.

    {b Exported} : Yes.                                                       *)
(* ************************************************************************** *)
exception Scheme_contains_type_vars of (
  Parsetree.vname *   (** The method's name whose scheme contains variables. *)
  Types.type_scheme * (** The scheme containing the variables. *)
  Location.t)         (** Location where the issue arises in the source. *)
;;



(* ********************************************************************** *)
(** {b Descr} : Exception raised when a delayed termination proof is told
    to be related to a function that doesn't exist in the current species.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception No_function_for_termination_proof of (Location.t * Parsetree.vname)
;;



(* ********************************************************************** *)
(** {b Descr} : Exception raised when a delayed termination proof profile
    uses a parameter that doesn't exist in the original function.

    {b Exported} : Yes.                                                   *)
(* ********************************************************************** *)
exception Invalid_parameter_in_delayed_proof_termination of
  (Location.t * Parsetree.vname)
;;



(* ********************************************************************* *)
(** {b Descr} : Exception raised when a method or signature is redefined
    with another logical flag than the original one. This prevents from
    having a "logical let" and a "let" having the same name.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
exception No_mix_between_logical_defs of (Location.t * Parsetree.vname);;



(* ************************************************************************* *)
(** {b Descr} : Datastructure recording the various information required and
    propagated during the type inference. It is much more convenient to
    group the various flags and stuff needed than passing them all the time
    as arguments of each recursive call.
    This datastructure serves especially this purpose.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
type typing_context = {
  (** The name of the currently analysed compilation unit (i.e. the name
      of the file without extension and not capitalized). *)
  current_unit : Types.fname;
  (** The name of the current species if relevant. *)
  current_species : Parsetree.qualified_species option;
  (** Optional type Self is known to be equal to. *)
  self_manifest : Types.type_simple option;
  (** Mapping between 'variables [vname]s and the [simple_type] they are
      bound to. Used when creating a polymorphic type definition. *)
  tyvars_mapping : (Parsetree.vname * Types.type_simple) list
}
;;



(* ************************************************************************* *)
(* typing_context -> Env.Env.ScopeInformation.t ->                           *)
(*   Parsetree.type_expr -> Types.type_simple                                *)
(** {b Descr} : Translates a type expression into a [type_simple].
    Variable are translated according to the mapping found inside the current
    context. Hence, in case this function is used to create the body type of
    a type definition, if the definition is polymorphic, the
    parameter-variables must already exist inside the mapping.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let rec typecheck_type_expr ctx env ty_expr =
  let final_ty =
    (match ty_expr.Parsetree.ast_desc with
     | Parsetree.TE_ident ident ->
         (begin
         let ident_ty =
           match ident.Parsetree.ast_desc with
            | Parsetree.I_local ((Parsetree.Vqident _) as variable_qname) ->
                (begin
                (* Just handle the special where [ident] is a type variable. *)
                try List.assoc variable_qname ctx.tyvars_mapping
                with Not_found ->
                  raise
                    (Unbound_type_variable
                       (ident.Parsetree.ast_loc, variable_qname))
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
                       (ty_expr.Parsetree.ast_loc, ident,
                        ty_descr.Env.TypeInformation.type_arity, 0))
                else
                  Types.specialize ty_descr.Env.TypeInformation.type_identity in
         (* Record the type in the AST node of the [ident]. *)
         ident.Parsetree.ast_type <- Parsetree.ANTI_type ident_ty;
         ident_ty
         end)
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
         (* Record in the AST node of the type of the [ty_cstr_ident]. *)
         ty_cstr_ident.Parsetree.ast_type <-
           Parsetree.ANTI_scheme ty_descr.Env.TypeInformation.type_identity;
         (* Check the type constructor's arity. *)
         let args_ty_len = List.length args_ty_exprs in
         if args_ty_len <> ty_descr.Env.TypeInformation.type_arity then
           raise
             (Bad_type_arity
                (ty_cstr_ident.Parsetree.ast_loc,
                 ty_cstr_ident, ty_descr.Env.TypeInformation.type_arity,
                 args_ty_len));
         (* Synthetise the types for the arguments. *)
         let args_ty = List.map (typecheck_type_expr ctx env) args_ty_exprs in
         (* Now get a fresh instance of the type's type scheme in which the
            parameters are directly instanciated by the effective arguments
            the sum type constructor is applied to. *)
         Types.specialize_with_args
           ty_descr.Env.TypeInformation.type_identity args_ty
         end)
     | Parsetree.TE_prod ty_exprs ->
         let tys = List.map (typecheck_type_expr ctx env) ty_exprs in
         Types.type_tuple tys
     | Parsetree.TE_self -> Types.type_self ()
     | Parsetree.TE_prop -> Types.type_prop ()
     | Parsetree.TE_paren inner -> typecheck_type_expr ctx env inner) in
  (* Store the type information in the expression's node. *)
  ty_expr.Parsetree.ast_type <- Parsetree.ANTI_type final_ty;
  final_ty
;;



(* ******************************************************************** *)
(* typing_context -> Env.Env.ScopeInformation.t ->                      *)
(*   Parsetree.rep_type_def -> Types.type_simple                        *)
(** {b Descr} : Translates a rep type expression into a [type_simple].
    This function behaves exactly like [typecheck_type_expr].
    Variable are translated according to the mapping found inside the
    current context. Hence, in case this function is used to create the
    body type of a type definition, if the definition is polymorphic,
    the parameter-variables must already exist inside the mapping.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let rec typecheck_rep_type_def ctx env rep_type_def =
  let final_ty =
    (match rep_type_def.Parsetree.ast_desc with
     | Parsetree.RTE_ident ident ->
         (begin
         let ident_ty =
           (match ident.Parsetree.ast_desc with
            | Parsetree.I_local ((Parsetree.Vqident _) as variable_qname) ->
                (begin
                (* Just handle the special where [ident] is a type variable.
                   Note that for a "rep" type because polymorphism is not
                   allowed, this should never be used. We could safely raise an
                   error. But, in order to keep this function clean and smooth,
                   we will enable this and perform a non-polymorphism test
                   later on the obtained type. Anyway this kind of test is not
                   only required for "rep", but also for all the other methods
                   (they can't either be polymorphic). *)
                try List.assoc variable_qname ctx.tyvars_mapping
                with Not_found ->
                  raise
                    (Unbound_type_variable
                       (ident.Parsetree.ast_loc, variable_qname))
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
                       (ident.Parsetree.ast_loc, ident,
                        ty_descr.Env.TypeInformation.type_arity, 0))
                else
                  Types.specialize
                    ty_descr.Env.TypeInformation.type_identity) in
         (* Record the type in the AST node of the [ident]. *)
         ident.Parsetree.ast_type <- Parsetree.ANTI_type ident_ty;
         ident_ty
         end)
     | Parsetree.RTE_fun (ty_expr1, ty_expr2) ->
         Types.type_arrow
           (typecheck_rep_type_def ctx env ty_expr1)
           (typecheck_rep_type_def ctx env ty_expr2)
     | Parsetree.RTE_app (ty_cstr_ident, args_ty_exprs) ->
         let ty_descr =
           Env.TypingEnv.find_type
             ~loc: ty_cstr_ident.Parsetree.ast_loc
             ~current_unit: ctx.current_unit ty_cstr_ident env in
         (* Check the type constructor's arity. *)
         let args_ty_len = List.length args_ty_exprs in
         if args_ty_len <> ty_descr.Env.TypeInformation.type_arity then
           raise
             (Bad_type_arity
                (ty_cstr_ident.Parsetree.ast_loc, ty_cstr_ident,
                 ty_descr.Env.TypeInformation.type_arity,
                 args_ty_len));
         (* Synthetise the types for the arguments. *)
         let args_ty =
           List.map (typecheck_rep_type_def ctx env) args_ty_exprs in
         (* Now get a fresh instance of the type's type scheme in which the
            parameters are directly instanciated by the effective arguments
            the sum type constructor is applied to. *)
         Types.specialize_with_args
           ty_descr.Env.TypeInformation.type_identity args_ty
     | Parsetree.RTE_prod (ty_exprs) ->
         let tys = List.map (typecheck_rep_type_def ctx env) ty_exprs in
         Types.type_tuple tys
     | Parsetree.RTE_paren inner -> typecheck_rep_type_def ctx env inner) in
  (* Store the type information in the expression's node. *)
  rep_type_def.Parsetree.ast_type <- Parsetree.ANTI_type final_ty;
  final_ty
;;



(* ************************************************************************* *)
(* Parsetree.type_expr list -> (Parsetree.vname * Types.type_simple) list    *)
(** {b Descr} : Create a fresh variable mapping automatically variables in
    the types as generalized. This is used when one creates a type structure
    from an external value's type expression.
    In effect, in such a context, variables in the type are implicitely
    considered as generalized because the type constraint annotating the
    external value does not show explicitly "forall-bound-variables".
    Hence, in an external value definition like:
      [external value foc_error : string -> 'a = ...]
    the ['a] must be considered as generalized, then when typechecking this
    definition the context must have a variable mapping where ['a] is known.
    Using the present function, one can build such a mapping.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let make_implicit_var_mapping_from_type_exprs type_expressions =
  let mapping = ref [] in
  let rec rec_make texpr =
    match texpr.Parsetree.ast_desc with
    | Parsetree.TE_ident ident ->
        (begin
         match ident.Parsetree.ast_desc with
         | Parsetree.I_local ((Parsetree.Vqident _) as variable_qname) ->
             (* Just handle the special case where the ident is a type
                variable. *)
             if not (List.mem_assoc variable_qname !mapping) then
               mapping := (variable_qname, Types.type_variable ()) :: !mapping
         | _ -> ()
         end)
    | Parsetree.TE_fun (ty_expr1, ty_expr2) ->
        rec_make ty_expr1;
        rec_make ty_expr2
    | Parsetree.TE_app (_, args_ty_exprs) -> List.iter rec_make args_ty_exprs
    | Parsetree.TE_prod ty_exprs -> List.iter rec_make ty_exprs
    | Parsetree.TE_self
    | Parsetree.TE_prop -> ()
    | Parsetree.TE_paren inner -> rec_make inner in
  (* **************** *)
  (* Now really work. *)
  mapping := [];
  List.iter rec_make type_expressions;
  !mapping
;;



(* ************************************************************************* *)
(* Parsetree.logical_expr -> (string * Types.type_simple) list               *)
(** {b Descr} : Create a fresh variable mapping automatically variables in
    the type parts of a [logical_expr] as generalized. This is used when one
    creates a type structure from a theorem expression.
    In effect, in such a context, variables in the type are implicitely
    considered as generalized because the type constraint annotating the
    theorem does not show explicitly "forall-bound-variables".
    Hence, in an theorem definition like:
      [theorem beq_refl : all x in 'a, ...]
    the ['a] must be considered as generalized, then when typechecking this
    definitionn the context must have a variable mapping where ['a] is known.
    Using the present function, one can build such a mapping.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let make_implicit_var_mapping_from_logical_expr logical_expr_expression =
  let mapping = ref [] in
  let rec rec_make pexpr =
    match pexpr.Parsetree.ast_desc with
    | Parsetree.Pr_forall (_, ty, logical_expr)
    | Parsetree.Pr_exists (_, ty, logical_expr) ->
        (* First recover the mapping induced by the type expression. *)
        let mapping_from_ty = make_implicit_var_mapping_from_type_exprs [ty] in
        (* Assuming the current mapping doesn't contain doubles, we extend it
           by the one got from the  type expression. *)
        mapping :=
          Handy.list_concat_uniq_custom_eq
            (fun (n, _) (n', _) -> n = n') mapping_from_ty !mapping;
        rec_make logical_expr
    | Parsetree.Pr_imply (logical_expr1, logical_expr2)
    | Parsetree.Pr_or (logical_expr1, logical_expr2)
    | Parsetree.Pr_and (logical_expr1, logical_expr2)
    | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
        rec_make logical_expr1;
        rec_make logical_expr2
    | Parsetree.Pr_not logical_expr
    | Parsetree.Pr_paren logical_expr -> rec_make logical_expr
    | Parsetree.Pr_expr _ ->
        (* Inside expressions type variable must be bound by the previous
           parts of the logical_expr ! Hence, do not continue searching
           inside. *)
        () in
  rec_make logical_expr_expression;
  !mapping
;;



(* ********************************************************************* *)
(* current_unit:Types.fname -> Env.TypingEnv.t -> Parsetree.expr -> bool *)
(** {b Descr} : If returns [true] then one can generalise the expression
    passed as argument.

    {b Exported} : No.                                                   *)
(* ********************************************************************* *)
let rec expr_is_non_expansive ~current_unit env expr =
  match expr.Parsetree.ast_desc with
  | Parsetree.E_const _ -> true
  | Parsetree.E_var _ -> true
  | Parsetree.E_let (let_def, body) ->
      List.for_all
        (fun binding ->
          let body = binding.Parsetree.ast_desc.Parsetree.b_body in
          (* Be careful. Consider the comment in the function
             [typecheck_let_definition] dealing with body hiding the
             functional aspect of the whole definition. *)
          binding.Parsetree.ast_desc.Parsetree.b_params <> []
          ||
          binding_body_is_non_expansive ~current_unit env body)
        let_def.Parsetree.ast_desc.Parsetree.ld_bindings
      &&
      expr_is_non_expansive ~current_unit env body
  | Parsetree.E_fun _ -> true
  | Parsetree.E_tuple exprs ->
      List.for_all (expr_is_non_expansive ~current_unit env) exprs
  | Parsetree.E_record lbl_exp_list ->
      List.for_all
        (fun (lbl, e) ->
          let lbl_descr =
            Env.TypingEnv.find_label
              ~loc: expr.Parsetree.ast_loc ~current_unit lbl env in
          lbl_descr.Env.TypeInformation.field_mut =
            Env.TypeInformation.FM_immutable &&
          expr_is_non_expansive ~current_unit env e)
        lbl_exp_list
  | Parsetree.E_record_access (e, _) ->
      expr_is_non_expansive ~current_unit env e
  | Parsetree.E_constr (_, exprs) ->
      List.for_all (expr_is_non_expansive ~current_unit env) exprs
  | Parsetree.E_paren e -> expr_is_non_expansive ~current_unit env e
  | Parsetree.E_external _ ->
      (* [Unsure]. Needed to make external functions generalized but should be
         guarded by something because in fact the real external definition
         may indeed by EXPANSIVE ! *)
      true
  | _ -> false



and logical_expr_is_non_expansive ~current_unit env logical_expr =
  match logical_expr.Parsetree.ast_desc with
   | Parsetree.Pr_imply (p1, p2) | Parsetree.Pr_or (p1, p2)
   | Parsetree.Pr_and (p1, p2) | Parsetree.Pr_equiv  (p1, p2) ->
       (logical_expr_is_non_expansive ~current_unit env p1) &&
       (logical_expr_is_non_expansive ~current_unit env p2)
   | Parsetree.Pr_expr e -> expr_is_non_expansive ~current_unit env e
   | Parsetree.Pr_not p | Parsetree.Pr_paren p
   | Parsetree.Pr_forall (_, _, p) | Parsetree.Pr_exists (_, _, p) ->
       logical_expr_is_non_expansive ~current_unit env p



and binding_body_is_non_expansive ~current_unit env = function
  | Parsetree.BB_computational e ->
      expr_is_non_expansive ~current_unit env e
  | Parsetree.BB_logical p ->
      logical_expr_is_non_expansive ~current_unit env p
;;



let find_function_by_name fct_vname fields =
  let rec rec_find = function
    | [] -> raise Not_found
    | h :: q ->
        match h with
         | Env.TypeInformation.SF_let (_, n, args, sch, _, _, _, _) ->
             if n = fct_vname then (args, sch) else rec_find q
         | Env.TypeInformation.SF_let_rec l ->
             (begin
             try
               let (_, _, args, sch, _, _, _, _) =
                 List.find (fun (_, n, _, _, _, _, _, _) -> n = fct_vname) l in
               (args, sch)
             with Not_found -> rec_find q
             end)
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
         | Env.TypeInformation.SF_property (_, _, _, _, _) -> rec_find q in
  rec_find fields
;;



(* ******************************************************************** *)
(* Parsetree.constant -> Types.type_simple                              *)
(** {b Descr} : Infers the type of a constant. Records this type in the
              AST node and return the type.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let typecheck_constant constant_desc =
  let ty =
    match constant_desc.Parsetree.ast_desc with
    | Parsetree.C_int _ -> Types.type_int ()
    | Parsetree.C_float _ -> Types.type_float ()
    | Parsetree.C_bool _ -> Types.type_bool ()
    | Parsetree.C_string _ -> Types.type_string ()
    | Parsetree.C_char _ -> Types.type_char () in
  constant_desc.Parsetree.ast_type <- Parsetree.ANTI_type ty;
  ty
;;



(* ********************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.pattern ->             *)
(*   Types.type_simple * (Parsetree.vname * Types.type_scheme) list      *)
(** {b Descr} : Infers the type of a [pattern]. Records this type in the
    AST node and return the type and a list of bindings that map the
    variables found in the pattern and their types.

    {b Exported} : No.                                                   *)
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
     | Parsetree.P_constr (cstr_name, pats) ->
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
          | (nempty_pats, Env.TypeInformation.CA_some) ->
              let cstr_ty =
                Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
              (* Do not [fold_left] otherwise types of the arguments will be
                 reversed ! *)
              let (pre_cstr_arg_tys, sub_bindings) =
                List.fold_right
                  (fun pat (accu_tys, accu_binds) ->
                    let (ty, binds) = typecheck_pattern ctx env pat in
                    ((ty :: accu_tys), (binds @ accu_binds)))
                  nempty_pats
                  ([], []) in
              (* Embed the types in a [ST_sum_arguments]. *)
              let cstr_arg_ty = Types.type_sum_arguments pre_cstr_arg_tys in
              (* Constructeurs being functions, we will unify [cstr_type] with
                 an arrow type to ensure that it is really one and to ensure
                 the arguments types and extract the result type. *)
              let unified_cstr_ty =
                Types.unify
                  ~loc: pat_desc.Parsetree.ast_loc
                  ~self_manifest: ctx.self_manifest
                  (Types.type_arrow cstr_arg_ty (Types.type_variable ()))
                  cstr_ty in
              ((Types.extract_fun_ty_result
                  ~self_manifest: ctx.self_manifest unified_cstr_ty),
               sub_bindings)
          | (_, _) ->
              (* Just raise the exception with the right expected arity. *)
              raise
                (Bad_sum_type_constructor_arity
                   (cstr_name, cstr_decl.Env.TypeInformation.cstr_arity)))
     | Parsetree.P_record label_n_patterns ->
         (* Type for this pattern. Will be instanciated by side effect. *)
         let whole_pat_ty = ref (Types.type_variable ()) in
         (* Infer type of eack sub-pattern. *)
         let infer_sub_pat (lbl, pat) =
           (* Get bindings and sub-pattern type. *)
           let (sub_pat_ty, bnds) = typecheck_pattern ctx env pat in
           let lbl_desc =
             Env.TypingEnv.find_label
               ~loc: pat.Parsetree.ast_loc
               ~current_unit: ctx.current_unit lbl env in
           (* Get the related label type. *)
           let lbl_ty =
             Types.specialize
               lbl_desc.Env.TypeInformation.field_scheme in
           (* Now, ensure the 2 sub-patterns types are compatible and that the
              resulting record type for the whole pattern is consistent. *)
           let unified_field_ty =
             Types.unify
               ~loc: pat.Parsetree.ast_loc
               ~self_manifest: ctx.self_manifest lbl_ty
               (Types.type_arrow sub_pat_ty !whole_pat_ty) in
           whole_pat_ty :=
             Types.extract_fun_ty_result
               ~self_manifest: ctx.self_manifest unified_field_ty;
           (* Just returns the bindings. *)
           bnds in
         let bindings =
           List.flatten
             (List.map infer_sub_pat label_n_patterns) in
         (!whole_pat_ty, bindings)
     | Parsetree.P_tuple pats ->
         let (tys, bindings) =
           List.split (List.map (typecheck_pattern ctx env) pats) in
         let ty = Types.type_tuple tys in
         (ty, (List.flatten bindings))
     | Parsetree.P_paren pat -> typecheck_pattern ctx env pat) in
  (* And now, store the type information inside the node. *)
  pat_desc.Parsetree.ast_type <- Parsetree.ANTI_type final_ty;
  (* Return both infered type and bindings. *)
  (final_ty, bindings)
;;





(* Does not make any assumption. Crudely returns a fresh type variable. *)
let typecheck_external_expr ctx env ext_expr =
  let t_expr = ext_expr.Parsetree.ast_desc.Parsetree.ee_internal in
  let vmap' = make_implicit_var_mapping_from_type_exprs [t_expr] in
  let ctx_with_tv_vars_constraints = {
    ctx with tyvars_mapping = vmap' @ ctx.tyvars_mapping } in
  let ty = typecheck_type_expr ctx_with_tv_vars_constraints env t_expr in
  ext_expr.Parsetree.ast_type <- Parsetree.ANTI_type ty;
  ty
;;



(* ************************************************************************ *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.fact -> unit*             *)
(** {b Descr} : Typechecks a [fact]. Because the type of a fact is not
    relevant in FoCaL, this function does not returns any type.
    Its only goal is to verify the type of the AST-sub-expressions where it
    is relevant and to screw this type in the [ast_type] field of these
    AST-nodes (i.e especially throug [expr_ident]s.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let typecheck_fact ctx env fact =
  (* No relevant type information to insert in the AST node. *)
  fact.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  match fact.Parsetree.ast_desc with
   | Parsetree.F_definition expr_idents
   | Parsetree.F_property expr_idents ->
       let current_species_name =
         (match ctx.current_species with
          | None -> None
          | Some (_, n) -> Some (Parsetree_utils.name_of_vname n)) in
       List.iter
         (fun expr_ident ->
           let var_scheme =
             Env.TypingEnv.find_value
               ~loc: expr_ident.Parsetree.ast_loc
               ~current_unit: ctx.current_unit ~current_species_name
               expr_ident env in
           let ident_ty = Types.specialize var_scheme in
           (* Record the ident's type in the AST node. *)
           expr_ident.Parsetree.ast_type <- Parsetree.ANTI_type ident_ty)
         expr_idents
   | Parsetree.F_hypothesis _ | Parsetree.F_node _ | Parsetree.F_type _ -> ()
;;



(* ***************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->      *)
(*   Env.TypeInformation.species_field list ->                       *)
(*     Env.TypeInformation.species_field list                        *)
(* {b Descr} : Checks if the 2 lists of fields contain methods
   names that overlap. If so then raises en exception
   [Method_multiply_defined], else returns le concatenation of the 2
   lists (first @ second)

   {b Exported} : No.                                                *)
(* ***************************************************************** *)
let append_and_ensure_method_uniquely_defined current_species l1 l2 =
  (* Just a local flattening function... *)
  let local_flat_fields fields =
    List.fold_right
      (fun field accu ->
        match field with
        | Env.TypeInformation.SF_sig (_, v, _)
        | Env.TypeInformation.SF_let (_, v, _, _, _, _, _, _)
        | Env.TypeInformation.SF_theorem (_, v, _, _, _, _)
        | Env.TypeInformation.SF_property (_, v, _, _, _) -> v :: accu
        | Env.TypeInformation.SF_let_rec l ->
            let l' = List.map (fun (_, v, _, _, _, _, _, _) -> v) l in
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
    flat_l1;
  (* Return the concatenation of the 2 lists if nodoubles have been found. *)
  l1 @ l2
;;



(* ************************************************************************ *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.expr -> Types.type_simple *)
(** {b Descr} : Infers the type o an [expr] and assign it by side effect in
    the [ast_type] field of the [expr] node.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let rec typecheck_expr ctx env initial_expr =
  let current_species_name =
    (match ctx.current_species with
     | None -> None
     | Some (_, n) -> Some (Parsetree_utils.name_of_vname n)) in
  let final_ty =
    (match initial_expr.Parsetree.ast_desc with
     | Parsetree.E_self -> Types.type_self ()
     | Parsetree.E_const cst -> typecheck_constant cst
     | Parsetree.E_fun (arg_vnames, e_body) ->
         (* Create the type for each argument .*)
         let args_ty = List.map (fun _ -> Types.type_variable ()) arg_vnames in
         (* Build the environment extended by these arguments and types
            Preferably use a fold_left instead of a fold_right, this way
            arguments are "cons-ed" in the environment in their order of
            declaration. Because arguments can't depend on each other, that's
            not a big matter, but that's cleaner... *)
         let extended_env =
           List.fold_left2
             (fun accu_env arg_name arg_ty ->
               (* Do not generalize argument types ! No Mu-rule yet ! *)
               Env.TypingEnv.add_value
                 ~toplevel: None arg_name (Types.trivial_scheme arg_ty)
                 accu_env)
             env arg_vnames args_ty in
         (* Now, typecheck the body i nthe new environment. *)
         let ty_body = typecheck_expr ctx extended_env e_body in
         (* Remains to build the final functional type. And do not
            [fold_left], otherwise you'll get a mirrored type ! *)
         List.fold_right
           (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
           args_ty ty_body
     | Parsetree.E_var ident ->
         (* E_var is never "self" because "self" is a dedicated case
            Now, don't bother with the search order, this has already be done
            by both the scoping and the environment build process. As reminder,
            lookup will naturally find the ident among local identifiers,
            IN-params, IS-params, inheritance and finally global identifiers. *)
         let var_scheme =
           Env.TypingEnv.find_value
             ~loc: ident.Parsetree.ast_loc
             ~current_unit: ctx.current_unit ~current_species_name ident env in
         let ident_ty = Types.specialize var_scheme in
         (* Record the [ident]'s type in its AST node. *)
         ident.Parsetree.ast_type <- Parsetree.ANTI_type ident_ty;
         ident_ty
     | Parsetree.E_app (functional_expr, args_exprs) ->
         let fun_ty = typecheck_expr ctx env functional_expr in
         let ty_exprs = List.map (typecheck_expr ctx env) args_exprs in
         List.fold_left
           (fun accu_fun_ty arg_ty ->
            (* Temporary functionnal type to unify with the type of the
               current applicator. *)
            let tmp_fun_ty =
              Types.type_arrow arg_ty (Types.type_variable ()) in
            let unified_fun_ty =
              Types.unify
                ~loc: initial_expr.Parsetree.ast_loc
                ~self_manifest: ctx.self_manifest tmp_fun_ty accu_fun_ty in
            (* The result is the positive part of the arrow. *)
            Types.extract_fun_ty_result
              ~self_manifest: ctx.self_manifest unified_fun_ty)
           fun_ty
           ty_exprs
     | Parsetree.E_constr (cstr_ident, exprs) ->
         let cstr_decl =
           Env.TypingEnv.find_constructor
             ~loc: cstr_ident.Parsetree.ast_loc
             ~current_unit: ctx.current_unit cstr_ident env in
         (match (exprs, cstr_decl.Env.TypeInformation.cstr_arity) with
          | ([], Env.TypeInformation.CA_zero) ->
              (* Just get an instance of the constructor's type scheme. *)
              let ty =
                Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
              (* Record the type in the AST node of the [cstr_ident]. *)
              cstr_ident.Parsetree.ast_type <- Parsetree.ANTI_type ty;
              ty
          | (_, Env.TypeInformation.CA_some) ->
              (* The constructor must be viewed as a function. *)
              let tys = List.map (typecheck_expr ctx env) exprs in
              (* Get an instance of the constructor's type scheme. *)
              let cstr_ty =
                Types.specialize cstr_decl.Env.TypeInformation.cstr_scheme in
              (* Record the type in the AST node of the [cstr_ident]. *)
              cstr_ident.Parsetree.ast_type <- Parsetree.ANTI_type cstr_ty;
              (* Build the shadow [ST_sum_arguments] type as the real argument
                 of the constructor. *)
              let cstr_arg_ty = Types.type_sum_arguments tys in
              (* And simulate an application. *)
              let unified_cstr_ty =
                Types.unify
                  ~loc: initial_expr.Parsetree.ast_loc
                  ~self_manifest: ctx.self_manifest
                  cstr_ty
                  (Types.type_arrow cstr_arg_ty (Types.type_variable ())) in
              Types.extract_fun_ty_result
                ~self_manifest: ctx.self_manifest unified_cstr_ty
          | (_, _) ->
              raise
                (Bad_sum_type_constructor_arity
                   (cstr_ident, cstr_decl.Env.TypeInformation.cstr_arity)))
     | Parsetree.E_match (matched_expr, bindings) ->
         let matched_expr_ty = ref (typecheck_expr ctx env matched_expr) in
         (* Let's get a fresh type accumulator to unify the clauses' bodies
            types. *)
         let result_ty = ref (Types.type_variable ()) in
         (* Process each clause of the match. *)
         List.iter
           (fun (pat, expr) ->
             (* Infer the type and bindings induced by the pattern. *)
             let (pat_ty, bnds) = typecheck_pattern ctx env pat in
             (* Ensure the matched expression and the pattern have the same
                type always keep the type where Self is prefered. *)
             matched_expr_ty :=
               Types.unify
                 ~loc: initial_expr.Parsetree.ast_loc
                 ~self_manifest: ctx.self_manifest !matched_expr_ty pat_ty;
             (* Extend the environment with the pattern type bindings. *)
             let env' =
               List.fold_left
                 (fun accu_env (id, ty_scheme) ->
                   Env.TypingEnv.add_value
                     ~toplevel: None id ty_scheme accu_env)
                 env bnds in
             (* Infer the type of the match clause's body. *)
             let clause_ty = typecheck_expr ctx env' expr in
             (* Force every bodies to have the same result type  *)
             (* and always keep the type where Self is prefered. *)
             result_ty :=
               Types.unify
                 ~loc: initial_expr.Parsetree.ast_loc
                 ~self_manifest: ctx.self_manifest !result_ty clause_ty)
           bindings;
         (* Return the type of the bodies' clauses. *)
         !result_ty
     | Parsetree.E_if (e_cond, e_then, e_else) ->
         let ty_cond = typecheck_expr ctx env e_cond in
         (* Ensure the condition is a boolean. *)
         ignore
           (Types.unify
              ~loc: initial_expr.Parsetree.ast_loc
              ~self_manifest: ctx.self_manifest ty_cond (Types.type_bool ()));
         (* Typecheck the "then" expression. *)
         let ty_then = typecheck_expr ctx env e_then in
         let ty_else = typecheck_expr ctx env e_else in
         (* Enforce both branches to have the same type. *)
         Types.unify
           ~loc: initial_expr.Parsetree.ast_loc
           ~self_manifest: ctx.self_manifest ty_then ty_else
     | Parsetree.E_let (let_def, in_expr) ->
         (* Don't increase level, this will be done in the let inference. *)
         let bindings =
           typecheck_let_definition ~is_a_field: false ctx env let_def in
         (* Let's build the environment for typing the ending expression. *)
         let env' =
           List.fold_left
             (fun accu_env (id, ty_scheme, _) ->
               Env.TypingEnv.add_value ~toplevel: None id ty_scheme accu_env)
             env bindings in
         typecheck_expr ctx env' in_expr
     | Parsetree.E_record fields -> typeckeck_record_expr ctx env fields None
     | Parsetree.E_record_access (expr, label) ->
         let ty_expr = typecheck_expr ctx env expr in
         let label_desc =
           Env.TypingEnv.find_label
             ~loc: initial_expr.Parsetree.ast_loc
             ~current_unit: ctx.current_unit label env in
         (* Just remind that labels are types as functions of type "type of
            the field as seen by user -> type od the record". *)
         let label_ty =
           Types.specialize label_desc.Env.TypeInformation.field_scheme in
         let unified_label_ty =
           Types.unify
             ~loc: initial_expr.Parsetree.ast_loc
             ~self_manifest: ctx.self_manifest
             (Types.type_arrow (Types.type_variable ()) ty_expr) label_ty in
         Types.extract_fun_ty_arg
            ~self_manifest: ctx.self_manifest unified_label_ty
     | Parsetree.E_record_with (with_expr, fields) ->
         typeckeck_record_expr ctx env fields (Some with_expr)
     | Parsetree.E_tuple exprs ->
         assert (exprs <> []);  (* Just in case. O-ary tuple is non-sense ! *)
         let tys = List.map (typecheck_expr ctx env) exprs in
         Types.type_tuple tys
     | Parsetree.E_sequence exprs ->
         let rec typecheck_sequence = function
         | [] -> Types.type_unit ()
         | [expr] -> typecheck_expr ctx env expr
         | expr :: exprs ->
           let ty_expr = typecheck_expr ctx env expr in
           (* Ensure the condition is a boolean. *)
           ignore
             (Types.unify
                ~loc: initial_expr.Parsetree.ast_loc
                ~self_manifest: ctx.self_manifest
                ty_expr (Types.type_unit ()));
           typecheck_sequence exprs in
         typecheck_sequence exprs
     | Parsetree.E_external ext_expr ->
         typecheck_external_expr ctx env ext_expr
     | Parsetree.E_paren expr -> typecheck_expr ctx env expr) in
  (* Store the type information in the expression's node. *)
  initial_expr.Parsetree.ast_type <- Parsetree.ANTI_type final_ty;
  (* Check if the expression has type Self and set the flag according to. *)
  Types.check_for_decl_dep_on_self final_ty;
  final_ty



(* ************************************************************************** *)
(* Env.TypingEnv.t -> (Types.label_name * Parsetree.expr) list ->             *)
(*   Parsetree.expr option -> Types.type_simple                               *)
(** {b Descr} : Performs type inference on record expressions with or
    without "with" clause. Currently, the labels exhaustivity is not checked.
    It has to be done when there is no "with" clause.

    {b Args} :
      - env : The current typing environment.
      - fields : The list of fields values of the record expression.
      - opt_with_expr : The optional "with" clause.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
and typeckeck_record_expr ctx env fields opt_with_expr =
  (* At then end, must be the type of the host of all these labels. *)
  let result_ty = ref (Types.type_variable ()) in
  (* Typecheck the "with" construct if any. *)
  (match opt_with_expr with
   | None ->
       (* To disapear once implemented ! *)
       Format.eprintf "Labels exhaustivity not checked on record expression.@\n"
   | Some expr ->
       let expr_ty = typecheck_expr ctx env expr in
       result_ty :=
         Types.unify
           ~loc: expr.Parsetree.ast_loc
           ~self_manifest: ctx.self_manifest expr_ty !result_ty);
  (* Now proceed with the labels.
     Just remind that labels are types as functions of type "type of the
     field as seen by user -> type od the record". *)
  List.iter
    (fun (label, expr) ->
      let expr_ty = typecheck_expr ctx env expr in
      let lbl_descr =
        Env.TypingEnv.find_label
          ~loc: expr.Parsetree.ast_loc
          ~current_unit: ctx.current_unit label env in
      (* Get the functionnal type of this field. *)
      let field_ty =
        Types.specialize lbl_descr.Env.TypeInformation.field_scheme in
      (* Record the type of the field in the AST node of the [label_ident]. *)
      label.Parsetree.ast_type <- Parsetree.ANTI_type field_ty;
      Types.check_for_decl_dep_on_self field_ty;
       (* Unify the result type by side effect. *)
      let unified_field_ty =
        Types.unify
          ~loc: expr.Parsetree.ast_loc ~self_manifest: ctx.self_manifest
          (Types.type_arrow expr_ty !result_ty) field_ty in
      result_ty :=
        Types.extract_fun_ty_result
          ~self_manifest: ctx.self_manifest unified_field_ty)
    fields;
  Types.check_for_decl_dep_on_self !result_ty;
  !result_ty



(* ************************************************************************** *)
(* is_a_field: bool -> typing_context -> Env.TypingEnv.t ->                   *)
(*   Parsetree.let_def -> (Parsetree.vname * Types.type_scheme *              *)
(*                         Env.TypeInformation.dependency_on_rep) list        *)
(** {b Descr} : Infers the list of bindings induced by the let-def and that
    will extend the current typing environment.
    Because methods cannot be polymorphic (c.f. Virgile Prevosto's Phd
    section 3.3, page 24) the parameter [~is_a_field] permits to know if we
    must generalize or not the let bound identifiers, and even more, if the
    scheme must be down-leveled to 0 or not. If [~is_a_field] is [true],
    then the bound identifiers must NOT be generalized and even more, in
    order to be never generalizable (for instance in another species where
    the use of this ident is unfortunately done at the same binding level)
    the scheme's body must be fully toggled with a level equal to 0 ! This
    way, the obtained scheme will not be polymorphic forever.
    This flag also enable to clear the flag set by unifications telling if a
    def-dependency on "rep" was found for a method (i.e. a species field).
    In case we are dealing with a field, we clear this flag to start a new
    search. In case we are in an nested expression, we continue to
    accumulate the searh's result by not clearing this flag.
    Hence the search result will be propagated to the surrounding expression.
    When this function is called from under a field definition, then this
    flag must obviously be [true].

    {b Exported} : No.                                                        *)
(* ************************************************************************** *)
and typecheck_let_definition ~is_a_field ctx env let_def =
  (* Create once for all the flag used to insert the let-bound idents in the
     environment. *)
  let toplevel_loc =
     if is_a_field then None else Some let_def.Parsetree.ast_loc in
  (* A [let_definition] doesn't really has a type. Record in the AST node. *)
  let_def.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  let let_def_descr = let_def.Parsetree.ast_desc in
  (* Get information to possibly build the pre-environment, i.e. the induced
     environment bindings between idents and types and if they can be
     generalised or not. *)
  let pre_env_info =
    List.map
      (fun { Parsetree.ast_desc = binding_desc } ->
        (* Just use a hack telling that if the expression won't be
           generalisable, to typecheck it, we don't change the generalisation
           level, then we won't generalise it.
           Becareful, functions are non_expansive. Because the structure of
           the let-def includes the parameters of the bound ident, aside its
           body, a let-def is not directly, structurally a function expression.
           So a fun like[let f (x) = body] would not be considered as
           non_expansive if [body] is not because [body] is not a functional
           expression: this is hidden because the arguments are recorded apart,
           in the [b_params] field. Hence, if the list [b_params] is not empty,
           then the bound expression *is* in fact a function and *is*
           non_expansive whatever the [body] is. *)
        if not is_a_field &&
           (binding_desc.Parsetree.b_params <> [] ||
            binding_body_is_non_expansive
              ~current_unit: ctx.current_unit env
              binding_desc.Parsetree.b_body) then
          (begin
          (* The body expression will be authorised to be generalized. *)
          Types.begin_definition ();
          let ty = Types.type_variable () in
          Types.end_definition ();
          (* Say "true" to mean "generalizable" (implies that a
             begin/end_definition have been performed). *)
          (binding_desc.Parsetree.b_name, ty, true)
          end)
        else
          (begin
          (* The body expression won't be authorised to be generalised. *)
          let ty = Types.type_variable () in
          (* Say "false" to mean "NON-generalisable" (implies that no
             begin/end_definition have been performed). *)
          (binding_desc.Parsetree.b_name, ty, false)
          end))
      let_def_descr.Parsetree.ld_bindings in
  (* Now let's address the bindings bodies ! If the definition is recursive
     then we extend the current environment with the assumed types. *)
  let env' =
    if let_def_descr.Parsetree.ld_rec = Parsetree.RF_no_rec then env
    else
      List.fold_left
        (fun accu_env (vname, ty, _) ->
          (* No generalisation (polymorphism) of the function inside its body
             (that's would be Mu-rule). *)
          let scheme = Types.trivial_scheme ty in
          Env.TypingEnv.add_value ~toplevel: toplevel_loc vname scheme accu_env)
        env pre_env_info in
  (* Now typecheck each def's body. *)
  let tmp_env_bindings =
    List.map2
      (fun binding (_, assumed_ty, non_expansive) ->
        (* Clear the status of whether def/decl-dependency on "rep" exist if
           the let-definition is a species field. Otherwise, keep if as it is
           in order to accumulate the information for the global surrounding
           expression. *)
        if is_a_field then Types.reset_deps_on_rep ();
        let binding_desc =  binding.Parsetree.ast_desc in
        let binding_loc = binding.Parsetree.ast_loc in
        (* Get all the type constraints from both the params and the body
           annotations of the definition. *)
        let all_ty_constraints =
          List.fold_left
            (fun accu (_, tye_opt) ->
              match tye_opt with
              | None -> accu
              | Some tye -> tye :: accu)
            (match binding_desc.Parsetree.b_type with
             | None -> []
             | Some tye -> [tye])
            binding_desc.Parsetree.b_params in
        (* Then, same stuff than for scoping, we add the type variables
           appearing in the type contraints to the current variable_mapping.
           These type variables are in effect implicitely generalized ! *)
        if non_expansive then Types.begin_definition ();
        let vmap' =
          make_implicit_var_mapping_from_type_exprs all_ty_constraints in
        if non_expansive then Types.end_definition ();
        let ctx_with_tv_vars_constraints = {
          ctx with tyvars_mapping = vmap' @ ctx.tyvars_mapping } in
        (* Build a type for the arguments of the bound identier if there are
           some. If they have type constraints, then use it as primary type
           instead of using a type variable that we should unify afterwards. *)
        if non_expansive then Types.begin_definition ();
        let args_tys =
          List.map
            (fun (_, opt_arg_ty_expr) ->
              match opt_arg_ty_expr with
               | None -> Types.type_variable ()
               | Some ty_expr ->
                   typecheck_type_expr ctx_with_tv_vars_constraints env ty_expr)
            binding_desc.Parsetree.b_params in
        if non_expansive then Types.end_definition ();
        (* Extend the current environment with the arguments of the bound
            identifier if there are some. *)
        let local_env =
          List.fold_left2
            (fun accu_env (arg_name, _) arg_ty ->
              Env.TypingEnv.add_value
                (* [~toplevel_let] = None because parameters are not at
                   toplevel. *)
                ~toplevel: None arg_name (Types.trivial_scheme arg_ty) accu_env)
                env'
            binding_desc.Parsetree.b_params
            args_tys in
        (* Same hack as above for the variables that must not be
           generalised. *)
        if non_expansive then Types.begin_definition ();
        (* Guess the body's type. *)
        let infered_body_ty =
          typecheck_binding_body
            ctx_with_tv_vars_constraints local_env
            binding_desc.Parsetree.b_body in
        (* If there is some constraint on this type, then unify with it.
           But anyway KEEP the constraint for type ! Unification is only
           there to ensure compatibility between the infered type and the
           proposed constraint ! *)
        let infered_body_ty_with_constraint =
          (match binding_desc.Parsetree.b_type with
           | None -> infered_body_ty
           | Some ty_expr ->
             let constraint_ty =
               typecheck_type_expr ctx_with_tv_vars_constraints env ty_expr in
             ignore
               (Types.unify
                  ~loc: ty_expr.Parsetree.ast_loc
                  ~self_manifest: ctx_with_tv_vars_constraints.self_manifest
                  constraint_ty infered_body_ty);
             (* As said above, KEEP the constraint as the final type ! *)
             constraint_ty) in
        (* Now, reconstruct the functional type from the body's and args'
           types. DO NOT fold_left, otherwise the fun type gets mirored !
           By the way, be careful to create the type arrow with the right
           binding level, and especially, not outside the binding level used
           to infer the body !!! That's why the end_definition is done AFTER
           having created the [complete_ty]. *)
        let pre_complete_ty =
          List.fold_right
            (fun arg_ty accu_ty -> Types.type_arrow arg_ty accu_ty)
            args_tys
            infered_body_ty_with_constraint in
        (* And now address bug #220 in which because a method already
           signature-d by inheritance had not its type constraint (already
           given) taken into account, a method using it didn't know that some
           part of the typechecking should involve Self instead of something
           else. For instance:
             species S1 = 
               representation = int ; 
               signature to_int : Self -> int ; 
               end
             species S2 = 
               inherit S1 ; 
               let to_int (x) : int = x ; 
               let join (x) = to_int (x) ; 
               end
           Typing join, we think that to_int has type int->int because fusion
           is not yet done for the current species. Hence join has type
           int -> int. And when fusion is done, to_int really gets type
           Self -> int but it's too late for join.
           If we had taken benefits of the knowledge of signature of to_int,
           it wouls have type Self->int, hence same thing for join.
           That's basically what we do here: if a scheme already exists in
           the environment (hence from inherited stuff), then we use it to
           enforce constraints on the type we just inferred for the current
           method. This is only relevant to *methods* !!! *)
        let complete_ty =
          if is_a_field then (
            try
              let current_species_name =
                (match ctx.current_species with
                | None -> None
                | Some (_, n) -> Some (Parsetree_utils.name_of_vname n)) in
              (* Temporarily make an ident from the method name to lookup in the
                 environment. *)
              let fake_ident = {
                Parsetree.ast_desc =
                  Parsetree.EI_local binding_desc.Parsetree.b_name ;
                Parsetree.ast_loc = binding_loc ;
                Parsetree.ast_annot = [] ;
                Parsetree.ast_type = Parsetree.ANTI_none } in
              let old_scheme =
                Env.TypingEnv.find_value
                  ~loc: binding_loc
                  ~current_unit: ctx.current_unit ~current_species_name
                  fake_ident env in
              let old_type = Types.specialize old_scheme in
              (* Force unification en keep the prefered type. *)
              Types.unify
                ~loc: binding_loc
                ~self_manifest: ctx_with_tv_vars_constraints.self_manifest
                pre_complete_ty old_type
            with
              (* No previous type found, keep the infered one. *)
              Env.Unbound_identifier (_, _) -> pre_complete_ty
           )
          else pre_complete_ty in (* Not a method: keep the inferred type. *)
        (* Now, return to the initial binding level if needed. *)
        if non_expansive then Types.end_definition () ;
        (* Unify the found type with the type that was temporarily assumed. *)
        Types.begin_definition () ;
        let final_ty =
          Types.unify
            ~loc: binding_loc
            ~self_manifest: ctx_with_tv_vars_constraints.self_manifest
            assumed_ty complete_ty in
        Types.end_definition ();
        (* Check for a decl dependency on "rep". *)
        Types.check_for_decl_dep_on_self final_ty;
        (* And finally returns the type binding induced by this definition. *)
        let ty_scheme =
          if non_expansive
          then Types.generalize final_ty
          else Types.trivial_scheme final_ty in
        (* Record the scheme in the AST node of the [binding]. *)
        binding.Parsetree.ast_type <- Parsetree.ANTI_scheme ty_scheme;
        (* Recover if a def-dependency or a decl-dependency on "rep" was/were
           found for this binding. *)
        let dep_on_rep = {
          Env.TypeInformation.dor_def = Types.get_def_dep_on_rep ();
          Env.TypeInformation.dor_decl = Types.get_decl_dep_on_rep () } in
        (binding_desc.Parsetree.b_name, ty_scheme, binding_loc,
         dep_on_rep))
      let_def_descr.Parsetree.ld_bindings
      pre_env_info in
  (* Typecheck the termination proof if any. *)
  (match let_def_descr.Parsetree.ld_termination_proof with
   | None -> ()
   | Some tp -> typecheck_termination_proof ctx env' tp) ;
  (* We make the clean environment binding by discarding the location
     information we kept just to be able to pinpoint accurately the guilty
     method in case of one would have variables in its scheme. *)
  let env_bindings =
    List.map
      (fun (name, sc, _, dep_on_rep) -> (name, sc, dep_on_rep))
      tmp_env_bindings in
  (* Finally, returns the induced bindings. Note that [Parsetree.binding]
     and [Parsetree.let_def have an [ast_type] but in this case it has no
     relevance, so we just leave them [None]. *)
  env_bindings



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.logical_expr ->            *)
(*   Types.type_simple                                                       *)
(** {b Descr} : Infers the type of a [logical_expr]. This type is always
      expected to be [Prop], hence this inference moslty verifies the right
      types usages inside a property and ensures that the final type is
      really [Prop].
      It finally assign the type by side effect in the [ast_type] field of
      the [logical_expr] node.
      This function takes into account the fact that that carrier "rep" must
      be considered as unknown to prevent def-dependencies (C.f. Virgile
      Prevosto's Phd, section 3.9.4 pages 51 & 52).
      ATTENTION : Because idents (bound by forall and exists) are
      **expressions** and are directly entered in the environment with the
      type logical_expr, the rule of Virgile telling that expressions must
      be of type bool is incorrect. In effect, because idents are expressions
      and are already types logical_expr, unifying them with bool wil always
      fail. Moreover, this fact may leak all around the expression type, then
      one cannot restrict the check to only say that an expression-ident
      typed logical_expr is correct.
      This may flood all around the proposition expression. Then, in case of
      an expression, one allows both logical_expr and bool as types.
      The [~in_proof] boolean enables to abstract Self only in case we infer
      a [logical_expr]'s type inside a property/theorem definition and not
      in it's proof !

    {b Exported} : No.                                                        *)
(* ************************************************************************** *)
and typecheck_logical_expr ~in_proof ctx env logical_expr =
  (* The local recursive function to save carying and changing the context. *)
  let final_ty =
    (match logical_expr.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, t_expr, pr)
     | Parsetree.Pr_exists (vnames, t_expr, pr) ->
         Types.begin_definition ();
         let ty = typecheck_type_expr ctx env t_expr in
         Types.end_definition ();
         (* ATTENTION ! Since the final type does not involves the type of the
            quantified variables, we must check here is there is a dependency
            on "Self" here otherwise we will miss it forever ! *)
         Types.check_for_decl_dep_on_self ty;
         (* Now typecheck the logical_expr's body in the extended environment.*)
         (* Note that as often, the order bindings are inserted in the        *)
         (* environment does not matter since parameters can never depends    *)
         (* on each other.                                                    *)
         let scheme = Types.trivial_scheme ty in
         let env' =
           List.fold_left
             (fun accu_env th_name ->
               Env.TypingEnv.add_value ~toplevel: None th_name scheme accu_env)
             env vnames in
         (* Fix the type scheme in the [t_expr]. But ATTENTION, generalize the
            scheme ! In effect, the scheme used to put in the environment is
            not generalized (no mu-rule), but the one that must be stored in
            the ast_desc for the ident bound by the "forall/exists" MUST be
            considered as generalized. In effect, since the type of variables
            do not appear in the logical_expr's type, when we generalize it,
            since variables are not inside...beh they are not generalized
            afterwards. Hence outside the body of the theorem, they remain non
            generealized and the Coq generation code can't see that it must add
            extra "forall ... : Set,". *)
         let generalized_scheme = Types.generalize ty in
         t_expr.Parsetree.ast_type <- Parsetree.ANTI_scheme generalized_scheme;
         (* And go on with the body... *)
         typecheck_logical_expr ~in_proof ctx env' pr
     | Parsetree.Pr_imply (pr1, pr2)
     | Parsetree.Pr_or (pr1, pr2)
     | Parsetree.Pr_and (pr1, pr2)
     | Parsetree.Pr_equiv (pr1, pr2) ->
         let ty1 = typecheck_logical_expr ~in_proof ctx env pr1 in
         let ty2 = typecheck_logical_expr ~in_proof ctx env pr2 in
         ignore
           (Types.unify
              ~loc: logical_expr.Parsetree.ast_loc
              ~self_manifest: ctx.self_manifest ty1 ty2);
         (* Enforce the type to be [logical_expr]. *)
         let final_ty =
           Types.unify
             ~loc: logical_expr.Parsetree.ast_loc
             ~self_manifest: ctx.self_manifest ty1 (Types.type_prop ()) in
         final_ty
     | Parsetree.Pr_not pr ->
         let ty = typecheck_logical_expr ~in_proof ctx env pr in
         (* Enforce the type to be [logical_expr]. *)
         let final_ty =
           Types.unify
             ~loc: logical_expr.Parsetree.ast_loc
             ~self_manifest: ctx.self_manifest ty (Types.type_prop ()) in
         final_ty
     | Parsetree.Pr_expr expr ->
         (* Make the carrier abstract to prevent def-dependencies with "rep"
            (c.f Virgile Prevosto's Phd page 52, Fig3.3) rule [EXPR] only when
            the current logical_expr appears in a theorem/property definition,
            not in its proof. *)
         let ctx' =
           if in_proof then ctx else { ctx with self_manifest = None } in
         (* Expressions must be typed as [bool] OR [logical_expr]. If  so, then
            the returned  type is [logical_expr]. *)
         let ty = typecheck_expr ctx' env expr in
         (try
           (* First try to check if it is typed bool. *)
           ignore
             (Types.unify
                ~loc: logical_expr.Parsetree.ast_loc
                ~self_manifest: ctx'.self_manifest ty (Types.type_bool ()))
          with err ->
           (begin
            try
             (* If not bool,try to check if it is typed logical_expr. *)
             ignore
               (Types.unify
                  ~loc: logical_expr.Parsetree.ast_loc
                  ~self_manifest: ctx'.self_manifest ty (Types.type_prop ()))
            with _ ->
             (* If it's neither bool nor logical_expr, then restore the fisrt
                error cause  for error report. *)
             raise err
           end));
         Types.type_prop ()
     | Parsetree.Pr_paren pr -> typecheck_logical_expr ~in_proof ctx env pr) in
  logical_expr.Parsetree.ast_type <- Parsetree.ANTI_type final_ty;
  Types.check_for_decl_dep_on_self final_ty;
  final_ty



and typecheck_binding_body ctx env = function
  | Parsetree.BB_logical p ->
      (* Because these logical_exprs only appear in a logical let, they are
         allowed to know Self representation (like in proofs) ! *)
      typecheck_logical_expr ctx env ~in_proof: true p
  | Parsetree.BB_computational e -> typecheck_expr ctx env e




(* ********************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.proof -> unit          *)
(** {b Descr} : Typechecks a [proof]. Because the type of a proof is not
    relevant in FoCaL, this function does not returns any type. Its only
    goal is to verify the type of the AST-sub-expressions where it is
    relevant and to screw this type in the [ast_type] field of these
    AST-nodes (i.e especially throug [statement]s.

    {b Exported} : No.                                                   *)
(* ********************************************************************* *)
and typecheck_proof ctx env proof =
  let current_species_name =
    (match ctx.current_species with
     | None -> None
     | Some (_, n) -> Some (Parsetree_utils.name_of_vname n)) in
  (* No relevant type information to insert in the AST node. *)
  proof.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  match proof.Parsetree.ast_desc with
  | Parsetree.Pf_assumed enf_deps | Parsetree.Pf_coq (enf_deps, _) ->
      (begin
      List.iter
        (fun enforced_dep ->
          match enforced_dep.Parsetree.ast_desc with
           | Parsetree.Ed_definition expr_idents
           | Parsetree.Ed_property expr_idents ->
               List.iter
                 (fun expr_ident ->
                   let var_scheme =
                     Env.TypingEnv.find_value
                       ~loc: expr_ident.Parsetree.ast_loc
                       ~current_unit: ctx.current_unit
                       ~current_species_name expr_ident env in
                   let ident_ty = Types.specialize var_scheme in
                   (* Record the ident's type in the AST node. *)
                   expr_ident.Parsetree.ast_type <-
                     Parsetree.ANTI_type ident_ty)
                 expr_idents)
        enf_deps
      end)
  | Parsetree.Pf_auto facts -> List.iter (typecheck_fact ctx env) facts
  | Parsetree.Pf_node nodes -> List.iter (typecheck_node ctx env) nodes



and typecheck_node ctx env node =
  (* No relevant type information to insert in the AST node. *)
  node.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  match node.Parsetree.ast_desc with
  | Parsetree.PN_sub (_, statement, proof) ->
      let env' = typecheck_statement ctx env statement in
      typecheck_proof ctx env' proof
  | Parsetree.PN_qed (_, proof) -> typecheck_proof ctx env proof



(* ******************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.statement -> unit     *)
(** {b Descr} : Typechecks a [statement]. Hypotheses are entered in the
    current environment before typechecking the optional [s_concl]
    proposition.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
and typecheck_statement ctx env statement =
  (* No relevant type information to insert in the AST node. *)
  statement.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  (* Do not fold_right otherwise the hypotheses will be processed in reverse
     order !!! *)
  let env' =
    List.fold_left
      (fun accu_env hyp ->
        let (name, ty) =
          (match hyp.Parsetree.ast_desc with
           | Parsetree.H_variable (vname, type_expr) ->
               (vname, (typecheck_type_expr ctx accu_env type_expr))
           | Parsetree.H_hypothesis (vname, logical_expr) ->
               (* Be careful, because we are not in a theorem/property
                  description, but in its proof, we must not make Self
                  abstract here ! *)
               (vname,
                (typecheck_logical_expr
                   ~in_proof: true ctx accu_env logical_expr))
           | Parsetree.H_notation (vname, expr) ->
               (vname, (typecheck_expr ctx accu_env expr))) in
        (* Record the type information in the AST node. *)
        hyp.Parsetree.ast_type <- Parsetree.ANTI_type ty;
        (* Extend the environment for the next hypotheses and finally to build
           the complete environment that will be used to typecheck the
           conclusion of the statement. *)
        let scheme = Types.generalize ty in
        Env.TypingEnv.add_value ~toplevel: None name scheme accu_env)
      env
      statement.Parsetree.ast_desc.Parsetree.s_hyps in
  (* Now, typecheck the conclusion, if some, in the extended environment. *)
  (match statement.Parsetree.ast_desc.Parsetree.s_concl with
   | None -> ()
   | Some logical_expr ->
       (* Same remark than above pour Self being not abstract ! *)
       ignore (typecheck_logical_expr ~in_proof: true ctx env' logical_expr));
  (* Return the environment extended by the possible idents the statement
     binds via its hypotheses. *)
  env'



(* ********************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.theorem_def ->          *)
(*  Types.type_simple                                                     *)
(** {b Descr } : Typechecks a theorem definition, records its type inside
    the AST node and returns this type.
    We also return the names of type variables found in the "forall" and
    "exists". They will lead to extra "forall ... : Set" in front of the
    theorem's logical expression.

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
and typecheck_theorem_def ctx env theorem_def =
  (* For the same reason that in external definition, variables present in a
     type expression in a theorem are implicitely considered as universally
     quantified. In effect, there no syntax to make explicit the
     quantification. Then we first create a variable mapping from the type
     expression to prevent variables from being "unbound".
     We must increase the bindind level because when processing [Pr_forall]
     and [Pr_exists], the function [typecheck_logical_expr] needs to store
     the scheme of the idents introduced. And since generalisation if done
     in [typecheck_logical_expr] with a binding level of + 1 compared to
     our current one, generalization could not be done otherwise. *)
  Types.begin_definition ();
  let vmapp =
    make_implicit_var_mapping_from_logical_expr
      theorem_def.Parsetree.ast_desc.Parsetree.th_stmt in
  Types.end_definition ();
  let ctx' = { ctx with tyvars_mapping = vmapp } in
  (* Ensure that Self will be abstract during the theorem's definition type
     inference by setting [~in_proof: false]. *)
  let ty =
    typecheck_logical_expr
      ~in_proof: false ctx' env
      theorem_def.Parsetree.ast_desc.Parsetree.th_stmt in
  (* Record the type information in the AST node. *)
  theorem_def.Parsetree.ast_type <- Parsetree.ANTI_type ty;
  (* Now, typecheck the proof to fix types inside by side effet. *)
  typecheck_proof ctx' env theorem_def.Parsetree.ast_desc.Parsetree.th_proof;
  (* And return the type of the stamement as type of the theorem. Also
     return the mapping of type variables onto their names. We reverse the
     list so that they appear in the same order they are declared by the
     "forall" and "exists". *)
  (ty, (List.rev vmapp))



(* *********************************************************************** *)
(** {b Descr} Typechecks a termination proof. This mostly consists in
    typechecking the inner of the proof, without returning any significant
    information. This only ensures that inner expressions are well-types
    and get their "type" bucket set.

    {b Exported} : Not.                                                    *)
(* *********************************************************************** *)
and typecheck_termination_proof ctx env tp =
  (* Anyway, a proof has no type... So... *)
  tp.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  match tp.Parsetree.ast_desc with
   | Parsetree.TP_structural _ ->
       (* Nothing to record because a [vname] doesn't have a "type" bucket. *)
       ()
   | Parsetree.TP_lexicographic facts ->
       List.iter (typecheck_fact ctx env) facts
   | Parsetree.TP_measure (expr, _, proof)
   | Parsetree.TP_order (expr, _, proof) ->
       (* Nothing to record for the [vname]s because *)
       (* they do not have any "type" bucket.        *)
       ignore (typecheck_expr ctx env expr);
       typecheck_proof ctx env proof



(** [previous_fields] is used to recover the methods previously typechecked
    in the species in order to ensure that the profile refers to an
    existing method of Self and that specified arguments really exist inside
    this method. *)
and typecheck_termination_proof_profile ctx env previous_fields profile =
  let current_species_name =
    (match ctx.current_species with
     | None -> None
     | Some (_, n) -> Some (Parsetree_utils.name_of_vname n)) in
  let profile_desc = profile.Parsetree.ast_desc in
  (* Get the name of the function the termination proof belongs to. *)
  let fct_vname = profile_desc.Parsetree.tpp_name in
  (* One must first search the function name in the environment. We embedd the
     [vname] inside a dummy [ident] in order to lookup. *)
  let fake_ident = {
    Parsetree.ast_desc = Parsetree.EI_local fct_vname;
    (* Roughly correction as a location, even is not exact. *)
    Parsetree.ast_loc = profile.Parsetree.ast_loc;
    Parsetree.ast_annot = [];
    Parsetree.ast_type = Parsetree.ANTI_none } in
  (* Since we have a [vname], we can't store in the AST any type information.
     however we still lookup in the environment to ensure the identifier is
     really defined. *)
  ignore
    (Env.TypingEnv.find_value
       ~loc: profile.Parsetree.ast_loc
       ~current_unit: ctx.current_unit ~current_species_name fake_ident env);
  (* Now, ensure the method really exist in the current species. *)
  try
    let (args_names, scheme) =
      find_function_by_name fct_vname previous_fields in
    (* Now, ensure that specified parameters really exist in the method and
       that they have the rigth type is specified. *)
    let (expected, _, _) =
      MiscHelpers.bind_parameters_to_types_from_type_scheme
        ~self_manifest: ctx.self_manifest (Some scheme) args_names in
    List.iter
      (fun (prof_param, prof_opt_ty) ->
        try
          (begin
          (* Because we provided a scheme, when we called the function
             [bind_parameters_to_types_from_type_scheme], we are sure to get
             a "Some". *)
          let expected_ty =
            (match List.assoc prof_param expected with
               Some t -> t | _ -> assert false) in
          match prof_opt_ty with
           | None -> ()
           | Some ty_expr ->
               let ty = typecheck_type_expr ctx env ty_expr in
               (* Now, unify the mentioned type whith the one found when we
                  typechecked the original function. *)
               ignore
                 (Types.unify
                    ~loc: profile.Parsetree.ast_loc
                    ~self_manifest: ctx.self_manifest expected_ty ty)
          end)
        with Not_found ->
          (* The argument doesn't exist among those of the function. *)
          raise
            (Invalid_parameter_in_delayed_proof_termination
               (profile.Parsetree.ast_loc, prof_param)))
      profile_desc.Parsetree.tpp_params
  with Not_found ->
    (* The function whose the termination proof belongs to was not found in
       the current species fields. *)
    raise
      (Scoping.Termination_proof_delayed_only_on_self_meth
        (profile.Parsetree.ast_loc, fct_vname))



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_field ->           *)
(*   ((Env.TypeInformation.species_field list) * (Parsetree.proof_def list)) *)
(** {b Descr} : Infers the types of the species fields contained in the
    list. The typing environment is incrementally extended with the found
    methods and used to typecheck the next methods.
    The function returns a 5-uplet whose 3 first components are suitable to
    be inserted in the structure of a species's type, and the last ones are
    the "proof-of" and "termination-proof-of" fields that have been found
    among the fields. These "proof-of" must be collapsed with their related
    property BUT at the inheritance level where the proof is found (not at
    the one where the property was enounced), to lead to a theorem before
    the normalization process starts.
    The "termination-proof-of" must be collapsed with their related
    "let-rec" definitions also before the normalization process starts.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
and typecheck_species_fields initial_ctx initial_env initial_fields =
  let rec rec_typecheck ctx env accu_fields accu_proofs accu_term_proofs =
    function
    | [] -> (accu_fields, ctx, accu_proofs, accu_term_proofs)
    | field :: rem_fields ->
        let current_species =
          (match ctx.current_species with None -> assert false | Some n -> n) in
        let (fields_tys, new_ctx, new_env, new_proofs, new_termination_proofs) =
          (begin
          match field.Parsetree.ast_desc with
           | Parsetree.SF_rep rep_type_def ->
               (begin
               let rep_vname = Parsetree.Vlident "rep" in
               (* On must not defined several rep inside a species. *)
               if ctx.self_manifest <> None then
                 raise (Method_multiply_defined (rep_vname, current_species));
               Types.begin_definition ();
               let ty = typecheck_rep_type_def ctx env rep_type_def in
               Types.end_definition ();
               (* Before modifying the context, just check that no "rep" was
                  previously identified. If some, then fails. *)
               if ctx.self_manifest <> None then
                 raise (Rep_multiply_defined field.Parsetree.ast_loc);
               (* Extend the context with the type "rep" is equal to. Beware
                  we make a copy of the infered type in order to keep the
                  originally infered type aside any further modifications that
                  could arise while unifying anywhere "Self" with "its known
                  representation". In effect, unification in place would
                  establish a link by side effect from the representation to
                  the type [Types.ST_self_rep], hence fooling the explicit
                  structure of what is initially "rep".
                  This first would prevent us from being able to generate code
                  finally relying on the representation of "rep". Furthermore,
                  because of how [Types.unify] handles unification with
                  [Types.ST_self_rep] to prevent cycles, unification of this
                  **mangled** representation would succeed with any types, even
                  those incompatible with the original **CORRECT**
                  representation of "rep"'s type ! *)
               let ctx' = {
                 ctx with
                   self_manifest =
                     Some
                       (Types.copy_type_simple_but_variables
                          ~and_abstract: None ty) } in
               (* Record the type information in the AST node with again a
                  separate copy so that Self's type that don't risk to be
                  unified somewhere, hence that will keep its effective
                  structure forever. *)
               field.Parsetree.ast_type <-
                 Parsetree.ANTI_type
                   (Types.copy_type_simple_but_variables
                      ~and_abstract: None ty);
               (* Be careful : methods are not polymorphic (c.f. Virgile
                  Prevosto's Phd section 3.3, page 24). No generalization ! *)
               let rep_scheme = Types.trivial_scheme ty in
               let field_info =
                 Env.TypeInformation.SF_sig
                  ((Env.intitial_inheritance_history current_species),
                  rep_vname, rep_scheme) in
               (* Record the "rep" scheme in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_scheme rep_scheme;
               ((append_and_ensure_method_uniquely_defined
                   current_species accu_fields [field_info]),
                 ctx', env, accu_proofs, accu_term_proofs)
               end)
           | Parsetree.SF_sig sig_def ->
               (begin
               let sig_def_descr = sig_def.Parsetree.ast_desc in
               Types.begin_definition ();
               let ty =
                 typecheck_type_expr ctx env sig_def_descr.Parsetree.sig_type in
               Types.end_definition ();
               (* Extend the environment with this new method of Self.
                  Be careful : methods are not polymorphics (c.f. Virgile
                  Prevosto's Phd section 3.3, page 24). No generelization ! *)
               let scheme = Types.trivial_scheme ty in
               let env' =
                 Env.TypingEnv.add_value
                   ~toplevel: None sig_def_descr.Parsetree.sig_name scheme
                   env in
               let field_info =
                 Env.TypeInformation.SF_sig
                   ((Env.intitial_inheritance_history current_species),
                     sig_def_descr.Parsetree.sig_name, scheme) in
               (* Record the type information in the AST nodes. *)
               sig_def.Parsetree.ast_type <- Parsetree.ANTI_scheme scheme;
               field.Parsetree.ast_type <- Parsetree.ANTI_scheme scheme;
               ((append_and_ensure_method_uniquely_defined
                  current_species accu_fields [field_info]),
                ctx, env', accu_proofs, accu_term_proofs)
               end)
           | Parsetree.SF_let let_def ->
               (begin
               (* No relevant type information to record in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
               (* Don't increase level, this will be done in the let
                  inference.
                  Be careful : methods are not polymorphics (c.f. Virgile
                  Prevosto's Phd section 3.3, page 24). No generelization ! *)
               let bindings =
                 typecheck_let_definition ~is_a_field: true ctx env let_def in
               (* Let's build the environment with the bindings for this let. *)
               let env' =
                 List.fold_left
                   (fun accu_env (id, ty_scheme, _) ->
                      Env.TypingEnv.add_value
                        ~toplevel: None id ty_scheme accu_env)
                   env bindings in
               (* Build the record now to put it later in the environment. *)
               let let_def_flags = {
                 Env.TypeInformation.ldf_recursive =
                   let_def.Parsetree.ast_desc.Parsetree.ld_rec;
                 Env.TypeInformation.ldf_logical =
                   let_def.Parsetree.ast_desc.Parsetree.ld_logical } in
               (* We now collect the type information of these methods in
                  order to make them suitable for a "type of method". *)
               match let_def.Parsetree.ast_desc.Parsetree.ld_rec with
                | Parsetree.RF_rec ->
                    (begin
                    let field_infos =
                      List.map2
                        (fun (id, ty_scheme, has_def_dep_on_rep) binding ->
                          let expr =
                            binding.Parsetree.ast_desc.Parsetree.b_body in
                          let params_names =
                            List.map
                              fst binding.Parsetree.ast_desc.Parsetree.b_params in
                          (* Note that [expr] below is already typed here. *)
                          ((Env.intitial_inheritance_history current_species),
                           id, params_names, ty_scheme, expr,
                           (* [Unsure] We assign each mutually recursive
                              function the same proof. This is not correct but
                              anyway, we only handle unique recursive
                              function"s". *)
                           let_def.Parsetree.ast_desc.Parsetree.ld_termination_proof,
                           has_def_dep_on_rep,
                           let_def_flags))
                        bindings
                        let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
                    (* Recursive, so just 1 field with several names. *)
                    ((append_and_ensure_method_uniquely_defined
                        current_species accu_fields
                        [(Env.TypeInformation.SF_let_rec field_infos)]),
                     ctx, env', accu_proofs, accu_term_proofs)
                    end)
                | Parsetree.RF_no_rec ->
                    (begin
                    (* Not recursive, then the list should be only 1 long.
                       Anyway, if that not the case, this does not annoy.
                       So we return a list of n fields with 1 name in each. *)
                    let field_infos =
                      List.map2
                        (fun (id, ty_scheme, has_def_dep_on_rep) binding ->
                          let expr =
                            binding.Parsetree.ast_desc.Parsetree.b_body in
                          let params_names =
                            List.map fst
                              binding.Parsetree.ast_desc.Parsetree.b_params in
                          (* Note that [expr] below is already typed here. *)
                          Env.TypeInformation.SF_let
                            ((Env.intitial_inheritance_history current_species),
                             id, params_names, ty_scheme, expr,
                             (* Same remark than above, but with the additionnal
                                stuff that non-recursive functions should anyway
                                not have proofs since it is useless. *)
                             let_def.Parsetree.ast_desc.Parsetree.ld_termination_proof,
                             has_def_dep_on_rep,
                             let_def_flags))
                        bindings
                        let_def.Parsetree.ast_desc.Parsetree.ld_bindings in
                    ((append_and_ensure_method_uniquely_defined
                        current_species accu_fields field_infos),
                     ctx, env', accu_proofs, accu_term_proofs)
                    end)
               end)
           | Parsetree.SF_property property_def ->
               (begin
               Types.reset_deps_on_rep ();
               Types.begin_definition ();
               (* For the same reason that in external definition, variables
                  present in a type expression in a property are implicitely
                  considered as universally quantified. In effect, there no
                  syntax to make explicit the quantification. Then we first
                  create a variable mapping from the type expression to prevent
                  variables from being "unbound".
                  We must increase the bindind level because when processing
                  [Pr_forall] and [Pr_exists], the function
                  [typecheck_logical_expr] needs to store the scheme of the
                  idents introduced. And since generalisation if done in
                  [typecheck_logical_expr] with a binding level of + 1
                  compared to our current one, generalization could not be
                  done otherwise. *)
               Types.begin_definition ();
               let vmapp =
                 make_implicit_var_mapping_from_logical_expr
                   property_def.Parsetree.ast_desc.Parsetree.prd_logical_expr in
               Types.end_definition ();
               let ctx' = { ctx with tyvars_mapping = vmapp } in
               (* We ensure that Self will be abstract during the property's
                  definition type inference by setting [~in_proof: false]. *)
               let ty =
                 typecheck_logical_expr
                   ~in_proof: false ctx' env
                   property_def.Parsetree.ast_desc.Parsetree.prd_logical_expr in
               Types.end_definition ();
               (* Check for a decl dependency on "rep". *)
               Types.check_for_decl_dep_on_self ty;
               (* Record the type information in the AST node. *)
               property_def.Parsetree.ast_type <- Parsetree.ANTI_type ty;
               (* Extend the environment.
                  Be careful : methods are not polymorphics (c.f. Virgile
                  Prevosto's Phd section 3.3, page 24). No generelization ! *)
               let scheme = Types.trivial_scheme ty in
               let env' =
                 Env.TypingEnv.add_value
                   ~toplevel: None
                   property_def.Parsetree.ast_desc.Parsetree.prd_name
                   scheme env in
               (* Recover if a def-dependency or a decl-dependency on "rep"
                  was/were found for this binding. *)
               let dep_on_rep = {
                 Env.TypeInformation.dor_def = Types.get_def_dep_on_rep ();
                 Env.TypeInformation.dor_decl = Types.get_decl_dep_on_rep ()} in
               let field_info =
                 Env.TypeInformation.SF_property
                   ((Env.intitial_inheritance_history current_species),
                    property_def.Parsetree.ast_desc.Parsetree.prd_name,
                    (* Remind the mapping of type variables onto their names.
                       We reverse the list so that they appear in the same order
                       they are declared by the "forall" and "exists". *)
                    (List.rev vmapp),
                    property_def.Parsetree.ast_desc.Parsetree.prd_logical_expr,
                    dep_on_rep) in
               (* Record the property's scheme in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_scheme scheme;
               ((append_and_ensure_method_uniquely_defined
                  current_species accu_fields [field_info]),
                 ctx, env', accu_proofs, accu_term_proofs)
               end)
           | Parsetree.SF_theorem theorem_def ->
               (begin
               Types.reset_deps_on_rep ();
               (* Get the theorem's type (trivially should be Prop) and the
                  names of type variables found in the "forall" and "exists".
                  They will lead to extra "forall ... : Set" in front of the
                  theorem's logical expression. *)
               let (ty, polymorphic_vars_map) =
                 typecheck_theorem_def ctx env theorem_def in
               (* Check for a decl dependency on "rep". *)
               Types.check_for_decl_dep_on_self ty;
               (* Be careful : methods are not polymorphics (c.f. Virgile
                  Prevosto's Phd section 3.3, page 24). No generelization !
                  Anyway, the type of a theorem IS Prop. *)
               let scheme = Types.trivial_scheme ty in
               (* Extend the environment. *)
               let env' =
                 Env.TypingEnv.add_value
                  ~toplevel: None
                  theorem_def.Parsetree.ast_desc.Parsetree.th_name scheme env in
               (* Recover if a def-dependency or a decl-dependency on "rep"
                  was/were found for this binding. *)
               let dep_on_rep = {
                 Env.TypeInformation.dor_def = Types.get_def_dep_on_rep ();
                 Env.TypeInformation.dor_decl = Types.get_decl_dep_on_rep ()
                 } in
               let field_info =
                 Env.TypeInformation.SF_theorem
                  ((Env.intitial_inheritance_history current_species),
                   theorem_def.Parsetree.ast_desc.Parsetree.th_name,
                   polymorphic_vars_map,
                   theorem_def.Parsetree.ast_desc.Parsetree.th_stmt,
                   theorem_def.Parsetree.ast_desc.Parsetree.th_proof,
                   dep_on_rep) in
               (* Record the theorem's scheme in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_scheme scheme;
               ((append_and_ensure_method_uniquely_defined
                   current_species accu_fields [field_info]),
                 ctx, env', accu_proofs, accu_term_proofs)
               end)
           | Parsetree.SF_proof proof_def ->
               (begin
               let proof_def_desc = proof_def.Parsetree.ast_desc in
               (* Because the type of a proof is not relevant in FoCaL,
                  the typechecking of a proof does not returns any type.
                  We record the non-relevance of the type in the AST node. *)
               proof_def.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
               (* Type-cheking of the proof may induce def/decl dependency on
                  "rep" ! *)
               Types.reset_deps_on_rep ();
               typecheck_proof ctx env proof_def_desc.Parsetree.pd_proof;
               let dep_on_rep = {
                 Env.TypeInformation.dor_def = Types.get_def_dep_on_rep ();
                 Env.TypeInformation.dor_decl = Types.get_decl_dep_on_rep ()} in
               (* No relevant type information to record in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
               (* No extension there. *)
               (accu_fields, ctx, env, accu_proofs @ [(proof_def, dep_on_rep)],
                accu_term_proofs)
               end)
           | Parsetree.SF_termination_proof termination_proof_def ->
               (begin
               let desc = termination_proof_def.Parsetree.ast_desc in
               (* Mostly ensure that specified methods exist and same for
                  their specified parameters names. *)
               List.iter
                  (typecheck_termination_proof_profile ctx env accu_fields)
                  desc.Parsetree.tpd_profiles;
               (* Type-cheking of the proof may induce def/decl dependency on
                  "rep" ! *)
               Types.reset_deps_on_rep ();
               (* Typecheck the inner proof. *)
               typecheck_termination_proof
                  ctx env desc.Parsetree.tpd_termination_proof;
               let dep_on_rep = {
                 Env.TypeInformation.dor_def = Types.get_def_dep_on_rep ();
                 Env.TypeInformation.dor_decl = Types.get_decl_dep_on_rep ()} in
               (* No relevant type information to record in the AST node. *)
               field.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
               (* No extension there. *)
               (accu_fields, ctx, env, accu_proofs,
                accu_term_proofs @ [(termination_proof_def, dep_on_rep)])
               end)
          end) in
        let (final_accu_fields, final_ctx, final_accu_proofs,
             final_termination_proofs) =
          rec_typecheck
            new_ctx new_env fields_tys new_proofs new_termination_proofs
            rem_fields in
        (* And finally the result... *)
        (final_accu_fields, final_ctx, final_accu_proofs,
         final_termination_proofs) in
    (* Now, really do the job... *)
    rec_typecheck initial_ctx initial_env [] [] [] initial_fields
;;



type typed_species_parameter_argument =
  | TSPA_self
  | TSPA_non_self of
      (Types.type_collection * Env.TypeInformation.species_description)
;;



(* ********************************************************************* *)
(* Env.TypingEnv.t -> Env.TypeInformation.species_param ->               *)
(*   (Parsetree.vname * species_parameter_info)                          *)
(** {b Descr}: Transforms an arbitrary expression [Parsetree.expr] used as
    species parameter expression into a [species_param_expr]. Hence, the
    invariant that the [Parsetree.expr] can be only a sum type value
    constructor (possibly enclosed by paren) disapears since we enforce
    this structurally in the type [species_param_expr].

    {b Exported}: No.                                                    *)
(* ********************************************************************* *)
let rec expr_to_species_param_expr ~current_unit env expr =
  match expr.Parsetree.ast_desc with
   | Parsetree.E_self -> Parsetree_utils.SPE_Self
   | Parsetree.E_constr (cstr_expr, []) ->
       (begin
       let Parsetree.CI glob_ident = cstr_expr.Parsetree.ast_desc in
       let qualified_vname =
         (match glob_ident.Parsetree.ast_desc with
          | Parsetree.I_local vname -> Parsetree.Vname vname
          | Parsetree.I_global qvn -> qvn) in
       let species =
         Env.TypingEnv.find_species
           ~loc: Location.none ~current_unit glob_ident env in
       Parsetree_utils.SPE_Species
         (qualified_vname, species.Env.TypeInformation.spe_kind)
       end)
   | Parsetree.E_paren expr ->
       expr_to_species_param_expr ~current_unit env expr
   | _ -> Parsetree_utils.SPE_Expr_entity expr
;;



(* ************************************************************************ *)
(** {b Descr}: Transforms the description of what is a species parameter into
    a simpler structure than the one used in [Env.TypeInformation]. In fact
    we want to keep trace of wether a parameter is "IS" or "IN" and what is
    its species. For this last point, we transform the arbitrary expression
    [Parsetree.expr] into a [species_param_expr] where the invariant that
    the [Parsetree.expr] can be only a sum type value constructor (possibly
    enclosed by paren) disapears since we enforce this structurally in the
    type [species_param_expr].

    {b Exported}: No.                                                       *)
(* ************************************************************************ *)
let species_expr_to_species_param_expr ~current_unit env species_expr =
  let species_expr_desc = species_expr.Parsetree.ast_desc in
  let params =
    List.map
      (fun se_param ->
        let Parsetree.SP expr = se_param.Parsetree.ast_desc in
        expr_to_species_param_expr ~current_unit env expr)
      species_expr_desc.Parsetree.se_params in
  { Parsetree_utils.sse_name = species_expr_desc.Parsetree.se_name;
    Parsetree_utils.sse_effective_args = params }
;;



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.expr ->                    *)
(*   (Types.type_collection * Env.TypeInformation.species_description)       *)
(** {b Descr} : Typechecks an expression in the restricted case where is it
    used as a "IS" or "IN" parameter effective argument. In this particular
    case, the rule [COLL-INST] expects a collection identifier and nothing
    else. For this reason, the identifier is looked-up in the species
    environment.
    Because the AST structure cannot know a priori (i.e at parsing stage) is
    the expression used as argument will be the one of a "IS" or a "IN"
    argument, the [expr] rule is sufficiently general to absorbe any
    possible expression, ... but is also too large. Hence we perfom this
    check afterward, during the typing stage.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let rec typecheck_expr_as_species_parameter_argument ctx env initial_expr =
  match initial_expr.Parsetree.ast_desc with
  | Parsetree.E_self ->
      Format.eprintf
        "@[%tWarning:%t@ Self@ is@ subspecies@ by recursion@ not@ \
         @ checked:@ focalizec@ compiler@ TODO.@]@."
        Handy.pp_set_bold Handy.pp_reset_effects ;
      TSPA_self
  | Parsetree.E_constr (cstr_expr, []) ->
      (* We re-construct a fake ident from the constructor expression just to
         be able to lookup inside the environment. Be careful, in order to
         allow to acces "opened" species, we must check if  we have a
         qualification (even being [None]) to create either a GLOBAL or a
         LOCAL ident. If we don't have any module qualification, we must
         create a LOCAL ident otherwsise, [~allow_opened] in [find_species]
         will get stucked at [false] and we won't see the opended species.
         Otherwise, we create a GLOBAL identifier. *)
      let Parsetree.CI glob_ident = cstr_expr.Parsetree.ast_desc in
      let descr =
        Env.TypingEnv.find_species
          ~loc: glob_ident.Parsetree.ast_loc
          ~current_unit: ctx.current_unit glob_ident env in
      let (id_effective_module, id_name_as_string) =
        (match glob_ident.Parsetree.ast_desc with
         | Parsetree.I_local vname
         | Parsetree.I_global (Parsetree.Vname vname) ->
             (ctx.current_unit, (Parsetree_utils.name_of_vname vname))
         | Parsetree.I_global (Parsetree.Qualified (n, vname)) ->
             (n, (Parsetree_utils.name_of_vname  vname))) in
      (* Record the type in the AST nodes. *)
      let ty =
        Types.type_rep_species
          ~species_module: id_effective_module
          ~species_name: id_name_as_string in
      initial_expr.Parsetree.ast_type <- Parsetree.ANTI_type ty;
      cstr_expr.Parsetree.ast_type <- Parsetree.ANTI_type ty;
      (* We return the "collection type", and the collection's description. *)
      TSPA_non_self ((id_effective_module, id_name_as_string), descr)
  | Parsetree.E_paren expr ->
      (begin
      let typed_expr =
        typecheck_expr_as_species_parameter_argument ctx env expr in
      let ty =
        (match typed_expr with
         | TSPA_non_self ((mod_name, species_name), _) ->
             Types.type_rep_species
               ~species_module: mod_name ~species_name: species_name
         | TSPA_self -> Types.type_self ()) in
      (* Record the type in the AST node. *)
      initial_expr.Parsetree.ast_type <- Parsetree.ANTI_type ty;
      typed_expr
      end)
  | _ ->
      (* Should be always caught before, at scoping phase. *)
      raise
        (Scoping.Is_parameter_only_coll_ident initial_expr.Parsetree.ast_loc)
;;



(* ************************************************************************ *)
(* current_unit: Types.fname -> Types.type_collection ->                    *)
(*   Env.TypeInformation.species_field list ->                              *)
(*     Env.TypeInformation.species_field list                               *)
(** {b Descr} : Perform the abstraction of species methods. This implements
    the "A" function described in Virgile Prevosto's Phd, section 3.8, page
    41.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let abstraction ~current_unit cname fields =
  let rec rec_abstract = function
    | [] -> []
    | h :: q ->
        let h' =
          (match h with
           | Env.TypeInformation.SF_sig (from, vname, scheme)
           | Env.TypeInformation.SF_let (from, vname, _, scheme, _, _, _, _) ->
               Types.begin_definition ();
               let ty = Types.specialize scheme in
               let ty' =
                 Types.copy_type_simple_but_variables
                   ~and_abstract: (Some cname) ty in
               Types.end_definition ();
               [Env.TypeInformation.SF_sig
                  (from, vname, (Types.generalize ty'))]
           | Env.TypeInformation.SF_let_rec l ->
               List.map
                 (fun (from, vname, _, scheme, _, _, _, _) ->
                  Types.begin_definition () ;
                  let ty = Types.specialize scheme in
                  let ty' =
                    Types.copy_type_simple_but_variables
                      ~and_abstract: (Some cname) ty in
                  Types.end_definition ();
                  Env.TypeInformation.SF_sig
                    (from, vname, (Types.generalize ty')))
                 l
           | Env.TypeInformation.SF_theorem
               (from, vname, polymorphic_vars_names, logical_expr, _, deps_rep)
           | Env.TypeInformation.SF_property
               (from, vname, polymorphic_vars_names, logical_expr, deps_rep) ->
               (* We substitute Self by [cname] in the logical_expr. *)
               let abstracted_logical_expr =
                 SubstColl.subst_logical_expr ~current_unit SubstColl.SRCK_self
                   (Types.SBRCK_coll cname) logical_expr in
               [Env.TypeInformation.SF_property
                  (from, vname, polymorphic_vars_names, abstracted_logical_expr,
                  deps_rep)]) in
        h' @ rec_abstract q in
  (* Do the job now... *)
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

    {b Exported} : No.                                                   *)
(* ********************************************************************* *)
let is_sub_species_of ~loc ctx ~name_should_be_sub_spe s1
    ~name_should_be_over_spe s2 =
  let local_flat_fields fields =
    List.fold_right
      (fun field accu ->
        match field with
        | Env.TypeInformation.SF_sig (from, v, sc)
        | Env.TypeInformation.SF_let (from, v, _, sc, _, _, _, _) ->
            (v, sc, from) :: accu
        | Env.TypeInformation.SF_let_rec l ->
            let l' =
              List.map (fun (from, v, _, sc, _, _, _, _) -> (v, sc, from)) l in
            l' @ accu
        | Env.TypeInformation.SF_theorem (from, v, _, _, _, _)
        | Env.TypeInformation.SF_property (from, v, _, _, _) ->
            let sc = Types.trivial_scheme (Types.type_prop ()) in
            (v, sc, from) :: accu)
      fields [] in
  let flat_s1 = local_flat_fields s1 in
  let flat_s2 = local_flat_fields s2 in
  (* Check that for all (v, sc) in s2, ex (v, sc') in s1 and sc = sc'. *)
  List.iter
    (fun (v2, sc2, from) ->
      let found =
        List.exists
          (fun (v1, sc1, _) ->
            if v1 = v2 then
              (begin
               Types.begin_definition ();
               let ty1 = Types.specialize sc1 in
               let ty2 = Types.specialize sc2 in
               (begin
                (* We try to translate type errors into more significant error
                   messages. *)
                try
                  ignore
                    (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
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
                end);
               Types.end_definition ();
               true
               end)
            else false)
          flat_s1 in
      (* Check if we found the same method name than v2 amoung the v1's.
         Note that if 2 fields with the same name were found, then either
         they can be unified, hence [found = true], or the unification
         failed, hence we can't be here, since an exception will abort the
         computation ! *)
      if not found then
        raise
          (Not_subspecies_missing_field
             (name_should_be_sub_spe, name_should_be_over_spe, v2, from, loc)))
      flat_s2
;;



let apply_substitutions_reversed_list_on_type reversed_substs ty =
  (* ATTENTION Do not fold left otherwise substitutions will be performed in
     reverse order. *)
  List.fold_right
    (fun subst accu_type ->
      match subst with
       | Env.SK_collection_by_collection (c1, c2) ->
           Types.subst_type_simple c1 c2 accu_type
       | Env.SK_ident_by_expression (_, _, _) ->
           (* Nothing to do since ML types never contain expressions and
              expressions identifiers. *)
           accu_type)
    reversed_substs
    ty
;;



let apply_substitutions_reversed_list_on_fields
    ~current_unit fields reversed_substs =
  (* ATTENTION Do not fold left otherwise substitutions will be performed in
     reverse order. *)
  List.fold_right
    (fun subst accu_fields ->
      List.map
        (fun field ->
          match subst with
           | Env.SK_collection_by_collection (c1, c2) ->
               SubstColl.subst_species_field
                 ~current_unit (SubstColl.SRCK_coll c1) c2 field
           | Env.SK_ident_by_expression
                 (param_unit, param_name_c1, expr_desc_c2) ->
               SubstExpr.subst_species_field
                 ~param_unit param_name_c1 expr_desc_c2 field)
        accu_fields)
    reversed_substs
    fields
;;



(* ************************************************************** *)
(* typing_context -> Env.TypingEnv.t ->                           *)
(*   Env.TypeInformation.species_description ->                   *)
(*     Parsetree.species_param list ->                            *)
(*       Env.TypeInformation.species_field list *                 *)
(*       (Types.fname * Types.collection_name) list *             *)
(*       (Types.type_collection *                                 *)
(*        Types.substitution_by_replacement_collection_kind) list *)
(** {b Descr} : Function managing application of arguments during
    species applications in species expressions.

    {b Exported} : No.                                            *)
(* ************************************************************** *)
let apply_species_arguments ctx env base_spe_descr params =
  (* ****************************************************************** *)
  (** {b Args} :
        - [accu_substs] : The list of substitutions to apply to each
             effective parameter before processing it. This is the way
             to represent the fact that in rule COLL-INST, page 43,
             fig 3.2, section 3.8 in Virgile Prevosto's Phd, the
             substutition "[c1 <- c2]" is performed on ts, i.e. in
             it's methods types but also in the remaining effective
             species parameters (yep, indeed, the ts signature of a
             species contains both the methodes and the parameters).
             Because these parameters will be "evaluated" after the
             current one, we need to delay the substitution until they
             are really processed.
             This list contains the substitutions in *REVERSE* order of
             application order. This means that the first required
             the substitution is in tail.                               *)
  (* ****************************************************************** *)
  let rec rec_apply accu_meths accu_substs accu_self_must_be = function
    | ([], []) -> (accu_meths, accu_self_must_be, (List.rev accu_substs))
    | ((f_param :: rem_f_params), (e_param :: rem_e_params)) ->
        let (new_meths, new_accu_substs, new_accu_self_must_be) =
          (begin
          let (Parsetree.SP e_param_expr) = e_param.Parsetree.ast_desc in
          match f_param with
           | Env.TypeInformation.SPAR_in (f_name, f_ty, _) ->
               (begin
               (* First, get the argument expression's type. *)
               let expr_ty = typecheck_expr ctx env e_param_expr in
               (* The formal's collection type [f_ty] is the name of the
                  collection that the effective argument is expected to be a
                  carrier of. Then one must unify the effective expression's
                  type with a "carrier" of the formal collection. *)
               let repr_of_formal =
                 Types.type_rep_species
                   ~species_module: (fst f_ty) ~species_name: (snd f_ty) in
               (* We must apply the already seen substitutions to this type
                  to ensure that it is transformed if required to collections
                  of possible previous "is" parameters. For instance, let's
                  take the following code:
                    species Me (Naturals is Intmodel, n in Naturals) = ...
                    collection ConcreteInt implements Intmodel
;;
                    collection Foo implements Me
                      (ConcreteInt, ConcreteInt!un)
;;
                  While typechecking the ConcreteInt!un, we get a type
                  Naturals. But since in the Foo collection, Naturals is
                  instanciated by the effective ConcreteInt, Naturals appears
                  to be incompatible with ConcreteInt. However, in Foo, with
                  the first instanciation, we said that Naturals IS a
                  ConcreteInt, and we substituted everywhere Naturals by
                  ConcreteInt. So idem must we do in the type infered for the
                  effective argument ConcreteInt!un. *)
               let substed_repr_of_formal =
                 apply_substitutions_reversed_list_on_type
                   accu_substs repr_of_formal in
               let param_type_for_ast =
                 Types.unify
                   ~loc: e_param.Parsetree.ast_loc
                   ~self_manifest: ctx.self_manifest substed_repr_of_formal
                   expr_ty in
               (* Record the type in the AST node. *)
               e_param.Parsetree.ast_type <-
                 Parsetree.ANTI_type param_type_for_ast;
               (* And now, the new methods where x <- e (in Virgile's thesis) *)
               (* i.e. here, [f_name] <- [e_param_expr].                      *)
               let substd_meths =
                 List.map
                   (SubstExpr.subst_species_field
                      ~param_unit: (fst f_ty)
                      f_name e_param_expr.Parsetree.ast_desc)
                   accu_meths in
               (* Since thay are local to a species, there will never be
                  confusion if 2 species have a same "IN" parameter name. And
                  finally, since in a species, there is no "IN" parameters
                  wearing the same name, no risk of confusion. *)
               let new_substs =
                 (Env.SK_ident_by_expression
                    (ctx.current_unit, f_name,
                     e_param_expr.Parsetree.ast_desc)) ::
                    accu_substs in
               (substd_meths, new_substs, accu_self_must_be)
               end)
           | Env.TypeInformation.SPAR_is ((f_module, f_name), _, c1_ty, _, _) ->
               (begin
               let c1 = (f_module, f_name) in
               (* Get the argument species expression signature and methods.
                  Note that to be well-typed this expression must ONLY be an
                  [E_constr] (because species names are capitalized, parsed as
                  sum type constructors) that should be considered as a
                  species name. C.f. Virgile Prevosto's Phd, section 3.8,
                  page 43.
                  Rule [COLL-INST]. *)
               match typecheck_expr_as_species_parameter_argument
                    ctx env e_param_expr with
                | TSPA_non_self ((c2_mod_name, c2_species_name),
                                 expr_sp_description) ->
                    let substd_c1_ty =
                      apply_substitutions_reversed_list_on_fields
                        ~current_unit: ctx.current_unit c1_ty accu_substs in
                    (* The c2 of Virgile's Phd. *)
                    let c2 = (c2_mod_name, c2_species_name) in
                    (* Record the type in the AST node. *)
                    let param_type_for_ast =
                      Types.type_rep_species
                        ~species_module: c2_mod_name
                        ~species_name: c2_species_name in
                    e_param.Parsetree.ast_type <-
                      Parsetree.ANTI_type param_type_for_ast;
                    (* Proceed to abstraction and signature compatibility. *)
                    let big_A_i1_c2 =
                      abstraction
                        ~current_unit: ctx.current_unit c2 substd_c1_ty in
                    (* Ensure that i2 <= A(i1, c2). *)
                    is_sub_species_of
                      ~loc: e_param.Parsetree.ast_loc ctx
                      ~name_should_be_sub_spe: c2
                      expr_sp_description.Env.TypeInformation.spe_sig_methods
                      ~name_should_be_over_spe: c1
                      big_A_i1_c2;
                    (* And now, the new methods where c1 <- c2. *)
                    let substd_meths =
                      List.map
                        (SubstColl.subst_species_field
                           ~current_unit: ctx.current_unit
                           (SubstColl.SRCK_coll c1) (Types.SBRCK_coll c2))
                        accu_meths in
                    let new_substs =
                      (Env.SK_collection_by_collection
                         (c1, (Types.SBRCK_coll c2))) ::
                        accu_substs in
                    (substd_meths, new_substs, accu_self_must_be)
                | TSPA_self ->
                    (* Record the type in the AST node. *)
                    let param_type_for_ast = Types.type_self () in
                    e_param.Parsetree.ast_type <-
                      Parsetree.ANTI_type param_type_for_ast;
                    (* No abstraction to do: this would be replacing Self by
                       itself ! Moreover, since we don't know yet the set of
                       methods Self will have, we delay the signature
                       compatibility to later, reminding that Self is expected
                       to have at least [c1]'s methods. *)
                    let new_self_must_be = c1 :: accu_self_must_be in
                    (* And now, the new methods where c1 <- c2. *)
                    let substd_meths =
                      List.map
                        (SubstColl.subst_species_field
                           ~current_unit: ctx.current_unit
                           (SubstColl.SRCK_coll c1) Types.SBRCK_self)
                        accu_meths in
                    let new_substs =
                      (Env.SK_collection_by_collection
                         (c1, Types.SBRCK_self)) ::
                        accu_substs in
                    (substd_meths, new_substs, new_self_must_be)
               end)
          end) in
        rec_apply
          new_meths new_accu_substs new_accu_self_must_be
          (rem_f_params, rem_e_params)
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
    []
    []
    (base_spe_descr.Env.TypeInformation.spe_sig_params, params)
;;



(* ****************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_expr ->     *)
(*   ((Env.TypeInformation.species_field list) *                      *)
(*    ((Types.fname * Types.collection_name) list)) *                 *)
(*   Env.TypeInformation.species_collection_kind *                    *)
(*   (DepGraphData.name_node list)                                    *)
(** {b Descr} : Typechecks a species expression, record its type in
              the AST node and returns:
                - the list of its methods names type schemes and
                  possible bodies (the list of fields in fact).
                - the list of collection-types that "Self" must be
                  compatible with to ensure that is is really the
                  case.
                - the provenance of the base species used to make
                  the expression (i.e. toplevel  collection, toplevel
                  species or species parameter)

    {b Exported} :No.                                                 *)
(* ****************************************************************** *)
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
     | Parsetree.I_global (Parsetree.Vname vname) ->
         (ctx.current_unit, (Parsetree_utils.name_of_vname vname))
     | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
         (fname, (Parsetree_utils.name_of_vname vname))) in
  let species_carrier_type =
    Types.type_rep_species ~species_module ~species_name in
  (* Record the type in the AST nodes. *)
  species_expr.Parsetree.ast_type <- Parsetree.ANTI_type species_carrier_type;
  species_expr_desc.Parsetree.se_name.Parsetree.ast_type <-
    Parsetree.ANTI_type species_carrier_type;
  (* Now, create the "species type" (a somewhat of signature). *)
  let species_methods_n_substs =
    apply_species_arguments
      ctx env species_species_description
      species_expr_desc.Parsetree.se_params in
  (* Record the type in the AST node. *)
  species_expr.Parsetree.ast_type <- Parsetree.ANTI_type species_carrier_type;
  (species_methods_n_substs,
   species_species_description.Env.TypeInformation.spe_kind,
   species_species_description.Env.TypeInformation.spe_dep_graph)
;;



(* ********************************************************************* *)
(* typing_context -> Env.TypingEnv.t ->                                  *)
(*   (Parsetree.vname * Parsetree.species_param_type) list ->            *)
(*     (Env.TypingEnv.t * Env.TypeInformation.species_param list *       *)
(*      Types.type_species)                                              *)
(** {b Descr} : Performs the typechecking of a species definition
    parameters.
    It build the species type of the species owning these parameters and
    return it.
    It also build the list of [Env.TypeInformation.species_param] that
    will appear in the hosting species [spe_sig_params] field.
    It also extend the environment with the species induced by the
    parameters and the carrier types of these species induced by the
    parameters.

    {b Exported} : No.                                                   *)
(* ********************************************************************* *)
let typecheck_species_def_params ctx env species_params =
  let rec rec_typecheck_params accu_env accu_self_must_be = function
    | [] -> (accu_env, [], accu_self_must_be)
    | (param_vname, param_kind) :: rem ->
        (begin
        let param_name_as_string = Parsetree_utils.name_of_vname param_vname in
        match param_kind.Parsetree.ast_desc with
         | Parsetree.SPT_in ident ->
             (begin
             (* Recover the module and the species name that the parameter
                must be of. *)
             let (param_sp_module, param_sp_name) =
               (match ident.Parsetree.ast_desc with
                | Parsetree.I_local vname
                | Parsetree.I_global (Parsetree.Vname vname) ->
                    (ctx.current_unit, (Parsetree_utils.name_of_vname vname))
                | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
                    (fname, (Parsetree_utils.name_of_vname vname))) in
             (* Check that the species exists to avoid raising an error at
                application-time if the species doesn't exist. Get its
                provenance to know if it's a toplevel species, toplevel
                collection or another species parameter. This will be used to
                annotate the current species parameter's type. *)
             let species_of_param_type =
               Env.TypingEnv.find_species
                 ~loc: ident.Parsetree.ast_loc ~current_unit: ctx.current_unit
                 ident accu_env in
             (* Create the carrier type of the parameter and extend the
                current environment with this parameter as a value of the
                carrier type. *)
             Types.begin_definition ();
             let param_carrier_ty =
               Types.type_rep_species ~species_module: param_sp_module
                 ~species_name: param_sp_name in
             Types.end_definition ();
             (* Record the type of the parameter in the AST nodes. *)
             param_kind.Parsetree.ast_type <-
               Parsetree.ANTI_type param_carrier_ty;
             ident.Parsetree.ast_type <- Parsetree.ANTI_type param_carrier_ty;
             (* Extend the environment with the parameter bound to its type. *)
             let accu_env' =
               Env.TypingEnv.add_value
                 ~toplevel: None param_vname
                 (Types.generalize param_carrier_ty) accu_env in
             (* And now, build the species type of the application. *)
             let (accu_env'', rem_spe_params, rem_accu_self_must_be) =
               rec_typecheck_params accu_env' accu_self_must_be rem in
             (* Inject in the parameter the provenance of its type. *)
             let current_spe_param =
               Env.TypeInformation.SPAR_in
                 (param_vname, (param_sp_module, param_sp_name),
                  species_of_param_type.Env.TypeInformation.spe_kind) in
             (* Finally, we return the fully extended environment and the
                type of the species application we just built. *)
             (accu_env'', (current_spe_param :: rem_spe_params),
              rem_accu_self_must_be)
             end)
         | Parsetree.SPT_is species_expr ->
             (begin
             (* First, typecheck the species expression. *)
             let ((species_expr_fields, self_must_be, _), base_species_kind,
                  species_dep_graph) =
               typecheck_species_expr ctx accu_env species_expr in
             (* Create the [species_description] of the parameter and extend
                the current environment. Because the obtained species is not
                a declaration, it cannot have parameters or inheritance.
                This leads to a somewhat of "local" species, and we will bind
                it in the species environment under an internal name to be
                able to denote it in the type of the application.
                This internal name is the name of the parameter. *)
             let abstracted_methods =
               abstraction
                 ~current_unit: ctx.current_unit
                 (ctx.current_unit, param_name_as_string) species_expr_fields in
             let param_description = {
               Env.TypeInformation.spe_kind = Types.SCK_species_parameter;
               Env.TypeInformation.spe_is_closed = false;
               Env.TypeInformation.spe_sig_params = [];
               Env.TypeInformation.spe_sig_methods = abstracted_methods;
               Env.TypeInformation.spe_dep_graph = species_dep_graph } in
             let accu_env' =
               Env.TypingEnv.add_species
                 ~loc: species_expr.Parsetree.ast_loc
                 param_vname param_description accu_env in
             (* Create the carrier type of the parameter and extend the
                current environment. *)
             Types.begin_definition ();
             let param_carrier_ty =
               Types.type_rep_species ~species_module: ctx.current_unit
                 ~species_name: param_name_as_string in
             Types.end_definition ();
             (* Record the type of the parameter in the AST node. *)
             param_kind.Parsetree.ast_type <-
               Parsetree.ANTI_type param_carrier_ty;
             (* Create the type description of the carrier the parameter is. *)
             let param_carrier_ty_description = {
               Env.TypeInformation.type_loc = Location.none;
               Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract;
               Env.TypeInformation.type_identity =
               Types.generalize param_carrier_ty;
               (* No param because the species is fully applied. *)
               Env.TypeInformation.type_params = [];
               Env.TypeInformation.type_arity = 0 } in
             let accu_env'' =
               Env.TypingEnv.add_type
                 ~loc: species_expr.Parsetree.ast_loc
                 param_vname param_carrier_ty_description accu_env' in
             let accu_self_must_be' = self_must_be @ accu_self_must_be in
             (* And now, build the species type of the application. *)
             let (accu_env''', rem_spe_params, rem_accu_self_must_be) =
               rec_typecheck_params accu_env'' accu_self_must_be' rem in
             (* We keep the [species_expr]'s in the [SPAR_is] structure because
                it will be needed for Coq code generation.
                By keeping the current unit and the parameter name, we in fact
                keep the type-collection od the parameter. In effect, a "IN"
                parameter is given a type-collection is its (module * name).
                This means that a species parameter has a purely local type to
                the species.
                Because we also keep the methods it has, we are still able to
                verify species signature matching ! *)
             let current_spe_param =
               Env.TypeInformation.SPAR_is
                 ((ctx.current_unit, param_name_as_string), base_species_kind,
                  species_expr_fields,
                  (species_expr_to_species_param_expr
                     ~current_unit: ctx.current_unit accu_env'''
                     species_expr),
                  species_dep_graph) in
             (* Finally, we return the fully extended environment and the
                type of the species application we just built. *)
             (accu_env''', (current_spe_param :: rem_spe_params),
              rem_accu_self_must_be)
             end)
        end) in
  (* And now do the job... *)
  rec_typecheck_params env [] species_params
;;



(** applied_substs : The list of substitutions of formal species parameters by
    effective ones done during the inheritance expression. The substitutions
    are in the right order for application. This means that the first to
    perform is in head of the list. *)
let extend_from_history ~current_species ~current_unit env
    species_expr_inherited applied_substs field =
  let simple_expr =
    species_expr_to_species_param_expr
      ~current_unit env species_expr_inherited in
  match field with
   | Env.TypeInformation.SF_sig (from, n, sch) ->
       let from' = { from with
         Env.fh_inherited_along =
           (current_species, simple_expr, applied_substs) ::
             from.Env.fh_inherited_along } in
       Env.TypeInformation.SF_sig (from', n, sch)
   | Env.TypeInformation.SF_let
           (from, n, parms, sch, body, otp, rep_deps, lflag) ->
       let from' = { from with
         Env.fh_inherited_along =
           (current_species, simple_expr, applied_substs) ::
             from.Env.fh_inherited_along } in
       Env.TypeInformation.SF_let
         (from', n, parms, sch, body, otp, rep_deps, lflag)
   | Env.TypeInformation.SF_let_rec l ->
       let l' =
         List.map
           (fun (from, n, parms, sch, body, otp, rep_deps, lflag) ->
             let from' = { from with
               Env.fh_inherited_along =
                 (current_species, simple_expr, applied_substs) ::
                 from.Env.fh_inherited_along } in
             (from', n, parms, sch, body, otp, rep_deps, lflag))
           l in
       Env.TypeInformation.SF_let_rec l'
   | Env.TypeInformation.SF_theorem (from, n, sch, body, proof, rep_deps) ->
       let from' = { from with
         Env.fh_inherited_along =
           (current_species, simple_expr, applied_substs) ::
             from.Env.fh_inherited_along } in
       Env.TypeInformation.SF_theorem (from', n, sch, body, proof, rep_deps)
   | Env.TypeInformation.SF_property (from, n, sch, body, rep_deps) ->
       let from' = { from with
         Env.fh_inherited_along =
           (current_species, simple_expr, applied_substs) ::
             from.Env.fh_inherited_along } in
       Env.TypeInformation.SF_property (from', n, sch, body, rep_deps)
;;



(* *********************************************************************** *)
(* loc: Location.t -> typing_context -> Env.TypingEnv.t ->                 *)
(* Parsetree.species_expr list ->                                          *)
(*   (Env.TypeInformation.species_field list *                             *)
(*    Env.TypingEnv.t * typing_context)                                    *)
(** {b Descr} : Extends an environment as value bindings with the methods
    of the inherited species provided in argument. Methods are added in
    the same order than their hosting species comes in the inheritance
    list. This means that the methods of the first inherited species will
    be deeper in the resulting environment.
    Extends the typing_context if among the inherited methods "rep" is
    found. In this case, this means that the carrier is manifest and
    changes the typing_context with its representation.
    When adding an inherited method, we record that its "from" information
    is modified to show that in the history, the method is now in the
    current species but via an inheritance step. In other words, we cons
    in [fh_inherited_along] of the inherited method, the
    (current_species, simple_species_expr from which the method is
    inherited).

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let extend_env_with_inherits ~current_species ~loc ctx env spe_exprs =
  let rec rec_extend current_ctx current_env accu_found_methods
          accu_self_must_be = function
    | [] -> (accu_found_methods, current_env, current_ctx, accu_self_must_be)
    | inh :: rem_inhs ->
      (* First typecheck the species expression in the initial (non extended)
         and recover its methods names and types. *)
      let ((inh_species_methods, self_must_be, applied_substs), _, _) =
        typecheck_species_expr current_ctx env inh in
      (* Change inside inherited fields the history information. *)
      let inh_species_methods =
        List.map
          (extend_from_history
             ~current_species ~current_unit: ctx.current_unit env inh
             applied_substs)
          inh_species_methods in
      let (env', current_ctx')  =
        List.fold_left
          (fun (accu_env, accu_ctx) field ->
            match field with
             | Env.TypeInformation.SF_sig (_, meth_name, meth_scheme)
             | Env.TypeInformation.SF_let
                 (_, meth_name, _, meth_scheme, _, _, _, _) ->
               let e =
                 Env.TypingEnv.add_value
                   ~toplevel: None  meth_name meth_scheme accu_env in
               (* Now check if we inherited a [rep]. *)
               let m_name_as_str = Parsetree_utils.name_of_vname meth_name in
               let manifest =
                 (if m_name_as_str = "rep" then
                   (begin
                    (* Before modifying the context, just check that no "rep"
                       was previously identified with a different type by
                       multiple inheritance in which case we fail. *)
                    match accu_ctx.self_manifest with
                     | None -> Some (Types.specialize meth_scheme)
                     | Some prev_ty ->
                         let new_found_self = Types.specialize meth_scheme in
                         (try
                           (* Since we don't want any circularity on Self we
                              keep it abstract during the unification.
                              As the new "Self", we use the MGU of the previous
                              Self's type and the newly found one. *)
                           Some
                             (Types.unify
                                ~loc ~self_manifest: None prev_ty
                                new_found_self)
                         with _ ->
                           raise
                             (Rep_multiply_defined_by_multiple_inheritance
                               (prev_ty, new_found_self, loc)))
                   end)
                  else accu_ctx.self_manifest) in (* Else, keep unchanged. *)
               let c = { accu_ctx with self_manifest = manifest } in
               (e, c)
             | Env.TypeInformation.SF_let_rec l ->
               let e =
                 List.fold_left
                   (fun internal_accu_env
                        (_, meth_name, _, meth_scheme, _, _, _, _) ->
                    Env.TypingEnv.add_value
                      ~toplevel: None meth_name meth_scheme internal_accu_env)
                   accu_env
                   l in
               (e, accu_ctx)
             | Env.TypeInformation.SF_theorem  (_, theo_name, _, _, _, _) ->
                 let t_sch = Types.trivial_scheme (Types.type_prop ()) in
                 let e =
                   Env.TypingEnv.add_value
                     ~toplevel: None theo_name t_sch accu_env in
                 (e, accu_ctx)
             | Env.TypeInformation.SF_property (_, prop_name, _, _, _) ->
                 let prop_sch = Types.trivial_scheme (Types.type_prop ()) in
                 let e =
                   Env.TypingEnv.add_value
                     ~toplevel: None prop_name prop_sch accu_env in
                 (e, accu_ctx))
          (current_env, current_ctx)
          inh_species_methods in
      let new_accu_self_must_be = self_must_be @ accu_self_must_be in
      let new_accu_found_methods = accu_found_methods @ inh_species_methods in
      (* Go on with the next inherited species. *)
      rec_extend
        current_ctx' env' new_accu_found_methods new_accu_self_must_be
        rem_inhs in
  (* ****************** *)
  (* Now, let's work... *)
  rec_extend ctx env [] [] spe_exprs
;;



(* ************************************************************************ *)
(* Parsetree.proof_def_desc -> current_species:Parsetree.qualified_vname -> *)
(*   Env.TypeInformation.species_field list ->                              *)
(*     (Env.TypeInformation.species_field list * (Env.from_history option)) *)
(* {b Descr} : Searches in the list the first SF_property or SF_theorem
   field whose name is equal to the [proof_of]'s name, then convert
   this property field into a theorem fields by adding the [pd_proof] field
   of the [proof_of].
   Then return the initial [fields] list with this field transformed inside
   and an option telling if a change finally occured and if yes, we get a
   [Some] containing the [from_history] of the theorem.
   This process is used to make Parsetree.SF_proof diseaper, merging their
   proof in the related property definition in order to create an equivalent
   theorem instead. It also serves to merge proof re-done of a theorem
   because the user knew that the therem's original proof was invalidated
   and must be done again.

   ATTENTION:
   This function must only be used on non-inherited fields since the
   possibly changed fields remains in the returned fields list. This is
   wrong in case where the collapse occurs in an inherited field since
   the changed field must then be put in the non-inherited fields in
   order that the rest of the analysis works correctly !

   {b Exported} : No.                                                       *)
(* ************************************************************************ *)
let collapse_proof_in_non_inherited (proof_of, pr_deps_on_rep) ~current_species
    fields =
  let name_of_proof_of = proof_of.Parsetree.pd_name in
  let rec rec_find = function
    | [] -> ([], None)
    | field :: rem ->
        (begin
        match field with
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _)
         | Env.TypeInformation.SF_let_rec _ ->
             let (collapsed_rem, was_collapsed) = rec_find rem in
             (field :: collapsed_rem, was_collapsed)
         | Env.TypeInformation.SF_theorem
             (from, name, sch, logical_expr, _, deps_rep)
         | Env.TypeInformation.SF_property
             (from, name, sch, logical_expr, deps_rep) ->
           (begin
           if name_of_proof_of = name then
             (begin
             assert (from.Env.fh_initial_apparition = current_species);
             (* We first merge the found dependencies on "rep" for the proof
                and the property. *)
             let deps_rep' = {
               Env.TypeInformation.dor_def =
                 pr_deps_on_rep.Env.TypeInformation.dor_def ||
                 deps_rep.Env.TypeInformation.dor_def;
               Env.TypeInformation.dor_decl =
                 pr_deps_on_rep.Env.TypeInformation.dor_decl ||
                 deps_rep.Env.TypeInformation.dor_decl } in
             (* We found the property related to the proof. Change this
                property into a theorem. Since it's a definition, and this
                definition comes from the current species (i.e. not via
                inheritance, the inheritance history records only that the
                theorem comes from the current species without any other
                history. *)
             let new_field =
               Env.TypeInformation.SF_theorem
                 ((Env.intitial_inheritance_history current_species), name,
                  sch, logical_expr, proof_of.Parsetree.pd_proof, deps_rep') in
             if Configuration.get_verbose () then
               Format.eprintf
                 "Merging property '%a' from '%a' and proof from '%a'\
                 into theorem.@."
                 Sourcify.pp_vname name
                 Sourcify.pp_qualified_species from.Env.fh_initial_apparition
                 Sourcify.pp_qualified_species current_species;
                (* Stop the search now. Say that a change actually occured by
                   telling "Some" of the [from_history] of the theorem. *)
             (new_field :: rem, (Some (name, from)))
             end)
           else
             (begin
             (* Like the cas where the field was not a [SF_property]. *)
             let (collapsed_rem, was_collapsed) = rec_find rem in
             ((field :: collapsed_rem), was_collapsed)
             end)
           end)
        end) in
  rec_find fields
;;



(** {B Descr} : Same than [collapse_proof_in_non_inherited] but instead of
    re-inserting the changed field in the fields list, return the list
    without the field and the changed field aside. *)
let collapse_proof_in_inherited (proof_of, pr_deps_on_rep) ~current_species
    fields =
  let name_of_proof_of = proof_of.Parsetree.pd_name in
  let rec rec_find = function
    | [] -> ([], None)
    | field :: rem ->
        (begin
        match field with
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _)
         | Env.TypeInformation.SF_let_rec _ ->
             let (collapsed_rem, was_collapsed) = rec_find rem in
             (field :: collapsed_rem, was_collapsed)
         | Env.TypeInformation.SF_theorem
             (from, name, sch, logical_expr, _, deps_rep)
         | Env.TypeInformation.SF_property
             (from, name, sch, logical_expr, deps_rep) ->
             (begin
             if name_of_proof_of = name then
               (begin
               (* We first merge the found dependencies on "rep" for the proof
                  and the property. *)
               let deps_rep' = {
                 Env.TypeInformation.dor_def =
                   pr_deps_on_rep.Env.TypeInformation.dor_def ||
                   deps_rep.Env.TypeInformation.dor_def;
                 Env.TypeInformation.dor_decl =
                   pr_deps_on_rep.Env.TypeInformation.dor_decl ||
                   deps_rep.Env.TypeInformation.dor_decl } in
               (* We found the property related to the proof. Change this
                  property into a theorem. Since the addition of this proof
                  leads to an effective definition of the theorem, and this
                  definition comes from the current species (i.e. not via
                  inheritance, the inheritance history records only that
                  the theorem comes from the current species without any
                  other history. *)
               let new_field =
                 Env.TypeInformation.SF_theorem
                   ((Env.intitial_inheritance_history current_species), name,
                    sch, logical_expr, proof_of.Parsetree.pd_proof,
                    deps_rep') in
               if Configuration.get_verbose () then
                 Format.eprintf
                   "Merging property '%a' from '%a' and proof from '%a'\
                   into theorem.@."
                   Sourcify.pp_vname name
                   Sourcify.pp_qualified_species from.Env.fh_initial_apparition
                   Sourcify.pp_qualified_species current_species;
               (* Save the initial provenance of the property before we
                  collapsed it and its prooof. *)
               let from_before_collapse = (name, from) in
               (* Stop the search now. Say that a change actually occured. *)
               (rem, (Some (new_field, from_before_collapse)))
               end)
             else
               (begin
               (* Like the cas where the field was not a [SF_property]. *)
               let (collapsed_rem, was_collapsed) = rec_find rem in
               ((field :: collapsed_rem), was_collapsed)
               end)
             end)
        end) in
  rec_find fields
;;



(* ************************************************************************ *)
(* current_species: Parsetree.qualified_vname -> Parsetree.proof_def ->     *)
(*  Env.TypeInformation.species_field list ->                               *)
(*    Env.TypeInformation.species_field list ->                             *)
(*      (Env.TypeInformation.species_field list *                           *)
(*       Env.TypeInformation.species_field list *                           *)
(*       ((Parsetree.vname * Env.from_history) list))                       *)
(* {b Descr} : Tries to find among [methods], property fields whose proofs
   are separately given in the list of proofs [found_proofs_of].
   Each time the search succeeds, the property and the related proof are
   merged in a new theorem field, hence discarding the property fields.
   Because this process is performed before the normalization pass, we
   still require to have 3 separate lists of methods:
     - the inherited ones,
     - those defined at the current inheritance level.
     - the assoc list giing for each theorem what was it initial
       [from_history] of the property before the collapsing turned it into
       a theorem.
   For this reason, the search will be done first on the methods defined at
   the current inheritance level (in order to find the "most recent") and
   only if the search failed, we will try it again on the inherited methods.

   {b Rem} : BE CAREFUL, such a merge now require a re-ordering of the
   final fields. In effect, by moving the proof_of field when merging it as
   a [SF_theorem] located where the initial [SF_property] field was, if the
   proof (that was originally "later") uses stuff defined between the
   [SF_property] and the original location of the proof, then this stuff
   will now appear "after" the proof itself. And this is not well-formed.

   {b Exported} : Yes.                                                      *)
(* ************************************************************************ *)
let collapse_proofs_of ~current_species found_proofs_of
      inherited_methods_infos methods_info =
  (* A bit dirty, we could reminf this without a reference, but I get bored
     to have a third accumulator int the [fold_left] below... In this list, we
     will record for each theorem, the [from_history] of the property it was
     before collapsing. If there was non collapsing, then the theorem was
     already a theorem, then we remind if [from_history] however. *)
  let original_properties_from_histories = ref [] in
  (* We must first reverse the lists of methods so that the collapse
     procedure will find first the most recent methods. *)
  let revd_inherited_methods_infos = List.rev inherited_methods_infos in
  let revd_methods_info = List.rev methods_info in
  let (revd_collapsed_inherited_methods, revd_collapsed_current_methods) =
    List.fold_left
      (fun (accu_inherited, accu_current) (found_proof_of, pr_desp_rep) ->
        (* First, try on the "most recent" methods, i.e. the current *)
        (* inheritance level's ones.                                 *)
        let (collapsed_current, was_collapsed) =
          collapse_proof_in_non_inherited
            ~current_species (found_proof_of.Parsetree.ast_desc, pr_desp_rep)
            accu_current in
        match was_collapsed with
         | Some theo_from ->
             (begin
             (* Collapsing without inheritance. Hence we the [from_history]. *)
             original_properties_from_histories :=
               theo_from :: !original_properties_from_histories;
             (accu_inherited, collapsed_current)
             end)
         | None ->
             (begin
             (* No collapse in the current level's methods, then try on the
                inherited ones. *)
             let (collapsed_inherited, collapsed_option) =
               collapse_proof_in_inherited
                 ~current_species
                 (found_proof_of.Parsetree.ast_desc, pr_desp_rep)
                 accu_inherited in
             match collapsed_option with
              | None ->
                  (* No collapse at all ! This means that we have a proof not
                     related to an existing property. This is an error. *)
                  raise
                    (Proof_of_unknown_property
                       (found_proof_of.Parsetree.ast_loc,
                        current_species,
                        found_proof_of.Parsetree.ast_desc.Parsetree.pd_name))
              | Some (fresh_theorem, from_before_collapse) ->
                  (* Collapsing with inheritance. Hence we record the
                     [from_history]. *)
                  original_properties_from_histories :=
                    from_before_collapse ::
                    !original_properties_from_histories;
                  (* Transfer the inherited field that had no proof to the
                     non-inherited fields list since it received its proof now,
                     i.e. in the non-inherited fields !
                     Note that now, in [collapsed_inherited] the collapsed
                     field has now disapeared. *)
                  (collapsed_inherited, (fresh_theorem :: accu_current))
             end))
      (revd_inherited_methods_infos, revd_methods_info)
      found_proofs_of in
  (* And then, reverse again the result to get again the initial and correct
     order. *)
  (List.rev revd_collapsed_inherited_methods,
   List.rev revd_collapsed_current_methods,
   !original_properties_from_histories)
;;



(* ********************************************************************* *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->          *)
(*   (Env.TypeInformation.species_field *                                *)
(*    (Env.TypeInformation.species_field list))                          *)
(** {b Descr} : Among the fields list [fields], find the one binding the
    name [name]. Return both the found field and the list minus this
    found field.

    {b Exported} : No.                                                   *)
(* ********************************************************************* *)
let extract_field_from_list_by_name name fields =
  let rec rec_extract = function
    | [] -> raise Not_found
    | field :: rem ->
      (begin
        let found =
          (match field with
           | Env.TypeInformation.SF_sig (_, n, _)
           | Env.TypeInformation.SF_let (_, n, _, _, _, _, _, _)
           | Env.TypeInformation.SF_theorem (_, n, _, _, _, _)
           | Env.TypeInformation.SF_property (_, n, _, _, _) -> name = n
           | Env.TypeInformation.SF_let_rec l ->
               List.exists (fun (_, n, _, _, _, _, _, _) -> name = n) l) in
        if found then (field, rem) else
          let (found_field, tail) = rec_extract rem in
          (found_field, field :: tail)
       end) in
  rec_extract fields
;;



(* ******************************************************************** *)
(* Parsetree.vname list -> Parsetree.vname list                         *)
(** {b Descr} : Looks for "rep" in the names list [l]. If found, then
    remove it from its place and put it again but as the first element
    of the list.

    {b Rem} : Implicitely assumes that "rep" exists at most once in the
    list [l] (no doubles).

    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
let ensure_rep_in_first l =
  let rec rec_search = function
    | [] -> ([], false)
    | h :: q ->
        (* If it's "rep", then stop and return the list without "rep".
           Of course, by stopping search we assume that "rep" is at most once
           in the list (no doubles). *)
        if h = Parsetree.Vlident "rep" then (q, true) else
          let (filtered_q, found) = rec_search q in
          (h :: filtered_q, found) in
  (* Go... *)
  let (new_l, was_found) = rec_search l in
  (* If "rep" was found, then it was also removed, then re-add it in front. *)
  if was_found then (Parsetree.Vlident "rep") :: new_l else new_l
;;



(* ******************************************************************** *)
(* Parsetree.vname list ->  Env.TypeInformation.species_field list ->   *)
(*   Env.TypeInformation.species_field list                             *)
(** {b Descr} : Effectively reorganize the fields contained in the list
    [fields] to make them appear in the order provided by the list of
    names [order].
    For [Let_rec] fields, their order of apparition is given by the
    order of the first name (rec bound) appearing in the order list.
    ATTENTION : The only exception is "rep" which if present is always
    put at the beginning of the list !
    ATTENTION: Moreover, because the order is induced by dependencies,
    one may have dependencies on "rep" although "rep" is not defined
    (remember that is "rep" is not given a structure, then instead of
    declaring it as "rep;", we do not mention it at all). In this
    case the lookup via [extract_field_from_list_by_name] may fail.
    That's the only case where we accept it to fail.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let order_fields_according_to order fields =
  let rec rec_reorder rec_order rec_fields =
    match (rec_order, rec_fields) with
     | ([], []) -> []
     | ([], _) | (_, []) ->
         (* If there are spurious fields or names then it's because we went
            wrong somewhere ! *)
         assert false
     | ((name :: rem_rec_order), _) ->
         (begin
         try
           (* We first find the field hosting [name]. This field will be
              inserted here in the result list. We then must remove from the
              order list, all the name rec-bound with [name]. Then we continue
              with this new order and the fields list from which we remove the
              found field. This way, the fields list in which we search will
              be smaller and smaller (cool for efficiency), and will finish
              to be empty. *)
           let (related_field, new_rec_fields) =
             extract_field_from_list_by_name name rec_fields in
           let names_bound =
             List.map
               fst
               (Dep_analysis.ordered_names_list_of_fields [related_field]) in
           (* So, remove from the order the rec-bound names... *)
           let new_rec_order =
             List.filter
               (fun n -> not (List.mem n names_bound))
               rem_rec_order in
           related_field :: (rec_reorder new_rec_order new_rec_fields)
         with Not_found ->
           (* See second ATTENTION in the header of the function. *)
           (rec_reorder rem_rec_order rec_fields)
         end) in
  (* Now do the job. First, if "rep" is in the order, then ensure that it is
     the first in the list. *)
  let order_with_rep_in_front = ensure_rep_in_first order in
  rec_reorder order_with_rep_in_front fields
;;



(* ********************************************************************** *)
(* loc: Location.t -> typing_context -> Parsetree.vname ->                *)
(*   Types.type_scheme ->                                                 *)
(*     (Types.species_name * Parsetree.vname * Types.type_scheme *        *)
(*      Parsetree.expr * Env.TypeInformation.dependency_on_rep) list ->   *)
(*       Env.TypeInformation.species_field                                *)
(** {b Descr} : Implements the "fusion" algorithm (c.f [fields_fusion])
    in the particular case of fusionning 1 field Sig and 1 field Let_rec.

    {b Args}:
      - [loc] : Location of the whole hosting species expression.

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let fusion_fields_let_rec_sig ~loc ctx sig_name sig_scheme sig_hist rec_meths =
  let rec_meths' =
    List.map
      (fun ((from, n, params_names, sc, body, otp, dep_on_rep, log_f)
              as rec_meth) ->
        if n = sig_name then
          begin
           (* Fusion, by unification may induce def dependency on "rep" ! *)
           Types.reset_deps_on_rep ();
           Types.begin_definition ();
           let sig_ty = Types.specialize sig_scheme in
           let ty = Types.specialize sc in
           (* Don't keep the type where Self is prefered. *)
           (try
             ignore
               (Types.unify ~loc ~self_manifest: ctx.self_manifest sig_ty ty)
           with _ ->
             (* Try to have a more comprehensive error message. *)
             raise
               (Wrong_type_by_inheritance
                  (loc, n, sig_ty, ty, sig_hist, from)));
           Types.end_definition ();
           let dep_on_rep' = {
             Env.TypeInformation.dor_def =
               dep_on_rep.Env.TypeInformation.dor_def ||
               Types.get_def_dep_on_rep ();
             Env.TypeInformation.dor_decl =
               dep_on_rep.Env.TypeInformation.dor_decl ||
               Types.get_decl_dep_on_rep () } in
           (* If a sig is specified, we always keep it as type. *)
           (from, n, params_names, sig_scheme, body, otp, dep_on_rep', log_f)
          end
        else rec_meth)
      rec_meths in
  Env.TypeInformation.SF_let_rec rec_meths'
;;



(* ***************************************************************** *)
(* 'a -> ('b * 'a * 'c * 'd * 'e * 'f * 'g) list ->                  *)
(*   ('b * 'a * 'c * 'd * 'e * 'f* 'g) *                             *)
(*    ('b * 'a * 'c * 'd * 'e * 'f * 'g) list                        *)
(* {b Descr} : Searches in the list the first element whose first
   component is equal to [name], then returns it and the list minus
   this element.
   If the searched name is not found in the list, then the exception
   [Not_found] is raised.

   {b Exported} : No.                                                *)
(* ***************************************************************** *)
let find_and_remain name meths =
  let rec rec_find = function
    | [] -> raise Not_found
    | ((_, n, _, _, _, _, _, _) as meth) :: rem ->
        if name = n then (meth, rem) else
          let (found, tail) = rec_find rem in
          (found, (meth :: tail)) in
  rec_find meths
;;



(* ******************************************************************** *)
(* loc: Location.t -> typing_context ->                                 *)
(*   (Types.species_name * Parsetree.vname * Types.type_scheme *        *)
(*    Parsetree.expr) list ->                                           *)
(*     (Types.species_name *  Parsetree.vname * Types.type_scheme *     *)
(*      Parsetree.expr) list ->                                         *)
(*       Env.TypeInformation.species_field                              *)
(** {b Descr} : Implements the "fusion" algorithm (c.f [fields_fusion])
    in the particular case of fusionning 2 fields Let_rec.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let fusion_fields_let_rec_let_rec ~loc ctx rec_meths1 rec_meths2 =
  let rec rec_fusion l1 l2 =
    match l1 with
    | [] -> l2
    | ((f1, n1, _, sc1, _, _, _, log_flag1) as meth) :: rem1 ->
      let (fused_meth, new_l2) =
        (try
          let (m2, rem_of_l2) = find_and_remain n1 l2 in
          let (f2, n2, args2, sc2, body2, otp2, dep2, log_flag2) = m2 in
          (* We don't allow to redefine methods mixing logical and computation
             flag. *)
          if log_flag1.Env.TypeInformation.ldf_logical !=
             log_flag2.Env.TypeInformation.ldf_logical then
            raise (No_mix_between_logical_defs (loc, n1));
          Types.begin_definition ();
          let ty1 = Types.specialize sc1 in
          let ty2 = Types.specialize sc2 in
          Types.reset_deps_on_rep ();
          (* Ensure that the 2 versions of the method are type-compatible. *)
          (try
            ignore
              (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
          with _ ->
            (* Try to have a more comprehensive error message. *)
            raise
              (Wrong_type_by_inheritance (loc, n1, ty1, ty2, f1, f2)));
          Types.end_definition ();
          (* And return the seconde one (late binding) with the presence of
             def-dependency on "rep" updated. *)
          let dep' = {
            Env.TypeInformation.dor_def =
              dep2.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
            Env.TypeInformation.dor_decl =
              dep2.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep ()
            } in
          let m2' = (f2, n2, args2, sc2, body2, otp2, dep', log_flag2) in
          (m2', rem_of_l2)
         with Not_found ->
          (* The method doesn't belong to l2, then keep this one. *)
          (meth, l2)) in
      (* Now make the fusion of the remaining of l1 and the remaining of l2
         (this last one being possibly l2 if the search failed). *)
      let rem_fused_methods = rec_fusion rem1 new_l2 in
      fused_meth :: rem_fused_methods in
  (* Go... *)
  Env.TypeInformation.SF_let_rec (rec_fusion rec_meths1 rec_meths2)
;;



(** Mainly ensure that the method to replace exists in the bunch of newest
    recursive methods, ensure that its type is compatible and return the
    bunch of the newest recursive methods. Hence, the "replaced" method
    totally disapears. *)
let fusion_fields_let_let_rec ~loc ctx meth1 rec_meths2 =
  try
    let (f1, n1, _, sc1, _, _, _, log_flag1) = meth1 in
    let (m2, rem_of_l2) = find_and_remain n1 rec_meths2 in
    let (f2, n2, args2, sc2, body2, otp2, dep2, log_flag2) = m2 in
    (* We don't allow to redefine methods mixing logical and computation
       flag. *)
    if log_flag1.Env.TypeInformation.ldf_logical !=
       log_flag2.Env.TypeInformation.ldf_logical then
      raise (No_mix_between_logical_defs (loc, n1));
    Types.begin_definition ();
    let ty1 = Types.specialize sc1 in
    let ty2 = Types.specialize sc2 in
    Types.reset_deps_on_rep ();
    (* Ensure that the 2 versions of the method are type-compatible. *)
    (try
      ignore
        (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
    with _ ->
      (* Try to have a more comprehensive error message. *)
      raise (Wrong_type_by_inheritance (loc, n1, ty1, ty2, f1, f2)));
    Types.end_definition ();
    (* And return the second one (late binding) with the presence of
       def-dependency on "rep" updated. We re-insert the method in the
       remaining bunch of initial recursive method. *)
    let dep' =
      { Env.TypeInformation.dor_def =
          dep2.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
        Env.TypeInformation.dor_decl =
          dep2.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep () } in
    let m2' = (f2, n2, args2, sc2, body2, otp2, dep', log_flag2) in
    (* No matter if the position of the re-inserted method differs from its
       original place. So, finally as result, if the fusion succeeded we
       totally forgot the old non-recursive method and we get the new bunch of
       recursive ones. *)
    m2' :: rem_of_l2
  with Not_found ->
    (* The method doesn't belong to [rec_meths2]. *)
    assert false (* From Virgile's thesis Lemma 8 p 37 *)
;;



(* We accept to merge an old non-recursive method with several new recursive
   methods. We just insert the newly found method in the bunch ofo the old
   recursive ones, removing from them the method that has to be replaced by
   the new one. *)
let fusion_fields_let_rec_let ~loc ctx rec_meths1 meth2 =
  try
    let (f2, n2, args2, sc2, body2, otp2, dep2, log_flag2) = meth2 in
    let (m1, rem_of_l1) = find_and_remain n2 rec_meths1 in
    let (f1, n1, _, sc1, _, _, _, log_flag1) = m1 in
    (* We don't allow to redefine methods mixing logical and computation
       flag. *)
    if log_flag1.Env.TypeInformation.ldf_logical !=
       log_flag2.Env.TypeInformation.ldf_logical then
      raise (No_mix_between_logical_defs (loc, n1));
    Types.begin_definition ();
    let ty1 = Types.specialize sc1 in
    let ty2 = Types.specialize sc2 in
    Types.reset_deps_on_rep ();
    (* Ensure that the 2 versions of the method are type-compatible. *)
    (try
      ignore
        (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
    with _ ->
      (* Try to have a more comprehensive error message. *)
      raise (Wrong_type_by_inheritance (loc, n1, ty2, ty1, f2, f1)));
    Types.end_definition ();
    (* And return the first one (late binding) with the presence of
       def-dependency on "rep" updated. We re-insert the method in the
       remaining bunch of initial recursive method. *)
    let dep' =
      { Env.TypeInformation.dor_def =
          dep2.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
        Env.TypeInformation.dor_decl =
          dep2.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep () } in
    let m2' = (f2, n2, args2, sc2, body2, otp2, dep', log_flag2) in
    (* No matter if the position of the re-inserted method differs from its
       original place. So, finally as result, if the fusion succeeded we get
       the old recursive methods except the one wearing the name of newly
       merged, and this newly merged one is inserted among the old recursive
       ones. *)
    m2' :: rem_of_l1
  with Not_found ->
    (* The method doesn't belong to [rec_meths2]. *)
    assert false (* From Virgile's thesis Lemma 8 p 37 *)
;;



(* ************************************************************************* *)
(* loc: Location.t -> typing_context -> Env.TypeInformation.species_field -> *)
(*   Env.TypeInformation.species_field -> Env.TypeInformation.species_field  *)
(** {b Descr} : Implements the "fusion" algorithm described in Virgile
    Prevosto's Phd, Section 3.6, page 35 and completed in Section 3.9.7,
    page 56, definition 35.
    This basically ensures that 2 fields with at leat 1 common name are
    type-compatible and select the new field information that summarizes
    these 2 original fields (implementing the late binding feature by the
    way).

    {b Args}:
      - [loc] : Location of the whole hosting species expression.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let fields_fusion ~loc ctx phi1 phi2 =
  match phi1, phi2 with
   (* *** *)
   | (Env.TypeInformation.SF_sig (from1, n1, sc1),
      Env.TypeInformation.SF_sig (from2, n2, sc2)) when n1 = n2 ->
        (* sig / sig. *)
        Types.begin_definition ();
        let ty1 = Types.specialize sc1 in
        let ty2 = Types.specialize sc2 in
        (try
          ignore (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
        with _ ->
          (* Try to have a more comprehensive error message. *)
          raise (Wrong_type_by_inheritance (loc, n1, ty1, ty2, from1, from2)));
        Types.end_definition ();
        (* If 2 sigs are specified, we always keep the last one as type. *)
        Env.TypeInformation.SF_sig (from2, n2, sc2)
   | (Env.TypeInformation.SF_sig (from1, n1, sc1),
      Env.TypeInformation.SF_let
          (from2, n2, pars2, sc2, body, opt2, dep2, log2)) when n1 = n2 ->
        (* sig / let. *)
        Types.reset_deps_on_rep ();
        Types.begin_definition ();
        let ty1 = Types.specialize sc1 in
        let ty2 = Types.specialize sc2 in
        (try
          ignore (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
        with _ ->
          (* Try to have a more comprehensive error message. *)
          raise (Wrong_type_by_inheritance (loc, n1, ty1, ty2, from1, from2)));
        Types.end_definition ();
        let dep' = {
          Env.TypeInformation.dor_def =
            dep2.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
          Env.TypeInformation.dor_decl =
            dep2.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep ()
          } in
        (* If a sig is specified, we always keep it as type. *)
        Env.TypeInformation.SF_let
          (from2, n2, pars2, sc1, body, opt2, dep', log2)
   | (Env.TypeInformation.SF_sig (from1, n1, sc1),
      Env.TypeInformation.SF_let_rec rec_meths) ->
        (* sig / let rec. *)
        fusion_fields_let_rec_sig ~loc ctx n1 sc1 from1 rec_meths
   (* *** *)
   | (Env.TypeInformation.SF_let
        (from1, n1, pars1, sc1, body, otp1, dep1, log1),
      Env.TypeInformation.SF_sig (from2, n2, sc2)) when n1 = n2 ->
        (* let / sig. *)
        Types.reset_deps_on_rep ();
        Types.begin_definition ();
        let ty1 = Types.specialize sc1 in
        let ty2 = Types.specialize sc2 in
        (try
          ignore (Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2)
        with _ ->
          (* Try to have a more comprehensive error message. *)
          raise (Wrong_type_by_inheritance (loc, n1, ty1, ty2, from1, from2)));
        Types.end_definition ();
        let dep' = {
          Env.TypeInformation.dor_def =
            dep1.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
          Env.TypeInformation.dor_decl =
            dep1.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep ()
          } in
        (* If a sig is specified, we always keep it as type. *)
        Env.TypeInformation.SF_let
          (from1, n1, pars1, sc2, body, otp1, dep', log1)
   | (Env.TypeInformation.SF_let (from1, n1, _, sc1, _, _, _, log_flag1),
      Env.TypeInformation.SF_let
          (from2, n2, pars2, sc2, body, otp2, dep, log_flag2)) when n1 = n2 ->
        (* let / let. *)
        (* Late binding : keep the second body ! *)
        if log_flag1.Env.TypeInformation.ldf_logical !=
           log_flag2.Env.TypeInformation.ldf_logical then
          raise (No_mix_between_logical_defs (loc, n1));
        Types.reset_deps_on_rep ();
        Types.begin_definition ();
        let ty1 = Types.specialize sc1 in
        let ty2 = Types.specialize sc2 in
        let ty =
          (try Types.unify ~loc ~self_manifest: ctx.self_manifest ty1 ty2
          with _ ->
            (* Try to have a more comprehensive error message. *)
            raise
              (Wrong_type_by_inheritance (loc, n1, ty1, ty2, from1, from2))) in
        Types.end_definition ();
        let dep' = {
          Env.TypeInformation.dor_def =
            dep.Env.TypeInformation.dor_def || Types.get_def_dep_on_rep ();
          Env.TypeInformation.dor_decl =
            dep.Env.TypeInformation.dor_decl || Types.get_decl_dep_on_rep ()
          } in
        Env.TypeInformation.SF_let
          (from2, n2, pars2, (Types.generalize ty), body, otp2, dep', log_flag2)
   | (Env.TypeInformation.SF_let meth1,
      Env.TypeInformation.SF_let_rec rec_meths2) ->
        Env.TypeInformation.SF_let_rec
          (fusion_fields_let_let_rec ~loc ctx meth1 rec_meths2)
   (* *** *)
   | (Env.TypeInformation.SF_let_rec rec_meths,
      Env.TypeInformation.SF_sig (from2, n2, sc2)) ->
        (* let rec / sig. *)
        (* Symetric case than for sig / let_rec. *)
        fusion_fields_let_rec_sig ~loc ctx n2 sc2 from2 rec_meths
   | (Env.TypeInformation.SF_let_rec rec_meths1,
      Env.TypeInformation.SF_let meth2) ->
        Env.TypeInformation.SF_let_rec
          (fusion_fields_let_rec_let ~loc ctx rec_meths1 meth2)
   | ((Env.TypeInformation.SF_let_rec rec_meths1),
      (Env.TypeInformation.SF_let_rec rec_meths2)) ->
        fusion_fields_let_rec_let_rec ~loc ctx rec_meths1 rec_meths2
   | ((Env.TypeInformation.SF_property (h1, n1, ntyvar1, logical_expr1, _)),
      (Env.TypeInformation.SF_property (h2, n2, ntyvar2, logical_expr2, _)))
   | ((Env.TypeInformation.SF_property (h1, n1, ntyvar1, logical_expr1, _)),
      (Env.TypeInformation.SF_theorem (h2, n2, ntyvar2, logical_expr2, _, _)))
   | ((Env.TypeInformation.SF_theorem (h1, n1, ntyvar1, logical_expr1, _, _)),
      (Env.TypeInformation.SF_theorem
         (h2, n2, ntyvar2, logical_expr2, _, _))) ->
        (* First, ensure that the names are the same. *)
        if n1 = n2 then
          (begin
            (* Now ensure that there is the same number ot type variables. *)
            assert (ntyvar1 = ntyvar2);
            (* Finally, ensure that the propositions are the same. *)
            if Ast_equal.logical_expr_equal_p logical_expr1 logical_expr2 then
              (* Return the theorem in case of property / theorem and return
                 the last theorem in case of theorem / theorem. *)
              phi2
            else
              raise
                (Logical_statements_mismatch
                   (n1, h1.Env.fh_initial_apparition,
                    logical_expr1.Parsetree.ast_loc,
                    h2.Env.fh_initial_apparition,
                    logical_expr2.Parsetree.ast_loc))
           end)
        else assert false
   | ((Env.TypeInformation.SF_theorem (h1, n1, ntyvar1, logical_expr1, _, _)),
      (Env.TypeInformation.SF_property (h2, n2, ntyvar2, logical_expr2, _))) ->
        (* First, ensure that the names are the same. *)
        if n1 = n2 then
          (begin
            (* Now ensure that there is the same number ot type variables. *)
            assert (ntyvar1 = ntyvar2);
            (* Finally, ensure that the propositions are the same. *)
            if Ast_equal.logical_expr_equal_p logical_expr1 logical_expr2
            then phi1 (* Return the theorem. *)
            else
              raise
                (Logical_statements_mismatch
                   (n1, h1.Env.fh_initial_apparition,
                    logical_expr1.Parsetree.ast_loc,
                    h2.Env.fh_initial_apparition,
                    logical_expr2.Parsetree.ast_loc))
          end)
        else assert false
   | _ -> assert false (* From Virgile's thesis Lemma 8 p 37 *)
;;



(* ************************************************************************ *)
(* Env.TypeInformation.species_field ->                                     *)
(*   Env.TypeInformation.species_field list ->                              *)
(*     (Env.TypeInformation.species_field option *                          *)
(*      Env.TypeInformation.species_field list *                            *)
(*      Env.TypeInformation.species_field list)                             *)
(** {b Descr} : Searches in the list of fields [fields] the oldest field
    sharing a name in common with the field [phi]. Then it returns this
    field of [fields] and the list [fields] itself minus the found field
    splitted in two parts :
      - the head formed by the elements before the found field,
      - the tail formed by the elements after the found field.
    This function intends to serves in the normalization algorithm described
    in Virgile Prevosto's Phd, Section 3.7.1, page 36. It addresses the
    problem of "finding i_0 the smallest index such as
    N(phi) inter N(psi_0) <> empty".

    Since the list is ordered in the inheritance direction, i.e. oldest
    inherited fields in head, the oldest is the first found in the list.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let oldest_inter_n_field_n_fields phi fields =
  let flat_phi_names =
    (match phi with
     | Env.TypeInformation.SF_sig (_, v, _)
     | Env.TypeInformation.SF_let (_, v, _, _, _, _, _, _) -> [v]
     | Env.TypeInformation.SF_let_rec l ->
         List.map (fun (_, v, _, _, _, _, _, _) -> v) l
     | Env.TypeInformation.SF_theorem (_, v, _, _, _, _) -> [v]
     | Env.TypeInformation.SF_property (_, v, _, _, _) -> [v]) in
  (* We will now check for an intersection between the list of names from phi
     and the names of one field of the argument [fields]. *)
  let rec rec_hunt = function
    | [] -> (None, [], [])
    | f :: rem_f ->
      (begin
        match f with
        | Env.TypeInformation.SF_sig (_, v, _)
        | Env.TypeInformation.SF_let (_, v, _, _, _, _, _, _)
        | Env.TypeInformation.SF_theorem (_, v, _, _, _, _)
        | Env.TypeInformation.SF_property (_, v, _, _, _) ->
            if List.mem v flat_phi_names then ((Some f), [], rem_f)
            else
              let (found, head_list, rem_list) = rec_hunt rem_f in
              (* Not found in [f], then add it in the head part. *)
              (found, (f :: head_list), rem_list)
        | Env.TypeInformation.SF_let_rec l ->
            let names_in_l = List.map (fun (_, v, _, _, _, _, _, _) -> v) l in
            if Handy.list_intersect_p flat_phi_names names_in_l then
              ((Some f), [], rem_f)
            else
              let (found, head_list, rem_list) = rec_hunt rem_f in
              (* Not found in [f], then add it in the head part. *)
              (found, (f :: head_list), rem_list)
      end) in
  rec_hunt fields
;;


(** {b Descr} : Implement the silently "described" notion of conflict
    detection mentionned in Virgile Prevosto's Phd page 57 line 6. *)
let non_conflicting_fields_p f1 f2 =
  match (f1, f2) with
   | (Env.TypeInformation.SF_sig (_, v1, sch1),
      Env.TypeInformation.SF_sig (_, v2, sch2))
   | (Env.TypeInformation.SF_let (_, v1, _, sch1, _, _, _, _),
      Env.TypeInformation.SF_sig (_, v2, sch2))
   | (Env.TypeInformation.SF_sig (_, v1, sch1),
      Env.TypeInformation.SF_let (_, v2, _, sch2, _, _, _, _)) ->
        (* If signatures/lets wear the same names and have the same scheme
           then fields are not conflicting. *)
        if v1 = v2 then
          (begin
          let ty1 = Types.specialize sch1 in
          let ty2 = Types.specialize sch2 in
          try
            ignore
              (Types.unify
                 ~loc: Location.none ~self_manifest: None ty1 ty2);
            (* Unification succeeded so fields have same type. *)
            true
          with _ -> false  (* Unification failed, then types are conflicting. *)
          end)
        else false (* Not the same fields names, then fields are conflicting. *)
   | (Env.TypeInformation.SF_sig (_, v1, sch1),
      (Env.TypeInformation.SF_let_rec l2))
   | ((Env.TypeInformation.SF_let_rec l2),
      Env.TypeInformation.SF_sig (_, v1, sch1)) ->
        (* There is no conflict if one of the rec-bound identifiers wears the
           same name than the signature and have the same type. *)
        List.exists
          (fun (_, v, _, sch, _, _, _, _) ->
            if v = v1 then
              (begin
              (* For each unification, take a fresh scheme for the signature
                 to prevent being poluted by previous unifications. *)
              let ty1 = Types.specialize sch1 in
              let t = Types.specialize sch in
              try
                ignore
                  (Types.unify ~loc: Location.none ~self_manifest: None ty1 t);
                (* Unification succeeded so fields have same type. *)
                true
              with _ -> false  (* Unification failed: types are conflicting. *)
              end)
            else false)
          l2
   | (Env.TypeInformation.SF_let (from1, v1, _, _, _, _, _, log1),
      Env.TypeInformation.SF_let (from2, v2, _, _, _, _, _, log2)) ->
        (* Unless fields are wearing the same name and are coming from the
           same species, we consider that 2 let definitions are conflicting.
           We could go further, applying equality modulo alpha-conversion but
           we don't do for the moment. *)
        (v1 = v2) &&
        (from1.Env.fh_initial_apparition = from2.Env.fh_initial_apparition) &&
        (log1 = log2)
   | (Env.TypeInformation.SF_theorem (from1, v1, _, _, _, _),
      Env.TypeInformation.SF_theorem (from2, v2, _, _, _, _)) ->
       (* Since we don't want to inspect proofs, theorems are compatible only
          if they wear the same names and come from the same species. *)
       (v1 = v2) &&
       (from1.Env.fh_initial_apparition = from2.Env.fh_initial_apparition)
   | (Env.TypeInformation.SF_property (_, v1, _, b1, _),
      Env.TypeInformation.SF_property (_, v2, _, b2, _)) ->
        (* Two properties are the same if they are wearing the same name and
           have the same logical expression as body. *)
        v1 = v2 && Ast_equal.logical_expr_equal_p b1 b2
   | ((Env.TypeInformation.SF_let_rec l1),
      (Env.TypeInformation.SF_let_rec l2)) ->
        (* Same thing than for lets but on all the bound names. *)
       (begin
       try
         List.for_all2
           (fun (from1, v1, _, _, _, _, _, log1)
                (from2, v2, _, _, _, _, _, log2) ->
             (from1.Env.fh_initial_apparition =
              from2.Env.fh_initial_apparition) && (v1 = v2) && (log1 = log2))
           l1 l2
       with Invalid_argument "List.for_all2" -> false
       end)
   | (_, _) -> false
;;



(* ****************************************************************** *)
(** {b Descr} : Implements the normalization algorithm described in
    Virgile Prevosto's Phd, Section 3.7.1, page 36 plus its extention
    to properties and theorems in Section 3.9.7, page 57.

    ATTENTION: Something that was not told in Virgile's Phd: in case
    of inheriting a field several times via the SAME parent, erasing
    must not be performed ! That's like if we did as if there was no
    erasing to do.

    {b Args}:
      - [loc] : Location of the whole hosting species expression.

    {b Exported}: No.                                                 *)
(* ****************************************************************** *)
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
          | (None, _, _) ->
              w1 := bigX;
              w2 := !w2 @ [phi]
          | (Some psi_i0, head_sniped_w2, tail_sniped_w2) ->
              (begin
              (* Extract the names forming the erasing context. *)
              let psi_i0_names =
                Dep_analysis.ordered_names_list_of_fields [psi_i0] in
              w1 :=  (fields_fusion ~loc ctx psi_i0 phi) :: bigX;
              (* Apply the formula Section of Section 3.9.7, page 57.
                 Hence we erase in the tail of the list, i.e. in fields found
                 after [psi_i0]. *)
              let current_species =
                (match ctx.current_species with
                 | None -> assert false
                 | Some sp_name -> sp_name) in
              (* We only erase if the 2 found fields are conflicting. *)
              let erased_tail =
                if non_conflicting_fields_p phi psi_i0 then tail_sniped_w2
                else
                  Dep_analysis.erase_fields_in_context
                    ~current_species psi_i0_names tail_sniped_w2 in
              w2 := head_sniped_w2 @ erased_tail
              end)
         end)
  done;
  !w2
;;



(* *********************************************************************** *)
(* typing_context -> Env.TypeInformation.species_field list -> unit        *)
(** {b Descr} : Verifies that a species subject to become a collection
    is really fully defined. This means that this species must not contain
    any "declared" fields (i.e. SF_sig) except for "rep" who must be
    declared (of course, its declaration is also its definition !).

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let ensure_collection_completely_defined ctx fields =
  (* Let just make a reference for checking the presence pf "rep" instead of
     passing a boolean flag. This way, the function keeps terminal. *)
  let rep_found = ref false in
  (* Because we want to have the warning about recursive functions without
     termination proof, we can't raise an exception ass soon as we find a
     method not defined because if the recursive function is after this non
     defined method, it won't be examined because of the exception raising.
     So, we force to examine all the fields of the species (a bit longuer,
     but anyway we must do this, so doing it here saves another descent on
     the list of methods). To remind if all the methods other than "rep" are
     defined, we use this list that records the non-defined methods. *)
  let non_defined_methods = ref [] in
  let rec rec_ensure = function
    | [] -> ()
    | field :: rem_fields ->
      (begin
        match field with
        | Env.TypeInformation.SF_sig (_, vname, _) ->
          if vname = (Parsetree.Vlident "rep") then rep_found := true
          else
           (begin
             match ctx.current_species with
             | None -> assert false
             | Some _ ->
                 (* Signature, hence non-defined ! *)
                 non_defined_methods :=
                   (NDMK_prototype vname) :: !non_defined_methods
            end)
        | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _) -> ()
        | Env.TypeInformation.SF_let_rec l ->
            List.iter
              (fun (_, vname, _, _, _, otp, _, _) ->
                if otp = None then
                  (begin
                  (* Recursive functions are fully defined if they have
                     proofs. We only raise an error if the option
                     "-impose-termination-proof" is enabled. Otherwise we only
                     generate a warning. *)
                  match ctx.current_species with
                   | None -> assert false
                   | Some curr_spec ->
                       if Configuration.get_impose_termination_proof () then
                         non_defined_methods :=
                           (NDMK_termination_proof vname)
                           :: !non_defined_methods
                       else
                         (begin
                         (* Since just a warning, it's not a reason of
                            non-definition. Then, nothing to add into the
                            list. *)
                         Format.eprintf
                           "@[%tWarning:%t@ Species@ '%t%a%t'@ should@ not@ \
                           be@ turned@ into@ a@ collection.@ Field@ \
                           '%t%a%t'@ does@ not@ have@ a@ termination@ \
                           proof. Proof@ is@ assumed.@]@."
                           Handy.pp_set_bold Handy.pp_reset_effects
                           Handy.pp_set_underlined
                           Sourcify.pp_qualified_species curr_spec
                           Handy.pp_reset_effects
                           Handy.pp_set_underlined
                           Sourcify.pp_vname vname
                           Handy.pp_reset_effects
                         end)
                  end))
              l
        | Env.TypeInformation.SF_theorem (_, _, _, _, _, _) -> ()
        | Env.TypeInformation.SF_property (_, vname, _, _, _) ->
            (begin
            (* A property does not have proof. So, it is not fully defined. *)
            match ctx.current_species with
             | None -> assert false
             | Some _ ->
                 non_defined_methods :=
                   (NDMK_prototype vname) :: !non_defined_methods
           end)
       end);
      rec_ensure rem_fields in
  (* Now do the job... *)
  rec_ensure fields ;
  (* Finally, ckeck if the carrier "rep" was actually found. *)
  if not !rep_found then
    non_defined_methods :=
      (NDMK_prototype (Parsetree.Vlident "rep")) :: !non_defined_methods ;
  match !non_defined_methods with
   | [] -> ()   (* Right, all was defined. *)
   | any ->
       (begin
       match ctx.current_species with
        | None -> assert false
        | Some curr_spec ->
            (* There was at least one missing defined method. Raise the error
               telling the names of non-defined methods. *)
            raise (Collection_not_fully_defined (curr_spec, any))
     end)
;;



(* ************************************************************************ *)
(* loc: Location.t -> Env.TypeInformation.species_field -> unit             *)
(** {b Descr} : Detects and reject polymorphic methods. This proces is done
    once the fusion of inherited methods and current methods is done.
    In effect, it is possible to have an inherited signature specifying a
    non polymorphic type and a definition of this method not specifying the
    types (hence that could seem polymorphic). The expected behaviour is
    that the type constraint inherited comes and apply to the method
    definition to in fact make it non polymorphic.
    For instance:
      species A =
        sig equal : Self -> Self -> basics#bool;
      end
;;
     species B inherits A =
       let equal (x, y) = true;
     end
;;
    In A "equal" is not polymorphic since its type is explicitely given.
    However, in "B", no type constraint is done. Hence the "equal" method
    in B looks 'a -> 'b -> bool. But because it is the same than in A, it
    really has the type Self -> Self -> bool ad must not be rejected.
    When the species is normalized and inherited fields a fusionned,
    unification is performed between fields of the same name. Then the
    polymorphic-like scheme of "equal" in B gets instancied by the one of
    "equal" in A, and doesn't look anymore polymorphics. That's why the
    polymorphic hunt must be done only once fields have been fusionned.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let detect_polymorphic_method ~loc = function
  | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
  | Env.TypeInformation.SF_property (_, _, _, _, _) ->
      (* Type of theorems and properties is always trivially "Prop". So it is
         never ploymorphic. *)
      ()
  | Env.TypeInformation.SF_sig (_, name, sch)
  | Env.TypeInformation.SF_let (_, name, _, sch, _, _, _, _) ->
      if Types.scheme_contains_variable_p sch then
        raise (Scheme_contains_type_vars (name, sch, loc))
  | Env.TypeInformation.SF_let_rec defs ->
      List.iter
        (fun (_, name, _, sch, _, _, _, _) ->
          if Types.scheme_contains_variable_p sch then
            raise (Scheme_contains_type_vars (name, sch, loc)))
        defs
;;



(* ******************************************************************** *)
(** {b Descr} : Search the [from_history] of the field [name] among the
    list of fields [fields]. Also return a boolean telling if the found
    field is defined (opposed to declared).
    This function must be called on a list in normal form, hence having
    no double methods.
    Attention, if the name is "rep" and if the carrier is not defined,
    this function will raise [Not_found].

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let find_field_history_and_if_defined_by_name name fields =
  let rec rec_search = function
    | [] -> raise Not_found
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_let (from, n, _, _, _, _, _, _)
         | Env.TypeInformation.SF_theorem (from, n, _, _, _, _) ->
             (* let and theorem are "defined" -> true. *)
             if n = name then (from, true) else rec_search q
         | Env.TypeInformation.SF_sig (from, n, _) ->
             if n = name then
               (* Be careful, if the signature is "rep", then it is DEFINED ! *)
               let defined_p = n = (Parsetree.Vlident "rep") in
               (from, defined_p)
             else rec_search q
         | Env.TypeInformation.SF_property (from, n, _, _, _) ->
             (* property is only "declared" -> false. *)
             if n = name then (from, false) else rec_search q
         | Env.TypeInformation.SF_let_rec l ->
             (begin
             try
               let (from, _, _, _, _, _, _, _) =
                 List.find (fun (_, n, _, _, _, _, _, _) -> n = name) l in
               (* let rec is "defined" -> true. *)
               (from, true)
             with Not_found -> rec_search q
             end)
        end) in
  (* Let's do the job... *)
  rec_search fields
;;



(* ******************************************************************** *)
(* Parsetree.qualified_species list list -> Parsetree.qualified_species *)
(* ******************************************************************** *)
let find_longuest_common_prefix found_inherited_alongs =
  (* Local function that compute the common part of 2 paths. *)
  let rec follow old_path new_path =
    match (old_path, new_path) with
     | (_, []) | ([], _) -> []
     | ((h1 :: q1), (h2 :: q2)) ->
         if h1 = h2 then h1 :: (follow q1 q2) else [] in
  match found_inherited_alongs with
   | [] ->
       (* Since the processed species is assumed to inherits something, we
          should never arrive here. *)
       assert false
   | h :: q ->
       (* Accumulates the computation of the common part of each path. *)
       let found_common_path =
         List.fold_left (fun accu path -> follow accu path) h q in
       (* Now, we must get the deepest species of the path, i.e. the oldest
          common one to all the processed paths. *)
       (try Handy.list_last_elem found_common_path
       with Not_found -> assert false)
;;



(* ********************************************************************** *)
(** {b Descr} : Check if a proof could be done earlier because it has
    dependencies on methods that are all inherited.
    We have as input the list of "proof of" we found. Since "proof of"'s
    are not allowed to exists without related property, and since they
    are incrementally collapsed together, if we found "proof of"'s at
    this inheritance level that's because the SF_proof was present in the
    species at the *current* inheritance level. So we are sure that in
    the fields list of the species we will find the related theorem.
    For each theorem, wearing th esame name than the proof, we will search
    in the dependency graph the node representing this theorem (it is
    mandatory it exists as stated above.
    From this node, we will check if its dependency children were already
    defined/declared (according to the kind decl/def of the dependency)
    before the current species.
    Note that in case of dependency on "rep", since it is always declared
    even if there is no related signature, we must handle it in a special
    way. In effect, in case of decl-dependency on "rep", we must signal
    that the proof could be done earlier ... only if the species inherits.
    In other words, only if there is a previous level where "rep" was
    already implicitly declared. This prevent from signaling that a proof
    could be done earlier when a guy writes a property and a proof
    separatly but in the same species that doesn't inherit and that the
    proof only has a decl-dependency on "rep". In effect, in this case,
    there is no other dependency on methods than "rep", and since "rep"
    is always declared, we would say, "rep" is already declared, to the
    proof could be done before ! But in fact, there is no "before".

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let check_if_proof_could_be_done_earlier ~current_species ~inherits_p proof_of
    fields original_properties_from_histories species_depgraph_nodes =
  (* Recover the name of the theorem the proof ... proves. *)
  let th_name = proof_of.Parsetree.ast_desc.Parsetree.pd_name in
  (* The list of inheritance paths found for each dependency. We will search
     for a longuest common prefix inside to determine in which species all
     id the sooner available to make the proof.
     By default, to handle the case where there is no dependencies or if there
     is only a decl-dependency on the carrier, the earliest point where the
     proof can be done is directly when the property is stated. So, we
     initialize the list of path with the history of the property related to
     this proof. *)
  let seed =
    (try
      let fr = List.assoc th_name original_properties_from_histories in
      (List.map (fun (s, _, _) -> s) fr.Env.fh_inherited_along) @
      [fr.Env.fh_initial_apparition]
    with
    | Not_found ->
        (* Since we forbid "proof of" not related to a property, obviously each
           proof must have been collapsed, hence we must find a theorem with the
           same name than th proof in the assoc list. *)
        assert false) in
  let found_inherited_alongs =
    ref ([seed] : Parsetree.qualified_species list list) in
  (* Now, recover the theorem dependency graph's node. *)
  let th_node =
    (try
      List.find
        (fun node -> node.DepGraphData.nn_name = th_name) species_depgraph_nodes
    with Not_found -> assert false) in
  (* Now, for each dependency child, we check if it is inherited. *)
  let may_be =
    List.for_all
      (fun (child_node, dependency_kind) ->
        try
          let (from, defined_p) =
            find_field_history_and_if_defined_by_name
              child_node.DepGraphData.nn_name fields in
          (* Depending on the dependency kind, we must check is the method needs
             to be defined or simply declared. *)
          match dependency_kind with
           | DepGraphData.DK_decl _ ->
               (* Since the dependency is only a decl, no matter if the method
                  we depend on is defined or declared. We must just check if
                  the method we depend on is inherited. *)
               let tmp = current_species <> from.Env.fh_initial_apparition in
               if tmp then
                 (begin
                 let full_path =
                   (List.map (fun (s, _, _) -> s) from.Env.fh_inherited_along) @
                   [from.Env.fh_initial_apparition] in
                 found_inherited_alongs :=
                   full_path :: !found_inherited_alongs;
                 tmp
                 end)
               else false
           | DepGraphData.DK_def _ ->
               (* Since the dependency is only a def, we require to have the
                  method we depend on to be DEFINED ! Declared is not
                  sufficient. We also must check if the method we depend on is
                  inherited. *)
               let tmp =
                 (current_species <> from.Env.fh_initial_apparition) &&
                 defined_p in
               if tmp then
                 (begin
                 let full_path =
                   (List.map (fun (s, _, _) -> s) from.Env.fh_inherited_along) @
                   [from.Env.fh_initial_apparition] in
                 found_inherited_alongs :=
                   full_path :: !found_inherited_alongs;
                 tmp
                 end)
               else false
        with Not_found ->
          (begin
          (* Handle the special case of "rep" that is always declared even if
             there is no related field in the AST. *)
          if child_node.DepGraphData.nn_name = Parsetree.Vlident "rep" then
            match dependency_kind with
             | DepGraphData.DK_decl _ ->
                 (* If the dependency is a decl, then we must signal that the
                    proof could be done earlier only if there is a "before".
                    Hence, only if the species inherits. *)
                 inherits_p
             | DepGraphData.DK_def _ ->
                 (* Since we didn't find "rep", this means that it is not
                    defined, hence since the dependency is a def, the proof
                    can't anyway be done. Normaly, the dependency calculus
                    should prevent us from falling in this case. *)
                 false
          else assert false
          end))
      th_node.DepGraphData.nn_children in
  if may_be then
    (begin
    let where = find_longuest_common_prefix !found_inherited_alongs in
    (* Since all the methods have the same initial species in head of the path,
       that is the current species, we must ensure that we are not telling that
       the proof could be done earlier ... "in the current species" ! *)
    if where <> current_species then
      Format.eprintf
        "@[%tWarning:%t@ In@ species@ '%t%s#%a%t',@ proof@ of@ '%t%a%t'@ \
        could@ be@ done@ earlier@ in@ '%t%s#%a%t'@."
        Handy.pp_set_bold Handy.pp_reset_effects
        Handy.pp_set_underlined
        (fst current_species) Sourcify.pp_vname (snd current_species)
        Handy.pp_reset_effects
        Handy.pp_set_underlined Sourcify.pp_vname th_name Handy.pp_reset_effects
        Handy.pp_set_underlined
        (fst where) Sourcify.pp_vname (snd where)
        Handy.pp_reset_effects
     end)
;;



(* ******************************************************************* *)
(** {b Descr} : Helper-type to record the various information from the
    typechecking pass needed to go to the code generation pass.
    This is mostly obvious and self-describing.

    {b Exported} : Yes.                                                *)
(* ******************************************************************* *)
type please_compile_me =
  | PCM_annotation_title
  | PCM_use of (Location.t * Parsetree.module_name)
  | PCM_open of (Location.t * Parsetree.module_name)
  | PCM_coq_require of
      (** Coq modules to open due to external definitions. This allow to make
          external Coq files where these definitions lie visible in Coq, without
          having to search in the whole FoCaL source file for [external_expr]s
          that imply a reference to a module in the string they contain. *)
      Parsetree.module_name
  | PCM_species of
      ((** The species expression. *)
        Parsetree.species_def *
        (** The species description from the typechecking pass, with the
            list of methods contained in its normalized form, with
            "oldestly" inherited in head of the list. *)
        Env.TypeInformation.species_description *
        (** The depency graph of the species's methods. *)
        (DepGraphData.name_node list))
  | PCM_collection of
      ((** The collection expression. *)
       Parsetree.collection_def *
       (** The collection description from the typechecking pass, with
           the list of methods contained in its normalized form, with
           "oldestly" inherited in head of the list and Self replaced by
           the collection name inside. *)
       Env.TypeInformation.species_description *
         (** The depency graph of the collection's methods. *)
         (DepGraphData.name_node list))
  | PCM_testing of Parsetree.testing_def 
  | PCM_type of (Parsetree.vname * Env.TypeInformation.type_description)
  | PCM_let_def of (Parsetree.let_def * (Types.type_scheme list))
  | PCM_theorem of
      (Parsetree.theorem_def *
       (** The mapping of type variables found in the "forall" and "exists" in
           the theorem's logical expressions onto their name. *)
       ((Parsetree.vname * Types.type_simple) list))
  | PCM_expr of Parsetree.expr
;;



let ensure_no_proof_of_doubles proofs_of =
  let rec rec_ensure = function
    | [] -> ()
    | (h, _) :: q ->
        let h_name = h.Parsetree.ast_desc.Parsetree.pd_name in
        (* Search in the remaining "proof of"s. *)
        List.iter
          (fun (p, _) ->

            if p.Parsetree.ast_desc.Parsetree.pd_name = h_name then
              raise
                (Proof_of_multiply_defined
                   (h.Parsetree.ast_loc, h_name, p.Parsetree.ast_loc)))
          q;
        rec_ensure q in
  rec_ensure proofs_of
;;



(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.species_def ->             *)
(*  (Types.type_simple * Env.TypingEnv.t)                                    *)
(** {b Descr} : Typechecks a species definition. Il infers its signature and
    bind it to the species name in the environment. Finally, adds a type
    binding representing the species's carrier type.
    Also performs the interface printing stuff of a species.
    It returns both the extended environment and the species's carrier type.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let typecheck_species_def ctx env species_def =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let current_species =
    (ctx.current_unit, species_def_desc.Parsetree.sd_name) in
  if Configuration.get_verbose () then
    Format.eprintf "Typechecking species '%a'.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name;
  (* First of all, we are in a species !!! *)
  let ctx = { ctx with current_species = Some current_species } in
  (* Extend the environment with the species param and synthetize the species
     type of the current species. A priori [_self_must_be1] should always be
     an empty list since expressions of parameters are not allowed to use
     [Self]. So ther eis no reason to have found that [Self] must be compatible
     with a species type while processing parameters. *)
  let (env_with_species_params, sig_params, _self_must_be1) =   (*** ***)
    typecheck_species_def_params
      ctx env species_def_desc.Parsetree.sd_params in
  (* We first load the inherited methods in the environment and get their
     signatures and methods information by the way.
     We also get a possibly new context where the fact that Self is now
     manifest is updated, in case we inherited a [repr]. *)
  let (inherited_methods_infos,
       env_with_inherited_methods,
       ctx_with_inherited_repr,
       _self_must_be2) = (*** [Unsure] ***)
    extend_env_with_inherits
      ~current_species ~loc: species_def.Parsetree.ast_loc ctx
      env_with_species_params
      species_def_desc.Parsetree.sd_inherits.Parsetree.ast_desc in
  (* Now infer the types of the current field's and recover the context  where
     we may know the shape of [repr]. *)
  let (methods_info, ctx', found_proofs_of, (* [Unsure] *) _) =
    typecheck_species_fields
      ctx_with_inherited_repr env_with_inherited_methods
      species_def_desc.Parsetree.sd_fields in
  (* We ensure that there is not several times a same "proof of". *)
  ensure_no_proof_of_doubles found_proofs_of;
  (* We first collapse "proof-of"s with their related property to lead to a
     theorem. *)
  let (collapsed_inherited_methods_infos, collapsed_methods_info,
      original_properties_from_histories) =
    collapse_proofs_of
      ~current_species found_proofs_of inherited_methods_infos methods_info in
  if Configuration.get_verbose () then
    Format.eprintf
      "Normalizing species '%a'.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name;
  (* Then one must ensure that each method has the same type everywhere in the
     inheritance tree and more generaly create the normalised form of the
     species. *)
  let normalized_methods =
    normalize_species
      ~loc: species_def.Parsetree.ast_loc ctx' collapsed_methods_info
      collapsed_inherited_methods_infos in
  (* Ensure that the species is well-formed. *)
  Dep_analysis.ensure_species_well_formed ~current_species normalized_methods;
  (* Ensure that no method is polymorphic  (c.f. Virgile Prevosto's Phd section
     3.3, page 24). *)
  List.iter
    (detect_polymorphic_method ~loc: species_def.Parsetree.ast_loc)
    normalized_methods;
  (* Now, compute the fields order to prevent ill-formness described in the
     [collapse_proofs_of] function's header. In effect, we may have injected in
     the fresh species fields some theorem obtained by merging inherited
     properties whose proofs were given in the current species. Hence, the
     order of insertion may not be correct according to the really defined here
     fields of the species. So, let's reorganize topologically according to the
     dependencies. *)
  let new_order_for_normalizes =
    Dep_analysis.compute_fields_reordering
      ~current_species normalized_methods in
  (* Now really re-order the normalized fields. *)
  let reordered_normalized_methods =
    order_fields_according_to new_order_for_normalizes normalized_methods in
  if Configuration.get_verbose () then
    Format.eprintf "Computing dependencies inside species '%a'.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name;
  (* The methods are now completly correct, i.e. with no multiple times the
     same name as it can be before the normalization process, we get the
     species's final dependency graph. *)
  let species_dep_graph =
    Dep_analysis.build_dependencies_graph_for_fields
      ~current_species reordered_normalized_methods in
  (* Check the warning telling that a proof could be done earlier. *)
  let inherits_p =
    species_def_desc.Parsetree.sd_inherits.Parsetree.ast_desc <> [] in
  List.iter
    (fun (proof_of, _) ->
      check_if_proof_could_be_done_earlier
        ~current_species ~inherits_p proof_of
        reordered_normalized_methods original_properties_from_histories
        species_dep_graph)
    found_proofs_of;
  (* If asked, generate the dotty output of the dependencies. *)
  (match Configuration.get_dotty_dependencies () with
   | None -> ()
   | Some dirname ->
       Dep_analysis.dependencies_graph_to_dotty
         ~dirname ~current_species species_dep_graph);
  (* Check whether the collection is fully defined. If so, then at code
     generation-time, then en collection generator must be created. Note that
     this is independant of the fact to be a collection. *)
  let is_closed =
    (try
       ensure_collection_completely_defined ctx reordered_normalized_methods;
       (* If the check didn't fail, then the species if fully defined. *)
       true with
     | Collection_not_fully_defined _ -> false) in
  (* Let's build our "type" information. Since we are managing a species
     and NOT a collection, we must set [spe_kind] to [SCK_toplevel_species]. *)
  let species_description = {
    Env.TypeInformation.spe_kind = Types.SCK_toplevel_species;
    Env.TypeInformation.spe_is_closed = is_closed;
    Env.TypeInformation.spe_sig_params = sig_params;
    Env.TypeInformation.spe_sig_methods = reordered_normalized_methods;
    Env.TypeInformation.spe_dep_graph = species_dep_graph } in
  (* If asked, generate the textual output of the methods history. *)
  (match Configuration.get_methods_history_to_text () with
   | None -> ()
   | Some dirname ->
       InfoOutput.methods_history_to_text
         ~dirname ~current_species reordered_normalized_methods) ;
  (* If asked, generate the dotty output of the methods history. *)
  (match Configuration.get_methods_history_to_dotty () with
   | None -> ()
   | Some dirname ->
       InfoOutput.methods_history_to_dot
         ~dirname ~current_species reordered_normalized_methods) ;
  (* Extend the initial environment with the species. Not the environment
     used to typecheck the internal definitions of the species !!! *)
  let env_with_species =
    Env.TypingEnv.add_species
      ~loc: species_def.Parsetree.ast_loc
      species_def_desc.Parsetree.sd_name species_description env in
  (* We don't insert a type constructor that represents the species carrier
     even if the species is fully defined.
     This especially avoid such things:
       species A =
         signature f : int
       end ;;
       species B =
         signature b : A -> A
       end ;;
     If we generate the code for this, in ocaml we won't have any type
     definition for "me_as_carrier" in "A". And in "B", "g" will have the
     type "A.me_as_carrier -> A.me_as_carrier" where "A.me_as_carrier" is
     unbound. This is moral since "A" doesn't have a carrier. If it really has
     a carrier OCaml code will compile since it doesn't really make use of the
     type. However, Coq complains since the "effective_collection" doesn't
     exist. *)
  (* Record the type in the AST node. *)
  Types.begin_definition () ;
  let species_carrier_type =
    Types.type_rep_species
      ~species_module: ctx'.current_unit
      ~species_name:
        (Parsetree_utils.name_of_vname species_def_desc.Parsetree.sd_name) in
  Types.end_definition () ;
  species_def.Parsetree.ast_type <- Parsetree.ANTI_type species_carrier_type;
  if Configuration.get_verbose () then
    Format.eprintf "Species '%a' accepted.@."
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name;
  (* Interface printing stuff. *)
  if Configuration.get_do_interface_output () then
    (begin
    Format.printf "@[<2>species %a%a@]@\n"
      Sourcify.pp_vname species_def_desc.Parsetree.sd_name
      Env.TypeInformation.pp_species_description species_description
    end);
  (PCM_species (species_def, species_description, species_dep_graph),
   species_carrier_type, env_with_species)
;;



(* ************************************************************************ *)
(* typing_context -> is_repr_of_external: bool -> Env.TypingEnv.t ->        *)
(* Parsetree.regular_type_def_body ->                                        *)
(*   (Env.TypingEnv.t * Env.TypeInformation.type_description)               *)
(** {b Descr} : Transforms a simple type definition's body into a somewhat
    that can be inserted inside the environment. Also generates type
    constructors in case of a sum type definition and field labels in case
    of a record type definition.
    The passed context must already have its [tyvars_mapping] field
    initialized, telling the link between the type definition's parameters
    and the type variables they are related to.
    Both field labels and constructors with arguments are assigned a type
    scheme like a function taking as argument the field's type (or several
    arguments that are the constructor's arguments types, NOT THE TUPLE OF
    THEM) and returning a ST_construct embedding the record/sum type's name.
    For instance:
      [type t = A of int]
    will create a constructor [A : int -> t].
      [type t = B of (int, int)]
    will create a constructor [B : int -> int -> t].
      [type t = C of (int * int)]
    will create a constructor [C : (int * int) -> t].
    For other instance:
      [type u = { junk : string }]
    will create a field label [junk : string -> u]
    Sum type constructors with no argument are typed as constants of this
    type.
    Also performs the interface printing stuff is needed.

    {b Args} :
      - [~is_repr_of_external] : Boolean telling if the current type
         definition's body is in fact the "internal" representation of an
         external type definition. If it is the case, then the type
         description elaborated will not be inserted in the environment.
         It will only be built (and returned of course), then the guy who
         called us will be in charge to insert the correct type description
         to take into account that the type must not show its sum
         constructors or fields labels in its structure.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let typecheck_regular_type_def_body ctx ~is_repr_of_external env type_name
    regular_type_def_body =
  (* Recover on which type variables the parameters are mapped. *)
  let vars_of_mapping = List.map snd ctx.tyvars_mapping in
  (* Get the type constructor's arity. *)
  let nb_params = List.length vars_of_mapping in
  (* Insert ourselves in the environment in case of recursive definition. *)
  Types.begin_definition ();
  (* Make the type constructor... We know it's vname. Now its hosting module
     is the current one because it is defined inside it, eh ! *)
  let futur_type_type =
    Types.type_basic
      (Types.make_type_constructor
         ctx.current_unit (Parsetree_utils.name_of_vname type_name))
      vars_of_mapping in
  Types.end_definition ();
  let proto_identity =
    Types.build_type_def_scheme
      ~variables: vars_of_mapping  ~body: futur_type_type in
  let proto_descrip = {
    Env.TypeInformation.type_loc = regular_type_def_body.Parsetree.ast_loc;
    (* Make an "abstract" proto-type, even if it is of another kind, this is
       not important because will never be used. *)
    Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract;
    Env.TypeInformation.type_identity = proto_identity;
    Env.TypeInformation.type_params = vars_of_mapping;
    Env.TypeInformation.type_arity = nb_params } in
  (* Extend the environment with ourselves' proto-type. *)
  let env_with_proto_ourselves =
    Env.TypingEnv.add_type
      ~loc: regular_type_def_body.Parsetree.ast_loc type_name
      proto_descrip env in
  (* Process the body of the type definition. *)
  match regular_type_def_body.Parsetree.ast_desc with
   | Parsetree.RTDB_alias ty ->
       (begin
       (* We do not insert the defined name itself to reject recursive type
          abbreviations. *)
       Types.begin_definition ();
       (* This definition will only add a type name, no new type constructor. *)
       let identity_type =
         typecheck_type_expr ctx env_with_proto_ourselves ty in
       Types.end_definition ();
       (* Record the type representing this body in the AST node. *)
       regular_type_def_body.Parsetree.ast_type <-
         Parsetree.ANTI_type identity_type;
       (* Generalize the got type to get the real identity. *)
       let identity_scheme =
         Types.build_type_def_scheme
           ~variables: vars_of_mapping ~body: identity_type in
       let ty_descr = {
         Env.TypeInformation.type_loc =
           regular_type_def_body.Parsetree.ast_loc;
         Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract;
         Env.TypeInformation.type_identity = identity_scheme;
         Env.TypeInformation.type_params = vars_of_mapping;
         Env.TypeInformation.type_arity = nb_params } in
       (* Extend the ORIGINAL environment (not the one where we put our
          proto-type) by the type itself. Hence we are sur not to have the
          type twice. *)
       let env' =
         if is_repr_of_external then env
         else
           Env.TypingEnv.add_type
             ~loc: regular_type_def_body.Parsetree.ast_loc type_name
             ty_descr env in
       (* Return the extended environment and the type description. *)
       (env', ty_descr)
       end)
  | Parsetree.RTDB_union constructors ->
    (begin
      (* Now process the constructors of the type. Create the list of
         couples : (constructor name * type_simple). *)
      let cstr_bindings =
        List.map
          (fun (cstr_name, cstr_args) ->
            match cstr_args with
            | [] ->
              (* No argument for the constructor. So it's a constant. *)
              let cstr_descr = {
                Env.TypeInformation.cstr_arity = Env.TypeInformation.CA_zero;
                Env.TypeInformation.cstr_scheme =
                  Types.generalize futur_type_type } in
              (cstr_name, Env.TypeInformation.CA_zero, cstr_descr)
            | _ ->
              (* There are some argument(s). So the constructor is type as a
                 function taking as many argument(s) as the arity of the value
                 constructor and returning the type of the current
                 definition. *)
              Types.begin_definition ();
              let args_ty =
                List.map
                  (typecheck_type_expr ctx env_with_proto_ourselves)
                  cstr_args in
              (* Make a [ST_sum_arguments] of the arguments. *)
              let as_sum_arguments_ty = Types.type_sum_arguments args_ty in
              let arrow =
                Types.type_arrow as_sum_arguments_ty futur_type_type in
              Types.end_definition ();
              let cstr_descr = {
                Env.TypeInformation.cstr_arity = Env.TypeInformation.CA_some;
                Env.TypeInformation.cstr_scheme = Types.generalize arrow } in
              (cstr_name, Env.TypeInformation.CA_some, cstr_descr))
          constructors in
      (* And finally, extends the environment with the constructors. *)
      let env_with_constructors =
        List.fold_left
          (fun accu_env (cstr_name, _, cstr_descr) ->
           Env.TypingEnv.add_constructor cstr_name cstr_descr accu_env)
          env
          cstr_bindings in
      (* Record the type representing this body in the AST node. *)
      regular_type_def_body.Parsetree.ast_type <-
        Parsetree.ANTI_type futur_type_type;
      (* Now add the type itself. *)
      let type_identity =
        Types.build_type_def_scheme
          ~variables: vars_of_mapping ~body: futur_type_type in
      let final_type_descr = {
        Env.TypeInformation.type_loc = regular_type_def_body.Parsetree.ast_loc;
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_variant
            (List.map
               (fun (n, arity, descr) ->
                (n, arity, descr.Env.TypeInformation.cstr_scheme))
               cstr_bindings);
        Env.TypeInformation.type_identity = type_identity;
        Env.TypeInformation.type_params = vars_of_mapping;
        Env.TypeInformation.type_arity = nb_params } in
      (* Extend the environment by the type itself. *)
      let env' =
        if is_repr_of_external then env_with_constructors
        else
          Env.TypingEnv.add_type
            ~loc: regular_type_def_body.Parsetree.ast_loc type_name
            final_type_descr env_with_constructors in
      (* Return the extended environment and the type description. *)
      (env', final_type_descr)
     end)
  | Parsetree.RTDB_record labels ->
      (* First, we sort the label list in order to get a canonical
         representation of a record. *)
      let labels = Sort.list (fun (n1, _) (n2, _) -> n1 <= n2) labels in
      (* Now typecheck the fields of the record. *)
      let fields_descriptions =
        List.map
          (fun (lbl_name, lbl_ty_expr) ->
            Types.begin_definition ();
            let lbl_ty =
              typecheck_type_expr ctx env_with_proto_ourselves lbl_ty_expr in
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
      (* Record the type representing this body in the AST node. *)
      regular_type_def_body.Parsetree.ast_type <-
        Parsetree.ANTI_type futur_type_type;
      (* Now add the type itself. *)
      let type_identity =
        Types.build_type_def_scheme
          ~variables: vars_of_mapping ~body: futur_type_type in
      let final_type_descr = {
        Env.TypeInformation.type_loc = regular_type_def_body.Parsetree.ast_loc;
        Env.TypeInformation.type_kind =
          Env.TypeInformation.TK_record
            (List.map
               (fun (lbl_name, lbl_descr) ->
                 (lbl_name,
                  lbl_descr.Env.TypeInformation.field_mut,
                  lbl_descr.Env.TypeInformation.field_scheme))
               fields_descriptions);
          Env.TypeInformation.type_identity = type_identity;
          Env.TypeInformation.type_params = vars_of_mapping;
          Env.TypeInformation.type_arity = nb_params } in
      (* Extend the environment by the type itself. *)
    let env' =
      if is_repr_of_external then env_with_labels
      else
        Env.TypingEnv.add_type
          ~loc: regular_type_def_body.Parsetree.ast_loc
          type_name final_type_descr env_with_labels in
    (* Return the extended environment and the type description. *)
    (env', final_type_descr)
;;



(* ************************************************************************* *)
(** {b Descr} : Typecheck the body of an external type definition. If the
    definition doesn't have any "internal" representation, the type is
    considered as fully abstract. I.e. it has no mapping onto the FoCaL
    type algebra. The type name is then mapped onto a [ST_construct] with
    it's name as parameter of the [ST_construct] constructor.
    We return the typing environment extended by the bindings induced by
    the definition and the description of the type definition as entered
    in the environment's structure.
    Note that in the returned environment, there are not the type
    variables representing the parameters ! They are in the variable
    mapping of the context. Hence there is no risk to see them escaping
    outside the type's definition.

    {b Args} :
      - [ctx] : The current typing context in which the "variable mapping"
           (i.e. mapping from the definition's parameters to type variables)
            is already recorded.

      - [type_name] : The name of the type definition (i.e. the name of
          the type constructor defined by this ... definition).

      - [env] : The current typing environment (which will be extented by
          the current typechecking process).

      - [params] : The positionnal list of type variables the type
          definition's parameters are related to.

      - [external_type_def_body] : The external definition's body to
          typecheck.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let typecheck_external_type_def_body ctx env type_name params
    external_type_def_body =
  (* An external type definition "has no type". Record in the AST node. *)
  external_type_def_body.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  (* Same remark for the [external_expr] and the [external_bindings] telling
     how to map this external type definition. *)
  external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_external.
    Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_mapping.
    Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  List.iter
    (fun ext_binding ->
      ext_binding.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
      (* Propagate into the [external_expr] of the [external_binding]. *)
      let (_, ext_expr) = ext_binding.Parsetree.ast_desc in
      ext_expr.Parsetree.ast_type <- Parsetree.ANTI_irrelevant)
    external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_mapping.
      Parsetree.ast_desc;
  (* We build the type's structure and the environment where this type's name
     is bound to this structure. *)
  match external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_internal with
   | None ->
       (begin
       (* We will build an external type of this name with as many parameters
          we find in the [ctx.tyvars_mapping] list. *)
       Types.begin_definition ();
       (* Make the type constructor... We know it's vname. Now its hosting
          module is the current one because it is defined inside it, eh ! *)
       let ty =
         Types.type_basic
           (Types.make_type_constructor
              ctx.current_unit
              (Parsetree_utils.name_of_vname type_name))
           params in
       Types.end_definition ();
       let identity =
         Types.build_type_def_scheme ~variables: params ~body: ty in
       (* And now make the type's description to insert in the environment. *)
       let ty_descr = {
         Env.TypeInformation.type_loc =
           external_type_def_body.Parsetree.ast_loc;
         Env.TypeInformation.type_kind =
           Env.TypeInformation.TK_external
             (external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_external,
             external_type_def_body.Parsetree.ast_desc.Parsetree.etdb_mapping);
         Env.TypeInformation.type_identity = identity;
         Env.TypeInformation.type_params = params;
         Env.TypeInformation.type_arity = List.length params } in
       if Configuration.get_do_interface_output () then
         (begin
         Format.printf "@[<2>external@ type %a@ =@ %a@]@\n"
           Sourcify.pp_vname type_name Types.pp_type_scheme identity
         end);
       (* Return the extended environment. *)
       let final_env =
         Env.TypingEnv.add_type
           ~loc: external_type_def_body.Parsetree.ast_loc type_name ty_descr
           env in
       (final_env, ty_descr)
       end)
   | Some internal_repr ->
       (* Build the type structure of the "internal" representation. This will
          be used to build the real type's structure. *)
       let (env_without_type_def, internal_descr) =
         typecheck_regular_type_def_body
           ctx ~is_repr_of_external: true env type_name internal_repr in
       (* Force the type to be [TK_external] in order to prevent ot from
          being "generated". Even if this type has an internal structure,
          the generated code must always map on its external view ! *)
       let externalized_descr = {
         internal_descr with
           Env.TypeInformation.type_kind =
             Env.TypeInformation.TK_external
               (external_type_def_body.Parsetree.ast_desc.
                 Parsetree.etdb_external,
                external_type_def_body.Parsetree.ast_desc.
                  Parsetree.etdb_mapping) } in
       let final_env =
         Env.TypingEnv.add_type
           ~loc: internal_repr.Parsetree.ast_loc type_name
         externalized_descr env_without_type_def in
       (final_env, externalized_descr)
;;



let typecheck_type_def_body_simple
  ctx env td_name params type_def_body_simple =
  (* A type definition "has no type". Record in the AST node. *)
  type_def_body_simple.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  match type_def_body_simple.Parsetree.ast_desc with
   | Parsetree.TDBS_regular regular_type_def_body ->
       typecheck_regular_type_def_body
         ctx ~is_repr_of_external:false env td_name regular_type_def_body
   | Parsetree.TDBS_external external_type_def_body ->
       typecheck_external_type_def_body
         ctx env td_name params external_type_def_body
;;



let typecheck_type_def ctx env type_def =
  let type_def_desc = type_def.Parsetree.ast_desc in
  (* A type definition "has no type". Record in the AST node. *)
  type_def.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
  type_def_desc.Parsetree.td_body.Parsetree.ast_type <-
    Parsetree.ANTI_irrelevant;
  let { Parsetree.td_name = type_def_name;
        Parsetree.td_params = type_def_params;
        Parsetree.td_body = type_def_body;
      } = type_def_desc in
  (* First, extend the [tyvars_mapping] of the current context with parameters
     of the type definition. The position of variables in the mapping is and
     must be the same that in the type's parameters list. *)
  Types.begin_definition ();
  let vmapp =
    List.map
      (fun var_name -> (var_name, Types.type_variable ()))
      type_def_params in
  Types.end_definition ();
  let new_ctx = { ctx with tyvars_mapping = vmapp } in
  match type_def_body.Parsetree.ast_desc with
   | Parsetree.TDB_abstract type_def_body_simple ->
       typecheck_type_def_body_simple new_ctx env
         type_def_name (List.map snd vmapp) type_def_body_simple
   | Parsetree.TDB_private type_def_body_simple ->
       typecheck_type_def_body_simple new_ctx env
         type_def_name (List.map snd vmapp) type_def_body_simple
   | Parsetree.TDB_public type_def_body_simple ->
       typecheck_type_def_body_simple new_ctx env
         type_def_name (List.map snd vmapp) type_def_body_simple
   | Parsetree.TDB_relational type_def_body_simple ->
       typecheck_type_def_body_simple new_ctx env
         type_def_name (List.map snd vmapp) type_def_body_simple
;;




(* ************************************************************************* *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.coll_def ->                *)
(*   (Types.type_simple * Env.TypingEnv.t)                                   *)
(** {b Descr} : Typecheck a definition of collection. It recovers its fields
    and their types and verifies that the resulting species is fully defined.
    Once the collection is successfully built, it is added to the current
    environment.
    The function returns both the extended environment and the carrier type
    pf the species.

    {b Args} :
      - [ctx] : Current structure recording the various information
              required and propagated to typecheck.
      - [env] : The current typing environment mapping identifiers
              onto types.
      - [coll_def] : Collection definition to typecheck.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let typecheck_collection_def ctx env coll_def =
  let coll_def_desc = coll_def.Parsetree.ast_desc in
  if Configuration.get_verbose () then
    Format.eprintf "Typechecking collection '%a'.@."
      Sourcify.pp_vname coll_def_desc.Parsetree.cd_name;
  let current_species = (ctx.current_unit, coll_def_desc.Parsetree.cd_name) in
  (* First of all, we are in a species !!! *)
  let ctx = { ctx with current_species = Some current_species } in
  (* Typecheck the body's species expression .*)
  let ((species_expr_fields, _self_must_be, applied_substs), _, _) =
    (* [Unsure] self_must_be *)
    typecheck_species_expr ctx env coll_def_desc.Parsetree.cd_body in
  (* One must ensure that the collection is really a completely defined
     species. *)
  ensure_collection_completely_defined ctx species_expr_fields;
  (* Update the inheritance history, saying that in fact the collection
     "inherits" from the "implemented" species. *)
  let species_expr_fields =
    List.map
      (extend_from_history
         ~current_species ~current_unit: ctx.current_unit env
         coll_def_desc.Parsetree.cd_body applied_substs)
      species_expr_fields in
  let myself_coll_ty =
    (ctx.current_unit,
     (Parsetree_utils.name_of_vname coll_def_desc.Parsetree.cd_name)) in
  (* In the collection's fields, substitute Self <- the collection name. *)
  let collection_fields =
    List.map
      (SubstColl.subst_species_field
         ~current_unit: ctx.current_unit SubstColl.SRCK_self
          (Types.SBRCK_coll myself_coll_ty))
      species_expr_fields in
  (* Get the dependencies graph of the species. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "Computing dependencies inside collection '%a'.@."
      Sourcify.pp_vname coll_def_desc.Parsetree.cd_name;
  let collection_dep_graph =
    Dep_analysis.build_dependencies_graph_for_fields
      ~current_species collection_fields in
  (* If asked, generate the dotty output of the dependencies. *)
  (match Configuration.get_dotty_dependencies () with
   | None -> ()
   | Some dirname ->
       Dep_analysis.dependencies_graph_to_dotty
         ~dirname ~current_species collection_dep_graph);
  (* Let's build our "type" information. Since we are managing a collection
     and NOT a species, we must set [spe_kind] to [SCK_toplevel_collection]. *)
  let collec_description = {
    Env.TypeInformation.spe_kind = Types.SCK_toplevel_collection;
    Env.TypeInformation.spe_is_closed = true;            (* Obviously, eh ! *)
    Env.TypeInformation.spe_sig_params = [];
    Env.TypeInformation.spe_sig_methods = collection_fields;
    Env.TypeInformation.spe_dep_graph = collection_dep_graph } in
  (* Add this collection in the environment. *)
  let env_with_collection =
    Env.TypingEnv.add_species
      ~loc: coll_def.Parsetree.ast_loc
      coll_def_desc.Parsetree.cd_name collec_description env in
  (* Now, extend the environment with a type that is this collection. *)
  Types.begin_definition ();
  let collec_carrier_type =
    Types.type_rep_species
      ~species_module: ctx.current_unit
      ~species_name:
        (Parsetree_utils.name_of_vname coll_def_desc.Parsetree.cd_name) in
  Types.end_definition ();
  let collec_as_type_description = {
    Env.TypeInformation.type_loc = Location.none;
    Env.TypeInformation.type_kind = Env.TypeInformation.TK_abstract;
    Env.TypeInformation.type_identity = Types.generalize collec_carrier_type;
    (* Nevers parameters for a species's carrier type ! *)
    Env.TypeInformation.type_params = [];
    Env.TypeInformation.type_arity = 0 } in
  let full_env =
    Env.TypingEnv.add_type
      ~loc: coll_def.Parsetree.ast_loc
      coll_def_desc.Parsetree.cd_name collec_as_type_description
      env_with_collection in
  (* Record the type in the AST node. *)
  coll_def.Parsetree.ast_type <- Parsetree.ANTI_type collec_carrier_type;
  (* Interface printing stuff. *)
  if Configuration.get_do_interface_output () then
    Format.printf "@[<2>collection %a%a@]@\n"
      Sourcify.pp_vname coll_def_desc.Parsetree.cd_name
      Env.TypeInformation.pp_species_description collec_description;
  (PCM_collection (coll_def, collec_description, collection_dep_graph),
   collec_carrier_type, full_env)
;;

(* ****************************************************************** *)
(* typing_context -> Env.TypingEnv.t -> Parsetree.phrase ->           *)
(*   (please_compile_me * Env.TypingEnv.t)                            *)
(** {b Descr} : Performs type inference on a [phrase] and returns the
    initial environment extended with the possible type bindings
    induced by the [phrase].
    Also assign the infered type in the [ast_type] field of the
    [phrase] node.

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let typecheck_phrase ctx env phrase =
  match phrase.Parsetree.ast_desc with
   | Parsetree.Ph_annotation_title ->
       (PCM_annotation_title, env)
   | Parsetree.Ph_use fname ->
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
       (* Nothing to do, the scoping pass already ensured that "modules"
          opened or used were previously "use"-d. *)
       ((PCM_use (phrase.Parsetree.ast_loc, fname)), env)
   | Parsetree.Ph_open fname ->
       (* Load this module interface to extend the current environment. *)
       let env' =
         Env.type_open_module ~loc: phrase.Parsetree.ast_loc fname env in
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
       ((PCM_open (phrase.Parsetree.ast_loc, fname)), env')
   | Parsetree.Ph_coq_require fname ->
       (* Really nothing to do... *)
       ((PCM_coq_require fname), env)
   | Parsetree.Ph_species species_def ->
       (* Interface printing stuff is done inside. *)
       let (compil_info, ty, env') =
         typecheck_species_def ctx env species_def  in
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_type ty;
       (compil_info, env')
   | Parsetree.Ph_collection collection_def ->
       (* Interface printing stuff is done inside. *)
       let (compil_info, ty, env') =
         typecheck_collection_def ctx env collection_def in
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_type ty;
       (compil_info, env')
   | Parsetree.Ph_testing testing_def ->
       Format.eprintf
         "No type inference for testing instructions.@.";
       ((PCM_testing testing_def),env)
   | Parsetree.Ph_type type_def ->
       let (env', ty_descr) = typecheck_type_def ctx env type_def in
       (* Interface printing stuff must be bone inside. *)
       if Configuration.get_do_interface_output () then
         Format.printf "type ... @\n";  (* [Unsure] TODO. *)
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
       ((PCM_type (type_def.Parsetree.ast_desc.Parsetree.td_name, ty_descr)),
        env')
   | Parsetree.Ph_let let_def  ->
       let envt_bindings =
         typecheck_let_definition ~is_a_field: false ctx env let_def in
       (* Extend the current environment with the bindings induced the
          let-definition. *)
       let env' =
         List.fold_left
           (fun accu_env (id, ty_scheme, _) ->
             (* Interface printing stuff. *)
             if Configuration.get_do_interface_output () then
               Format.printf "val %a in %a@\n"
                 Sourcify.pp_vname id Types.pp_type_scheme ty_scheme;
             (* Extend the environment with the current binding. *)
             Env.TypingEnv.add_value
               ~toplevel: (Some let_def.Parsetree.ast_loc) id ty_scheme
               accu_env)
           env envt_bindings in
       (* Just recover the type scheme of each bound identifier. *)
       let bound_schemes = List.map (fun (_, sc, _) -> sc) envt_bindings in
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
       (* Return unit and the extended environment. *)
       ((PCM_let_def (let_def, bound_schemes)), env')
   | Parsetree.Ph_theorem theorem_def ->
       Types.begin_definition ();
       (* Get the theorem's type (trivially should be Prop) and the number of
          type variables found in the "forall" and "exists".
          They will lead to extra "forall ... : Set" in front of the theorem's
          logical expression. *)
       let (ty, polymorphic_vars_names) =
         typecheck_theorem_def ctx env theorem_def in
       Types.end_definition ();
       let scheme = Types.trivial_scheme (Types.type_prop ()) in
       let env' =
         Env.TypingEnv.add_value
           ~toplevel: (Some theorem_def.Parsetree.ast_loc)
           theorem_def.Parsetree.ast_desc.Parsetree.th_name scheme env in
       (* Interface printing stuff. *)
       if Configuration.get_do_interface_output () then
         Format.printf "theorem %a in %a@\n"
           Sourcify.pp_vname theorem_def.Parsetree.ast_desc.Parsetree.th_name
           Types.pp_type_simple ty;
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_scheme scheme;
       ((PCM_theorem (theorem_def, polymorphic_vars_names)), env')
   | Parsetree.Ph_expr expr ->
       let expr_ty = typecheck_expr ctx env expr in
       (* Store the type information in the phrase's node. *)
       phrase.Parsetree.ast_type <- Parsetree.ANTI_type expr_ty;
       (* No interface printing stuff because the expression is not bound. *)
       ((PCM_expr expr), env)
;;



(* ********************************************************************* *)
(* current_unit: Types.fname -> Parsetree.file ->                        *)
(*   (Env.TypingEnv.t * (please_compile_me list))                        *)
(** {b Descr} : Performs type inference on a complete FoCaL source file.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
let typecheck_file ~current_unit ast_file =
  match ast_file.Parsetree.ast_desc with
   | Parsetree.File phrases ->
     (* A file is always typed in an empty context. *)
     let ctx = {
       current_unit = current_unit;
       current_species = None;
       self_manifest = None;
       tyvars_mapping = [] } in
     let global_env = ref (Env.TypingEnv.pervasives ()) in
     let what_to_compile =
       List.map
         (fun phrase ->
          let (stuff_to_compile, new_global_env) =
            typecheck_phrase ctx !global_env phrase in
          (* Make the global typing environment growing by side effect. *)
          global_env := new_global_env;
          stuff_to_compile)
         phrases in
     (* Store the type information in the phrase's node. A file has no type. *)
     ast_file.Parsetree.ast_type <- Parsetree.ANTI_irrelevant;
     (!global_env, what_to_compile)
;;
