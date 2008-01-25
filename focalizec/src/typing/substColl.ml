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

(* $Id: substColl.ml,v 1.16 2008-01-25 15:21:10 pessaux Exp $ *)

(* ************************************************************************ *)
(** {b Descr} : This module performs substitution of a collection name [c1]
            by [c2]. This means that [c1] will be replaced by [c2].
            The substitution operates in both expressions and types.
            Types are handled by the [Types.subst_type_simple] function.    *)
(* ************************************************************************ *)


(* ************************************************************************** *)
(** {b Descr} : Describes which kind of collection type must be replaced
              by a substitution. It can be either Self or another collection
              "name". This enables factorizing the substitution code for
              replacing Self (in case of collection creation) or another
              collection (in case of "abstraction" function).
              One must make a difference because Self is a special
              constructor in both types and expressions.

    {b Rem} : Exported outside this module.                                   *)
(* ************************************************************************** *)
type substitution_collection_kind =
    (** The collection to replace is the one named in the argument. *)
  | SCK_coll of Types.type_collection
  | SCK_self          (** The collection to replace is Self. *)
;;



(* ******************************************************************* *)
(* substitution_collection_kind -> Types.type_collection ->            *)
(*   Parsetree.ast_node_type_information -> Types.type_simple option   *)
(** {b Descr} : Performs the collection name substitution [c1] <- [c2]
              in an optional [Types.type_simple] under the form of a
              [Parsetree.ast_node_type_information].

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let subst_ast_node_type_information c1 c2 = function
  | Parsetree.ANTI_none -> Parsetree.ANTI_none
  | Parsetree.ANTI_non_relevant -> Parsetree.ANTI_non_relevant
  | Parsetree.ANTI_type ty ->
      (begin
      match c1 with
       | SCK_coll c -> Parsetree.ANTI_type (Types.subst_type_simple c c2 ty)
       | SCK_self ->
           Parsetree.ANTI_type
             (Types.copy_type_simple_but_variables ~and_abstract: (Some c2) ty)
      end)
  | Parsetree.ANTI_scheme sch ->
      (begin
      match c1 with
       | SCK_coll c -> Parsetree.ANTI_scheme (Types.subst_type_scheme c c2 sch)
       | SCK_self -> Parsetree.ANTI_scheme (Types.abstract_in_scheme c2 sch)
      end)
;;



(* substitution_collection_kind -> Types.type_collection ->    *)
(*   Parsetree.ident -> Parsetree.ident                        *)
let subst_ident c1 c2 ident =
  (* Substitution in the AST node description is trivially empty.          *)
  (* Because [idents] can only be [I_local] of [I_global] stuff, there is  *)
  (* never collection names inside, hence nothing to change in the [desc]. *)
  (* Substitute in the AST node type. *)
  let new_type =
    subst_ast_node_type_information c1 c2 ident.Parsetree.ast_type in
  { ident with Parsetree.ast_type = new_type }
;;



let subst_expr_ident ~current_unit c1 c2 ident =
  (* Substitute in the AST node description. *)
  let new_desc =
    match ident.Parsetree.ast_desc with
    | Parsetree.EI_local _ | Parsetree.EI_global _
    | Parsetree.EI_method (None, _) ->
      (* No collection name inside, hence nothing to change. *)
      ident.Parsetree.ast_desc
    | Parsetree.EI_method (Some coll_qvname, vname) ->
      match c1 with
      | SCK_self ->
        (* Because Self is a special constructor, an expr_ident can't  *)
        (* be Self. Then in this case, the substitution is identity.   *)
        ident.Parsetree.ast_desc
      | SCK_coll effective_coll_ty ->
        let coll_ty =
          match coll_qvname with
          | Parsetree.Vname coll_vname ->
            (* If no module specifier appears in the ident, then it *)
            (* implicitely refers to the current compilation unit.  *)
            (* Should never happen because the scoping pass should  *)
            (* have made the hosting module explicit.               *)
            (current_unit, Parsetree_utils.name_of_vname coll_vname)
          | Parsetree.Qualified (modname, coll_vname) ->
            (modname, Parsetree_utils.name_of_vname coll_vname) in
        (* Attention, [c2] is a [Types.collection_type], then it has *)
        (* to be transformed into a [Parsetree.vname] before being   *)
        (* inserted in the [Parsetree.EI_method]. Because collection *)
        (* names are always capitalized, the transformation is       *)
        (* trivially to surround [c2] byt a [Parsetree.Vuident].     *)
        if coll_ty = effective_coll_ty then
          let new_species_qvname =
            Parsetree.Qualified (fst c2, Parsetree.Vuident (snd c2)) in
          Parsetree.EI_method
            (Some new_species_qvname, vname)
        else ident.Parsetree.ast_desc in
  (* Substitute in the AST node type. *)
  let new_type =
    subst_ast_node_type_information c1 c2 ident.Parsetree.ast_type in
  { ident with
      Parsetree.ast_desc = new_desc;
      Parsetree.ast_type = new_type; }
;;



(* ************************************************************************ *)
(* substitution_collection_kind -> Types.type_collection ->                 *)
(*   Parsetree.pattern -> Parsetree.pattern                                 *)
(** {b Descr} : Performs the collection name substitution [c1] <- [c2] in
              a [pattern]. Note that because patterns cannot be collections
              (there is no collection-pattern), the substitution does not
              affect the pattern's structure.
              However, because patterns can be variables and match a
              collection-value, the type of these variable may be of a
              collection. Then we need to substitute in the types hooked
              in the AST's pattern [Parsetree.ast_type] field.
    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let subst_pattern c1 c2 pattern =
  (* Let's just make a local recursive function to save the stack, *)
  (* avoiding passing each time the 2 arguments [c1] and [c2].     *)
  let rec rec_subst pat =
    (* Substitute in the AST node description. *)
    let new_desc =
      (match pat.Parsetree.ast_desc with
       | Parsetree.P_const _
       | Parsetree.P_var _
       | Parsetree.P_wild ->
           (* Structurally nothing to substitute. *)
           pat.Parsetree.ast_desc
       | Parsetree.P_as (pat', vname) ->
           Parsetree.P_as ((rec_subst pat'), vname)
       | Parsetree.P_constr (ident, pats) ->
           (* Because the [ident] here is a sum type constructor, *)
           (* there is no substitution to do here.                *)
           let pats' = List.map rec_subst pats in
           Parsetree.P_constr (ident, pats')
       | Parsetree.P_record fields ->
           let fields' =
             List.map (fun (label, p) -> (label, (rec_subst p))) fields in
           Parsetree.P_record fields'
       | Parsetree.P_tuple pats -> Parsetree.P_tuple (List.map rec_subst pats)
       | Parsetree.P_paren pat' -> Parsetree.P_paren (rec_subst pat')) in
    (* Substitute in the AST node type. *)
    let new_type =
      subst_ast_node_type_information c1 c2 pat.Parsetree.ast_type in
    (* An finally, make a new AST node. *)
    { pat with
        Parsetree.ast_desc = new_desc; Parsetree.ast_type = new_type } in
  (* Now, do the job. *)
  rec_subst pattern
;;



let subst_type_expr c1 c2 type_expression =
  (* Let's just make a local recursive function to save the stack, *)
  (* avoiding passing each time the 2 arguments [c1] and [c2].     *)
  let rec rec_subst ty_expr =
    (* Substitute in the AST node description. *)
    let new_desc =
      (match ty_expr.Parsetree.ast_desc with
       | Parsetree.TE_ident ident ->
           Parsetree.TE_ident (subst_ident c1 c2 ident)
       | Parsetree.TE_fun (t1, t2) ->
           Parsetree.TE_fun ((rec_subst t1), (rec_subst t2))
       | Parsetree.TE_app (ident, tys) ->
           let ident' = subst_ident c1 c2 ident in
           let tys' = List.map rec_subst tys in
           Parsetree.TE_app (ident', tys')
       | Parsetree.TE_prod tys -> Parsetree.TE_prod (List.map rec_subst tys)
       | Parsetree.TE_self
       | Parsetree.TE_prop -> ty_expr.Parsetree.ast_desc
       | Parsetree.TE_paren ty -> Parsetree.TE_paren (rec_subst ty)) in
    (* Substitute in the AST node type. *)
    let new_type =
      subst_ast_node_type_information c1 c2 ty_expr.Parsetree.ast_type in
    (* An finally, make a new AST node. *)
    { ty_expr with
        Parsetree.ast_desc = new_desc; Parsetree.ast_type = new_type } in
  (* Now, do the job. *)
  rec_subst type_expression
;;



let rec subst_expr ~current_unit c1 c2 expression =
  (* Let's just make a local recursive function to save the stack, avoiding *)
  (* passing each time the 3 arguments [~current_unit], [c1] and [c2].      *)
  let rec rec_subst initial_expr =
    (* Substitute in the AST node description. *)
    let new_desc =
      (match initial_expr.Parsetree.ast_desc with
       | Parsetree.E_self
       | Parsetree.E_const _ ->
           (* Structurally, no possible change in expressions or types below. *)
           initial_expr.Parsetree.ast_desc
       | Parsetree.E_fun (arg_vnames, e_body) ->
           Parsetree.E_fun (arg_vnames, (rec_subst e_body))
       | Parsetree.E_var ident ->
           Parsetree.E_var (subst_expr_ident ~current_unit c1 c2 ident)
       | Parsetree.E_app (functional_expr, args_exprs) ->
           let functional_expr' = rec_subst functional_expr in
           let args_exprs' = List.map rec_subst args_exprs in
           Parsetree.E_app (functional_expr', args_exprs')
       | Parsetree.E_constr (cstr_expr, exprs) ->
           (* The constructor expression can not be substituted. May be only *)
           (* in its type, and it's even pretty sure that no. So just hack   *)
           (* it's type, but leave the structure unchanged.                  *)
           let cstr_expr' = { cstr_expr with
             Parsetree.ast_type =
               subst_ast_node_type_information
                 c1 c2 cstr_expr.Parsetree.ast_type } in
           let exprs' = List.map rec_subst exprs in
           Parsetree.E_constr (cstr_expr', exprs')
       | Parsetree.E_match (matched_expr, bindings) ->
           let matched_expr' = rec_subst matched_expr in
           let bindings' =
             List.map
               (fun (pat, expr) ->
         let pat' = subst_pattern c1 c2 pat in
         let expr' = rec_subst expr in
         (pat', expr'))
               bindings in
           Parsetree.E_match (matched_expr', bindings')
       | Parsetree.E_if (e_cond, e_then, e_else) ->
           let e_cond' = rec_subst e_cond in
           let e_then' = rec_subst e_then in
           let e_else' = rec_subst e_else in
           Parsetree.E_if (e_cond', e_then', e_else')
       | Parsetree.E_let (let_def, in_expr) ->
           let let_def' = subst_let_definition ~current_unit c1 c2 let_def in
           let in_expr' = rec_subst in_expr in
           Parsetree.E_let (let_def', in_expr')
       | Parsetree.E_record fields ->
           let fields' =
             List.map (fun (name, expr) -> (name, (rec_subst expr))) fields in
           Parsetree.E_record fields'
       | Parsetree.E_record_access (expr, label) ->
           Parsetree.E_record_access ((rec_subst expr), label)
       | Parsetree.E_record_with (with_expr, fields) ->
           let with_expr' = rec_subst with_expr in
           let fields' =
             List.map (fun (name, expr) -> (name, (rec_subst expr))) fields in
           Parsetree.E_record_with (with_expr', fields')
       | Parsetree.E_tuple exprs -> Parsetree.E_tuple (List.map rec_subst exprs)
       | Parsetree.E_external _ ->
           (* Because this is in fact just a string, nowhere to substitute . *)
           initial_expr.Parsetree.ast_desc
       | Parsetree.E_paren expr -> Parsetree.E_paren (rec_subst expr)) in
    (* Substitute in the AST node type. *)
    let new_type =
      subst_ast_node_type_information c1 c2 initial_expr.Parsetree.ast_type in
    (* An finally, make a new AST node. *)
    { initial_expr with
        Parsetree.ast_desc = new_desc; Parsetree.ast_type = new_type } in
  (* Do je job now. *)
  rec_subst expression



and subst_let_binding ~current_unit c1 c2 binding =
  (* Substitute in the AST node description. *)
  let binding_desc = binding.Parsetree.ast_desc in
  let b_params' =
    List.map
      (fun (vname, ty_opt) ->
        let ty_opt' =
          (match ty_opt with
           | None -> None
           | Some ty -> Some (subst_type_expr c1 c2 ty)) in
        (vname, ty_opt'))
      binding_desc.Parsetree.b_params in
    let b_type' =
      (match binding_desc.Parsetree.b_type with
       | None -> None
       | Some ty_expr -> Some (subst_type_expr c1 c2 ty_expr)) in
    let b_body' =
      subst_expr ~current_unit c1 c2 binding_desc.Parsetree.b_body in
    let desc' = { binding_desc with
       Parsetree.b_params = b_params';
       Parsetree.b_type = b_type';
       Parsetree.b_body = b_body' } in
    { binding with
        (* Substitute in the AST node type. *)
        Parsetree.ast_type =
          subst_ast_node_type_information c1 c2 binding.Parsetree.ast_type;
        Parsetree.ast_desc = desc' }



and subst_let_definition ~current_unit c1 c2 let_def =
  let let_def_desc = let_def.Parsetree.ast_desc in
  (* Substitute in the AST node description. *)
  let bindings' =
    List.map
      (subst_let_binding ~current_unit c1 c2)
      let_def_desc.Parsetree.ld_bindings in
  let desc' = { let_def_desc with Parsetree.ld_bindings = bindings' } in
  { let_def with
     (* Substitute in the AST node type. *)
      Parsetree.ast_type =
        subst_ast_node_type_information c1 c2 let_def.Parsetree.ast_type;
      Parsetree.ast_desc = desc' }
;;



(* ******************************************************************* *)
(* current_unit:Types.fname -> substitution_collection_kind ->         *)
(*   Types.type_collection -> Parsetree.prop -> Parsetree.prop         *)
(** {b Descr} : Performs the collection name substitution [c1] <- [c2]
              in a [Parsetree.prop].

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
let subst_prop ~current_unit c1 c2 initial_prop_expr =
  let rec rec_subst prop_expr =
    let new_desc =
      (match prop_expr.Parsetree.ast_desc with
       |  Parsetree.Pr_forall (vnames, type_expr, prop) ->
           let type_expr' = subst_type_expr c1 c2 type_expr in
           let body' = rec_subst prop in
           Parsetree.Pr_forall (vnames, type_expr', body')
       | Parsetree.Pr_exists (vnames, type_expr, prop) ->
           let type_expr' = subst_type_expr c1 c2 type_expr in
           let body' = rec_subst prop in
           Parsetree.Pr_exists (vnames, type_expr', body')
       | Parsetree.Pr_imply (prop1, prop2) ->
           let prop1' = rec_subst prop1 in
           let prop2' = rec_subst prop2 in
           Parsetree.Pr_imply (prop1', prop2')
       | Parsetree.Pr_or (prop1, prop2) ->
           let prop1' = rec_subst prop1 in
           let prop2' = rec_subst prop2 in
           Parsetree.Pr_or (prop1', prop2')
       | Parsetree.Pr_and (prop1, prop2) ->
           let prop1' = rec_subst prop1 in
           let prop2' = rec_subst prop2 in
           Parsetree.Pr_and (prop1', prop2')
       | Parsetree.Pr_equiv (prop1, prop2) ->
           let prop1' = rec_subst prop1 in
           let prop2' = rec_subst prop2 in
           Parsetree.Pr_equiv (prop1', prop2')
       | Parsetree.Pr_not prop -> Parsetree.Pr_not (rec_subst prop)
       | Parsetree.Pr_expr expr ->
           let expr' = subst_expr ~current_unit c1 c2 expr in
           Parsetree.Pr_expr expr'
       | Parsetree.Pr_paren prop -> Parsetree.Pr_paren (rec_subst prop)) in
    { prop_expr with Parsetree.ast_desc = new_desc } in
  (* Now do the job. *)
  rec_subst initial_prop_expr
;;



let subst_species_field ~current_unit c1 c2 = function
  | Env.TypeInformation.SF_sig (from, vname, scheme) ->
      (begin
      Types.begin_definition () ;
      let ty = Types.specialize scheme in
      let ty' =
        (match c1 with
         | SCK_coll c -> Types.subst_type_simple c c2 ty
         | SCK_self ->
             Types.copy_type_simple_but_variables
               ~and_abstract: (Some c2) ty) in
      Types.end_definition () ;
      let scheme' = Types.generalize ty' in
      Env.TypeInformation.SF_sig (from, vname, scheme')
      end)
  | Env.TypeInformation.SF_let (from, vname, params_names, scheme, body, dep) ->
      (begin
      Types.begin_definition () ;
      let ty = Types.specialize scheme in
      let ty' =
        (match c1 with
         | SCK_coll c -> Types.subst_type_simple c c2 ty
         | SCK_self ->
             Types.copy_type_simple_but_variables
               ~and_abstract: (Some c2) ty) in
      Types.end_definition () ;
      let scheme' = Types.generalize ty' in
      let body' = subst_expr ~current_unit c1 c2 body in
      Env.TypeInformation.SF_let
	(from, vname, params_names, scheme', body', dep)
      end)
  | Env.TypeInformation.SF_let_rec l ->
      (begin
      let l' =
        List.map
          (fun (from, vname, params_names, scheme, body, dep) ->
            let ty = Types.specialize scheme in
            let ty' =
              (match c1 with
               | SCK_coll c -> Types.subst_type_simple c c2 ty
               | SCK_self ->
           Types.copy_type_simple_but_variables
             ~and_abstract: (Some c2) ty) in
            Types.end_definition ();
            let scheme' = Types.generalize ty' in
            let body' = subst_expr ~current_unit c1 c2 body in
            (from, vname, params_names, scheme', body', dep))
          l in
      Env.TypeInformation.SF_let_rec l'
      end)
  | Env.TypeInformation.SF_theorem
      (from, vname, scheme, body, proof, deps_rep) ->
      (begin
      (* No substitution inside the proof. *)
      Types.begin_definition () ;
      let ty = Types.specialize scheme in
      let ty' =
        (match c1 with
         | SCK_coll c -> Types.subst_type_simple c c2 ty
         | SCK_self ->
             Types.copy_type_simple_but_variables
               ~and_abstract: (Some c2) ty) in
      Types.end_definition () ;
      let scheme' = Types.generalize ty' in
      let body' = subst_prop ~current_unit c1 c2 body in
      Env.TypeInformation.SF_theorem
	(from, vname, scheme', body', proof, deps_rep)
      end)
  | Env.TypeInformation.SF_property (from, vname, scheme, body, deps_rep) ->
      (begin
      Types.begin_definition () ;
      let ty = Types.specialize scheme in
      let ty' =
        (match c1 with
         | SCK_coll c -> Types.subst_type_simple c c2 ty
         | SCK_self ->
             Types.copy_type_simple_but_variables
               ~and_abstract: (Some c2) ty) in
      Types.end_definition () ;
      let scheme' = Types.generalize ty' in
      let body' = subst_prop ~current_unit c1 c2 body in
      Env.TypeInformation.SF_property (from, vname, scheme', body', deps_rep)
      end)
;;
