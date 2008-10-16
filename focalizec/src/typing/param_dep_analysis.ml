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

(* $Id: param_dep_analysis.ml,v 1.19 2008-10-16 13:18:52 pessaux Exp $ *)

(* ******************************************************************** *)
(** {b Descr} : This module deals with the computation of which methods
    of a collection parameter an expression "needs" (i.e. depends on).
    This information is required in order to be able to generate the
    Coq/OCaml code.                                                     *)
(* ******************************************************************** *)



(** Détermine le style de méthode du paramètre d'espèce afin de pouvoir
  soit mémoriser son type-scheme si c'est une méthode calculatoire, soit
  son énoncé si c'est une propriété logique. Ca sert à ensuite construire
  les [dependencies_from_params_...]. Si c'est un let on renvoie le type
    [meth_ty] originellement reçu en argument, sinon on renvoie l'expression
    logique trouvée comme "type" de la priperty ou du theorem. *)
let guess_method_computational_or_logical meth_name meth_ty among =
  let rec rec_guess = function
    | [] -> raise Not_found
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_sig (_, n, _)
         | Env.TypeInformation.SF_let (_, n, _, _, _, _, _) ->
             if n = meth_name then Parsetree_utils.DETK_computational meth_ty
             else rec_guess q
         | Env.TypeInformation.SF_let_rec let_infos ->
             if
               List.exists
                 (fun (_, n, _, _, _, _, _) -> n = meth_name) let_infos then
               Parsetree_utils.DETK_computational meth_ty
           else rec_guess q
       | Env.TypeInformation.SF_theorem (_, n, _, lexpr, _, _)
       | Env.TypeInformation.SF_property (_, n, _, lexpr, _) ->
           if n = meth_name then Parsetree_utils.DETK_logical lexpr
           else rec_guess q
      end) in
  rec_guess among
;;



(* ********************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->      *)
(*  Parsetree.expr_ident -> Parsetree_utils.DepNameSet.t                 *)
(** {b Descr} : Computes the set of methods names the identifier [ident]
    represents as involving a dependency with the [param_coll_name]
    collection name.
    In fact, either the identifier is a method call from the the same
    species as [param_coll_name] and is counted as a dependency. Or it
    is not and then the returned set of dependencies is empty.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let param_deps_ident ~current_species (param_coll_name, param_coll_meths)
    local_idents ident =
  (* Recover the ident's type. *)
  let ident_ty =
    (match ident.Parsetree.ast_type with
     | Parsetree.ANTI_none
     | Parsetree.ANTI_non_relevant
     | Parsetree.ANTI_scheme  _ -> assert false
     | Parsetree.ANTI_type t -> t) in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local n ->
       (* Be careful. Because "in" parameters smell like regular local
          identifiers, we must check here if the current identifier is in fact
          a "in-parameter" of the species. To check this, one must be careful
          to possible masking that could exist if a really local identifier
          wearing the same name than a "IN" parameter was bound since the
          "IN"-parameter was bound.
          Because local bound idents can only appear in a "let" EXPRESSION
          (not species fields because it would not be used directly by its
          name but by "!it's name") and can't escape this "let" EXPRESSION,
          and because they only can be more recent that the species
          "IN"-paramater definition, to know is a more recent ident is wearing
          the same name that a "IN"-parameter (hence, masks it) , we just
          need to check if it exists in the list of locally-bound idents. *)
       if param_coll_name = n && not (List.mem n local_idents) then
         Parsetree_utils.ParamDepSet.singleton
           (n, (Parsetree_utils.DETK_computational ident_ty))
       else Parsetree_utils.ParamDepSet.empty
   | Parsetree.EI_global _ ->
       (* These are not a method call, then they induce no dependency. *)
       Parsetree_utils.ParamDepSet.empty
   | Parsetree.EI_method (None, _) ->
       (* A method of self, then induces no dependency like those were are
          looking for. *)
       Parsetree_utils.ParamDepSet.empty
   | Parsetree.EI_method (Some coll_specifier, vname) ->
       (begin
        match coll_specifier with
        | Parsetree.Vname coll_name ->
            (* Check it this method call is from the species parameter we are
               working with. Should never happen because the scoping pass
               should make explicit the hosting module. *)
            if coll_name = param_coll_name then
              (begin
              let ty_or_log_expr =
                guess_method_computational_or_logical
                  vname ident_ty param_coll_meths in
              Parsetree_utils.ParamDepSet.singleton (vname, ty_or_log_expr)
              end)
            else Parsetree_utils.ParamDepSet.empty
        | Parsetree.Qualified (module_name, coll_name) ->
            (* If the module specification matches the one of the
               [current_species] and if the collection name matches species
               parameter then we have a dependency. *)
            if module_name = fst current_species &&
              coll_name = param_coll_name then
              (begin
              let ty_or_log_expr =
                guess_method_computational_or_logical
                  vname ident_ty param_coll_meths in
              Parsetree_utils.ParamDepSet.singleton (vname, ty_or_log_expr)
              end)
            else Parsetree_utils.ParamDepSet.empty
       end)
;;



(* ************************************************************************ *)
(** {b Descr} : Basically really does the job of [param_deps_expr] but
      has the extra parameter [start_local_idents] allowing to start
      with a non-empty list of identifiers considered as local.
      This is needed for [__param_deps_logical_expr] that needs to call
      ourselves with its already accumulated list of local identifiers.
      However, outside this module, the exported function [param_deps_expr]
      does not have any local identifier list as parameter because it
      must never be called in a context where there would already exists
      local identifiers.

  {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************ *)
let rec __param_deps_expr ~current_species (param_coll_name, param_coll_meths)
    start_local_idents expression =
  let rec rec_deps local_idents expr =
    match expr.Parsetree.ast_desc with
    | Parsetree.E_self
    | Parsetree.E_const _
    | Parsetree.E_external _ -> Parsetree_utils.ParamDepSet.empty
    | Parsetree.E_fun (bound_name, e_body) ->
        (* Here, the function parameter name may mask a "in"-parameter. *)
        rec_deps (bound_name @ local_idents) e_body
    | Parsetree.E_var ident ->
        param_deps_ident
          ~current_species (param_coll_name, param_coll_meths) local_idents
          ident
    | Parsetree.E_app (functional_expr, args_exprs) ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.ParamDepSet.union
              accu_deps (rec_deps local_idents e))
          (rec_deps local_idents functional_expr)
          args_exprs
    | Parsetree.E_match (matched_expr, bindings) ->
        List.fold_left
          (fun accu_deps (pat, e) ->
            (* Here, each name of the pattern may mask a "in"-parameter. *)
            let local_idents' =
              (Parsetree_utils.get_local_idents_from_pattern pat) @
              local_idents in
            Parsetree_utils.ParamDepSet.union
              accu_deps (rec_deps local_idents' e))
          (rec_deps local_idents matched_expr)
          bindings
    | Parsetree.E_if (e_cond, e_then, e_else) ->
        let deps1 = rec_deps local_idents e_cond in
        let deps2 = rec_deps local_idents e_then in
        let deps3 = rec_deps local_idents e_else in
        Parsetree_utils.ParamDepSet.union
          (Parsetree_utils.ParamDepSet.union deps1 deps2) deps3
    | Parsetree.E_let (let_def, in_expr) ->
        List.fold_left
          (fun accu_deps binding ->
            (* Here, each parameter name of the binding may mask a
               "IN"-parameter. *)
            let local_idents' =
              (List.map fst binding.Parsetree.ast_desc.Parsetree.b_params) @
              local_idents in
            let deps =
              (match binding.Parsetree.ast_desc.Parsetree.b_body with
               | Parsetree.BB_logical p ->
                   __param_deps_logical_expr
                     ~current_species (param_coll_name, param_coll_meths)
                     local_idents' p
               | Parsetree.BB_computational e -> rec_deps local_idents' e) in
            Parsetree_utils.ParamDepSet.union accu_deps deps)
          (rec_deps local_idents in_expr)
          let_def.Parsetree.ast_desc.Parsetree.ld_bindings
    | Parsetree.E_record fields ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.ParamDepSet.union
              accu_deps (rec_deps local_idents e))
          Parsetree_utils.ParamDepSet.empty
          fields
    | Parsetree.E_record_access (e, _) -> rec_deps local_idents e
    | Parsetree.E_record_with (e, labs_exprs) ->
        List.fold_left
          (fun accu_deps (_, e) ->
            Parsetree_utils.ParamDepSet.union
              accu_deps (rec_deps local_idents e))
          (rec_deps local_idents e)
          labs_exprs
    | Parsetree.E_constr (_, exprs)
    | Parsetree.E_tuple exprs ->
        List.fold_left
          (fun accu_deps e ->
            Parsetree_utils.ParamDepSet.union
              accu_deps (rec_deps local_idents e))
          Parsetree_utils.ParamDepSet.empty
          exprs
    | Parsetree.E_paren e -> rec_deps local_idents e
    | Parsetree.E_equality (e1, e2) ->
        let deps1 = rec_deps local_idents e1 in
        let deps2 = rec_deps local_idents e2 in
        Parsetree_utils.ParamDepSet.union deps1 deps2 in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps start_local_idents expression



and __param_deps_logical_expr ~current_species
    (param_coll_name, param_coll_meths) start_local_idents proposition =
  let rec rec_deps local_idents logical_expr =
    match logical_expr.Parsetree.ast_desc with
     | Parsetree.Pr_forall (vnames, _, logical_expr')
     | Parsetree.Pr_exists (vnames, _, logical_expr') ->
         (* Here, the quantifid names may mask a "in"-parameter. *)
        rec_deps (vnames @ local_idents) logical_expr'
     | Parsetree.Pr_imply (logical_expr1, logical_expr2)
     | Parsetree.Pr_or (logical_expr1, logical_expr2)
     | Parsetree.Pr_and (logical_expr1, logical_expr2)
     | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
         let deps1 = rec_deps local_idents logical_expr1 in
         let deps2 = rec_deps local_idents logical_expr2 in
         Parsetree_utils.ParamDepSet.union deps1 deps2
     | Parsetree.Pr_not logical_expr' -> rec_deps local_idents logical_expr'
     | Parsetree.Pr_expr expr ->
         __param_deps_expr
           ~current_species (param_coll_name, param_coll_meths) local_idents
           expr
     | Parsetree.Pr_paren logical_expr' ->
         rec_deps local_idents logical_expr' in
  (* **************** *)
  (* Now, do the job. *)
  rec_deps start_local_idents proposition
 ;;



let param_deps_enforced_deps_in_proof ~current_species
    (param_coll_name, param_coll_meths) fact =
  match fact.Parsetree.ast_desc with
   | Parsetree.Ed_definition expr_idents
   | Parsetree.Ed_property expr_idents ->
       List.fold_left
         (fun accu_deps ident ->
           let deps = 
             param_deps_ident
               ~current_species (param_coll_name, param_coll_meths) [] ident in
           Parsetree_utils.ParamDepSet.union deps accu_deps)
         Parsetree_utils.ParamDepSet.empty
         expr_idents
;;



let param_deps_fact ~current_species (param_coll_name, param_coll_meths) fact =
  match fact.Parsetree.ast_desc with
   | Parsetree.F_definition expr_idents
   | Parsetree.F_property expr_idents ->
       List.fold_left
         (fun accu_deps ident ->
           let deps = 
             param_deps_ident
               ~current_species (param_coll_name, param_coll_meths) [] ident in
           Parsetree_utils.ParamDepSet.union deps accu_deps)
         Parsetree_utils.ParamDepSet.empty
         expr_idents
   | Parsetree.F_hypothesis _
   | Parsetree.F_node  _ -> Parsetree_utils.ParamDepSet.empty
;;




let param_deps_hyp ~current_species (param_coll_name, param_coll_meths) hyp =
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_variable (_, _) -> Parsetree_utils.ParamDepSet.empty
   | Parsetree.H_hypothesis (_, prop) ->
       __param_deps_logical_expr
         ~current_species (param_coll_name, param_coll_meths) [] prop
   | Parsetree.H_notation (_, expr) ->
       __param_deps_expr
         ~current_species (param_coll_name, param_coll_meths) [] expr
;;



let param_deps_statement ~current_species (param_coll_name, param_coll_meths)
    stmt =
  let hyps_deps =
    List.fold_left
      (fun accu_deps hyp ->
        let hyp_deps =
          param_deps_hyp
            ~current_species (param_coll_name, param_coll_meths) hyp in
        Parsetree_utils.ParamDepSet.union accu_deps hyp_deps)
      Parsetree_utils.ParamDepSet.empty
      stmt.Parsetree.ast_desc.Parsetree.s_hyps in
  let concl_deps =
    match stmt.Parsetree.ast_desc.Parsetree.s_concl with
     | None -> Parsetree_utils.ParamDepSet.empty
     | Some log_expr ->
         __param_deps_logical_expr
           ~current_species (param_coll_name, param_coll_meths) [] log_expr in
  Parsetree_utils.ParamDepSet.union hyps_deps concl_deps
;;



(* Not exported. *)
let rec param_deps_proof_node ~current_species
    (param_coll_name, param_coll_meths) proof_node =
  match proof_node.Parsetree.ast_desc with
   | Parsetree.PN_sub (_, statement, proof) ->
       let deps1 =
         param_deps_statement
           ~current_species (param_coll_name, param_coll_meths) statement in
       let deps2 =
         param_deps_proof
           ~current_species (param_coll_name, param_coll_meths) proof in
       Parsetree_utils.ParamDepSet.union deps1 deps2
   | Parsetree.PN_qed (_, proof) ->
       param_deps_proof
         ~current_species (param_coll_name, param_coll_meths) proof



(* current_species: Parsetree.qualified_species ->                      *)
(*   Parsetree.vname -> Parsetree.proof -> Parsetree_utils.ParamDepSet.t *)
(* Exported. *)
and param_deps_proof ~current_species (param_coll_name, param_coll_meths)
    proof =
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed (enforced_deps, _)
   | Parsetree.Pf_coq (enforced_deps, _) ->
       List.fold_left
         (fun accu_deps enf_dep ->
           Parsetree_utils.ParamDepSet.union
             accu_deps
             (param_deps_enforced_deps_in_proof
                ~current_species (param_coll_name, param_coll_meths) enf_dep))
         Parsetree_utils.ParamDepSet.empty
         enforced_deps
   | Parsetree.Pf_auto facts ->
       List.fold_left
         (fun accu_deps fact ->
           Parsetree_utils.ParamDepSet.union
             accu_deps
             (param_deps_fact
                ~current_species (param_coll_name, param_coll_meths) fact))
         Parsetree_utils.ParamDepSet.empty
         facts
   | Parsetree.Pf_node proof_nodes ->
       List.fold_left
         (fun accu_deps p ->
           Parsetree_utils.ParamDepSet.union
             accu_deps
             (param_deps_proof_node
                ~current_species (param_coll_name, param_coll_meths) p))
         Parsetree_utils.ParamDepSet.empty
         proof_nodes
;;



(* ************************************************************************* *)
(* current_species: Parsetree.qualified_species -> Parsetree.vname ->        *)
(*   Parsetree.expr -> Parsetree_utils.ParamDepSet.t                     *)
(** {b Descr} : Computes the dependencies of an expression on the collection
    parameter name [param_coll_name]. In other words, detects which methods
    of [param_coll_name] (that is considered as a collection (i.e. "is")
    parameter), the current expression needs.

    {b Args}:
      - ~current_species : The name (module + effective name) of the
          currently analyzed species, i.e. the species where we asked for
          the dependencies to be computed.
      - param_coll_name : The name of the species parameter we want to
          to detect dependencies to in the expression.
      - expression : The expression in which we want to detect possible
          dependencies on the species parameter whose name is
          [param_coll_name].

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let param_deps_expr ~current_species (param_coll_name, param_coll_meths)
    expression =
  __param_deps_expr
    ~current_species (param_coll_name, param_coll_meths) [] expression
;;



(* current_species: Parsetree.qualified_species -> Parsetree.vname -> *)
(*   Parsetree.logical_expr -> Parsetree_utils.ParamDepSet.t          *)
let param_deps_logical_expr ~current_species
    (param_coll_name, param_coll_meths) proposition =
  __param_deps_logical_expr
    ~current_species (param_coll_name, param_coll_meths) [] proposition
;;
