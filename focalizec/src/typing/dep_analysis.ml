(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* *********************************************************************** *)
(** {b Descr} : This module performs the well-formation analysis described
    in Virgile Prevosto's Phd, section 3.5.

    {b Rem} : Be careful that in the Phd, the well-formation formula is
    incorrect. It should be read:
       x1 <| x2 <=>
         \ex {y_i}_(i=1..n) such as
           (y_1 \circarrow x_1 and y_n \circarrow x_2 and
            \all j < n, y_j \in \lbag y_j+1 \rbag_s)
    And in the condition for well-formation, <| must be subscripted by
    s and not a.                                                           *)
(* *********************************************************************** *)


(* For debugging purpose only. *)
(*
let debug_print_dependencies_from_parameters l =
  List.iter
    (fun (species_param, methods) ->
      (match species_param with
       | Env.TypeInformation.SPAR_is ((mod_name, spe_name), _, _, _, _) ->
           Format.eprintf "From IS-parameter '%s#%s', methods: "
             mod_name spe_name;
       | Env.TypeInformation.SPAR_in (n, _, _) ->
           Format.eprintf "From IN-parameter '%a', methods: "
             Sourcify.pp_vname n);
      Parsetree_utils.ParamDepSet.iter
        (fun (n, k) -> Format.eprintf "\t%a: " Sourcify.pp_vname n;
          match k with
           | Parsetree_utils.DETK_computational t ->
               Format.eprintf "%a@." Types.pp_type_simple t
           | Parsetree_utils.DETK_logical e ->
               Format.eprintf "%a@." Sourcify.pp_logical_expr e)
        methods;
      Format.eprintf "@.")
    l
;;
*)

(*
let debug_print_dependencies_from_parameters2 l =
  List.iter
    (fun (species_param, (Env.ODFP_methods_list methods)) ->
      (match species_param with
       | Env.TypeInformation.SPAR_is ((mod_name, spe_name), _, _, _, _) ->
           Format.eprintf "From IS-parameter '%s#%s', methods: "
             mod_name spe_name;
       | Env.TypeInformation.SPAR_in (n, _, _) ->
           Format.eprintf "From IN-parameter '%a', methods: "
             Sourcify.pp_vname n);
      List.iter
        (fun (n, k) ->
          Format.eprintf "%a " Sourcify.pp_vname n;
          match k with
           | Parsetree_utils.DETK_computational t ->
               Format.eprintf "%a@." Types.pp_type_simple t
           | Parsetree_utils.DETK_logical e ->
               Format.eprintf "%a@." Sourcify.pp_logical_expr e)
        methods;
      Format.eprintf "@.")
    l
;;
*)

(*
let debug_print_dependencies_from_parameters3 l =
  List.iter
    (fun (param_name, (Env.ODFP_methods_list methods)) ->
      Format.eprintf "From parameter '%a', methods: "
        Sourcify.pp_vname param_name;
      List.iter
        (fun (n, k) ->
          Format.eprintf "%a " Sourcify.pp_vname n;
          match k with
           | Parsetree_utils.DETK_computational t ->
               Format.eprintf "%a@." Types.pp_type_simple t
           | Parsetree_utils.DETK_logical e ->
               Format.eprintf "%a@." Sourcify.pp_logical_expr e)
        methods;
      Format.eprintf "@.")
    l
;;
*)



(* ****************************************************************** *)
(* current_species: Parsetree.qualified_vname -> Parsetree.expr ->    *)
(*  Parsetree_utils.DepNameSet.t                                      *)
(** {b Descr} : Compute the set of vnames the expression [expression]
    decl-depends of in the species [~current_species].

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let rec expr_decl_dependencies ~current_species expression =
  (* Let's just make a local function to save the stack, avoid passing each
     time the parameter [~current_species]. *)
  let rec rec_depend expr =
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self
     | Parsetree.E_const _ -> Parsetree_utils.SelfDepSet.empty
     | Parsetree.E_fun (_, body) -> rec_depend body
     | Parsetree.E_var ident ->
       (begin
       (* Recover the ident's type. *)
       let ident_ty =
         (match ident.Parsetree.ast_type with
          | Parsetree.ANTI_none
          | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_scheme  _ -> assert false
          | Parsetree.ANTI_type t -> t) in
        match ident.Parsetree.ast_desc with
        | Parsetree.EI_local _ ->
            (* Because scoping pass already renamed all the identfiers that
               "looked like" local identifiers into "method identifiers" if
               they indeed denoted methods, we can safely consider that
               remaining "local identifiers" are really local and introduce
               no dependency. *)
            Parsetree_utils.SelfDepSet.empty
        | Parsetree.EI_global _ -> Parsetree_utils.SelfDepSet.empty
        | Parsetree.EI_method (None, vname) ->
            (* Case c!x in Virgile Prevosto's Phd, section 3.5, page 30,
               definition 12. *)
            Parsetree_utils.SelfDepSet.singleton (vname, ident_ty)
        | Parsetree.EI_method (Some coll_specifier, vname) ->
            (begin
             match coll_specifier with
             | Parsetree.Vname _ ->
               (* In this case, may be there is some scoping process missing. *)
               assert false
             | Parsetree.Qualified (module_name, coll_vname) ->
               if current_species = (module_name, coll_vname) then
                 (* Case c!x in Virgile Prevosto's Phd, section 3.5, page 30,
                    definition 12. *)
                 Parsetree_utils.SelfDepSet.singleton (vname, ident_ty)
               else Parsetree_utils.SelfDepSet.empty
             end)
       end)
     | Parsetree.E_app (fun_expr, args_exprs) ->
         let fun_expr_deps = rec_depend fun_expr in
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           fun_expr_deps
           args_exprs
     | Parsetree.E_constr (_, args_exprs) ->
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           Parsetree_utils.SelfDepSet.empty
           args_exprs
     | Parsetree.E_match (matched_e, pats_exprs) ->
         let matched_e_deps = rec_depend matched_e in
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           matched_e_deps
           pats_exprs
     | Parsetree.E_if (if_expr, then_expr, else_expr) ->
         let if_expr_deps = rec_depend if_expr in
         let then_expr_deps = rec_depend  then_expr in
         let else_expr_deps = rec_depend else_expr in
         Parsetree_utils.SelfDepSet.union
           if_expr_deps
           (Parsetree_utils.SelfDepSet.union then_expr_deps else_expr_deps)
     | Parsetree.E_let (let_def, in_expr) ->
         (* No substration here because the let-definition is NOT a field
            definition ! *)
         let in_expr_deps = rec_depend in_expr in
         List.fold_left
           (fun accu_deps binding ->
             let deps =
             (match binding.Parsetree.ast_desc.Parsetree.b_body with
              | Parsetree.BB_logical p ->
                  prop_decl_dependencies ~current_species p
              | Parsetree.BB_computational e ->rec_depend e) in
             Parsetree_utils.SelfDepSet.union deps accu_deps)
           in_expr_deps
           let_def.Parsetree.ast_desc.Parsetree.ld_bindings
     | Parsetree.E_record labels_exprs ->
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           Parsetree_utils.SelfDepSet.empty
           labels_exprs
     | Parsetree.E_record_access (e, _) -> rec_depend e
     | Parsetree.E_record_with (e, labels_exprs) ->
         let e_deps = rec_depend e in
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           e_deps
           labels_exprs
     | Parsetree.E_tuple exprs ->
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           Parsetree_utils.SelfDepSet.empty
           exprs
     | Parsetree.E_sequence exprs ->
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.SelfDepSet.union (rec_depend e) accu_deps)
           Parsetree_utils.SelfDepSet.empty
           exprs
     | Parsetree.E_external _ -> Parsetree_utils.SelfDepSet.empty
     | Parsetree.E_paren e -> rec_depend e in
  rec_depend expression




(* ************************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.prop ->           *)
(*   Parsetree_utils.DepNameSet.t                                            *)
(** {b Descr} : Compute the set of vnames the prop expression
    [initial_prop_expression] decl-depends in the species [~current_species].

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
and prop_decl_dependencies ~current_species initial_prop_expression =
  let rec rec_depend prop_expression =
    match prop_expression.Parsetree.ast_desc with
     | Parsetree.Pr_forall (_, _, prop)
     | Parsetree.Pr_exists (_, _, prop)
     | Parsetree.Pr_not prop
     | Parsetree.Pr_paren prop -> rec_depend prop
     | Parsetree.Pr_imply (prop1, prop2)
     | Parsetree.Pr_or (prop1, prop2)
     | Parsetree.Pr_and (prop1, prop2)
     | Parsetree.Pr_equiv (prop1, prop2) ->
         let prop1_decl_deps = rec_depend prop1 in
         let prop2_decl_deps = rec_depend prop2 in
         (Parsetree_utils.SelfDepSet.union prop1_decl_deps prop2_decl_deps)
     | Parsetree.Pr_expr expr ->
         expr_decl_dependencies ~current_species expr in
  (* **************** *)
  (* Now, do the job. *)
  rec_depend initial_prop_expression
;;



(* ************************************************************************ *)
(* current_species: Parsetree.qualified_vname -> Parsetree.ident->          *)
(*   Parsetree_utils.DepNameSet.t                                           *)
(** {b Descr} : Compute the set of vnames the identifier [ident] represents
    as dependencies when this ident is located in a [fact].
    In such a context, the [ident] is in fact a dependency.
    Depending on wether the ident appears under a "Def" or "Decl" the
    dependency will be considered as a "decl" or a "def" dependency.

    {b Rem} : MUST only called with idents extracted from a [fact]'s
    structure !

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
let ident_in_fact_dependencies ~current_species ident =
  (* Recover the ident's type. *)
  let ident_ty =
    (match ident.Parsetree.ast_type with
     | Parsetree.ANTI_none
     | Parsetree.ANTI_irrelevant -> assert false
     | Parsetree.ANTI_scheme sch -> Types.specialize sch
     | Parsetree.ANTI_type t -> t) in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       (* Because scoping pass already renamed all the identifiers that
          "looked like" local identifiers into "method identifiers" if they
          indeed denoted methods, we can safely consider that remaining
          "local identifiers" are really local and introduce no dependency.
          Furthermore, there is no reason to get here real local identifier
          unless the user put an erroneous fact. *)
       (* failwith "To investigate: may be erroneous fact in the proof." *)
       Parsetree_utils.SelfDepSet.empty
   | Parsetree.EI_global _ ->
       (* Since dependencies are computed inside A species architecture,
          invocation of a global stuff does not involve dependency because it
          does not belong to the currently analyzed species. *)
       Parsetree_utils.SelfDepSet.empty
   | Parsetree.EI_method (None, vname) ->
       (* A method of Self. *)
       Parsetree_utils.SelfDepSet.singleton (vname, ident_ty)
   | Parsetree.EI_method (Some coll_specifier, vname) ->
       (begin
        match coll_specifier with
        | Parsetree.Vname _ -> assert false
        | Parsetree.Qualified (module_name, coll_vname) ->
          (* If the module specification and the collection name match the
             [current_species] then are are still in the case of Self. Else we
             are in the case of another species. *)
          if (module_name, coll_vname) = current_species then
            (* A method of Self. *)
            Parsetree_utils.SelfDepSet.singleton (vname, ident_ty)
           else
            (* Method from another species, then no dep. *)
            Parsetree_utils.SelfDepSet.empty
       end)
;;



(* ******************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.fact->      *)
(*   (Parsetree_utils.DepNameSet.t * Parsetree_utils.DepNameSet.t)     *)
(** {b Descr} : Compute the set of vnames the fact [fact] decl-depends
    and def-depends of in the species [~current_species].

    {b Exported} : No.                                                 *)
(* ******************************************************************* *)
let fact_decl_n_def_dependencies ~current_species fact =
  match fact.Parsetree.ast_desc with
   | Parsetree.F_property idents ->
       (* Here are some "decl"-dependencies ! *)
       let decl_deps =
         List.fold_left
           (fun accu ident ->
             Parsetree_utils.SelfDepSet.union accu
               (ident_in_fact_dependencies ~current_species ident))
           Parsetree_utils.SelfDepSet.empty
           idents in
       (decl_deps, Parsetree_utils.SelfDepSet.empty)
   | Parsetree.F_definition idents ->
       (* These are "def"-dependencies, not "decl" !!! *)
       let def_deps =
         List.fold_left
           (fun accu ident ->
             Parsetree_utils.SelfDepSet.union accu
               (ident_in_fact_dependencies ~current_species ident))
           Parsetree_utils.SelfDepSet.empty
           idents in
       (Parsetree_utils.SelfDepSet.empty, def_deps)
   | Parsetree.F_hypothesis _ | Parsetree.F_node _ | Parsetree.F_type _ ->
       (Parsetree_utils.SelfDepSet.empty,
        Parsetree_utils.SelfDepSet.empty)
;;



let hyp_decl_dependencies ~current_species hyp =
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_variable (_, _) ->
       (* No decl-dependency from type expressions. *)
       Parsetree_utils.SelfDepSet.empty
   | Parsetree.H_hypothesis (_, prop) ->
       prop_decl_dependencies ~current_species prop
   | Parsetree.H_notation (_, expr) ->
       expr_decl_dependencies ~current_species expr
;;



let statement_decl_dependencies ~current_species stmt =
  let stmt_desc = stmt.Parsetree.ast_desc in
  (* First, get the decl-dependencies from the hypothses. *)
  let hyps_decl_deps =
    List.fold_left
      (fun accu_decl_deps hyp ->
        let hyp_decl_deps = hyp_decl_dependencies ~current_species hyp in
        Parsetree_utils.SelfDepSet.union accu_decl_deps hyp_decl_deps)
      Parsetree_utils.SelfDepSet.empty
      stmt_desc.Parsetree.s_hyps in
  (* An now, accumulate with those of the conclusion. *)
  match stmt_desc.Parsetree.s_concl with
   | None -> hyps_decl_deps
   | Some prop ->
       let prop_decl_defs =
         prop_decl_dependencies ~current_species prop in
       Parsetree_utils.SelfDepSet.union hyps_decl_deps prop_decl_defs

;;



let rec proof_decl_n_def_dependencies ~current_species proof =
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed enf_deps
   | Parsetree.Pf_coq (enf_deps, _) ->
       List.fold_left
         (fun (accu_decl, accu_def) dep ->
           match dep.Parsetree.ast_desc with
            | Parsetree.Ed_definition idents ->
                let accu_def' =
                  List.fold_left
                    (fun accu ident ->
                      Parsetree_utils.SelfDepSet.union accu
                        (ident_in_fact_dependencies ~current_species ident))
                    accu_def
                    idents in
                (accu_decl, accu_def')
            | Parsetree.Ed_property idents ->
                let accu_decl' =
                  List.fold_left
                    (fun accu ident ->
                      Parsetree_utils.SelfDepSet.union accu
                        (ident_in_fact_dependencies ~current_species ident))
                    accu_decl
                    idents in
                (accu_decl', accu_def))
         (Parsetree_utils.SelfDepSet.empty,
          Parsetree_utils.SelfDepSet.empty)
         enf_deps
   | Parsetree.Pf_auto facts ->
       (begin
       List.fold_left
         (fun (accu_decl_deps, accu_def_deps) fact ->
           let (fact_decl_deps, fact_def_deps) =
             fact_decl_n_def_dependencies ~current_species fact in
           (* Return both "decl" and "def" dependencies. *)
           ((Parsetree_utils.SelfDepSet.union
               accu_decl_deps fact_decl_deps),
            (Parsetree_utils.SelfDepSet.union
               accu_def_deps fact_def_deps)))
         (Parsetree_utils.SelfDepSet.empty,
          Parsetree_utils.SelfDepSet.empty)
         facts
       end)
   | Parsetree.Pf_node proof_nodes ->
       (begin
       List.fold_left
         (fun (accu_decl_deps, accu_def_deps) proof_node ->
           let (proof_node_decl_deps, proof_node_def_deps) =
             (match proof_node.Parsetree.ast_desc with
              | Parsetree.PN_sub (_, stmt, p) ->
          let (sub_proof_decl_deps, sub_proof_def_deps) =
            proof_decl_n_def_dependencies ~current_species p in
          (* Statement only have decl-dependencies. *)
          let stmt_decl_deps =
            statement_decl_dependencies ~current_species stmt in
          ((Parsetree_utils.SelfDepSet.union
              sub_proof_decl_deps stmt_decl_deps),
           sub_proof_def_deps)
              | Parsetree.PN_qed (_, p) ->
          proof_decl_n_def_dependencies ~current_species p) in
           (* Return both "decl" and "def" dependencies. *)
           ((Parsetree_utils.SelfDepSet.union
               accu_decl_deps proof_node_decl_deps),
            (Parsetree_utils.SelfDepSet.union
               accu_def_deps proof_node_def_deps)))
         (Parsetree_utils.SelfDepSet.empty,
          Parsetree_utils.SelfDepSet.empty)
         proof_nodes
       end)
;;



let termination_proof_decl_n_def_dependencies ~current_species t_proof =
  match t_proof.Parsetree.ast_desc with
   | Parsetree.TP_structural _ ->
       (Parsetree_utils.SelfDepSet.empty,
        Parsetree_utils.SelfDepSet.empty)
   | Parsetree.TP_lexicographic facts ->
       (begin
       List.fold_left
         (fun (accu_decl_deps, accu_def_deps) fact ->
           let (fact_decl_deps, fact_def_deps) =
             fact_decl_n_def_dependencies ~current_species fact in
           (* Return both "decl" and "def" dependencies. *)
           ((Parsetree_utils.SelfDepSet.union
               accu_decl_deps fact_decl_deps),
            (Parsetree_utils.SelfDepSet.union
               accu_def_deps fact_def_deps)))
         (Parsetree_utils.SelfDepSet.empty,
          Parsetree_utils.SelfDepSet.empty)
         facts
       end)
   | Parsetree.TP_measure (expr, _,  sub_pr)
   | Parsetree.TP_order (expr, _,  sub_pr) ->
       (* No def-dependencies in an [expr]. *)
       let expr_decl = expr_decl_dependencies ~current_species expr in
       let (sub_proof_decl_deps, sub_proof_def_deps) =
         proof_decl_n_def_dependencies ~current_species sub_pr in
       ((Parsetree_utils.SelfDepSet.union expr_decl sub_proof_decl_deps),
        sub_proof_def_deps)
;;



let binding_body_decl_dependencies ~current_species = function
  | Parsetree.BB_computational e -> expr_decl_dependencies ~current_species e
  | Parsetree.BB_logical p -> prop_decl_dependencies ~current_species p
;;



(* ******************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                        *)
(*   Env.TypeInformation.species_field -> Parsetree_utils.DepNameSet.t  *)
(** {b Descr} : Compute the set of vnames the argument field depends of
    in the species [~current_species].

    {b Exported} : Yes.                                                 *)
(* ******************************************************************** *)
let field_only_decl_dependencies ~current_species = function
  | Env.TypeInformation.SF_sig (_, _, _) ->
      Parsetree_utils.SelfDepSet.empty
  | Env.TypeInformation.SF_let (_, _, _, _, body, _, rep_deps, _) ->
      (* We simply ignore the termination proof stuff since for non recursive
         functions this is not needed. In fact, we could even ensure that
         there is no proof on a non-recursive function. *)
      let body_deps = binding_body_decl_dependencies ~current_species body in
      (* Take into account dependencies on the carrier. *)
      if rep_deps.Env.TypeInformation.dor_decl then
        Parsetree_utils.SelfDepSet.add
          ((Parsetree.Vlident "rep"), (Types.type_self ())) body_deps
      else body_deps
  | Env.TypeInformation.SF_let_rec l ->
      (begin
      (* Create the set of names to remove afterwards. *)
      let names_of_l =
        List.fold_left
          (fun accu_set (_, n, _, _, _, _, _, _) ->
            (* Give a dummy type variable as type... *)
            Parsetree_utils.SelfDepSet.add
              (n, Types.type_variable ()) accu_set)
          Parsetree_utils.SelfDepSet.empty
          l in
      (* Now, compute the dependencies on all the rec-bound-names but we are
         not interested in proofs yet. *)
      let deps_of_l =
        List.fold_left
          (fun accu_deps (_, _, _, _, body, _, rep_deps, _) ->
            let d = binding_body_decl_dependencies ~current_species body in
            (* Take into account dependencies on the carrier. *)
            let d' =
              if rep_deps.Env.TypeInformation.dor_decl then
                Parsetree_utils.SelfDepSet.add
                  ((Parsetree.Vlident "rep"), (Types.type_self ())) d
              else d in
            Parsetree_utils.SelfDepSet.union d' accu_deps)
          Parsetree_utils.SelfDepSet.empty
          l in
      (* And now, remove the rec-bound-names from the dependencies. *)
      Parsetree_utils.SelfDepSet.diff deps_of_l names_of_l
      end)
  | Env.TypeInformation.SF_theorem (_, _, _, body, proof, rep_deps) ->
      (begin
      let body_decl_deps = prop_decl_dependencies ~current_species body in
      (* Now, recover the explicit "decl" dependencies of the proof and ignore
         here the "def"-dependencies. *)
      let (proof_deps, _) =
        proof_decl_n_def_dependencies ~current_species proof in
      let deps_without_carrier =
        Parsetree_utils.SelfDepSet.union body_decl_deps proof_deps in
      (* Take into account dependencies on the carrier. *)
      if rep_deps.Env.TypeInformation.dor_decl then
        Parsetree_utils.SelfDepSet.add
          ((Parsetree.Vlident "rep"), (Types.type_self ())) deps_without_carrier
      else deps_without_carrier
      end)
  | Env.TypeInformation.SF_property (_, _, _, body, _) ->
      prop_decl_dependencies ~current_species body
;;



(* ********************************************************************* *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->          *)
(*   Env.TypeInformation.species_field list                              *)
(** {b Descr} : Compute the set of all the names involved in the
    "clockwise arrow" relation (c.f. Virgile Prevosto's Pdh, section
    3.5, page 30) for the name [field_name] in the fields [fields] that
    must include the inherited fields of the analysed species.

    {b Rem} : MUST BE used on a fields list containing all the
    **normalized inherited fields** of the species and the
    **non normalized fields of the current inheritance level**.
    This means that the inherited fields must already be fusionned if
    they needed, and the current level fields may not be fusionned this
    the inherited ones.
    This enables not to have to seach all along the inheritance tree
    because let rec will be inductively correct (i.e. all the recursive
    idents will be in ONE field) for inherited fields and keeps the
    current level fields present in order to check for let rec at this
    level.                                                               *)
(* ********************************************************************* *)
let clockwise_arrow field_name fields =
  List.fold_right
    (fun field accu ->
      match field with
      | Env.TypeInformation.SF_sig (_, vname, _)
      | Env.TypeInformation.SF_let (_, vname, _, _, _, _, _, _)
      | Env.TypeInformation.SF_theorem (_, vname, _, _, _, _)
      | Env.TypeInformation.SF_property (_, vname, _, _, _) ->
          if vname = field_name then Handy.list_cons_uniq_eq vname accu
          else accu
       | Env.TypeInformation.SF_let_rec l ->
           (* Check if the searched field name is among those in this
              recursive let definition. If so, then the relation includes all
              the bound names of this recursive let definition. *)
           if List.exists
               (fun (_, vname, _, _, _, _, _, _) -> vname = field_name) l then
             List.fold_right
               (fun (_, n, _, _, _, _, _, _) accu' ->
                 Handy.list_cons_uniq_eq n accu')
               l accu
           else accu)
    fields
    []
;;



(* ******************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->         *)
(*   Env.TypeInformation.species_field list                             *)
(** {b Descr} : Computes the set Where as defined in Virgile Prevosto's
    Phd, section 3.5, page 32, definition 15.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let where field_name fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_sig (_, vname, _)
       | Env.TypeInformation.SF_let (_, vname, _, _, _, _, _, _)
       | Env.TypeInformation.SF_theorem (_, vname, _, _, _, _)
       | Env.TypeInformation.SF_property (_, vname, _, _, _) ->
           if vname = field_name then field :: accu else accu
       | Env.TypeInformation.SF_let_rec l ->
           (* Check if the searched field name is among those in this recursive
              let definition. If so, then the relation includes all the bound
              names of this recursive let definition. *)
           if List.exists
               (fun (_, vname, _, _, _, _, _, _) -> vname = field_name) l then
             field :: accu
           else accu)
    fields
    []
;;



(* ******************************************************************* *)
(* Env.TypeInformation.species_field -> Parsetree_utils.DepNameSet.t   *)
(** {b Descr} : Just an helper returning the set of all names bound in
    a species fields.

    {b Exported} : No.                                                 *)
(* ******************************************************************* *)
let names_set_of_field = function
  | Env.TypeInformation.SF_property (_, vname, _, _, _)
  | Env.TypeInformation.SF_theorem (_, vname, _, _, _, _) ->
      let ty = Types.type_prop () in
      Parsetree_utils.SelfDepSet.singleton (vname, ty)
  | Env.TypeInformation.SF_sig (_, vname, sch)
  | Env.TypeInformation.SF_let (_, vname, _, sch, _, _, _, _) ->
      let ty = Types.specialize sch in
      Parsetree_utils.SelfDepSet.singleton (vname, ty)
  | Env.TypeInformation.SF_let_rec l ->
      List.fold_left
        (fun accu_names (_, n, _, sch, _, _, _, _) ->
          let ty = Types.specialize sch in
          Parsetree_utils.SelfDepSet.add (n, ty) accu_names)
        Parsetree_utils.SelfDepSet.empty
        l
;;



(* ************************************************************************ *)
(* Env.TypeInformation.species_field list ->                                *)
(*   Parsetree_utils.DepNameSet.elt list                                    *)
(** {b Descr} : Just helper returning the list of all names bound in a list
    of fields. The resulting list preserves the order the names appear in
    the list of fields and in the list of names in case of recursive
    let-def field.
    Example: [let s; sig y; let rec z ... and t] will give the
    list [s; y; z; t].

    {b Exported} : Yes.                                                     *)
(* ************************************************************************ *)
let ordered_names_list_of_fields fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_property (_, n, _, _, _)
       | Env.TypeInformation.SF_theorem (_, n, _, _, _, _) ->
           let ty = Types.type_prop () in
           (n, ty) :: accu
       | Env.TypeInformation.SF_sig (_, n, sch)
       | Env.TypeInformation.SF_let (_, n, _, sch, _, _, _, _) ->
           let ty = Types.specialize sch in
           (n, ty) :: accu
       | Env.TypeInformation.SF_let_rec l ->
           List.fold_right
             (fun (_, n, _, sch, _, _, _, _) accu' ->
               let ty = Types.specialize sch in
               (n, ty) :: accu')
             l accu)
    fields
    []
;;



(* ****************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->       *)
(*   Env.TypeInformation.species_field                                *)
(** {b Descr} : Looks for the most recently defined field that
    let-rec-binds [y_name] among [fields] and return it.
    This function relies on the fact that the field list is ordered
    with oldest inherited fields are in head of the list and the most
    recent are in tail.

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let find_most_recent_rec_field_binding y_name fields =
  (* The search is a simple walk in the list, starting by the head and going
     on with the tail. Because fields list is assumed to be ordered with the
     oldest inherited fields in head, one will have to reverse the initial
     list of field before applying the present function. *)
  let rec rec_search = function
    | [] -> raise Not_found
    | h :: q -> (
        match h with
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _)
         | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
         | Env.TypeInformation.SF_property (_, _, _, _, _) -> rec_search q
         | Env.TypeInformation.SF_let_rec l ->
             if List.exists (fun (_, n, _, _, _, _, _, _) -> n = y_name) l then
               h
             else rec_search q
       ) in
  (* Reverse the list so that most recent names are in head. *)
  rec_search (List.rev fields)
;;



(* *********************************************************************** *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->        *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     Parsetree_utils.DepNameSet.t                                        *)
(** {b Descr} : Implements the second case of the definition 16 in Virgile
    Prevosto's Phd, section 3.5, page 32.

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let union_y_clock_x_etc ~current_species x_name fields =
  let all_ys = clockwise_arrow x_name fields in
  List.fold_left
    (fun accu_deps y_name ->
      (* First compute the great union.
         We look for the most recently defined field that binds [y_name].
         This way, we really "compute" \Cal B_s (y) because \Cal B is defined
         to return the body of the most recent fiedl in the inheritance. *)
      let field_y = find_most_recent_rec_field_binding y_name fields in
      let u =
        Parsetree_utils.SelfDepSet.union
          (field_only_decl_dependencies ~current_species field_y) accu_deps in
      (*  Then remove the recursive bound names. *)
      let rec_bound_names = names_set_of_field field_y in
      Parsetree_utils.SelfDepSet.diff u rec_bound_names)
    Parsetree_utils.SelfDepSet.empty
    all_ys
;;



(* ******************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                        *)
(*   (Parsetree.vname * Parsetree.binding_body) ->                      *)
(*     Env.TypeInformation.species_field list ->                        *)
(*       Parsetree_utils.DepNameSet.t                                   *)
(** {b Descr} : Compute the dependencies of a sig, let or let-rec bound
    name in a species. Namely this is the \lbag x \rbag_s in Virgile
    Prevosto's Pdh, section 3.5, page 32, definition 16.
    Does take into account dependencies on the carrier (they must be
    handled appart).


    {b Rem} : MUST be called only with a [name] sig, let or let-rec
    bound !

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let in_species_decl_dependencies_for_one_function_name ~current_species
    (name, body) fields =
  let where_x = where name fields in
  (* Check if Where (x) does NOT contain Let_rec fields. *)
  if List.for_all
      (function
        | Env.TypeInformation.SF_sig (_, _, _)
        | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _) -> true
        | Env.TypeInformation.SF_let_rec _ -> false
        | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
        | Env.TypeInformation.SF_property (_, _, _, _, _) ->
            (* Because this function is intended to be called only on names
               bound by sig, let or let-rec fields, these cases should never
               arise ! *)
            assert false)
      where_x then binding_body_decl_dependencies ~current_species body
  else union_y_clock_x_etc ~current_species name fields
;;



(* ********************************************************************** *)
(** {b Descr} : Compute the dependencies of a property or theorem bound
    name in a species. Namely this is the \lbag x \rbag_s in Virgile
    Prevosto's Pdh, section 3.9.5, page 53, definition 30. Returns both
    the "decl" (separately those coming from the "type", i.e. the
    proposition of the property/theorem and those coming from the "body",
    i.e. the proof of the theorem) and "def" dependencies.
    Note that names we depend on are inevitably names from the current
    species inheritance tree's fields. That is by definition of the
    dependencies computation !

    {b Rem} : MUST be called only with a [name] property or theorem
    bound !

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
let in_species_decl_n_def_dependencies_for_one_theo_property_name
    ~current_species (t_prop, opt_body) =
  let t_prop_decl_deps = prop_decl_dependencies ~current_species t_prop in
  let (opt_body_decl_deps, opt_body_def_deps) =
    (match opt_body with
     | None ->
         (* No body, then no "def" not "decl"-dependencies. *)
         (Parsetree_utils.SelfDepSet.empty,
          Parsetree_utils.SelfDepSet.empty)
     | Some proof -> proof_decl_n_def_dependencies ~current_species proof) in
  (t_prop_decl_deps, opt_body_decl_deps, opt_body_def_deps)
;;



(* ********************************************************** *)
(** {b Descr} : Raised if a species appears to be ill-formed.

    {b Exported} : Yes.                                       *)
(* ********************************************************** *)
exception Ill_formed_species of
  (Parsetree.qualified_vname *  (** Species considered as ill-formed. *)
   DepGraphData.name_node *     (** The field name that trigerred the
                                    ill-formation. *)
   DepGraphData.name_node list)   (** The undeclared recusion path discovered,
                                      proving that the species is ill-formed. *)
;;



(* ****************************************************************** *)
(* DepGraphData.name_node list ref ->                                 *)
(*  (Parsetree.vname * Types.type_simple) -> DepGraphData.name_node   *)
(** {b Descr} : Looks for a node labeled [name] in the list of nodes
    [tree_nodes]. If a node with this name is found, then we return
    it. Otherwise, a fresh node is created with [name] as name and no
    child, and this fresh node is returned.
    This is mostly a helper for the function
    [build_dependencies_graph_for_fields].

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let find_or_create tree_nodes (name, ty) =
  try List.find (fun node -> node.DepGraphData.nn_name = name) !tree_nodes
  with Not_found ->
    let new_node = {
      DepGraphData.nn_name = name;
      DepGraphData.nn_type = ty; DepGraphData.nn_children = [] } in
    tree_nodes := new_node :: !tree_nodes;
    new_node
;;



(* ***************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                     *)
(*   Env.TypeInformation.species_field list ->                       *)
(*     DepGraphData.name_node list                                   *)
(** {b Descr} : Build the dependencies graph of the names present in
    the fields list [fields] of the species [~current_species].
    In such a graph, if an arrow exists from n1 to n2, then it means
    that in the body of n1, call(s) to n2 is (are) performed.

    {b Exported} : Yes.                                              *)
(* ***************************************************************** *)
let build_dependencies_graph_for_fields ~current_species fields =
  (* The root hoot used to remind all the created nodes in the graph. *)
  let tree_nodes = ref ([] : DepGraphData.name_node list) in

  (* ********************************************************************* *)
  (** {b Descr} : Just make a local function dealing with one let binding.
      We then use it once for a Let and iter it for a Let_Rec.
      Apply the rules section 3.5, page 32, definition  16 to get the
      dependencies.                                                        *)
  (* ********************************************************************* *)
  let local_build_for_one_let n ty b opt_term_proof dep_on_rep =
    (* Find the dependencies node for the current name. *)
    let n_node = find_or_create tree_nodes (n, ty) in
    (* Check if there is a decl-dependency on "rep". *)
    if dep_on_rep.Env.TypeInformation.dor_decl then
      (begin
      (* Hard-build a node for "rep". *)
      let node =
        find_or_create
          tree_nodes ((Parsetree.Vlident "rep"), (Types.type_self ())) in
      (* Now add an edge from the current name's node to the decl-dependencies
         node of "rep". In "Let/rec" methods, "decl" dependencies can only
         come from the type of the method. *)
      let edge = (node, DepGraphData.DK_decl DepGraphData.DcDK_from_type) in
      n_node.DepGraphData.nn_children <-
        Handy.list_cons_uniq_custom_eq
          (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
          edge n_node.DepGraphData.nn_children
      end);
    (* Find the names decl-dependencies for the current name. *)
    let n_decl_deps_names =
      in_species_decl_dependencies_for_one_function_name
        ~current_species (n, b) fields in
    (* Now, find the decl-dependencies nodes for these names. *)
    let n_deps_nodes =
      Parsetree_utils.SelfDepSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (* In "Let/rec" methods, "decl" dependencies can only come from the
             BODY of the method. *)
          (node, (DepGraphData.DK_decl DepGraphData.DcDK_from_body)) :: accu)
        n_decl_deps_names
        [] in
    (* Now add an edge from the current name's node to each of the
       decl-dependencies names' nodes. *)
    n_node.DepGraphData.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_deps_nodes n_node.DepGraphData.nn_children;
    (* We now process termination proof aside.
       Find the names decl-dependencies for the current name in the
       termination proof. *)
    (begin
     match opt_term_proof with
      | None -> ()
      | Some term_pr ->
          let (term_pr_decl_names, term_pr_def_names) =
            termination_proof_decl_n_def_dependencies
              ~current_species term_pr in
          (* Now, find the decl-dependencies nodes for these names. *)
          let n_term_decl_nodes =
            Parsetree_utils.SelfDepSet.fold
              (fun n accu ->
                let node = find_or_create tree_nodes n in
                (node, (DepGraphData.DK_decl DepGraphData.DcDK_from_term_proof))
                :: accu)
              term_pr_decl_names
              [] in
          (* Now, find the def-dependencies nodes for these names. *)
          let n_term_def_nodes =
            Parsetree_utils.SelfDepSet.fold
              (fun n accu ->
                let node = find_or_create tree_nodes n in
                (node, (DepGraphData.DK_def DepGraphData.DfDK_from_term_proof))
                :: accu)
              term_pr_def_names
              [] in
          (* Now add an edge from the current name's node to each of the
             dependencies names' nodes. *)
          n_node.DepGraphData.nn_children <-
            Handy.list_concat_uniq_custom_eq
              (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
              n_term_decl_nodes n_node.DepGraphData.nn_children;
          n_node.DepGraphData.nn_children <-
            Handy.list_concat_uniq_custom_eq
              (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
              n_term_def_nodes n_node.DepGraphData.nn_children
    end);
    (* Now, check if there is a def-dependency on "rep". *)
    if dep_on_rep.Env.TypeInformation.dor_def then
      (begin
      (* Hard-build a node for "rep". *)
      let node =
        find_or_create
          tree_nodes ((Parsetree.Vlident "rep"), (Types.type_self ())) in
      (* Now add an edge from the current name's node to the def-dependencies
         node of "rep". We tag it as not coming from the termination proof
         since anyway if the termination proof refers to "Self", then the
         parts outside the proof obviously also. Hence, the carrier will
         always need to be lambda-lifted. *)
      let edge =
        (node, (DepGraphData.DK_def DepGraphData.DfDK_not_from_term_proof)) in
      n_node.DepGraphData.nn_children <-
        Handy.list_cons_uniq_custom_eq
          (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
          edge n_node.DepGraphData.nn_children
      end)
    (* Note that in a "Let", there cannot be other def-dependencies than on
       the carrier. So, non need to check for other def-dependencies *) in


  (* ***************************************************************** *)
  (** {b Descr} : Just make a local function dealing with one property
      or theorem name.
      Apply rules from section 3.9.5, page 53, definition 30.          *)
  (* ***************************************************************** *)
  let local_build_for_one_theo_property n ty prop_t opt_b dep_on_rep =
    (* Find the dependencies node for the current name. *)
    let n_node = find_or_create tree_nodes (n, ty) in
    (* Check if there is a decl-dependency on "rep". *)
    if dep_on_rep.Env.TypeInformation.dor_decl then
      (begin
      (* Hard-build a node for "rep". *)
      let node =
        find_or_create
          tree_nodes ((Parsetree.Vlident "rep"), (Types.type_self ())) in
      (* Now add an edge from the current name's node to the node of "rep".
         Even in a theorem, a decl-dependency on the carrier can only come
         from the type (of course, one can't say in the body, i.e. in the
         proof, "by def Self" or "by property Self" !). *)
      let edge = (node, DepGraphData.DK_decl DepGraphData.DcDK_from_type) in
      n_node.DepGraphData.nn_children <-
        Handy.list_cons_uniq_custom_eq
          (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
          edge n_node.DepGraphData.nn_children
      end);
    (* Find the names decl and defs dependencies for the current name. *)
    let (n_decl_deps_names_from_type,
         n_decl_deps_names_from_body,
         n_def_deps_names) =
      in_species_decl_n_def_dependencies_for_one_theo_property_name
        ~current_species (prop_t, opt_b) in
    (* Now, find the decl-dependencies nodes for these names. *)
    let n_decl_deps_nodes =
      Parsetree_utils.SelfDepSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, (DepGraphData.DK_decl DepGraphData.DcDK_from_type)) :: accu)
        n_decl_deps_names_from_type
        [] in
    let n_decl_deps_nodes =
      Parsetree_utils.SelfDepSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, (DepGraphData.DK_decl DepGraphData.DcDK_from_body)) :: accu)
        n_decl_deps_names_from_body
        n_decl_deps_nodes in
    (* Now add an edge from the current name's node to each of the
       decl-dependencies names' nodes. *)
    n_node.DepGraphData.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_decl_deps_nodes n_node.DepGraphData.nn_children;
    (* Now, find the def-dependencies nodes for these names. *)
    let n_def_deps_nodes =
      Parsetree_utils.SelfDepSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, (DepGraphData.DK_def DepGraphData.DfDK_not_from_term_proof))
          :: accu)
        n_def_deps_names
        [] in
    (* Now add an edge from the current name's node to each of the
       def-dependencies names' nodes. *)
    n_node.DepGraphData.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_def_deps_nodes n_node.DepGraphData.nn_children;
    (* Now, check if there is a def-dependency on "rep". *)
    if dep_on_rep.Env.TypeInformation.dor_def then
      (begin
      (* Hard-build a node for "rep". *)
      let node =
        find_or_create
          tree_nodes ((Parsetree.Vlident "rep"), (Types.type_self ())) in
      (* Now add an edge from the current name's node to the def-dependencies
         node of "rep". *)
      let edge =
        (node, (DepGraphData.DK_def DepGraphData.DfDK_not_from_term_proof)) in
      n_node.DepGraphData.nn_children <-
        Handy.list_cons_uniq_custom_eq
          (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
          edge n_node.DepGraphData.nn_children
      end) in

  (* *************** *)
  (* Now do the job. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (_, n, sch) ->
          if not
              (List.exists
                 (fun node -> node.DepGraphData.nn_name = n) !tree_nodes) then
            (begin
            let ty = Types.specialize sch in
            tree_nodes :=
              { DepGraphData.nn_name = n; DepGraphData.nn_type = ty;
                DepGraphData.nn_children = [] } :: !tree_nodes
            end)
      | Env.TypeInformation.SF_let (_, n, _, sch, b, _, deps_on_rep, _) ->
          let ty = Types.specialize sch in
          (* ALways ignore the proof if some in non-recursive functions. *)
          local_build_for_one_let n ty b None deps_on_rep
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, n, _, sch, b, opt_term_proof, deps_on_rep, _) ->
              let ty = Types.specialize sch in
              local_build_for_one_let n ty b opt_term_proof deps_on_rep)
            l
      | Env.TypeInformation.SF_theorem (_, n, _, prop, body, deps_on_rep) ->
          let ty = Types.type_prop () in
          local_build_for_one_theo_property n ty prop (Some body) deps_on_rep;
      | Env.TypeInformation.SF_property (_, n, _, prop, deps_on_rep) ->
          let ty = Types.type_prop () in
          local_build_for_one_theo_property n ty prop None deps_on_rep)
    fields;
  (* Return the list of nodes of the graph. *)
  !tree_nodes
;;



(* ************************************************************************ *)
(* dirname: string -> current_species: Parsetree.qualified_vname) ->        *)
(*   DepGraphData.name_node list -> unit                                    *)
(** {b Descr} : Prints the dependencies graph of a species in dotty format.

    {b Exported} : Yes.                                                     *)
(* ************************************************************************ *)
let dependencies_graph_to_dotty ~dirname ~current_species tree_nodes =
  (* For each species, a file named with "deps_", the species name and the
     suffix ".dot" will be generated in the directory. *)
  let (current_species_module, current_species_vname) = current_species in
  let out_filename =
    Filename.concat
      dirname
      ("deps_" ^ current_species_module ^ "_" ^
       (Parsetree_utils.name_of_vname current_species_vname) ^ ".dot") in
  let out_hd = open_out_bin out_filename in
  (* First, outputs the header of the dotty file. *)
  Printf.fprintf out_hd "digraph G {\n";
  (* Output the meaning of the colors code just to remind the user. *)
  Printf.fprintf out_hd "\n\
\"def dep\" [fontsize=10]\n\
\"def dep (in term proof)\" [fontsize=10]\n\
\"decl dep (in type)\" [fontsize=10]\n\
\"decl dep (in body)\" [fontsize=10]\n\
\"decl dep (in term proof)\" [fontsize=10]\n\
\"def dep\" -> \"def dep\" [style=dotted,color=blue,fontsize=10];\n\
\"def dep (in term proof)\" -> \"def dep (in term proof)\" [style=dotted,color=yellow,fontsize=10];\n\
\"decl dep (in type)\" -> \"decl dep (in type)\" [color=red,fontsize=10];\n\
\"decl dep (in body)\" -> \"decl dep (in body)\" [color=pink,fontsize=10];\n\
\"decl dep (in term proof)\" -> \"decl dep (in term proof)\" [color=purple,fontsize=10];\n";
  (* Outputs all the nodes of the graph. *)
  List.iter
    (fun { DepGraphData.nn_name = n } ->
      Printf.fprintf out_hd "\"%s\" [shape=box,fontsize=10];\n"
        (Parsetree_utils.name_of_vname n))
    tree_nodes;
  (* Outputs all the edges between the nodes. *)
  List.iter
    (fun { DepGraphData.nn_name = n; DepGraphData.nn_children = children } ->
      List.iter
        (fun ({ DepGraphData.nn_name = child_name }, decl_kind) ->
          (* Just make a different style depending on the kind of dependency. *)
          let (style, color) =
            (match decl_kind with
             | DepGraphData.DK_decl DepGraphData.DcDK_from_type -> ("", "red")
             | DepGraphData.DK_decl DepGraphData.DcDK_from_body -> ("", "pink")
             | DepGraphData.DK_decl DepGraphData.DcDK_from_term_proof ->
                 ("", "purple")
             | DepGraphData.DK_def DepGraphData.DfDK_not_from_term_proof ->
                 ("style=dotted,", "blue")
             | DepGraphData.DK_def DepGraphData.DfDK_from_term_proof ->
                 ("style=dotted,", "yellow") ) in
          Printf.fprintf out_hd
            "\"%s\" -> \"%s\" [%scolor=%s,fontsize=10];\n"
            (Parsetree_utils.name_of_vname n)
            (Parsetree_utils.name_of_vname child_name) style color)
        children)
    tree_nodes;
  (* Finally, outputs the trailer of the dotty file. *)
  Printf.fprintf out_hd " \n}\n";
  close_out out_hd
;;



(* ********************************************************************** *)
(** {b Descr} : Module stuff to create maps of [DepGraphData.name_node]s.

    {b Exported} : No.                                                    *)
(* ********************************************************************** *)
module NameNodeMod = struct
  type t = DepGraphData.name_node
  let compare nn1 nn2 =
    compare nn1.DepGraphData.nn_name nn2.DepGraphData.nn_name
end
;;
module NameNodeMap = Map.Make (NameNodeMod);;



(* ******************************************************************** *)
(* type name_node -> int                                                *)
(** {b Descr} : Compute the "out degree" of a node, i.e. the number of
    DIFFERENT children nodes it has. By different, we mean that 2 edges
    of different kinds between 2 same nodes are considered as only 1
    edge.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let node_out_degree node =
  let count = ref 0 in
  let seen = ref ([] : Parsetree.vname list) in
  List.iter
    (fun (n, _) ->
      if not (List.mem n.DepGraphData.nn_name !seen) then
        (begin
        seen := n.DepGraphData.nn_name :: !seen;
        incr count
        end))
    node.DepGraphData.nn_children;
  !count
;;



(* ************************************************************************** *)
(* DepGraphData.nanem_node list -> Parsetree.vname list                       *)
(** {b Descr} : Determines the order of apparition of the names inside a
    species from its dependency graph.
    The aim is to prevent fields depending on other fields from appearing
    before. In others words, this prevents from having [lemma2; lemma1] if
    the proof of [lemma2] requires [lemma1].
    We then must make in head of the species, the deepest fields in the
    dependency graph, before adding those of the immediately upper level,
    and so on.
    Hence, this is a kind of reverse-topological sort. In effect in our graph
    an edge i -> j does not mean that i must be "processed" before j, but
    exactly the opposite !
    If M1 depends on M2 then M2 will in front of M1 in the resulting list.
    This ways, we are sure that any apparition of M2 in M1 will be bound in
    the generated codes.

    {b Rem} : Because of well-formation properties, this process should never
    find a cyclic graph. If so, then may be the well-formness process is
    bugged somewhere-else.

    {b Exported} : No.                                                        *)
(* ************************************************************************** *)
let ___compute_names_reordering dep_graph_nodes =
  (* Map recording for each node its "outputs degree", *)
  (* that's to say, the number of children it has.     *)
  let out_degree = ref NameNodeMap.empty in
  (* First, initialize the out degree of each node, taking care not to
     double-count 2 edges of different dependency kind between two same
     nodes. *)
  List.iter
    (fun name_node ->
      let nb_distict_children = node_out_degree name_node in
      out_degree :=
        NameNodeMap.add name_node (ref nb_distict_children) !out_degree)
    dep_graph_nodes;
  (* The working list... *)
  let c_queue = Queue.create () in
  (* Initialization with nodes having a out degree equal to 0.*)
  NameNodeMap.iter
    (fun node degree ->
      if !degree = 0 then
        begin
        Queue.push node c_queue;
        out_degree := NameNodeMap.remove node !out_degree
        end)
    !out_degree;
  (* The list with the newly ordered fields names. We build it reversed for
     sake of efficiency. We will need to reverse it at the end. *)
  let revd_order_list = ref ([] : Parsetree.vname list) in
  (* Now, iterate until the working list gets empty. *)
  (begin
  try
    while true do
      let j = Queue.take c_queue in
      (* [j] can now be output. *)
      revd_order_list := j.DepGraphData.nn_name :: !revd_order_list;
      (* Search all parents, i, of  j to decrement their out degree. *)
      NameNodeMap.iter
        (fun i i_out_degree ->
          (* Tests if [j] belongs to [i]'s children, ignoring the dependency
             kind. *)
          let is_i_parent_of_j =
            List.exists
              (fun (child, _) -> child == j) i.DepGraphData.nn_children in
          if is_i_parent_of_j then
            (begin
            (* The node [j] appears in [i]'s childrens, hence [i] is right a
               parent of [j]. *)
            decr i_out_degree;
            if !i_out_degree = 0 then
              (begin
              (* This parent is now of degree 0, it can now be processed
                 because all its children have already been. *)
              Queue.push i c_queue;
              out_degree := NameNodeMap.remove i !out_degree
              end)
            end))
        !out_degree
    done
  with Queue.Empty ->
    (* If there remain nodes in the [out_degree] table, this means that there
       exists nodes one could not classify because their degree never reached
       0. This corresponds to cycles in the graph. Because on well-formness
       properties, this should never appear except if the well-formness
       algorithm is buggy somewhere else. *)
    if not (NameNodeMap.is_empty !out_degree) then assert false
  end);
  (* And finaly, reverse the order list to get it in the right ... order. *)
  List.rev !revd_order_list
;;



(* ************************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                              *)
(*   Env.TypeInformation.species_field list -> Parsetree.vname list           *)
(** {b Descr} : Determines the order of apparition of the fields inside a
    species to prevent fields depending on other fields from appearing
    before.
    Exported outside this module.                                             *)
(* ************************************************************************** *)
let compute_fields_reordering ~current_species fields =
  (* First, compute the depency graph of the species fields. *)
  let dep_graph_nodes =
    build_dependencies_graph_for_fields ~current_species fields in
  ___compute_names_reordering dep_graph_nodes
;;



let order_species_params_methods spe_params_n_meths_set =
  List.map
    (fun (species_param, meths_set) ->
      match species_param with
       | Env.TypeInformation.SPAR_in (_, _, _) ->
           (* Trivially no problem since "IN" parameters only have 1 method
              wearing structurally the same name than the parameter. *)
           (species_param,
            (Env.ODFP_methods_list
               (Parsetree_utils.ParamDepSet.elements meths_set)))
       | Env.TypeInformation.SPAR_is ((_, _), _, _, _, dep_graph) ->
           (* We first build an ordered list. *)
           let order =  ___compute_names_reordering dep_graph in
           (* In this ordered list, we them only keep the names appearing in
              the original [meths_set] set of names. *)
           (* Directly make the list containing the elements of the set. This
              will be easier to manipulate ! *)
           let meths_set_as_list =
             Parsetree_utils.ParamDepSet.elements meths_set in
           let rec prune = function
             | [] -> []
             | h :: q ->
                 (begin
                 try
                   let kept =
                     List.find (fun (n, _) -> n = h) meths_set_as_list in
                   kept :: (prune q)
                 with Not_found -> prune q
                 end) in
           (species_param, (Env.ODFP_methods_list (prune order))))
    spe_params_n_meths_set
;;



(* ******************************************************************** *)
(* DepGraphData.name_node -> DepGraphData.name_node ->                  *)
(*   DepGraphData.name_node list                                        *)
(** {b Descr} : Checks for a non trivial path from [start_node] to
    [start_node]. "Non-trivial" means that the path must at least be of
    length 1.
    ATTENTION: Since now "rep" is a field like the others, but does not
    participate to the path detection, we must prevent search through
    paths involving "rep", hence stop as soon as we find it, returning
    [false].
    If a path exists, we return the list of nodes forming it WITHOUT
    the last node (the one ending the searched path) since we don't
    need it for error reporting purpose. Hence, since the searcher path
    is non-trivial, it always content at least ONE node, i.e. the
    starting node.
    If no path is found, we return the empty list.

    {b Exported} : No.                                                  *)
(* ******************************************************************** *)
let is_reachable start_node end_node =
  (* List of already seen nodes. Will be extended during the search. *)
  let seen = ref ([] : DepGraphData.name_node list) in

  let rec find_on_children path = function
    | [] -> []   (* No path found. *)
    | (n, _) :: q ->
        match rec_search path n with
         | [] ->
             (* Try to find a path on the other children. *)
             find_on_children path q
         | found -> found    (* We found one path. Then stop search. *)

  and rec_search path current_node =
    (* If the current node was already seen, this means that ... we already
       saw it, then we already checked if the [end_node] was in its children
       and the anwser was NOT. Hence there is no reason to start again the
       search, we will get the same answer forever (and loop forever of course
       by the way). *)
    if List.memq current_node !seen ||
       current_node.DepGraphData.nn_name = (Parsetree.Vlident "rep") then []
    else
      (begin
      (* We build the path in reverse order for sake of efficiency. *)
      let path' = current_node :: path in
      (* We check if the current node's children contain the [end_node]. This
         way, for each node, we are sure that the possibly found path is not
         the trivial path (because we explicitly look inside the children and
         if a node is acceptable in the children, then the path length is
         mandatorily non-null). *)
      if List.exists
          (fun (n, _) -> n == end_node)
          current_node.DepGraphData.nn_children then
        path'
      else
        (begin
        seen := current_node :: !seen;
        (* The [end_node] was not found in the children, then search in the
           children. *)
        find_on_children path' current_node.DepGraphData.nn_children
        end)
      end) in
  (* Start the search with an empty path history and put back the path in the
     rigth order. *)
  List.rev (rec_search [] start_node)
;;



(* ****************************************************************** *)
(* name_node list -> -> Parsetree.vname -> Parsetree.vname ->         *)
(*   Env.TypeInformation.species_field list ->                        *)
(*     (name_node * name_node) option                                 *)
(** {b Descr} : Implements the relation "left-oriented triangle" of
    Virgile Prevosto's Phd, section 3.5, page 32, definition 17.
    We added a way to simply detect and pinpoint the ill-formation
    of the species by returning [Some] of the 2 conflicting fields.
    Then according to the original definition, None means [false] and
    [Some] means [true] (i.e. there exists a path leading to a
    ill-formation) and we provide by the way the 2 fields between
    which the path exists.

    {b Exported} : No.                                                *)
(* ****************************************************************** *)
let left_triangle dep_graph_nodes x1 x2 fields =
  (* Guess the fields where x1 is recursively bound. *)
  let x1_arrow = clockwise_arrow x1 fields in
  (* Guess the fields where x2 is recursively bound. *)
  let x2_arrow = clockwise_arrow x2 fields in
  (* Now we will apply the "well-formness" predicate on the cartesian product
     of the names bound by "clockwise-arrow" of [x1] and those bound by
     "clockwise-arrow" of [x2]. Intuitively, we will apply this predicate on
     all possible combinaisons of [y_1] and [y_n]. *)
  let bad_formed = ref None in
  let check () =
    List.exists
      (fun y1 ->
        List.exists
          (fun yn ->
            (* Search a path in the graph from yn to y1. The lookup for nodes
               y1 and yn should never fail because the graph was created
               before. *)
            let y1_node =
              List.find
                (fun node -> node.DepGraphData.nn_name = y1) dep_graph_nodes in
            let yn_node =
              List.find
                (fun node -> node.DepGraphData.nn_name = yn) dep_graph_nodes in
            (* Because our graph edges link from yn to y1, we must invert the
               start/end nodes. *)
            match is_reachable yn_node y1_node with
             | [] -> false   (* No path found. *)
             | found_path ->
                 (* That's a bit casual a programming fashion, but it works..
                    This allows to easily return the error reason... *)
                 bad_formed := Some (yn_node, y1_node, found_path);
                 true)
          x2_arrow)
      x1_arrow in
  ignore (check ());
  (* Oooooh, cheater ! *)
  !bad_formed
;;



(* ************************************************************************ *)
(* current_species: Parsetree.qualified_vname ->                            *)
(*   Env.TypeInformation.species_field list -> unit                         *)
(** {b Descr} : Checks if a species is well-formed, applying the definition
    17 in Virgile Prevosto's Phd, section 3.5, page 32.

    {b Exported} : Yes.                                                     *)
(* ************************************************************************ *)
let ensure_species_well_formed ~current_species fields =
  let names =
    List.map fst (ordered_names_list_of_fields fields) in
  (* Now, let's build the global dependencies graph for all the names. *)
  let dep_graph_nodes =
    build_dependencies_graph_for_fields ~current_species fields in
  (* Now check the well-formness. *)
  List.iter
    (fun x_name ->
      let ill_f = left_triangle dep_graph_nodes x_name x_name fields in
      match ill_f with
       | Some (node1, _, found_path) ->
           (* Forget the second node, we always call the path-search with
              twice the same name... *)
           let (modname, species_vname) = current_species in
           raise
             (Ill_formed_species
                ((Parsetree.Qualified (modname, species_vname)), node1,
                 found_path))
       | None ->
           (* No path leading to recursive fields that were not declared
              recursive at the origin, hence, all is right... *)
           ())
    names
;;



(* ********************************************************************** *)
(* Env.TypeInformation.species_field ->                                   *)
(*   Env.TypeInformation.species_field list                               *)
(** {b Descr} : Implements the erasing procedure of one field described
    in Virgile Prevosto's Phd, Section 3.9.5, page 53, definition 33.

    {b Rem} : Because the erasing of one Let_rec leads to several Sig
    fields this function takes 1 fields and may return several.
    In the same spirit, because we don't have any "silent"  Sig for "rep"
    (of course, "rep" is always a Sig when present), if we find "rep"
    defined, then the only way to abstract it is to remove it. Hence this
    function may also return an empty list of fields.

   {b Exported} : No.                                                      *)
(* *********************************************************************** *)
let erase_field ~current_species field =
  match field with
  | Env.TypeInformation.SF_sig (from, vname, _) ->
    (* Also includes "rep". *)
    let m_name_as_str = Parsetree_utils.name_of_vname vname in
    if m_name_as_str = "rep" then
      (begin
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname vname
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition;
      []  (* No explicit "rep" means ... no "rep". *)
      end)
    else [field]
  | Env.TypeInformation.SF_let (from, vname, _, sch, _, _, _, _) ->
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname vname
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
      (* Turn the "let" into a "sig". *)
      [Env.TypeInformation.SF_sig (from, vname, sch)]
  | Env.TypeInformation.SF_let_rec l ->
      (* Just turn the whole list into "sig"s. *)
      List.map
        (fun (from, n, _, sch, _, _, _, _) ->
          if Configuration.get_verbose () then
            Format.eprintf "Erasing field '%a' coming from '%a'.@."
              Sourcify.pp_vname n
              Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
          Env.TypeInformation.SF_sig (from, n, sch))
        l
  | Env.TypeInformation.SF_theorem (from, n, num_ty_vars, prop, _, deps_rep) ->
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname n
          Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
      (* Turn the "theorem" into a "property".
         Hence, destroys any def-dependency on the carrier ! Generate a
         warning since the proof is not valid anymore, hence if the species
         was able to be turned into a collection, it can't anymore. *)
      Format.eprintf
        "@[%tWarning:%t In species@ '%t%a%t'@ proof@ of@ method@ '%t%a%t'\
         @ inherited@ from@ '%t%a%t'@ was@ invalidated.@ Proof@ has@ to@ be\
         @ done@ again.@]@."
         Handy.pp_set_bold Handy.pp_reset_effects
         Handy.pp_set_underlined
         Sourcify.pp_qualified_species current_species
         Handy.pp_reset_effects
         Handy.pp_set_underlined
         Sourcify.pp_qualified_species from.Env.fh_initial_apparition
         Handy.pp_reset_effects
         Handy.pp_set_underlined
         Sourcify.pp_vname n
         Handy.pp_reset_effects ;
(* [Unsure] Recalculer ces dépendances ? Je pense que le plus safe serait
   de ne pas les changer ! *)
      let deps_rep' = { deps_rep with Env.TypeInformation.dor_def = false } in
      [Env.TypeInformation.SF_property (from, n, num_ty_vars, prop, deps_rep')]
  | _ -> [field]                       (* Everything else is unchanged. *)
;;



(* ****************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                      *)
(*   Parsetree_utils.DepNameSet.elt list ->                           *)
(*     Env.TypeInformation.species_field list ->                      *)
(*       Env.TypeInformation.species_field list                       *)
(** {b Descr} : Implements the erasing procedure in a list of fields
    [fields] as described in Virgile Prevosto's Phd, Section 3.9.5,
    page 53, definition 33.
    Erases in the list of fields definitions according to the context
    represented by the list of names [context].

    {b Exported} : Yes.                                               *)
(* ****************************************************************** *)
let erase_fields_in_context ~current_species context fields =
  (* Now, the recursive function dealing with each field... *)
  let rec rec_erase rec_context = function
    | [] -> []
    | m_field :: l_rem_fields ->
      (begin
      (* We check if the [m_field] is already astracted. *)
      match m_field with
      | Env.TypeInformation.SF_sig (_, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _, _) ->
          (* [m_field] is already astracted. If so, then nothing to do on
             it and just go on with the remaining fields [l_rem_fields].  *)
          m_field :: (rec_erase rec_context l_rem_fields)
      | _ ->
          (* All other cases. First compute the intersection between
             [m_field]'s def-dependencies and the context. This is the
             \lbag\lbag m \rbag\rbag \inter \Cal N formula  of definition 33,
             page 53. *)
          let def_deps =
            (match m_field with
             | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _) ->
                 (* No "def"-dependencies for non-recursive functions
                    since no proof (C.f. definition 30 in Virgile Prevosto's
                    Phd, section 3.9.5, page 53). *)
                 Parsetree_utils.SelfDepSet.empty
             | Env.TypeInformation.SF_let_rec rec_funs -> (
                 (* We will get the def-dependencies from the optional
                    termination proofs. *)
                 List.fold_left
                   (fun accu (_, _, _, _, _, opt_term_proof, _, _) ->
                     match opt_term_proof with
                      | None -> accu
                      | Some term_proof ->
                          let (_, defs) =
                            termination_proof_decl_n_def_dependencies
                              ~current_species term_proof in
                          Parsetree_utils.SelfDepSet.union accu defs)
                   Parsetree_utils.SelfDepSet.empty
                   rec_funs
                )
             | Env.TypeInformation.SF_theorem (_, _, _, prop, proof, _) ->
               let (_, _, n_def_deps_names) =
                 in_species_decl_n_def_dependencies_for_one_theo_property_name
                   ~current_species (prop, (Some proof)) in
               (* Just return the "def"-dependencies. *)
               n_def_deps_names
             | Env.TypeInformation.SF_property (_, _, _, _, _)
             | Env.TypeInformation.SF_sig (_, _, _) ->
                 (* Can not arise because these cases were matched above. *)
                 assert false) in
          (* Compute the intersection with the context... *)
          if Parsetree_utils.SelfDepSet.is_empty
               (Parsetree_utils.SelfDepSet.inter def_deps rec_context) then
            (* Intersection is empty. *)
            m_field :: (rec_erase rec_context l_rem_fields)
          else
            (* Intersection non-empty. Erase the current field. *)
            (begin
            (* Useful information if verbose mode enabled. *)
            if Configuration.get_verbose () then
              (begin
              (* Verbose information. *)
              let (erase_from, erase_names) =
                (match m_field with
                 | Env.TypeInformation.SF_let (from, n, _, _, _, _, _, _)
                 | Env.TypeInformation.SF_theorem (from, n, _, _, _, _)
                 | Env.TypeInformation.SF_property (from, n, _, _, _)
                 | Env.TypeInformation.SF_sig (from, n, _) -> (from, [n])
                 | Env.TypeInformation.SF_let_rec flds ->
                     (* Should nevers fail since "let"s must bind at least one
                        identifier. *)
                     let (from, _, _, _, _, _, _, _) = List.hd flds in
                     let ns =
                       List.map (fun (_, n, _, _, _, _, _, _) -> n) flds in
                     (from, ns)) in
              (* Print field(s) name(s) to erase. *)
              Format.eprintf "Field(s): ";
              List.iter
                (fun n -> Format.eprintf "'%a' " Sourcify.pp_vname n)
                erase_names;
              Format.eprintf  "from '%a' must be erased in context: "
                Sourcify.pp_qualified_species
                erase_from.Env.fh_initial_apparition;
              (* Print the context in which the fields must be erased. *)
              Parsetree_utils.SelfDepSet.iter
                (fun (n, _) -> Format.eprintf "%a, " Sourcify.pp_vname n)
                rec_context;
              Format.eprintf "@.";
              Format.eprintf "since intersection with context is non-empty: ";
              Parsetree_utils.SelfDepSet.iter
                (fun (n, _) -> Format.eprintf "%a, " Sourcify.pp_vname n)
                (Parsetree_utils.SelfDepSet.inter def_deps rec_context);
              Format.eprintf ".@.";
              end);
            (* Now, really process erasing. *)
            let erased_m_field = erase_field ~current_species m_field in
            (* Extent the erasing context with names of the current field. *)
            let new_context =
              Parsetree_utils.SelfDepSet.union
                rec_context (names_set_of_field m_field) in
            (* And then process the remaining fields. *)
            erased_m_field @ (rec_erase new_context l_rem_fields)
            end)
      end) in
  (* ***************** *)
  (* Now do the job... *)
  (* Build once for all the set on names contained in the context list. *)
  let context_as_set =
    List.fold_left
      (fun accu n -> Parsetree_utils.SelfDepSet.add n accu)
      Parsetree_utils.SelfDepSet.empty context in
  rec_erase context_as_set fields
;;
