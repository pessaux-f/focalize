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

(* $Id: dep_analysis.ml,v 1.31 2008-01-25 15:21:10 pessaux Exp $ *)

(* *********************************************************************** *)
(** {b Descr} : This module performs the well-formation analysis described
              in Virgile Prevosto's Phd, section 3.5.

    {b Rem} : Be careful that in the Phd, the well-formation formula is
            incorrect. It should be read:
            x1 <| x2 <=>
              \ex {y_i}_(i=1..n) such as
                (y_1 \circarrow x_1 and y_n \circarrow x_2 and
                 \all j < n, y_j \in \lbag y_j+1 \rbag_s)
              And in the condition for well-formation, <| must be
              subscripted by s and not a.                                  *)
(* *********************************************************************** *)


open Parsetree;;

(* ********************************************************** *)
(** {b Descr} : Raised if a species appears to be ill-formed.

    {b Rem} : Exported outside this module.                   *)
(* ********************************************************** *)
exception Ill_formed_species of Parsetree.qualified_vname ;;



(* ****************************************************************** *)
(* current_species: Parsetree.qualified_vname -> Parsetree.expr ->    *)
(*  Parsetree_utils.DepNameSet.t                                      *)
(** {b Descr} : Compute the set of vnames the expression [expression]
              decl-depends of in the species [~current_species].

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let expr_decl_dependencies ~current_species expression =
  (* Let's just make a local function to save the stack, avoid *)
  (* passing each time the parameter [~current_species].       *)
  let rec rec_depend expr =
    match expr.Parsetree.ast_desc with
     | Parsetree.E_self
     | Parsetree.E_const _ -> Parsetree_utils.DepNameSet.empty
     | Parsetree.E_fun (_, body) -> rec_depend body
     | Parsetree.E_var ident ->
       (begin
       (* Recover the ident's type. *)
       let ident_ty =
         (match ident.Parsetree.ast_type with
          | Parsetree.ANTI_none
          | Parsetree.ANTI_non_relevant
          | Parsetree.ANTI_scheme  _ -> assert false
          | Parsetree.ANTI_type t -> t) in
        match ident.Parsetree.ast_desc with
        | Parsetree.EI_local _ ->
            (* Because scoping pass already renamed all the identfiers that *)
            (* "looked like" local identifiers into "method identifiers" if *)
            (* they indeed denoted methods, we can safely consider that     *)
            (* remaining "local identifiers" are really local and introduce *)
            (* no dependency.                                               *)
            Parsetree_utils.DepNameSet.empty
        | Parsetree.EI_global _ -> Parsetree_utils.DepNameSet.empty
        | Parsetree.EI_method (None, vname) ->
            (* Case c!x in Virgile Prevosto's Phd, section 3.5, *)
            (* page 30, definition 12.                          *)
            Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
        | Parsetree.EI_method (Some coll_specifier, vname) ->
            (begin
             match coll_specifier with
             | Vname _ ->
               (* In this case, may be there is *)
               (* some scoping process missing. *)
               assert false
             | Qualified (module_name, coll_vname) ->
               if current_species = (module_name, coll_vname) then
                 (* Case c!x in Virgile Prevosto's Phd, section 3.5, *)
                 (* page 30, definition 12.                          *)
                 Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
               else Parsetree_utils.DepNameSet.empty
             end)
       end)
     | Parsetree.E_app (fun_expr, args_exprs) ->
         let fun_expr_deps = rec_depend fun_expr in
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           fun_expr_deps
           args_exprs
     | Parsetree.E_constr (_, args_exprs) ->
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           Parsetree_utils.DepNameSet.empty
           args_exprs
     | Parsetree.E_match (matched_e, pats_exprs) ->
         let matched_e_deps = rec_depend matched_e in
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           matched_e_deps
           pats_exprs
     | Parsetree.E_if (if_expr, then_expr, else_expr) ->
         let if_expr_deps = rec_depend if_expr in
         let then_expr_deps = rec_depend  then_expr in
         let else_expr_deps = rec_depend else_expr in
         Parsetree_utils.DepNameSet.union
           if_expr_deps
           (Parsetree_utils.DepNameSet.union then_expr_deps else_expr_deps)
     | Parsetree.E_let (let_def, in_expr) ->
         (* No substration here because the let-definition *)
         (* is NOT a field definition !                    *)
         let in_expr_deps = rec_depend in_expr in
         List.fold_left
           (fun accu_deps binding ->
             Parsetree_utils.DepNameSet.union
               (rec_depend binding.Parsetree.ast_desc.Parsetree.b_body)
               accu_deps)
           in_expr_deps
           let_def.Parsetree.ast_desc.Parsetree.ld_bindings
     | Parsetree.E_record labels_exprs ->
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           Parsetree_utils.DepNameSet.empty
           labels_exprs
     | Parsetree.E_record_access (e, _) -> rec_depend e
     | Parsetree.E_record_with (e, labels_exprs) ->
         let e_deps = rec_depend e in
         List.fold_left
           (fun accu_deps (_, e) ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           e_deps
           labels_exprs
     | Parsetree.E_tuple exprs ->
         List.fold_left
           (fun accu_deps e ->
             Parsetree_utils.DepNameSet.union (rec_depend e) accu_deps)
           Parsetree_utils.DepNameSet.empty
           exprs
     | Parsetree.E_external _ -> Parsetree_utils.DepNameSet.empty
     | Parsetree.E_paren e -> rec_depend e in
  rec_depend expression
;;



(* **************************************************************** *)
(* current_species: Parsetree.qualified_vname -> Parsetree.prop ->  *)
(*   Parsetree_utils.DepNameSet.t                                   *)
(** {b Descr} : Compute the set of vnames the prop expression
              [initial_prop_expression] decl-depends in the species
              [~current_species].

    {b Rem} : Not exported outside this module.                     *)
(* **************************************************************** *)
let prop_decl_dependencies ~current_species initial_prop_expression =
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
         (Parsetree_utils.DepNameSet.union prop1_decl_deps prop2_decl_deps)
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
              Depending on wether the ident appears under a "Def" or "Decl"
              the dependency will be considered as a "decl" or a "def"
              dependency.

    {b Rem} : MUST only called with idents extracted from a [fact]'s
            structure !
            Not exported outside this module.                               *)
(* ************************************************************************ *)
let ident_in_fact_dependencies ~current_species ident =
  (* Recover the ident's type. *)
  let ident_ty =
    (match ident.Parsetree.ast_type with
     | Parsetree.ANTI_none
     | Parsetree.ANTI_non_relevant
     | Parsetree.ANTI_scheme  _ -> assert false
     | Parsetree.ANTI_type t -> t) in
  match ident.Parsetree.ast_desc with
   | Parsetree.EI_local _ ->
       (* Because scoping pass already renamed all the identfiers that *)
       (* "looked like" local identifiers into "method identifiers" if *)
       (* they indeed denoted methods, we can safely consider that     *)
       (* remaining "local identifiers" are really local and introduce *)
       (* no dependency. Furthermore, there is no reason to get here   *)
       (* real local identifier unless the user put an erroneous fact. *)
       failwith "To investigate: may be erroneous fact in the proof."
   | Parsetree.EI_global _ ->
       (* Since dependencies are computed inside A species architecture,   *)
       (* invocation of a global stuff does not involve dependency because *)
       (* it does not belong to the currently analyzed species.            *)
       Parsetree_utils.DepNameSet.empty
   | Parsetree.EI_method (None, vname) ->
       (* A method of Self. *)
       Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
   | Parsetree.EI_method (Some coll_specifier, vname) ->
       (begin
        match coll_specifier with
        | Vname _ -> assert false
        | Qualified (module_name, coll_vname) ->
          (* If the module specification and the collection name       *)
          (* match the [current_species] then are are still in the     *)
          (* case of Self. Else we are in the case of another species. *)
          if (module_name, coll_vname) = current_species then
            (* A method of Self. *)
            Parsetree_utils.DepNameSet.singleton (vname, ident_ty)
           else
            (* Method from another species, then no dep. *)
            Parsetree_utils.DepNameSet.empty
       end)
;;



(* ******************************************************************* *)
(* current_species: Parsetree.qualified_vname -> Parsetree.fact->      *)
(*   (Parsetree_utils.DepNameSet.t * Parsetree_utils.DepNameSet.t)     *)
(** {b Descr} : Compute the set of vnames the fact [fact] decl-depends
              and def-depends of in the species [~current_species].

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let fact_decl_n_def_dependencies ~current_species fact =
  match fact.Parsetree.ast_desc with
   | Parsetree.F_property idents ->
       (* Here are some "decl"-dependencies ! *)
       let decl_deps =
         List.fold_left
           (fun accu ident ->
             Parsetree_utils.DepNameSet.union accu
               (ident_in_fact_dependencies ~current_species ident))
           Parsetree_utils.DepNameSet.empty
           idents in
       (decl_deps, Parsetree_utils.DepNameSet.empty)
   | Parsetree.F_def idents ->
       (* These are "def"-dependencies, not "decl" !!! *)
       let def_deps =
         List.fold_left
           (fun accu ident ->
             Parsetree_utils.DepNameSet.union accu
               (ident_in_fact_dependencies ~current_species ident))
           Parsetree_utils.DepNameSet.empty
           idents in
       (Parsetree_utils.DepNameSet.empty, def_deps)
   | Parsetree.F_hypothesis _
   | Parsetree.F_node _ ->
       (Parsetree_utils.DepNameSet.empty, Parsetree_utils.DepNameSet.empty)
;;



let hyp_decl_dependencies ~current_species hyp =
  match hyp.Parsetree.ast_desc with
   | Parsetree.H_var (_, _) ->
       (* No decl-dependency from type expressions. *)
       Parsetree_utils.DepNameSet.empty
   | Parsetree.H_hyp (_, prop) -> prop_decl_dependencies ~current_species prop
   | Parsetree.H_not (_, expr) -> expr_decl_dependencies ~current_species expr
;;



let statement_decl_dependencies ~current_species stmt =
  let stmt_desc = stmt.Parsetree.ast_desc in
  (* First, get the decl-dependencies from the hypothses. *)
  let hyps_decl_deps =
    List.fold_left
      (fun accu_decl_deps hyp ->
        let hyp_decl_deps = hyp_decl_dependencies ~current_species hyp in
        Parsetree_utils.DepNameSet.union accu_decl_deps hyp_decl_deps)
      Parsetree_utils.DepNameSet.empty
      stmt_desc.Parsetree.s_hyps in
  (* An now, accumulate with those of the conclusion *)
  match stmt_desc.Parsetree.s_concl with
   | None ->hyps_decl_deps
   | Some prop ->
       let prop_decl_defs =
         prop_decl_dependencies ~current_species prop in
       Parsetree_utils.DepNameSet.union hyps_decl_deps prop_decl_defs

;;



let rec proof_decl_n_def_dependencies ~current_species proof =
  match proof.Parsetree.ast_desc with
   | Parsetree.Pf_assumed
   | Parsetree.Pf_coq _ ->
       (Parsetree_utils.DepNameSet.empty, Parsetree_utils.DepNameSet.empty)
   | Parsetree.Pf_auto facts ->
       (begin
       List.fold_left
         (fun (accu_decl_deps, accu_def_deps) fact ->
           let (fact_decl_deps, fact_def_deps) =
             fact_decl_n_def_dependencies ~current_species fact in
           (* Return both "decl" and "def" dependencies. *)
           ((Parsetree_utils.DepNameSet.union accu_decl_deps fact_decl_deps),
            (Parsetree_utils.DepNameSet.union accu_def_deps fact_def_deps)))
         (Parsetree_utils.DepNameSet.empty, Parsetree_utils.DepNameSet.empty)
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
          ((Parsetree_utils.DepNameSet.union
              sub_proof_decl_deps stmt_decl_deps),
           sub_proof_def_deps)
              | Parsetree.PN_qed (_, p) ->
          proof_decl_n_def_dependencies ~current_species p) in
           (* Return both "decl" and "def" dependencies. *)
           ((Parsetree_utils.DepNameSet.union
               accu_decl_deps proof_node_decl_deps),
            (Parsetree_utils.DepNameSet.union
               accu_def_deps proof_node_def_deps)))
         (Parsetree_utils.DepNameSet.empty, Parsetree_utils.DepNameSet.empty)
         proof_nodes
       end)
;;



(* ******************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                        *)
(*   Env.TypeInformation.species_field -> Parsetree_utils.DepNameSet.t  *)
(** {b Descr} : Compute the set of vnames the argument field depends of
       in the species [~current_species]. Does not take into account
       dependencies on the carrier. They must be handled appart.

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
let field_only_decl_dependencies ~current_species = function
  | Env.TypeInformation.SF_sig (_, _, _) -> Parsetree_utils.DepNameSet.empty
  | Env.TypeInformation.SF_let (_, _, _, _, body, _) ->
      expr_decl_dependencies ~current_species body
  | Env.TypeInformation.SF_let_rec l ->
      (begin
      (* Create the set of names to remove afterwards. *)
      let names_of_l =
        List.fold_left
          (fun accu_set (_, n, _, _, _, _) ->
            (* Give a dummy type variable as type... *)
            Parsetree_utils.DepNameSet.add (n, Types.type_variable ()) accu_set)
          Parsetree_utils.DepNameSet.empty
          l in
      (* Now, compute the dependencies on all the rec-bound-names. *)
      let deps_of_l =
        List.fold_left
          (fun accu_deps (_, _, _, _, body, _) ->
            let d = expr_decl_dependencies ~current_species body in
            Parsetree_utils.DepNameSet.union d accu_deps)
          Parsetree_utils.DepNameSet.empty
          l in
      (* And now, remove the rec-bound-names from the dependencies. *)
      Parsetree_utils.DepNameSet.diff deps_of_l names_of_l
      end)
  | Env.TypeInformation.SF_theorem (_, _, _, body, proof, _) ->
      (begin
      let body_decl_deps = prop_decl_dependencies ~current_species body in
      (* Now, recover the explicit "decl" dependencies of  *)
      (* the proof and ignore here the "def"-dependencies. *)
      let (proof_deps, _) =
        proof_decl_n_def_dependencies ~current_species proof in
      Parsetree_utils.DepNameSet.union body_decl_deps proof_deps
      end)
  | Env.TypeInformation.SF_property (_, _, _, body, _) ->
      prop_decl_dependencies ~current_species body
;;



(* ********************************************************************* *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->          *)
(*   Env.TypeInformation.species_field list                              *)
(** {b Descr} : Compute the set of all the names involved in the
              "clockwise arrow" relation (c.f. Virgile Prevosto's
              Pdh, section 3.5, page 30) for the name [field_name]
              in the fields [fields] that must include the inherited
              fields of the analysed species.

    {b Rem} : MUST BE used on a fields list containing all the
             **normalized inherited fields** of the species and the
             **non normalized fields of the current inheritance level**.
             This means that the inherited fields must already be
             fusionned if they needed, and the current level fields may
             not be fusionned this the inherited ones.
             This enables not to have to seach all along the inheritance
             tree because let rec will be inductively correct (i.e. all
             the recursive ident will be in ONE field) for inherited
             fields and keeps the current level fields present in order
             to check for let rec at this level.                         *)
(* ********************************************************************* *)
let clockwise_arrow field_name fields =
  List.fold_right
    (fun field accu ->
      match field with
      | Env.TypeInformation.SF_sig (_, vname, _)
      | Env.TypeInformation.SF_let (_, vname, _, _, _, _)
      | Env.TypeInformation.SF_theorem (_, vname, _, _, _, _)
      | Env.TypeInformation.SF_property (_, vname, _, _, _) ->
          if vname = field_name then Handy.list_cons_uniq_eq vname accu
          else accu
       | Env.TypeInformation.SF_let_rec l ->
           (* Check if the searched field name is among those in this     *)
           (* recursive let definition. If so, then the relation includes *)
           (* all the bound names of this recursive let definition.       *)
           if List.exists (fun (_, vname, _, _, _, _) -> vname = field_name) l
           then
             List.fold_right
               (fun (_, n, _, _, _, _) accu' -> Handy.list_cons_uniq_eq n accu')
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

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let where field_name fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_sig (_, vname, _)
       | Env.TypeInformation.SF_let (_, vname, _, _, _, _)
       | Env.TypeInformation.SF_theorem (_, vname, _, _, _, _)
       | Env.TypeInformation.SF_property (_, vname, _, _, _) ->
           if vname = field_name then field :: accu else accu
       | Env.TypeInformation.SF_let_rec l ->
           (* Check if the searched field name is among those in this     *)
           (* recursive let definition. If so, then the relation includes *)
           (* all the bound names of this recursive let definition.       *)
           if List.exists (fun (_, vname, _, _, _, _) -> vname = field_name) l
           then field :: accu
           else accu)
    fields
    []
;;



(* ******************************************************************* *)
(* Env.TypeInformation.species_field -> Parsetree_utils.DepNameSet.t   *)
(** {b Descr} : Just an helper returning the set of all names bound in
              a species fields.

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let names_set_of_field = function
  | Env.TypeInformation.SF_sig (_, vname, sch)
  | Env.TypeInformation.SF_let (_, vname, _, sch, _, _)
  | Env.TypeInformation.SF_theorem (_, vname, sch, _, _, _)
  | Env.TypeInformation.SF_property (_, vname, sch, _, _)  ->
      let ty = Types.specialize sch in
      Parsetree_utils.DepNameSet.singleton (vname, ty)
  | Env.TypeInformation.SF_let_rec l ->
      List.fold_left
        (fun accu_names (_, n, _, sch, _, _) ->
          let ty = Types.specialize sch in
          Parsetree_utils.DepNameSet.add (n, ty) accu_names)
        Parsetree_utils.DepNameSet.empty
        l
;;



(* ************************************************************************ *)
(* Env.TypeInformation.species_field list ->                                *)
(*   Parsetree_utils.DepNameSet.elt list                                    *)
(** {b Descr} : Just helper returning the list of all names bound in a list
              of fields. The resulting list preserves the order the names
              appear in the list of fields and in the list of names in
              case of recursive let-def field.
              Example: [let s; sig y; let rec z ... and t] will give the
              list [s; y; z; t].

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let ordered_names_list_of_fields fields =
  List.fold_right
    (fun field accu ->
      match field with
       | Env.TypeInformation.SF_sig (_, n, sch)
       | Env.TypeInformation.SF_let (_, n, _, sch, _, _)
       | Env.TypeInformation.SF_theorem (_, n, sch, _, _, _)
       | Env.TypeInformation.SF_property (_, n, sch, _, _) ->
           let ty = Types.specialize sch in (n, ty) :: accu
       | Env.TypeInformation.SF_let_rec l ->
           List.fold_right
             (fun (_, n, _, sch, _, _) accu' ->
               let ty = Types.specialize sch in (n, ty) :: accu')
             l accu)
    fields
    []
;;



(* ****************************************************************** *)
(* Parsetree.vname -> Env.TypeInformation.species_field list ->       *)
(*   Env.TypeInformation.species_field                                *)
(** {b Descr} : Looks for the most recently defined field that
              let-rec-binds [y_name] among [fields] and return it.
              This function relies on the fact that the field list is
              ordered with oldest inherited fields are in head of the
              list and the most recent are in tail.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let find_most_recent_rec_field_binding y_name fields =
  (* The search is a simple walk in the list, starting by the head and    *)
  (* going on with the tail. Because fields list is assumed to be ordered *)
  (* with the oldest inherited fields in head, one will have to reverse   *)
  (* the initial list of field before applying the present function.      *)
  let rec rec_search = function
    | [] -> raise Not_found
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_let (_, _, _, _, _, _)
         | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
         | Env.TypeInformation.SF_property (_, _, _, _, _) -> rec_search q
         | Env.TypeInformation.SF_let_rec l ->
             if List.exists (fun (_, n, _, _, _, _) -> n = y_name) l then h
             else rec_search q
        end) in
  (* Reverse the list so that most recent names are in head. *)
  rec_search (List.rev fields)
;;



(* *********************************************************************** *)
(* current_species: Parsetree.qualified_vname -> Parsetree.vname ->        *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     Parsetree_utils.DepNameSet.t                                        *)
(** {b Descr} : Implements the second case of the definition 16 in Virgile
              Prevosto's Phd, section 3.5, page 32.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let union_y_clock_x_etc ~current_species x_name fields =
  let all_ys = clockwise_arrow x_name fields in
  List.fold_left
    (fun accu_deps y_name ->
      (* First compute the great union. *)
      (* We look for the most recently defined field that binds [y_name]. *)
      (* This way, we really "compute" \Cal B_s (y) because \Cal B is     *)
      (* defined to return the body of the most recent fiedl in the       *)
      (* inheritance.                                                     *)
      let field_y = find_most_recent_rec_field_binding y_name fields in
      let u =
        Parsetree_utils.DepNameSet.union
          (field_only_decl_dependencies ~current_species field_y) accu_deps in
      (*  Then remove the recursive bound names. *)
      let rec_bound_names = names_set_of_field field_y in
      Parsetree_utils.DepNameSet.diff u rec_bound_names)
    Parsetree_utils.DepNameSet.empty
    all_ys
;;



(* ******************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                        *)
(*   (Parsetree.vname * Parsetree.expr) ->                              *)
(*     Env.TypeInformation.species_field list ->                        *)
(*       Parsetree_utils.DepNameSet.t                                   *)
(** {b Descr} : Compute the dependencies of a sig, let or let-rec bound
      name in a species. Namely this is the \lbag x \rbag_s in Virgile
      Prevosto's Pdh, section 3.5, page 32, definition 16.
      Does take into account dependencies on the carrier (they must be
      handled appart).
     

    {b Rem} : MUST be called only with a [name] sig, let or let-rec
              bound !
              Not exported outside this module.                         *)
(* ******************************************************************** *)
let in_species_decl_dependencies_for_one_function_name ~current_species
    (name, body) fields =
  let where_x = where name fields in
  (* Check if Where (x) does NOT contain Let_rec fields. *)
  if List.for_all
      (function
        | Env.TypeInformation.SF_sig (_, _, _)
        | Env.TypeInformation.SF_let (_, _, _, _, _, _) -> true
        | Env.TypeInformation.SF_let_rec _ -> false
        | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
        | Env.TypeInformation.SF_property (_, _, _, _, _) ->
            (* Because this function is intended to be called only on *)
            (* names bound by sig, let or let-rec fields, these cases *)
            (* should never arise !                                   *)
            assert false)
      where_x then expr_decl_dependencies ~current_species body
  else union_y_clock_x_etc ~current_species name fields
;;



(* ******************************************************************** *)
(** {b Descr} : Compute the dependencies of a property or theorem bound
    name in a species. Namely this is the \lbag x \rbag_s in Virgile
    Prevosto's Pdh, section 3.9.5, page 53, definition 30. Returns both
    the "decl" and "def" dependencies.
    Note that names we depend on are inevitably names from the current
    species inheritance tree's fields. That is by definition of the
    dependencies computation !

    {b Rem} : MUST be called only with a [name] property or theorem
              bound !
              Not exported outside this module.                         *)
(* ******************************************************************** *)
let in_species_decl_n_def_dependencies_for_one_theo_property_name
    ~current_species (t_prop, opt_body) =
  let t_prop_decl_deps = prop_decl_dependencies ~current_species t_prop in
  match opt_body with
   | None ->
       (* No body, then no "def"-dependencies. *)
       (t_prop_decl_deps, Parsetree_utils.DepNameSet.empty)
   | Some proof ->
       let (proof_decl_deps, proof_def_deps) =
         proof_decl_n_def_dependencies ~current_species proof in
       ((Parsetree_utils.DepNameSet.union t_prop_decl_deps proof_decl_deps),
        proof_def_deps)
;;



(* ************************************************************** *)
(** {b Descr} : Describes the kind of dependency between 2 nodes.
    Can be either "def" or "dep" dependency.

    {b Rem} : Exported outside this module.                       *)
(* ************************************************************** *)
type dependency_kind =
  | DK_decl
  | DK_def
;;



(* ******************************************************************* *)
(** {b Descr} : Strutrure of a node in a dependency graph representing
    the fact that some names' bodies contain call to non-let-rec-bound
    othernames (relation \lbag n \rbag in Virgile Prevosto's Phd,
    section 3.5, definition 16, page 32.

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
type name_node = {
  (** Name of the node, i.e. one name of a species fields. *)
  nn_name : Parsetree.vname ;
  (** The type of the field wearing this name. *)
  nn_type : Types.type_simple ;
  (** Means that the current names depends of the children nodes. I.e. the
      current name's body contains calls to the children names. *)
  mutable nn_children : (name_node * dependency_kind) list
}
;;



(* ******************************************************************* *)
(* name_node list ref -> (Parsetree.vname * Types.type_simple) ->      *)
(*   name_node                                                         *)
(** {b Descr} : Looks for a node labeled [name] in the list of nodes
    [tree_nodes]. If a node with this name is found, then we return
    it. Otherwise, a fresh node is created with [name] as name and no
    child, and this fresh node is returned.
    This is mostly a helper for the function
    [build_dependencies_graph_for_fields].

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let find_or_create tree_nodes (name, ty) =
  try List.find (fun node -> node.nn_name = name) !tree_nodes
  with Not_found ->
    let new_node = { nn_name = name ; nn_type = ty ; nn_children = [] } in
    tree_nodes := new_node :: !tree_nodes ;
    new_node
;;



(* ***************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                     *)
(*   Env.TypeInformation.species_field list -> name_node list        *)
(** {b Descr} : Build the dependencies graph of the names present in
    the fields list [fields] of the species [~current_species].
    In such a graph, if an arrow exists from n1 to n2, then it means
    that in the body of n1, call(s) to n2 is (are) performed.

    {b Rem} : Exported outside this module.                          *)
(* ***************************************************************** *)
let build_dependencies_graph_for_fields ~current_species fields =
  (* The root hoot used to remind all the created nodes in the graph. *)
  let tree_nodes = ref ([] : name_node list) in

  (* ********************************************************************* *)
  (** {b Descr} : Just make a local function dealing with one let binding.
      We then use it once for a Let and iter it for a Let_Rec.
      Apply the rules section 3.5, page 32, definition  16 to get the
      dependencies.                                                        *)
  (* ********************************************************************* *)
  let local_build_for_one_let n ty b =
    (* Find the dependencies node for the current name. *)
    let n_node = find_or_create tree_nodes (n, ty) in
    (* Find the names decl-dependencies for the current name. *)
    let n_decl_deps_names =
      in_species_decl_dependencies_for_one_function_name
        ~current_species (n, b) fields in
    (* Now, find the decl-dependencies nodes for these names. *)
    let n_deps_nodes =
      Parsetree_utils.DepNameSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, DK_decl) :: accu)
        n_decl_deps_names
        [] in
    (* Now add an edge from the current name's node to each of the *)
    (* decl-dependencies names' nodes.                             *)
    n_node.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_deps_nodes n_node.nn_children in

  (* ***************************************************************** *)
  (** {b Descr} : Just make a local function dealing with one property
              or theorem name.
              Apply rules from section 3.9.5, page 53, definition 30.  *)
  (* ***************************************************************** *)
  let local_build_for_one_theo_property n ty prop_t opt_b =
    (* Find the dependencies node for the current name. *)
    let n_node = find_or_create tree_nodes (n, ty) in
    (* Find the names decl and defs dependencies for the current name. *)
    let (n_decl_deps_names, n_def_deps_names) =
      in_species_decl_n_def_dependencies_for_one_theo_property_name
        ~current_species (prop_t, opt_b) in
    (* Now, find the decl-dependencies nodes for these names. *)
    let n_decl_deps_nodes =
      Parsetree_utils.DepNameSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, DK_decl) :: accu)
        n_decl_deps_names
        [] in
    (* Now add an edge from the current name's node to each of the *)
    (* decl-dependencies names' nodes.                             *)
    n_node.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_decl_deps_nodes n_node.nn_children;
    (* Now, find the def-dependencies nodes for these names. *)
    let n_def_deps_nodes =
      Parsetree_utils.DepNameSet.fold
        (fun n accu ->
          let node = find_or_create tree_nodes n in
          (node, DK_def) :: accu)
        n_def_deps_names
        [] in
    (* Now add an edge from the current name's node to each of the *)
    (* def-dependencies names' nodes.                              *)
    n_node.nn_children <-
      Handy.list_concat_uniq_custom_eq
        (fun (n1, dk1) (n2, dk2) -> n1 == n2 && dk1 = dk2)
        n_def_deps_nodes n_node.nn_children in

  (* *************** *)
  (* Now do the job. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (_, n, sch) ->
          if not (List.exists (fun node -> node.nn_name = n) !tree_nodes) then
            (begin
            let ty = Types.specialize sch in
            tree_nodes :=
              { nn_name = n ; nn_type = ty ; nn_children = [] } :: !tree_nodes
            end)
      | Env.TypeInformation.SF_let (_, n, _, sch, b, _) ->
          let ty = Types.specialize sch in
          local_build_for_one_let n ty b
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, n, _, sch, b, _) ->
              let ty = Types.specialize sch in
              local_build_for_one_let n ty b) l
      | Env.TypeInformation.SF_theorem (_, n, sch, prop, body, _) ->
          let ty = Types.specialize sch in
          local_build_for_one_theo_property n ty prop (Some body)
      | Env.TypeInformation.SF_property (_, n, sch, prop, _) ->
          let ty = Types.specialize sch in
          local_build_for_one_theo_property n ty prop None)
    fields ;
  (* Return the list of nodes of the graph. *)
  !tree_nodes
;;



(* ************************************************************************ *)
(* dirname: string -> current_species: Parsetree.qualified_vname) ->        *)
(*   name_node list -> unit                                                 *)
(** {b Descr} : Prints the dependencies graph of a species in dotty format.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let dependencies_graph_to_dotty ~dirname ~current_species tree_nodes =
  (* For each species, a file named with "deps_", the species name *)
  (* and the suffix ".dot" will be generated in the directory.     *)
  let (current_species_module, current_species_vname) = current_species in
  let out_filename =
    Filename.concat
      dirname
      ("deps_" ^ current_species_module ^ "_" ^
       (Parsetree_utils.name_of_vname current_species_vname) ^ ".dot") in
  let out_hd = open_out_bin out_filename in
  (* First, outputs the header of the dotty file. *)
  Printf.fprintf out_hd "digraph G {\n";
  (* Outputs all the nodes og the graph. *)
  List.iter
    (fun { nn_name = n } ->
      Printf.fprintf out_hd "\"%s\" [shape=box,fontsize=10];\n"
        (Parsetree_utils.name_of_vname n))
    tree_nodes ;
  (* Outputs all the edges between the nodes. *)
  List.iter
    (fun { nn_name = n; nn_children = children } ->
      List.iter
        (fun ({ nn_name = child_name }, decl_kind) ->
          (* Just make a different style depending on the kind of dependency. *)
          let (style, color) =
            (match decl_kind with
             | DK_decl -> ("", "red")
             | DK_def -> ("style=dotted,", "blue")) in
          Printf.fprintf out_hd
            "\"%s\" -> \"%s\" [%scolor=%s,fontsize=10];"
            (Parsetree_utils.name_of_vname n)
            (Parsetree_utils.name_of_vname child_name) style color)
        children)
    tree_nodes ;
  (* Finally, outputs the trailer of the dotty file. *)
  Printf.fprintf out_hd " \n}\n" ;
  close_out out_hd
;;



(* ********************************************************* *)
(** {b Descr} : Module stuff to create maps of [name_node]s.

    {b Rem} : Not exported outside this module.              *)
(* ********************************************************* *)
module NameNodeMod = struct
  type t = name_node
  let compare nn1 nn2 = compare nn1.nn_name nn2.nn_name
end
;;
module NameNodeMap = Map.Make (NameNodeMod);;



(* ******************************************************************* *)
(* type name_node -> int                                               *)
(** {b Descr} : Compute the "out degree" of a node, i.e. the number of
              DIFFERENT children nodes it has. By different, we mean
              that 2 edges of different kinds between 2 same nodes are
              considered as only 1 edge.

    {b Rem} : Not exported outside this module.                        *)
(* ******************************************************************* *)
let node_out_degree node =
  let count = ref 0 in
  let seen = ref ([] : Parsetree.vname list) in
  List.iter
    (fun (n, _) ->
      if not (List.mem n.nn_name !seen) then
        (begin
        seen := n.nn_name :: !seen ;
        incr count
        end))
    node.nn_children ;
  !count
;;



(* ************************************************************************** *)
(* current_species: Parsetree.qualified_vname ->                              *)
(*   Env.TypeInformation.species_field list -> Parsetree.vname list           *)
(** {b Descr} : Determines the order of apparition of the fields inside a
              species to prevent fields depending on other fields from
              appearing before. In others words, this prevents from
              having [lemma2; lemma1] if the proof of [lemma2] requires
              [lemma1].
              We then must make in head of the species, the deepest fields
              in the dependency graph, before adding those of the immediately
              upper level, and so on.
              Hence, this is a kind of reverse-topological sort. In effect
              in our graph an edge i -> j does not mean that i must be
              "processed" before j, but exactly the opposite !

    {b Rem} : Because of well-formation properties, this process should never
              find a cyclic graph. If so, then may be the well-formness
              process is bugged somewhere-else.
              Exported outside this module.                                   *)
(* ************************************************************************** *)
let compute_fields_reordering ~current_species fields =
  (* First, compute the depency graph of the species fields. *)
  let dep_graph_nodes =
    build_dependencies_graph_for_fields ~current_species fields in
  (* Map recording for each node its "outputs degree", *)
  (* that's to say, the number of children it has.     *)
  let out_degree = ref NameNodeMap.empty in
  (* First, initialize the out degree of each node, taking care not *)
  (* to double-count 2 edges of different dependency kind between   *)
  (* two same nodes.                                                *)
  List.iter
    (fun name_node ->
      let nb_distict_children = node_out_degree name_node in
      out_degree :=
        NameNodeMap.add name_node (ref nb_distict_children) !out_degree)
    dep_graph_nodes ;
  (* The working list... *)
  let c_queue = Queue.create () in
  (* Initialization with nodes having a out degree equal to 0.*)
  NameNodeMap.iter
    (fun node degree ->
      if !degree = 0 then
        begin
        Queue.push node c_queue ;
        out_degree := NameNodeMap.remove node !out_degree
        end)
    !out_degree ;
  (* The list with the newly ordered fields names. We build it reversed *)
  (* for sake of efficiency. We will need to reverse it at the end.     *)
  let revd_order_list = ref ([] : Parsetree.vname list) in
  (* Now, iterate until the working list gets empty. *)
  (begin
  try
    while true do
      let j = Queue.take c_queue in
      (* [j] can now be output. *)
      revd_order_list := j.nn_name :: !revd_order_list ;
      (* Search all parents, i, of  j to decrement their out degree. *)
      NameNodeMap.iter
        (fun i i_out_degree ->
          (* Tests if [j] belongs to [i]'s children, *)
          (* ignoring the dependency kind.           *)
          let is_i_parent_of_j =
            List.exists (fun (child, _) -> child == j) i.nn_children in
          if is_i_parent_of_j then
            (begin
            (* The node [j] appears in [i]'s childrens, hence [i] *)
            (* is right a parent of [j].                          *)
            decr i_out_degree ;
            if !i_out_degree = 0 then
              (begin
              (* This parent is now of degree 0, it can now be processed *)
              (* because all its children have already been.             *)
              Queue.push i c_queue ;
              out_degree := NameNodeMap.remove i !out_degree
              end)
            end))
        !out_degree
    done
  with Queue.Empty ->
    (* If there remain nodes in the [out_degree] table, this means  *)
    (* that there exists nodes one could not classify because their *)
    (* degree never reached 0. This corresponds to cycles in the    *)
    (* graph. Because on well-formness properties, this should      *)
    (* never appear except if the well-formness algorithm is        *)
    (* buggy somewhere else.                                        *)
    if not (NameNodeMap.is_empty !out_degree) then assert false
  end);
  (* And finaly, reverse the order list to get it in the right ... order. *)
  List.rev !revd_order_list
;;



(* ************************************************************************ *)
(* name_node -> name_node -> bool                                           *)
(** {b Descr} : Checks for a non trivial path from [start_node] to
              [start_node]. "Non-trivial" means that the path must at least
              be of length 1.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let is_reachable start_node end_node =
  (* List of already seen nodes. Will be extended during the search. *)
  let seen = ref ([] : name_node list) in
  let rec rec_search current_node =
    (* If the current node was already seen, this means that ... we already  *)
    (* saw it, then we already checked if the [end_node] was in its children *)
    (* and the anwser was NOT. Hence there is no reason to start again the   *)
    (* search, we will get the same answer forever (and loop forever of      *)
    (* course by the way).                                                   *)
    if List.memq current_node !seen then false
    else
      (begin
      (* We check if the current node's children contain en [end_node]. This *)
      (* way, for each node, we are sure that the possibly found path is not *)
      (* the trivial path (because we explicitly look inside the children    *)
      (* and if a node is acceptable in the children, then the path length   *)
      (* is mandatorily non-null).                                           *)
      if List.exists
          (fun (n, _) -> n == end_node) current_node.nn_children then true
      else
        (begin
        seen := current_node :: !seen ;
        (* The [end_node] was not found in the children, *)
        (* then search in the children.                  *)
        List.exists (fun (n, _) -> rec_search n) current_node.nn_children
        end)
      end) in
  (* Start the search. *)
  rec_search start_node
;;



(* *********************************************************************** *)
(* name_node list -> -> Parsetree.vname -> Parsetree.vname ->              *)
(*   Env.TypeInformation.species_field list -> bool                        *)
(** {b Descr} : Implements the relation "left-oriented triangle" of
              Virgile Prevosto's Phd, section 3.5, page 32, definition 17.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let left_triangle dep_graph_nodes x1 x2 fields =
  (* Guess the fields where x1 is recursively bound. *)
  let x1_arrow = clockwise_arrow x1 fields in
  (* Guess the fields where x2 is recursively bound. *)
  let x2_arrow = clockwise_arrow x2 fields in
  (* Now we will apply the "well-formness" predicate on the cartesian  *)
  (* product of the names bound by "clockwise-arrow" of [x1] and those *)
  (* bound by "clockwise-arrow" of [x2]. Intuitively, we will apply    *)
  (* this predicate on all possible combinaisons of [y_1] and [y_n].   *)
  List.exists
    (fun y1 ->
      List.exists
        (fun yn ->
          (* Search a path in the graph from yn to y2. The lookup for *)
          (* nodes y1 and yn should never fail because the graph was  *)
          (* created before.                                          *)
          let y1_node =
            List.find (fun node -> node.nn_name = y1) dep_graph_nodes in
          let yn_node =
            List.find (fun node -> node.nn_name = yn) dep_graph_nodes in
          (* Because our graph edges link from yn to y1, we must invert *)
          (* the start/end nodes.                                       *)
          is_reachable yn_node y1_node)
        x2_arrow)
    x1_arrow
;;



(* ************************************************************************ *)
(* current_species: Parsetree.qualified_vname ->                            *)
(*   Env.TypeInformation.species_field list -> unit                         *)
(** {b Descr} : Checks if a species is well-formed, applying the definition
              17 in Virgile Prevosto's Phd, section 3.5, page 32

    {b Rem} : Exported outside this module.                                 *)
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
      if ill_f then
       let (modname, species_vname) = current_species in
       raise (Ill_formed_species (Qualified (modname, species_vname))))
    names
;;



(* ************************************************************************* *)
(* Env.TypeInformation.species_field ->                                      *)
(*   Env.TypeInformation.species_field list                                  *)
(** {b Descr} : Implements the erasing procedure of one field described
              in Virgile Prevosto's Phd, Section 3.9.5, page 53, definition
              33.

    {b Rem} : Not exported outside this module.
              Because the erasing of one Let_rec leads to several Sig fields
              this function takes 1 fields and may return several.
              In the same spirit, because we don't have any "slilent"  Sig
              for "rep" (of course, "rep" is always a Sig when present), if
              we find "rep" defined, then the only way to abstract it is to
              remove it. Hence this function may also return an empty list
              of fields.                                                     *)
(* ************************************************************************* *)
let erase_field field =
  match field with
  | Env.TypeInformation.SF_sig (from, vname, _) ->
    (* Also includes "rep". *)
    let m_name_as_str = Parsetree_utils.name_of_vname vname in
    if m_name_as_str = "rep" then
      (begin
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname vname Sourcify.pp_qualified_species from ;
      []  (* No explicit "rep" means ... no "rep". *)
      end)
    else [field]
  | Env.TypeInformation.SF_let (from, vname, _, sch, _, _) ->
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname vname Sourcify.pp_qualified_species from ;
      (* Turn the "let" into a "sig". *)
      [Env.TypeInformation.SF_sig (from, vname, sch)]
  | Env.TypeInformation.SF_let_rec l ->
      (* Just turn the whole list into "sig"s. *)
      List.map
        (fun (from, n, _, sch, _, _) ->
          if Configuration.get_verbose () then
            Format.eprintf "Erasing field '%a' coming from '%a'.@."
              Sourcify.pp_vname n Sourcify.pp_qualified_species from ;
          Env.TypeInformation.SF_sig (from, n, sch))
        l
  | Env.TypeInformation.SF_theorem (from, n, sch, prop, _, deps_rep) ->
      if Configuration.get_verbose () then
        Format.eprintf "Erasing field '%a' coming from '%a'.@."
          Sourcify.pp_vname n Sourcify.pp_qualified_species from ;
      (* Turn the "theorem" into a "property".               *)
      (* Hence, destroys any def-dependency on the carrier ! *)
      let deps_rep' = { deps_rep with Env.TypeInformation.dor_def = false } in
      [Env.TypeInformation.SF_property (from, n, sch, prop, deps_rep')]
  | _ -> [field]                       (* Everything else is unchanged. *)
;;



(* ******************************************************************* *)
(* current_species: Parsetree.qualified_vname ->                       *)
(*   Parsetree_utils.DepNameSet.elt list ->                            *)
(*     Env.TypeInformation.species_field list ->                       *)
(*       Env.TypeInformation.species_field list                        *)
(** {b Descr} : Implements the erasing procedure in a list of fields
    [fields] as described in Virgile Prevosto's Phd, Section 3.9.5,
    page 53, definition 33.
    Erases in the list of fields definitions according to the context
    represented by the list of names [context].

    {b Rem} : Exported outside this module.                            *)
(* ******************************************************************* *)
let erase_fields_in_context ~current_species context fields =
  (* Now, the recursive function dealing with each field...*)
  let rec rec_erase rec_context = function
    | [] -> []
    | m_field :: l_rem_fields ->
      (begin
      (* We check if the [m_field] is already astracted. *)
      match m_field with
      | Env.TypeInformation.SF_sig (_, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _, _) ->
          (* [m_field] is already astracted. If so, then nothing to do on *)
          (* it and just go on with the remaining fields [l_rem_fields].  *)
          m_field :: (rec_erase rec_context l_rem_fields)
      | _ ->
          (* All other cases. First compute the intersection between *)
          (* [m_field]'s def-dependencies and the context. This is   *)
          (* the \lbag\lbag m \rbag\rbag \inter \Cal N formula  of   *)
          (* definition 33, page 53.                                 *)
          let def_deps =
            (match m_field with
             | Env.TypeInformation.SF_let (_, _, _, _, _, _)
             | Env.TypeInformation.SF_let_rec _ ->
               (* No "def"-dependencies for functions (C.f. definition   *)
               (* 30 in Virgile Prevosto's Phd, section 3.9.5, page 53). *)
               Parsetree_utils.DepNameSet.empty
             | Env.TypeInformation.SF_theorem (_, _, _, prop, proof, _) ->
               let (_, n_def_deps_names) =
                 in_species_decl_n_def_dependencies_for_one_theo_property_name
                   ~current_species (prop, (Some proof)) in
               (* Just return the "def"-dependencies. *)
               n_def_deps_names
             | Env.TypeInformation.SF_property (_, _, _, _, _)
             | Env.TypeInformation.SF_sig (_, _, _) ->
                 (* Can not arise because these cases were matched above. *)
                 assert false) in
          (* Compute the intersection with the context... *)
          if Parsetree_utils.DepNameSet.is_empty
               (Parsetree_utils.DepNameSet.inter def_deps rec_context) then
            (* Intersection is empty. *)
            m_field :: (rec_erase rec_context l_rem_fields)
          else
            (* Intersection non non-empty. Erase the current field. *)
            let erased_m_field = erase_field m_field in
            (* Extent the erasing context with names of the current field. *)
            let new_context =
              Parsetree_utils.DepNameSet.union
                rec_context (names_set_of_field m_field) in
            (* And then process the remaining fields. *)
            erased_m_field @ (rec_erase new_context l_rem_fields)
      end) in
  (* ***************** *)
  (* Now do the job... *)
  (* Build once for all the set on names contained in the context list. *)
  let context_as_set =
    List.fold_left
      (fun accu n -> Parsetree_utils.DepNameSet.add n accu)
      Parsetree_utils.DepNameSet.empty context in
  rec_erase context_as_set fields
;;
