(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Fran�ois Pessaux                                                *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************* *)
(** {b Desc} : This module performs abstraction computation over the species
    parameters of a species. It computes the dependencies on species
    parameters and creates a structure that sumarises all the abstractions
    the methods of a species require. To have *all* the dependencies, it
    asks to build the minimal coq/dedukti typing environment and the visible
    universe. It will use the fact that dependencies on methods on "Self"
    for each method is already computed.
    The output data-structure will be sent to the code generation pass.      *)
(* ************************************************************************* *)



(* ********************************************************************* *)
(** {b Descr} : Describes if the argument passed to the function
    [compute_lambda_liftings_for_field] is the body of a "let", "logical
    let" or of a "property/theorem". This allows the function to process
    at once the case of the liftings computation for expressions,
    propositions and proofs.

    {b Exported} : Yes.                                                  *)
(* ********************************************************************* *)
type field_body_kind =
  | FBK_expr of Parsetree.expr
  | FBK_logical_expr of Parsetree.logical_expr
  | FBK_proof of Parsetree.proof option
;;


type field_type_kind =
  | FTK_computational of Types.type_simple
  | FTK_logical of Parsetree.logical_expr
;;



(* ************************************************************************* *)
(** {b Descr} Serves in [Misc_common.follow_instanciations_for_xxx] and some
    other functions in [Misc_common] to pass the code generation environment
    of the current language backend.
    This information is present in all the code generation environments but
    these environments have different types.
    Hence, when we will need to access an environment, with this 2
    constructors, we will know its type and which primitives to use.

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
type environment_kind =
  | EK_ml of Env.MlGenEnv.t
  | EK_coq of Env.CoqGenEnv.t
  | EK_dk of Env.DkGenEnv.t
;;



type abstractions_comput_context = {
  (** The name of the currently analysed compilation unit. *)
  acc_current_unit : Types.fname ;
  (** The name of the current species. *)
  acc_current_species : Parsetree.qualified_species ;
  (** The nodes of the current species's dependency graph. *)
  acc_dependency_graph_nodes : DepGraphData.name_node list ;
  (** The list of the current species species parameters if we are in the
      scope of a species and if it has some parameters. We record for
      each parameter it's kind (i.e. "in" or "is"). For "is" parameters, the
      name is in [Env.TypeInformation.SPAR_is ((_, n), _, _)].
      For "in" parameters, the name is [Env.TypeInformation.SPAR_is (n, _)]. *)
  acc_species_parameters_names : Env.TypeInformation.species_param list ;
} ;;



(* *********************************************************************** *)
(** {b Descr}�Helper that creates the structure denoting an empty species
    parameters dependencies set. The "empty" dependencies cannot simple be
    [] because all our "union" functions on dependencies rely on a list of
    sets with 1 set for each species parameter name. So we create our
    initial accumulator as the list mapping each species parameter name
    onto the empty dependencies set.

    {b Exported}: Yes.                                                     *)
(* *********************************************************************** *)
let make_empty_param_deps species_parameters_names =
  List.fold_right
    (fun species_param accu ->
      (species_param, Parsetree_utils.ParamDepSet.empty) :: accu)
    species_parameters_names
    []
;;



(* ************************************************************************* *)
(* Types.fname -> Parsetree.vname ->                                         *)
(*   ((Types.fname * Types.collection_name) *                                *)
(*    Types.substitution_by_replacement_collection_kind) list ->             *)
(*     Parsetree.vname                                                       *)
(** {b Descr} : Applies a list of substitutions on a [Parsetree.vname].
    This [Parsetree.vname] is expected to be the name of a formal collection
    parameter of a species from which we inherit.
    The aim is to replace each formal collection parameter of the inherited
    [used_species_parameter_tys] information, by applying the substitution
    induced by the "inherits" clause making us inherit.

    {b Args}:
      -[pmodname]: The compilation unit where the species parameter name
       appears. It is in fact the name of the compilation unit hosting the
       species having this *FORMAL* species parameter.

      - [pvname]: The formal species parameter name.

    {b Rem} : Raises [Not_found] if the substitution replace a parameter by
    "Self".

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let apply_substitutions_list_on_formal_param_vname pmodname pvname substs =
  (* First, convert the species parameter's name into a [string] to make
     comparison easier. *)
  let pvname_as_string = Parsetree_utils.name_of_vname pvname in
  let substed_pvname_as_string =
    List.fold_left
      (fun accu_vname -> function
        | Env.SK_ident_by_expression (_, _, _) ->
            (* We only deal with collection parameters. *)
            accu_vname
        | Env.SK_collection_by_collection ((c1_mod_name, c1_spe_name), c2) -> (
            match c2 with
             | Types.SBRCK_coll (_, x) ->
                 (* By construction, the compilation unit name of c2 should
                    always be the current compilation unit. *)
                 if c1_mod_name = pmodname && c1_spe_name = accu_vname then x
                 else accu_vname
             | Types.SBRCK_self ->
                 if c1_mod_name = pmodname && c1_spe_name = accu_vname then
                   (* Substituting a formal collection parameter make the
                      dependency on the parameter disepear. In effect, in this
                      case, instead of depending on the collection parameter
                      carrier, we depend on OUR carrier. And this is caught by
                      the minimal Coq/Dedukti typing environment. So this
                      parameter purely disapears. *)
                   raise Not_found
                 else accu_vname
           ))
      pvname_as_string
      substs in
  (* Since during the substitutions we used [string]s we must finally convert
     the result back to a [vname]. Because collection names are always
     capitalized, we convert into a [Vuident]. *)
  Parsetree.Vuident substed_pvname_as_string
;;



(* ************************************************************************* *)
(* Env.from_history -> Parsetree.qualified_species                           *)
(** {b Descr} : Find the species that is the most recent parent according to
    the passed [from_history].
    Attention, this function must be called on a method about which we are
    sure is it inherited. If it is not the case, then assersion will fail.
    This function is used by [remap_dependencies_on_params_for_field] in
    order to recover from which species an inherited method comes to later
    look at its dependencies on parameters and adjust the computed
    abstractions for the parameters in the inheriting species according to
    those computed in the inherited species.

    {b Args} :
      - [from_history] : The history structure in which to search.

    {b Ret} :
      - [Parsetree.qualified_species] : The full name (i.e. compilation
        unit name and species name) of the species being the most recent
        parent.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let find_most_recent_parent from_history =
  match from_history.Env.fh_inherited_along with
   | [] -> assert false
   | [_] ->
       (* We must take the initial apparition since the method has been
          inherited only by one inheritance step. *)
       from_history.Env.fh_initial_apparition
   | _ :: ((parent_mod, parent_spe), _, _) :: _ ->
       (* We skip the first element of the list since it represents the species
          where the method arrived the most recently and we take the second
          element which is hence the most recent *parent*. *)
       (parent_mod, parent_spe)
;;



let rec get_species_types_in_type_annots_of_logical_expr lexpr =
  match lexpr.Parsetree.ast_desc with
   | Parsetree.Pr_forall (_, type_expr, logical_expr)
   | Parsetree.Pr_exists (_, type_expr, logical_expr) ->
       (* No need to recurse inside the [type_expr] structure. We just use
          the type annotation inserted during typechecking phase in the
          [ast_node]. *)
       let tys1 =
         (match type_expr.Parsetree.ast_type with
          | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_none -> assert false
          | Parsetree.ANTI_scheme sch ->
              let t = Types.specialize sch in
              Types.get_species_types_in_type t
          | Parsetree.ANTI_type t -> Types.get_species_types_in_type t) in
       let tys2 =
         get_species_types_in_type_annots_of_logical_expr logical_expr in
       Types.SpeciesCarrierTypeSet.union tys1 tys2
   | Parsetree.Pr_imply (logical_expr1, logical_expr2)
   | Parsetree.Pr_or (logical_expr1, logical_expr2)
   | Parsetree.Pr_and (logical_expr1, logical_expr2)
   | Parsetree.Pr_equiv (logical_expr1, logical_expr2) ->
       let tys1 =
         get_species_types_in_type_annots_of_logical_expr logical_expr1 in
       let tys2 =
         get_species_types_in_type_annots_of_logical_expr logical_expr2 in
       Types.SpeciesCarrierTypeSet.union tys1 tys2
   | Parsetree.Pr_expr expr ->
       get_species_types_in_type_annots_of_expr expr
   | Parsetree.Pr_not logical_expr
   | Parsetree.Pr_paren logical_expr ->
       get_species_types_in_type_annots_of_logical_expr logical_expr



and get_species_types_in_type_annots_of_expr expr =
  match expr.Parsetree.ast_desc with
   | Parsetree.E_self
   | Parsetree.E_const _
   | Parsetree.E_var _
   | Parsetree.E_external _ -> Types.SpeciesCarrierTypeSet.empty
   | Parsetree.E_app (e, es) ->
       List.fold_left
         (fun accu e ->
           Types.SpeciesCarrierTypeSet.union accu
             (get_species_types_in_type_annots_of_expr e))
         (get_species_types_in_type_annots_of_expr e)
         es
   | Parsetree.E_constr (_, es)
   | Parsetree.E_tuple es ->
       List.fold_left
         (fun accu e ->
           Types.SpeciesCarrierTypeSet.union accu
             (get_species_types_in_type_annots_of_expr e))
         Types.SpeciesCarrierTypeSet.empty
         es
   | Parsetree.E_sequence es ->
       List.fold_left
         (fun accu e ->
           Types.SpeciesCarrierTypeSet.union accu
             (get_species_types_in_type_annots_of_expr e))
         Types.SpeciesCarrierTypeSet.empty
         es
   | Parsetree.E_match (e, pats_exprs) ->
       List.fold_left
         (fun accu (_, e) ->
           (* No type annotation in patterns, so no need to inspect their
              structure. *)
           let tys = get_species_types_in_type_annots_of_expr e in
           Types.SpeciesCarrierTypeSet.union tys accu)
         (get_species_types_in_type_annots_of_expr e)
         pats_exprs
   | Parsetree.E_if (e1, e2, e3) ->
       let tys1 = get_species_types_in_type_annots_of_expr e1 in
       let tys2 = get_species_types_in_type_annots_of_expr e2 in
       let tys3 = get_species_types_in_type_annots_of_expr e3 in
       Types.SpeciesCarrierTypeSet.union tys1
         (Types.SpeciesCarrierTypeSet.union tys2 tys3)
   | Parsetree.E_let (let_def, e) ->
       let tys = get_species_types_in_type_annots_of_let_def let_def in
       Types.SpeciesCarrierTypeSet.union
         tys (get_species_types_in_type_annots_of_expr e)
   | Parsetree.E_record fields ->
       List.fold_left
         (fun accu (_, e) ->
           Types.SpeciesCarrierTypeSet.union accu
             (get_species_types_in_type_annots_of_expr e))
         Types.SpeciesCarrierTypeSet.empty
         fields
   | Parsetree.E_record_with (e, fields) ->
       List.fold_left
         (fun accu (_, e) ->
           Types.SpeciesCarrierTypeSet.union accu
             (get_species_types_in_type_annots_of_expr e))
         (get_species_types_in_type_annots_of_expr e)
         fields
   | Parsetree.E_fun (_, e)
   | Parsetree.E_paren e
   | Parsetree.E_record_access (e, _) ->
       get_species_types_in_type_annots_of_expr e



and get_species_types_in_type_annots_of_let_binding let_binding =
  let let_binding_desc = let_binding.Parsetree.ast_desc in
  (* First, extract the types used in the parameters bound by the binding. *)
  let tys_from_params =
    List.fold_left
      (fun accu (_, ty_expr_opt) ->
        match ty_expr_opt with
         | None -> accu
         | Some ty_expr ->
             let tys =
               (match ty_expr.Parsetree.ast_type with
                | Parsetree.ANTI_irrelevant
                | Parsetree.ANTI_none
                | Parsetree.ANTI_scheme _ -> assert false
                | Parsetree.ANTI_type t -> Types.get_species_types_in_type t) in
             Types.SpeciesCarrierTypeSet.union tys accu)
      Types.SpeciesCarrierTypeSet.empty
      let_binding_desc.Parsetree.b_params in
  (* Now, get the types used in the return type of the binding. *)
  let tys_with_ret =
    (match let_binding_desc.Parsetree.b_type with
     | None -> tys_from_params
     | Some type_expr ->
         match type_expr.Parsetree.ast_type with
          | Parsetree.ANTI_irrelevant
          | Parsetree.ANTI_none
          | Parsetree.ANTI_scheme _ -> assert false
          | Parsetree.ANTI_type t ->
              Types.SpeciesCarrierTypeSet.union
                tys_from_params (Types.get_species_types_in_type t)) in
  (* And now, get the types used in the body of the binding. *)
  match let_binding_desc.Parsetree.b_body with
   | Parsetree.BB_logical lexpr ->
       Types.SpeciesCarrierTypeSet.union
         tys_with_ret (get_species_types_in_type_annots_of_logical_expr lexpr)
   | Parsetree.BB_computational e ->
       Types.SpeciesCarrierTypeSet.union
         tys_with_ret (get_species_types_in_type_annots_of_expr e)



and get_species_types_in_type_annots_of_let_def let_def =
  List.fold_left
    (fun accu binding ->
      let tys = get_species_types_in_type_annots_of_let_binding binding in
      Types.SpeciesCarrierTypeSet.union accu tys)
    Types.SpeciesCarrierTypeSet.empty
    let_def.Parsetree.ast_desc.Parsetree.ld_bindings
;;



let get_if_field_logical_statement name fields =
  let rec rec_get = function
    | [] -> None
    | h :: q ->
        match h with
         | Env.TypeInformation.SF_sig (_, _, _)
         | Env.TypeInformation.SF_let (_, _, _, _, _, _, _, _)
         | Env.TypeInformation.SF_let_rec _ ->
             rec_get q
         | Env.TypeInformation.SF_property (_, n, _, lexpr, _)
         | Env.TypeInformation.SF_theorem (_, n, _, lexpr, _, _) ->
             if n = name then Some lexpr else rec_get q in
  rec_get fields
;;



(* ************************************************************************* *)
(** {b Descr} : Pre-process a field before its compilation to OCaml. We
    compute here the information related to the extra parameters a method
    will have by lambda-lifting due to the species parameters and the
    dependencies of the method.
    We extract the methods we decl-depend on, the methods we def-depend on,
    the methods of the species parameters we depend on via our "body" for
    lets and "type" for theorems and properties.
    ATTENTION, the set of  the methods of the species parameters we depend
    on is not complete: it must be completed to achieve the definition 72
    page 153 in Virgile Prevosto's Phd. In fact, the present function only
    implements rule [BODY] !

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let compute_lambda_liftings_for_field ~current_unit ~current_species
    species_parameters_names dependency_graph_nodes name body my_type
    opt_term_pr all_my_fields =
  (* Get all the methods we directly decl-depend on. They will lead each to an
     extra parameter of the final OCaml function (lambda-lifing). Get the
     methods we directly def-depend. They will be ignored for OCaml but used
     for Coq and Dedukti. *)
  let (decl_children, def_children) =
    (try
      let my_node =
        List.find
          (fun { DepGraphData.nn_name = n } -> n = name)
          dependency_graph_nodes in
      (* Only keep "decl-dependencies" . *)
      List.partition
        (function
          | (_, DepGraphData.DK_decl _) -> true
          | (_, DepGraphData.DK_def _) -> false)
        my_node.DepGraphData.nn_children
    with Not_found -> ([], [])  (* No children at all. *)) in
  (* Get the list of the methods from the species parameters the current
     method depends on. Do not [fold_left] to keep the extra parameters in the
     same order than the species parameters order. I.e. for a species
     [Foo (A ..., B) ...] we want to have the extra parameters due to
     lambda-lifting in the OCaml function ordered such as those coming from
     [A] are first, then come those from [B]. *)
  let dependencies_from_params_in_bodies =
    List.fold_right
      (fun species_param accu ->
        (* Recover the species parameter's name. *)
        let (species_param_name, species_param_meths) =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> (n, [])
           | Env.TypeInformation.SPAR_is ((_, n), _, meths, _, _) ->
               ((Parsetree.Vuident n), meths) in
        (* First, search dependencies in the body of the method. *)
        let meths_from_param1 =
          (match body with
           | FBK_expr e ->
               Param_dep_analysis.param_deps_expr
                 ~current_species (species_param_name, species_param_meths) e
           | FBK_logical_expr p ->
               Param_dep_analysis.param_deps_logical_expr
                 ~current_species
                 (species_param_name, species_param_meths) p
           | FBK_proof None -> Parsetree_utils.ParamDepSet.empty
           | FBK_proof (Some proof) ->
               (* Implements rule BODY of the definition 72 page 153 of
                  Virgile Prevosto's Phd. for theorems and properties. *)
               Param_dep_analysis.param_deps_proof
                 ~current_species (species_param_name, species_param_meths)
                 proof) in
        (* Now, if this method has a termination proof, we must find the
           dependencies in the expression (a kind of body) representing its
           order. *)
        let meths_from_param2 =
          (match opt_term_pr with
           | None -> Parsetree_utils.ParamDepSet.empty
           | Some term_pr ->
               match term_pr.Parsetree.ast_desc with
                | Parsetree.TP_order (expr, _, pr)
                | Parsetree.TP_measure (expr, _, pr) -> (
                    let deps1 =
                      Param_dep_analysis.param_deps_expr
                        ~current_species
                        (species_param_name, species_param_meths) expr in
                    let deps2 =
                      Param_dep_analysis.param_deps_proof
                        ~current_species
                        (species_param_name, species_param_meths) pr in
                         Parsetree_utils.ParamDepSet.union deps1 deps2
                   )
                | Parsetree.TP_structural _ ->
                    (* Abstracted stuff can not be used for structural
                       termination. Moreover, structural termination can only
                       rely on a function parameter, not an expression. Hence
                       it can't induce any dependencies. *)
                    Parsetree_utils.ParamDepSet.empty
                | _ -> failwith "TODO: compute_lambda_liftings_for_field. termination proof other than struct/measure/order while computing dependencies." (* [Unsure] *)) in
        (* Finally, the complete dependencies are the union of above. *)
        let meths_from_param =
          Parsetree_utils.ParamDepSet.union
            meths_from_param1 meths_from_param2 in
        (* Return a couple binding the species parameter's name with the
           methods of it we found as required for the current method. *)
        (species_param, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* By side effect, we will remind the species types appearing. *)
  let carriers_appearing_in_types = ref Types.SpeciesCarrierTypeSet.empty in
  (* Attention, for properties and theorems, the type doesn't contain valuable
     information since it is always "Prop". We must inspect the logical_expr
     (in fact its type) to check if some quantified variables in the
     proposition refers to species carrier types. In effect, if the proof or
     the theorem/property's statement doesn't use some parameters methods,
     then the [dependencies_from_params_in_bodies] will be empty and when,
     below, we will search used species parameters tys in "bodies", since no
     method of the parameters are used, we will never see that the parameter
     carriers are indeed used to quantify variables.
     Example:
       species Test (A is Setoid, B is Setoid) inherits Setoid =
         theorem b_is_snd_pair: all a in A, all b in B, true
         proof: assumed {* *};
       end
;;
     The body of [b_is_snd_pair] doesn't call any method of A or B. However,
     carriers of A and B are used to quantiy a and b. *)
  (match my_type with
   | FTK_computational t ->
       carriers_appearing_in_types := Types.get_species_types_in_type t
   | FTK_logical lexpr ->
       let params_carriers =
         get_species_types_in_type_annots_of_logical_expr lexpr in
       carriers_appearing_in_types := params_carriers);
  (* If the method has a termination proof, then we look for carriers of
     species parameters appearing in the type of the expression representing
     its order. *)
  (match opt_term_pr with
   | None -> ()
   | Some term_pr ->
       match term_pr.Parsetree.ast_desc with
        | Parsetree.TP_order (expr, _, _)
        | Parsetree.TP_measure (expr, _, _) ->
            let t =
              (match expr.Parsetree.ast_type with
               | Parsetree.ANTI_type t -> t
               | _ -> assert false) in
            carriers_appearing_in_types :=
              Types.SpeciesCarrierTypeSet.union
                (Types.get_species_types_in_type t)
                !carriers_appearing_in_types
        | Parsetree.TP_structural _ ->
            (* Abstracted stuff can not be used for structural termination.
               Moreover, structural termination can only rely on a function
               parameter, not an expression. Hence it can't induce any
               dependencies. *)
            ()
        | _ -> failwith "todo 124");
  (* By side effect, we remind the species types appearing in the species
     parameters methods' types we depend on. *)
  List.iter
    (fun (_, meths) ->
      Parsetree_utils.ParamDepSet.iter
        (fun (_, meth_kind) ->
          let st_set =
            (match meth_kind with
             | Parsetree_utils.DETK_computational meth_ty ->
                 Types.get_species_types_in_type meth_ty
             | Parsetree_utils.DETK_logical lexpr ->
                 get_species_types_in_type_annots_of_logical_expr lexpr) in
          carriers_appearing_in_types :=
            Types.SpeciesCarrierTypeSet.union
              st_set !carriers_appearing_in_types)
        meths)
    dependencies_from_params_in_bodies;
  (* Same thing for the methods of ourselves we decl-depend. Note that if we
     have a decl-dependency on "rep" then we do not need to inspect its
     structure to know if it contains references to some species parameter
     types since this means that the "rep" is still kept abstract. *)
  List.iter
    (fun (node, _) ->
      if node.DepGraphData.nn_name <> (Parsetree.Vlident "rep") then (
        let st_set =
          Types.get_species_types_in_type node.DepGraphData.nn_type in
        carriers_appearing_in_types :=
          Types.SpeciesCarrierTypeSet.union
            st_set !carriers_appearing_in_types;
        (* Since for a property or a theorem the ML-like type is not sufficient
           and we must inspect the logical statement, we recover our field
           kind. *)
        match
           get_if_field_logical_statement
             node.DepGraphData.nn_name all_my_fields with
         | None -> ()
         | Some lexpr ->
             (* We must find in the logical statement the dependencies on
                species parameters carriers. *)
             let from_lexpr_annots =
               get_species_types_in_type_annots_of_logical_expr lexpr in
             carriers_appearing_in_types :=
               Types.SpeciesCarrierTypeSet.union
                 from_lexpr_annots !carriers_appearing_in_types
       ))
    decl_children ;
  (* Same thing for the methods of ourselves we def-depend on. Attention, if we
     have a def-dependency on "rep", we must inspect its structure to know if
     it contains references to some species parameter since "rep"'s structure
     will appear in clear, possibly using these species parameters carrier
     types. So, conversely to just above, we don't make any difference between
     "rep" and other methods of ourselves. *)
  List.iter
    (fun (node, _) ->
      let st_set =
        Types.get_species_types_in_type node.DepGraphData.nn_type in
      carriers_appearing_in_types :=
        Types.SpeciesCarrierTypeSet.union st_set !carriers_appearing_in_types;
      (* Since for a property or a theorem the ML-like type is not sufficient
         and we must inspect the logical statement, we recover our field
         kind. *)
      match
         get_if_field_logical_statement
           node.DepGraphData.nn_name all_my_fields with
       | None -> ()
       | Some lexpr ->
           (* We must find in the logical statement the dependencies on
              species parameters carriers. *)
           let from_lexpr_annots =
             get_species_types_in_type_annots_of_logical_expr lexpr in
           carriers_appearing_in_types :=
             Types.SpeciesCarrierTypeSet.union
               from_lexpr_annots !carriers_appearing_in_types)
    def_children;
  (* Now compute the set of species parameters types used in the types of the
     methods comming from the species parameters that the current field uses.
     This information is required for Coq and Dedukti since they will lead to
     extra args of type "Set". *)
(* [Unsure] Ne garder seulement les param�tres en "is" ? *)
  let species_param_names =
    List.map
      (fun (species_param, _) ->
        (* Recover the species parameter's name. *)
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             Parsetree. Vuident n)
      dependencies_from_params_in_bodies in
  let used_species_parameter_tys =
    List.filter
      (fun species_param_name ->
        let as_string = Parsetree_utils.name_of_vname species_param_name in
        Types.SpeciesCarrierTypeSet.mem
          (current_unit, as_string) !carriers_appearing_in_types)
      species_param_names in
  (used_species_parameter_tys,
   dependencies_from_params_in_bodies,
   decl_children,
   def_children)
;;



(** {b Descr} : For intermediate internal computation where we need to remind
    from where the found dependencies on species parameters come.

    {b Exported}: No.                                                       *)
type internal_abstraction_info = {
  iai_used_species_parameter_tys : Parsetree.vname list;
  (** Dependencies found via [BODY] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  iai_dependencies_from_params_via_body :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
      list;
  (** Dependencies found via [TYPE] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  iai_dependencies_from_params_via_type :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
      list;
  (** Dependencies found via only [PRM]. Obviously they are all present in
      the set below ([iai_dependencies_from_params_via_completions]). *)
  iai_dependencies_from_params_via_PRM :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
       Parsetree_utils.ParamDepSet.t)
      list;
  (** Other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of definition
      72 page 153 of Virgile Prevosto's Phd + [DIDOU] applied on the rules
      [DEF-DEP], [UNIVERSE] and [PRM]. *)
  iai_dependencies_from_params_via_completions :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
      list;
  iai_min_coq_env : Env.TypeInformation.min_coq_env_element list ;
  iai_min_dk_env : Env.TypeInformation.min_dk_env_element list
}
;;



(* ************************************************************************** *)
(* (Env.TypeInformation.species_param * Parsetree_utils.DepNameSet.t) list -> *)
(*  (Env.TypeInformation.species_param * Parsetree_utils.DepNameSet.t) list ->*)
(*   (Env.TypeInformation.species_param * Parsetree_utils.DepNameSet.t) list  *)
(** {b Descr} : Merge 2 lists representing abstraction information into
    a single one. Each list has the form
      (Env.TypeInformation.species_param * Parsetree_utils.DepNameSet.t) list
    and it is assumed that the lists have the same length and that the
    Env.TypeInformation.species_param appear in the same order inthe lists.
    Such lists represent for each species parameter of a species, the set
    of methods from the parameter we depend on.
    Hence, all the lists to merge must have been built in the scope of the
    same species to ensure these invariants.

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let merge_abstraction_infos ai1 ai2 =
  List.map2
    (fun (prm1, deps1) (prm2, deps2) ->
      (* A few asserts to ensure the compiler is fine. *)
      assert (prm1 = prm2);
      let deps = Parsetree_utils.ParamDepSet.union deps1 deps2 in
      (prm1, deps))
    ai1 ai2
;;



(** Used to store the abstraction info of non mutually recursive field and
    the abstraction infoS of mutually recursive fieldS. *)
type ('a) variadic_data =
  | VD_One of 'a
  | VD_More of 'a list
;;



(** May return None is the searched field is "rep" since it may not be
    defined. *)
let find_field_abstraction_by_name name abstractions =
  let rec find_in_let_rec let_infos abstr_infos =
    match (let_infos, abstr_infos) with
    | ([], []) -> raise Not_found
    | (((_, n, _, _, _, _, _, _) :: qn), (ai :: qai)) ->
        if n = name then ai else find_in_let_rec qn qai
    | (_, _) -> assert false in
  let rec rec_find = function
    | [] ->
        if name = (Parsetree.Vlident "rep") then None
        else assert false
    | (field, (VD_One ai)) :: q -> (
        match field with
         | Env.TypeInformation.SF_sig (_, n, _)
         | Env.TypeInformation.SF_let (_, n, _, _, _, _, _, _) 
         | Env.TypeInformation.SF_theorem (_, n, _, _, _, _)
         | Env.TypeInformation.SF_property (_, n, _, _, _) ->
             if n = name then Some (ai) else rec_find q
         | Env.TypeInformation.SF_let_rec _ -> assert false
       )
    | (field, (VD_More ais)) :: q -> (
        match field with
        | Env.TypeInformation.SF_let_rec l -> (
            try Some (find_in_let_rec l ais) with Not_found -> rec_find q
           )
        | _ -> assert false) in
  rec_find abstractions
;;



(* *********************************************************************** *)
(* current_unit:Parsetree.module_name ->                                   *)
(*   Env.TypeInformation.species_param list ->                             *)
(*   Parsetree_utils.simple_species_expr ->                                *)
(*   (Parsetree.ident *                                                    *)
(*     ((Parsetree_utils.simple_species_expr_as_effective_parameter * int) *)
(*     list))                                                              *)
(** {b Descr} Get the list of species parameters names used as effective
     arguments of the **parametrised** species expression [spe_expr] and
     their position according to the [spe_expr]'s parameters (i.e. first,
     second or whatever).
     ATTENTION: [spe_expr] is assumed to be parametrised !!!

     For example: species S (Cp is ..., Cp' is S'(Cp))
     We want to know that Cp' uses Cp as 1st argument for the species S'.
     So we want to get the pair (Cp', (S', [(Cp, 1)])). If Cp' used
     another Cq' as third argument, we would get the pair:
     (Cp', (S', [(Cp, 1); (Cq, 3)])).

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
let get_user_of_parameters_with_position ~current_unit species_parameters
    spe_expr =
  (* Do not [fold_right] otherwise, the counter will be reversed compared to
     the order of the elements of the list, i.e. otherwise when processing the
     last element of the list, the counter will be 0 and once on the first
     element of the list it will be length of the list minus 1. *)
  let (params_with_pos, _) =
    List.fold_left
      (fun (accu, counter) effective_arg ->
        match effective_arg with
         | Parsetree_utils.SPE_Expr_entity _ ->
             (* "In" parameters are never involved. *)
             (accu, (counter + 1))
         | Parsetree_utils.SPE_Self ->
             (* "Self" is never a species parameter. It can be used as an
                effective argument, but NEVER declared as a parameter ! *)
             (accu, (counter + 1))
         | Parsetree_utils.SPE_Species (eff_arg_qual_vname, _) -> (
             match eff_arg_qual_vname with
              | Parsetree.Qualified (modname, eff_arg_vname)
                when modname = current_unit ->
                  let eff_arg_name =
                    Parsetree_utils.name_of_vname eff_arg_vname in
                  (* We check if this [eff_arg_name] is a species parameter of
                     the current species. If so, we keep in the result ! *)
                  if List.exists
                      (function
                        | Env.TypeInformation.SPAR_in (_, _, _) ->
                            false      (* "In" parameters are never involved. *)
                        | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                            n = eff_arg_name)
                      species_parameters
                  then
                    (((effective_arg, counter) :: accu), (counter + 1))
                  else
                    (accu, (counter + 1))
              | Parsetree.Qualified (_, _) -> (accu, (counter + 1))
              | Parsetree.Vname _ ->
                  (* Scoping should have transformed it into a [Qualified]. *)
                  assert false
            ))
      ([], 0)
      spe_expr.Parsetree_utils.sse_effective_args in
  (* Tells that species [sse_name] uses [params_with_pos] as arguments... *)
  (spe_expr.Parsetree_utils.sse_name, params_with_pos)
;;



(* ************************************************************************* *)
(* param_name: Parsetree.qualified_vname ->                                  *)
(*   deps:Parsetree_utils.ParamDepSet.t ->                                   *)
(*    to_deps:                                                               *)
(*      (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t)  *)
(*        list ->                                                            *)
(*  (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t)      *)
(*    list                                                                   *)
(** {b Descr}: Adds the dependencies (i.e. the set of methods names) [~deps]
    into the [~to_deps] list (being an assoc list (species param name, set
    of methods names representing the dependencies) in the bucket of the
    species parameter [~param].

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let add_param_dependencies ~param_name ~deps ~to_deps =
  let param_name_as_string =
    (match param_name with
     | Parsetree.Vname _ ->
         (* Scoping pass should have transformed all [Vname] into
            [Qualified]. *)
         assert false
     | Parsetree.Qualified (_, n) -> Parsetree_utils.name_of_vname n) in
  let rec rec_add = function
    | [] -> []
    | (p, d) :: q -> (
        match p with
         | Env.TypeInformation.SPAR_in (_, _, _) ->
             (* "In" parameters are never involved in the process. *)
             (p, d) :: (rec_add q)
         | Env.TypeInformation.SPAR_is ((_, name), _, _, _, _) ->
             (* If we are in the bucket of the searched species parameter, we
                add. *)
             if name = param_name_as_string then
               (p, (Parsetree_utils.ParamDepSet.union deps d)) :: q
             else (p, d) :: (rec_add q)
       ) in
  rec_add to_deps
;;



let subst_param_dep_set ~current_unit ~replaced ~replaced_by in_deps =
  Parsetree_utils.ParamDepSet.fold
    (fun (meth_name, dep_kind) accu ->
      match dep_kind with
       | Parsetree_utils.DETK_computational ty ->
           let ty' =
             Types.subst_type_simple
               replaced (Types.SBRCK_coll replaced_by) ty in
           Parsetree_utils.ParamDepSet.add
             (meth_name, (Parsetree_utils.DETK_computational ty')) accu
       | Parsetree_utils.DETK_logical lexpr ->
           let lexpr' =
             SubstColl.subst_logical_expr
               ~current_unit (SubstColl.SRCK_coll replaced)
               (Types.SBRCK_coll replaced_by) lexpr in
           Parsetree_utils.ParamDepSet.add
             (meth_name, (Parsetree_utils.DETK_logical lexpr')) accu)
    in_deps
    Parsetree_utils.ParamDepSet.empty
;;



(** Add the carriers present in the types of species parameter methods found
    during the completion done by [complete_dependencies_from_params]. *)
let complete_used_species_parameters_ty ~current_unit species_params initial_set
    used_species_parameter_tys_via_rep deps_in_type deps_via_compl =
    (* By side effect, we remind the species types appearing in our type. *)
  let carriers_appearing_in_types = ref used_species_parameter_tys_via_rep in
  (* Just the local function that will be used twice to process each
     dependencies set. *)
  let process_one_deps_set deps_set =
    List.iter
      (fun (_, meths) ->
        Parsetree_utils.ParamDepSet.iter
          (fun (_, meth_kind) ->
            let st_set =
              (match meth_kind with
               | Parsetree_utils.DETK_computational meth_ty ->
                   Types.get_species_types_in_type meth_ty
               | Parsetree_utils.DETK_logical lexpr ->
                   get_species_types_in_type_annots_of_logical_expr lexpr) in
            carriers_appearing_in_types :=
              Types.SpeciesCarrierTypeSet.union
                st_set !carriers_appearing_in_types)
          meths)
      deps_set in
  (* Now, process the first set. *)
  process_one_deps_set deps_in_type;
  (* Now, process the second set. *)
  process_one_deps_set deps_via_compl;
  (* Just recover the parameters names. *)
(* [Unsure] Ne garder seulement les param�tres en "is" ? *)
  let species_param_names =
    List.map
      (fun species_param ->
        (* Recover the species parameter's name. *)
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             Parsetree. Vuident n)
      species_params in
  (* And now, filter in the accumulator the carriers that are among our
     species parameters. *)
  let extra_params_carriers =
    List.filter
      (fun species_param_name ->
        let as_string = Parsetree_utils.name_of_vname species_param_name in
        Types.SpeciesCarrierTypeSet.mem
          (current_unit, as_string) !carriers_appearing_in_types)
      species_param_names in
  (* And finally, make the union with the initial set of carriers. *)
  Handy.list_concat_uniq initial_set extra_params_carriers
;;



(* Returns an extension, not an union, BUT CONTAINS FORCELY the initial set
   [initial_to_complete]. So ... not the union of the [via_body], [via_type] and
   [initial_to_complete]. *)
let complete_dependencies_from_params_rule_didou ~current_unit ~via_body
    ~via_type ~initial_to_complete =
  (* Join the 3 dependencies sets to lookup for fixpoint in only 1 set. *)
  let found_dependencies_from_params =
    merge_abstraction_infos
      via_body (merge_abstraction_infos via_type initial_to_complete) in
  (* We create the set that will be the final completion. Hence it at least
     contains the dependencies initially found in the [initial_to_complete]. *)
  let completed_by_fixpoint = ref initial_to_complete in
  let changed = ref true in
  while !changed do
    changed := false;   (* Reset the fixpoint reached signal. *)
    completed_by_fixpoint :=
    List.map2
      (fun (_, meths_new) (param, meths_old) ->
        (* [meths_new] : the set of new methods added compared to the set of
           methods that were initially already found by the previous rules.
           [meths_old] : the set of new methods freshly added compared to the
           set of methods that were initially already found by the previous
           rules. *)
        match param with
         | Env.TypeInformation.SPAR_in (_, _, _) -> (param, meths_new)
         | Env.TypeInformation.SPAR_is
              (spe_par_name, _, spe_meths, _, dep_graph) ->
             (* Let's have an accumulator for the set of new methods freshly
                added in order to work by side effect. This will be much
                simpler. *)
             let accu = ref meths_new in
             (* For each method of the species parameters we already found we
                depended on via completion... *)
             Parsetree_utils.ParamDepSet.iter
               (fun (meth_name, _) ->
                 (* For all decl-dependency coming from **the type** of the
                    method of this species parameter, we must add it as a
                    dependency on this species parameter method. *)
                 let decl_children =
                   (try
                     let my_node =
                       List.find
                         (fun { DepGraphData.nn_name = n } -> n = meth_name)
                         dep_graph in
                     List.filter
                       (function
                         | (_,
                            (DepGraphData.DK_decl
                               (DepGraphData.DcDK_from_type_logic |
                                DepGraphData.DcDK_from_type_comput))) -> true
                         | (_,
                            (DepGraphData.DK_decl
                               (DepGraphData.DcDK_from_body_logic |
                               DepGraphData.DcDK_from_body_comput))) -> false
                         | (_, (DepGraphData.DK_def _)) -> false)
                       my_node.DepGraphData.nn_children
                   with Not_found -> []  (* No children at all. *)) in
                 List.iter
                   (fun (node, _) ->
                     if node.DepGraphData.nn_name <> Parsetree.Vlident "rep" then
                       (begin
                       (* We found a method that must be possibly added if it is
                          not already present in the already found dependencies
                          and not in the already added dependencies. *)
                       let mkind =
                         Param_dep_analysis.guess_method_computational_or_logical
                           node.DepGraphData.nn_name None spe_meths in
                       (* We must replace occurrences of "Self" in this method
                          by the species parameter from where this method
                          comes. *)
                       let mkind =
                         match mkind with
                          | Parsetree_utils.DETK_computational ty ->
                              let ty' =
                                Types.copy_type_simple_but_variables
                                  ~and_abstract: (Some spe_par_name) ty in
                              Parsetree_utils.DETK_computational ty'
                          | Parsetree_utils.DETK_logical lexpr ->
                              let lexpr' =
                                SubstColl.subst_logical_expr
                                  ~current_unit SubstColl.SRCK_self
                                  (Types.SBRCK_coll spe_par_name) lexpr in
                              Parsetree_utils.DETK_logical lexpr' in
                       let elem = (node.DepGraphData.nn_name, mkind) in
                       if not
                           (Parsetree_utils.ParamDepSet.mem elem meths_old) &&
                          not (Parsetree_utils.ParamDepSet.mem elem !accu) then (
                         (* We must add the method into the accu since it was
                            never seen before. *)
                         accu := Parsetree_utils.ParamDepSet.add elem !accu;
                         (* For the fixpoint, we say that it is not yet
                            reached. *)
                         changed := true
                        )
                       end))
                   decl_children)
               meths_old ;
             (* Return the new set of added methods. *)
             (param, !accu))
      !completed_by_fixpoint
      found_dependencies_from_params
  done ;
  !completed_by_fixpoint
;;





(* ************************************************************************ *)
(* environment_kind -> current_unit:Parsetree.module_name ->                *)
(*   Env.TypeInformation.species_param list ->                              *)
(*    (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t)   *)
(*       list ->                                                            *)
(*      (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) *)
(*         list                                                             *)
(* {b Args}:
     - [dependencies_from_params] Those computer by dependencies found via
       the [TYPE] + [BODY] rules otherwise one get too many useless
       dependencies !

   {b Exported} : No.                                                       *)
(* ************************************************************************ *)
let complete_dependencies_from_params_rule_PRM env ~current_unit
    species_parameters deps_from_type_and_body =
  (* First, we look for "is" parameters themselves parametrised. We hunt in
     the [species_parameters], to get some [Env.TypeInformation.SPAR_is]
     whose [simple_species_expr] has a non empty list [sse_effective_args]. *)
  let params_being_parametrised =
    List.filter
      (function
        | Env.TypeInformation.SPAR_is ((_, _), _, _, spe_expr, _) ->
            (* Keep it only if there are parameters in the expression. *)
            spe_expr.Parsetree_utils.sse_effective_args <> []
        | Env.TypeInformation.SPAR_in (_, _, _) -> false)
      species_parameters in
  (* Now, get for each parametrised parameter of the species, which other
     parameters it uses as effective argument in which species and at
     which position.
     For example: species S (Cp is ..., Cp' is S'(Cp))
     We want to know that Cp' uses Cp as 1st argument for the species S'.
     So we want to get the pair (Cp', (S', [(Cp, 1)])). If Cp' used another
     Cq as third argument, we would get the pair:
     (Cp', (S', [(Cp, 1); (Cq, 3)])). *)
  let parametrised_params_with_their_effective_args_being_params =
    List.map
      (function
        | Env.TypeInformation.SPAR_in (_, _, _) ->
            (* "In" parameters are filtered just above ! *)
            assert false
        | Env.TypeInformation.SPAR_is ((_, n), _, _, spe_expr, _) ->
            (* In our example, [n] is Cp'. *)
            (n,
             (get_user_of_parameters_with_position
                ~current_unit species_parameters spe_expr)))
      params_being_parametrised in
  (* Now, we know that Cp' is a species parameter built from S' applying Cp at
     position 0. We must find the name of the formal parameter in S'
     corresponding to the position where Cp is applied. Let's call it K. We
     have now to find all the dependencies (methods y) of K in S' and we must
     add them to the dependencies of Cp. *)
  List.fold_left
    (fun accu_deps_from_params (cpprim, (sprim, usages)) ->
      (* Recover the abstraction infos of methods of S'. *)
      let (sprim_params, sprim_meths_abstr_infos) =
        let sprim_info =
          Env.TypingEnv.find_species
            ~loc: Location.none ~current_unit sprim env in
        (sprim_info.Env.TypeInformation.spe_sig_params,
         sprim_info.Env.TypeInformation.spe_meths_abstractions) in
      List.fold_left
        (fun inner_accu_deps_from_params (effective_arg, position) ->
          (* Here, [effective_arg] is Cp. *)
          match effective_arg with
           | Parsetree_utils.SPE_Self | Parsetree_utils.SPE_Expr_entity _ ->
               (* See remark in [get_user_of_parameters_with_position]. *)
               assert false
           | Parsetree_utils.SPE_Species (eff_arg_qual_vname, _) ->
               (* Here, [eff_arg_qual_vname] is the parameter in which we will
                  add new dependencies.
                  We now get the name of the formal parameter (Cp) of S' at
                  the position where the effective argument was used. *)
               let formal_name = List.nth sprim_params position in
               (* Now, get the z in Deps (S, C_{p'}) (that can be found in
                  [deps_from_type_and_body]), ...
                  We need to take into accound both TYPE and BODY because some
                  methods (theorems for instance) may make reference to no
                  parameter method in their type (logical statement), but only
                  in their body (proof). This was bug #18. *)
               let all_z =
                 Handy.list_assoc_custom_eq
                   (fun x y ->
                     match x with
                      | Env.TypeInformation.SPAR_in (_, _, _) ->
                          (* Cp' is a "IS" parameter, so no chance to find it
                             amongst the "IN" parameters ! *)
                          false
                      | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                          n = y)
                   cpprim deps_from_type_and_body in
               Parsetree_utils.ParamDepSet.fold
                 (fun z accu_deps_for_zs ->
                   (* Forall z, we must search the set of methods, y, on which
                      z depends on in S' via [formal_name] ...
                      So, first get z's dependencies information. *)
                   let z_priv_meth_info =
                     List.assoc (fst z) sprim_meths_abstr_infos in
                   (* Find the y's by looking in dependencies via the TYPE of
                      z.
                      [UNSURE] Currently, not only the TYPE, rather:
                        [DIDOU] on ([TYPE]),
                      i.e. the same set than used to known what to lambda-lift
                      for the record type. C.f bug #18 *)
                   let z_dependencies =
                     z_priv_meth_info.Env.TypeInformation.
                       ad_dependencies_from_parameters_in_type in
                   (* Now, find the one corresponding to [formal_name]. If
                      none is found in the assoc list that because there no
                      method in the dependencies on this parameter. *)
                   let y =
                     (try
                       let (Env.ODFP_methods_list lst) =
                         List.assoc formal_name z_dependencies in
                       Parsetree_utils.list_to_param_dep_set lst
                     with
                     | Not_found -> Parsetree_utils.ParamDepSet.empty) in
                   (* ATTENTION: We must instanciate the formal parameter of
                      S' by the effective argument provided. This means that
                      we must replace [formal_name] by [eff_arg_qual_vname] in
                      the dependencies [y]. In effect, in the bodies/types of
                      the methods of S', parameters are those of S', not our
                      current ones we use to instanciate the formal ones of
                      S' ! To prevent those of S' to remain in the expressions
                      and be unbound, we do the instanciation here. *)
                   let replaced =
                     (match formal_name with
                      | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
                      | Env.TypeInformation.SPAR_is (ty_coll, _, _, _, _) ->
                          ty_coll) in
                   let replaced_by =
                     (match eff_arg_qual_vname with
                      | Parsetree.Vname n ->
                          (current_unit, (Parsetree_utils.name_of_vname n))
                      | Parsetree.Qualified (m, n) ->
                          (m, (Parsetree_utils.name_of_vname n))) in
                   let substituted_y =
                     subst_param_dep_set
                       ~current_unit ~replaced ~replaced_by y in
                   (* ... and add it to the dependencies of
                      [eff_arg_qual_vname] in the current dependencies
                      accumulator, i.e into [inner_accu_deps_from_params]. *)
                   add_param_dependencies
                     ~param_name: eff_arg_qual_vname ~deps: substituted_y
                     ~to_deps: accu_deps_for_zs)
                 (* Arguments of the deepest [DepNameSet.fold]. *)
                 all_z
                 inner_accu_deps_from_params)
        (* Arguments of the inner [List.fold_left]. *)
        accu_deps_from_params
        usages)
    (* Arguments of the outer [List.fold_left]. *)
    (make_empty_param_deps species_parameters) (* Start from an empty set. We
                                                  do not accumulate with the
                                                  already found dependencies. *)
    parametrised_params_with_their_effective_args_being_params
;;



(* ************************************************************************* *)
(* environment_kind -> current_unit: Types.fname ->                          *)
(*   current_species: Parsetree.qualified_species ->                         *)
(*     internal_field_abstraction_info list ->                               *)
(*       Env.TypeInformation.species_param list ->                           *)
(*         (DepGraphData.name_node * 'a) list ->                             *)
(*           'b VisUniverse.Universe.t -> field_type_kind ->                 *)
(*             (Env.TypeInformation.species_param *                          *)
(*              Parsetree_utils.ParamDepSet.t) list *                        *)
(*             (Env.TypeInformation.species_param *                          *)
(*              Parsetree_utils.ParamDepSet.t) list *                        *)
(*             (Env.TypeInformation.species_param *                          *)
(*       Parsetree_utils.ParamDepSet.t) list *                               *)
(*             Types.SpeciesCarrierTypeSet.t                                 *)
(** {b Descr} : Implements rules [TYPE], [DEF-DEP], [UNIVERSE] and [PRM] of
    the definition 72 page 153 of Virgile Prevosto's Phd.

    {b Args} :
      - [env] : The current code generation environment.

      - [~current_unit] : The current compilation unit.

      - [~current_species] : The current species, i.e. the one that hosts
        the fields currently processed.

      - [seen_abstractions] : The list of [abstraction_info] computed for
        the methods of the current species we already processed. It is in
        fact the accumulator of the [abstraction_info]s we are computing.

      - [species_parameters] : The information abou species parameters
        the current species has.

      - [def_children] : The methods on which the current method has
        def-dependencies (i.e. the children of the current method in the
        dependency graph of the current method).

      - [universe] : The visible universe of the current method (the method
        for which we are currently computing abstractions).

      - [type_kind] : The "type" of the method, i.e. the ML-like type if
        the method is computational, the logical expression if the method
        is logical (theorem or property).

    {b Ret} :
      - [(Env.TypeInformation.species_param *
          Parsetree_utils.ParamDepSet.t) list] : The dependencies on species
        parameters' methods induced by only the [TYPE] rule.

      - [(Env.TypeInformation.species_param *
          Parsetree_utils.ParamDepSet.t) list] : The dependencies on species
        parameters' methods induced by only the [PRM] rule.

      - [(Env.TypeInformation.species_param *
          Parsetree_utils.ParamDepSet.t) list] : The union of dependencies
        on species coming from [DEF-DEP], [UNIVERSE] and [PRM].

      - [Types.SpeciesCarrierTypeSet.t] : The list of carriers of species
        parameters appearing in types that appears in the above
        dependencies (i.e. in the types of methods tagged as we depend
        on).

    {b Exported} : No.                                                       *)
(* ************************************************************************* *)
let complete_dependencies_from_params env ~current_unit ~current_species
    seen_abstractions species_parameters def_children universe type_kind
    deps_from_body =
  (* Rule [TYPE] possible only if a logical expression is provided. In effect,
     in a type scheme, species_parameters can never appear since it is a
     ML-like type. Furthermore, even in case of termination proof, we have
     nothing to do since expressions appearing have ML-like types and proofs
     are not considered as "type". *)
  let dependencies_from_params_via_type =
    (match type_kind with
     | FTK_computational _ ->
         (* The "empty" dependencies cannot simple be [] because all our "union"
            functions on dependencies rely on a list of sets with 1 set for each
            species parameter name. So we create our initial accumulator as the
            list mapping each species parameter name onto the empty dependencies
            set. *)
         make_empty_param_deps species_parameters
     | FTK_logical lexpr ->
         (* Same remark about [fold_right] than for the function
            [compute_lambda_liftings_for_field] when computing
            [dependencies_from_params_in_bodies]. *)
         List.fold_right
           (fun species_param accu ->
             (* Recover the species parameter's name. *)
             let (species_param_name, species_param_meths) =
               match species_param with
                | Env.TypeInformation.SPAR_in (n, _, _) -> (n, [])
                | Env.TypeInformation.SPAR_is ((_, n), _, meths, _, _) ->
                    ((Parsetree. Vuident n), meths) in
             let meths_from_param =
               Param_dep_analysis.param_deps_logical_expr
                 ~current_species (species_param_name, species_param_meths)
                 lexpr in
             (* Return a couple binding the species parameter's name with the
                methods of it we found as required for the current method. *)
             (species_param, meths_from_param) :: accu)
           species_parameters
           []) in
  (* Rule [DEF-DEP]. *)
  (* By side effect, we remind the species types appearing in our type. *)
  let carriers_appearing_in_types = ref Types.SpeciesCarrierTypeSet.empty in
  let abstr_infos_from_all_def_children =
    List.fold_left
      (fun accu (def_child, _) ->
        let st_set =
          Types.get_species_types_in_type def_child.DepGraphData.nn_type in
        carriers_appearing_in_types :=
          Types.SpeciesCarrierTypeSet.union
            st_set !carriers_appearing_in_types;
        (* Get the abstraction info of the child we def-depend on. *)
        (find_field_abstraction_by_name def_child.DepGraphData.nn_name
           seen_abstractions) :: accu)
      []
      def_children in
  (* The "empty" dependencies cannot simple be [] because all our "union"
     functions on dependencies rely on a list of sets with 1 set for each
     species parameter name. So we create our initial accumulator as the
     list mapping each species parameter name onto the empty dependencies
     set. *)
  let empty_initial_deps_accumulator =
    make_empty_param_deps species_parameters in
  (* Since methods on which we depend are from Self, all of them share the
     same species parameter names, and by construction, each of them have the
     same structure of list (i.e. species parameter names at the same place in
     the list) for their [iai_dependencies_from_params_via_body].
     Hence, instead of making [List.map] on the [species_parameter_names] to
     individually merge the methods from each children for a species parameter,
     we simply make the union (without double) of all the
     [iai_dependencies_from_params_via_body] of the def-children. *)
  let dependencies_from_params_via_compl1 =
    List.fold_left
      (fun accu_deps_from_params abstr_infos_opt ->
        match abstr_infos_opt with
         | Some abstr_infos ->
             (* We merge the found abstraction info and the abstraction info
                accumulator. *)
             merge_abstraction_infos
               abstr_infos.iai_dependencies_from_params_via_body
               accu_deps_from_params
         | None ->
             (* No abstr_infos found, so leave the accumulator as it was. *)
             accu_deps_from_params)
      empty_initial_deps_accumulator
      abstr_infos_from_all_def_children in
  (* Rule [UNIVERS]. We extend [dependencies_from_params_via_compl1]. *)
  let dependencies_from_params_via_compl2 =
    VisUniverse.Universe.fold
      (fun z_name_in_univ _ accu_deps_from_params ->
        (* For each z (c.f. notation in Virgile) in the visible universe, we
           must add its [iai_dependencies_from_params_via_type].
           So we must first search the abstraction info of [z_name_in_univ].
           Since "rep" is a method like the others, it may appear in the
           universe. *)
        let abstr_info_opt =
          find_field_abstraction_by_name z_name_in_univ seen_abstractions in
        match abstr_info_opt with
         | Some abstr_info ->
             carriers_appearing_in_types :=
               List.fold_left
                 (fun  accu tn ->
                   let tn = Parsetree_utils.name_of_vname tn in
                   let tn = (current_unit, tn) in
                   Types.SpeciesCarrierTypeSet.add (tn) accu)
                 !carriers_appearing_in_types
                 abstr_info.iai_used_species_parameter_tys;
             (* Now, add the [iai_dependencies_from_params_via_type] to the
                dependencies accumulator. *)
             merge_abstraction_infos
               abstr_info.iai_dependencies_from_params_via_type
               accu_deps_from_params
         | None ->
             (* No abstr_infos found, so leave the accumulator as it was. *)
             accu_deps_from_params)
      universe
      dependencies_from_params_via_compl1 in
  (* Join all the found dependencies via [TYPE] and [BODY] in a unique bunch
     so that [complete_dependencies_from_params_rule_PRM] will have 1 bunch into
     which it has to search for dependencies on other parameters. *)
  let found_deps_via_type_and_body =
    merge_abstraction_infos dependencies_from_params_via_type deps_from_body in
  (* Get the found dependencies via rule [PRM]. This rule must only apply to
     dependencies found from the [TYPE] rule otherwise one get too many
     useless dependencies ! *)
  let dependencies_from_params_via_PRM =
    complete_dependencies_from_params_rule_PRM
      env ~current_unit species_parameters found_deps_via_type_and_body in
  (* Merge the completions. *)
  let dependencies_from_params_via_compl3 =
    merge_abstraction_infos
      dependencies_from_params_via_compl2 dependencies_from_params_via_PRM in
 (dependencies_from_params_via_type, (* The dependencies induced by only the
                                        [TYPE] rule. *)
  dependencies_from_params_via_PRM, (* The dependencies induced by only the
                                       [PRM] rule. *)
  dependencies_from_params_via_compl3, (* The union of dependencies coming
                                          from [DEF-DEP], [UNIVERSE] and
                                          [PRM]. *)
  !carriers_appearing_in_types)
;;



(* ************************************************************************** *)
(* Env.TypingEnv.t ->
   abstractions_comput_context ->
   Env.TypeInformation.species_field list ->
     (Env.TypeInformation.species_field *
      internal_abstraction_info variadic_data) list

   {b Exported} : No.                                                         *)
(* ************************************************************************** *)
let __compute_abstractions_for_fields env ctx fields =
  let reversed_abstractions =
    (* ATTENTION: do not [fold_right] ! We build the list in reverse order
       end finally reverse it at the end for sake of efficiency. We explicitly
       [fold_left] to have in our accumulator, the list of fields already
       processed, and in the right order (in their order of apparition) AND to
       process fields of the list [fields] in their order of apparition. We
       need this in order to recover the already computed dependencies from
       params on previous fields since this info will possibly used to apply
       rules [DEF-DEP], [UNIVERSE] and [PRM] of definition 72 page 153 from
       Virgile Prevosto's Phd. *)
    List.fold_left
      (fun abstractions_accu current_field ->
        match current_field with
         | Env.TypeInformation.SF_sig (_, _, sch) ->
             (* Trivially, sigs can't induce dependencies on methods since
                they only involve types. However, they may induce species
                parameters carrier types used. *)
             let ty = Types.specialize sch in
             let used_species_parameter_tys =
               Types.get_species_types_in_type ty in
             let species_param_names =
               List.map
                 (fun species_param ->
                   (* Recover the species parameter's name. *)
                   match species_param with
                    | Env.TypeInformation.SPAR_in (n, _, _) -> n
                    | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
                        Parsetree. Vuident n)
                 ctx.acc_species_parameters_names in
             let as_set =
               List.filter
                 (fun species_param_name ->
                   let as_string =
                     Parsetree_utils.name_of_vname species_param_name in
                   Types.SpeciesCarrierTypeSet.mem
                     (ctx.acc_current_unit, as_string)
                     used_species_parameter_tys)
                 species_param_names in
             (* The "empty" dependencies cannot simple be [] because all our
                "union" functions on dependencies rely on a list of sets with 1
                set for each species parameter name. So we create our initial
                accumulator as the list mapping each species parameter name
                onto the empty dependencies set. *)
             let empty_deps =
               make_empty_param_deps ctx.acc_species_parameters_names in
             let abstr_info = {
               iai_used_species_parameter_tys = as_set;
               iai_dependencies_from_params_via_body = empty_deps;
               iai_dependencies_from_params_via_type = empty_deps;
               iai_dependencies_from_params_via_PRM = empty_deps;
               iai_dependencies_from_params_via_completions = empty_deps;
               iai_min_coq_env = [];
               iai_min_dk_env = []} in
             (current_field, (VD_One abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let (_, name, _, sch, body, _, _, _) ->
             let method_ty = Types.specialize sch in
             (* ATTENTION, the [dependencies_from_params_in_body] is not
                the complete set of dependencies. It must be completed to
                fully represent the definition 72 page 153 from Virgile
                Prevosto's Phd. *)
             let (used_species_parameter_tys_in_self_methods_bodies,
                  dependencies_from_params_in_body,
                  decl_children, def_children) =
               let body_as_fbk =
                 match body with
                  | Parsetree.BB_logical p -> FBK_logical_expr p
                  | Parsetree.BB_computational e -> FBK_expr e in
               compute_lambda_liftings_for_field
                 ~current_unit: ctx.acc_current_unit
                 ~current_species: ctx.acc_current_species
                 ctx.acc_species_parameters_names
                 ctx.acc_dependency_graph_nodes name
                 body_as_fbk (FTK_computational method_ty) None fields in
             (* Compute the visible universe of the method. *)
             let universe =
               VisUniverse.visible_universe
                 ctx.acc_dependency_graph_nodes decl_children
                 def_children in
             (* Complete the dependencies from species parameters info. By the
                way, we record the species parameters carrier appearing in the
                methods of self that were added during the completion phase. *)
             let (dependencies_from_params_in_type,
                  dependencies_from_params_via_prm,
                  dependencies_from_params_via_compl,
                  used_species_parameter_tys_in_meths_self_after_completion) =
               complete_dependencies_from_params
                 env ~current_unit: ctx.acc_current_unit
                 ~current_species: ctx.acc_current_species
                 abstractions_accu ctx.acc_species_parameters_names
                 def_children universe (FTK_computational method_ty)
                 dependencies_from_params_in_body in
             (* Extra completion by a transitive closure that was missing in
                Virgile Prevosto's Phd. *)
             let dependencies_from_params_via_didou =
               complete_dependencies_from_params_rule_didou
                 ~current_unit: ctx.acc_current_unit
                 ~via_body: dependencies_from_params_in_body
                 ~via_type: dependencies_from_params_in_type
                 ~initial_to_complete: dependencies_from_params_via_compl in
             (* Now, we complete the species parameters carriers seen by
                taking into account types of methods obtained by the
                completion of the dependencies on parameters achieved by
                [complete_dependencies_from_params] and
                [complete_dependencies_from_params_rule_didou]. *)
             let all_used_species_parameter_tys =
               complete_used_species_parameters_ty
                 ~current_unit: ctx.acc_current_unit
                 ctx.acc_species_parameters_names
                 used_species_parameter_tys_in_self_methods_bodies
                 used_species_parameter_tys_in_meths_self_after_completion
                 dependencies_from_params_in_type
                 dependencies_from_params_via_didou in
             (* Now, its minimal Coq/Dedukti typing environment. *)
             let min_coq_env =
               MinEnv.minimal_coq_typing_environment universe fields in
             let min_dk_env =
               MinEnv.minimal_dk_typing_environment universe fields in
             let abstr_info = {
               iai_used_species_parameter_tys = all_used_species_parameter_tys;
               iai_dependencies_from_params_via_body =
                 dependencies_from_params_in_body;
               iai_dependencies_from_params_via_type =
                 dependencies_from_params_in_type;
               iai_dependencies_from_params_via_PRM =
                 dependencies_from_params_via_prm;
               iai_dependencies_from_params_via_completions =
                 dependencies_from_params_via_didou;
               iai_min_coq_env = min_coq_env;
               iai_min_dk_env = min_dk_env } in
             (current_field, (VD_One abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let_rec l ->
             let deps_infos =
               List.map
                 (fun (_, name, _, sch, body, opt_term_pr, _, _) ->
                   let body_as_fbk =
                     match body with
                      | Parsetree.BB_logical p -> FBK_logical_expr p
                      | Parsetree.BB_computational e -> FBK_expr e in
                   let method_ty = Types.specialize sch in
                   (* ATTENTION, the [dependencies_from_params_in_bodies] is
                      not the complete set of dependencies. It must be  to
                      completed fully represent the definition 72 page 153
                      from Virgile Prevosto's Phd. *)
                   let (used_species_parameter_tys_in_self_methods_bodies,
                        dependencies_from_params_in_bodies,
                        decl_children, def_children) =
                     compute_lambda_liftings_for_field
                       ~current_unit: ctx.acc_current_unit
                       ~current_species: ctx.acc_current_species
                       ctx.acc_species_parameters_names
                       ctx.acc_dependency_graph_nodes name
                       body_as_fbk (FTK_computational method_ty) opt_term_pr
                       fields in
                   (* Compute the visible universe of the method. *)
                   let universe =
                     VisUniverse.visible_universe
                       ctx.acc_dependency_graph_nodes
                       decl_children def_children in
                   (* Complete the dependencies from species parameters info.
                      By the way, we record the species parameters carrier
                      appearing in the methods of self that were added during
                      the completion phase. *)
                   let (dependencies_from_params_in_type,
                        dependencies_from_params_via_prm,
                        dependencies_from_params_via_compl,
                        used_species_parameter_tys_in_meths_self_after_completion) =
                     complete_dependencies_from_params
                       env ~current_species: ctx.acc_current_species
                       ~current_unit: ctx.acc_current_unit
                       abstractions_accu ctx.acc_species_parameters_names
                       def_children universe (FTK_computational method_ty)
                       dependencies_from_params_in_bodies in
                   (* Extra completion by a transitive closure that was missing
                      in Virgile Prevosto's Phd. *)
                   let dependencies_from_params_via_didou =
                     complete_dependencies_from_params_rule_didou
                       ~current_unit: ctx.acc_current_unit
                       ~via_body: dependencies_from_params_in_bodies
                       ~via_type: dependencies_from_params_in_type
                       ~initial_to_complete:
                         dependencies_from_params_via_compl in
                   (* Now, we complete the species parameters carriers seen
                      by taking into account types of methods obtained by
                      the completion of the dependencies on parameters achieved
                      by [complete_dependencies_from_params]. *)
                   let all_used_species_parameter_tys =
                     complete_used_species_parameters_ty
                       ~current_unit: ctx.acc_current_unit
                       ctx.acc_species_parameters_names
                       used_species_parameter_tys_in_self_methods_bodies
                       used_species_parameter_tys_in_meths_self_after_completion
                       dependencies_from_params_in_type
                       dependencies_from_params_via_didou in
                   (* Now, its minimal Coq/Dedukti typing environment. *)
                   let min_coq_env =
                     MinEnv.minimal_coq_typing_environment universe fields in
                   let min_dk_env =
                     MinEnv.minimal_dk_typing_environment universe fields in
                     { iai_used_species_parameter_tys =
                         all_used_species_parameter_tys;
                       iai_dependencies_from_params_via_body =
                         dependencies_from_params_in_bodies;
                       iai_dependencies_from_params_via_type =
                         dependencies_from_params_in_type;
                       iai_dependencies_from_params_via_PRM =
                         dependencies_from_params_via_prm;
                       iai_dependencies_from_params_via_completions =
                         dependencies_from_params_via_didou;
                       iai_min_coq_env = min_coq_env ;
                       iai_min_dk_env = min_dk_env })
                 l in
             (current_field, (VD_More deps_infos)) :: abstractions_accu
         | Env.TypeInformation.SF_theorem
             (_, name, _, logical_expr, proof, _) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not
                  the complete set of dependencies. It must be completed to
                  fully represent the definition 72 page 153 from Virgile
                  Prevosto's Phd. *)
               let (used_species_parameter_tys_in_self_methods_bodies,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.acc_current_unit
                   ~current_species: ctx.acc_current_species
                   ctx.acc_species_parameters_names
                   ctx.acc_dependency_graph_nodes name
                   (FBK_proof (Some proof)) (FTK_logical logical_expr)
                   None fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ctx.acc_dependency_graph_nodes decl_children
                   def_children in
               (* Now, its minimal Coq/Dedukti typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_coq_typing_environment universe fields in
               let min_dk_env =
                 MinEnv.minimal_dk_typing_environment universe fields in
               (* Complete the dependencies from species parameters info. By the
                  way, we record the species parameters carrier appearing in the
                  methods of self that were added during the completion
                  phase. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_prm,
                    dependencies_from_params_via_compl,
                    used_species_parameter_tys_in_meths_self_after_completion) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.acc_current_species
                   ~current_unit: ctx.acc_current_unit
                   abstractions_accu ctx.acc_species_parameters_names
                   def_children universe (FTK_logical logical_expr)
                   dependencies_from_params_in_bodies in
               (* Extra completion by a transitive closure that was missing in
                  Virgile Prevosto's Phd. *)
               let dependencies_from_params_via_didou =
                 complete_dependencies_from_params_rule_didou
                   ~current_unit: ctx.acc_current_unit
                   ~via_body: dependencies_from_params_in_bodies
                   ~via_type: dependencies_from_params_in_type
                   ~initial_to_complete: dependencies_from_params_via_compl in
               (* Now, we complete the species parameters carriers seen by
                  taking into account types of methods obtained by the
                  completion of the dependencies on parameters achieved by
                  [complete_dependencies_from_params]. *)
               let all_used_species_parameter_tys =
                 complete_used_species_parameters_ty
                   ~current_unit: ctx.acc_current_unit
                   ctx.acc_species_parameters_names
                   used_species_parameter_tys_in_self_methods_bodies
                   used_species_parameter_tys_in_meths_self_after_completion
                   dependencies_from_params_in_type
                   dependencies_from_params_via_didou in
               let abstr_info = {
                 iai_used_species_parameter_tys =
                   all_used_species_parameter_tys;
                 iai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies;
                 iai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type;
                 iai_dependencies_from_params_via_PRM =
                   dependencies_from_params_via_prm;
                 iai_dependencies_from_params_via_completions =
                   dependencies_from_params_via_didou;
                 iai_min_coq_env = min_coq_env;
                 iai_min_dk_env = min_dk_env } in
               (current_field, (VD_One abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_property (_, name, _, logical_expr, _) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not
                  the complete set of dependencies. It must be completed to
                  fully represent the definition 72 page 153 from Virgile
                  Prevosto's Phd. *)
               let (used_species_parameter_tys_in_self_methods_bodies,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.acc_current_unit
                   ~current_species: ctx.acc_current_species
                   ctx.acc_species_parameters_names
                   ctx.acc_dependency_graph_nodes name
                   (FBK_proof None) (FTK_logical logical_expr) None fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ctx.acc_dependency_graph_nodes decl_children
                   def_children in
               (* Complete the dependencies from species parameters info. By the
                  way, we record the species parameters carrier appearing in the
                  methods of self that were added during the completion
                  phase. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_prm,
                    dependencies_from_params_via_compl,
                    used_species_parameter_tys_in_meths_self_after_completion) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.acc_current_species
                   ~current_unit: ctx.acc_current_unit
                   abstractions_accu ctx.acc_species_parameters_names
                   def_children universe (FTK_logical logical_expr)
                   dependencies_from_params_in_bodies in
               (* Extra completion by a transitive closure that was missing in
                  Virgile Prevosto's Phd. *)
               let dependencies_from_params_via_didou =
                 complete_dependencies_from_params_rule_didou
                   ~current_unit: ctx.acc_current_unit
                   ~via_body: dependencies_from_params_in_bodies
                   ~via_type: dependencies_from_params_in_type
                   ~initial_to_complete: dependencies_from_params_via_compl in
               (* Now, we complete the species parameters carriers seen by
                  taking into account types of methods obtained by the
                  completion of the dependencies on parameters achieved by
                  [complete_dependencies_from_params]. *)
               let all_used_species_parameter_tys =
                 complete_used_species_parameters_ty
                   ~current_unit: ctx.acc_current_unit
                   ctx.acc_species_parameters_names
                   used_species_parameter_tys_in_self_methods_bodies
                   used_species_parameter_tys_in_meths_self_after_completion
                   dependencies_from_params_in_type
                   dependencies_from_params_via_didou in
               (* Now, its minimal Coq/Dedukti typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_coq_typing_environment universe fields in
               let min_dk_env =
                 MinEnv.minimal_dk_typing_environment universe fields in
               let abstr_info = {
                 iai_used_species_parameter_tys =
                   all_used_species_parameter_tys;
                 iai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies;
                 iai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type;
                 iai_dependencies_from_params_via_PRM =
                   dependencies_from_params_via_prm;
                 iai_dependencies_from_params_via_completions =
                   dependencies_from_params_via_didou;
                 iai_min_coq_env = min_coq_env;
                 iai_min_dk_env = min_dk_env } in
               (current_field, (VD_One abstr_info)) :: abstractions_accu)
      []      (* Initial empty abstractions accumulator. *)
      fields in
  (* Finally, put the list of abstractions in the right order, i.e. in the
     order of apparition of the fields in the species. *)
  List.rev reversed_abstractions
;;




(* [Unsure] Not efficient: we examine the species parameter at each turn ! *)
let rec find_subst_of spe_param = function
  | [] -> raise Not_found
  | h :: q -> (
      match h with
       | Env.SK_collection_by_collection (c1, _) -> (
           match spe_param with
            | Env.TypeInformation.SPAR_in (_, _, _) ->
                (* Substitution of a collection is not for "IN" parameters. *)
                find_subst_of spe_param q
            | Env.TypeInformation.SPAR_is (prm_ty_col, _, _, _, _) ->
                if prm_ty_col = c1 then h else find_subst_of spe_param q
          )
       | Env.SK_ident_by_expression (_, entp_name, _) -> (
           match spe_param with
            | Env.TypeInformation.SPAR_in (in_name, _, _) ->
                if in_name = entp_name then h else find_subst_of spe_param q
            | Env.TypeInformation.SPAR_is (_, _, _, _, _) ->
                (* Substitution of an expression identifier is not for "IS"
                   parameters. *)
                find_subst_of spe_param q
          )
     )
;;




let remap_dependencies_on_params_for_field env ctx from name
    non_mapped_used_species_parameter_tys non_mapped_deps =
  (* We first check if the method was inherited. Only if it is the case then
     we need to remap the computed dependencies on the parent's dependencies
     scheme. *)
  if from.Env.fh_initial_apparition <> ctx.acc_current_species then
    (begin
    (* We recover most recent inherited method abstractions information. This
       should never fail since the species must exist before the current one
       is built. *)
    let (inh_from_mod, inh_from_sp) = find_most_recent_parent from in
    let (_, _, substs) = List.hd from.Env.fh_inherited_along in
    (* Just create a temporary ident from the original hosting species name. *)
    let fake_ident = {
      Parsetree.ast_loc = Location.none ;
      Parsetree.ast_annot = [] ;
      Parsetree.ast_type = Parsetree.ANTI_none ;
      Parsetree.ast_desc =
        (* If the hosting species is in the current compilation unit, then we
           make a fake LOCAL ident. Otherwise, a fake GLOBAL ident. *)
        if inh_from_mod = ctx.acc_current_unit then
          Parsetree.I_local inh_from_sp
        else
          Parsetree.I_global
            (Parsetree.Qualified (inh_from_mod, inh_from_sp)) } in
    (* Really get in the environment the information about the method. *)
    let original_hosting_species_meths =
      (* Just keep the information about methods. *)
      let orig_host_info =
        Env.TypingEnv.find_species
          ~loc: Location.none
          ~current_unit: ctx.acc_current_unit fake_ident env in
      orig_host_info.Env.TypeInformation.spe_meths_abstractions in
    (* We now must look for the inherited method name among the found
       information. *)
    let found_meth = List.assoc name original_hosting_species_meths in
    (* Implicitely the collection parameters of the found species are
       hosted in the compilation unit where their species is hosted,
       i.e. [inh_from_mod]. *)
    (* The first step of the mapping addresses the
       [used_species_parameter_tys]. We must make sure that original
       parameters instanciated by the same effective parameter lead to
       several times the corresponding argument in the abstraction info.
       For instance, in:
         species Couple (S is Simple, T is Simple) =
           let equiv (e1, e2) = T!equal (!morph (e1), !morph (e2));
         end ;;
         species Bug (G is Simple) inherits Couple (G, G) = ... end ;;
       the formal parameters S and T are instanciated by inheritance both by
       G. In Couple, equiv depends on the types S and T.
       So application of the method generator of equiv in Bug must have twice
       __p_G_T provided: once for the lambda-lift of S and once for the one of
       T in Couple (yep, remember that in Couple, the method equiv depends on
       2 species parameter types, S and T).

       To do so we take the inherited method's [used_species_parameter_tys] and
       we substitute the vnames by what they were instanciated during the
       inheritance in Bug. Obviously we can now have in the result list several
       times a same collection parameter [vname].

       ATTENTION: If the collection used to instanciate was not a parameter but
       a toplevel species or collection, then makeing anyway the substitution
       doesn't matter because the list [used_species_parameter_tys] will not
       contains a pointer to the instancier's carrier ... since the instancier
       IS NOT a collection parameter. Hence, applying the substitution to a
       list of [used_species_parameter_tys] that doesn't contain the toplevel
       species/collection's carrier will trivially be "doing nothing". *)
    (* Here is the new list of [used_species_parameter_tys] in our current
       method. Do not [fold_left] otherwise parameters carriers list will be
       reversed. We can't simply use [List.map] since in certain cases, the
       dependency on collection parameter can disappear in the current
       species. *)
    let new_used_species_parameter_tys =
      List.fold_right
        (fun param_vname accu ->
          try
            let substitued_param =
              apply_substitutions_list_on_formal_param_vname
                inh_from_mod param_vname substs in
            substitued_param :: accu
          with
          | Not_found ->
              (* Case where the substitution replaced the formal parameter by
                 "Self". In this case, the dependency on the parameter present
                 in the inherited species disappears in the current species. *)
              accu)
        found_meth.Env.TypeInformation.ad_used_species_parameter_tys [] in
    (* The second step deals with the [ai_dependencies_from_params]. The
       computation done on the body of our method may be biased because
       methods of same names but belonging to different formal collection
       parameters instanciated by a same effective collection will appear
       only once.
       For instance, modifing Couple of the above example:
       species Couple (S is Simple, T is Simple) =
         signature morph: S -> T;
         let equiv(e1, e2) =
           let _to_force_usage = S!equal in
           T!equal (!morph (e1), !morph (e2));
       end
;;
       T!equal and S!equal will be mapped onto only one G!equal.
       At this point, we do not want to substitute because information
       contained in the [ai_dependencies_from_params] is to big and too complex
       and takes benefits of sharing. Substituting inside would be long and
       break this sharing.
       Instead, we take benefits that all the required dependencies are already
       in [ai_dependencies_from_params], but not with the right number of
       occurrences (hence order) compared to what is expected in the inherited
       method generator.

       To fix this, we will recover the inherited method's dependencies on
       parameters scheme (i.e. which parameter, which methods) and build a
       new [ai_dependencies_from_params] with the same scheme but replacing
       information dealing with the formal parameters of the inherited method
       generator by the dependencies information computed in the current
       species for the corresponding effective parameters used to instanciate
       the formal ones. *)
    let new_deps_on_params_reved =
      List.fold_left
        (fun accu_deps_on_params
            (inh_spe_param, (Env.ODFP_methods_list inh_ordered_meths)) ->
          try
            (* [inh_spe_param] : the instanciated formal parameter of the
               inherited species. *)
            (* [inh_ordered_meths] : the ordered list of methods of the
               formal parameters the inherited had dependencies on. *)
            (* We must find the corresponding argument that was used in the
               current species to instanciate this formal collection
               parameter. The stuff we search is then the bucket in
               [non_mapped_deps] having the same name than what was used during
               substitution to replace [inh_spe_param]. *)
            let instantiating_subst = find_subst_of inh_spe_param substs in
            (match instantiating_subst with
             | Env.SK_collection_by_collection (_, Types.SBRCK_self) ->
                 (* Instanciation was not done by a species parameter. Make so
                    that we go in the exception handler that handles the case
                    where instanciation was done by a toplevel species or
                    collection. In effect, as well as the dependency on the
                    parameter's carrier disappeared, it's the same thing about
                    its methods that are now methods of "Self" and these
                    methods of "Self" are taken into account by the minimal
                    Coq/Dedukti typing environment. *)
                 raise Not_found
             | Env.SK_collection_by_collection
                 (_, Types.SBRCK_coll effective_instanciater) ->
                 (* Now we know that the currently processed formal argument was
                    instanciated by the rightmost part of the substitution
                    [instantiating_subst].
                    We must recover the list of methods of
                    [effective_instanciater] i.e. in the current species
                    dependencies information to pick inside. *)
                   let (param_of_instanciater,
                        (Env.ODFP_methods_list meths_of_instanciater)) =
                     Handy.list_find_custom_eq
                       (fun ty_col (param, _) ->
                         match param with
                          | Env.TypeInformation.SPAR_in (_, _, _) ->
                              (* We do not deal here with "IN" parameters. *)
                              false
                          | Env.TypeInformation.SPAR_is
                                (prm_ty_col, _, _, _, _) ->
                              prm_ty_col = ty_col)
                       effective_instanciater non_mapped_deps in
                   (* We now must pick in [meths_of_instanciater] the methods
                      having the same name in the inherited dependencies
                      information and put them according the same layout than
                      in the inherited dependencies information. We will finaly
                      reconstruct a bucket for [param_of_instanciater] with
                      this new list. *)
                   let new_meths =
                     List.fold_right
                       (fun (meth_name_in_inherited, _) accu ->
                         (* Let's find the same method name in the current
                            species and current collection parameter
                            dependencies information. *)
                         try
                           let m =
                             List.find
                               (fun (n, _) -> n = meth_name_in_inherited)
                               meths_of_instanciater in
                           m :: accu
                         with Not_found ->
                           (* The method appearing in the "inherited"
                              dependencies is not present in the species that
                              inherits. This can arise for instance when
                              computing mapping for "partial" dependencies used
                              for the record type. So, in this case, just
                              ignore the method. *)
                           accu)
                       inh_ordered_meths [] in
                     (param_of_instanciater, (Env.ODFP_methods_list new_meths))
                     :: accu_deps_on_params
             | Env.SK_ident_by_expression (_, _, expr_desc) ->
                 (* We must be in the case of a "IN" parameter. So we know that
                    during inheritance, the "IN" parameter of the inherited
                    species has been instantiated by the expression [expr_desc].
                    We must check if this expression contains identifiers that
                    are entity parameters of the inheriting (current) species.
                    If so, then we must pick in our species the dependency
                    information for each entity parameter and map it on the
                    one computed for the inherited species.
                    We try to extract from the instantiation expression the
                    [vnames] (if some) that could be entity parameters. If
                    this fails, we jump in the handler dealing with
                    instantiations that are not done with parameters. *)
                 let expr_identifiers =
                   Parsetree_utils.get_free_local_vnames_from_expr_desc
                     expr_desc in
                 List.fold_left
                   (fun accu_entity_prms expr_id ->
                     (* Now, we wonder if this identifier belongs to our
                        (i.e. the inheriting species) entity parameters. *)
                     try
                       let (param_of_instanciater,
                            (Env.ODFP_methods_list meths_of_instanciater)) =
                         Handy.list_find_custom_eq
                           (fun possible_entity_pname (param, _) ->
                             match param with
                             | Env.TypeInformation.SPAR_in (n, _, _) ->
                                 n = possible_entity_pname
                             | Env.TypeInformation.SPAR_is (_, _, _, _, _) ->
                                 (* We do not deal here with "IS" parameters. *)
                                 false)
                           expr_id non_mapped_deps in
                       (* Right, we found that the identifier is really one of
                          our entity parameters.
                          Since we are in the case of an entity parameter, if
                          there is a dependency, there will be only 1 method in
                          the list, wearing the same name than the entity
                          parameter itself.
                          Hence, if [inh_ordered_meths] is not empty, we just
                          reconstruct a bucket for [param_of_instanciater] with
                          exactly the list of dependencies computed in our
                          species. *)
                       let new_meths =
                         (match inh_ordered_meths with
                         | [] -> []
                         | [ _ ] -> meths_of_instanciater
                         | _ -> assert false) in
                       (param_of_instanciater,
                        (Env.ODFP_methods_list new_meths)) ::
                       accu_entity_prms
                     with Not_found -> accu_entity_prms)
                   accu_deps_on_params
                   expr_identifiers
            )
          with Not_found ->
            (* If we didn't find the collection used to instanciate among the
               species parameters that's because the instanciation was done
               by a toplevel collection or species or, for entity parameter,
               by an expression not involving entity parameters of the
               inheriting species. *)
            accu_deps_on_params)
        []
        found_meth.Env.TypeInformation.ad_dependencies_from_parameters in
    (* Since we may have detected that some instanciations were done with
       toplevel species/collections and not a species parameter, we may have
       some [None]s in the list. Then just forget them to get the list
       containing really only stuff related to species parameters. *)
    let new_deps_on_params = List.rev new_deps_on_params_reved in
    (new_used_species_parameter_tys, new_deps_on_params)
    end)
  else
    (* The method is not inherited, so leave the dependencies as they were
       naturally computed. *)
    (non_mapped_used_species_parameter_tys, non_mapped_deps)
;;



(* ************************************************************************* *)
(*   environment_kind ->                                                     *)
(*   abstractions_comput_context ->                                          *)
(*     Env.TypeInformation.species_field list -> field_abstraction_info list *)
(** {b Descr} : Wrapper above [_compute_abstractions_for_fields] that
    returns the dependencies on species parameters once merged and sorted.
    This avoid all the language backends to have to do this work since the
    exploded form of the dependencies (i.e. "from type", "from body", "from
    completion" etc is only something needed during internal computation).

    {b Exported} : Yes.                                                      *)
(* ************************************************************************* *)
let compute_abstractions_for_fields env ctx fields =
  let internal_abstractions =
    __compute_abstractions_for_fields env ctx fields in
  (* Now convert the internal form of the abstractions information into the
     public one. Make a local function dealing with one binding. We'll map it
     onto the list of internal abstractions and calling it several times on
     "let rec" fields. *)
  let process_one_field from name iai =
      let all_deps_from_params =
        merge_abstraction_infos
          iai.iai_dependencies_from_params_via_body
          (merge_abstraction_infos
             iai.iai_dependencies_from_params_via_type
             iai.iai_dependencies_from_params_via_completions) in
      (* Build the dependencies used to generate the record type
         parameters, i.e. [DIDOU] on ([TYPE]). Naturally contains
         [TYPE]. *)
      let for_record_ty_deps_from_params =
        complete_dependencies_from_params_rule_didou
          ~current_unit: ctx.acc_current_unit
          ~via_body: (make_empty_param_deps ctx.acc_species_parameters_names)
          ~via_type: iai.iai_dependencies_from_params_via_type
          ~initial_to_complete: iai.iai_dependencies_from_params_via_type in
      let sorted_deps_from_params =
        Dep_analysis.order_species_params_methods all_deps_from_params in
      let sorted_for_record_ty_deps_from_params =
        Dep_analysis.order_species_params_methods
          for_record_ty_deps_from_params in
      (* Remap computed dependencies onto the inherited parent's scheme if
         the method is inherited. *)
      let (mapped_used_species_parameter_tys, mapped_deps) =
        remap_dependencies_on_params_for_field
          env ctx from name iai.iai_used_species_parameter_tys
          sorted_deps_from_params in
      let (_, mapped_for_record_ty_deps_from_params) =
        remap_dependencies_on_params_for_field
          env ctx from name iai.iai_used_species_parameter_tys
          sorted_for_record_ty_deps_from_params in
      (* Build the final [abstraction_info]. *)
      let abstraction_info = {
        Env.TypeInformation.ad_used_species_parameter_tys =
          mapped_used_species_parameter_tys ;
        Env.TypeInformation.ad_raw_dependencies_from_params =
          sorted_deps_from_params ;
        Env.TypeInformation.ad_dependencies_from_parameters = mapped_deps ;
        Env.TypeInformation.ad_dependencies_from_parameters_in_type =
          mapped_for_record_ty_deps_from_params ;
        Env.TypeInformation.ad_min_coq_env = iai.iai_min_coq_env ;
        Env.TypeInformation.ad_min_dk_env = iai.iai_min_dk_env } in
      (name, abstraction_info) in
    (* Really do the job. And *do not* fold_left otherwise bindings will be
       reversed in the list. *)
    List.fold_right
      (fun (field, variadic_iai) accu ->
        match field with
        | Env.TypeInformation.SF_sig (from, name, _)
        | Env.TypeInformation.SF_let (from, name, _, _, _, _, _, _)
        | Env.TypeInformation.SF_theorem (from, name, _, _, _, _)
        | Env.TypeInformation.SF_property (from, name, _, _, _) ->
            let iai =
              match variadic_iai with
              | VD_One v -> v | VD_More _ -> assert false in
            (process_one_field from name iai) :: accu
        | Env.TypeInformation.SF_let_rec fields ->
            let iais =
              match variadic_iai with
              | VD_One _ -> assert false | VD_More vs -> vs in
            let fields' =
              List.map2
                (fun (from, name, _, _, _, _, _, _) i ->
                  process_one_field from name i)
                fields iais in
            fields' @ accu)
      internal_abstractions
      [(* Initial accu is empty. *)]
;;
