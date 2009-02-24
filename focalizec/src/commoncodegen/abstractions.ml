(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)
let debug_flg = ref false ;;


(* $Id: abstractions.ml,v 1.65 2009-02-24 16:24:03 pessaux Exp $ *)


(* ******************************************************************** *)
(** {b Descr} : Describes if the argument passed to the function
    [compute_lambda_liftings_for_field] is the body of a "let", "logical
    let" or of a "property/theorem". This allows the function to process
    at once the case of the liftings computation for expressions,
    propositions and proofs.

    {b Rem} : Exported outside this module.                             *)
(* ******************************************************************** *)
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
(** {b Descr} Serves to pass to [complete_dependencies_from_params] the
    code generation environment of the current language backend. We need this
    since to apply rule [PRM], wee need to recover the dependencies on
    species parameter of some species. This information is present in all the
    code generation environments but these environments have different types.
    Hence, when we will need to access an environment, with this 2
    constructors, we will know its type and which primitives to use.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
type environment_kind =
  | EK_ml of Env.MlGenEnv.t
  | EK_coq of Env.CoqGenEnv.t
;;



(* *********************************************************************** *)
(** {b Descr} Helper that creates the structure denoting an empty species
    parameters dependencies set. The "empty" dependencies cannot simple be
    [] because all our "union" functions on dependencies rely on a list of
    sets with 1 set for each species parameter name. So we create our
    initial accumulator as the list mapping each species parameter name
    onto the empty dependencies set.

    {b Rem}: Exported outside this module.                                 *)
(* *********************************************************************** *)
let make_empty_param_deps species_parameters_names =
  List.fold_right
    (fun species_param accu ->
      (species_param, Parsetree_utils.ParamDepSet.empty) :: accu)
    species_parameters_names
    []
;;



(** {b Descr} : Applies a list of substitution on a [Parsetree.vname].
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

    {b Rem} : Not exported outside this module.
    Raises [Not_found] if the substitution replace a parameter by "Sefl".   *)
let apply_substitutions_list_on_formal_param_vname pmodname pvname substs =
  (* First, convert the species parameter's name into a [string] to make
     comparison easier. *)
  let pvname_as_string = Parsetree_utils.name_of_vname pvname in
  let substed_pvname_as_string =
    List.fold_left
      (fun accu_vname ((c1_mod_name, c1_spe_name), c2) ->
        match c2 with
         | Types.SBRCK_coll (_, x) ->
             (* By construction, the compilation unit name of c2 should always
                be the current compilation unit. *)
             if c1_mod_name = pmodname && c1_spe_name = accu_vname then x
             else accu_vname
         | Types.SBRCK_self ->
             if c1_mod_name = pmodname && c1_spe_name = accu_vname then
               (* Substituting a formal collection parameter make the dependency
                  on the parameter disepear. In effect, in this case, instead of
                  depending on the collection parameter carrier, we depend on
                  OUR carrier. And this is caught by the minimal Coq typing
                  environment. So this parameter purely disapears. *)
               raise Not_found
             else accu_vname)
      pvname_as_string
      substs in
  (* Since during the substitutions we used [string]s we must finally convert
     the result back to a [vname]. Because collection names are always
     capitalized, we convert into a [Vuident]. *)
  Parsetree.Vuident substed_pvname_as_string
;;



(** Must be called on a method about which we are sure is it inherited ! *)
let find_most_recent_parent from_history =
  match from_history.Env.fh_inherited_along with
   | [] -> assert false
   | [_] ->
       (* We must take the initial apparition since the method has been
          inherited only by one inheritance step. *)
       from_history.Env.fh_initial_apparition
   | _ :: ((parent_mod, parent_spe), _, _) :: _ ->
       (* We skip the first element of the list since it represent the species
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
          | Parsetree.ANTI_non_relevant
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
                | Parsetree.ANTI_non_relevant
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
          | Parsetree.ANTI_non_relevant
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
(* current_unit: Types.fname ->                                              *)
(*   current_species: Parsetree.qualified_species ->                         *)
(*     (Parsetree.vname * Context.species_param_kind) list ->                *)
(*       Dep_analysis.name_node list ->                                      *)
(*       Parsetree.vname -> field_body_kind -> Types.simple_types            *)
(*           ((Parsetree.vname list) *                                       *)
(*            (Parsetree.vname * Parsetree_utils.DepNameSet.t) list *        *)
(*            (Dep_analysis.name_node * Dep_analysis.dependency_kind) list * *)
(*            (Dep_analysis.name_node * Dep_analysis.dependency_kind) list)  *)
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

    {b Rem} : Exported oustide this module.                                  *)
(* ************************************************************************* *)
let compute_lambda_liftings_for_field ~current_unit ~current_species
    species_parameters_names dependency_graph_nodes name body my_type
    opt_term_pr all_my_fields =
  (* Get all the methods we directly decl-depend on. They will   *)
  (* lead each to an extra parameter of the final OCaml function *)
  (* (lambda-lifing). Get the methods we directly def-depend.    *)
  (* They will be ignored for OCaml but used for Coq.            *)
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
                | Parsetree.TP_measure (expr, _, pr) ->
                    (begin
                    let deps1 =
                      Param_dep_analysis.param_deps_expr
                        ~current_species
                        (species_param_name, species_param_meths) expr in
                    let deps2 =
                      Param_dep_analysis.param_deps_proof
                        ~current_species
                        (species_param_name, species_param_meths) pr in
                         Parsetree_utils.ParamDepSet.union deps1 deps2
                    end)
                | _ -> failwith "todo 123") in
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
     method of the parameterss are used, we will never see that the parameter
     carriers are indeed used to quantify variables.
     Example:
       species Test (A is Setoid, B is Setoid) inherits Setoid =
         theorem b_is_snd_pair: all a in A, all b in B, true
         proof: assumed {* *} ;
       end ;;
     The body of [b_is_snd_pair] doesn't call any method of A or B. However,
     carriers of A and B are used to quantiy a and b. *)
  (match my_type with
   | FTK_computational t ->
       carriers_appearing_in_types := Types.get_species_types_in_type t
   | FTK_logical lexpr ->
       let params_carriers =
         get_species_types_in_type_annots_of_logical_expr lexpr in
       carriers_appearing_in_types := params_carriers) ;
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
        | _ -> failwith "todo 124") ;
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
    dependencies_from_params_in_bodies ;
  (* Same thing for the methods of ourselves we decl-depend. Note that if we
     have a decl-dependency on "rep" then we do not need to inspect its
     structure to know if it contains references to some species parameter
     types since this means that the "rep" is still kept abstract. *)
  List.iter
    (fun (node, _) ->
      if node.DepGraphData.nn_name <> (Parsetree.Vlident "rep") then
        begin
        let st_set =
          Types.get_species_types_in_type node.DepGraphData.nn_type in
        carriers_appearing_in_types :=
          Types.SpeciesCarrierTypeSet.union
            st_set !carriers_appearing_in_types ;
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
        end)
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
        Types.SpeciesCarrierTypeSet.union st_set !carriers_appearing_in_types ;
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
    def_children ;
  (* Now compute the set of species parameters types used in the types of the
     methods comming from the species parameters that the current field uses.
     This information is required for Coq since they will lead to extra args
     of type "Set". *)
(* [Unsure] Ne garder seulement les paramètres en "is" ? *)
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



(** We are at toplevel, not in the context of a species, so there is no way
    to have dependencies via species parameters. Hence, we just need to matter
    about the other toplevel theorems and properties we depend on. *)
let compute_lambda_liftings_for_toplevel_theorem dependency_graph_nodes name =
  (* Get all the properties or theorems we directly decl-depend on. Get the
     properties or theorems we directly def-depend. *)
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
  (decl_children, def_children)
;;



(** For intermediate internal computation where we need to remind from where
    the found dependencies on species parameters come.

    {b Rem}: Not exported outside this module. *)
type internal_abstraction_info = {
  iai_used_species_parameter_tys : Parsetree.vname list ;
  (** Dependencies found via [BODY] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  iai_dependencies_from_params_via_body :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
  list ;
  (** Dependencies found via [TYPE] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  iai_dependencies_from_params_via_type :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
  list ;
  (** Dependencies found via only [PRM]. Obviously they are all present in
      the set below ([iai_dependencies_from_params_via_completions]). *)
  iai_dependencies_from_params_via_PRM :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
       Parsetree_utils.ParamDepSet.t)
  list ;
  (** Other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of definition
      72 page 153 of Virgile Prevosto's Phd. *)
  iai_dependencies_from_params_via_completions :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
  list ;
  iai_min_coq_env : MinEnv.min_coq_env_element list
} ;;



(** {b Rem}: Exported outside this module. *)
type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  (** Dependencies on species parameters' methods. They are the union of:
        - dependencies found via [BODY] of definition 72 page 153 of Virgile
          Prevosto's Phd,
        - dependencies found via [TYPE] of definition 72 page 153 of Virgile
          Prevosto's Phd,
        - other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of
          definition 72 page 153 of Virgile Prevosto's Phd + those found
          by the missing rule in Virgile Prevosto's Phd that temporarily
          named [DIDOU]. *)
  ai_dependencies_from_params :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Env.ordered_methods_from_params)  (** The set of methods we depend on. *)
  list ;
  (* Dependencies used to generate the record type's parameters. It only
     contains dependencies obtained by [TYPE] and [DIDOU]. *)
  ai_dependencies_from_params_for_record_type :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Env.ordered_methods_from_params)  (** The set of methods we depend on
                                           only through types and completion. *)
  list ;
  ai_min_coq_env : MinEnv.min_coq_env_element list
} ;;



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

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let merge_abstraction_infos ai1 ai2 =
  List.map2
    (fun (prm1, deps1) (prm2, deps2) ->
      (* A few asserts to ensure the compiler is fine. *)
      assert (prm1 = prm2) ;
      let deps = Parsetree_utils.ParamDepSet.union deps1 deps2 in
      (prm1, deps))
    ai1 ai2
;;



(** Not exported. Only for internal intermediate computation where we need to
  remind from which rules the found dependencies come from. *)
type internal_field_abstraction_info =
  | IFAI_sig of
      (Env.TypeInformation.sig_field_info * internal_abstraction_info)
  | IFAI_let of
      (Env.TypeInformation.let_field_info * internal_abstraction_info)
  | IFAI_let_rec of
      (Env.TypeInformation.let_field_info * internal_abstraction_info) list
  | IFAI_theorem of
      (Env.TypeInformation.theorem_field_info * internal_abstraction_info)
  | IFAI_property of
      (Env.TypeInformation.property_field_info * internal_abstraction_info)
;;



(** Exported. *)
type field_abstraction_info =
  | FAI_sig of (Env.TypeInformation.sig_field_info * abstraction_info)
  | FAI_let of (Env.TypeInformation.let_field_info * abstraction_info)
  | FAI_let_rec of (Env.TypeInformation.let_field_info * abstraction_info) list
  | FAI_theorem of (Env.TypeInformation.theorem_field_info * abstraction_info)
  | FAI_property of (Env.TypeInformation.property_field_info * abstraction_info)
;;



(** May return None is the searched field is "rep" since it may not be
    defined. *)
let find_field_abstraction_by_name name abstractions =
  let rec rec_find = function
    | [] ->
        if name = (Parsetree.Vlident "rep") then None
        else assert false
    | h :: q ->
        match h with
         | IFAI_sig ((_, n, _), abstraction_info) 
         | IFAI_let ((_, n, _, _, _, _, _, _), abstraction_info)
         | IFAI_theorem ((_, n, _, _, _, _) , abstraction_info)
         | IFAI_property ((_, n, _, _, _), abstraction_info) ->
             if n = name then Some abstraction_info else rec_find q
         | IFAI_let_rec l ->
             (begin
             try
               let (_, abstraction_info) =
                 List.find (fun ((_, n, _, _, _, _, _, _), _) -> n = name) l in
               Some abstraction_info
             with Not_found -> rec_find q
             end) in
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

    {b Rem} : Not exported outside this module.                            *)
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
         | Parsetree_utils.SPE_Species (eff_arg_qual_vname, _) ->
             (begin
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
             end))
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

    {b Rem} : Not exported outside this module.                              *)
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
    | (p, d) :: q ->
        (begin
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
        end) in
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
  process_one_deps_set deps_in_type ;
  (* Now, process the second set. *)
  process_one_deps_set deps_via_compl ;
  (* Just recover the parameters names. *)
(* [Unsure] Ne garder seulement les paramètres en "is" ? *)
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



(* Returns an extension, not an union. *)
let complete_dependencies_from_params_rule_didou ~current_unit ~via_body
    ~via_type ~via_completion  =
  (* Join the 3 dependencies sets to lookup for fixpoint in only 1 set. *)
  let found_dependencies_from_params =
    merge_abstraction_infos
      via_body (merge_abstraction_infos via_type via_completion) in
  (* We create the set that will be the final completion. Hence it at least
     contains the dependencies initially found in the [via_completion]. *)
  let new_via_completion = ref via_completion in
  let changed = ref true in
  while !changed do
    changed := false ;   (* Reset the fixpoint reached signal. *)
    new_via_completion :=
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
                               DepGraphData.DcDK_from_type)) -> true
                         | (_,
                            (DepGraphData.DK_decl
                               DepGraphData.DcDK_from_body)) -> false
                         | (_,
                            (DepGraphData.DK_decl
                               DepGraphData.DcDK_from_term_proof)) -> false
                         | (_, (DepGraphData.DK_def _)) -> false)
                       my_node.DepGraphData.nn_children
                   with Not_found -> []  (* No children at all. *)) in
                 List.iter
                   (fun (node, _) ->
                     if node.DepGraphData.nn_name <>
                        (Parsetree.Vlident "rep") then
                       (begin
                       (* We found a method that must be possibly added if it is
                          not already present in the already found dependencies
                          and not in the already added dependencies. *)
                       let mkind =
                         Param_dep_analysis.guess_method_computational_or_logical
                           node.DepGraphData.nn_name
                           None
                           spe_meths in
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
                                  (Types.SBRCK_coll spe_par_name)
                                  lexpr in
                              Parsetree_utils.DETK_logical lexpr' in
                       let elem = (node.DepGraphData.nn_name, mkind) in
                       if not
                           (Parsetree_utils.ParamDepSet.mem elem meths_old) &&
                          not (Parsetree_utils.ParamDepSet.mem elem !accu) then
                         (begin
                         (* We must add the method into the accu since it was
                            never seen before. *)
                         accu := Parsetree_utils.ParamDepSet.add elem !accu ;
                         (* For the fixpoint, we say that it is not yet
                            reached. *)
                         changed := true
                         end)
                       end))
                   decl_children)
               meths_old ;
             (* Return the new set of added methods. *)
             (param, !accu))
      !new_via_completion
      found_dependencies_from_params
  done ;
  !new_via_completion
;;





(* ************************************************************************ *)
(* environment_kind -> current_unit:Parsetree.module_name ->                *)
(*   Env.TypeInformation.species_param list ->                              *)
(*    (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t)   *)
(*       list ->                                                            *)
(*      (Env.TypeInformation.species_param * Parsetree_utils.ParamDepSet.t) *)
(*         list                                                             *)
(* {b Args}:
     - [dependencies_from_params] Those computer by all the other rules.

   {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************ *)
let complete_dependencies_from_params_rule_PRM env ~current_unit
    species_parameters starting_dependencies_from_params =
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
     Cq' as third argument, we would get the pair:
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
        (match env with
         | EK_ml env ->
             (* Just extract the components we need. *)
             let (a, b, _, _) =
               Env.MlGenEnv.find_species
                 ~loc: Location.none ~current_unit sprim env in
             (a, b)
         | EK_coq env -> 
             (* Just extract the components we need. *)
             let (a, b, _, _) =
               Env.CoqGenEnv.find_species
                 ~loc: Location.none ~current_unit sprim env in
             (a, b)) in
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
                  [starting_dependencies_from_params]), ... *)
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
                   cpprim starting_dependencies_from_params in
               Parsetree_utils.ParamDepSet.fold
                 (fun z accu_deps_for_zs ->
                   (* Forall z, we must search the set of methods, y, on which
                      z depends on in S' via [formal_name] ...
                      So, first get z's dependencies information. *)
                   let z_priv_meth_info =
                     List.find
                       (fun m_info -> m_info.Env.mi_name = (fst z))
                       sprim_meths_abstr_infos in
                   let z_dependencies =
                     z_priv_meth_info.Env.mi_dependencies_from_parameters in
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



(** Implements rules [TYPE], [DEF-DEP], [UNIVERSE] and [PRM] of the
    definition 72 page 153 of Virgile Prevosto's Phd. *)
let complete_dependencies_from_params env ~current_unit ~current_species
    seen_abstractions species_parameters def_children universe type_kind =
  (* Rule [TYPE] possible only if a logical expression is provided. In effect,
     in a type scheme, species_parameters can never appear since it is a
     ML-like type. Furthermore, even in case pf termination proof, we have
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
            st_set !carriers_appearing_in_types ;
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
                 abstr_info.iai_used_species_parameter_tys ;
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
  (* Join all the found dependencies in a unique bunch so that
     [complete_dependencies_from_params_rule_PRM] will have 1 bunch to know
     which ones have already be found. *)
  let all_found_deps_until_now =
    merge_abstraction_infos
      dependencies_from_params_via_type dependencies_from_params_via_compl2 in
  (* Get the found dependencies via rule [PRM]. *)
  let dependencies_from_params_via_PRM =
    complete_dependencies_from_params_rule_PRM
      env ~current_unit species_parameters all_found_deps_until_now in
  (* Merge the completions. *)
  let dependencies_from_params_via_compl3 =
    merge_abstraction_infos
      dependencies_from_params_via_compl2 dependencies_from_params_via_PRM in
 (dependencies_from_params_via_type, (* The dependencies induces by only the
                                        [TYPE] rule. *)
  dependencies_from_params_via_PRM, (* The dependencies induces by only the
                                       [PRM] rule. *)
  dependencies_from_params_via_compl3, (* The union of dependencies coming
                                          from [DEF-DEP], [UNIVERSE] and
                                          [PRM]. *)
  !carriers_appearing_in_types)
;;



(* ************************************************************************** *)
(** {b Descr}:
    To be usable for OCaml generation, the [with_def_deps_n_term_pr] flag
    enables to forget the def-dependencies and their implied
    transitive decl-dependencies and also dependencies induced by recursive
    functions termination proofs. In effect, in OCaml, only decl-dependencies
    are relevant and since there is no termination proof, dependencies induced
    by them must be forgotten

   {b Rem}: Not exported outside this module.                                 *)
(* ************************************************************************** *)
let __compute_abstractions_for_fields ~with_def_deps_n_term_pr env ctx fields =
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
         | Env.TypeInformation.SF_sig ((_, _, sch) as si) ->
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
                 ctx.Context.scc_species_parameters_names in
             let as_set =
               List.filter
                 (fun species_param_name ->
                   let as_string =
                     Parsetree_utils.name_of_vname species_param_name in
                   Types.SpeciesCarrierTypeSet.mem
                     (ctx.Context.scc_current_unit, as_string)
                     used_species_parameter_tys)
                 species_param_names in
             (* The "empty" dependencies cannot simple be [] because all our
                "union" functions on dependencies rely on a list of sets with 1
                set for each species parameter name. So we create our initial
                accumulator as the list mapping each species parameter name
                onto the empty dependencies set. *)
             let empty_deps =
               make_empty_param_deps ctx.Context.scc_species_parameters_names in
             let abstr_info = {
               iai_used_species_parameter_tys = as_set ;
               iai_dependencies_from_params_via_body = empty_deps ;
               iai_dependencies_from_params_via_type = empty_deps ;
               iai_dependencies_from_params_via_PRM = empty_deps ;
               iai_dependencies_from_params_via_completions = empty_deps ;
               iai_min_coq_env = [] } in
             (IFAI_sig (si, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let
                ((_, name, _, sch, body, _, _, _) as li) ->
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
                 ~current_unit: ctx.Context.scc_current_unit
                 ~current_species: ctx.Context.scc_current_species
                 ctx.Context.scc_species_parameters_names
                 ctx.Context.scc_dependency_graph_nodes name
                 body_as_fbk (FTK_computational method_ty) None fields in
             (* Compute the visible universe of the method. *)
             let universe =
               VisUniverse.visible_universe
                 ~with_def_deps_n_term_pr
                 ctx.Context.scc_dependency_graph_nodes decl_children
                 def_children in
             (* Complete the dependencies from species parameters info. By the
                way, we record the species parameters carrier appearing in the
                methods of self that were added during the completion phase. *)
             let (dependencies_from_params_in_type,
                  dependencies_from_params_via_prm,
                  dependencies_from_params_via_compl,
                  used_species_parameter_tys_in_meths_self_after_completion) =
               complete_dependencies_from_params
                 env ~current_unit: ctx.Context.scc_current_unit
                 ~current_species: ctx.Context.scc_current_species
                 abstractions_accu ctx.Context.scc_species_parameters_names
                 def_children universe (FTK_computational method_ty) in
             (* Extra completion by a transitive closure that was missing in
                Virgile Prevosto's Phd. *)
             let dependencies_from_params_via_didou =
               complete_dependencies_from_params_rule_didou
                 ~current_unit: ctx.Context.scc_current_unit
                 ~via_body: dependencies_from_params_in_body
                 ~via_type: dependencies_from_params_in_type
                 ~via_completion: dependencies_from_params_via_compl in
             (* Now, we complete the species parameters carriers seen by
                taking into account types of methods obtained by the
                completion of the dependencies on parameters achieved by
                [complete_dependencies_from_params]. *)
             let all_used_species_parameter_tys =
               complete_used_species_parameters_ty
                 ~current_unit: ctx.Context.scc_current_unit
                 ctx.Context.scc_species_parameters_names
                 used_species_parameter_tys_in_self_methods_bodies
                 used_species_parameter_tys_in_meths_self_after_completion
                 dependencies_from_params_in_type
                 dependencies_from_params_via_didou in
             (* Now, its minimal Coq typing environment. *)
             let min_coq_env =
               MinEnv.minimal_typing_environment universe fields in
             let abstr_info = {
               iai_used_species_parameter_tys = all_used_species_parameter_tys ;
               iai_dependencies_from_params_via_body =
                 dependencies_from_params_in_body ;
               iai_dependencies_from_params_via_type =
                 dependencies_from_params_in_type ;
               iai_dependencies_from_params_via_PRM =
                 dependencies_from_params_via_prm ;
               iai_dependencies_from_params_via_completions =
                 dependencies_from_params_via_didou ;
               iai_min_coq_env = min_coq_env } in
             (IFAI_let (li, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let_rec l ->
             let deps_infos =
               List.map
                 (fun ((_, name, _, sch, body, opt_term_pr, _, _) as li) ->
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
                       ~current_unit: ctx.Context.scc_current_unit
                       ~current_species: ctx.Context.scc_current_species
                       ctx.Context.scc_species_parameters_names
                       ctx.Context.scc_dependency_graph_nodes name
                       body_as_fbk (FTK_computational method_ty) opt_term_pr
                       fields in
                   (* Compute the visible universe of the method. *)
                   let universe =
                     VisUniverse.visible_universe
                       ~with_def_deps_n_term_pr
                       ctx.Context.scc_dependency_graph_nodes
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
                       env ~current_species: ctx.Context.scc_current_species
                       ~current_unit: ctx.Context.scc_current_unit
                       abstractions_accu ctx.Context.
                         scc_species_parameters_names
                       def_children universe (FTK_computational method_ty) in
                   (* Extra completion by a transitive closure that was missing
                      in Virgile Prevosto's Phd. *)
                   let dependencies_from_params_via_didou =
                     complete_dependencies_from_params_rule_didou
                       ~current_unit: ctx.Context.scc_current_unit
                       ~via_body: dependencies_from_params_in_bodies
                       ~via_type: dependencies_from_params_in_type
                       ~via_completion: dependencies_from_params_via_compl in
                   (* Now, we complete the species parameters carriers seen
                      by taking into account types of methods obtained by
                      the completion of the dependencies on parameters achieved
                      by [complete_dependencies_from_params]. *)
                   let all_used_species_parameter_tys =
                     complete_used_species_parameters_ty
                       ~current_unit: ctx.Context.scc_current_unit
                       ctx.Context.scc_species_parameters_names
                       used_species_parameter_tys_in_self_methods_bodies
                       used_species_parameter_tys_in_meths_self_after_completion
                       dependencies_from_params_in_type
                       dependencies_from_params_via_didou in
                   (* Now, its minimal Coq typing environment. *)
                   let min_coq_env =
                     MinEnv.minimal_typing_environment universe fields in
                   let abstr_info = {
                     iai_used_species_parameter_tys =
                       all_used_species_parameter_tys ;
                     iai_dependencies_from_params_via_body =
                       dependencies_from_params_in_bodies ;
                     iai_dependencies_from_params_via_type =
                       dependencies_from_params_in_type ;
                     iai_dependencies_from_params_via_PRM =
                       dependencies_from_params_via_prm ;
                     iai_dependencies_from_params_via_completions =
                       dependencies_from_params_via_didou ;
                     iai_min_coq_env = min_coq_env } in
                   (li, abstr_info))
                 l in
             (IFAI_let_rec deps_infos) :: abstractions_accu
         | Env.TypeInformation.SF_theorem
             ((_, name, _, logical_expr, proof, _) as ti) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not
                  the complete set of dependencies. It must be completed to
                  fully represent the definition 72 page 153 from Virgile
                  Prevosto's Phd. *)
               let (used_species_parameter_tys_in_self_methods_bodies,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.Context.scc_current_unit
                   ~current_species: ctx.Context.scc_current_species
                   ctx.Context.scc_species_parameters_names
                   ctx.Context.scc_dependency_graph_nodes name
                   (FBK_proof (Some proof)) (FTK_logical logical_expr)
                   None fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps_n_term_pr
                   ctx.Context.scc_dependency_graph_nodes decl_children
                   def_children in
               (* Now, its minimal Coq typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_typing_environment universe fields in
               (* Complete the dependencies from species parameters info. By the
                  way, we record the species parameters carrier appearing in the
                  methods of self that were added during the completion
                  phase. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_prm,
                    dependencies_from_params_via_compl,
                    used_species_parameter_tys_in_meths_self_after_completion) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe (FTK_logical logical_expr) in
               (* Extra completion by a transitive closure that was missing in
                  Virgile Prevosto's Phd. *)
               let dependencies_from_params_via_didou =
                 complete_dependencies_from_params_rule_didou
                   ~current_unit: ctx.Context.scc_current_unit
                   ~via_body: dependencies_from_params_in_bodies
                   ~via_type: dependencies_from_params_in_type
                   ~via_completion: dependencies_from_params_via_compl in
               (* Now, we complete the species parameters carriers seen by
                  taking into account types of methods obtained by the
                  completion of the dependencies on parameters achieved by
                  [complete_dependencies_from_params]. *)
               let all_used_species_parameter_tys =
                 complete_used_species_parameters_ty
                   ~current_unit: ctx.Context.scc_current_unit
                   ctx.Context.scc_species_parameters_names
                   used_species_parameter_tys_in_self_methods_bodies
                   used_species_parameter_tys_in_meths_self_after_completion
                   dependencies_from_params_in_type
                   dependencies_from_params_via_didou in
               let abstr_info = {
                 iai_used_species_parameter_tys =
                   all_used_species_parameter_tys ;
                 iai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 iai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 iai_dependencies_from_params_via_PRM =
                   dependencies_from_params_via_prm ;
                 iai_dependencies_from_params_via_completions =
                   dependencies_from_params_via_didou ;
                 iai_min_coq_env = min_coq_env } in
               (IFAI_theorem (ti, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_property
             ((_, name, _, logical_expr, _) as pi) ->
               (* ATTENTION, the [dependencies_from_params_in_bodies] is not
                  the complete set of dependencies. It must be completed to
                  fully represent the definition 72 page 153 from Virgile
                  Prevosto's Phd. *)
               let (used_species_parameter_tys_in_self_methods_bodies,
                    dependencies_from_params_in_bodies,
                    decl_children, def_children) =
                 compute_lambda_liftings_for_field
                   ~current_unit: ctx.Context.scc_current_unit
                   ~current_species: ctx.Context.scc_current_species
                   ctx.Context.scc_species_parameters_names
                   ctx.Context.scc_dependency_graph_nodes name
                   (FBK_proof None) (FTK_logical logical_expr) None fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps_n_term_pr
                   ctx.Context.scc_dependency_graph_nodes decl_children
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
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe (FTK_logical logical_expr) in
               (* Extra completion by a transitive closure that was missing in
                  Virgile Prevosto's Phd. *)
               let dependencies_from_params_via_didou =
                 complete_dependencies_from_params_rule_didou
                   ~current_unit: ctx.Context.scc_current_unit
                   ~via_body: dependencies_from_params_in_bodies
                   ~via_type: dependencies_from_params_in_type
                   ~via_completion: dependencies_from_params_via_compl in
               (* Now, we complete the species parameters carriers seen by
                  taking into account types of methods obtained by the
                  completion of the dependencies on parameters achieved by
                  [complete_dependencies_from_params]. *)
               let all_used_species_parameter_tys =
                 complete_used_species_parameters_ty
                   ~current_unit: ctx.Context.scc_current_unit
                   ctx.Context.scc_species_parameters_names
                   used_species_parameter_tys_in_self_methods_bodies
                   used_species_parameter_tys_in_meths_self_after_completion
                   dependencies_from_params_in_type
                   dependencies_from_params_via_didou in
               (* Now, its minimal Coq typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_typing_environment universe fields in
               let abstr_info = {
                 iai_used_species_parameter_tys =
                   all_used_species_parameter_tys ;
                 iai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 iai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 iai_dependencies_from_params_via_PRM =
                   dependencies_from_params_via_prm ;
                 iai_dependencies_from_params_via_completions =
                   dependencies_from_params_via_didou ;
                 iai_min_coq_env = min_coq_env } in
               (IFAI_property (pi, abstr_info)) :: abstractions_accu)
      []      (* Initial empty abstractions accumulator. *)
      fields in
  (* Finally, put the list of abstractions in the right order, i.e. in the
     order of apparition of the fields in the species. *)
  List.rev reversed_abstractions
;;



let remap_dependencies_on_params_for_field env ctx from name
    non_mapped_used_species_parameter_tys non_mapped_deps =
  (* We first check if the method was inherited. Only if it is the case then
     we need to remap the computed dependencies on the parent's dependencies
     scheme. *)
  if from.Env.fh_initial_apparition <> ctx.Context.scc_current_species then
    (begin
    (* We recover most recent inherited method abstractions information. This
       should never fail since the species must exist before the current one
       is built. *)
    let (inh_from_mod, inh_from_sp) = find_most_recent_parent from in
    let (_, _, substs) = List.hd from.Env.fh_inherited_along in
    (* Just create a temporary ident from the original hosting species name. *)
    let fake_ident = {
      Parsetree.ast_loc = Location.none ;
      Parsetree.ast_doc = [] ;
      Parsetree.ast_type = Parsetree.ANTI_none ;
      Parsetree.ast_desc =
        (* If the hosting species is in the current compilation unit, then we
           make a fake LOCAL ident. Otherwise, a fake GLOBAL ident. *)
        if inh_from_mod = ctx.Context.scc_current_unit then
          Parsetree.I_local inh_from_sp
        else
          Parsetree.I_global
            (Parsetree.Qualified (inh_from_mod, inh_from_sp)) } in

if !debug_flg then
begin
Format.eprintf "Field: %a coming from: %a@."
  Sourcify.pp_vname name Sourcify.pp_ident fake_ident ;
end;

    (* Really get in the environment the information about the method. *)
    let original_hosting_species_meths =
      (match env with
       | EK_coq e ->
           (* Just keep the information about methods. *)
           let (_, ms, _, _) =
             Env.CoqGenEnv.find_species
               ~loc: Location.none
               ~current_unit: ctx.Context.scc_current_unit fake_ident e in
           ms
       | EK_ml e ->
           (* Just keep the information about methods. *)
           let (_, ms, _, _) =
             Env.MlGenEnv.find_species
               ~loc: Location.none
               ~current_unit: ctx.Context.scc_current_unit fake_ident e in
           ms) in
    (* We now must look for the inherited method name among the found
       information. *)
if !debug_flg then
begin
Format.eprintf "Searching for field: %a...@." Sourcify.pp_vname name ;
end;

    let found_meth =
      List.find
        (fun info -> info.Env.mi_name = name) original_hosting_species_meths in

if !debug_flg then
begin
Format.eprintf "Found.@." ;
end;

    (* Implicitely the collection parameters of the found species are
       hosted in the compilation unit where their species is hosted,
       i.e. [inh_from_mod]. *)
    (* The first step of the mapping addresses the
       [used_species_parameter_tys]. We must make sure that original
       parameters instanciated by the same effective parameter lead to
       several times the corresponding argument in the abstraction info.
       For instance, in:
         species Couple (S is Simple, T is Simple) =
           let equiv(e1, e2) = T!equal(!morph(e1), !morph(e2)) ;
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

if !debug_flg then
begin
Format.eprintf "For carriers, searching by what parameter formal parameter %a was instanciated.@."
  Sourcify.pp_vname param_vname ;
end;

          try
            let substitued_param =
              apply_substitutions_list_on_formal_param_vname
                inh_from_mod param_vname substs in
if !debug_flg then
begin
Format.eprintf "For carriers, found instanciated by effective parameter %a@."
  Sourcify.pp_vname substitued_param ;
end;
            substitued_param :: accu
          with
          | Not_found ->

if !debug_flg then
begin
Format.eprintf "For carriers, not found instanciated by an effective parameter@."
end;

              (* Case where the substitution replaced the formal parameter by
                 "Self". In this case, the dependency on the parameter present
                 in the inherited species disappears in the current species. *)
              accu)
        found_meth.Env.mi_used_species_parameter_tys [] in
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
         T!equal(!morph(e1), !morph(e2)) ;
       end ;;
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

if !debug_flg then
begin
Format.eprintf "Computing new_deps_on_params_as_option@."
end;


    let new_deps_on_params_as_option =
      List.map
        (fun (inh_spe_param, (Env.ODFP_methods_list inh_ordered_meths)) ->
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

if !debug_flg then
begin
Format.eprintf "For methods, looking for the effective param used to instanciate the formal " ;
(match inh_spe_param with
  | Env.TypeInformation.SPAR_in (_, _, _) -> Format.eprintf "IN (forget)"
  | Env.TypeInformation.SPAR_is (prm_ty_col, _, _, _, _) ->
      Format.eprintf "%a" Types.pp_type_collection prm_ty_col);
Format.eprintf " of the inherited species@."
end;

            let formal_instanciation =
              Handy.list_assoc_custom_eq
                (fun ty_col_to_replace param ->
                  (* [ty_col_to_replace] is the type collection mentionned
                     to be replace in the substitution. *)
                  (* [param] is the formal collection parameter in the
                     inherited species. *)
                  match param with
                   | Env.TypeInformation.SPAR_in (_, _, _) ->
                       (* We do not deal here with "IN" parameters. *)
                       false
                   | Env.TypeInformation.SPAR_is (prm_ty_col, _, _, _, _) ->
                       prm_ty_col = ty_col_to_replace)
                inh_spe_param substs in

if !debug_flg then
begin
Format.eprintf "Ok, found@." ;
end;

            (* Now we know that the currently processed formal argument was
               instanciated by the substitution [formal_instanciation]. *)
            let effective_instanciater =
              (match formal_instanciation with
               | Types.SBRCK_self ->
                   (* Instanciation was not done by a species parameter. Make
                      so that we go in the exception handler that handles the
                      case where instanciation was done by a toplevel species
                      or collection. In effect, as well as the dependency on
                      the parameter's carrier disappeared, it's the same thing
                      about its methods that are now methods of "Self" and
                      these methods of "Self" are taken into account by the
                      minimal Coq typing environment. *)
                   raise Not_found
               | Types.SBRCK_coll tc -> tc) in

if !debug_flg then
begin
Format.eprintf "... and instancier is %a@."
  Types.pp_type_collection effective_instanciater ;
end;


            (* Must recover the list of methods of [effective_instanciater] i.e.
               in the current species dependencies information to pick
               inside. *)
            let (param_of_instanciater,
                 (Env.ODFP_methods_list meths_of_instanciater)) =
              Handy.list_find_custom_eq
                (fun ty_col (param, _) ->
                  match param with
                   | Env.TypeInformation.SPAR_in (_, _, _) ->
                       (* We do not deal here with "IN" parameters. *)
                       false
                   | Env.TypeInformation.SPAR_is (prm_ty_col, _, _, _, _) ->
                       prm_ty_col = ty_col)
                effective_instanciater non_mapped_deps in

if !debug_flg then
begin
Format.eprintf
  "Methods of the instancier (i.e. in the current species) found via non yet mapped deps (i.e. those computed regularly):@." ;
List.iter (fun (n, _) -> Format.eprintf "%a " Sourcify.pp_vname n)
  meths_of_instanciater ;
Format.eprintf "@." ;
end;

            (* We now must pick in [meths_of_instanciater] the methods having
               the same name in the inherited dependencies information and put
               them according the same layout than in the inherited dependencies
               information. We will finaly reconstruct a bucket for
               [param_of_instanciater] with this new list. *)
            let new_meths =
              List.fold_right
                (fun (meth_name_in_inherited, _) accu ->
                  (* Let's fidn the same method name in the current species and
                     current collection parameter dependencies information. *)

if !debug_flg then
begin
Format.eprintf "Trying to get in the current species parameter (instancier), the method that was called %a in the inherited species@."
  Sourcify.pp_vname meth_name_in_inherited
end ;
		  try
let tmp =
                  List.find
                    (fun (n, _) ->
		      n = meth_name_in_inherited)
                    meths_of_instanciater
in
if !debug_flg then
begin
Format.eprintf "The method that was called %a in the inherited species was found@."
  Sourcify.pp_vname meth_name_in_inherited
end ;

tmp :: accu
		  with Not_found ->
		      (* The method appearing in the "inherited" dependencies
			 is not present in the species that inherits. This can
			 arise for instance when computing mapping for "partial"
			 dependencies used for the record type. So, in this
			 case, just ignore the method. *)
		      accu)
                inh_ordered_meths [] in
            Some (param_of_instanciater, (Env.ODFP_methods_list new_meths))
          with Not_found ->
            (* If we didn't find the collection used to instanciate among the
               species parameters that's because the instanciation was done
               by a toplevel collection or species or because the parameter
               is a IN parameter. *)
            None)
        found_meth.Env.mi_dependencies_from_parameters in
    (* Since we may have detected that some instanciations were done with
     toplevel species/collections and not a species parameter, we may have
       some [None]s in the list. Then juste forget them to get the list
       containing really only stuff related to species parameters. *)
    let new_deps_on_params =
      Handy.option_list_to_list new_deps_on_params_as_option in
    (new_used_species_parameter_tys, new_deps_on_params)
    end)
  else
    (* The method is not inherited, so leave the dependencies as they were
       naturally computed. *)
    (non_mapped_used_species_parameter_tys, non_mapped_deps)
;;



(** Wrapper above [_compute_abstractions_for_fields] that returns the
    dependencies on species parameters once merged and sorted. This avoid
    all the language backends to have to do this work since the exploded
    form of the dependencies (i.e. "from type", "from body", "from completion"
    etc is only something needed during internal computation. *)
let compute_abstractions_for_fields ~with_def_deps_n_term_pr env ctx fields =
  let internal_abstractions =
    __compute_abstractions_for_fields ~with_def_deps_n_term_pr env ctx fields in
  (* Now convert the internal form of the abstractions information into the
     public one. *)
  List.map
    (function
      | IFAI_sig (sig_field_info, iai) ->
          let all_deps_from_params =
            merge_abstraction_infos
              iai.iai_dependencies_from_params_via_body
              (merge_abstraction_infos
                 iai.iai_dependencies_from_params_via_type
                 iai.iai_dependencies_from_params_via_completions) in
          (* Build the dependencies used to generate the record type
             parameters. *)
          let pre_partial_deps_from_params =
            complete_dependencies_from_params_rule_didou
              ~current_unit: ctx.Context.scc_current_unit
              ~via_body:
                (make_empty_param_deps ctx.Context.scc_species_parameters_names)
              ~via_type: iai.iai_dependencies_from_params_via_type
              ~via_completion: iai.iai_dependencies_from_params_via_PRM in
          let partial_deps_from_params =
            merge_abstraction_infos
              pre_partial_deps_from_params
              iai.iai_dependencies_from_params_via_type in
          let sorted_deps_from_params =
            Dep_analysis.order_species_params_methods all_deps_from_params in
          let sorted_partial_deps_from_params =
            Dep_analysis.order_species_params_methods
              partial_deps_from_params in
          (* Remap computed dependencies onto the inherited parent's scheme if
             the method is inherited. *)
          let (from, name, _) = sig_field_info in
          let (mapped_used_species_parameter_tys, mapped_deps) =
            remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_deps_from_params in
          let (_, mapped_partial_deps_from_params) =
             remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_partial_deps_from_params in
          (* Build the final [abstraction_info]. *)
          let abstraction_info = {
            ai_used_species_parameter_tys = mapped_used_species_parameter_tys ;
            ai_dependencies_from_params = mapped_deps ;
            ai_dependencies_from_params_for_record_type =
              mapped_partial_deps_from_params ;
            ai_min_coq_env = iai.iai_min_coq_env } in
          FAI_sig (sig_field_info, abstraction_info)
      | IFAI_let (let_field_info, iai) ->
          let all_deps_from_params =
            merge_abstraction_infos
              iai.iai_dependencies_from_params_via_body
              (merge_abstraction_infos
                 iai.iai_dependencies_from_params_via_type
                 iai.iai_dependencies_from_params_via_completions) in
          (* Build the dependencies used to generate the record type
             parameters. *)
          let pre_partial_deps_from_params =
            complete_dependencies_from_params_rule_didou
              ~current_unit: ctx.Context.scc_current_unit
              ~via_body:
                (make_empty_param_deps ctx.Context.scc_species_parameters_names)
              ~via_type: iai.iai_dependencies_from_params_via_type
              ~via_completion: iai.iai_dependencies_from_params_via_PRM in
          let partial_deps_from_params =
            merge_abstraction_infos
              pre_partial_deps_from_params
              iai.iai_dependencies_from_params_via_type in
          let sorted_deps_from_params =
            Dep_analysis.order_species_params_methods all_deps_from_params in
          let sorted_partial_deps_from_params =
            Dep_analysis.order_species_params_methods
              partial_deps_from_params in
          (* Remap computed dependencies onto the inherited parent's scheme if
             the method is inherited. *)
          let (from, name, _, _, _, _, _, _) = let_field_info in
          let (mapped_used_species_parameter_tys, mapped_deps) =
            remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_deps_from_params in
          let (_, mapped_partial_deps_from_params) =
             remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_partial_deps_from_params in
          (* Build the final [abstraction_info]. *)
          let abstraction_info = {
            ai_used_species_parameter_tys = mapped_used_species_parameter_tys ;
            ai_dependencies_from_params = mapped_deps ;
            ai_dependencies_from_params_for_record_type =
              mapped_partial_deps_from_params ;
            ai_min_coq_env = iai.iai_min_coq_env } in
          FAI_let (let_field_info, abstraction_info)
      | IFAI_let_rec internal_infos ->
          let abstraction_infos =
            List.map
              (fun (let_field_info, iai) ->
                let all_deps_from_params =
                  merge_abstraction_infos
                    iai.iai_dependencies_from_params_via_body
                    (merge_abstraction_infos
                       iai.iai_dependencies_from_params_via_type
                       iai.iai_dependencies_from_params_via_completions) in
                (* Build the dependencies used to generate the record type
                   parameters. *)
                let pre_partial_deps_from_params =
                  complete_dependencies_from_params_rule_didou
                    ~current_unit: ctx.Context.scc_current_unit
                    ~via_body:
                    (make_empty_param_deps
                       ctx.Context.scc_species_parameters_names)
                    ~via_type: iai.iai_dependencies_from_params_via_type
                    ~via_completion: iai.iai_dependencies_from_params_via_PRM in
                let partial_deps_from_params =
                  merge_abstraction_infos
                    pre_partial_deps_from_params
                    iai.iai_dependencies_from_params_via_type in
                let sorted_deps_from_params =
                  Dep_analysis.order_species_params_methods
                    all_deps_from_params in
                let sorted_partial_deps_from_params =
                  Dep_analysis.order_species_params_methods
                    partial_deps_from_params in
                (* Remap computed dependencies onto the inherited parent's
                   scheme if the method is inherited. *)
                let (from, name, _, _, _, _, _, _) = let_field_info in
                let (mapped_used_species_parameter_tys, mapped_deps) =
                  remap_dependencies_on_params_for_field
                    env ctx from name iai.iai_used_species_parameter_tys
                    sorted_deps_from_params in
		let (_, mapped_partial_deps_from_params) =
		   remap_dependencies_on_params_for_field
		    env ctx from name iai.iai_used_species_parameter_tys
		    sorted_partial_deps_from_params in
                (* Build the final [abstraction_info]. *)
                let abstraction_info = {
                  ai_used_species_parameter_tys =
                    mapped_used_species_parameter_tys ;
                  ai_dependencies_from_params = mapped_deps ;
                  ai_dependencies_from_params_for_record_type =
                    mapped_partial_deps_from_params ;
                  ai_min_coq_env = iai.iai_min_coq_env } in
                (let_field_info, abstraction_info))
              internal_infos in
          FAI_let_rec abstraction_infos
      | IFAI_theorem (theorem_field_info, iai) ->
          let all_deps_from_params =
            merge_abstraction_infos
              iai.iai_dependencies_from_params_via_body
              (merge_abstraction_infos
                 iai.iai_dependencies_from_params_via_type
                 iai.iai_dependencies_from_params_via_completions) in
          (* Build the dependencies used to generate the record type
             parameters. *)
          let pre_partial_deps_from_params =
            complete_dependencies_from_params_rule_didou
              ~current_unit: ctx.Context.scc_current_unit
              ~via_body:
                (make_empty_param_deps ctx.Context.scc_species_parameters_names)
              ~via_type: iai.iai_dependencies_from_params_via_type
              ~via_completion: iai.iai_dependencies_from_params_via_PRM in

let (_, n, _, _, _, _) = theorem_field_info in
(*                                  "tau_rbac_acc_r_correct" *)
debug_flg := (n = Parsetree.Vlident "additive_compatibility") ;
if !debug_flg then
begin
Format.eprintf "Theorem: %a of species %a@."
  Sourcify.pp_vname n
 Sourcify.pp_qualified_species ctx.Context.scc_current_species ;
Format.eprintf "[TYPE]@." ;
Dep_analysis.debug_print_dependencies_from_parameters
  iai.iai_dependencies_from_params_via_type ;
Format.eprintf "[PRM]@." ;
Dep_analysis.debug_print_dependencies_from_parameters
  iai.iai_dependencies_from_params_via_PRM ;
Format.eprintf "[DIDOU]@." ;
Dep_analysis.debug_print_dependencies_from_parameters
  pre_partial_deps_from_params
end;


          let partial_deps_from_params =
            merge_abstraction_infos
              pre_partial_deps_from_params
              iai.iai_dependencies_from_params_via_type in
          let sorted_deps_from_params =
            Dep_analysis.order_species_params_methods all_deps_from_params in
          let sorted_partial_deps_from_params =
            Dep_analysis.order_species_params_methods
              partial_deps_from_params in

          (* Remap computed dependencies onto the inherited parent's scheme if
             the method is inherited. *)
          let (from, name, _, _, _, _) = theorem_field_info in

if !debug_flg then
begin
Format.eprintf "[ALL]@." ;
Dep_analysis.debug_print_dependencies_from_parameters2 sorted_deps_from_params ;
Format.eprintf "Ready to map [ALL]@." ;
end;

          let (mapped_used_species_parameter_tys, mapped_deps) =
            remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_deps_from_params in

if !debug_flg then
begin
Format.eprintf "[SORTED PARTIAL]@." ;
Dep_analysis.debug_print_dependencies_from_parameters2
  sorted_partial_deps_from_params
end;

if !debug_flg then
begin
Format.eprintf "Ready to map [PARTIAL]@." ;
end;

          let (_, mapped_partial_deps_from_params) =
             remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_partial_deps_from_params in

if !debug_flg then
begin
Format.eprintf "[ALL MAPPED]@." ;
Dep_analysis.debug_print_dependencies_from_parameters2 mapped_deps ;
Format.eprintf "[PARTIAL MAPPED]@." ;
Dep_analysis.debug_print_dependencies_from_parameters2
  mapped_partial_deps_from_params ;
debug_flg := false ;
end;

          (* Build the final [abstraction_info]. *)
          let abstraction_info = {
            ai_used_species_parameter_tys = mapped_used_species_parameter_tys ;
            ai_dependencies_from_params = mapped_deps ;
            ai_dependencies_from_params_for_record_type =
              mapped_partial_deps_from_params ;
            ai_min_coq_env = iai.iai_min_coq_env } in
          FAI_theorem (theorem_field_info, abstraction_info)
      | IFAI_property (property_field_info, iai) ->
          let all_deps_from_params =
            merge_abstraction_infos
              iai.iai_dependencies_from_params_via_body
              (merge_abstraction_infos
                 iai.iai_dependencies_from_params_via_type
                 iai.iai_dependencies_from_params_via_completions) in
          (* Build the dependencies used to generate the record type
             parameters. *)
          let pre_partial_deps_from_params =
            complete_dependencies_from_params_rule_didou
              ~current_unit: ctx.Context.scc_current_unit
              ~via_body:
                (make_empty_param_deps ctx.Context.scc_species_parameters_names)
              ~via_type: iai.iai_dependencies_from_params_via_type
              ~via_completion: iai.iai_dependencies_from_params_via_PRM in
          let partial_deps_from_params =
            merge_abstraction_infos
              pre_partial_deps_from_params
              iai.iai_dependencies_from_params_via_type in
          let sorted_deps_from_params =
            Dep_analysis.order_species_params_methods all_deps_from_params in
          let sorted_partial_deps_from_params =
            Dep_analysis.order_species_params_methods
              partial_deps_from_params in
          (* Remap computed dependencies onto the inherited parent's scheme if
             the method is inherited. *)
          let (from, name, _, _, _) = property_field_info in
          let (mapped_used_species_parameter_tys, mapped_deps) =
            remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_deps_from_params in
          let (_, mapped_partial_deps_from_params) =
             remap_dependencies_on_params_for_field
              env ctx from name iai.iai_used_species_parameter_tys
              sorted_partial_deps_from_params in
          (* Build the final [abstraction_info]. *)
          let abstraction_info = {
            ai_used_species_parameter_tys = mapped_used_species_parameter_tys ;
            ai_dependencies_from_params = mapped_deps ;
            ai_dependencies_from_params_for_record_type =
              mapped_partial_deps_from_params ;
            ai_min_coq_env = iai.iai_min_coq_env } in
          FAI_property (property_field_info, abstraction_info))
    internal_abstractions
;;



let compute_abstractions_for_toplevel_theorem ctx theorem =
  let th_desc = theorem.Parsetree.ast_desc in
  let (decl_children, def_children) =
    compute_lambda_liftings_for_toplevel_theorem
      ctx.Context.scc_dependency_graph_nodes th_desc.Parsetree.th_name in
  (* Compute the visible universe of the theorem. *)
  let universe =
    VisUniverse.visible_universe
      ~with_def_deps_n_term_pr: true
      ctx.Context.scc_dependency_graph_nodes decl_children def_children in
  (* Now, its minimal Coq typing environment. *)
  let min_coq_env = MinEnv.minimal_typing_environment universe [] in
  let abstr_info = {
    ai_used_species_parameter_tys = [] ;
    ai_dependencies_from_params = [] ;
    ai_dependencies_from_params_for_record_type = [] ;
    ai_min_coq_env = min_coq_env } in
  abstr_info
;;
