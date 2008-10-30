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


(* $Id: abstractions.ml,v 1.46 2008-10-30 10:37:57 pessaux Exp $ *)


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
  | FTK_computational
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



(* For debugging purpose only.
let debug_print_dependencies_from_parameters l =
  List.iter
    (fun (species_param, methods) ->
      (match species_param with
       | Env.TypeInformation.SPAR_is ((mod_name, spe_name), _, _, _, _) ->
           Format.eprintf "From IS-parameter '%s#%s', methods: "
             mod_name spe_name ;
       | Env.TypeInformation.SPAR_in (n, _, _) ->
           Format.eprintf "From IN-parameter '%a', methods: "
             Sourcify.pp_vname n) ;
      Parsetree_utils.ParamDepSet.iter
        (fun (n, _) -> Format.eprintf "%a " Sourcify.pp_vname n) methods ;
      Format.eprintf "@.")
    l
;;
*)



(* *********************************************************************** *)
(** {b Descr} Helper that creates the structure denoting an empty species
    parameters dependencies set. The "empty" dependencies cannot simple be
    [] because all our "union" functions on dependencies rely on a list of
    sets with 1 set for each species parameter name. So we create our
    initial accumulator as the list mapping each species parameter name
    onto the empty dependencies set.

    {b Rem}: Not exported outside this module.                             *)
(* *********************************************************************** *)
let make_empty_param_deps species_parameters_names =
  List.fold_right
    (fun species_param accu ->
      (species_param, Parsetree_utils.ParamDepSet.empty) :: accu)
    species_parameters_names
    []
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
         | Env.TypeInformation.SF_let (_, _, _, _, _, _, _)
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
    all_my_fields =
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
          | (_, DepGraphData.DK_def) -> false)
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
        let meths_from_param =
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
        (* Return a couple binding the species parameter's name with the
           methods of it we found as required for the current method. *)
        (species_param, meths_from_param) :: accu)
      species_parameters_names
      [] in
  (* By side effect, we remind the species types appearing in our type. *)
  let carriers_appearing_in_types =
    ref (Types.get_species_types_in_type my_type) in
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
          | (_, DepGraphData.DK_def) -> false)
        my_node.DepGraphData.nn_children
    with Not_found -> ([], [])  (* No children at all. *)) in
  (decl_children, def_children)
;;



type abstraction_info = {
  ai_used_species_parameter_tys : Parsetree.vname list ;
  (** Dependencies found via [BODY] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  ai_dependencies_from_params_via_body :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
  list ;
  (** Dependencies found via [TYPE] of definition 72 page 153 of Virgile
      Prevosto's Phd. *)
  ai_dependencies_from_params_via_type :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
  list ;
  (** Other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of definition
      72 page 153 of Virgile Prevosto's Phd. *)
  ai_dependencies_from_params_via_completion :
    ((** The species parameter's name and kind. *)
     Env.TypeInformation.species_param *
     Parsetree_utils.ParamDepSet.t)  (** The set of methods we depend on. *)
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

    {b Rem} : Exported outside this module.                                  *)
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
         | FAI_sig ((_, n, _), abstraction_info) 
         | FAI_let ((_, n, _, _, _, _, _), abstraction_info)
         | FAI_theorem ((_, n, _, _, _, _) , abstraction_info)
         | FAI_property ((_, n, _, _, _), abstraction_info) ->
             if n = name then Some abstraction_info else rec_find q
         | FAI_let_rec l ->
             (begin
             try
               let (_, abstraction_info) =
                 List.find (fun ((_, n, _, _, _, _, _), _) -> n = name) l in
               Some abstraction_info
             with Not_found -> rec_find q
             end) in
  rec_find abstractions
;;



(* ********************************************************************** *)
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

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
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
                   (* ... and add it to the dependencies of
                      [eff_arg_qual_vname] in the current dependencies
                      accumulator, i.e into [inner_accu_deps_from_params]. *)
                   add_param_dependencies
                     ~param_name: eff_arg_qual_vname ~deps: y
                     ~to_deps: accu_deps_for_zs)
                 (* Arguments of the deepest [DepNameSet.fold]. *)
                 all_z
                 inner_accu_deps_from_params)
        (* Arguments of the inner [List.fold_left]. *)
        accu_deps_from_params
        usages)
    (* Arguments of the outer [List.fold_left]. *)
    starting_dependencies_from_params
    parametrised_params_with_their_effective_args_being_params
;;



(** Implements rules [TYPE], [DEF-DEP], [UNIVERSE] and [PRM] of the
    definition 72 page 153 of Virgile Prevosto's Phd. *)
let complete_dependencies_from_params env ~current_unit ~current_species
    seen_abstractions species_parameters def_children universe type_kind =
  (* Rule [TYPE] possible only if a logical expression is provided. In effect,
     in a type scheme, species_parameters can never appear since it is a
     ML-like type. *)
  let dependencies_from_params_via_type =
    (match type_kind with
     | FTK_computational ->
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
     the list) for their [ai_dependencies_from_params_via_body].
     Hence, instead of making [List.map] on the [species_parameter_names] to
     individually merge the methods from each children for a species parameter,
     we simply make the union (without double) of all the
     [ai_dependencies_from_params_via_body] of the def-children. *)
  let dependencies_from_params_via_compl1 =
    List.fold_left
      (fun accu_deps_from_params abstr_infos_opt ->
        match abstr_infos_opt with
         | Some abstr_infos ->
             (* We merge the found abstraction info and the abstraction info
                accumulator. *)
             merge_abstraction_infos
               abstr_infos.ai_dependencies_from_params_via_body
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
           must add its [ai_dependencies_from_params_via_type].
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
                 abstr_info.ai_used_species_parameter_tys ;
             (* Now, add the [ai_dependencies_from_params_via_type] to the
                dependencies accumulator. *)
             merge_abstraction_infos
               abstr_info.ai_dependencies_from_params_via_type
               accu_deps_from_params
         | None ->
             (* No abstr_infos found, so leave the accumulator as it was. *)
             accu_deps_from_params)
      universe
      dependencies_from_params_via_compl1 in
  (* Extend the found dependencies via rule [PRM]. *)
  let dependencies_from_params_via_compl3 =
    complete_dependencies_from_params_rule_PRM
      env ~current_unit species_parameters
      dependencies_from_params_via_compl2 in
 (dependencies_from_params_via_type, dependencies_from_params_via_compl3,
  !carriers_appearing_in_types)
;;



(**
    To be usable for OCaml generation, the [with_def_deps] flag
    enables to forget the def-dependencies and their implied
    transitive decl-dependencies. In effect, in OCaml, only
    decl-dependencies are relevant.
*)
let compute_abstractions_for_fields ~with_def_deps env ctx fields =
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
               ai_used_species_parameter_tys = as_set ;
               ai_dependencies_from_params_via_body = empty_deps ;
               ai_dependencies_from_params_via_type = empty_deps ;
               ai_dependencies_from_params_via_completion = empty_deps ;
               ai_min_coq_env = [] } in
             (FAI_sig (si, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let ((_, name, _, sch, body, _, _) as li) ->
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
                 body_as_fbk (Types.specialize sch) fields in
             (* Compute the visible universe of the method. *)
             let universe =
               VisUniverse.visible_universe
                 ~with_def_deps
                 ctx.Context.scc_dependency_graph_nodes decl_children
                 def_children in
             (* Complete the dependencies from species parameters info. By the
                way, we record the species parameters carrier appearing in the
                methods of self that were added during the completion phase. *)
             let (dependencies_from_params_in_type,
                  dependencies_from_params_via_compl,
                  used_species_parameter_tys_in_meths_self_after_completion) =
               complete_dependencies_from_params
                 env ~current_unit: ctx.Context.scc_current_unit
                 ~current_species: ctx.Context.scc_current_species
                 abstractions_accu ctx.Context.scc_species_parameters_names
                 def_children universe FTK_computational in
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
                 dependencies_from_params_via_compl in
             (* Now, its minimal Coq typing environment. *)
             let min_coq_env =
               MinEnv.minimal_typing_environment universe fields in
             let abstr_info = {
               ai_used_species_parameter_tys = all_used_species_parameter_tys ;
               ai_dependencies_from_params_via_body =
                 dependencies_from_params_in_body ;
               ai_dependencies_from_params_via_type =
                 dependencies_from_params_in_type ;
               ai_dependencies_from_params_via_completion =
                 dependencies_from_params_via_compl ;
               ai_min_coq_env = min_coq_env } in
             (FAI_let (li, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_let_rec l ->
             let deps_infos =
               List.map
                 (fun ((_, name, _, sch, body, _, _) as li) ->
                   let body_as_fbk =
                     match body with
                      | Parsetree.BB_logical p -> FBK_logical_expr p
                      | Parsetree.BB_computational e -> FBK_expr e in
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
                       body_as_fbk (Types.specialize sch) fields in
                   (* Compute the visible universe of the method. *)
                   let universe =
                     VisUniverse.visible_universe
                       ~with_def_deps
                       ctx.Context.scc_dependency_graph_nodes
                       decl_children def_children in
                   (* Complete the dependencies from species parameters info.
                      By the way, we record the species parameters carrier
                      appearing in the methods of self that were added during
                      the completion phase. *)
                   let (dependencies_from_params_in_type,
                        dependencies_from_params_via_compl,
                        used_species_parameter_tys_in_meths_self_after_completion) =
                     complete_dependencies_from_params
                       env ~current_species: ctx.Context.scc_current_species
                       ~current_unit: ctx.Context.scc_current_unit
                       abstractions_accu ctx.Context.
                         scc_species_parameters_names
                       def_children universe FTK_computational in
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
                       dependencies_from_params_via_compl in
                   (* Now, its minimal Coq typing environment. *)
                   let min_coq_env =
                     MinEnv.minimal_typing_environment universe fields in
                   let abstr_info = {
                     ai_used_species_parameter_tys =
                       all_used_species_parameter_tys ;
                     ai_dependencies_from_params_via_body =
                       dependencies_from_params_in_bodies ;
                     ai_dependencies_from_params_via_type =
                       dependencies_from_params_in_type ;
                     ai_dependencies_from_params_via_completion =
                       dependencies_from_params_via_compl ;
                     ai_min_coq_env = min_coq_env } in
                   (li, abstr_info))
                 l in
             (FAI_let_rec deps_infos) :: abstractions_accu
         | Env.TypeInformation.SF_theorem
             ((_, name, _, logical_expr, proof, _) as ti) ->
               let sch = Types.trivial_scheme (Types.type_prop ()) in
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
                   (FBK_proof (Some proof)) (Types.specialize sch) fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps
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
                    dependencies_from_params_via_compl,
                    used_species_parameter_tys_in_meths_self_after_completion) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe (FTK_logical logical_expr) in
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
                   dependencies_from_params_via_compl in
               let abstr_info = {
                 ai_used_species_parameter_tys =
                   all_used_species_parameter_tys ;
                 ai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 ai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 ai_dependencies_from_params_via_completion =
                   dependencies_from_params_via_compl ;
                 ai_min_coq_env = min_coq_env } in
               (FAI_theorem (ti, abstr_info)) :: abstractions_accu
         | Env.TypeInformation.SF_property
             ((_, name, _, logical_expr, _) as pi) ->
               let sch = Types.trivial_scheme (Types.type_prop ()) in
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
                   (FBK_proof None) (Types.specialize sch) fields in
               (* Compute the visible universe of the theorem. *)
               let universe =
                 VisUniverse.visible_universe
                   ~with_def_deps
                   ctx.Context.scc_dependency_graph_nodes decl_children
                   def_children in
               (* Complete the dependencies from species parameters info. By the
                  way, we record the species parameters carrier appearing in the
                  methods of self that were added during the completion
                  phase. *)
               let (dependencies_from_params_in_type,
                    dependencies_from_params_via_compl,
                    used_species_parameter_tys_in_meths_self_after_completion) =
                 complete_dependencies_from_params
                   env ~current_species: ctx.Context.scc_current_species
                   ~current_unit: ctx.Context.scc_current_unit
                   abstractions_accu ctx.Context.scc_species_parameters_names
                   def_children universe (FTK_logical logical_expr) in
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
                   dependencies_from_params_via_compl in
               (* Now, its minimal Coq typing environment. *)
               let min_coq_env =
                 MinEnv.minimal_typing_environment universe fields in
               let abstr_info = {
                 ai_used_species_parameter_tys =
                   all_used_species_parameter_tys ;
                 ai_dependencies_from_params_via_body =
                   dependencies_from_params_in_bodies ;
                 ai_dependencies_from_params_via_type =
                   dependencies_from_params_in_type ;
                 ai_dependencies_from_params_via_completion =
                   dependencies_from_params_via_compl ;
                 ai_min_coq_env = min_coq_env } in
               (FAI_property (pi, abstr_info)) :: abstractions_accu)
      []      (* Initial empty abstractions accumulator. *)
      fields in
  (* Finally, put the list of abstractions in the right order, i.e. in the
     order of apparition of the fields in the species. *)
  List.rev reversed_abstractions
;;



let compute_abstractions_for_toplevel_theorem ctx theorem =
  let th_desc = theorem.Parsetree.ast_desc in
  let (decl_children, def_children) =
    compute_lambda_liftings_for_toplevel_theorem
      ctx.Context.scc_dependency_graph_nodes th_desc.Parsetree.th_name in
  (* Compute the visible universe of the theorem. *)
  let universe =
    VisUniverse.visible_universe
      ~with_def_deps: true
      ctx.Context.scc_dependency_graph_nodes decl_children def_children in
  (* Now, its minimal Coq typing environment. *)
  let min_coq_env = MinEnv.minimal_typing_environment universe [] in
  let abstr_info = {
    ai_used_species_parameter_tys = [] ;
    ai_dependencies_from_params_via_body = [] ;
    ai_dependencies_from_params_via_type = [] ;
    ai_dependencies_from_params_via_completion = [] ;
    ai_min_coq_env = min_coq_env } in
  abstr_info
;;
