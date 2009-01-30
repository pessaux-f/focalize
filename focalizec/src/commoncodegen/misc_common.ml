(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
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

(* $Id: misc_common.ml,v 1.18 2009-01-30 11:52:20 pessaux Exp $ *)



(* ************************************************************************** *)
(** {b Descr} : Lower-level species field (relevant for collection generator)
        description recording information about dependency and extra
        parameters already computed while generating the methods and that
        will be re-used while generating the collection generator.
        This avoids computing several the same things and ensure that the
        information is formated in the same way everywhere (in other words
        that the extra parameters discovered will appear in the same order
        between method declaration and method application).

    {b Rem} : Exported outside this module.                                   *)
(* ************************************************************************** *)
type compiled_field_memory = {
  (** Boolean telling if the method is logical, i.e. is either a logical let
      or a property or a theorem. *)
  cfm_is_logical : bool ;
  (** Where the method comes from via inheritance history. *)
  cfm_from_species : Env.from_history ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's type scheme. *)
  cfm_method_scheme : Env.method_type_kind ;
  (* The positionnal list of species parameter carriers appearing in the
     type of the method. They lead to extra arguments of type "Set" and
     must be instanciated by the correct type when applying the method's
     generator. This is mostly due to the fact that in Coq polymorphism
     is explicit and lead to a dependant type.
     NEVER USED FOR OCAML CODE GENERATION. *)
  cfm_used_species_parameter_tys : Parsetree.vname list ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and kind and the second is the set of methods the
      current method depends on from this species parameter. This contains
      ALL the dependencies found via definition 72 p 153 in Virgile Prevosto's
      PhD. *)
  cfm_dependencies_from_parameters :
    (Env.TypeInformation.species_param * Env.ordered_methods_from_params) list ;
  (** The positional list of method names appearing in the minimal Coq typing
      environment. *)
  cfm_coq_min_typ_env_names : Parsetree.vname list
} ;;



(* ************************************************************************ *)
(** {b Descr} : Type of the fields significant for the collection generator
       creation, once the methods of the species have been generated. It
       remove all the fields that are not relevant for the collection
       generator.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
type compiled_species_fields =
  | CSF_sig of compiled_field_memory
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of compiled_field_memory
  | CSF_property of compiled_field_memory
;;



type collection_effective_arguments =
  | CEA_collection_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr
;;



(* ************************************************************************* *)
(* Parsetree.species_param list ->                                           *)
(*   (Parsetree.vname * Env.ScopeInformation.species_parameter_kind) list    *)
(*     collection_effective_arguments list                                   *)
(** {b Descr} : Extract the collections names used in an "implements" clause
       as arguments of the species that it used to make the collection.
       The parsetree encodes these parameters [Parsetree.expr]s but this
       is a too large structure for the actual legal parameters expressions.
       Then we extracts here just the names of effective collections hidden
       in these [Parsetree.expr]s.

    {b Rem} : Exported outside this module.                                  *)
(* ************************************************************************* *)
let get_implements_effectives species_params_exprs species_formals_info =
  List.map2
    (fun param_expr (_, param_kind) ->
      let Parsetree.SP expr = param_expr.Parsetree.ast_desc in
      match param_kind with
       | Env.ScopeInformation.SPK_is ->
           (begin
           match expr.Parsetree.ast_desc with
            | Parsetree.E_constr (cstr_ident, []) ->
                let Parsetree.CI effective_species_name =
                  cstr_ident.Parsetree.ast_desc in
                CEA_collection_name_for_is effective_species_name
            | _ ->
                (* Collections expressions used as parameters of an      *)
                (* "implements" clause should always be represented by   *)
                (* a sum-type value and because these collections are    *)
                (* not parametrized, this value should have no argument. *)
                (* If it's not the case here, then we missed something   *)
                (* before during the analyses !                          *)
                assert false
           end)
       | Env.ScopeInformation.SPK_in ->
           (* For an entity parameter, all first-class expressions are legal. *)
           CEA_value_expr_for_in expr)
    species_params_exprs
    species_formals_info
;;



(** {b Descr} : Helper that ... *)
let find_entity_params_with_position params =
  (* Let's implement this imperative way to spare 1 argument on the stack. *)
  (* This is the counter used to report the positions of arguments in the  *)
  (* species signature. Always counting from 0.                            *)
  let cnt = ref 0 in
  (* We build the list in reverse order for sake of efficiency. *)
  let rec rec_find accu = function
    | [] -> accu
    | h :: q ->
        let accu' =
          (match h with
           | Env.TypeInformation.SPAR_in (n, _, _) -> (n, !cnt) :: accu
           | Env.TypeInformation.SPAR_is ((_, _), _, _, _, _) -> accu) in
        incr cnt ;  (* Always pdate the position for next parameter. *)
        rec_find accu' q in
  (* Now really do the job. *)
  (List.rev (rec_find [] params))
;;



(* ************************************************************************** *)
(* Context.species_compil_context -> Abstractions.environment_kind->          *)
(* Parsetree.vname -> Parsetree.module_name -> int ->                         *)
(*     (Parsetree.qualified_species * Parsetree_utils.simple_species_expr)    *)
(*       list -> Parsetree.expr                                               *)
(** {b Descr} : Takes the index of the parameter we want to instanciate
    among the list of parameters of the species where this parameter
    lives. Reminds that this parameter belongs to the species who DEFINED
    the method we are processing to create the collection generator (i.e. 
    the method whose method generator is inherited in the currently
    compiled species).

    When invocated, this function starts processing from the oldest species
    where the currently compiled method appeared, i.e. the species where
    it was really DEFINED, and we incrementally, setp by step, go "back"
    in the futur, to finally arrive at the "present". Along these steps, we
    substitute all the IN species parameters of an inherited species by
    all the corresponding effective expressions used in the inheritance
    expression. So, at the first call, we deal whith only 1 parameter
    since it is the one we are initially trying to instanciate, but during
    recursions along the inheritance history, we may have several
    parameters to instanciate since interleaving inherited species may
    have several.

    Once the process ends, we return the expression obtained by applying
    all the found substitutions of effective to formal parameters along
    the inheritance.
   
    {b Args} :
    - [ctx] : The current OCaml code generation context.

    - [env] : The current OCaml code generation environment.

    - [original_param_name] : The name of the species IN parameter we are
      trying to instanciate.

    - [original_param_unit] : The compilation unit where this parameter
      appears (in fact, that the compilation unit of the species it belongs
      to).

    -[original_param_index] : The index of the species parameter in the
      original species. We search by what it was instanciated along the
      inheritance. Hence, the search starts from the original hosting
      species, with the original hosting species's parameter's index. Then
      we go upward (more recent) along the inheritance tree.
    - [inheritance_steps] : The inheritance history describing where the
      currently processed method was defined and how it is propagated by
      inheritance up to the currently compiled species.

    {b Rem} : Exported outside this module.                                     *)
(* ************************************************************************** *)
let follow_instanciations_for_in_param ctx env original_param_name
    original_param_unit original_param_index inheritance_steps =
  let current_species_parameters = ctx.Context.scc_species_parameters_names in
  let current_unit = ctx.Context.scc_current_unit in
  let rec rec_follow params_unit ent_params accu_instanciated_expr = function
    | [] -> accu_instanciated_expr
    | inheritance_step :: rem_steps ->
        let (step_species, step_inheritance_expr, _) = inheritance_step in
        (* Here, we know that the method appears in a species that inherited. *)
        (* We have this species's name and the species expression used during *)
        (* inheritance.                                                       *)
        (* We must look at the effective expression used at the index to      *)
        (* replace each occurrence of the entity parameters by this effective *)
        (* expression.                                                        *)
        if Configuration.get_verbose () then
          Format.eprintf "This method flows in the derivated species: %a@."
            Sourcify.pp_qualified_species step_species ;
        (* Process all the substitution to do on each parameter. ATTENTION *)
        (* Do not fold left otherwise substitutions will be performed in   *)
        (* reverse order.                                                  *)
        let new_instanciation_expr =
          List.fold_right
            (fun (param_name, param_index) accu_expr ->
              let effective_arg_during_inher =
                List.nth
                  step_inheritance_expr.Parsetree_utils.sse_effective_args
                  param_index in
              if Configuration.get_verbose () then
                Format.eprintf "@[<2>Effective expression used to inherit \
                  in the derivated species:@ %a@]@."
                  Sourcify.pp_simple_species_expr_as_effective_parameter
                  effective_arg_during_inher ;
              (* Performs the substitution. *)
              let effective_expr_during_inher =
                (match effective_arg_during_inher with
                 | Parsetree_utils.SPE_Self -> assert false
                 | Parsetree_utils.SPE_Species _ -> assert false
                 | Parsetree_utils.SPE_Expr_entity expr -> expr) in
              let new_accu_expr =
                SubstExpr.subst_expr
                  ~param_unit: params_unit param_name
                  ~by_expr: effective_expr_during_inher.Parsetree.ast_desc
                  ~in_expr: accu_expr in
              new_accu_expr)
            ent_params
            accu_instanciated_expr in
        if Configuration.get_verbose () then
          Format.eprintf "@[<2>Instanciation expression after substitutions \
            of effective arguments: %a@]@."
            Sourcify.pp_expr new_instanciation_expr ;
        (* Now we must check the current level species's entity parameters *)
        (* and note their name and positions to try to instanciate them    *)
        (* at the next level.                                              *)
        (* First, get the species info of the current level. If it's the *)
        (* current_species we are analysing, then it won't be in the     *)
        (* environment. So we first test this before.                    *)
        let curr_level_species_params =
          if step_species = ctx.Context.scc_current_species then
            current_species_parameters
          else
            (begin
            (* This ident is temporary and created *)
            (* just to lookup in the environment.  *)
            let curr_level_species_ident =
              Parsetree_utils.make_pseudo_species_ident
                ~current_unit step_species in
              (match env with
               | Abstractions.EK_ml env ->
                   let (sp_prms, _, _, _) =
                     Env.MlGenEnv.find_species
                       ~loc: Location.none ~current_unit
                       curr_level_species_ident env in
                   sp_prms
               | Abstractions.EK_coq env ->
                   let (sp_prms, _, _, _) =
                     Env.CoqGenEnv.find_species
                       ~loc: Location.none ~current_unit
                       curr_level_species_ident env in
                   sp_prms)
            end) in
        let new_params_to_later_instanciate =
          find_entity_params_with_position curr_level_species_params in
        (* OPTIM POSSIBLE: si [] alors plus besoin de remonter l'héritage,
           car tout est déjà instancié. *)
        if Configuration.get_verbose () then
          (begin
          Format.eprintf
            "We found the following \"in\" parameters of the species \
            '%s#%a' t current inheritance level and they will possibly be \
            instanciated next inheritance step:@."
            (fst step_species)
            Sourcify.pp_vname (snd step_species) ;
          List.iter
            (fun (vn, pos) ->
              Format.eprintf "\t Parameter '%a' at position: %d@."
                Sourcify.pp_vname vn pos)
            new_params_to_later_instanciate
          end) ;
        (* Finally we must go upward in the inheritance to look for    *)
        (* further possible instanciations of these entity parameters. *)
        let new_params_unit = (fst step_species) in
        rec_follow
          new_params_unit new_params_to_later_instanciate
          new_instanciation_expr rem_steps in
  (* ***************************** *)
  (* Now, let's really do the job. *)
  (* We must walk the inheritance steps in reverse order *)
  (* since it is built with most recent steps in head.   *)
  let fake_original_expr = {
    Parsetree.ast_loc = Location.none ;
    Parsetree.ast_desc =
      Parsetree.E_var {
        Parsetree.ast_loc = Location.none ;
        Parsetree.ast_desc = Parsetree.EI_local original_param_name ;
        Parsetree.ast_doc = [] ;
        Parsetree.ast_type = Parsetree.ANTI_none } ;
    Parsetree.ast_doc = [] ;
    Parsetree.ast_type = Parsetree.ANTI_none } in
  rec_follow
    original_param_unit [(original_param_name, original_param_index)]
    fake_original_expr (List.rev inheritance_steps)
;;



(* ************************************************************************* *)
(** {b Descr} : Describes by what a parameter of collection generator must
    be instanciated. In effect, during inheritance "is" (i.e. collection
    parameters) are instanciated. When creating the collection generator,
    one must apply each method generator to effective arguments
    representing the abstractions we did for dependencies on species
    parameters.
    Three cases are possible:
      - [IPI_by_toplevel_collection] : The species parameter was
        instanciated by a toplevel collection. In this case, the way to
        reach functions to apply to the method generator is based on the
        compilation scheme using the
        "Module_representing_collection.effective_collection.fct"
        indirection.
      - [IPI_by_toplevel_species] : The species parameter was instanciated
        by a toplevel species fully defined. In this case, the way to
        reach functions to apply to the method generator is directly based
        on the indirection "Module_representing_species.fct".
      - [IPI_by_species_parameter] : The species parameter was instanciated
        by a species parameter of the species who inherits. In this case,
        we must use the extra parameter added to the collection generator
        and that lambda-lifts the dependency on the method of this
        parameter.

    {b Rem} Not exported outside this module.                                *)
(* ************************************************************************* *)
type is_parameter_instanciation =
  | IPI_by_toplevel_collection of Types.type_collection
  | IPI_by_toplevel_species of Types.type_collection
  | IPI_by_species_parameter of Env.TypeInformation.species_param
;;



(* ************************************************************************** *)
(* Context.species_compil_context -> Abstractions.environment_kind -> int ->  *)
(*  (Parsetree.qualified_species * Parsetree_utils.simple_species_expr)       *)
(*    list ->                                                                 *)
(*      is_parameter_instanciation                                            *)
(** {b Descr} : Takes the index of the parameter we want to instanciate
    among the list of parameters of the species where this parameter
    lives. Reminds that this parameter belongs to the species who DEFINED
    the method we are processing to create the collection generator (i.e. 
    the method whose method generator is inherited in the currently
    compiled species).

    When invocated, this function starts processing from the oldest species
    where the currently compiled method appeared, i.e. the species where
    it was really DEFINED, and we incrementally, setp by step, go "back" in
    the futur, to finally arrive at the "present". Along these steps, we
    look at the effective argument used for the formal corresponding one in
    the inheritance expression and substitute this effective to the formal
    at each step. This process lasts until either the instanciation is
    done by a toplevel species/collection (in this case, no more
    instanciation of parameter can be done) or until we arrive in the
    inheritance level of the currently compiled species.

    Once the process ends, we return the description of by what the initial
    species parameter was really instanciated all along the inheritance.
    ATTENTION: in case of instanciation by a toplevel species, we return
    the species used to instanciate the parameter, but we still need
    afterwards to exactly find in which parent of this species each method
    we have dependencies on was REALLY defined. This job has to be done after
    getting the result of thi function. This is architectured like this to
    allow preventing to compute several times instanciations when they appear
    to be done by toplevel collections or species parameters.

    {b Args} :
    - [ctx] : The current OCaml code generation context.

    - [env] : The current OCaml code generation environment.

    - [original_param_index] : The index of the species parameter in the
      original species where the method was REALLY defined (not the one
      where it is inherited). We search by what it was instanciated along
      the inheritance. Hence, the search starts from the original hosting
      species, with the original hosting species's parameter's index. Then
      we go upward (more recent) along the inheritance tree.

    - [inheritance_steps] : The inheritance history describing where the
      currently processed method was defined and how it is propagated by
      inheritance up to the currently compiled species.

    {b Rem} : Exported outside this module.                                    *) 
(* ************************************************************************* *)
let follow_instanciations_for_is_param ctx env original_param_index
    inheritance_steps =
  let current_species_parameters = ctx.Context.scc_species_parameters_names in
  let current_unit = ctx.Context.scc_current_unit in
  let rec rec_follow param_index = function
    | [] ->
        if Configuration.get_verbose () then
          Format.eprintf
            "Final instanciation by the species %dth species parameter.@."
            param_index ;
        IPI_by_species_parameter
          (List.nth current_species_parameters param_index)
    | inheritance_step :: rem_steps ->
        let (step_species, step_inheritance_expr, _) = inheritance_step in
        (* Here, we know that the method appears in a species that inherited. *)
        (* We have this species's name and the species expression used during *)
        (* inheritance.                                                       *)
        (* We must look at the effective expression used at the index to see  *)
        (* if it's a toplevel collection, toplevel closed species or a        *)
        (* species parameter of our level.                                    *)
        if Configuration.get_verbose () then
          Format.eprintf "This method flows in the derivated species: %a@."
            Sourcify.pp_qualified_species step_species ;
        let effective_arg_during_inher =
          List.nth
            step_inheritance_expr.Parsetree_utils.sse_effective_args
            param_index in
        if Configuration.get_verbose () then
          Format.eprintf "Effective expression used to inherit in the \
            derivated species:@ %a@."
            Sourcify.pp_simple_species_expr_as_effective_parameter
            effective_arg_during_inher ;
        (* First, get the species info of the current level. If it's the *)
        (* current_species we are analysing, then it won't be in the     *)
        (* environment. So we first test this before.                    *)
        let curr_level_species_params =
          if step_species = ctx.Context.scc_current_species then
            current_species_parameters
          else
            (begin
            (* This ident is temporary and created *)
            (* just to lookup in the environment.  *)
            let curr_level_species_ident =
              Parsetree_utils.make_pseudo_species_ident
                ~current_unit step_species in
            match env with
             | Abstractions.EK_ml env ->
                 let (sp_prms, _, _, _) =
                   Env.MlGenEnv.find_species
                     ~loc: Location.none ~current_unit
                     curr_level_species_ident env in
                 sp_prms
             | Abstractions.EK_coq env ->
                 let (sp_prms, _, _, _) =
                   Env.CoqGenEnv.find_species
                     ~loc: Location.none ~current_unit
                     curr_level_species_ident env in
                 sp_prms
            end) in
        (* Now, really check if its a species parameter. *)
        match effective_arg_during_inher with
         | Parsetree_utils.SPE_Self -> failwith "Euhhhh2 ?"  (* [Unsure] *)
         | Parsetree_utils.SPE_Expr_entity _ -> assert false
         | Parsetree_utils.SPE_Species (effective_qual_vname, provenance) ->
             (begin
             let (effective_mod, effective_name_as_string) =
               (match effective_qual_vname with
                | Parsetree.Vname _ ->
                    (* During scoping and typing, everything should *)
                    (* have been explicitely qualified !            *)
                    assert false
                | Parsetree.Qualified (x, y) ->
                    (x, Parsetree_utils.name_of_vname y)) in
             match provenance with
              | Types.SCK_species_parameter ->
                  (* The instanciation is done by a one *)
                  (* of our species parameters.         *)
                  let index_of_instancier =
                    Handy.list_first_index
                      (function
                        | Env.TypeInformation.SPAR_in (_, _, _) -> false
                        | Env.TypeInformation.SPAR_is
                            ((formal_mod, formal_name), _, _, _, _) ->
                              (effective_mod = formal_mod) &&
                              (effective_name_as_string = formal_name))
                      curr_level_species_params in
                      (* We must instanciate by the abstraction corresponding *)
                      (* to our parameter and continue waking up along the    *)
                      (* inheritance history.                                 *)
                      (* So we first get the index of this parameter in the   *)
                      (* current level species.                               *)
                      if Configuration.get_verbose () then
                        Format.eprintf
                          "Instanciation by the species %dth species \
                          parameter. Let's continue search along the \
                          inheritance tree for further instanciations.@."
                          index_of_instancier ;
                      rec_follow index_of_instancier rem_steps
              | Types.SCK_toplevel_collection ->
                  (begin
                  (* The instanciation is done by a toplevel collection. In *)
                  (* this case, no need from now to continue walking up     *)
                  (* along the inheritance history, there won't be anymore  *)
                  (* instanciations.                                        *)
                  if Configuration.get_verbose () then
                    Format.eprintf
                      "Final instanciation by toplevel collection.@." ;
                  IPI_by_toplevel_collection
                    (effective_mod, effective_name_as_string)
                  end)
              | Types.SCK_toplevel_species ->
                  (begin
                  (* The instanciation is done by a toplevel species. In this
                     case, no need from now to continue walking up along the
                     inheritance history, there won't be anymore
                     instanciations. *)
                  if Configuration.get_verbose () then
                    Format.eprintf
                      "Final instanciation by toplevel species.@." ;
                  IPI_by_toplevel_species
                    (effective_mod, effective_name_as_string)
                  end)
             end) in
  (* We must walk the inheritance steps in reverse order since it is built
     with most recent steps in head. *)
  rec_follow original_param_index (List.rev inheritance_steps)
;;



(* ************************************************************************** *)
(*  Abstractions.environment_kind -> current_unit: Parsetree.module_name ->   *)
(*    start_spec_mod: Parsetree.module_name -> start_spec_name: string ->     *)
(*      method_name: Parsetree.vname -> (Parsetree.module_name * string)      *)
(** {b Descr} : This function looks for the real species that defines
    the method [method_name] of the species [(start_spec_mod,start_spec_name)]
    along its inheritance tree. The species in which we start looking is one
    that the instanciation process (during collection generator creation)
    reported us to be the effective instanciation of a species parameter.

    The example below show shuch a case:

    species Sp0 = let m = 5 ; end ;;
    species Sp1 inherits Sp0 = rep = int ; let n = 5 ; end ;;
    species Foo0 (A0 is Sp0) = rep = int ; let v = A0!m ; end ;;
    species Foo1_1_1 inherits Foo0 (Sp1) = end ;;
    
    In Foo1_1_1, wer want to get the OCaml code
         let local_v = Foo0.v Sp0.m.
    In effect, the method generator for the method 'm' on which we depend
    if not really in 'Sp1' ('Sp1' is the effective instanciation of the
    parameter 'Sp0' of 'Foo0' during inheritance in 'Foo1_1_1').
    The method generator is in the parent of 'Sp1', i.e. 'Sp0'.
      
    So, we just need to recover the species description, find the method's
    description and see in which species the method was comming "from".
    We return the pair containing the species module name and the species
    name where the method was really DEFINED.

    {b Args}:
    - [env] : The current OCaml code generation environment.

    - [current_unit] : The current compilation unit.

    - [start_spec_mod] [start_spec_name] : Module and species names from where
      to start the search and follow ("along the past") the inheritance tree.

    - [method_name] : The name of the method fow which we are searching the
      definition.

    {b Rem} : Exported outside this module.                                   *)
(* ************************************************************************** *)
let find_toplevel_spe_defining_meth_through_inheritance env ~current_unit
    ~start_spec_mod ~start_spec_name ~method_name  =
  try
    (* This ident is temporary and created just to lookup in the environment. *)
    let species_ident =
      Parsetree_utils.make_pseudo_species_ident
        ~current_unit (start_spec_mod, (Parsetree.Vuident start_spec_name)) in
    (* We get the species' methods information. *)
    let methods_infos =
      (match env with
       | Abstractions.EK_ml env ->
           let (_, data, _, _) =
             Env.MlGenEnv.find_species
               ~loc: Location.none ~current_unit species_ident env in
           data
       | Abstractions.EK_coq env ->
           let (_, data, _, _) =
             Env.CoqGenEnv.find_species
               ~loc: Location.none ~current_unit species_ident env in
           data) in
    (* Now, just search information about the method... *)
    let meth_info =
      List.find (fun mi -> mi.Env.mi_name = method_name) methods_infos in
    (* And now, just return the [from_history.fh_initial_apparition]. *)
    let (mod_name, spec_name) =
     (meth_info.Env.mi_history).Env.fh_initial_apparition in
    (mod_name, (Parsetree_utils.name_of_vname spec_name))
  with _ ->
    (* Since the species is toplevel, the lookup in the environment must never
       fail ! And the method searched must also exist in the found species
       since we were told that it was inherited via this method. If any
       failure occurs, then the compiler is wrong somewhere ! *)
    assert false
;;



(* *********************************************************************** *)
(** {b Descr} : Describes when starting the code of a let binding how it
    must be linked to the possible previous code. If the binding is the
    first of a non-recursive definition, then it must be introduced by
    "let ". If it is the first of a recursive definition, then it must be
    introduced by "let rec ". If it is not the first of a multiple
    definition, then it must be introduced by "and ".

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
type let_connector =
  | LC_first_non_rec   (** The binding is the first of a non-recursive
                           definition. *)
  | LC_first_rec   (** The binding is the first of a recursive definition. *)
  | LC_following   (** The binding is not the first of a multiple definition
                       (don't matter if the definition is recursive or not). *)
;;



(* ******************************************************************** *)
(** {b Descr} : Build the list of abstract parameters according to the
    information found in the [abstraction_info] [ai]. This is similar
    to create the list of strings represented by the sequence of
    parameters of a method due to its dependencies.
    This list first starts by the parameters coming from the species
    parameters's methods the current method depends on.
    Then come the parameters coming from the methods of ourselves we
    depend on.

    {b Args} :
      - [care_logical] : Boolean telling if the target language needs
        logical definitions (i.e. theorems, properties). For instance,
        is true for Coq and false for OCaml.

      - [care_types] : Boolean telling if the target language needs to
        explicitely manage abstraction of types. For instance, is true for
        Coq and false for OCaml.

      - [ai] : The [abstraction_info] of a method, recording this
        method's dependencies.

    {b Return} :
      - The list of names corresponding to the arguments lambda-lifting
        the dependencies (those from methods of the species parameters
        first, in the order of species parameters, then those from the
        methods of ourselves).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let make_params_list_from_abstraction_info ~care_logical ~care_types ai =
  (* Build the list by side effect in reverse order for efficiency. *)
  let the_list_reversed = ref [] in
  (* First, abstract according to the species's parameters carriers types the
     current method depends on only if [care_types] is on. *)
  if care_types then
    List.iter
      (fun sp_param_ty_name ->
        (* Each abstracted type name is built like "_p_" + parameter's name
           + "_T". *)
        let llift_name =
          "_p_" ^
          (Parsetree_utils.vname_as_string_with_operators_expanded
             sp_param_ty_name) ^
          "_T" in
        the_list_reversed := llift_name :: !the_list_reversed)
      ai.Abstractions.ai_used_species_parameter_tys ;
  (* Next, abstract according to the species's parameters methods the current
     method depends on. *)
  List.iter
    (fun (species_param, (Env.ODFP_methods_list meths_from_param)) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _, _) ->
             Parsetree.Vuident n in
      (* Each abstracted method will be named like "_p_", followed by the
         species parameter name, followed by "_", followed by the method's
         name.
         We don't care here about whether the species parameters is "IN" or
         "IS". *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      List.iter
        (fun (meth, _) ->
          the_list_reversed :=
            (prefix ^
             (Parsetree_utils.vname_as_string_with_operators_expanded meth))
            :: !the_list_reversed)
        meths_from_param)
    ai.Abstractions.ai_dependencies_from_params ;
  (* Next, the extra arguments due to methods of ourselves we depend on.
     They are always present in the species under the name "self_...". *)
  List.iter
    (function
      | MinEnv.MCEE_Defined_carrier _
      | MinEnv.MCEE_Defined_computational (_, _, _, _, _)
      | MinEnv.MCEE_Defined_logical (_, _, _) ->
          (* Anything defined is not abstracted. *)
          ()
      | MinEnv.MCEE_Declared_logical (_, _) ->
          (* In Ocaml, (i.e if [care_logical] is [false]) logical properties
             are forgotten. *)
          if care_logical then failwith "TODO 42"
      | MinEnv.MCEE_Declared_carrier ->
         (* In Ocaml generation model (i.e. if [care_types] is [false], the
             carrier is never lambda-lifted then doesn't appear as an extra
             parameter. Hence, we take care of the carrier only in Coq, i.e. if
             [care_types] is [true]. *)
          if care_types then
            (begin
            (* In Coq, the carrier is always abstracted by "abst_T". *)
            the_list_reversed := "abst_T" :: !the_list_reversed
            end)
      | MinEnv.MCEE_Declared_computational (n, _) ->
          let llift_name =
            "abst_" ^
            (Parsetree_utils.vname_as_string_with_operators_expanded n) in
          the_list_reversed := llift_name :: !the_list_reversed)
    ai.Abstractions.ai_min_coq_env ;
  (* Finally, reverse the list to keep the right oder. *)
  List.rev !the_list_reversed
;;
