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

(* $Id: species_coq_generation.ml,v 1.27 2008-02-26 13:00:56 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
              Coq of FoCaL's collections and species.            *)
(* *************************************************************** *)




(* [Unsure] Comme pour OCaml ! Factoriser ! Nettoyer !!! *)
type let_connector =
  | LC_first_non_rec   (** The binding is the first of a non-recursive
                           definition. *)
  | LC_first_rec   (** The binding is the first of a recursive definition. *)
  | LC_following   (** The binding is not the first of a multiple definition
                       (don't matter if the definition is recursive or not). *)
;;



type compiled_method_body =
  | CMB_expr  of Parsetree.expr
  | CMB_prop of Parsetree.prop
;;


(* [Unsure] Comme pour OCaml ! Factoriser ! Nettoyer !!! *)
type compiled_field_memory = {
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_species ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's body. *)
  cfm_method_body  : compiled_method_body ;
  (* The positionnal list of species parameter carriers appearing in the
     type of the method. They lead to extra arguments of type "Set" and
     must be instanciated by the correct type when applying the method's
     generator. This is mostly due to the fact that in Coq polymorphism
     is explicit and lead to a dependant type. *)
  cfm_used_species_parameter_tys : Parsetree.vname list ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and the second is the set of methods the current
      method depends on from this species parameter.*)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ;
  (** The positional list of method names appearing in the minimal Coq typing
      environment. Note that for Sig, Let, Let_rec and Property, this list
      is always empty. The notion of minimal environment is only required for
      theorems. *)
  cfm_coq_min_typ_env_names : Parsetree.vname list
} ;;



(* [Unsure] Quasiment comme pour OCaml ! Factoriser ! Nettoyer !!! En plus, incomplet ! *)
type compiled_species_fields =
  | CSF_sig of Parsetree.vname
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of compiled_field_memory
  | CSF_property of Parsetree.vname (* [Unsure] *)
;;



(* ******************************************************************** *)
(** {b Descr} : Describes how a method arrives into a visible universe.
    Either by a decl-dependency and NO transitive def-dependency. Or by
    at least a transitive def-dependency (in this case, no matter if it
    also arrives thanks a decl-dependency.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
type in_the_universe_because =
  | IU_only_decl   (** The method arrives in the visible universe by the
                       presence of only a decl-dependency and NO transitive
                       def-dependency. *)
  | IU_trans_def   (** The method arrives in the visible universe by the
                       presence at least of a transitive def-dependency
                       (no matter whether the also is a decl-dependency in
                       this case). *)
;;



(* ********************************************************************* *)
(** {b Descr} : Structure of a visible universe. It's a [Map] linking
    a method name with the reason how this method arrives in the visible
  universe.
    Building the visible universe with this information ease the
    computation of the \inter\smallinter operation of Virgile Prevosto's
    Phd, page 116, definition 58, section 6.4.4. In effect, with the
    reason of why the method is in the universe reminded, the choice of
    rule 3 or 4 is immediate instead of having to look again if the
    method was a introduced my a decl ou a transitive-def dependency.

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
module UniverseElem = struct
  type t = Parsetree.vname
  let compare = compare
end ;;
module Universe = Map.Make(UniverseElem) ;;



(* ************************************************************** *)
(* (Dep_analysis.name_node * 'a) list ->                          *)
(*   (Dep_analysis.name_node * 'b) list ->                        *)
(*     in_the_universe_because Universe.t                         *)
(** {b Descr} : Computes the visible universe of a species field
    whose decl, def-dependencies and fields are given as argument.
    This function works according to the definition 57 page 116
    section 6.4.4 in Virgile Prevosto's Phg.

    {b Rem} : Not exported outside this module.                   *)
(* ************************************************************** *)
let visible_universe dep_graph x_decl_dependencies x_def_dependencies =
  (* First, apply rule 1. Because decl-dependencies are already computed  *)
  (* when computing the visible universe, just take them as parameter     *)
  (* instead of computing them again. We add each method with the tag     *)
  (* telling that it comes here thanks to a decl-dependency. If it        *)
  (* appears later to also come thank to a transitive def-dependency,     *)
  (* then it will be removed and changed to that def tag in the universe. *)
  let universe = ref Universe.empty in
  List.iter
    (fun (n, _) ->
      universe := Universe.add n.Dep_analysis.nn_name IU_only_decl !universe)
    x_decl_dependencies ;
  (* Next, apply rule 2 and 3. Add the def-dependencies. Like            *)
  (* decl-dependencies,  they are already available, so take them as a   *)
  (* parameter instead of computing them again. For each of them we      *)
  (* follow the transitive links to add the transitive def-dependencies. *)
  (* Rule 3 is implemented by adding for each transitive def-dependency  *)
  (* node, its decl-dependencies.                                        *)
  (* First, create the set of already isited nodes. *)
  let seen = ref Parsetree_utils.VnameSet.empty in
  (* *********************************************************** *)
  (* transitive_addition : name_node -> unit                     *)
  (* {b Descr} : The local recursive function that will walk the *)
  (* dependencies graph to hunt transitive def-dependencies.     *)
  (* It also add the decl-dependencies for each def-dependency   *)
  (* found. This way, one unique walk is needed.                 *)
  (* *********************************************************** *)
  let rec transitive_addition n =
    if not (Parsetree_utils.VnameSet.mem n.Dep_analysis.nn_name !seen) then
      (begin
      (* Mark it as seen. *)
      seen := Parsetree_utils.VnameSet.add n.Dep_analysis.nn_name !seen ;
      (* Add the node that has def-dependency to the universe. If the method *)
      (* already appeared with only the decl tag, then it gets cleared and   *)
      (* replaced with the tag meaning that this method comes here thanks to *)
      (* a transitive def-dependency.                                        *)
      universe := Universe.add n.Dep_analysis.nn_name IU_trans_def !universe ;
      List.iter
        (function
          | (child_node, Dep_analysis.DK_def) ->
              (* Add the decl-dependencies of this node to the universe. *)
              List.iter
                (function
                  | (child_node_decl_child, (Dep_analysis.DK_decl _)) ->
                      (begin
                      (* If the method already appeared with the tag meaning *)
                      (* that is comes here thanks to a transitive def-dep , *)
                      (* let it unchanged, otherwise add it with the tag     *)
                      (* meaning that it come here thanks to a decl-dep.     *)
                      (* In fact the process is simpler: if the method       *)
                      (* already appears in the universe: either it's with a *)
                      (* transitive def-dep, and then no change to do. Or    *)
                      (* it's with de decl-dep tag, and in this case, it's   *)
                      (* useless to add it again with this tag: then also no *)
                      (* change to do.                                       *)
                      if not
                          (Universe.mem
                             child_node_decl_child.Dep_analysis.nn_name
                             !universe) then
                        universe :=
                          Universe.add
                            child_node_decl_child.Dep_analysis.nn_name
                            IU_only_decl !universe
                      end)
                  | (_, Dep_analysis.DK_def) -> ())
                child_node.Dep_analysis.nn_children ;
              (* Now recurse to walk deeper in the graph *)
              (* on def-dependency children only.        *)
              transitive_addition child_node
          | (_, (Dep_analysis.DK_decl _)) -> ())
        n.Dep_analysis.nn_children
      end) in
  (* Now, start the transitive hunt for each initial def-dependencies nodes. *)
  List.iter
    (fun (def_node, _) -> transitive_addition def_node)
    x_def_dependencies ;
  (* Now, compute the fixpoint for rule 4. This rule only takes into account *)
  (* decl-dependencies of the "type" of a field. This is equivalent to only  *)
  (* use the "decl"-dependencies tagged {DDK_from_type].                     *)
  let continue = ref true in
  while !continue do
    (* Reset the fixpoint end's flag. *)
    continue := false ;
    (* For each member of the universe... *)
    Universe.iter
      (fun z_name _ ->    (* [z_name] is "z" in definition 57 of the Phd. *)
        (* ... Find its node in the graph (must never fail because the     *)
        (* graph is already built and is built from the species fields)... *)
        let z_node =
          (try
            List.find (fun node -> node.Dep_analysis.nn_name = z_name) dep_graph
          with Not_found -> assert false) in
        (* ... For each dependency of the node... *)
        List.iter
          (function
            | (child_node, (Dep_analysis.DK_decl Dep_analysis.DDK_from_type)) ->
                (* ... Only consider those tagged as coming       *)
                (* from the "type" of the field having this mode. *)
                (* If the child is not already in universe, then add it with  *)
                (* a [IU_only_decl] tag. If it is already in the universe,    *)
                (* then do nothing. In effet, either it is inside with a      *)
                (* [IU_only_decl] tag and then it is useless to add it again. *)
                (* Or it is inside with a [IU_trans_def] and this flag has    *)
                (* a higher priority and must not be changed.                 *)
                if not
                    (Universe.mem child_node.Dep_analysis.nn_name !universe)
                then
                  (begin
                  universe :=
                    Universe.add
                      child_node.Dep_analysis.nn_name IU_only_decl !universe ;
                  (* Mark that we must continue the fixpoint. *)
                  continue := true ;
                  end)
            | (_, _) -> ())
          z_node.Dep_analysis.nn_children)
      !universe
  done ;
  (* Finally, return the visible universe. *)
  !universe
;;



(* *********************************************************************** *)
(** {b Descr} Elements of the minimal Coq typing environment for methods.
    We can't directly use [Env.TypeInformation.species_field] because they
    can't make appearing the fact that the carrier belongs to the minimal
    environment even if not *defined* (remind that in species fields, if
    "rep" appears then is it *defined* otherwise; it is silently
    declared and does't appear in the list of fields).

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
type min_coq_env_element =
  | MCEE_Declared_carrier    (** The carrier belongs to the environment but
                                 only via a decl-dependency. Hence it doesn't
                                 need to be explicitely defined, but need to
                                 be in the environment. *)
  | MCEE_Defined_carrier of Types.type_scheme (** The carrier belongs to the
               environment via at least a def-dependency. Then is have to
               be explicitely declared. *)
  | MCEE_Declared_computational of
      (Parsetree.vname * Types.type_scheme) (** Abstract computational method,
           i.e. abstracted Let or abstracted Let_rec or Sig other than "rep". *)
  | MCEE_Defined_computational of
      (Parsetree.qualified_species *
       Parsetree.vname * Types.type_scheme)  (** Defined computational method,
           i.e. Let or Let_rec. *)
  | MCEE_Declared_logical of
      (Parsetree.vname * Parsetree.prop)  (** Abstract logical property,
           i.e. Property or abstracted Theorem. *)
  | MCEE_Defined_logical of      (** Defined logical property, i.e. Theorem. *)
      (Parsetree.qualified_species * Parsetree.vname * Parsetree.prop)
;;



(* *********************************************************************** *)
(* in_the_universe_because Universe.t ->                                   *)
(*   Env.TypeInformation.species_field list ->                             *)
(*     min_coq_env_element list                                            *)
(** {b Descr} Compute the minimal Coq typing environment for a field whose
    visible universe is passed as [universe]. Proceeds following Virgile
    Prevosto's Phd, page 116, definition 58, section 6.4.4.
    If the carrier belongs to this list, then it ALWAYS is in head.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let minimal_typing_environment universe species_fields =
  (* A local function to process one let-binding. Handy to *)
  (* factorize code for both [Let] and [Let_rec] fields.   *)
  let process_one_let_binding l_binding =
    try
      let (from, n, _, sch, _, _) = l_binding in
      let reason = Universe.find n universe in
      if reason = IU_only_decl then
        (* Keep in the environment, but as abstracted. *)
        [MCEE_Declared_computational (n, sch)]
      else
        (* Otherwise, keep the full definition. *)
        [MCEE_Defined_computational (from, n, sch)]
    with Not_found ->
      (* Not in the universe. Hence not in the minimal typing env. *)
      [] in
  (* We make a temporary flag that will remind if "rep" appears among the *)
  (* fields. This is a bit casual, but this avoid searching again in the  *)
  (* list for this field. We need this because "rep" will be processed    *)
  (* aside the other Sigs. See below.                                     *)
  let found_rep_field = ref None in
  (* Now the local recursive function that will examine each species field. *)
  let rec build = function
   | [] -> []
   | h :: q ->
       let h' =
         (match h with
          | Env.TypeInformation.SF_sig (_, n, sch) ->
              (* We will process "rep" specially because it may belong to *)
              (* the universe although not present among the fields. See  *)
              (* below.                                                   *)
              if n <> (Parsetree.Vlident "rep") then
                (if Universe.mem n universe then
                  [MCEE_Declared_computational (n, sch)]
                else [])
              else (found_rep_field := Some sch ; [])
          | Env.TypeInformation.SF_let l_binding ->
              process_one_let_binding l_binding
          | Env.TypeInformation.SF_let_rec l ->
              List.flatten (List.map process_one_let_binding l)
          | Env.TypeInformation.SF_theorem (from, n, _, body, _, _) ->
              (begin
              try
                let reason = Universe.find n universe in
                if reason = IU_only_decl then
                  (* Keep in the environment, but as abstracted. *)
                  [MCEE_Declared_logical (n, body) ]
                else
                  (* Otherwise, keep the full definition. *)
                  [MCEE_Defined_logical (from, n, body) ]
              with Not_found ->
                (* Not in the universe. Hence not in the minimal typing env. *)
                []
              end)
          | Env.TypeInformation.SF_property (_, n, _, body, _) ->
              if Universe.mem n universe then
                [MCEE_Declared_logical (n, body)]
              else []) in
       h' @ (build q) in
  (* *************************** *)
  (* Now, let do the real job... *)
  let env_without_carrier = build species_fields in
  (* Now, we need to handle "rep" aside. In effect, it may belong to the   *)
  (* universe although not present among the fields. Moreover, if present  *)
  (* among the fields, one must make it "declared" or "defined" depending  *)
  (* on if it is in the universe via [IU_only_decl] or via [IU_trans_def]. *)
  (* The regular processing of Sigs doesn't take this last point in        *)
  (* account. So we proces "rep" aside here. If "rep" has to be added,     *)
  (* then, always put it in head !                                         *)
  try
    (begin
    let reason = Universe.find (Parsetree.Vlident "rep") universe in
    match (!found_rep_field, reason) with
     | (None, IU_only_decl) ->
         (* A decl-dependency was found even if "rep" is not defined. *)
         MCEE_Declared_carrier :: env_without_carrier
     | (None, IU_trans_def) ->
         (* Impossible to have a def-dependency if *)
         (* the carrier's structure is unknown !   *)
         assert false
     | ((Some _), IU_only_decl) ->
         (* A decl-dependency was found. No matter what "rep" is. *)
         MCEE_Declared_carrier :: env_without_carrier
     | ((Some sch), IU_trans_def) ->
         (* A def-dependency was found. So, record the carrier's structure. *)
         (MCEE_Defined_carrier sch) :: env_without_carrier
    end)
  with Not_found ->
    (* Not in the universe. Hence not in the minimal typing env. *)
    env_without_carrier
;;



(* ************************************************************************ *)
(** {b Descr} : Recover the parameters abstracted from our method to apply
    to a method generator. This function must only be used when the
    method whom we search the generator has been identified as inherited.
    Otherwise, the search will fail.

    {b Args} :
      - [~current_unit] : The current compilation unit.
      - [from_species] : The [Parsetree.qualified_species]
      - [method_name] : The name of the method the generator is looked for.
      - [env] : The current Coq code generation environment.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let find_inherited_method_generator_abstractions ~current_unit from_species
    method_name env =
  let (species_module, species_name) = from_species in
  let from_as_ident_desc =
    if species_module = current_unit then
      Parsetree.I_global (Parsetree.Vname species_name)
    else
      Parsetree.I_global (Parsetree.Qualified (species_module, species_name)) in
  (* This ident is temporary and created just to lookup in the environment. *)
  let from_as_ident = {
    Parsetree.ast_loc = Location.none ;
    Parsetree.ast_desc = from_as_ident_desc ;
    Parsetree.ast_doc = [] ;
    Parsetree.ast_type = Parsetree.ANTI_none } in
  try
    let species_info =
      Env.CoqGenEnv.find_species
        ~loc: Location.none ~current_unit from_as_ident env in
    (* Now, find the method in the species information. *)
    let method_info =
      List.find
        (fun { Env.CoqGenInformation.mi_name = n } -> n = method_name)
        species_info in
    method_info.Env.CoqGenInformation.mi_abstracted_methods
  with _ ->
    (* Because the generator is inherited, the species where it is hosted *)
    (* MUST be in the environment. Otherwise, something went wrong...     *)
    assert false
;;



(* ************************************************************************* *)
(* Species_gen_basics.species_compil_context ->                              *)
(*  Types.coq_print_context -> Env.CoqGenEnv.t ->                            *)
(*    min_coq_env_element list -> Parsetree.vname list ->                    *)
(*      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ->             *)
(*        Parsetree.vname -> Parsetree.expr -> let_connect: let_connector -> *)
(*          Parsetree.vname list -> Types.type_scheme ->                     *)
(*            Parsetree.vname list                                           *)
(** {b Descr} Gererate the Coq code for a method defined in the current
    species (i.e. not inherited). In fact, this generates the method
    generator for this method in this species.
    It returns the list of methods of ourselves we depend on and that were
    lambda-lifted according to th minimal coq environment.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
let generate_defined_method ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params name
    body ~let_connect params scheme =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let species_name = snd ctx.Species_gen_basics.scc_current_species in
  (* We need to check if "Self" is abstracted i.e. leads to an extra *)
  (* parameter "(abst_T : Set)" of the method. This is the case if   *)
  (* the method has a decl-dependency on the carrier and no def      *)
  (* dependency on the carrier (in other words, if the minimal       *)
  (* environment contains [MCEE_Declared_carrier]). If so, then      *)
  (* "Self" will be denoted (and printed) by "abst_T". Otherwise     *)
  (* "Self" will be denoted directly by "Self_T".                    *)
  let how_to_print_Self =
    if List.mem MCEE_Declared_carrier min_coq_env then Types.CSR_abst
    else Types.CSR_self in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Start the Coq function definition. *)
  (match let_connect with
   | LC_first_non_rec ->
       (* Beware that the definition corresponding to the *)
       (* method outside the record type has 2 "_"'s !    *)
       Format.fprintf out_fmter "@[<2>Definition %a__%a"
         Parsetree_utils.pp_vname_with_operators_expanded species_name
         Parsetree_utils.pp_vname_with_operators_expanded name
   | LC_first_rec ->
       (* [Unsure] *)
       (*
         Format.fprintf out_fmter "@[<2>let rec %a"
         Parsetree_utils.pp_vname_with_operators_expanded name
        *)
       failwith "TODO 1"
   | LC_following ->
       (* [Unsure] *)
       (*
         Format.fprintf out_fmter "@[<2>and %a"
         Parsetree_utils.pp_vname_with_operators_expanded name) ;
        *)
       failwith "TODO 2") ;
  (* Generate the parameters from the species parameters' methods we use. *)
  (* By the way, we get the stuff to add to the current collection        *)
  (* carrier mapping to make so the type expressions representing some    *)
  (* species parameter carrier types, will be automatically be mapped     *)
  (* onto our freshly created extra args.                                 *)
  let cc_mapping_extension =
    List.map
      (fun species_param_type_name ->
        let as_string =
          Parsetree_utils.vname_as_string_with_operators_expanded
            species_param_type_name in
        let param_name =  "_p_" ^ as_string ^ "_T" in
        (* First, generate the parameter. *)
        Format.fprintf out_fmter "@ (%s :@ Set)" param_name ;
        (* Return the stuff to extend the collection_carrier_mapping. *)
        ((ctx.Species_gen_basics.scc_current_unit, as_string),
         (param_name, Species_gen_basics.CCMI_is)))
      used_species_parameter_tys in
  (* Extend the collection_carrier_mapping of the context. *)
  let new_ctx = { ctx with
    Species_gen_basics.scc_collections_carrier_mapping =
    cc_mapping_extension @
    ctx.Species_gen_basics.scc_collections_carrier_mapping } in
  (* Same thing for the printing comtext. *) 
  let new_print_ctx = {
    print_ctx with
      Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      (List.map
         (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
         cc_mapping_extension)
      @ print_ctx.Types.cpc_collections_carrier_mapping } in
  List.iter
    (fun (species_param_name, meths) ->
      (* Each abstracted method will be named like "_p_", followed by *)
      (* the species parameter name, followed by "_", followed by the *)
      (* method's name.                                               *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, meth_ty) ->
          Format.fprintf out_fmter "@ (%s%a :@ %a)"
            prefix
            Parsetree_utils.pp_vname_with_operators_expanded meth
            (Types.pp_type_simple_to_coq
               new_print_ctx ~reuse_mapping: false
               ~self_as: how_to_print_Self)
            meth_ty)
        meths)
    dependencies_from_params ;
  (* Generate the parameters denoting methods of ourselves we *)
  (* depend on according the the minimal typing environment.  *)
  let abstracted_methods =
    List.flatten
      (List.map
         (function
           | MCEE_Defined_carrier _
           | MCEE_Defined_computational (_, _, _)
           | MCEE_Defined_logical (_, _, _) -> []
           | MCEE_Declared_carrier ->
               (* Note that by construction, the *)
               (* carrier is first in the env.   *)
               Format.fprintf out_fmter "@ (abst_T : Set)" ;
               [Parsetree.Vlident "rep"]
           | MCEE_Declared_computational (n, sch) ->
               (* Due to e decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               Format.fprintf out_fmter "@ (abst_%a : %a)"
                 Parsetree_utils.pp_vname_with_operators_expanded n
                 (Types.pp_type_simple_to_coq
                    new_print_ctx ~reuse_mapping: false
                    ~self_as: how_to_print_Self)
                 ty ;
               [n]
           | MCEE_Declared_logical (n, b) ->
               Format.fprintf out_fmter "@ (%a :@ "
                 Parsetree_utils.pp_vname_with_operators_expanded n ;
               Species_record_type_generation.generate_prop
                 new_ctx ~local_idents: [] ~self_as: how_to_print_Self env
                 b ;
               Format.fprintf out_fmter ")" ;
               [n])
         min_coq_env) in
  (* Add the parameters of the let-binding with their type.   *)
  (* Ignore the result type of the "let" if it's a function   *)
  (* because we never print the type constraint on the result *)
  (* of the "let". We only print them in the arguments of the *)
  (* let-bound ident.                                         *)
  (* Because methods are not polymorphic, one should never    *)
  (* have instanciate variables. We just check for this.      *)
  let (params_with_type, ending_ty_opt, instanciated_vars) =
    Misc_ml_generation.bind_parameters_to_types_from_type_scheme
      (Some scheme) params in
  assert (instanciated_vars = []) ;
  let ending_ty =
    (match ending_ty_opt with
     | None ->
         (* Because we always provide a type scheme (a [Some ...]), one *)
         (* must always be returned a type, i.e, something [Some ...].  *)
         assert false
     | Some t -> t) in
  (* We are printing each parameter's type. These types in fact belong *)
  (* to a same type scheme. Hence, they may share variables together.  *)
  (* For this reason, we first purge the printing variable mapping and *)
  (* after, activate its persistence between each parameter printing.  *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  List.iter
    (fun (param_vname, opt_param_ty) ->
      match opt_param_ty with
       | Some param_ty ->
           Format.fprintf out_fmter "@ (%a : %a)"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname
             (Types.pp_type_simple_to_coq
                new_print_ctx ~reuse_mapping: true
                ~self_as: how_to_print_Self)
             param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Now, we print the ending type of the method. *)
  Format.fprintf out_fmter " :@ %a :=@ "
    (Types.pp_type_simple_to_coq
       new_print_ctx ~reuse_mapping: true ~self_as: how_to_print_Self)
    ending_ty ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should *)
  (* not be useful because the other guys usign printing should manage   *)
  (* this themselves (as we did just above by cleaning before activating *)
  (* the sharing), but anyway, it is safer an not costly. So...          *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  (* Generates the body's code of the method.                       *)
  (* No local idents in the context because we just enter the scope *)
  (* of a species fields and so we are not under a core expression. *)
  Species_record_type_generation.generate_expr
    new_ctx ~local_idents: [] ~self_as: how_to_print_Self env body ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n" ;
  abstracted_methods
;;



(* ********************************************************************** *)
(** {b Descr} Generate the coq code for a method of the current species.
    If the method is defined in this species, then it generates the
    method generator. If the method is inherited, it recovers the
    methods abstracted in the generator without generating again the code.
    And in any case, it generate the local definition "self_..." by
    applying the generator to the local definitions.

    {b Rem} Not exported outside this module.                             *)
(* ********************************************************************** *)
let generate_one_field_binding ctx print_ctx env min_coq_env ~let_connect
    used_species_parameter_tys dependencies_from_params
    (from, name, params, scheme, body, _) =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  (* First of all, only methods defined in the current species must *)
  (* be generated. Inherited methods ARE NOT generated again !      *)
  let abstracted_methods =
    if from = ctx.Species_gen_basics.scc_current_species then
      generate_defined_method
        ctx print_ctx env min_coq_env used_species_parameter_tys
        dependencies_from_params name body ~let_connect params scheme
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited but not (re)-declared uses inherited \
           generator.@."
          Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* Recover the arguments for abstracted methods *)
      (* of self in the inherited generator.          *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Species_gen_basics.scc_current_unit from name env
      end) in
  (* In any case, if the method is declared or inherited, we generate the *)
  (* "Let self_..." by applying the right method generator. If the method *)
  (* defined at this level, then it's the generator created just by the   *)
  (* above process. Otherwise, it's the generator of the species that     *)
  (* provides the method by inheritance.                                  *)
  (* The creation of this "Let" even if the species is not fully defined  *)
  (* is required in case where a "property" uses the current method. In   *)
  (* effect, because in "property"s we don't lambda-lift, to keep late    *)
  (* binding, "property"s always use the "self_..." Coq "Variable"        *)
  (* representing the method.                                             *)
  Format.fprintf out_fmter "@[<2>Let self_%a :=@ "
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* The method generator's name... If the generator *)
  (* is in another module, then qualify its name.    *)
  if (fst from) <> ctx.Species_gen_basics.scc_current_unit then
    Format.fprintf out_fmter "%s.%a" (fst from)
      Parsetree_utils.pp_vname_with_operators_expanded (snd from)
  else
    Format.fprintf out_fmter "%a"
      Parsetree_utils.pp_vname_with_operators_expanded (snd from) ;
  Format.fprintf out_fmter "__%a"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Now, apply to each extra parameter coming from the lambda liftings. *)
  (* First, the extra arguments that represent the types of the species   *)
  (* parameters used in the method. It is always the species name + "_T". *)
  List.iter
     (fun species_param_type_name ->
        Format.fprintf out_fmter "@ %a_T"
          Parsetree_utils.pp_vname_with_operators_expanded
          species_param_type_name)
  used_species_parameter_tys ;
  (* Next, the extra arguments due to the species parameters methods we *)
  (* depends on. They are "Variables" previously declared and named:     *)
  (* species parameter name + "_" + method name.                         *)
  List.iter
    (fun (species_param_name, meths_from_param) ->
      let prefix = Parsetree_utils.name_of_vname species_param_name in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s_%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths_from_param)
    dependencies_from_params ;
  (* Next, the extra arguments due to methods of ourselves we depend on. *)
  (* They are always present in the species under the name "self_...".   *)
  List.iter
    (fun dep_meth_vname ->
      if dep_meth_vname = Parsetree.Vlident "rep" then
        Format.fprintf out_fmter "@ self_T"
      else
        Format.fprintf out_fmter "@ self_%a"
          Parsetree_utils.pp_vname_with_operators_expanded dep_meth_vname)
    abstracted_methods ;
  Format.fprintf out_fmter ".@]@\n" ;
  (* Finally, return the methods we found abstracted in the minimal Coq *)
  (* typing environment and that leaded to extra parameters via         *)
  (* lambda-lifting.                                                    *)
  abstracted_methods
;;



(* ************************************************************************ *)
(* Parsetree.vname -> compiled_species_fields list -> compiled_field_memory *)
(** {b Descr} : Search for the [compiled_field_memory] of the method [name]
    in the list [fields]. This function is used to recover while generating
    a generator application which arguments it takes.
    Obviously the [name] must be found because by construction of the
    FoCaL model, generators somebody depends on ARE created before this
    somebody.
    Basically, while processing a field, one will search for its
    dependencies among the list of the previously compiled fields of the
    species. And this list will grow wih the newly ompiled field and wil be
    passed to compile the next field on the species. And so on...

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let find_compiled_field_memory name fields =
  let rec find = function
    | [] -> assert false
    | h :: q ->
        (begin
        match h with
         | CSF_let field_memory
         | CSF_theorem field_memory ->
             if field_memory.cfm_method_name = name then field_memory
             else find q
         | CSF_let_rec fields_memories ->
             (begin
             try
               List.find (fun fm -> fm.cfm_method_name = name) fields_memories
             with Not_found -> find q
             end)
         | _ -> find q
        end) in
  find fields
;;



let generate_theorem ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields
    (from, name, prop, _) =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  let curr_species_name = (snd ctx.Species_gen_basics.scc_current_species) in
  (* A "theorem" defined in the species leads to a Coq *)
  (* "Theorem" enclosed in a dedicated "Section".      *)
  let abstracted_methods =
    if from = ctx.Species_gen_basics.scc_current_species then
      (begin
      (* Just a bit of debug. *)
      if Configuration.get_verbose () then
        Format.eprintf "Generating Coq code for field '%a'.@."
          Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* Put an extra newline before the theorem to make some air ! *)
      Format.fprintf out_fmter "@\n(* From species %a. *)@\n"
        Sourcify.pp_qualified_species from ;
      (* Open a Coq "Section". *)
      Format.fprintf out_fmter "@[<2>Section %a.@\n"
        Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* For each method in the minimal Coq typing environment, if it is  *)
      (* only declared, then we abstract them (i.e. make a "Variable"     *)
      (* named "abst_" + the method name and bind its type).              *)
      (* Otherwise, we make a local definition using the method generator *)
      (* of this method. This generator one has obligatorily been created *)
      (* previously and used the effective generator induced by the       *)
      (* inheritance level the method was declared defined. Because we    *)
      (* always create the "local" method generators, (self_xxx) we are   *)
      (* sure that we can find and call it.                               *)
      (* We keep the name of these abstracted in order to generate in the *)
      (* theorem proof some artificial "assert" to force Coq to abstract  *)
      (* the related "Variable"s even if they are not used (in fact, by   *)
      (* using, these artificial "assert", they *get* used, hence         *)
      (* abstracted by Coq !).                                            *)
      let abstracted_methods =
        List.flatten
          (List.map
             (function
               | MCEE_Declared_carrier ->
                   (* Generate a comment before the Let. *)
                   Format.fprintf out_fmter
                     "(* Due to a decl-dependency on 'rep'. *)@\n" ;
                   Format.fprintf out_fmter "Variable abst_T : Set.@\n" ;
                   (* Carrier is abstract. *)
                   [Parsetree.Vlident "rep"]
               | MCEE_Defined_carrier sch ->
                   (* Generate a comment before the Let. *)
                   Format.fprintf out_fmter
                     "(* Due to a def-dependency on 'rep'. *)@\n" ;
                   let ty = Types.specialize sch in
                   Format.fprintf out_fmter "@[<2>Let abst_T :=@ %a.@]@\n"
                     (Types.pp_type_simple_to_coq
                        print_ctx ~reuse_mapping: false
                        ~self_as: Types.CSR_species)
                     ty ;
                   []
               | MCEE_Declared_computational (name, sch) ->
                   (* Generate a comment before the variable. *)
                   Format.fprintf out_fmter
                     "(* Due to a decl-dependency on '%a'. *)@\n"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   let ty = Types.specialize sch in
                   (* "Self" is always represented by "abst_T" in "Variables" *)
                   (* representing the decl-dependencies of a theorem.        *)
                   Format.fprintf out_fmter "@[<2>Variable abst_%a :@ %a.@]@\n"
                     Parsetree_utils.pp_vname_with_operators_expanded name
                     (Types.pp_type_simple_to_coq
                        print_ctx ~reuse_mapping: false
                        ~self_as: Types.CSR_abst)
                     ty ;
                   (* Method abstracted by a "Variable". Hence will *)
                   (* be an argument of the theorem generator.      *)
                   [name]
               | MCEE_Defined_computational (from, name, sch) ->
                   (* Generate a comment before the Let. *)
                   Format.fprintf out_fmter
                     "(* Due to a def-dependency on '%a'. *)@\n"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   let ty = Types.specialize sch in
                   (* Now, create the definition using the method generator. *)
                   Format.fprintf out_fmter "@[<2>Let abst_%a :@ %a :=@ "
                     Parsetree_utils.pp_vname_with_operators_expanded name
                     (Types.pp_type_simple_to_coq
                        print_ctx ~reuse_mapping: false
                        ~self_as: Types.CSR_abst)
                     ty ;
                   (* Generate the application of the method generator. *)
                   if (fst from) <> ctx.Species_gen_basics.scc_current_unit then
                     Format.fprintf out_fmter "%s.%a" (fst from)
                       Parsetree_utils.pp_vname_with_operators_expanded
                       (snd from)
                   else
                     Format.fprintf out_fmter "%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       (snd from) ;
                   Format.fprintf out_fmter "__%a"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   (* Now, recover from the already generated fields, *)
                   (* what to apply to this generator.                *)
                   let memory =
                     find_compiled_field_memory name generated_fields in
                   (* We first instanciate the parameters corresponding to   *)
                   (* the carriers types of species parameters and appearing *)
                   (* in the method's type.                                  *)
                   List.iter
                     (fun species_param_type_name ->
                        (* [Unsure] Je voudrais bien trouver un cas qui *)
                        (* passe là-dedans !!! *)
                        Format.fprintf out_fmter "@ %a_T"
                          Parsetree_utils.pp_vname_with_operators_expanded
                          species_param_type_name)
                     memory.cfm_used_species_parameter_tys ;
                   (* Now apply the abstracted methods from the species *)
                   (* parameters we depend on.                          *)
                   List.iter
                     (fun (species_param_name, meths_from_param) ->
                       let prefix =
                         Parsetree_utils.name_of_vname species_param_name in
                       Parsetree_utils.DepNameSet.iter
                         (fun (meth, _) ->
                           Format.fprintf out_fmter "@ %s_%a"
                             prefix
                             Parsetree_utils.pp_vname_with_operators_expanded
                             meth)
                         meths_from_param)
                     memory.cfm_dependencies_from_parameters ;
                   List.iter
                     (fun dep_name ->
                       if dep_name = (Parsetree.Vlident "rep") then
                         Format.fprintf out_fmter "@ abst_T"
                       else
                         Format.fprintf out_fmter "@ abst_%a"
                           Parsetree_utils.pp_vname_with_operators_expanded
                           dep_name)
                     memory.cfm_coq_min_typ_env_names ;
                   (* End the application of the generator. *)
                   Format.fprintf out_fmter ".@]@\n" ;
                   (* Method not abstracted. Hence not an *)
                   (* argument of the theorem generator.  *)
                   []
               | MCEE_Defined_logical (from, name, body) ->
                   (* Generate a comment before the Let. *)
                   Format.fprintf out_fmter
                     "(* Due to a def-dependency on '%a'. *)@\n"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   (* Now, create the definition using the method generator. *)
                   Format.fprintf out_fmter "@[<2>Let abst_%a :@ "
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   Species_record_type_generation.generate_prop
                     ctx ~local_idents: [] ~self_as: Types.CSR_abst env body ;
                   Format.fprintf out_fmter " :=@ " ;
                   (* Generate the application of the method generator.   *)
                   (* Since one cannot depend from something that is not  *)
                   (* in our inheritance and because generators we depend *)
                   (* on are obligatorily generated before, we always use *)
                   (* the theorem generator.                              *)
                   if (fst from) <> ctx.Species_gen_basics.scc_current_unit then
                     Format.fprintf out_fmter "%s.%a" (fst from)
                       Parsetree_utils.pp_vname_with_operators_expanded
                       (snd from)
                   else
                     Format.fprintf out_fmter "%a"
                       Parsetree_utils.pp_vname_with_operators_expanded
                       (snd from) ;
                   Format.fprintf out_fmter "__%a"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   (* Now, recover from this theorem's minimal environment, *)
                   (* what to apply to this generator.                      *)
                   let memory =
                     find_compiled_field_memory name generated_fields in
                   List.iter
                     (fun n ->
                       if n = (Parsetree.Vlident "rep") then
                         Format.fprintf out_fmter "@ abst_T"
                       else
                         Format.fprintf out_fmter "@ abst_%a"
                           Parsetree_utils.pp_vname_with_operators_expanded n)
                     memory.cfm_coq_min_typ_env_names ;
                   (* End the application of the generator. *)
                   Format.fprintf out_fmter ".@]@\n" ;
                   (* Method not abstracted. Hence not an *)
                   (* argument of the theorem generator.  *)
                   []
               | MCEE_Declared_logical (name, body) ->
                   (* The type of a property or a theorem is  *)
                   (* its body, not its ML type !             *)
                   (* Generate a comment before the variable. *)
                   Format.fprintf out_fmter
                     "(* Due to a decl-dependency on '%a'. *)@\n"
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   (* "Self" is always represented by "abst_T" in "Variables" *)
                   (* representing the decl-dependencies of a theorem.        *)
                   Format.fprintf out_fmter "@[<2>Variable abst_%a :@ "
                     Parsetree_utils.pp_vname_with_operators_expanded name ;
                   (* Rem: no local idents in the context at this point. *)
                   Species_record_type_generation.generate_prop
                     ctx ~local_idents: [] ~self_as: Types.CSR_abst env body ;
                   Format.fprintf out_fmter ".@]@\n" ;
                   (* Method abstracted by a "Variable". Hence will *)
                   (* be an argument of the theorem generator.      *)
                   [name])
             min_coq_env) in
      (* Finally, the theorem itself. Inside, any method of "Self" is *)
      (* abstracted (i.e. as if it was lambda-lifted), hence named    *)
      (* "abst_xxx". That's why we use the mode [Types.CSR_abst].     *)
      Format.fprintf out_fmter "@[<2>Theorem %a__%a :@ "
        Parsetree_utils.pp_vname_with_operators_expanded curr_species_name
        Parsetree_utils.pp_vname_with_operators_expanded name ;
      Species_record_type_generation.generate_prop
        ~local_idents: [] ~self_as: Types.CSR_abst ctx env prop ;
      Format.fprintf out_fmter ".@]@\n" ;
      (* Generate "assert"s to be sure that Coq will really abstract *)
      (* in the section all the "Variable"s we created for detected  *)
      (* decl-dependencies.                                          *)
      List.iter
        (fun dep_name ->
          (* Skip the carrier in the list. *)
          if dep_name <> (Parsetree.Vlident "rep") then
            begin
            Format.fprintf out_fmter
              "@[(* Artificial@ use@ of@ method@ '%a'@ to@ ensure@ \
              abstraction@ of@ it's@ related@ variable@ in@ the@ theorem@ \
              section.@ *)@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded dep_name ;
            Format.fprintf out_fmter
              "@[<2>assert@ (___force_abstraction_%a :=@ abst_%a).@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded dep_name
              Parsetree_utils.pp_vname_with_operators_expanded dep_name
            end)
        abstracted_methods ;
      (* [Unsure] We now need to handle the proof. *)
      Format.fprintf out_fmter "apply basics.magic_prove.@\nQed.@\n" ;
      (* Close the theorem's "Section". *)
      Format.fprintf out_fmter "End %a.@]@\n"
        Parsetree_utils.pp_vname_with_operators_expanded name ;
      abstracted_methods
      end)
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited but not (re)-declared uses inherited \
          generator.@."
          Parsetree_utils.pp_vname_with_operators_expanded name ;
      (* Recover the arguments for abstracted methods *)
      (* of self in the inherited generator.          *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Species_gen_basics.scc_current_unit from name env
      end) in
  (* In any case, if the method is declared or inherited, we apply *)
  (* the theorem generator  to the "local" methods "self_xxx".     *)
  Format.fprintf out_fmter "@[<2>Let self_%a :@ "
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  Species_record_type_generation.generate_prop
    ~local_idents: [] ~self_as: Types.CSR_self ctx env prop ;
  (* The theorem generator's name... If the generator *)
  (* is in another module,  then qualify its name.    *)
  Format.fprintf out_fmter " :=@ " ;
  if (fst from) <> ctx.Species_gen_basics.scc_current_unit then
    Format.fprintf out_fmter "%s.%a" (fst from)
      Parsetree_utils.pp_vname_with_operators_expanded (snd from)
  else
    Format.fprintf out_fmter "%a"
      Parsetree_utils.pp_vname_with_operators_expanded (snd from) ;
  Format.fprintf out_fmter "__%a"
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Because we don't print any types, no need to extend the collection   *)
  (* carrier mapping at this point.                                       *)
  (* Now, apply to each extra parameter coming from the lambda liftings.  *)
  (* First, the extra arguments that represent the types of the species   *)
  (* parameters used in the method. It is always the species name + "_T". *)
  List.iter
     (fun species_param_type_name ->
       (* [Unsure] Je voudrais bien trouver un cas qui passe là-dedans !!! *)
       Format.fprintf out_fmter "@ %a_T"
         Parsetree_utils.pp_vname_with_operators_expanded
         species_param_type_name)
  used_species_parameter_tys ;
  (* Apply the species parameters' methods we use. *)
  List.iter
    (fun (species_param_name, meths) ->
      (* Each created variable was species parameter name, followed *)
      (* by "_", followed by the method's name.                     *)
      let prefix = (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths)
    dependencies_from_params ;
  (* And now apply its arguments with the local "Self_xxx" definitions. *)
  (* These arguments are those from the minimal environment that are    *)
  (* "only declared".                                                   *)
  List.iter
    (fun dep_meth_vname ->
      if dep_meth_vname = Parsetree.Vlident "rep" then
        Format.fprintf out_fmter "@ self_T"
      else
        Format.fprintf out_fmter "@ self_%a"
          Parsetree_utils.pp_vname_with_operators_expanded dep_meth_vname)
  abstracted_methods ;
  Format.fprintf out_fmter ".@]@\n" ;
  (* Return the names abstracted in the minimal typing environment. *)
  abstracted_methods
;;



(** generated_fields : The list of previous fields of the species that have
    already be generated. Used while generating theorems to know what to apply
        to the methods generators the theorem depends on. *)
let generate_methods ctx print_ctx env species_parameters_names
    all_fields generated_fields field =
  let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
  match field with
   | Env.TypeInformation.SF_sig (from, name, sch) ->
       (* "rep" is specially handled before, then ignore it now. *)
       if (Parsetree_utils.name_of_vname name) <> "rep" then
         (begin
         (* Because methods are not polymorphic, we take the shortcut not *)
         (* to verify if the need extra parameters to the type due to     *)
         (* polymorphism.                                                 *)
         let ty = Types.specialize sch in
         Format.fprintf out_fmter "(* From species %a. *)@\n"
           Sourcify.pp_qualified_species from ;
         (* Only declared method. Hence appears as a "Variable". In OCaml *)
         (* "sig"s are ignored and methods using them are lambda-lifted.  *)
         (* In Coq, we also lambda-lift this way methods, so the "sig"s   *)
         (* could seem to be ignored. However, it's impossible to lambda  *)
         (* lift in "property"s. So the variable is still needed. It will *)
         (* then be automatically abstracted by Coq in the "property".    *)
         (* Then, for methods generators where lambda-abstraction has     *)
         (* been done, we will apply these generators to this variable.   *)
         (* [Unsure] En fait, comme on n'utilise les générateurs de       *)
         (* méthodes dans les générateurs de collection et qu'à ce moment *)
         (* toutes les methodes sont définies, si ça se trouve on n'aura  *)
         (* même pas à utiliser la Variable, maisd directement la VRAIE   *)
         (* méthodes.                                                     *)
         Format.fprintf out_fmter
           "@[<2>Variable self_%a :@ %a.@]@\n"
           Parsetree_utils.pp_vname_with_operators_expanded name
           (Types.pp_type_simple_to_coq
              print_ctx ~reuse_mapping: false ~self_as: Types.CSR_self) ty
         end) ;
       (* Nothing to keep for the collection generator. *)
       CSF_sig name
   | Env.TypeInformation.SF_let (from, name, params, scheme, body, deps_rep) ->
       let (used_species_parameter_tys, dependencies_from_params,
            decl_children, def_children,  _) =
         Misc_ml_generation.compute_lambda_liftings_for_field
           ~current_unit: ctx.Species_gen_basics.scc_current_unit
           ~current_species: ctx.Species_gen_basics.scc_current_species
           species_parameters_names
           ctx.Species_gen_basics.scc_dependency_graph_nodes name
           (Misc_ml_generation.FBK_expr body) in
       (* Compute the visible universe of the method. *)
       let universe =
         visible_universe
           ctx.Species_gen_basics.scc_dependency_graph_nodes decl_children
           def_children in
       (* Now, its minimal Coq typing environment. *)
       let min_coq_env = minimal_typing_environment universe all_fields in
       (* No recursivity, then the method cannot call itself in its body *)
       (* then no need to set the [scc_lambda_lift_params_mapping] of    *)
       (* the context.                                                   *)
       let coq_min_typ_env_names =
         generate_one_field_binding
           ctx print_ctx env min_coq_env ~let_connect: LC_first_non_rec
           used_species_parameter_tys dependencies_from_params
           (from, name, params, scheme, body, deps_rep) in
       (* Now, build the [compiled_field_memory], even if the method  *)
       (* was not really generated because it was inherited.          *)
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_method_body = CMB_expr body ;
         cfm_used_species_parameter_tys = used_species_parameter_tys ;
         cfm_dependencies_from_parameters = dependencies_from_params ;
         cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
       CSF_let compiled_field
   | Env.TypeInformation.SF_let_rec _l ->
       (* [Unsure]. *)
       CSF_let_rec []
   | Env.TypeInformation.SF_theorem (from, name, _, prop, _, deps_on_rep) ->
       let (used_species_parameter_tys, dependencies_from_params,
            decl_children, def_children, _) =
         Misc_ml_generation.compute_lambda_liftings_for_field
           ~current_unit: ctx.Species_gen_basics.scc_current_unit
           ~current_species: ctx.Species_gen_basics.scc_current_species
           species_parameters_names
           ctx.Species_gen_basics.scc_dependency_graph_nodes name
           (Misc_ml_generation.FBK_prop prop) in
       (* Compute the visible universe of the theorem. *)
       let universe =
         visible_universe
           ctx.Species_gen_basics.scc_dependency_graph_nodes decl_children
           def_children in
       (* Now, its minimal Coq typing environment. *)
       let min_coq_env = minimal_typing_environment universe all_fields in
       let coq_min_typ_env_names =
         generate_theorem
           ctx print_ctx env min_coq_env used_species_parameter_tys
           dependencies_from_params generated_fields
           (from, name, prop, deps_on_rep) in
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_method_body = CMB_prop prop ;
         cfm_used_species_parameter_tys = used_species_parameter_tys ;
         cfm_dependencies_from_parameters = dependencies_from_params ;
         cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
       CSF_theorem compiled_field
   | Env.TypeInformation.SF_property (from, name, _, prop, _) ->
       (* [Unsure] Pas besoin de connaitre les dépendances sur "rep" ? *)
       (* "Property"s lead to a Coq "Hypothesis". *)
       Format.fprintf out_fmter "(* From species %a. *)@\n"
         Sourcify.pp_qualified_species from ;
       Format.fprintf out_fmter
         "@[<2>Hypothesis self_%a :@ "
         Parsetree_utils.pp_vname_with_operators_expanded name ;
       Species_record_type_generation.generate_prop
         ~local_idents: [] ~self_as: Types.CSR_self ctx env prop ;
       Format.fprintf out_fmter ".@]@\n" ;
       CSF_property name
;;



(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
    the species definition parameters and the names to be used later during
    the Coq translation.
    For a species parameter [A is/in ... ], the name that will be used is
    the name of the species parameter + "_T". No need like in OCaml to add
    a stamp because we don't lowercase names. Hence parameters will never
    wear the same name.
    This avoids the need to remind the stamp of a "is" parameter that is
    used to make a "in" parameter. In effect, for the
    "species Me (Naturals is IntModel, n in Naturals)" code,
    "Naturals" would be mapped on "Naturals0" and then everywhere "Natural"
    was used in the FoCaL code, one should replace by "Naturals0" in the
    Coq code !

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, carrier_name), _, _) ->
          (* Now, build the "collection type" this name will be bound to. *)
          (* According to how the "collection type" of parameters are     *)
          (* built, this will be the couple of the current compilation    *)
          (* unit and the name of the parameter.                          *)
          let type_coll = (current_unit, carrier_name) in
          (* And now create the binding... Record that *)
          (* the parameter is a "is" parameter.        *)
          (type_coll, (carrier_name ^ "_T", Species_gen_basics.CCMI_is))
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name that will represent this parameter's *)
          (* carrier seen from Coq.                              *)
          let carrier_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't    *)
          (* need any species expression to annotate this parameter in the   *)
          (* Coq type expression annotating this parameter in the hosting    *)
          (* species record type: it will simply be of the type [type_coll]. *)
          (type_coll,
           (carrier_name ^"_T", Species_gen_basics.CCMI_in_or_not_param)))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ********************************************************************** *)
(* Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->          *)
(*   Env.CoqGenEnv.t                                                      *)
(** {b Descr} : This function extend the coq code generation envionnment
      for a species generation. Because in Coq we need information about
    the number of extra parameters to add to function idents due to the
    fact that in oq polymorphism is explicit, we need to make methods of
    a species known before generating its body. It's the same problem for
    the species's parameters that must be bound in the environment, in
    order to inductively known their methods.
    This function add all this information in the current environment and
    return the extended environment.
    Note that because in FoCaL methods are not polymorphic, the number
    of extra parameters due to polymorphism is trivially always 0.

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let extend_env_for_species_def env species_descr =
  (* We first add the species methods. Because methods are not polymorphic,  *)
  (* we can safely bind them to 0 extra parameters-induced-by-polymorphism.  *)
  let species_methods_names =
    Dep_analysis.ordered_names_list_of_fields
      species_descr.Env.TypeInformation.spe_sig_methods in
  let env_with_methods_as_values =
    List.fold_left
      (fun accu_env (m_name, _) -> Env.CoqGenEnv.add_value m_name 0 accu_env)
      env
      species_methods_names in
  (* Now, add the species's parameters in the environment. And do not *)
  (* [fold_right] otherwise species will be inserted in reverse order *)
  (* in the environment !                                             *)
  List.fold_left
    (fun accu_env species_param ->
      match species_param with
       | Env.TypeInformation.SPAR_in _ ->
           (* "In" parameters are not species. They are "values" of *)
           (* species, "instances". Hence they do not lead to any   *)
           (* species in the environment.                           *)
           accu_env
       | Env.TypeInformation.SPAR_is ((_, param_name), param_methods, _) ->
           let methods_names =
             Dep_analysis.ordered_names_list_of_fields param_methods in
           (* A "is" parameter is a collection. Hence it is fully *)
           (* instanciated and doesn't have anymore lifted extra  *)
           (* parameters. Then the builf [method_info] is trivial *)
           (* empty about [mi_dependencies_from_parameters] and   *)
           (* [mi_abstracted_methods].                            *)
           let bound_methods =
             List.map
               (fun (n, _) -> {
                 Env.CoqGenInformation.mi_name = n ;
                 Env.CoqGenInformation.mi_dependencies_from_parameters = [] ;
                 Env.CoqGenInformation.mi_abstracted_methods = [] })
               methods_names in
           (* Because species names are capitalized, we explicitely build *)
           (* a [Parsetree.Vuident] to wrap the species name string.      *)
           Env.CoqGenEnv.add_species
             ~loc: Location.none (Parsetree.Vuident param_name)
             bound_methods accu_env)
    env_with_methods_as_values
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ********************************************************************** *)
(* Format.formatter -> Types.coq_print_context ->                         *)
(*   Env.TypeInformation.species_field list -> unit                       *)
(** {b Descr} : Search for an explicit representation of "Self" among the
    [fields]. If one is found, then generate the Coq code that "Let"-bind
    "Self_T" to the carrier's type representation in Coq.
    If none found, then generate a Coq "Variable" of type "Set".
    This function must be called before generating the species other
    fields because it defines the "Self" representation in Coq and this
    representation may be required to type the other methods. Because
    this representation cant depend of the other methods, it can always
    be processed first (no mutual dependencies between "Self" and
    methods).

    {b Rem} : Not exported outside this module.                           *)
(* ********************************************************************** *)
let generate_self_representation out_fmter print_ctx species_fields =
  let rec rec_find = function
    | [] ->
        (* No explicit "rep" structure found. Then, generate a Coq variable. *)
        Format.fprintf out_fmter "@\n(* Carrier representation. *)@\n" ;
        Format.fprintf out_fmter "@[<2>Variable self_T : Set.@]@\n@\n"
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_sig (_, (Parsetree.Vlident "rep"), sch) ->
             (* We finally found an explicit representation of the carrier. *)
             let (type_from_scheme, generalized_instanciated_vars) =
               Types.specialize_n_show_instanciated_generalized_vars sch in
             (* Because "rep" is never polymorphic, its type must never *)
             (* contain instanciated variables coming from the scheme.  *)
             assert (generalized_instanciated_vars = []) ;
             Format.fprintf out_fmter "@\n(* Carrier representation. *)@\n" ;
             (* We print the "rep"'s type using the [CSR_self] mode but in    *)
             (* fact, because the carrier can't be recursive, it can't        *)
             (* appear in it's own structure, and the mode has no importance. *)
             Format.fprintf out_fmter "@[<2>Let self_T : Set :=@ %a.@]@\n@\n"
               (Types.pp_type_simple_to_coq
                  print_ctx ~reuse_mapping: false ~self_as: Types.CSR_self)
               type_from_scheme
         | _ -> rec_find q
        end) in
  rec_find species_fields
;;



(* [Unsure] On calcule 2 fois compute_lambda_liftings_for_field. Il faudra
  factoriser ! *)
let generate_variables_for_species_parameters_methods ctx print_ctx
    species_parameters_names methods =
  (* To keep tail-rec, we will accumulate by side effect. *)
  let accu_found_dependencies =
    ref ([] : (Parsetree.vname * Parsetree_utils.DepNameSet.t) list) in
  (* We first harvest the list of all methods from species parameters our   *)
  (* fields depend on. Hence, we get a list a list of (species parameter    *)
  (* names * the set of its methods we depend on). This list would need to  *)
  (* be cleaned-up to prevent doubles. Instead, of cleaning it, we just     *)
  (* avoid generating several times the same "Variable" by recording those  *)
  (* already seen.                                                          *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (_, _, _) -> ()
      | Env.TypeInformation.SF_let (_, name, _, _, body, _) ->
          let (_, dependencies_from_params, _, _, _) =
            Misc_ml_generation.compute_lambda_liftings_for_field
              ~current_unit: ctx.Species_gen_basics.scc_current_unit
              ~current_species: ctx.Species_gen_basics.scc_current_species
              species_parameters_names
              ctx.Species_gen_basics.scc_dependency_graph_nodes name
              (Misc_ml_generation.FBK_expr body) in
          accu_found_dependencies :=
            dependencies_from_params @ !accu_found_dependencies
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (_, name, _, _, body, _) ->
              let (_, dependencies_from_params, _, _, _) =
                Misc_ml_generation.compute_lambda_liftings_for_field
                  ~current_unit: ctx.Species_gen_basics.scc_current_unit
                  ~current_species: ctx.Species_gen_basics.scc_current_species
                  species_parameters_names
                  ctx.Species_gen_basics.scc_dependency_graph_nodes name
                  (Misc_ml_generation.FBK_expr body) in
              accu_found_dependencies :=
                dependencies_from_params @ !accu_found_dependencies)
            l
      | Env.TypeInformation.SF_theorem (_, _name, _, _prop, _, _) ->
          (* [Unsure] *)
          ()
      | Env.TypeInformation.SF_property (_, _name, _, _prop, _) ->
          (* [Unsure] *)
          ())
    methods ;
  (* Now print the Coq "Variable"s, avoiding to print several times the same. *)
  (* The naming scheme of the methods is species param name + method name.    *)
  if !accu_found_dependencies <> [] then
    (begin
    let out_fmter = ctx.Species_gen_basics.scc_out_fmter in
    Format.fprintf out_fmter
      "(* Variable(s) induced by dependencies from species \
       parameter(s). *)@\n" ;
    let seen = ref [] in
    List.iter
      (fun (spe_param_name, deps_set) ->
        Parsetree_utils.DepNameSet.iter
          (fun (meth_name, meth_type) ->
            let remind_me = (spe_param_name, meth_name) in
            if not (List.mem remind_me !seen) then
              (begin
              seen := remind_me :: !seen ;
              (* Just a note: because in the type of the species parameter's *)
              (* method there is no reason to see "Self" appearing, the way  *)
              (* to print "Self" passed to [pp_type_simple_to_coq] has no    *)
              (* importance.                                                 *)
              Format.fprintf out_fmter "@[<2>Variable %a_%a :@ %a.@]@\n"
                Parsetree_utils.pp_vname_with_operators_expanded spe_param_name
                Parsetree_utils.pp_vname_with_operators_expanded meth_name
                (Types.pp_type_simple_to_coq
                   print_ctx ~reuse_mapping: false ~self_as: Types.CSR_self)
                meth_type
              end))
          deps_set)
      !accu_found_dependencies ;
    (* Just an extra line feed to make the source more readable. *)
    Format.fprintf out_fmter "@\n"
    end)
;;



let species_compile env ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the chapter encapsulating the species representation. *)
  let chapter_name =
    String.capitalize (Parsetree_utils.name_of_vname species_name) in
  Format.fprintf out_fmter "@[<2>Chapter %s.@\n" chapter_name ;
  (* Now, establish the mapping between collections available *)
  (* and the names representing their carrier.                *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Compute the list of names of parameters of the species. This   *)
  (* will be use to compute for each method the set of methods from *)
  (* the parameters the method depends on.                          *)
  let species_parameters_names =
    List.map
      (function
        | Env.TypeInformation.SPAR_in (n, _) -> n
        | Env.TypeInformation.SPAR_is ((_, n), _, _) -> Parsetree. Vuident n)
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Species_gen_basics.scc_current_unit = current_unit ;
    Species_gen_basics.scc_current_species = (current_unit, species_name) ;
    Species_gen_basics.scc_dependency_graph_nodes = dep_graph ;
    Species_gen_basics.scc_species_parameters_names =
      species_parameters_names ;
    Species_gen_basics.scc_collections_carrier_mapping =
      collections_carrier_mapping ;
    Species_gen_basics.scc_lambda_lift_params_mapping = [] ;
    Species_gen_basics.scc_out_fmter = out_fmter } in
  (* Insert in the environment the value bindings of the species methods *)
  (* and the species bindings for its parameters.                        *)
  let env' = extend_env_for_species_def env species_descr in
  (* The record type representing the species' type. *)
  Species_record_type_generation.generate_record_type ctx env' species_descr ;
  (* We now extend the collections_carrier_mapping with ourselves known. *)
  (* Hence, if we refer to our "rep" we will be directly mapped onto the *)
  (* "self_T" without needing to re-construct this name each time.       *)
  let collections_carrier_mapping' =
    ((current_unit, (Parsetree_utils.name_of_vname species_name)),
     ("self_T", Species_gen_basics.CCMI_in_or_not_param)) ::
    ctx.Species_gen_basics.scc_collections_carrier_mapping in
  let ctx' = { ctx with
     Species_gen_basics.scc_collections_carrier_mapping =
       collections_carrier_mapping' } in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx.Species_gen_basics.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx.Species_gen_basics.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      (* Throw the [collection_carrier_mapping_info] *)
      (* in the printing context.                    *)
      List.map
        (fun (ctype, (mapped_name, _)) -> (ctype, mapped_name))
        ctx'.Species_gen_basics.scc_collections_carrier_mapping } in
  (* Now we generate a "Variable" of type "Set" for each species's parameter *)
  (* with the same name used during the record type generation, i.e. the     *)
  (* parameter's name + "_T".                                                *)
  List.iter
    (fun p_vname ->
      let p_name = Parsetree_utils.name_of_vname p_vname in
      Format.fprintf out_fmter
        "(* Variable abstracting the species parameter [%s]. *)@\n" p_name ;
      Format.fprintf out_fmter "@[<2>Variable %s_T :@ Set.@]@\n" p_name)
    species_parameters_names ;
  (* Now, the methods of the species. We deal with "rep" first *)
  (* and then it will be ignore while generating the methods.  *)
  generate_self_representation
    out_fmter print_ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  (* Generate for each method of a species parameter we        *)
  (* decl-depend on and don't def-depend on, a Coq "Variable". *)
  generate_variables_for_species_parameters_methods
     ctx' print_ctx species_parameters_names
    species_descr.Env.TypeInformation.spe_sig_methods ;
  (* Now, generate the Coq code of the methods.  Do not [fold_right] *)
  (* otherwise the fields will be generated in reverse order.        *)
  let compiled_fields =
    List.fold_left
      (fun accu field ->
        (* Pass the accu to be able to remind the already generated fields. *)
        let compiled_field =
          generate_methods
            ctx' print_ctx env' species_parameters_names
            species_descr.Env.TypeInformation.spe_sig_methods accu field in
        (* Not efficient, but required to keep the fields in the right order. *)
        accu @ [compiled_field])
      []
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* The end of the chapter hosting the species. *)
  Format.fprintf out_fmter "@]End %s.@\n@." chapter_name ;
  (* Now, extract the fields names to create the [species_binding_info]. *)
  let species_binding_info =
    List.flatten
      (List.map
         (function
           | CSF_sig vname ->
               [{ Env.CoqGenInformation.mi_name = vname ;
                  Env.CoqGenInformation.mi_dependencies_from_parameters = [] ;
                  Env.CoqGenInformation.mi_abstracted_methods = [] }]
           | CSF_let compiled_field_memory
           | CSF_theorem compiled_field_memory ->
               [{ Env.CoqGenInformation.mi_name =
                    compiled_field_memory.cfm_method_name ;
                  Env.CoqGenInformation.mi_dependencies_from_parameters =
                    compiled_field_memory.cfm_dependencies_from_parameters ;
                  Env.CoqGenInformation.mi_abstracted_methods =
                    compiled_field_memory.cfm_coq_min_typ_env_names }]
           | CSF_let_rec compiled_field_memories ->
               List.map
                 (fun cfm ->
                   { Env.CoqGenInformation.mi_name = cfm.cfm_method_name ;
                     Env.CoqGenInformation.mi_dependencies_from_parameters =
                       cfm.cfm_dependencies_from_parameters ;
                     Env.CoqGenInformation.mi_abstracted_methods =
                       cfm.cfm_coq_min_typ_env_names })
                 compiled_field_memories
           | CSF_property vname ->
               [ { Env.CoqGenInformation.mi_name = vname ;
                   Env.CoqGenInformation.mi_dependencies_from_parameters = [] ;
                   Env.CoqGenInformation.mi_abstracted_methods = [] }])
         compiled_fields) in
  species_binding_info
;;
