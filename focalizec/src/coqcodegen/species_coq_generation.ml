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

(* $Id: species_coq_generation.ml,v 1.56 2008-05-19 10:18:10 pessaux Exp $ *)


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
  | CMB_logical_expr of Parsetree.logical_expr
;;


type compiled_field_memory = {
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_species ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
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
      parameter's name and the third is the set of methods the current
      method depends on from this species parameter. The second component of
      the tuple indicates if the species parameter is "in" or "is". *)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Parsetree_utils.species_param_kind *
     Parsetree_utils.DepNameSet.t) list ;
  (** The positional list of method names appearing in the minimal Coq typing
      environment. *)
  cfm_coq_min_typ_env_names : Parsetree.vname list
} ;;



type compiled_species_fields =
  | CSF_sig of Parsetree.vname
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
  | CSF_theorem of compiled_field_memory
  | CSF_property of compiled_field_memory
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
        (fst species_info) in
    method_info.Env.CoqGenInformation.mi_abstracted_methods
  with _ ->
    (* Because the generator is inherited, the species where it is hosted *)
    (* MUST be in the environment. Otherwise, something went wrong...     *)
    assert false
;;



(** Factorise la genération des abstrations pour let let let-rec.
    ~rec_let: bool disant si l'on travaille pour la génération d'un let
    ou d'un let-rec. Ca changera la façon dont on abstrait: "Variables" ou
    simple paramètres de la fonction. *)
let generate_defined_let_prelude ~rec_let ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Generate the parameters from the species parameters' types we use. *)
  (* By the way, we get the stuff to add to the current collection      *)
  (* carrier mapping to make so the type expressions representing some  *)
  (* species parameter carrier types, will be automatically be mapped   *)
  (* onto our freshly created extra args.                               *)
  let cc_mapping_extension =
    List.map
      (fun species_param_type_name ->
        let as_string =
          Parsetree_utils.vname_as_string_with_operators_expanded
            species_param_type_name in
        let param_name =  "_p_" ^ as_string ^ "_T" in
        (* First, generate the parameter or a "Variable". *)
        if rec_let then
          Format.fprintf out_fmter "@[<2>Variable %s :@ Set.@]@\n" param_name
        else Format.fprintf out_fmter "@ (%s :@ Set)" param_name ;
        (* Return the stuff to extend the collection_carrier_mapping. *)
        ((ctx.Context.scc_current_unit, as_string),
         (param_name, Types.CCMI_is)))
      used_species_parameter_tys in
  (* We need to check if "Self" is abstracted i.e. leads to an extra *)
  (* parameter "(abst_T : Set)" of the method. This is the case if   *)
  (* the method has a decl-dependency on the carrier and no def      *)
  (* dependency on the carrier (in other words, if the minimal       *)
  (* environment contains [MCEE_Declared_carrier]). If so, then      *)
  (* "Self" will be denoted (and printed) by "abst_T". Otherwise     *)
  (* "Self" will be denoted directly by "self_T".                    *)
  let self_carrier_mapping =
    if List.mem MinEnv.MCEE_Declared_carrier min_coq_env then
      Species_record_type_generation.make_Self_cc_binding_abst_T
        ~current_species: ctx.Context.scc_current_species
    else
      Species_record_type_generation.make_Self_cc_binding_self_T
        ~current_species: ctx.Context.scc_current_species in
  (* Extend the collection_carrier_mapping of the context with species *)
  (* parameters stuff and the way to print "Self".                     *)
  let new_ctx = { ctx with
    Context.scc_collections_carrier_mapping =
      self_carrier_mapping ::
       cc_mapping_extension @ ctx.Context.scc_collections_carrier_mapping } in
  (* Same thing for the printing comtext. *) 
  let new_print_ctx = {
    print_ctx with
      Types.cpc_collections_carrier_mapping =
        self_carrier_mapping :: cc_mapping_extension @
        print_ctx.Types.cpc_collections_carrier_mapping } in
  List.iter
    (fun (species_param_name, _, meths) ->
      (* Each abstracted method will be named like "_p_", followed by *)
      (* the species parameter name, followed by "_", followed by the *)
      (* method's name.                                               *)
      (* We don't care here about whether the species parameters is   *)
      (* "in" or "is".                                                *)
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, meth_ty) ->
          if rec_let then
            Format.fprintf out_fmter "@[<2>Variable %s%a :@ %a.@]@\n"
              prefix
              Parsetree_utils.pp_vname_with_operators_expanded meth
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: false)
              meth_ty
          else
            Format.fprintf out_fmter "@ (%s%a :@ %a)"
              prefix
              Parsetree_utils.pp_vname_with_operators_expanded meth
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: false)
              meth_ty)
        meths)
    dependencies_from_params ;
  (* Generate the parameters denoting methods of ourselves we *)
  (* depend on according the the minimal typing environment.  *)
  let abstracted_methods =
    List.flatten
      (List.map
         (function
           | MinEnv.MCEE_Defined_carrier sch ->
               (* Now, if "rep" is defined, then we generate an equivalence  *)
               (* between "self_T" and it's representation using the         *)
               (* abstracted types passed as arguments to the Definition to  *)
               (* represent carriers of the species parameters we depend on. *)
               let ty = Types.specialize sch in
               if rec_let then
                 Format.fprintf out_fmter "@[<2>Variable self_T := %a.@]@\n"
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false) ty
               else
                 Format.fprintf out_fmter "@ (self_T := %a)"
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false) ty ;
               []
           | MinEnv.MCEE_Defined_computational (_, _, _)
           | MinEnv.MCEE_Defined_logical (_, _, _) -> []
           | MinEnv.MCEE_Declared_carrier ->
               (* Note that by construction, the *)
               (* carrier is first in the env.   *)
               if rec_let then
                 Format.fprintf out_fmter "@[<2>Variable abst_T : Set.@]@\n"
               else Format.fprintf out_fmter "@ (abst_T : Set)" ;
               [Parsetree.Vlident "rep"]
           | MinEnv.MCEE_Declared_computational (n, sch) ->
               (* Due to a decl-dependency, hence: abstract. *)
               let ty = Types.specialize sch in
               if rec_let then
                 Format.fprintf out_fmter "@[<2>Variable abst_%a : %a.@]@\n"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty
               else
                 Format.fprintf out_fmter "@ (abst_%a : %a)"
                   Parsetree_utils.pp_vname_with_operators_expanded n
                   (Types.pp_type_simple_to_coq
                      new_print_ctx ~reuse_mapping: false)
                   ty ;
               [n]
           | MinEnv.MCEE_Declared_logical (n, b) ->
               if rec_let then
                 Format.fprintf out_fmter "@[<2>Variable %a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n
               else
                 Format.fprintf out_fmter "@ (%a :@ "
                   Parsetree_utils.pp_vname_with_operators_expanded n ;
               (* Since we are generating a "let" prelude, methods from      *)
               (* Self are printed "abst_XXX" since dependencies have        *)
               (* leaded to "Variables abst_XXX" before this new "Variable". *)
               Species_record_type_generation.generate_logical_expr
                 new_ctx ~local_idents: []
                 ~self_methods_status:
                   Species_record_type_generation.SMS_abstracted
                 ~in_hyp: false env b ;
               if rec_let then Format.fprintf out_fmter "@].@\n"
               else Format.fprintf out_fmter ")" ;
               [n])
         min_coq_env) in
  (abstracted_methods, new_ctx, new_print_ctx)
;;



(* ************************************************************************* *)
(* Context.species_compil_context ->                                         *)
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
let generate_defined_non_recursive_method ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params name
    body params scheme =
  let out_fmter = ctx.Context.scc_out_fmter in
  let species_name = snd ctx.Context.scc_current_species in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for field '%a'.@."
      Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Start the Coq function definition. *)
  (* Beware that the definition corresponding to the *)
  (* method outside the record type has 2 "_"'s !    *)
  Format.fprintf out_fmter "@[<2>Definition %a__%a"
    Parsetree_utils.pp_vname_with_operators_expanded species_name
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Generate the prelude of the method, i.e the sequence of parameters *)
  (* and their types induced by the various lamda-liftings.             *)
  (* By the way, we get updated in the [new_print_ctx] the way "Self"   *)
  (* must be printed.                                                   *)
  let (abstracted_methods, new_ctx, new_print_ctx) =
    generate_defined_let_prelude
      ~rec_let: false ctx print_ctx env min_coq_env
      used_species_parameter_tys dependencies_from_params in
  (* Add the parameters of the let-binding with their type.   *)
  (* Ignore the result type of the "let" if it's a function   *)
  (* because we never print the type constraint on the result *)
  (* of the "let". We only print them in the arguments of the *)
  (* let-bound ident.                                         *)
  (* Because methods are not polymorphic, one should never    *)
  (* have instanciate variables. We just check for this.      *)
  let (params_with_type, ending_ty_opt, instanciated_vars) =
    MiscHelpers.bind_parameters_to_types_from_type_scheme
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
             (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: true)
             param_ty
       | None ->
           Format.fprintf out_fmter "@ %a"
             Parsetree_utils.pp_vname_with_operators_expanded param_vname)
    params_with_type ;
  (* Now, we print the ending type of the method. *)
  Format.fprintf out_fmter " :@ %a :=@ "
    (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: true)
    ending_ty ;
  (* Now we don't need anymore the sharing. Hence, clean it. This should *)
  (* not be useful because the other guys usign printing should manage   *)
  (* this themselves (as we did just above by cleaning before activating *)
  (* the sharing), but anyway, it is safer an not costly. So...          *)
  Types.purge_type_simple_to_coq_variable_mapping () ;
  (* Generates the body's code of the method.                       *)
  (* No local idents in the context because we just enter the scope *)
  (* of a species fields and so we are not under a core expression. *)
  (* Since we are generating a "let", methods from Self are printed *)
  (* "abst_XXX" since dependencies have leaded to                   *)
  (* "Variables abst_XXX" before this new "Variable".               *)
  (match body with
   | Parsetree.BB_computational e ->
       Species_record_type_generation.generate_expr
         new_ctx ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~in_hyp: false env e
   | Parsetree.BB_logical p ->
       Species_record_type_generation.generate_logical_expr
         new_ctx ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_abstracted
         ~in_hyp: false env p) ;
  (* Done... Then, final carriage return. *)
  Format.fprintf out_fmter ".@]@\n" ;
  abstracted_methods
;;



(* ********************************************************************** *)
(** {b Descr} Generate the coq code for a non-recursive method of the
    current species.
    If the method is defined in this species, then it generates the
    method generator. If the method is inherited, it recovers the
    methods abstracted in the generator without generating again the code.
    And in any case, it generate the local definition "self_..." by
    applying the generator to the local definitions.

    {b Rem} Not exported outside this module.                             *)
(* ********************************************************************** *)
let generate_non_recursive_field_binding ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params
    (from, name, params, scheme, body) =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* First of all, only methods defined in the current species must *)
  (* be generated. Inherited methods ARE NOT generated again !      *)
  let abstracted_methods =
    if from = ctx.Context.scc_current_species then
      generate_defined_non_recursive_method
        ctx print_ctx env min_coq_env used_species_parameter_tys
        dependencies_from_params name body params scheme
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited from species '%a' but not (re)-declared uses \
          inherited generator.@."
          Parsetree_utils.pp_vname_with_operators_expanded name
          Sourcify.pp_qualified_species from ;
      (* Recover the arguments for abstracted methods *)
      (* of self in the inherited generator.          *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit from name env
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
  if (fst from) <> ctx.Context.scc_current_unit then
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
  (* depends on. They are "Variables" previously declared and named:    *)
  (* species parameter name + "_" + method name is coming from "is"     *)
  (* parameter of simply by the species parameter name if coming from a *)
  (* "in" parameter.                                                    *)
  (* Hence, here we care here about whether the species parameters is   *)
  (* "in" or "is" !                                                     *)
  List.iter
    (fun (species_param_name, species_param_kind, meths_from_param) ->
      match species_param_kind with
       | Parsetree_utils.SPK_is ->
           let prefix = Parsetree_utils.name_of_vname species_param_name in
           Parsetree_utils.DepNameSet.iter
             (fun (meth, _) ->
               (* Don't print the type to prevent being too verbose. *)
               Format.fprintf out_fmter "@ %s_%a"
                 prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
             meths_from_param
       | Parsetree_utils.SPK_in ->
           (* Since a "in" parameter does not have methods, the list should *)
           (* trivially be of length 1, with the name of the species.       *)
           (* The generated identifier's name is the parameter's name twice *)
           (* (because this last one is computed as the "stuff" a           *)
           (* dependency was found on, and inthe case of a "in"-parameter,  *)
           (* the dependency can only be on the parameter's value itself,   *)
           (* not on any method since there is none !).                     *)
           Parsetree_utils.DepNameSet.iter
             (fun (meth, _) ->
               (* Don't print the type to prevent being too verbose. *)
               Format.fprintf out_fmter "@ %a_%a"
                 Parsetree_utils.pp_vname_with_operators_expanded meth
                 Parsetree_utils.pp_vname_with_operators_expanded meth)
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



(* ************************************************************************* *)
(* Context.species_compil_context -> Types.coq_print_context ->              *)
(*   Env.CoqGenEnv.t -> min_coq_env_element list ->                          *)
(*     compiled_species_fields list -> Parsetree.qualified_species ->        *)
(*       Parsetree.vname -> Parsetree.logical_expr -> Parsetree.vname list   *)
(** {b Descr} Gererate the Coq code for a theorem defined in the current
    species (i.e. not inherited). In fact, this generates the theorem
    generator for this theorem in this species.
    It returns the list of methods of ourselves we depend on and that were
    abstracted by a "Variable abst_..." in the current Coq theorem's section
    according to the minimal coq environment.

    {b Rem}: Not exported outside this module.                               *)
(* ************************************************************************* *)
let generate_defined_theorem ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields
    from name logical_expr =
  let out_fmter = ctx.Context.scc_out_fmter in
  let curr_species_name = (snd ctx.Context.scc_current_species) in
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
  (* Generate the Variable from the species parameters' types we use.  *)
  (* By the way, we get the stuff to add to the current collection     *)
  (* carrier mapping to make so the type expressions representing some *)
  (* species parameter carrier types, will be automatically be mapped  *)
  (* onto our freshly created extra args.                              *)
  let cc_mapping_extension =
    List.map
      (fun species_param_type_name ->
        let as_string =
          Parsetree_utils.vname_as_string_with_operators_expanded
            species_param_type_name in
        let param_name =  "_p_" ^ as_string ^ "_T" in
        (* First, generate the Variable. *)
        Format.fprintf out_fmter
          "(* Due to a decl-dependency on species parameter carrier \
           type '%s'. *)@\n"
          as_string ;
        Format.fprintf out_fmter "Variable@ %s :@ Set.@\n" param_name ;
        (* Return the stuff to extend the collection_carrier_mapping. *)
        ((ctx.Context.scc_current_unit, as_string),
         (param_name, Types.CCMI_is)))
      used_species_parameter_tys in
  (* Extend the collection_carrier_mapping of the context *)
  (* with species parameters stuff.                       *)
  let new_ctx = { ctx with
    Context.scc_collections_carrier_mapping =
      cc_mapping_extension @
      ctx.Context.scc_collections_carrier_mapping } in
  (* Same thing for the printing context. *) 
  let new_print_ctx = {
    print_ctx with
      Types.cpc_collections_carrier_mapping =
        cc_mapping_extension @
        print_ctx.Types.cpc_collections_carrier_mapping } in
  (* For each method from the species parameters we depend on, we create a *)
  (* Variable named "_" + species parameter name + "_" + method name.      *)
  List.iter
    (fun (species_param_name, _, meths_from_param) ->
      let prefix = "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, meth_ty) ->
          Format.fprintf out_fmter
            "@[(* Due@ to@ a@ decl-dependency@ on@ method@ '%a'@ of@ \
            species@ parameter@ '%a'. *)@]@\n"
            Parsetree_utils.pp_vname_with_operators_expanded species_param_name
            Parsetree_utils.pp_vname_with_operators_expanded meth ;
          Format.fprintf out_fmter
            "@[<2>Variable %s_%a : %a.@]@\n"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth
            (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: false)
            meth_ty)
        meths_from_param)
    dependencies_from_params ;
  (* In order to print the Theorem, we must check if "rep" belongs to the *)
  (* minimal coq environment and hence has been abstacted to know how to  *)
  (* print "Self" (i.e. what to put in the [collection_carrier_mapping]). *)
  (* If it belongs, then "Self" must be printed "abst_T" otherwise        *)
  (* "self_T". For this, we maintain a dedicated boolean flag updated by  *)
  (* side effect.                                                         *)
  let carrier_belongs_to_min_coq_env = ref false in
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
           | MinEnv.MCEE_Declared_carrier ->
               (* Generate a comment before the Let. *)
               Format.fprintf out_fmter
                 "(* Due to a decl-dependency on 'rep'. *)@\n" ;
               Format.fprintf out_fmter "Variable abst_T : Set.@\n" ;
               (* Yep, carrier was seen in the minimal Coq environment. *)
               carrier_belongs_to_min_coq_env := true ;
               (* Carrier is abstract. *)
               [Parsetree.Vlident "rep"]
           | MinEnv.MCEE_Defined_carrier sch ->
               (* Generate a comment before the Let. *)
               Format.fprintf out_fmter
                 "(* Due to a def-dependency on 'rep'. *)@\n" ;
               let ty = Types.specialize sch in
               (* Self's methods are always named by using the species name. *)
               let new_print_ctx' = {
                 new_print_ctx with
                 Types.cpc_collections_carrier_mapping =
                   (Species_record_type_generation.
                      make_Self_cc_binding_current_species_T
                        ~current_species: ctx.Context.scc_current_species) ::
                   new_print_ctx.Types.cpc_collections_carrier_mapping } in
               Format.fprintf out_fmter "@[<2>Let abst_T :=@ %a.@]@\n"
                 (Types.pp_type_simple_to_coq
                    new_print_ctx' ~reuse_mapping: false)
                 ty ;
               (* Yep, carrier was seen in the minimal Coq environment. *)
               carrier_belongs_to_min_coq_env := true ;
               []
           | MinEnv.MCEE_Declared_computational (name, sch) ->
               (* Generate a comment before the variable. *)
               Format.fprintf out_fmter
                 "(* Due to a decl-dependency on '%a'. *)@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded name ;
               let ty = Types.specialize sch in
               (* "Self" is always represented by "abst_T" in "Variables" *)
               (* representing the decl-dependencies of a theorem.        *)
               let new_print_ctx' = {
                 new_print_ctx with
                 Types.cpc_collections_carrier_mapping =
                   (Species_record_type_generation.make_Self_cc_binding_abst_T
                    ~current_species: ctx.Context.scc_current_species)
                   :: new_print_ctx.Types.cpc_collections_carrier_mapping } in
               Format.fprintf out_fmter "@[<2>Variable abst_%a :@ %a.@]@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 (Types.pp_type_simple_to_coq
                    new_print_ctx' ~reuse_mapping: false) ty ;
               (* Method abstracted by a "Variable". Hence will *)
               (* be an argument of the theorem generator.      *)
               [name]
           | MinEnv.MCEE_Defined_computational (from, name, sch) ->
               (* Generate a comment before the Let. *)
               Format.fprintf out_fmter
                 "(* Due to a def-dependency on '%a'. *)@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded name ;
               let ty = Types.specialize sch in
               (* "Self" is always represented by "abst_T" in "Variables" *)
               (* representing the def-dependencies of a theorem.         *)
               let new_print_ctx' = {
                 new_print_ctx with
                 Types.cpc_collections_carrier_mapping =
                   (Species_record_type_generation.make_Self_cc_binding_abst_T
                      ~current_species: ctx.Context.scc_current_species)
                   :: new_print_ctx.Types.cpc_collections_carrier_mapping } in
               (* Now, create the definition using the method generator. *)
               Format.fprintf out_fmter "@[<2>Let abst_%a :@ %a :=@ "
                 Parsetree_utils.pp_vname_with_operators_expanded name
                 (Types.pp_type_simple_to_coq
                    new_print_ctx' ~reuse_mapping: false)
                 ty ;
               (* Generate the application of the method generator. *)
               if (fst from) <> new_ctx.Context.scc_current_unit then
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
               let memory = find_compiled_field_memory name generated_fields in
               (* We first instanciate the parameters corresponding to   *)
               (* the carriers types of species parameters and appearing *)
               (* in the method's type.                                  *)
               List.iter
                 (fun species_param_type_name ->
                   (* Because species parameters' carriers are mapped in   *)
                   (* the [cc_mapping_extension] to "_p_" + the species    *)
                   (* parameter's name + "T", we must apply the            *)
                   (* definition to arguments folowing this naming scheme. *)
                   Format.fprintf out_fmter "@ _p_%a_T"
                     Parsetree_utils.pp_vname_with_operators_expanded
                     species_param_type_name)
                 memory.cfm_used_species_parameter_tys ;
               (* Now apply the abstracted methods from the species *)
               (* parameters we depend on.                          *)
               (* [Unsure] Euh, on fait quoi des "in" ? *)
               List.iter
                 (fun (species_param_name, _, meths_from_param) ->
                   let prefix =
                     Parsetree_utils.name_of_vname species_param_name in
                   Parsetree_utils.DepNameSet.iter
                     (fun (meth, _) ->
                       Format.fprintf out_fmter "@ %s_%a"
                         prefix Parsetree_utils.pp_vname_with_operators_expanded
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
           | MinEnv.MCEE_Defined_logical (from, name, body) ->
               (* Generate a comment before the Let. *)
               Format.fprintf out_fmter
                 "(* Due to a def-dependency on '%a'. *)@\n"
                 Parsetree_utils.pp_vname_with_operators_expanded name ;
               (* Now, create the definition using the method generator. *)
               Format.fprintf out_fmter "@[<2>Let abst_%a :@ "
                 Parsetree_utils.pp_vname_with_operators_expanded name ;
               (* We are generating a Theorem, not a property, *)
               (* hence, not an Hypothesis.                    *)
               Species_record_type_generation.generate_logical_expr
                 new_ctx ~local_idents: []
                 ~self_methods_status:
                   Species_record_type_generation.SMS_abstracted
                 ~in_hyp: false env body ;
               Format.fprintf out_fmter " :=@ " ;
               (* Generate the application of the method generator.   *)
               (* Since one cannot depend from something that is not  *)
               (* in our inheritance and because generators we depend *)
               (* on are obligatorily generated before, we always use *)
               (* the theorem generator.                              *)
               if (fst from) <> new_ctx.Context.scc_current_unit then
                 Format.fprintf out_fmter "%s.%a" (fst from)
                   Parsetree_utils.pp_vname_with_operators_expanded (snd from)
               else
                 Format.fprintf out_fmter "%a"
                   Parsetree_utils.pp_vname_with_operators_expanded (snd from) ;
               Format.fprintf out_fmter "__%a"
                 Parsetree_utils.pp_vname_with_operators_expanded name ;
               (* Now, recover from this theorem's minimal environment, *)
               (* what to apply to this generator.                      *)
               let memory = find_compiled_field_memory name generated_fields in
               List.iter
                 (fun n ->
                   if n = (Parsetree.Vlident "rep") then
                     Format.fprintf out_fmter "@ abst_T"
                   else
                     Format.fprintf out_fmter "@ abst_%a"
                       Parsetree_utils.pp_vname_with_operators_expanded n)
                 memory.cfm_coq_min_typ_env_names ;
(* [Unsure] Et on n'applique pas à tout le reste du bastringue, comme dans
   le cas de MCEE_Defined_computational ??? *)
               (* End the application of the generator. *)
               Format.fprintf out_fmter ".@]@\n" ;
               (* Method not abstracted. Hence not an *)
               (* argument of the theorem generator.  *)
               []
           | MinEnv.MCEE_Declared_logical (name, body) ->
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
               Species_record_type_generation.generate_logical_expr
                 new_ctx ~local_idents: []
                 ~self_methods_status:
                   Species_record_type_generation.SMS_abstracted
                 ~in_hyp: true env body ;
               Format.fprintf out_fmter ".@]@\n" ;
               (* Method abstracted by a "Variable". Hence will *)
               (* be an argument of the theorem generator.      *)
               [name])
         min_coq_env) in
  (* Finally, the theorem itself. Inside, any method of "Self" is *)
  (* abstracted (i.e. as if it was lambda-lifted), hence named    *)
  (* "abst_xxx". That's why we use the mode [SMS_abstracted].     *)
  (* Attention, we must enrich the [collection_carrier_mapping]   *)
  (* with the way to print "Self". We use the dedicated boolean   *)
  (* flag [carrier_belongs_to_min_coq_env]. See comment at its    *)
  (* definition for more information.                             *)
  let new_ctx = {
    new_ctx with Context.scc_collections_carrier_mapping =
      (if !carrier_belongs_to_min_coq_env then
        Species_record_type_generation.make_Self_cc_binding_abst_T
          ~current_species: new_ctx.Context.scc_current_species
      else
        Species_record_type_generation.make_Self_cc_binding_self_T
          ~current_species: new_ctx.Context.scc_current_species)
      :: new_ctx.Context.scc_collections_carrier_mapping } in
  Format.fprintf out_fmter "@[<2>Theorem %a__%a :@ "
    Parsetree_utils.pp_vname_with_operators_expanded curr_species_name
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  Species_record_type_generation.generate_logical_expr
    ~local_idents: []
    ~self_methods_status: Species_record_type_generation.SMS_abstracted
    ~in_hyp: false new_ctx env logical_expr ;
  Format.fprintf out_fmter ".@]@\n" ;
  (* Generate "assert"s to be sure that Coq will really abstract *)
  (* in the section all the parameters for types containing      *)
  (* species parameters carriers.                                *)
  List.iter
    (fun species_carrier_ty_name ->
       Format.fprintf out_fmter
          "@[(* Artificial@ use@ of@ type@ '%a_T'@ to@ ensure@ \
          abstraction@ of@ it's@ related@ variable@ in@ the@ theorem@ \
          section.@ *)@]@\n"
          Parsetree_utils.pp_vname_with_operators_expanded
          species_carrier_ty_name ;
        Format.fprintf out_fmter
          "@[<2>assert@ (___force_abstraction_p_%a_T :=@ _p_%a_T).@]@\n"
          Parsetree_utils.pp_vname_with_operators_expanded
          species_carrier_ty_name
          Parsetree_utils.pp_vname_with_operators_expanded
          species_carrier_ty_name)
  used_species_parameter_tys ;
  (* Do the same thing with methods from the species parameters. *)
  List.iter
    (fun (species_param_name, _, meths_from_param) ->
      let prefix = "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          Format.fprintf out_fmter
            "@[(* Artificial@ use@ of@ method@ '%s_%a'@ to@ ensure@ \
             abstraction@ of@ it's@ related@ variable@ in@ the@ theorem@ \
             section.@ *)@]@\n"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth ;
          Format.fprintf out_fmter
            "@[<2>assert@ (___force_abstraction_%s_%a :=@ %s_%a).@]@\n"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        meths_from_param)
  dependencies_from_params ;
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
;;



let generate_theorem ctx print_ctx env min_coq_env
    used_species_parameter_tys dependencies_from_params generated_fields
    (from, name, logical_expr, _) =
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Modify the [collection_carrier_mapping] so that *)
  (* "Self" is printed in types by "abst_T". *)
  let ctx = {
    ctx with
    Context.scc_collections_carrier_mapping =
       (Species_record_type_generation.make_Self_cc_binding_abst_T
          ~current_species: ctx.Context.scc_current_species)
       :: ctx.Context.scc_collections_carrier_mapping } in
  (* A "theorem" defined in the species leads to a Coq *)
  (* "Theorem" enclosed in a dedicated "Section".      *)
  let abstracted_methods =
    if from = ctx.Context.scc_current_species then
      generate_defined_theorem 
        ctx print_ctx env min_coq_env used_species_parameter_tys
        dependencies_from_params generated_fields from name logical_expr
    else
      (begin
      (* Just a bit of debug/information if requested. *)
      if Configuration.get_verbose () then
        Format.eprintf
          "Field '%a' inherited from species '%a' but not (re)-declared uses \
          inherited generator.@."
          Parsetree_utils.pp_vname_with_operators_expanded name
          Sourcify.pp_qualified_species from ;
      (* Recover the arguments for abstracted methods *)
      (* of self in the inherited generator.          *)
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit from name env
      end) in
  (* In any case, if the method is declared or inherited, we apply *)
  (* the theorem generator to the "local" methods "self_xxx".      *)
  Format.fprintf out_fmter "@[<2>Let self_%a :@ "
    Parsetree_utils.pp_vname_with_operators_expanded name ;
  (* Extend the [collection_carrier_mapping] of the context in order to *)
  (* make "Self" now printed in types as "self_T".                      *)
  let ctx = {
    ctx with
      Context.scc_collections_carrier_mapping =
        (Species_record_type_generation.make_Self_cc_binding_self_T
          ~current_species: ctx.Context.scc_current_species)
        :: ctx.Context.scc_collections_carrier_mapping } in
  Species_record_type_generation.generate_logical_expr
(* [Unsure] Ici ~in_hyp: true est correct, mais le nom du flag n'est pas bon
   car il ne reflète pas vraiment le fait qu'on soit dans une Hypothesis.
   A changer ! *)
    ~local_idents: []
    ~self_methods_status: Species_record_type_generation.SMS_from_self
    ~in_hyp: true ctx env logical_expr ;
  (* The theorem generator's name... If the generator *)
  (* is in another module, then qualify its name.     *)
  Format.fprintf out_fmter " :=@ " ;
  if (fst from) <> ctx.Context.scc_current_unit then
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
  (* Apply the species parameters' methods we use. Here we care here *)
  (* about whether the species parameters is "in" or "is" !          *)
  List.iter
    (fun (species_param_name, species_param_kind, meths) ->
      match species_param_kind with
       | Parsetree_utils.SPK_is ->
           (* Each created variable was species parameter name, followed *)
           (* by "_", followed by the method's name.                     *)
           let prefix =
             (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
           Parsetree_utils.DepNameSet.iter
             (fun (meth, _) ->
               Format.fprintf out_fmter "@ %s%a"
                 prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
             meths
       | Parsetree_utils.SPK_in ->
           (* Since a "in" parameter does not have methods, the list should *)
           (* trivially be of length 1, with the name of the species.       *)
           (* The generated identifier's name is the parameter's name twice *)
           (* (because this last one is computed as the "stuff" a           *)
           (* dependency was found on, and inthe case of a "in"-parameter,  *)
           (* the dependency can only be on the parameter's value itself,   *)
           (* not on any method since there is none !).                     *)
           Parsetree_utils.DepNameSet.iter
             (fun (meth, _) ->
               Format.fprintf out_fmter "@ %a_%a"
                 Parsetree_utils.pp_vname_with_operators_expanded meth
                 Parsetree_utils.pp_vname_with_operators_expanded meth)
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




(* [Unsure] Factoriser en mettant un boolean disant si on est en mode Coq ou
   Caml car c'est franchement pareil que la fonction de même nom utilisée
   pour Caml ! *)
let make_params_list_from_abstraction_info ai =
  (* Build the list by side effect in reverse order for efficiency. *)
  let the_list_reversed = ref [] in
  (* First, abstract according to the species's parameters types the *)
  (* current method depends on.                                      *)
  List.iter
    (fun sp_param_ty_name ->
      (* Each abstracted type name is built like "_p_" + parameter's name *)
      (* + "_T".                                                         *)
      let llift_name =
        "_p_" ^
        (Parsetree_utils.vname_as_string_with_operators_expanded
           sp_param_ty_name) ^
        "_T" in
      the_list_reversed := llift_name :: !the_list_reversed)
    ai.Abstractions.ai_used_species_parameter_tys ;
  (* Next, abstract according to the species's parameters methods the *)
  (* current method depends on.                                       *)
    List.iter
      (fun (species_param_name, _, meths_from_param) ->
        (* Each abstracted method will be named like "_p_", followed by *)
        (* the species parameter name, followed by "_", followed by the *)
        (* method's name.                                               *)
        (* We don't care here about whether the species parameters is   *)
        (* "in" or "is".                                                *)
        let prefix =
          "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
        Parsetree_utils.DepNameSet.iter
          (fun (meth, _) ->
            the_list_reversed :=
              (prefix ^
               (Parsetree_utils.vname_as_string_with_operators_expanded meth))
              :: !the_list_reversed)
          meths_from_param)
    ai.Abstractions.ai_dependencies_from_params ;
  (* Next, the extra arguments due to methods of ourselves we depend on. *)
  (* They are always present in the species under the name "self_...".   *)
  List.iter
    (function
      | MinEnv.MCEE_Defined_carrier _
      | MinEnv.MCEE_Defined_computational (_, _, _)
      | MinEnv.MCEE_Defined_logical (_, _, _) ->
          (* Anything defined is not abstracted. *)
          ()
      | MinEnv.MCEE_Declared_logical (_, _) ->
          failwith "TODO 42"
      | MinEnv.MCEE_Declared_carrier ->
          (* The carrier is always abstracted by "abst_T". *)
          the_list_reversed := "abst_T" :: !the_list_reversed
      | MinEnv.MCEE_Declared_computational (n, _) ->
          let llift_name =
            "abst_" ^
            (Parsetree_utils.vname_as_string_with_operators_expanded n) in
          the_list_reversed := llift_name :: !the_list_reversed)
    ai.Abstractions.ai_min_coq_env ;
  (* Finally, reverse the list to keep the right oder. *)
  List.rev !the_list_reversed
;;




(** 
    Since this function is called after the function
    [bind_parameters_to_types_from_type_scheme] who was provided a type
    scheme, the optionnal typec in the list are always or the form [Some] !

    {b Rem} : Not exported outside this module. *)
let print_types_as_tuple_if_several print_ctx out_fmter types =
  let rec rec_print = function
    | [] -> assert false
    | [(_, one)] -> 
        let ty = match one with None -> assert false | Some t -> t in
        Format.fprintf out_fmter "%a"
          (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty
    | (_, h) :: q ->
        let ty = match h with None -> assert false | Some t -> t in
        Format.fprintf out_fmter "@[<1>(prod %a@ "
          (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: true) ty ;
        rec_print q ;
        Format.fprintf out_fmter ")@]" in
  rec_print types
;;

 
 
let generate_termination_proof _ctx _print_ctx _env _name = function
  | None -> ()
(*     "Variable self_term_order_%a"
     "Variable self_term_obl_%a" *)
  | Some _termination_proof -> ()
     (* Abstraction par lambda *)
(*     "Definition %a__term_order_%a"
     "Let self_term_order_%a" *)

     (* Abstraction par Variable *)
(*     "Section"
     "Theorem %a__term_obl_%a %a"
     "End"

     "Let self_term_obl_%a " *)
;;



let generate_recursive_let_definition ctx print_ctx env l =
  let out_fmter = ctx.Context.scc_out_fmter in
  match l with
   | [] ->
       (* A "let", then a fortiori "let rec" construct *)
       (* must at least bind one identifier !          *)
       assert false
   | [((from, name, params, scheme, body, _, _), ai)] ->
       (begin
       match body with
        | Parsetree.BB_logical _ ->
            (* [Unsure] *)
            failwith "recursive logical : TODO"
        | Parsetree.BB_computational body_expr ->
            let species_name = snd (ctx.Context.scc_current_species) in
            (* Extend the context with the mapping between these *)
            (* recursive functions and their extra arguments.    *)
            let ctx' = {
              ctx with
                Context.scc_lambda_lift_params_mapping =
                  [(name, make_params_list_from_abstraction_info ai)] } in
            (* Open the "Section" for the recursive definition. *)
            Format.fprintf out_fmter
              "@[<2>Section %a.@\n"
              Parsetree_utils.pp_vname_with_operators_expanded name ;
            (* Now, generate the prelude of the only method *)
            (* introduced by "let rec".                     *)
            let (abstracted_methods, new_ctx, new_print_ctx) =
              generate_defined_let_prelude
                ~rec_let: true ctx' print_ctx env ai.Abstractions.ai_min_coq_env
                ai.Abstractions.ai_used_species_parameter_tys
                ai.Abstractions.ai_dependencies_from_params in
            (* We now generate the order. It always has 2 arguments having    *)
            (* the same type. This type is a tuple if the method hase several *)
            (* arguments.                                                     *)
            Format.fprintf out_fmter
              "@\n@\n(* Abstracted termination order. *)@\n" ;
            Format.fprintf out_fmter "@[<2>Variable __term_order@ :@ " ;
            let (params_with_type, return_ty_opt, _) =
              MiscHelpers.bind_parameters_to_types_from_type_scheme
                (Some scheme) params in
            let return_ty =
              match return_ty_opt with None -> assert false | Some t -> t in
            (*  *)
            Types.purge_type_simple_to_coq_variable_mapping () ;
            (* Print the tuple that is the method's arguments' types. *)
            Format.fprintf out_fmter "%a -> %a -> Prop.@]@\n"
              (print_types_as_tuple_if_several new_print_ctx) params_with_type
              (print_types_as_tuple_if_several new_print_ctx) params_with_type ;

            (* We now prove that this order is well-founded. *)
            Types.purge_type_simple_to_coq_variable_mapping () ;
            
            let recursive_calls =
              Recursion.list_recursive_calls name params [] body_expr in
            Format.fprintf out_fmter
              "@[<2>Variable __term_obl : (well_founded __term_order) /\\@ " ;



            (* It's now time to generate the lemmas proving  *)
            (* that each recursive call decreases.           *)
            Rec_let_gen.generate_termination_lemmas
              new_ctx new_print_ctx env recursive_calls ;
            Format.fprintf out_fmter ".@]@\n@\n" ;
            (* Generate the recursive uncurryed function *)
            Format.fprintf out_fmter
              "@[<2>Function %a@ (__arg:@ %a)@ \
              {wf __term_order __arg}:@ %a@ :=@ @[<2>let (%a) :=@ __arg in@]@ "
              Parsetree_utils.pp_vname_with_operators_expanded name
              (print_types_as_tuple_if_several new_print_ctx) params_with_type
              (Types.pp_type_simple_to_coq new_print_ctx ~reuse_mapping: true)
              return_ty
              (Handy.pp_generic_separated_list ","
                Parsetree_utils.pp_vname_with_operators_expanded) params ;
            Species_record_type_generation.generate_expr new_ctx
              ~local_idents: []
              ~self_methods_status:
                 Species_record_type_generation.SMS_from_self
              ~in_hyp: true env body_expr ;
            Format.fprintf out_fmter ".@]@\n" ;
            Format.fprintf out_fmter "@[<v 2>Proof.@ %a Qed.@]@\n"
              (Handy.pp_generic_n_times ((List.length recursive_calls) + 1)
                Format.fprintf)
              "coq_builtins.prove_term_obl __term_obl.@ " ;

            (* Generate the curryed version *)
            Format.fprintf out_fmter "@[Definition %a__%a %a :=@ %a (%a).@]"
              Parsetree_utils.pp_vname_with_operators_expanded species_name
              Parsetree_utils.pp_vname_with_operators_expanded name
              (Handy.pp_generic_separated_list " "
                Parsetree_utils.pp_vname_with_operators_expanded) params
              Parsetree_utils.pp_vname_with_operators_expanded name
              (Handy.pp_generic_separated_list ","
                Parsetree_utils.pp_vname_with_operators_expanded) params ;
            (* Finally close the opened "Section". *)
            Format.fprintf out_fmter "End %a.@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded name ;
            let compiled = {
              cfm_from_species = from ;
              cfm_method_name = name ;
              cfm_used_species_parameter_tys =
                ai.Abstractions.ai_used_species_parameter_tys ;
              cfm_dependencies_from_parameters =
                ai.Abstractions.ai_dependencies_from_params ;
              cfm_coq_min_typ_env_names = abstracted_methods } in
            CSF_let_rec [compiled]
       end)
   | _ :: _ -> raise Recursion.MutualRecursion
;;



(** generated_fields : The list of previous fields of the species that have
    already be generated. Used while generating theorems to know what to apply
        to the methods generators the theorem depends on. *)
let generate_methods ctx print_ctx env generated_fields field =
  let out_fmter = ctx.Context.scc_out_fmter in
  match field with
   | Abstractions.FAI_sig (from, name, sch) ->
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
         let print_ctx' = {
           print_ctx with
           Types.cpc_collections_carrier_mapping =
              (Species_record_type_generation.make_Self_cc_binding_self_T
                 ~current_species: ctx.Context.scc_current_species)
              :: print_ctx.Types.cpc_collections_carrier_mapping } in
         Format.fprintf out_fmter
           "@[<2>Variable self_%a :@ %a.@]@\n"
           Parsetree_utils.pp_vname_with_operators_expanded name
           (Types.pp_type_simple_to_coq print_ctx' ~reuse_mapping: false) ty
         end) ;
       (* Nothing to keep for the collection generator. *)
       CSF_sig name
   | Abstractions.FAI_let ((from, name, params, scheme, body, _, _),
                           abstraction_info) ->
       (* No recursivity, then the method cannot call itself in its body *)
       (* then no need to set the [scc_lambda_lift_params_mapping] of    *)
       (* the context.                                                   *)
       let coq_min_typ_env_names =
         generate_non_recursive_field_binding
           ctx print_ctx env abstraction_info.Abstractions.ai_min_coq_env
           abstraction_info.Abstractions.ai_used_species_parameter_tys
           abstraction_info.Abstractions.ai_dependencies_from_params
           (from, name, params, scheme, body) in
       (* Now, build the [compiled_field_memory], even if the method  *)
       (* was not really generated because it was inherited.          *)
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_used_species_parameter_tys =
           abstraction_info.Abstractions.ai_used_species_parameter_tys ;
         cfm_dependencies_from_parameters =
           abstraction_info.Abstractions.ai_dependencies_from_params ;
         cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
       CSF_let compiled_field
   | Abstractions.FAI_let_rec l ->
       generate_recursive_let_definition ctx print_ctx env l
   | Abstractions.FAI_theorem ((from, name, _, logical_expr, _, deps_on_rep),
                               abstraction_info) ->
       let coq_min_typ_env_names =
         generate_theorem
           ctx print_ctx env abstraction_info.Abstractions.ai_min_coq_env
           abstraction_info.Abstractions.ai_used_species_parameter_tys
           abstraction_info.Abstractions.ai_dependencies_from_params
           generated_fields (from, name, logical_expr, deps_on_rep) in
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_used_species_parameter_tys =
           abstraction_info.Abstractions.ai_used_species_parameter_tys ;
         cfm_dependencies_from_parameters =
           abstraction_info.Abstractions.ai_dependencies_from_params ;
         cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
       CSF_theorem compiled_field
   | Abstractions.FAI_property ((from, name, _, logical_expr, _),
                                abstraction_info) ->
       (* "Property"s lead to a Coq "Hypothesis". Inherited properties *)
       (* are always generated again by just enouncing their body.     *)
       Format.fprintf out_fmter "(* From species %a. *)@\n"
         Sourcify.pp_qualified_species from ;
       Format.fprintf out_fmter
         "@[<2>Hypothesis self_%a :@ "
         Parsetree_utils.pp_vname_with_operators_expanded name ;
         (* Modify the [collection_carrier_mapping] so that *)
         (* "Self" is printed in types by "self_T". *)
         let ctx' = {
           ctx with
           Context.scc_collections_carrier_mapping =
              (Species_record_type_generation.make_Self_cc_binding_self_T
                 ~current_species: ctx.Context.scc_current_species)
              :: ctx.Context.scc_collections_carrier_mapping } in
        (* Be careful, in Hypothesis, methods from species parameters we *)
       (* depend on are NOT "_p_..." (the naming scheme used when we    *)
       (* use extra parameters to lambda-lift). Instead, one must refer *)
       (* To the Variables created in the Chapter and that are named by *)
       (* species parameter name + method name.                         *)
       Species_record_type_generation.generate_logical_expr
         ~local_idents: []
         ~self_methods_status: Species_record_type_generation.SMS_from_self
         ~in_hyp: true ctx' env
         logical_expr ;
       Format.fprintf out_fmter ".@]@\n" ;
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_used_species_parameter_tys =
           abstraction_info.Abstractions.ai_used_species_parameter_tys ;
         cfm_dependencies_from_parameters =
           abstraction_info.Abstractions.ai_dependencies_from_params ;
         cfm_coq_min_typ_env_names = [] (* Leave blank because never used. *)
         } in
       CSF_property compiled_field
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
          (type_coll, (carrier_name ^ "_T", Types.CCMI_is))
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name that will represent this "in" *)
          (* parameter seen from Coq.                     *)
          let param_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't    *)
          (* need any species expression to annotate this parameter in the   *)
          (* Coq type expression annotating this parameter in the hosting    *)
          (* species record type: it will simply be of the type [type_coll]. *)
          (type_coll, (param_name, Types.CCMI_in_or_not_param)))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ********************************************************************** *)
(* Env.CoqGenEnv.t -> Env.TypeInformation.species_description ->          *)
(*   Env.CoqGenEnv.t                                                      *)
(** {b Descr} : This function extend the coq code generation envionnment
    for a species generation. Because in Coq we need information about
    the number of extra parameters to add to function idents due to the
    fact that in Coq polymorphism is explicit, we need to make methods of
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
           (* a [Parsetree.Vuident] to wrap the species name string. Not  *)
           (* since we don't need any collection generator information,   *)
           (* we simply build the species binding in the environment by   *)
           (* just putting None inside.                                   *)
           Env.CoqGenEnv.add_species
             ~loc: Location.none (Parsetree.Vuident param_name)
             (bound_methods, None) accu_env)
    env_with_methods_as_values
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ********************************************************************** *)
(* Format.formatter -> Types.coq_print_context ->                         *)
(*   Env.TypeInformation.species_field list -> unit                       *)
(** {b Descr} : Search for an explicit representation of "Self" among the
    [fields]. If one is found, then generate the Coq code that "Let"-bind
    "self_T" to the carrier's type representation in Coq.
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
             (* We print the "rep"'s type but since the carrier can't be *)
             (* recursive, it can't appear in it's own structure, and    *)
             (* we don't need to add a binding for it in the             *)
             (* [collection_carrier_mapping].                            *)
             Format.fprintf out_fmter "@[<2>Let self_T : Set :=@ %a.@]@\n@\n"
               (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false)
               type_from_scheme
         | _ -> rec_find q
        end) in
  rec_find species_fields
;;



(* Only for "is" parameters !!! *)
let generate_variables_for_species_parameters_methods ctx print_ctx
    field_abstraction_infos =
  (* To keep tail-rec, we will accumulate by side effect. *)
  let accu_found_dependencies =
    ref ([] : (Parsetree.vname * Parsetree_utils.species_param_kind *
               Parsetree_utils.DepNameSet.t) list) in
  (* We first harvest the list of all methods from species parameters our   *)
  (* fields depend on. Hence, we get a list a list of (species parameter    *)
  (* names * the set of its methods we depend on). This list would need to  *)
  (* be cleaned-up to prevent doubles. Instead, of cleaning it, we just     *)
  (* avoid generating several times the same "Variable" by recording those  *)
  (* already seen.                                                          *)
  List.iter
    (function
      | Abstractions.FAI_sig _ -> ()
      | Abstractions.FAI_let (_, fai)
      | Abstractions.FAI_theorem (_, fai)
      | Abstractions.FAI_property (_, fai) ->
          accu_found_dependencies :=
            fai.Abstractions.ai_dependencies_from_params @
            !accu_found_dependencies
      | Abstractions.FAI_let_rec l ->
          List.iter
            (fun (_, fai) ->
              accu_found_dependencies :=
                fai.Abstractions.ai_dependencies_from_params @
                !accu_found_dependencies)
            l)
    field_abstraction_infos ;
  (* Now print the Coq "Variable"s, avoiding to print several times the same. *)
  (* The naming scheme of the methods is species param name + method name.    *)
  if !accu_found_dependencies <> [] then
    (begin
    let out_fmter = ctx.Context.scc_out_fmter in
    Format.fprintf out_fmter
      "@[<3>(* Variable(s)@ induced@ by@ dependencies@ on@ methods@ from@ \
      species@ parameter(s). *)@]@\n" ;
    let seen = ref [] in
    List.iter
      (fun (spe_param_name, spe_param_kind, deps_set) ->
        (* We only generate variables for *real* methods, i.e. for        *)
        (* dependencies coming through a "is" parameter, not a "in" one ! *)
        if spe_param_kind = Parsetree_utils.SPK_is then
          Parsetree_utils.DepNameSet.iter
            (fun (meth_name, meth_type) ->
              let remind_me = (spe_param_name, meth_name) in
              if not (List.mem remind_me !seen) then
                (begin
                seen := remind_me :: !seen ;
                (* Just note: because in the type of the species parameter's *)
                (* method there is no reason to see "Self" appearing, we     *)
                (* don't need to add any binding in the                      *)
                (* [collection_carrier_mapping].                             *)
                Format.fprintf out_fmter "@[<2>Variable %a_%a :@ %a.@]@\n"
                  Parsetree_utils.pp_vname_with_operators_expanded
                  spe_param_name
                  Parsetree_utils.pp_vname_with_operators_expanded meth_name
                  (Types.pp_type_simple_to_coq print_ctx ~reuse_mapping: false)
                  meth_type
                end))
            deps_set)
      !accu_found_dependencies ;
    (* Just an extra line feed to make the source more readable. *)
    Format.fprintf out_fmter "@\n"
    end)
;;



(** Apply the mk_record to the species parameters carriers representation
    then to "Self" representation ("self_T"). *)
let dump_collection_generator_first_arguments ctx out_fmter =
  (* The species parameters carrier types. *)
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      Format.fprintf out_fmter "@ " ;
      (match param_kind with
       | Types.CCMI_is ->
           if param_ty_mod <> ctx.Context.scc_current_unit then
             Format.fprintf out_fmter "%s." param_ty_mod ;
           Format.fprintf out_fmter "%s_T" param_ty_coll
       | Types.CCMI_in_or_not_param ->
           (* One must use the "Variable" created after the record type to *)
           (* abstract the "in" parameter. This variable is losely name by *)
           (* the dependency process as: name of the "is" parameter twice, *)
           (* separated by "_". In fact, there is no abstraction (i.e.     *)
           (* lambda-lifting) over "in" parameters.                        *)
           Format.fprintf out_fmter "%s_%s" param_name param_name))
    ctx.Context.scc_collections_carrier_mapping ;
  (* Then, always the "self_T" since the first record field represents *)
  (* what is to be the species carrier (foo_T :> Set.)                 *)
  Format.fprintf out_fmter "@ self_T"
;;



let generate_collection_generator ctx compiled_species_fields =
  let current_species_name = snd ctx.Context.scc_current_species in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "@\nSpecies %a is fully defined. Generating its collection generator@."
      Sourcify.pp_vname current_species_name ;
  (* A little comment in the generated Coq code. *)
  Format.fprintf out_fmter
    "@\n(* Fully defined '%a' species's collection generator. *)@\n"
    Sourcify.pp_vname current_species_name ;
  (* The generic name of the collection generator: the species' name + *)
  (* "_collection_create".                                              *)
  Format.fprintf out_fmter "@[<2>Definition %a_collection_create"
    Parsetree_utils.pp_vname_with_operators_expanded current_species_name ;
  Format.fprintf out_fmter " :=@ mk_%a"
    Parsetree_utils.pp_vname_with_operators_expanded current_species_name ;
  (* The collection generator first arguments are those corresponding *)
  (* to the species parameters, hence to the record type parameters.  *)
  (* All of them are in the [collection_carrier_mapping] of the       *)
  (* current compilation context.                                     *)
  dump_collection_generator_first_arguments ctx out_fmter ;
  (* No need to generate the local functions that will be used to fill *)
  (* the record value since in Coq we always generate them. It's       *)
  (* already done !                                                    *)
  (* And now, the record value. *)
  List.iter
    (function
      | CSF_let field_memory | CSF_theorem field_memory ->
          Format.fprintf ctx.Context.scc_out_fmter "@ self_%a"
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.cfm_method_name
      | CSF_let_rec l ->
          List.iter
            (fun field_memory ->
              Format.fprintf ctx.Context.scc_out_fmter "@ self_%a"
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.cfm_method_name)
            l
      | CSF_sig vname ->
          (* In a fully defined species, no sig should remain.  The only *)
          (* exception is "rep" that is **defined** when it appears as a *)
          (* "sig".                                                      *)
          if vname <> Parsetree.Vlident "rep" then assert false
      | CSF_property _ ->
          (* In a fully defined species, no property should remain. *)
          assert false)
    compiled_species_fields ;
  (* Close the pretty-print box of the "Let collection_create ... :=". *)
  Format.fprintf ctx.Context.scc_out_fmter ".@]@\n"
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
        | Env.TypeInformation.SPAR_in (n, _) -> (n, Parsetree_utils.SPK_in)
        | Env.TypeInformation.SPAR_is ((_, n), _, _) ->
            ((Parsetree. Vuident n), Parsetree_utils.SPK_is))
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, species_name) ;
    Context.scc_dependency_graph_nodes = dep_graph ;
    Context.scc_species_parameters_names = species_parameters_names ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* Insert in the environment the value bindings of the species methods *)
  (* and the species bindings for its parameters.                        *)
  let env' = extend_env_for_species_def env species_descr in
  (* The record type representing the species' type. *)
  Species_record_type_generation.generate_record_type ctx env' species_descr ;
  (* WE DON'T extend the collections_carrier_mapping in the context     *)
  (* with the mapping of "Self" to <coll_T> NOW otherwise, below we     *)
  (* will create an extra "Variable <coll_T> : self_T" which is wrong ! *)
  (* In effect, a bit like in the function                              *)
  (* [Species_record_type_generation.generate_record_type], we create   *)
  (* the variable, assuming that species parameters are stuff present   *)
  (* in the current [collections_carrier_mapping].                      *)
  (* Now we generate a "Variable" of type "Set" for each species's "is"  *)
  (* parameter with the same name used during the record type            *)
  (* generation, i.e. the parameter's name + "_T". This serves to        *)
  (* "create" the "type" of this parameter for Coq. In Ocaml, the        *)
  (* polymorphism (used to represent parameters) is implicit, but not    *)
  (* in Coq. That's why we need to make a "Variable" for these types.    *)
  (* If it is a "in", then we introduce a "Variable" whose type is the   *)
  (* type is the "in" parameter, recovered from the [param_ty_mod] and   *)
  (* the [param_ty_coll] of the parameter.                               *)
  (* To make our job, we use the current [collections_carrier_mapping]   *)
  (* of the context because is already contains the species parameters   *)
  (* with their binding.                                                 *)
  List.iter
    (fun ((param_ty_mod, param_ty_coll), (param_name, param_kind)) ->
      Format.fprintf out_fmter
         "(* Variable abstracting the species parameter [%s]. *)@\n"
         param_name ;
       match param_kind with
       | Types.CCMI_is ->
           (* Note that the trailing "_T" is already embedded in the *)
           (* [param_name] of the collection carrier mapping.        *)
           Format.fprintf out_fmter "@[<2>Variable %s :@ Set.@]@\n" param_name ;
       | Types.CCMI_in_or_not_param ->
           (* Attention, "in"-parameters are named by the name twice, *)
           (* separated by "_".                                       *)
           Format.fprintf out_fmter "@[<2>Variable %s_%s :@ "
             param_name param_name;
           (* If needed, qualify the type. *)
           if param_ty_mod <> ctx.Context.scc_current_unit then
             Format.fprintf out_fmter "%s." param_ty_mod ;
           Format.fprintf out_fmter "%s_T.@]@\n" param_ty_coll)
    ctx.Context.scc_collections_carrier_mapping ;
  (* NOW we can extend the collections_carrier_mapping with ourselves known. *)
  (* Hence, if we refer to our "rep" we will be directly mapped onto the     *)
  (* "self_T" without needing to re-construct this name each time.           *)
  let collections_carrier_mapping' =
    (Species_record_type_generation.make_Self_cc_binding_current_species_T
       ~current_species: ctx.Context.scc_current_species) ::
    ctx.Context.scc_collections_carrier_mapping in
  let ctx' = { ctx with
     Context.scc_collections_carrier_mapping = collections_carrier_mapping' } in
  (* Build the print context for the methods once for all. *)
  let print_ctx = {
    Types.cpc_current_unit = ctx'.Context.scc_current_unit ;
    Types.cpc_current_species =
      Some
        (Parsetree_utils.type_coll_from_qualified_species
           ctx'.Context.scc_current_species) ;
    Types.cpc_collections_carrier_mapping =
      ctx'.Context.scc_collections_carrier_mapping } in
  (* Now, the methods of the species. We deal with "rep" first *)
  (* and then it will be ignore while generating the methods.  *)
  generate_self_representation
    out_fmter print_ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  let field_abstraction_infos =
    Abstractions.compute_abstractions_for_fields
      ~with_def_deps: true
      ctx' species_descr.Env.TypeInformation.spe_sig_methods in
  (* Generate for each method of a species parameter we        *)
  (* decl-depend on and don't def-depend on, a Coq "Variable". *)
  generate_variables_for_species_parameters_methods
     ctx' print_ctx field_abstraction_infos ;
  (* Now, generate the Coq code of the methods.  Do not [fold_right] *)
  (* otherwise the fields will be generated in reverse order.        *)
  let compiled_fields =
    List.fold_left
      (fun accu field ->
        (* Pass the accu to be able to remind the already generated fields. *)
        let compiled_field = generate_methods ctx' print_ctx env' accu field in
        (* Not efficient, but required to keep the fields in the right order. *)
        accu @ [compiled_field])
      []
      field_abstraction_infos in
  (* Now build the list of the species parameters names to make *)
  (* them public in the future ml generation environnment.      *)
  let species_params_names_n_kinds =
    List.map
      (fun (pname, pkind) ->
        match pkind.Parsetree.ast_desc with
         | Parsetree.SPT_in _ -> (pname, MiscHelpers.SPK_in)
         | Parsetree.SPT_is _ -> (pname, MiscHelpers.SPK_is))
      species_def_desc.Parsetree.sd_params in
  (* Now check if the species supports a collection generator because fully *)
  (* defined and get the information about which arguments to pass in order *)
  (* to later call the collection generator.                                *)
  let extra_args_from_spe_params =
    if species_descr.Env.TypeInformation.spe_is_closed then
      (begin
      generate_collection_generator ctx compiled_fields ;
      Some
        { Env.CoqGenInformation.cgi_implemented_species_params_names =
            species_params_names_n_kinds ;
          Env.CoqGenInformation.cgi_generator_parameters = [] }
      end)
    else None in
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
           | CSF_property compiled_field_memory ->
               [ { Env.CoqGenInformation.mi_name =
                     compiled_field_memory.cfm_method_name ;
                   Env.CoqGenInformation.mi_dependencies_from_parameters =
                     compiled_field_memory.cfm_dependencies_from_parameters ;
                   (* For properties, this list should always be [] since *)
                   (* we do not compute the visible universe since it is  *)
                   (* never used.                                         *)
                   Env.CoqGenInformation.mi_abstracted_methods =
                     compiled_field_memory.cfm_coq_min_typ_env_names }])
         compiled_fields) in
  (species_binding_info, extra_args_from_spe_params)
;;


let print_implemented_species_for_coq ~current_unit out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another Coq "file-module". *)
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit,  *)
       (* then again no need to find the species's module in another Coq *)
       (* "file-module" otherwise we explicitely prefix by the module    *)
       (* name corresponding to the filename.                            *)
       if fname <> current_unit then
         Format.fprintf out_fmter "%s." fname ;
       Format.fprintf out_fmter "%s" (Parsetree_utils.name_of_vname vname)
;;



(* [Unsure] Au moins toute la première partie peut être factorisée avec
  Ocaml. C'est exactment le même code ! *)
let apply_generator_to_parameters ctx env collection_body_params
    col_gen_params_info =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the assoc list mapping the formal to the effective parameters. *)
  let formal_to_effective_map =
    (try
      List.map2
        (fun formal_info effective_info ->
          match (formal_info, effective_info) with
           | ((formal, MiscHelpers.SPK_is),
              MiscHelpers.CEA_collection_name_for_is qualified_vname) ->
               (begin
               (* "Is" parameter. Leads to collection name based stuff. *)
               match qualified_vname with
                | Parsetree.Vname _ ->
                    (* Assumed to be local to the current unit. *)
                    (formal,
                     MiscHelpers.CEA_collection_name_for_is qualified_vname)
                | Parsetree.Qualified (effective_fname, effective_vname) ->
                    (* If the species belongs to the current unit, then we   *)
                    (* don't need to qualify it in the OCaml generated code. *)
                    (* Then we simply discard its explicit hosting           *)
                    (* information.                                          *)
                    if effective_fname = current_unit then
                      (formal,
                       MiscHelpers.CEA_collection_name_for_is
                         (Parsetree.Vname effective_vname))
                    else
                      (formal,
                       MiscHelpers.CEA_collection_name_for_is
                         (Parsetree.Qualified
                            (effective_fname, effective_vname)))
               end)
           | ((formal, MiscHelpers.SPK_in),
              (MiscHelpers.CEA_value_expr_for_in effective_expr)) ->
               (begin
               (* "In" parameter. Leads to direct value based stuff. *)
               (formal, (MiscHelpers.CEA_value_expr_for_in effective_expr))
               end)
           | (_, _) ->
               (* This would mean that we try to apply an effective stuff    *)
               (* in/is-incompatible with the kind of the species parameter. *)
               (* This should have been caught before by the analyses !      *)
               assert false)
        col_gen_params_info.Env.CoqGenInformation.
          cgi_implemented_species_params_names
        collection_body_params
    with Invalid_argument "List.map2" ->
      assert false  (* The lists length must be equal. *)) in
  (* Now, generate the argment identifier or expression *)
  (* for each expected collection generator parameter.  *)
  List.iter
    (fun (formal_species_param_name, method_names) ->
      match List.assoc formal_species_param_name formal_to_effective_map with
       | MiscHelpers.CEA_collection_name_for_is corresponding_effective ->
           (begin
           let
             (corresponding_effective_opt_fname,
              corresponding_effective_vname) =
             match corresponding_effective with
              | Parsetree.Vname n -> (None, n)
              | Parsetree.Qualified (m, n) -> ((Some m), n) in
           Parsetree_utils.DepNameSet.iter
             (fun (meth_name, _) ->
               (* If needed, qualify the name of the species in the *)
               (* Coq code. Don't print the type to prevent being   *)
               (* too verbose.                                      *)
               (match corresponding_effective_opt_fname with
                | Some fname -> Format.fprintf out_fmter "%s." fname
                | None -> ()) ;
               (* Species name."effective_collection.". *)
               Format.fprintf out_fmter "@ %a.effective_collection."
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname ;
               (* If needed, qualify the name of the species *)
               (* in the Coq code.                           *)
               (match corresponding_effective_opt_fname with
                | Some fname -> Format.fprintf out_fmter "%s." fname
                | None -> ()) ;
               (* Species name.method name. *)
               Format.fprintf out_fmter "%a.%a"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname
                 Parsetree_utils.pp_vname_with_operators_expanded meth_name)
             method_names
           end)
       | MiscHelpers.CEA_value_expr_for_in expr ->
           (begin
           Format.fprintf out_fmter "(@[<1>" ;
           (* No local idents in the context because we just enter the scope  *)
           (* of a species fields and so we are not under a core expression.  *)
           (* For [~in_hyp], since everything must already be defined, we     *)
           (* must not have anymore abstraction, hence not anymore lambda-    *)
           (* lifting. So it should be non-relevant to chose a speciel value. *)
           (* For [~self_as], same thing, no relevant value since the         *)
           (* application of the generator should not involve any other       *)
           (* expressions than methods/theorems identifiers.                  *)
           Species_record_type_generation.generate_expr
             ctx ~local_idents: []
             ~self_methods_status:
              (* Or what you prefer. *)
               Species_record_type_generation.SMS_abstracted
             ~in_hyp: false               (* Or what you prefer. *)
             env expr ;
           Format.fprintf out_fmter ")@]" ;
           end))
    col_gen_params_info.Env.CoqGenInformation.cgi_generator_parameters
;;



let collection_compile env ~current_unit out_fmter collection_def
    collection_descr dep_graph =
  let collection_name = collection_def.Parsetree.ast_desc.Parsetree.cd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating Coq code for collection %a@."
      Sourcify.pp_vname collection_name ;
  (* Start the "Chapter" encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>Chapter %a.@\n"
    Sourcify.pp_vname collection_name ;
  (* Now, establish the mapping between collections available *)
  (* and the names representing their carrier.                *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit collection_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, collection_name) ;
    Context.scc_dependency_graph_nodes = dep_graph ;
    (* A collection never has parameter. *)
    Context.scc_species_parameters_names = [] ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* The record type representing the collection's type. *)
(* [Unsure] Peut-être même pas utile le record ! *)
  Species_record_type_generation.generate_record_type ctx env collection_descr ;
  (* We do not want any collection generator. Instead, we will call the  *)
  (* collection generator of the collection we implement and apply it to *)
  (* the functions it needs coming from the collection applied to its    *)
  (* parameters if there are some.                                       *)
  (* Now generate the value representing the effective instance of the   *)
  (* collection. We always name it by collection's name +                *)
  (* "_effective_collection".                                            *)
  Format.fprintf out_fmter "@[<2>Definition %a_effective_collection :=@\n"
    Sourcify.pp_vname collection_name ;
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    collection_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  print_implemented_species_for_coq
    ~current_unit out_fmter implemented_species_name ;
  (* Append the suffix representing the collection generator's name. *)
  Format.fprintf out_fmter "_collection_create" ;
  (* Finally, we must recover the arguments to apply to this collection    *)
  (* generator. These arguments of course come from the species parameters *)
  (* the closed species we implement has (if it has some). We must         *)
  (* make this application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE   *)
  (* RIGHT ORDER !                                                         *)
  (begin
  try
    let (_, opt_params_info) =
      Env.CoqGenEnv.find_species
        ~loc: collection_def.Parsetree.ast_loc ~current_unit
        implemented_species_name env in
    (match opt_params_info with
     | None ->
         (* The species has no collection generator. Hence it is not a   *)
         (* fully defined species. This should have be prevented before, *)
         (* by forbidding to make a collection from a non fully defined  *)
         (* species !                                                    *)
         assert false   (* [Unsure] car je crois qu'on n'a pas fait la vérif. *)
     | Some params_info ->
         (* Get the names of the collections or the value *)
         (* expressions effectively applied.              *)
         let collection_body_params =
           MiscHelpers.get_implements_effectives
             collection_def.Parsetree.ast_desc.
               Parsetree.cd_body.Parsetree.ast_desc.
             Parsetree.se_params
             params_info.Env.CoqGenInformation.
               cgi_implemented_species_params_names in
         apply_generator_to_parameters
           ctx env collection_body_params params_info)
  with Not_found ->
    (* Don't see why the species could not be present in the environment.  *)
    (* The only case would be to make a collection from a collection since *)
    (* collection are never entered in the environment because it's a non  *)
    (* sense to make a collection "implementing" a collection !            *)
    (* [Unsure]. Peut être lever un message d'erreur. *)
    assert false
  end) ;
  Format.fprintf out_fmter "@].@\n@\n"
;;
