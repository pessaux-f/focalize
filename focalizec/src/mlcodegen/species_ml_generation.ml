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

(* $Id: species_ml_generation.ml,v 1.72 2008-07-10 15:00:59 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
              Ocaml of FoCaL's collections and species.            *)
(* *************************************************************** *)


(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
    the species definition parameters and the type variables names to be
    used later during the OCaml translation. For a species parameter
    [A is/in ... ], the type variable that will be used is "'" + lowercased
    name of the species parameter + an int unique in this type +
    "_as_carrier".
    We need to add an extra int (a stamp) to prevent a same type variable
    from appearing several time in the tricky case where a IN and a IS
    parameters wear the same lowercased name. For instance:
    "species A (F is B, f in F)" where "F" and "f" will lead to a same name
    of ML type variable: "'f_as_carrier"

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  let cnt = ref 0 in
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, n_as_string), _, _, _) ->
          (* Build the name of the type variable that will represent *)
          (* this parameter's carrier type seen from OCaml. Just     *)
          (* lowerize the parameter name because collection names    *)
          (* are always syntactically capitalized and OCaml type     *)
          (* variable are always syntactically lowercase.            *)
          let carrier_type_variable_name =
            "'" ^ (String.uncapitalize n_as_string) ^ (string_of_int !cnt) ^
            "_as_carrier" in
          incr cnt ;
          (* Now, build the "collection type" this name will be bound to. *)
          (* According to how the "collection type" of parameters are     *)
          (* built, this will be the couple of the current compilation    *)
          (* unit and the name of the parameter.                          *)
          let type_coll = (current_unit, n_as_string) in
          (* And now create the binding... *)
          (type_coll, (carrier_type_variable_name, Types.CCMI_is))
      | Env.TypeInformation.SPAR_in (n, type_coll, provenance) ->
          (* Build the name of the type variable that will represent *)
          (* this parameter's carrier type seen from OCaml. Same     *)
          (* remark than above for lowercase/uppercase.              *)
          let carrier_type_variable_name =
            "'" ^ (String.uncapitalize (Parsetree_utils.name_of_vname n)) ^
            (string_of_int !cnt) ^ "_as_carrier" in
          incr cnt ;
          (type_coll,
           (carrier_type_variable_name, (Types.CCMI_in provenance))))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ******************************************************************** *)
(* ('a * (string, 'b)) list -> unit                                     *)
(** {b Descr} : Helper to print the list of known variables names in
    a collections carrier mapping as a legal OCaml list of type
    parameters, i.e, comma separated except for the last one.

    {b Args} :
      - [out_fmter] : The out channel where to generate the OCaml
          source code.
      - [vars_list] : The list of couples whose second component is the
          variable name. (We are not interested in the first component
          because we only want to print tha variables names).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let print_comma_separated_vars_list_from_mapping out_fmter vars_list =
  let rec rec_print = function
    | [] -> ()
    | [(_, (last_name, _))] -> Format.fprintf out_fmter "%s" last_name
    | (_, (h, _)) :: q ->
        Format.fprintf out_fmter "%s,@ " h ;
        rec_print q in
  rec_print vars_list
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_field list -> unit  *)
(** {b Descr} : Checks if "rep" is defined. If so, then generate the type
    constraint reflecting its effective structure. We loosely iterate on the
    list of fields, stopping at the first occurence of "rep" because, by
    construction if "rep" is present then it is only once. Hence it is safe
    to ignore the remaining fields.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_rep_constraint_in_record_type ctx fields =
  let rec rec_search = function
    | [] -> ()
    | h :: q ->
        (begin
        match h with
         | Env.TypeInformation.SF_sig (_, n, sch) ->
             (* Check if the sig is "rep". *)
             if (Parsetree_utils.name_of_vname n) = "rep" then
               (begin
               Format.fprintf ctx.Context.scc_out_fmter
                 "(* Carrier's structure explicitly given by \"rep\". *)@\n" ;
               Format.fprintf ctx.Context.scc_out_fmter "@[<2>type " ;
               (* First, output the type parameters if some, and enclose *)
               (* them by parentheses if there are several.              *)
               (match ctx.Context.scc_collections_carrier_mapping with
                | [] -> ()
                | [ (_, (only_var_name, _)) ] ->
                    Format.fprintf ctx.Context.scc_out_fmter "%s@ "
                      only_var_name
                | _ ->
                    (* More than one, then surround by parentheses. *)
                    Format.fprintf ctx.Context.scc_out_fmter "@[<1>(" ;
                    (* Print the variables names... *)
                    print_comma_separated_vars_list_from_mapping
                      ctx.Context.scc_out_fmter
                      ctx.Context.scc_collections_carrier_mapping ;
                    Format.fprintf ctx.Context.scc_out_fmter ")@]@ ") ;
               (* Now, output the type's name and body. *)
               let ty = Types.specialize sch in
               Format.fprintf ctx.Context.scc_out_fmter
                 "me_as_carrier =@ %a@]@\n"
                 (Types.pp_type_simple_to_ml
                    ~current_unit: ctx.Context.scc_current_unit
                    ~reuse_mapping: false
                    ctx.Context.scc_collections_carrier_mapping) ty
               end)
             else rec_search q
         | _ -> rec_search q
        end) in
  rec_search fields
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_description -> unit *)
(** {b Descr} : Generate the record type representing a species. This type
    contains a field per method. This type is named "me_as_species" to
    reflect the point that it represents the ML structure representing the
    FoCaL species.
    Depending on whether the species has parameters, this record type also
    has parameters. In any case, it at least has a parameter representing
    "self as it will be once instanciated" once "we" (i.e. the species) will
    be really living as a collection.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx species_descr =
  let out_fmter = ctx.Context.scc_out_fmter in
  let collections_carrier_mapping =
    ctx.Context.scc_collections_carrier_mapping in
  (* First, check if "rep" is defined. If so, then generate  *)
  (* the type constraint reflecting its effective structure. *)
  generate_rep_constraint_in_record_type
    ctx species_descr.Env.TypeInformation.spe_sig_methods ;
  (* The header of the OCaml type definition for the species record. *)
  Format.fprintf out_fmter "@[<2>type " ;
  (* Process parameters and "self" type variables names. *)
  if collections_carrier_mapping = [] then
    Format.fprintf out_fmter "'me_as_carrier "
  else
    (begin
    (* If there are several parameters, then enclose them by parentheses. *)
    Format.fprintf out_fmter "(@[<1>" ;
    List.iter
      (fun (_, (type_variable_name, _)) ->
        Format.fprintf out_fmter "%s,@ " type_variable_name)
      collections_carrier_mapping ;
    Format.fprintf out_fmter "'me_as_carrier)@] "
    end) ;
  (* The name of the type. *)
  Format.fprintf out_fmter "me_as_species = {@\n" ;
  (* We now extend the collections_carrier_mapping with ourselve known.     *)
  (* This is required when "rep" is defined.                                *)
  (* Hence, if we refer to our "rep" (i.e. "me_as_carrier"), not to Self, I *)
  (* mean to a type-collection that is "(our compilation unit, our species  *)
  (* name)" (that is the case when creating a collection where Self gets    *)
  (* especially abstracted to "(our compilation unit, our species name)",   *)
  (* we will be known and we wont get the fully qualified type name,        *)
  (* otherwise this would lead to a dependency with ourselve in term of     *)
  (* OCaml module.                                                          *)
  (* Indeed, we now may refer to our carrier explicitely here in the scope  *)
  (* of a collection (not species, really collection)  because there is no  *)
  (* more late binding: here when one say "me", it's not anymore            *)
  (* "what I will be finally" because we are already "finally". Before, as  *)
  (* long a species is not a collection, it always refers to itself's type  *)
  (* as "'me_as_carrier" because late binding prevents known until the last *)
  (* moment who "we will be". But because now it's the end of the species   *)
  (* specification, we know really "who we are" and "'me_as_carrier" is     *)
  (* definitely replaced by "who we really are" : "me_as_carrier".          *)
  let (my_fname, my_species_name) = ctx.Context.scc_current_species in
  let collections_carrier_mapping =
    ((my_fname, (Parsetree_utils.name_of_vname my_species_name)),
     (* [CCMI_is] or whatever, it's not used for OCaml code generation. *)
     ("me_as_carrier", Types.CCMI_is)) ::
    collections_carrier_mapping in
  (* The record's fields types. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, sch)
      | Env.TypeInformation.SF_let (from, n, _, sch, _, _, _) ->
          (begin
          (* Skip "rep", because it is a bit different and processed above *)
          (* c.f. function [generate_rep_constraint_in_record_type].       *)
          if (Parsetree_utils.name_of_vname n) <> "rep" then
            (begin
            let ty = Types.specialize sch in
            (* If the type of the sig refers to type "Prop", then the sig  *)
            (* is related to a logical let and hence must not be generated *)
            (* in OCaml.                                                   *)
            if not (Types.refers_to_prop_p ty) then
              (begin
              Format.fprintf out_fmter "(* From species %a. *)@\n"
                Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
              (* Since we are printing a whole type scheme, it is stand-alone *)
              (* and we don't need to keep name sharing with anythin else.    *)
              Format.fprintf out_fmter "@[<2>%a : %a ;@]@\n"
                Parsetree_utils.pp_vname_with_operators_expanded n
                (Types.pp_type_simple_to_ml
                   ~current_unit: ctx.Context.scc_current_unit
                   ~reuse_mapping: false collections_carrier_mapping) ty
              end)
            end)
          end)
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (from, n, _, sch, _, _, _) ->
              let ty = Types.specialize sch in
              (* If the type of the sig refers to type "Prop", then the sig  *)
              (* is related to a logical let and hence must not be generated *)
              (* in OCaml.                                                   *)
              if not (Types.refers_to_prop_p ty) then
                (begin
                Format.fprintf out_fmter "(* From species %a. *)@\n"
                  Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
                (* Since we are printing a whole type scheme, it is   *)
                (* stand-alone and we don't need to keep name sharing *)
                (* with anythin else.                                 *)
                Format.fprintf out_fmter "%a : %a ;@\n"
                  Parsetree_utils.pp_vname_with_operators_expanded n
                  (Types.pp_type_simple_to_ml
                     ~current_unit: ctx.Context.scc_current_unit
                     ~reuse_mapping: false collections_carrier_mapping) ty
                end))
            l
      | Env.TypeInformation.SF_theorem  (_, _, _, _, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _, _) ->
          (* Properties and theorems are purely  *)
          (* discarded in the Ocaml translation. *)
          ())
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}@\n"
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


(* ************************************************************************ *)
(** {b Descr} : Recover the parameters abstracted from our method to apply
    to a method generator. This function must only be used when the
    method whom we search the generator has been identified as inherited.
    Recover also the dependencies on the species parameters the method we
    inherit was depending on in its hosting species. They will have to be
    processed specially since the parameters names it refers to do not
    longer exist in the new species. However, this means that they must
    lead to the application of the effective method of the effective
    parameter passed to instanciate the species parameter.
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
  (* This ident is temporary and created just to lookup in the environment. *)
  let from_as_ident =
    Parsetree_utils.make_pseudo_species_ident ~current_unit from_species in
  try
    let (_, species_meths_infos, _, _) =
      Env.MlGenEnv.find_species
        ~loc: Location.none ~current_unit from_as_ident env in
    (* Now, find the method in the species information. *)
    let method_info =
      List.find
        (fun { Env.mi_name = n } -> n = method_name)
        species_meths_infos in
    (method_info.Env.mi_abstracted_methods,
     method_info.Env.mi_dependencies_from_parameters)
  with _ ->
    (* Because the generator is inherited, the species where it is hosted *)
    (* MUST be in the environment. Otherwise, something went wrong...     *)
    assert false
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
      - [ai] : The [abstraction_info] of a method, recording this
        method's dependencies.

    {b Return} :
      - The list of names corresponding to the arguments lambda-lifting
        the dependencies (those from methods of the species parameters
        first, in the order of species parameters, then those from the
        methods of ourselves).

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
let make_params_list_from_abstraction_info ai =
  (* Build the list by side effect in reverse order for efficiency. *)
  let the_list_reversed = ref [] in
  let all_deps_from_params =
    Abstractions.merge_abstraction_infos
      ai.Abstractions.ai_dependencies_from_params_via_body
      (Abstractions.merge_abstraction_infos
         ai.Abstractions.ai_dependencies_from_params_via_type
         ai.Abstractions.ai_dependencies_from_params_via_completion) in
  (* First, abstract according to the species's parameters the current  *)
  (* method depends on.                                                 *)
  List.iter
    (fun (species_param, meths_from_param) ->
      (* Recover the species parameter's name. *)
      let species_param_name =
        match species_param with
         | Env.TypeInformation.SPAR_in (n, _, _) -> n
         | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
             Parsetree.Vuident n in
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
    all_deps_from_params ;
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
          (* In Ocaml, logical properties are forgotten. *)
          ()
      | MinEnv.MCEE_Declared_carrier ->
          (* In Ocaml generation model, the carrier is never          *)
          (* lambda-lifted then doesn't appear as an extra parameter. *)
          ()
      | MinEnv.MCEE_Declared_computational (n, _) ->
          let llift_name =
            "abst_" ^
            (Parsetree_utils.vname_as_string_with_operators_expanded n) in
          the_list_reversed := llift_name :: !the_list_reversed)
    ai.Abstractions.ai_min_coq_env ;
  (* Finally, reverse the list to keep the right oder. *)
  List.rev !the_list_reversed
;;



(* ********************************************************************* *)
(** {b Descr} : Really dumps the OCaml code for ONE species Let or
    Let_rec binding.

    {b Args} :
      - [ctx] : The current generation context

      - [env] : The current generation environment.

      - [min_coq_env] : The current minimal environment of the
        method.

      - [~let_connect] flag telling whether we must start the function
        binding with "let" or "and".

      - [dependencies_from_params] : The information giving for each
        species parameter, the set of methods the current method depends
        on.

      - [(from, name, params, scheme, body)] : The constitutive elements
        of the method.

    {b Return} :
      - The list of methods names from ourselves the current method
        depends on and the list of methods from species parameters the
        current method depends on. This last list can be different from
        the initially received argument [dependencies_from_params] since
        some dependencies may come from fields that are not relevant for
        OCaml (properties, theorems).

    {b Rem} : Not exported outside this module.                          *)
(* ********************************************************************* *)
let generate_one_field_binding ctx env min_coq_env ~let_connect
    dependencies_from_params (from, name, params, scheme, body) =
  let out_fmter = ctx.Context.scc_out_fmter in
  let collections_carrier_mapping =
     ctx.Context.scc_collections_carrier_mapping in
  (* First of all, only methods defined in the current species must *)
  (* be generated. Inherited methods ARE NOT generated again !      *)
  if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
    (begin
    (* Just a bit of debug. *)
    if Configuration.get_verbose () then
      Format.eprintf "Generating OCaml code for field '%a'.@."
        Parsetree_utils.pp_vname_with_operators_expanded name ;
    (* Start the OCaml function definition. *)
    (match let_connect with
     | LC_first_non_rec ->
         Format.fprintf out_fmter "@[<2>let %a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | LC_first_rec ->
         Format.fprintf out_fmter "@[<2>let rec %a"
           Parsetree_utils.pp_vname_with_operators_expanded name
     | LC_following ->
         Format.fprintf out_fmter "@[<2>and %a"
           Parsetree_utils.pp_vname_with_operators_expanded name) ;
    (* First, abstract according to the species's parameters the current  *)
    (* method depends on.                                                 *)
    List.iter
      (fun (species_param, meths_from_param) ->
        (* Recover the species parameter's name. *)
        let species_param_name =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
               Parsetree.Vuident n in
        (* Each abstracted method will be named like "_p_", followed by *)
        (* the species parameter name, followed by "_", followed by the *)
        (* method's name.                                               *)
        (* We don't care here about whether the species parameters is   *)
        (* "in" or "is".                                                *)
        let prefix =
          "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^ "_" in
        Parsetree_utils.DepNameSet.iter
          (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
            Format.fprintf out_fmter "@ %s%a"
              prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
          meths_from_param)
      dependencies_from_params ;
    (* Next, the extra arguments due to methods of ourselves we depend on. *)
    (* They are always present in the species under the name "abst_...".   *)
    let abstracted_methods =
      List.flatten
        (List.map
           (function
             | MinEnv.MCEE_Defined_carrier _
             | MinEnv.MCEE_Defined_computational (_, _, _)
             | MinEnv.MCEE_Defined_logical (_, _, _) ->
                 (* Anything defined is not abstracted. *)
                 []
             | MinEnv.MCEE_Declared_logical (_, _) ->
                 (* In Ocaml, logical properties are forgotten. *)
                 []
             | MinEnv.MCEE_Declared_carrier ->
                 (* In Ocaml generation model, the carrier is never          *)
                 (* lambda-lifted then doesn't appear as an extra parameter. *)
                 []
             | MinEnv.MCEE_Declared_computational (n, _) ->
                 (* Don't print types. *)
                 Format.fprintf out_fmter "@ abst_%a"
                   Parsetree_utils.pp_vname_with_operators_expanded n ;
                 [n])
           min_coq_env) in
    (* Add the parameters of the let-binding with their type.   *)
    (* Ignore the result type of the "let" if it's a function   *)
    (* because we never print the type constraint on the result *)
    (* of the "let". We only print them in the arguments of the *)
    (* let-bound ident.                                         *)
    (* We also ignore the variables used to instanciate the     *)
    (* polymorphic ones of the scheme because in OCaml          *)
    (* polymorphism is not explicit.                            *)
    let (params_with_type, _, _) =
      MiscHelpers.bind_parameters_to_types_from_type_scheme scheme params in
    (* We are printing each parameter's type. These types in fact belong *)
    (* to a same type scheme. Hence, they may share variables together.  *)
    (* For this reason, we first purge the printing variable mapping and *)
    (* after, activate its persistence between each parameter printing.  *)
    Types.purge_type_simple_to_ml_variable_mapping () ;
    List.iter
      (fun (param_vname, opt_param_ty) ->
        match opt_param_ty with
         | Some param_ty ->
             Format.fprintf out_fmter "@ (%a : %a)"
               Parsetree_utils.pp_vname_with_operators_expanded param_vname
               (Types.pp_type_simple_to_ml
                  ~current_unit: ctx.Context.scc_current_unit
                  ~reuse_mapping: true collections_carrier_mapping) param_ty
         | None ->
             Format.fprintf out_fmter "@ %a"
               Parsetree_utils.pp_vname_with_operators_expanded param_vname)
      params_with_type ;
    (* Now we don't need anymore the sharing. Hence, clean it. This should *)
    (* not be useful because the other guys usign printing should manage   *)
    (* this themselves (as we did just above by cleaning before activating *)
    (* the sharing), but anyway, it is safer an not costly. So...          *)
    Types.purge_type_simple_to_ml_variable_mapping () ;
    (* The "=" sign ending the OCaml function's "header". With a *)
    (* NON-breakable space to prevent uggly hyphenation !        *)
    Format.fprintf out_fmter " =@ " ;
    (* Generates the body's code of the method. *)
    let expr_ctx = {
      Context.rcc_current_unit = ctx.Context.scc_current_unit ;
      Context.rcc_species_parameters_names =
        ctx.Context.scc_species_parameters_names ;
      Context.rcc_collections_carrier_mapping =
        ctx.Context.scc_collections_carrier_mapping ;
      Context.rcc_lambda_lift_params_mapping =
        ctx.Context.scc_lambda_lift_params_mapping ;
      Context.rcc_out_fmter = out_fmter } in
    (* No local idents in the context because we just enter the scope *)
    (* of a species fields and so we are not under a core expression. *)
    Base_exprs_ml_generation.generate_expr expr_ctx ~local_idents: [] env body ;
    (* Done... Then, final carriage return. *)
    Format.fprintf out_fmter "@]@\n" ;
    (* Since the method is not inherited, the effective dependencies on *)
    (* species parameters are really those passed as argument.          *)
    (abstracted_methods, dependencies_from_params)
    end)
  else
    (begin
    (* Just a bit of debug/information if requested. *)
    if Configuration.get_verbose () then
      Format.eprintf
        "Field '%a' inherited from species '%a' but not (re)-declared is not \
        generated again.@."
        Parsetree_utils.pp_vname_with_operators_expanded name
        Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
    (* Recover the arguments for abstracted methods of Self in the inherited *)
    (* generator EXCEPT the info about dependencies on species that are now  *)
    (* obsolete in the pair we get from the inherited species.               *)
    let (abstracted_methods, _) =
      find_inherited_method_generator_abstractions
        ~current_unit: ctx.Context.scc_current_unit
        from.Env.fh_initial_apparition name env in
    (* We always keep the new dependencies on species parameters we *)
    (* computed. The old ones, those recovered in the inherited     *)
    (* stuff are only valid in the scope of the inherited species.  *)
    (abstracted_methods, dependencies_from_params)
    end)
;;



(* *********************************************************************** *)
(* species_compil_context -> Parsetree.vname list ->                       *)
(*   Env.TypeInformation.species_field -> unit                             *)
(** {b Desc} : Generates the OCaml code for ONE method field (i.e. for one
    let-bound construct or for one bunch of items of a let-rec-bound
    construct.

    {b Args} :
      - [ctx] : The species-compilation-context merging the various
          stuffs sometimes needed during the compilation pass.
      - [field] : The species's field to compile (i.e. a "let", "let rec",
                "sig", "theorem" or "property").

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_methods ctx env field =
  match field with
   | Abstractions.FAI_sig (from, name, sch) ->
       (* Only declared, hence, no code to generate yet ! *)
       if Configuration.get_verbose () then
         Format.eprintf "OCaml code for signature '%a' leads to void code.@."
           Parsetree_utils.pp_vname_with_operators_expanded name ;
       let compiled_field = {
         Misc_common.cfm_from_species = from ;
         Misc_common.cfm_method_name = name ;
         Misc_common.cfm_method_scheme = sch ;
         (* Never used for OCaml. *)
         Misc_common.cfm_used_species_parameter_tys = [] ;
         Misc_common.cfm_dependencies_from_parameters = [] ;
         Misc_common.cfm_coq_min_typ_env_names = [] } in
       Some (Misc_common.CSF_sig compiled_field)
   | Abstractions.FAI_let ((from, name, params, scheme, body, _, _),
                           abstraction_info) ->
       (begin
       match body with
        | Parsetree.BB_logical _ ->
            (* In OCaml, logical lets are not generated. *)
            None
        | Parsetree.BB_computational body_expr ->
            (* No recursivity, then the method cannot call itself in its body *)
            (* then no need to set the [scc_lambda_lift_params_mapping] of    *)
            (* the context.                                                   *)
            let all_deps_from_params =
              Abstractions.merge_abstraction_infos
                abstraction_info.Abstractions.
                  ai_dependencies_from_params_via_body
                (Abstractions.merge_abstraction_infos
                   abstraction_info.Abstractions.
                     ai_dependencies_from_params_via_type
                   abstraction_info.Abstractions.
                     ai_dependencies_from_params_via_completion) in
            let (coq_min_typ_env_names, effective_deps_from_params) =
              generate_one_field_binding
                ctx env abstraction_info.Abstractions.ai_min_coq_env
                ~let_connect: LC_first_non_rec all_deps_from_params
                (from, name, params, (Some scheme), body_expr) in
            (* Now, build the [compiled_field_memory], even if the method  *)
            (* was not really generated because it was inherited.          *)
            let compiled_field = {
              Misc_common.cfm_from_species = from ;
              Misc_common.cfm_method_name = name ;
              Misc_common.cfm_method_scheme = scheme ;
              (* Never used for OCaml. *)
              Misc_common.cfm_used_species_parameter_tys = [] ;
              Misc_common.cfm_dependencies_from_parameters =
                effective_deps_from_params ;
              Misc_common.cfm_coq_min_typ_env_names = coq_min_typ_env_names } in
            Some (Misc_common.CSF_let compiled_field)
       end)
   | Abstractions.FAI_let_rec l ->
       (begin
       match l with
        | [] ->
            (* A "let", then a fortiori "let rec" construct *)
            (* must at least bind one identifier !          *)
            assert false
        | ((from, name, params, scheme, body, _, _), first_ai) :: q ->
            (begin
            match body with
             | Parsetree.BB_logical _ ->
                 (* In OCaml, logical lets are not generated. *)
                 None
             | Parsetree.BB_computational body_expr ->
                 (* Extend the context with the mapping between these *)
                 (* recursive functions and their extra arguments.    *)
                 let ctx' = {
                   ctx with
                   Context.scc_lambda_lift_params_mapping =
                     List.map
                       (fun ((_, n, _, _, _, _, _), ai) ->
                         (n, make_params_list_from_abstraction_info ai))
                       l } in
                 let all_deps_from_params =
                   Abstractions.merge_abstraction_infos
                     first_ai.Abstractions.ai_dependencies_from_params_via_body
                     (Abstractions.merge_abstraction_infos
                        first_ai.Abstractions.
                          ai_dependencies_from_params_via_type
                        first_ai.Abstractions.
                          ai_dependencies_from_params_via_completion) in
                 (* Now, generate the first method, introduced by "let rec". *)
                 let (first_coq_min_typ_env_names,
                      first_effective_deps_from_params) =
                   generate_one_field_binding
                     ctx' env first_ai.Abstractions.ai_min_coq_env
                     ~let_connect: LC_first_rec all_deps_from_params
                     (from, name, params, (Some scheme), body_expr) in
                 let first_compiled = {
                   Misc_common.cfm_from_species = from ;
                   Misc_common.cfm_method_name = name ;
                   Misc_common.cfm_method_scheme = scheme ;
                   (* Never used for OCaml. *)
                   Misc_common.cfm_used_species_parameter_tys = [] ;
                   Misc_common.cfm_dependencies_from_parameters =
                     first_effective_deps_from_params ;
                   Misc_common.cfm_coq_min_typ_env_names =
                     first_coq_min_typ_env_names } in
                 (* Finally, generate the remaining  methods, *)
                 (* introduced by "and".                      *)
                 let rem_compiled =
                   List.map
                     (fun ((from, name, params, scheme, body, _, _), ai) ->
                       let body_e =
                         (match body with
                          | Parsetree.BB_logical _ ->
                              (* If the first one was not a logical, then   *)
                              (* the remaning ones must not be otherwise    *)
                              (* the check done at scoping time is bugged ! *)
                              assert false
                          | Parsetree.BB_computational e -> e) in
                       let all_deps_from_params =
                         Abstractions.merge_abstraction_infos
                           ai.Abstractions.ai_dependencies_from_params_via_body
                           (Abstractions.merge_abstraction_infos
                              ai.Abstractions.
                                ai_dependencies_from_params_via_type
                              ai.Abstractions.
                                ai_dependencies_from_params_via_completion) in
                       let (coq_min_typ_env_names, effective_deps_from_params) =
                         generate_one_field_binding
                           ctx' env ai.Abstractions.ai_min_coq_env
                           ~let_connect: LC_following all_deps_from_params
                           (from, name, params, (Some scheme), body_e) in
                       { Misc_common.cfm_from_species = from ;
                         Misc_common.cfm_method_name = name ;
                         Misc_common.cfm_method_scheme = scheme ;
                         (* Never used for OCaml. *)
                         Misc_common.cfm_used_species_parameter_tys = [] ;
                         Misc_common.cfm_dependencies_from_parameters =
                           effective_deps_from_params ;
                         Misc_common.cfm_coq_min_typ_env_names =
                           coq_min_typ_env_names })
                     q in
                 Some (Misc_common.CSF_let_rec (first_compiled :: rem_compiled))
            end)
       end)
   | Abstractions.FAI_theorem ((from, name, sch, _, _, _), _) ->
       (* Theorems are purely discarded in the Ocaml translation. *)
       if Configuration.get_verbose () then
         Format.eprintf
           "OCaml code for theorem '%a' leads to void code.@."
           Parsetree_utils.pp_vname_with_operators_expanded name ;
       let compiled_field = {
         Misc_common.cfm_from_species = from ;
         Misc_common.cfm_method_name = name ;
         Misc_common.cfm_method_scheme = sch ;
         (* Never used for OCaml. *)
         Misc_common.cfm_used_species_parameter_tys = [] ;
         Misc_common.cfm_dependencies_from_parameters = [] ;
         Misc_common.cfm_coq_min_typ_env_names = [] } in
       Some (Misc_common.CSF_theorem compiled_field)
   | Abstractions.FAI_property ((from, name, sch, _, _), _) ->
       (* Properties are purely discarded in the Ocaml translation. *)
       if Configuration.get_verbose () then
         Format.eprintf
           "OCaml code for property '%a' leads to void code.@."
           Parsetree_utils.pp_vname_with_operators_expanded name ;
       let compiled_field = {
         Misc_common.cfm_from_species = from ;
         Misc_common.cfm_method_name = name ;
         Misc_common.cfm_method_scheme = sch ;
         (* Never used for OCaml. *)
         Misc_common.cfm_used_species_parameter_tys = [] ;
         Misc_common.cfm_dependencies_from_parameters = [] ;
         Misc_common.cfm_coq_min_typ_env_names = [] } in
       Some (Misc_common.CSF_property compiled_field)
;;



(* ************************************************************************** *)
(* Context.species_compil_context -> Env.MlGenEnv.t ->                        *)
(*   compiled_field_memory -> unit                                            *)
(** {b Descr} : We search to instanciate the parameters (IS and IN) of the
    method generator of [field_memory]. The parameters we deal with are those
    coming from the lambda-lifts we did to abstract dependencies of the
    method described by [field_memory] on species parameters of the species
    where this method is DEFINED.
    Hence we deal with the species parameters of the species where the method
    was DEFINED ! It must be clear that we do not matter of the parameters of
    the species who inherited !!! We want to trace by what the parameters of
    the original hosting species were instanciated along the inheritance.
    So we want to generate the OCaml code that enumerate the arguments to
    apply to the method generator. These arguments are the methods coming
    from species parameters on which the current method has dependencies on.
    The locations from where these methods come depend on the instanciations
    that have be done during inheritance.
    This function trace these instanciations to figure out exactly from where
    these methods come.
    Process sketch:
     - Find at the point (i.e. the species) where the method generator of
       [field_memory] was defined, the species parameters that were existing
       at this point.
     - Find the dependencies the original method had on these (its) species
       parameters.
     - For each of these parameters, we must trace by what it was
       instanciated along the inheritance history, (starting from oldest
       species where the method appeared to most recent) and then generate
       the corresponding OCaml code.
       - Find the index of the parameter in the species's signature from
         where the method was REALLY defined (not the one where it is
         inherited).
       - Follow instanciations that have been done on the parameter from
         past to now along the inheritance history.
       - If it is a IN parameter then we must generate the code
         corresponding to the FoCaL expression that instanciated the
         parameter. This expression is built by applying effective-to-formal
         arguments substitutions.
       - If it is a IS parameter, then we must generate for each method we
         have dependencies on, the OCaml code accessing the Ocaml code of
         the method inside its module structure (if instanciation is done by
         a toplevel species/collection) or directly use an existing
         collection generator parameter (if instanciation is done by a
         parameter of the species where the method is found inherited, i.e.
         the species we are currently compiling).

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let instanciate_parameter_through_inheritance ctx env field_memory =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* We first must search at the origin of the method generator, the     *)
  (* arguments it had. Since the method we are dealing with is inherited *)
  (* it is mandatorily hosted in an existing species reachable via the   *)
  (* environment.                                                        *)
  (* This ident is temporary and created just to lookup in the environment. *)
  let host_ident =
    Parsetree_utils.make_pseudo_species_ident
      ~current_unit
     field_memory.Misc_common.cfm_from_species.Env.fh_initial_apparition in
  let (original_host_species_params, host_method_infos, _, _) =
    Env.MlGenEnv.find_species
      ~loc: Location.none ~current_unit host_ident env in
  if Configuration.get_verbose () then
    Format.eprintf "Originally hosting species '%a' has %d parameters.@."
      Sourcify.pp_ident host_ident (List.length original_host_species_params) ;
  (* We search the dependencies the original method had on its species *)
  (* parameters' methods.                                              *)
  let meth_info =
    List.find
      (fun inf -> inf.Env.mi_name = field_memory.Misc_common.cfm_method_name)
      host_method_infos in
  if Configuration.get_verbose () then
    (begin
    Format.eprintf "Method '%a' has the following dependencies on parameters:@."
      Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
    List.iter
      (fun (species_param, meths_from_param) ->
        let species_param_name =
          match species_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
               Parsetree.Vuident n in
        Format.eprintf "\t From parameter '%a', dependencies on methods: "
          Sourcify.pp_vname species_param_name;
        Parsetree_utils.DepNameSet.iter
          (fun (meth, _) -> Format.eprintf "%a " Sourcify.pp_vname meth)
          meths_from_param ;
        Format.eprintf "@.")
      meth_info.Env.mi_dependencies_from_parameters
    end) ;
  (* For each species parameter, we must trace by what it was instanciated. *)
  List.iter
    (fun (species_param, meths_from_param) ->
      (* Find the index of the parameter in the species's signature from *)
      (* where the method was REALLY defined (not the one where it is    *)
      (* inherited).                                                     *)
      let original_param_index =
        Handy.list_first_index
          (fun p -> p = species_param) original_host_species_params in
      match species_param with
       | Env.TypeInformation.SPAR_in (param_name, _, _) ->
           (* By construction, in dependencies of "in" parameter, the list *)
           (* of methods is always 1-length at most and contains directly  *)
           (* the name of the parameter itself if it is really used.       *)
           let number_meth =
             Parsetree_utils.DepNameSet.cardinal meths_from_param in
           assert (number_meth <= 1) ;
           if number_meth = 1 then
             (begin
             (* For substitution, we technically need to know in which     *)
             (* compilation unit the parameter, hence in fact the species, *)
             (* was.                                                       *)
             let (original_param_unit, _) =
               field_memory.Misc_common.cfm_from_species.
                 Env.fh_initial_apparition in
             (* We get the FoCaL expression once substitutions are done. *)
             let instancied_expr =
               Misc_common.follow_instanciations_for_in_param
                 ctx (Abstractions.EK_ml env) param_name
                 original_param_unit original_param_index
                 field_memory.Misc_common.cfm_from_species.
                   Env.fh_inherited_along in
             (* We must now generate the OCaml *)
             (* code for this FoCaL expression. *)
             let reduced_ctx = {
               Context.rcc_current_unit = ctx.Context.scc_current_unit ;
               Context.rcc_species_parameters_names =
                 ctx.Context.scc_species_parameters_names ;
               Context.rcc_collections_carrier_mapping =
                 ctx.Context.scc_collections_carrier_mapping ;
               Context.rcc_lambda_lift_params_mapping =
                 ctx.Context.scc_lambda_lift_params_mapping ;
               Context.rcc_out_fmter = out_fmter } in
             Format.fprintf out_fmter "@ @[<1>(" ;
             Base_exprs_ml_generation.generate_expr
               reduced_ctx ~local_idents: [] env instancied_expr ;
             Format.fprintf out_fmter ")@]"
             end)
       | Env.TypeInformation.SPAR_is ((_, _), _, _, _) ->
           (begin
           (* Instanciation process of "IS" parameter. We start processing *)
           (* from the oldest species where the currently compiled method  *)
           (* appeared, i.e. the species where it was really DEFINED.      *)
           let instancied_with =
             Misc_common.follow_instanciations_for_is_param
               ctx (Abstractions.EK_ml env) original_param_index
               field_memory.Misc_common.cfm_from_species.
                 Env.fh_inherited_along in
           (* Now really generate the code of by what to instanciate. *)
           (match instancied_with with
            | Misc_common.IPI_by_toplevel_species (spec_mod, spec_name) ->
                (* We found that a toplevel species provides this method  *)
                (* because this species is finally used as effective      *)
                (* parameter. However, may be the method on which we have *)
                (* a dependency is not directly in this toplevel species. *)
                (* May be it is in one of its parents. We must search in  *)
                (* its inheritance to determine exactly in which species  *)
                (* each method is REALLY defined (not only inherited).    *)
                Parsetree_utils.DepNameSet.iter
                  (fun (meth, _) ->
                    let (real_spec_mod, real_spec_name) =
                      Misc_common.
                      find_toplevel_spe_defining_meth_through_inheritance
                        (Abstractions.EK_ml env) ~current_unit
                        ~start_spec_mod: spec_mod ~start_spec_name: spec_name
                        ~method_name: meth in
                    let capitalized_real_spec_mod =
                      String.capitalize real_spec_mod in
                    let prefix =
                      if real_spec_mod = current_unit then real_spec_name ^ "."
                      else
                        capitalized_real_spec_mod ^ "." ^ real_spec_name ^ "." ^
                        capitalized_real_spec_mod in
                    Format.fprintf out_fmter "@ %s%a"
                      prefix Parsetree_utils.pp_vname_with_operators_expanded
                      meth)
                  meths_from_param
            | Misc_common.IPI_by_toplevel_collection (coll_mod, coll_name) ->
                let capitalized_coll_mod = String.capitalize coll_mod in
                let prefix =
                  if coll_mod = current_unit then
                    coll_name ^ ".effective_collection." ^ coll_name ^ "."
                  else capitalized_coll_mod ^ "." ^ coll_name ^
                    ".effective_collection." ^ capitalized_coll_mod ^ "." ^
                    coll_name ^ "." in
                Parsetree_utils.DepNameSet.iter
                  (fun (meth, _) ->
                    (* Don't print the type to prevent being too verbose. *)
                    Format.fprintf out_fmter "@ %s%a"
                      prefix Parsetree_utils.pp_vname_with_operators_expanded
                      meth)
                  meths_from_param
              | Misc_common.IPI_by_species_parameter prm ->
                  let species_param_name =
                    match prm with
                     | Env.TypeInformation.SPAR_in (_, _, _) -> assert false
                     | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
                         Parsetree.Vuident n in
                  let prefix =
                    "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
                    "_" in
                  Parsetree_utils.DepNameSet.iter
                    (fun (meth, _) ->
                      (* Don't print the type to prevent being too verbose. *)
                      Format.fprintf out_fmter "@ %s%a"
                        prefix Parsetree_utils.pp_vname_with_operators_expanded
                        meth)
                    meths_from_param)
           end))
    meth_info.Env.mi_dependencies_from_parameters ;
;;


    
(* *********************************************************************** *)
(* Format.formatter -> compiled_species_fields list ->                     *)
(*  (Parsetree.vname * Parsetree_utils.DepNameSet.t) list                  *)
(** {b Descr} : Dumps as OCaml code the parameters required to the
         collection generator in order to make them bound in the
         collection generator's body. These parameters come from
         the methods of the species parameters that some of our methods
         depend on. This means that a closed species with no species
         parameters will have NO extra parameters in its collection
         generator.

         This function must UNIQUELY find the names of all the extra
         parameters the methods will need to make them arguments of the
         collection generator and record then in a precise order that must
         be made public for the guys who want to instanciate the collection.

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let dump_collection_generator_arguments_for_params_methods out_fmter
    compiled_species_fields =
  (* Let's create an assoc list mapping for each species paramater name *)
  (* the set of methods names from it that needed to be lambda-lifted,  *)
  (* hence that will lead to parameters of the collection generator.    *)
  let species_param_names_and_methods =
    ref ([] : (Parsetree.vname * Parsetree_utils.DepNameSet.t ref) list) in
  (* ************************************************************************ *)
  (** {b Descr} :  Local function to process only one [compiled_field_memory].
         Handy to factorize the code in both the cases of [CSF_let] and
         [CSF_let_rec]. This function effectivly accumulates by side effect
         for each species parameter the set of methods we depend on.

      { b Rem} : Local to the enclosing [dump_collection_generator_arguments]
               function. Not exported.                                        *)
  (* ************************************************************************ *)
  let rec process_one_field_memory field_memory =
    List.iter
      (fun (spe_param, meths_set) ->
        (* Recover the species parameter's name. *)
        let spe_param_name =
          match spe_param with
           | Env.TypeInformation.SPAR_in (n, _, _) -> n
           | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
               Parsetree.Vuident n in
        (* Get or create for this species parameter name, the bucket *)
        (* recording all the methods someone depends on.             *)
        (* We don't care here about whether the species parameters is   *)
        (* "in" or "is".                                                *)
        let spe_param_bucket =
          (try List.assoc spe_param_name !species_param_names_and_methods
          with Not_found ->
            let bucket = ref Parsetree_utils.DepNameSet.empty in
            species_param_names_and_methods :=
              (spe_param_name, bucket) :: !species_param_names_and_methods ;
            bucket) in
        (* And now, union the current methods we depend on with *)
        (* the already previously recorded.                     *)
        spe_param_bucket :=
          Parsetree_utils.DepNameSet.union meths_set !spe_param_bucket)
      field_memory.Misc_common.cfm_dependencies_from_parameters in

  (* ********************************************************** *)
  (* Now, really work, building by side effect for each species *)
  (* parameter the set of methods we depend on.                 *)
  List.iter
    (function
      | Misc_common.CSF_sig _ | Misc_common.CSF_property _
      | Misc_common.CSF_theorem _ -> ()
      | Misc_common.CSF_let field_memory ->
          process_one_field_memory field_memory
      | Misc_common.CSF_let_rec l -> List.iter process_one_field_memory l)
    compiled_species_fields ;
  (* Now we get the assoc list complete, we can dump the parameters of the  *)
  (* collection generator. To make them correct with their usage inside the *)
  (* local functions of the collection generator, we must give them a name  *)
  (* shaped in the same way, i.e:                                           *)
  (* "_p_" + species parameter name + "_" + called method name.             *)
  List.iter
    (fun (species_param_name, meths_set) ->
      let prefix =
        "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
        "_" in
      Parsetree_utils.DepNameSet.iter
        (fun (meth, _) ->
          (* Don't print the type to prevent being too verbose. *)
          Format.fprintf out_fmter "@ %s%a"
            prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
        !meths_set)
  !species_param_names_and_methods ;
  (* Finally, make this parameters information public by returning it. By     *)
  (* the way, the ref on the inner set is not anymore needed, then remove it. *)
  List.map
    (fun (sp_par_name, meths_set) -> (sp_par_name, !meths_set))
    !species_param_names_and_methods
;;



let generate_collection_generator ctx env compiled_species_fields =
  let current_species_name = snd ctx.Context.scc_current_species in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "Species %a is fully defined. Generating its collection generator.@."
      Sourcify.pp_vname current_species_name ;

  (* ******************************************************************* *)
  (** {b Descr} : A local function to process one field. This allows to
                factorize the processing for both [Let] and [Let_rec]
                definitions.

      {b Rem} : Local to the [generate_collection_generator] function.
               Not exported.                                             *)
  (* ******************************************************************* *)
  let process_one_field field_memory =
    let from = field_memory.Misc_common.cfm_from_species in
    Format.fprintf out_fmter "(* From species %a. *)@\n"
      Sourcify.pp_qualified_species from.Env.fh_initial_apparition ;
    Format.fprintf out_fmter "@[<2>let local_%a =@ "
      Parsetree_utils.pp_vname_with_operators_expanded
      field_memory.Misc_common.cfm_method_name ;
    if Configuration.get_verbose () then
      Format.eprintf "Generating OCaml code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this *)
    (* inheritance level or if it was inherited from another species.      *)
    if from.Env.fh_initial_apparition = ctx.Context.scc_current_species then
      (begin
      if Configuration.get_verbose () then
        Format.eprintf
          "Method '%a' not inherited, building method generator using \
          abstracted local species parameters as arguments.@."
          Sourcify.pp_vname field_memory.Misc_common.cfm_method_name ;
      (* It comes from the current inheritance level.   *)
      (* Then its name is simply the the method's name. *)
      Format.fprintf out_fmter "%a"
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments *)
      (* induced by the various lambda-lifting we previously performed. *)
      (* First, the extra arguments due to the species parameters methods we  *)
      (* depends on. Here we will not use them to lambda-lift them this time, *)
      (* but to apply them ! The name used for application is formed          *)
      (* according to the same scheme we used at lambda-lifting time:         *)
      (* "_p_" + species parameter name + "_" + called method name.           *)
      List.iter
        (fun (species_param, meths_from_param) ->
          (* Recover the species parameter's name. *)
          let species_param_name =
            match species_param with
             | Env.TypeInformation.SPAR_in (n, _, _) -> n
             | Env.TypeInformation.SPAR_is ((_, n), _, _, _) ->
                 Parsetree.Vuident n in
          (* We don't care here about whether the species parameters is   *)
          (* "in" or "is".                                                *)
          let prefix =
            "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
            "_" in
          Parsetree_utils.DepNameSet.iter
            (fun (meth, _) ->
              (* Don't print the type to prevent being too verbose. *)
              Format.fprintf out_fmter "@ %s%a"
                prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
            meths_from_param)
        field_memory.Misc_common.cfm_dependencies_from_parameters
      end)
    else
      (begin
      (* It comes from a previous inheritance level. Then its name is *)
      (* the the module where the species inhabits if not the same    *)
      (* compilation unit than the current + "." + species name as    *)
      (* module + "." + the method's name.                            *)
      let (defined_from_mod, defined_from_species) =
        from.Env.fh_initial_apparition in
      if Configuration.get_verbose () then
        Format.eprintf
          "Method '%a' inherited, from '%s#%a'.@."
          Sourcify.pp_vname field_memory.Misc_common.cfm_method_name
          defined_from_mod Sourcify.pp_vname defined_from_species ;
      if defined_from_mod <> ctx.Context.scc_current_unit then
        Format.fprintf out_fmter "%s.@,"
          (String.capitalize defined_from_mod) ;
      Format.fprintf out_fmter "%a.@,%a"
        Parsetree_utils.pp_vname_with_operators_expanded defined_from_species
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.Misc_common.cfm_method_name ;
      (* Now, apply the method generator to each of the extra arguments *)
      (* induced by the various lambda-lifting we previously in the     *)
      (* species from which we inherit, i.e. where the method was       *)
      (* defined. During the inheritance, parameters have been          *)
      (* instanciated. We must track these instanciations to know to    *)
      (* what apply the method generator.                               *)
      instanciate_parameter_through_inheritance ctx env field_memory
      end) ;
    (* Now, apply the method generator to each of the extra arguments         *)
    (* induced by the various lambda-lifting we previously performed.         *)
    (* Second, the methods of our inheritance tree we depend on and that are  *)
    (* only declared. These methods leaded to "local" functions defined       *)
    (* above. Hence, for each  method only declared of ourselves we depend    *)
    (* on, its name is "local_" + the method's name.                          *)
    List.iter
      (fun n ->
        Format.fprintf out_fmter "@ local_%a"
          Parsetree_utils.pp_vname_with_operators_expanded n)
      field_memory.Misc_common.cfm_coq_min_typ_env_names ;
    (* That's all for this field code generation. *)
    Format.fprintf out_fmter "@ in@]@\n" ;
    if Configuration.get_verbose () then
      Format.eprintf "End of OCaml code for method generator of '%a'.@."
        Sourcify.pp_vname field_memory.Misc_common.cfm_method_name in

  (* *********************** *)
  (* Now, let's really work. *)
  (* A little comment in the generated OCaml code. *)
  Format.fprintf out_fmter
    "(* Fully defined '%a' species's collection generator. *)@\n"
    Sourcify.pp_vname current_species_name ;
  (* The generic name of the collection generator: "collection_create". *)
  (* Be careful, if the collection generator has no extra parameter     *)
  (* then the "collection_create" will not be a function but directly   *)
  (* the record representing the species. In this case, if some fields  *)
  (* of these records are polymorphic, OCaml won't generalize because   *)
  (* it is unsound to generalize a value that is expansive (and         *)
  (* recor values are expansives). So to ensure this won't arise, we    *)
  (* always 1 unit argument to the generator. We could add it only if   *)
  (* there is no arguments to the generator, but is it really a matter? *)
  Format.fprintf out_fmter "@[<2>let collection_create ()" ;
  (* Generate the parameters the collection generator needs to build the   *)
  (* each of the current species's local function (functions corresponding *)
  (* to the actual method stored in the collection record).                *)
  (* These parameters of the generator come from the abstraction of        *)
  (* methods coming from our species parameters we depend on. By the way,  *)
  (* recover the list of species parameters linked together with their     *)
  (* methods we need to instanciate in order to apply the collection       *)
  (* generator.                                                            *)
  let extra_args_from_spe_params =
    dump_collection_generator_arguments_for_params_methods
      out_fmter compiled_species_fields in
  Format.fprintf out_fmter " =@ " ;
  (* Generate the local functions that will be used to fill the record value. *)
  List.iter
    (function
      | Misc_common.CSF_sig _ | Misc_common.CSF_property _
      | Misc_common.CSF_theorem _ -> ()
      | Misc_common.CSF_let field_memory -> process_one_field field_memory
      | Misc_common.CSF_let_rec l ->
          List.iter (fun fm -> process_one_field fm) l)
    compiled_species_fields ;
  (* And now, the record value. We just assign each record fields    *)
  (* corresponding to the current species's method the corresponding *)
  (* local function we defined just above. Remind that the record    *)
  (* field's is simply the method's name.                            *)
  (* The local function corresponding to the method is "local_" +    *)
  (* the method's name.                                              *)
  Format.fprintf ctx.Context.scc_out_fmter "@[<2>{ " ;
  List.iter
      (function
      | Misc_common.CSF_sig _ | Misc_common.CSF_property _
      | Misc_common.CSF_theorem _ -> ()
      | Misc_common.CSF_let field_memory ->
          Format.fprintf ctx.Context.scc_out_fmter "%a =@ local_%a ;@\n"
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.Misc_common.cfm_method_name
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.Misc_common.cfm_method_name
      | Misc_common.CSF_let_rec l ->
          List.iter
            (fun field_memory ->
              Format.fprintf ctx.Context.scc_out_fmter "%a =@ local_%a ;@\n"
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.Misc_common.cfm_method_name
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.Misc_common.cfm_method_name)
            l)
    compiled_species_fields ;
  (* Close the record expression. *)
  Format.fprintf ctx.Context.scc_out_fmter "@ }@]@\n" ;
  (* Close the pretty-print box of the "let collection_create ... =". *)
  Format.fprintf ctx.Context.scc_out_fmter "@]@\n" ;
  extra_args_from_spe_params
;;



let species_compile env ~current_unit out_fmter species_def species_descr
    dep_graph =
  let species_def_desc = species_def.Parsetree.ast_desc in
  let species_name = species_def_desc.Parsetree.sd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for species %a@."
      Sourcify.pp_vname species_name ;
  (* Start the module encapsulating the species representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname species_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit species_descr in
  (* Create the initial compilation context for this species. *)
  let ctx = {
    Context.scc_current_unit = current_unit ;
    Context.scc_current_species = (current_unit, species_name) ;
    Context.scc_dependency_graph_nodes = dep_graph ;
    Context.scc_species_parameters_names =
      species_descr.Env.TypeInformation.spe_sig_params ;
    Context.scc_collections_carrier_mapping = collections_carrier_mapping ;
    Context.scc_lambda_lift_params_mapping = [] ;
    Context.scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_descr ;
  (* Now, the methods of the species. *)
  let field_abstraction_infos =
    Abstractions.compute_abstractions_for_fields
      ~with_def_deps: false (Abstractions.EK_ml env)
      ctx species_descr.Env.TypeInformation.spe_sig_methods in
  let compiled_fields_as_options =
    List.map (generate_methods ctx env) field_abstraction_infos in
  let compiled_fields = Handy.option_list_to_list compiled_fields_as_options in
  (* Now build the list of the species parameters names to make *)
  (* them public in the future ml generation environnment.      *)
  let species_params_names_n_kinds =
    List.map
      (fun (pname, pkind) ->
        match pkind.Parsetree.ast_desc with
         | Parsetree.SPT_in _ -> (pname, Env.ScopeInformation.SPK_in)
         | Parsetree.SPT_is _ -> (pname, Env.ScopeInformation.SPK_is))
      species_def_desc.Parsetree.sd_params in
  (* Now check if the species supports a collection generator because fully *)
  (* defined and get the information about which arguments to pass in order *)
  (* to later call the collection generator.                                *)
  let extra_args_from_spe_params =
    if species_descr.Env.TypeInformation.spe_is_closed then
      Some
        { Env.MlGenInformation.cgi_implemented_species_params_names =
            species_params_names_n_kinds ;
          Env.MlGenInformation.cgi_generator_parameters =
            generate_collection_generator ctx env compiled_fields }
    else None in
  Format.fprintf out_fmter "end ;;@]@\n@." ;
  (* Now, extract the fields names to create the [species_binding_info]. *)
  let species_binding_info =
    List.flatten
      (List.map
         (function
           | Misc_common.CSF_sig compiled_field_memory
           | Misc_common.CSF_let compiled_field_memory
           | Misc_common.CSF_property compiled_field_memory
           | Misc_common.CSF_theorem compiled_field_memory ->
               [{ Env.mi_name =
                    compiled_field_memory.Misc_common.cfm_method_name ;
                  Env.mi_history =
                    compiled_field_memory.Misc_common.cfm_from_species ;
                  Env.mi_dependencies_from_parameters =
                    compiled_field_memory.Misc_common.
                      cfm_dependencies_from_parameters ;
                  Env.mi_abstracted_methods =
                    compiled_field_memory.Misc_common.
                      cfm_coq_min_typ_env_names }]
           | Misc_common.CSF_let_rec compiled_field_memories ->
               List.map
                 (fun cfm ->
                   { Env.mi_name =
                       cfm.Misc_common.cfm_method_name ;
                     Env.mi_history =
                       cfm.Misc_common.cfm_from_species ;
                     Env.mi_dependencies_from_parameters =
                       cfm.Misc_common.cfm_dependencies_from_parameters ;
                     Env.mi_abstracted_methods =
                       cfm.Misc_common.cfm_coq_min_typ_env_names })
                 compiled_field_memories)
         compiled_fields) in
  (* Return what is needed to enter this species *)
  (* in the ml generation environnment.          *)
  (species_descr.Env.TypeInformation.spe_sig_params, species_binding_info,
   extra_args_from_spe_params, Env.COS_species)
;;



(* ************************************************************************* *)
(* species_compil_context -> collection_effective_arguments list ->          *)
(*   Env.MlGenInformation.collection_generator_info -> unit                  *)
(** {b Descr} : This function recovers from the fully defined species that
    the collection implements, the names of its parameters names. Then it
    gets the parameters the collection generator needs and actually applies
    it to correct corresponding functions.
    This "correct corresponding" functions are those from the collection that
    instanciates the species parameter, but coming from the collection that
    instanciates this species parameter in the freshly defined collection.

    For example, let's take "collection A implements B (C, D)", with B being
    parameterized by the formal X and Y. Imagine we determined that B's
    collection generator needs to be feeded an argument representing a method
    "foo" of X and "bar" of Y, in the order "bar" before and "foo" next.
    To fully apply B's collection generator, we need to pass the abstractions
    (the functions) corresponding to [bar] and [foo] IN THIS ORDER.

    In the [param_info] parameter, we know the expected function names and
    from where they come in term of formal parameter in B (i.e. we know that
    coming from the formal species parameter "X", a function for "bar" is
    needed and for the formal species parameter "Y", a function for "foo" is
    needed). And the order in which to apply them is implicitly given by the
    structure of the list [params_info.cgi_generator_parameters].

    The question is now to know which formal species parameters name of B
    corresponds to the effective collection passed as argument at
    "implements-time" in A. This information is obtained from
    [params_info.cgi_implemented_species_params_names] that is the ordered
    list of the "implemented" species (i.e. used to create a collection).
    The first element is this list is the name of the first formal parameter
    of B, I.e. X. Now because we know that "A implements B (C, D)", we know
    that X is instanciated by C and Y by D.

    Then, to apply B's collection generator, we just walk the list of
    arguments it needs ([params_info.cgi_generator_parameters]) and create
    the function identifier denoting the expected function but coming from
    the effective collection corresponding to the formal parameter this
    expected function comes from in [params_info.cgi_generator_parameters].
    For example, if [params_info.cgi_generator_parameters] tells that we
    must apply "bar" from Y, then "foo" from X, then one must generate
    the identifiers "D.bar" then "C.foo".

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let apply_collection_generator_to_parameters ctx env collection_body_params
    col_gen_params_info =
  let current_unit = ctx.Context.scc_current_unit in
  let out_fmter = ctx.Context.scc_out_fmter in
  (* Create the assoc list mapping the formal to the effective parameters. *)
  let formal_to_effective_map =
    (try
      List.map2
        (fun formal_info effective_info ->
          match (formal_info, effective_info) with
           | ((formal, Env.ScopeInformation.SPK_is),
              Misc_common.CEA_collection_name_for_is qualified_vname) ->
               (begin
               (* "Is" parameter. Leads to collection name based stuff. *)
               match qualified_vname with
                | Parsetree.Vname _ ->
                    (* Assumed to be local to the current unit. *)
                    (formal,
                     Misc_common.CEA_collection_name_for_is qualified_vname)
                | Parsetree.Qualified (effective_fname, effective_vname) ->
                    (* If the species belongs to the current unit, then we   *)
                    (* don't need to qualify it in the OCaml generated code. *)
                    (* Then we simply discard its explicit hosting           *)
                    (* information.                                          *)
                    if effective_fname = current_unit then
                      (formal,
                       Misc_common.CEA_collection_name_for_is
                         (Parsetree.Vname effective_vname))
                    else
                      (formal,
                       Misc_common.CEA_collection_name_for_is
                         (Parsetree.Qualified
                            (effective_fname, effective_vname)))
               end)
           | ((formal, Env.ScopeInformation.SPK_in),
              (Misc_common.CEA_value_expr_for_in effective_expr)) ->
               (begin
               (* "In" parameter. Leads to direct value based stuff. *)
               (formal, (Misc_common.CEA_value_expr_for_in effective_expr))
               end)
           | (_, _) ->
               (* This would mean that we try to apply an effective stuff    *)
               (* in/is-incompatible with the kind of the species parameter. *)
               (* This should have been caught before by the analyses !      *)
               assert false)
        col_gen_params_info.Env.MlGenInformation.
          cgi_implemented_species_params_names
        collection_body_params
    with Invalid_argument "List.map2" ->
      assert false  (* The lists length must be equal. *)) in
  (* Now, generate the argment identifier or expression *)
  (* for each expected collection generator parameter.  *)
  List.iter
    (fun (formal_species_param_name, method_names) ->
      match List.assoc formal_species_param_name formal_to_effective_map with
       | Misc_common.CEA_collection_name_for_is corresponding_effective ->
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
               (* OCaml code. Don't print the type to prevent being *)
               (* too verbose.                                      *)
               Format.fprintf out_fmter "@ " ;
               (match corresponding_effective_opt_fname with
                | Some fname ->
                    Format.fprintf out_fmter "%s." (String.capitalize fname)
                | None -> ()) ;
               (* Collection name."effective_collection.". *)
               Format.fprintf out_fmter "%a.effective_collection."
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname ;
               (* If needed, qualify the name of the species *)
               (* in the OCaml code.                         *)
               (match corresponding_effective_opt_fname with
                | Some fname ->
                    Format.fprintf out_fmter "%s." (String.capitalize fname)
                | None -> ()) ;
               (* Species name.method name. *)
               Format.fprintf out_fmter "%a.%a"
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname
                 Parsetree_utils.pp_vname_with_operators_expanded meth_name)
             method_names
           end)
       | Misc_common.CEA_value_expr_for_in expr ->
           (begin
           Format.fprintf out_fmter "@ (@[<1>" ;
           let expr_ctx = {
             Context.rcc_current_unit = current_unit ;
             (* Since we are in the context of a collection and since a *)
             (* collection does not have parameters, the                *)
             (* [rcc_species_parameters_names] is trivially empty.      *)
             Context.rcc_species_parameters_names = [] ;
             Context.rcc_collections_carrier_mapping =
               ctx.Context.scc_collections_carrier_mapping ;
             Context.rcc_lambda_lift_params_mapping =
               ctx.Context.scc_lambda_lift_params_mapping ;
             Context.rcc_out_fmter = out_fmter } in
           (* No local idents in the context because we just enter the scope *)
           (* of a species fields and so we are not under a core expression. *)
           Base_exprs_ml_generation.generate_expr
             expr_ctx ~local_idents: [] env expr ;
           Format.fprintf out_fmter ")@]" ;
           end))
    col_gen_params_info.Env.MlGenInformation.cgi_generator_parameters
;;



(* ************************************************************************** *)
(* current_unit: Types.fname -> Format.formatter -> Parsetree.ident -> unit   *)
(** {b Descr} : Helper that prints a species name as an OCaml module, with
    module qualification if needed.
    In other words, each time we need to refer to a module qualification
    induced by a species, this function prints the the capitalize name of
    the species, prefixed by its hosting file considered as an OCaml module
    if this species is not in the current compilation unit.
    For example, imagine we are in the "foo.foc" file and we need to speak
    of a record field of a species "S" that lives in the "bar.foc" file.
    Then because each FoCaL compilation unit is mapped onto an OCaml file
    (hence an OCaml module corresponding to the file-as-module), it will be
    printed like "Bar.S". If the species "S" was in the same compilation unit
    (i.e. "foo.foc"), then it would be printed directly "S".

    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
let print_implemented_species_as_ocaml_module ~current_unit out_fmter
    impl_species_name =
  match impl_species_name.Parsetree.ast_desc with
   | Parsetree.I_local vname
   | Parsetree.I_global (Parsetree.Vname vname) ->
       (* Local species, so no need to find it in another ML "file-module". *)
       Format.fprintf out_fmter "%s"
         (String.capitalize (Parsetree_utils.name_of_vname vname))
   | Parsetree.I_global (Parsetree.Qualified (fname, vname)) ->
       (* If the specified module name is the current compilation unit, *)
       (* then again no need to find the species's module in another ML *)
       (* "file-module" otherwise we explicitely prefix by the module   *)
       (* name corresponding to the filename.                           *)
       if fname <> current_unit then
         Format.fprintf out_fmter "%s." (String.capitalize fname) ;
       Format.fprintf out_fmter "%s"
         (String.capitalize (Parsetree_utils.name_of_vname vname))
;; (* [Unsure] String.capitalize nécessaire ? *)



(* ************************************************************************ *)
(* Env.MlGenEnv.t -> current_unit: Types.fname -> Format.formatter ->       *)
(*   Parsetree.collection_def_desc Parsetree.ast ->                         *)
(*     Env.TypeInformation.species_description ->                           *)
(*       Dep_analysis.name_node list -> unit                                *)
(** {b Descr} : Generate the OCaml code for a collection implementation.
    The compilation model dumps:
      - The record type representing the species actual representation,
      - A call the the "implemented" species's collection generator,
      - And a final value (of type above) representing the actual species
        and borrowing every fields from the value obtained via the
        collection generator application in order to make the collection
        having its own record fields names.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
let collection_compile env ~current_unit out_fmter collection_def
    collection_descr dep_graph =
  let collection_name = collection_def.Parsetree.ast_desc.Parsetree.cd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for collection %a@."
      Sourcify.pp_vname collection_name ;
  (* Start the module encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname collection_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit collection_descr in
  (* Create the initial compilation context for this collection. *)
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
  generate_record_type ctx collection_descr ;
  (* We do not want any collection generator. Instead, we will call the  *)
  (* collection generator of the collection we implement and apply it to *)
  (* the functions it needs coming from the collection applied to its    *)
  (* parameters if there are some.                                       *)
  (* Now generate the value representing the effective instance of the *)
  (* collection. We always name it by "effective_collection".          *)
  Format.fprintf out_fmter "@[<2>let effective_collection =@\n" ;
  (* The temporary value resulting from the application of *)
  (* the collection generator mentionned just above...     *)
  Format.fprintf out_fmter "@[<2>let t =@\n" ;
  (* Now, get the collection generator from the closed species we implement. *)
  let implemented_species_name =
    collection_def.Parsetree.ast_desc.Parsetree.
      cd_body.Parsetree.ast_desc.Parsetree.se_name in
  print_implemented_species_as_ocaml_module
    ~current_unit out_fmter implemented_species_name ;
  (* Don't forget to add the extra unit arument. C.f  *)
  (* [generate_collection_generator] for explanation. *)
  Format.fprintf out_fmter ".collection_create ()" ;
  (* Finally, we must recover the arguments to apply to this collection    *)
  (* generator. These arguments of course come from the species parameters *)
  (* the closed species we implement has (if it has some). We must         *)
  (* make this application WITH THE RIGHT EFFECTIVE FUNCTIONS and IN THE   *)
  (* RIGHT ORDER !                                                         *)
  (begin
  try
    let (_, _, opt_params_info, _) =
      Env.MlGenEnv.find_species
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
           Misc_common.get_implements_effectives
             collection_def.Parsetree.ast_desc.
               Parsetree.cd_body.Parsetree.ast_desc.
             Parsetree.se_params
             params_info.Env.MlGenInformation.
               cgi_implemented_species_params_names in
         apply_collection_generator_to_parameters
           ctx env collection_body_params params_info) ;
    Format.fprintf out_fmter "@ in@]@\n" ;
    (* And now, create the final value representing the effective instance *)
    (* of our collection, borrowing each field from the temporary value    *)
    (* obtained above. This way, our collection will have ITS own record   *)
    (* fields names, preventing the need to use those coming from the      *)
    (* it implements.                                                      *)
    Format.fprintf out_fmter "@[<2>{@ " ;
    (* Make the record value borrowing every fields from the temporary  *)
    (* value generated by the collection generator. Remind that logical *)
    (* let ARE NOT générated in OCaml, hence must not appear in the     *)
    (* final record value !                                             *)
    List.iter
      (function
        | Env.TypeInformation.SF_sig (_, _, _)
        | Env.TypeInformation.SF_theorem (_, _, _, _, _, _)
        | Env.TypeInformation.SF_property (_, _, _, _, _) -> ()
        | Env.TypeInformation.SF_let (_, n, _, _, _, _, log_flag) ->
            (* Generate only if not a logical let ! *)
            if log_flag = Parsetree.LF_no_logical then
              (begin
              Format.fprintf out_fmter "%a =@ t."
                Parsetree_utils.pp_vname_with_operators_expanded n ;
              print_implemented_species_as_ocaml_module
                ~current_unit out_fmter implemented_species_name ;
              Format.fprintf out_fmter ".%a ;@\n"
                Parsetree_utils.pp_vname_with_operators_expanded n
              end)
        | Env.TypeInformation.SF_let_rec l ->
            List.iter
              (fun (_, n, _, _, _, _, log_flag) ->
                (* Generate only if not a logical let ! *)
                if log_flag = Parsetree.LF_no_logical then
                  (begin
                  Format.fprintf out_fmter "%a =@ t."
                    Parsetree_utils.pp_vname_with_operators_expanded n ;
                  print_implemented_species_as_ocaml_module
                    ~current_unit out_fmter implemented_species_name ;
                  Format.fprintf out_fmter ".%a ;@\n"
                    Parsetree_utils.pp_vname_with_operators_expanded n
                  end))
              l)
      collection_descr.Env.TypeInformation.spe_sig_methods ;
    (* End the definition of the value representing the effective instance. *)
    Format.fprintf out_fmter "@ }@]@]@\n" ;
    (* End the module representing the collection. *)
    Format.fprintf out_fmter "end ;;@]@\n@."
  with Not_found ->
    (* Don't see why the species could not be present in the environment.  *)
    (* The only case would be to make a collection from a collection since *)
    (* collection are never entered in the environment because it's a non  *)
    (* sense to make a collection "implementing" a collection !            *)
    (* [Unsure]. Peut être lever un message d'erreur. *)
    assert false
  end)
;;
