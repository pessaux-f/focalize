(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: species_ml_generation.ml,v 1.23 2008-01-15 13:46:40 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to 
              Ocaml of FoCaL's collections and species.            *)
(* *************************************************************** *)


(* ********************************************************************* *)
(** {b Descr} : Data structure to record the various stuff needed to
          generate the OCaml code for a species definition. Passing this
          structure prevents from recursively passing a bunch of
          parameters to the functions. Instead, one pass only one and
          functions use the fields they need. This is mostly to preserve
          the stack and to make the code more readable. In fact,
          information recorded in this structure is semantically pretty
          un-interesting to understand the compilation process: it is
           more utilities.

    {b Rem} Not exported outside this module.                            *)
(* ********************************************************************* *)
type species_compil_context = {
  (** The name of the currently analysed compilation unit. *)
  scc_current_unit : Types.fname ;
  (** The name of the current species. *)
  scc_current_species : Parsetree.qualified_species ;
  (** The nodes of the current species's dependency graph. *)
  scc_dependency_graph_nodes : Dep_analysis.name_node list ;
  (** The current correspondance between collection types and type variable
      names representing the carrier of a species type in the OCaml code. *)
  scc_collections_carrier_mapping : (Types.type_collection * string) list ;
  (** The current correspondance between method names of Self and their
      extra parameters they must be applied to because of the lambda-lifting
      process. This info is used when generating the OCaml code of a
      method, hence it is only relevant in case of recursive methods to know
      in their own body what they must be applied to in addition to their
      explicit arguments (those given by the FoCaL programmer). *)
  scc_lambda_lift_params_mapping :
    (Parsetree.vname * ((string * Types.type_simple) list)) list ;
  (** The current output formatter where to send the generated code. *)
  scc_out_fmter : Format.formatter
} ;;




(* ************************************************************************ *)
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the type variables
              names to be used later during the OCaml translation.
              For a species parameter [A is/in ... ], the type variable
              that will be used is "'" + lowercased name of the species
              parameter + an int unique in this type + "_as_carrier".
              We need to add an extra int (a stamp) to prevent a same type
              variable from appearing several time in the tricky case where
              a IN and a IS parameters wear the same lowercased name. For
              instance: "species A (F is B, f in F)" where "F" and "f" will
              lead to a same name of ML type variable: "'f_as_carrier"

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  let cnt = ref 0 in
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, n_as_string), _, _) ->
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
          (type_coll, carrier_type_variable_name)
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name of the type variable that will represent *)
          (* this parameter's carrier type seen from OCaml. Same     *)
          (* remark than above for lowercase/uppercase.              *)
          let carrier_type_variable_name =
            "'" ^ (String.uncapitalize (Parsetree_utils.name_of_vname n)) ^
            (string_of_int !cnt) ^ "_as_carrier" in
          incr cnt ;
          (type_coll, carrier_type_variable_name))
    species_descr.Env.TypeInformation.spe_sig_params
;;



(* ******************************************************************** *)
(* ('a * string) list -> unit                                           *)
(** {b Descr} : Helper to print the list of known variables names in
              a collections carrier mapping as a legal OCaml list of
              type parameters, i.e, comma separated except for the last
              one.

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
    | [(_, last_name)] -> Format.fprintf out_fmter "%s" last_name
    | (_, h) :: q ->
        Format.fprintf out_fmter "%s,@ " h ;
        rec_print q in
  rec_print vars_list
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_field list -> unit  *)
(** {b Descr} : Checks if "rep" is defined. If so, then generate the type
              constraint reflecting its effective structure. We loosely
              iterate on the list of fields, stopping at the first occurence
              of "rep" because, by construction if "rep" is present then it
              is only once. Hence it is safe to ignore the remaining fields.

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
               Format.fprintf ctx.scc_out_fmter
                 "(* Carrier's structure explicitly given by \"rep\". *)@\n" ;
               Format.fprintf ctx.scc_out_fmter "@[<2>type " ;
               (* First, output the type parameters if some, and enclose *)
               (* them by parentheses if there are several.              *)
               (match ctx.scc_collections_carrier_mapping with
                | [] -> ()
                | [ (_, only_var_name) ] ->
                    Format.fprintf ctx.scc_out_fmter "%s@ " only_var_name
                | _ ->
                    (* More than one, then surround by parentheses. *)
                    Format.fprintf ctx.scc_out_fmter "@[<1>(" ;
                    (* Print the variables names... *)
                    print_comma_separated_vars_list_from_mapping
                      ctx.scc_out_fmter ctx.scc_collections_carrier_mapping ;
                    Format.fprintf ctx.scc_out_fmter ")@]@ ") ;
               (* Now, output the type's name and body. *)
               let ty = Types.specialize sch in
               Format.fprintf ctx.scc_out_fmter
                 "me_as_carrier =@ %a@]@\n"
                 (Types.pp_type_simple_to_ml
                    ~current_unit: ctx.scc_current_unit
                    ~reuse_mapping: false
                    ctx.scc_collections_carrier_mapping) ty
               end)
             else rec_search q
         | _ -> rec_search q
        end) in
  rec_search fields
;;



(* ************************************************************************* *)
(* species_compil_context -> Env.TypeInformation.species_description -> unit *)
(** {b Descr} : Generate the record type representing a species. This type
          contains a field per method. This type is named "me_as_species"
          to reflect the point that it represents the ML structure
          representing the FoCaL species.
          Depending on whether the species has parameters, this record
          type also has parameters. In any case, it at least has a
          parameter representing "self as it will be once instanciated"
          once "we" (i.e. the species) will be really living as a
          collection.

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let generate_record_type ctx species_descr =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
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
      (fun (_, type_variable_name) ->
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
  let (my_fname, my_species_name) = ctx.scc_current_species in
  let collections_carrier_mapping =
    ((my_fname, (Parsetree_utils.name_of_vname my_species_name)),
     "me_as_carrier") ::
    collections_carrier_mapping in
  (* The record's fields types. *)
  List.iter
    (function
      | Env.TypeInformation.SF_sig (from, n, sch)
      | Env.TypeInformation.SF_let (from, n, _, sch, _, _) ->
          (begin
          (* Skip "rep", because it is a bit different and processed above *)
          (* c.f. function [generate_rep_constraint_in_record_type].       *)
          if (Parsetree_utils.name_of_vname n) <> "rep" then
            (begin
            let ty = Types.specialize sch in
            Format.fprintf out_fmter "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from ;
            (* Since we are printing a whole type scheme, it is stand-alone *)
            (* and we don't need to keep name sharing with anythin else.    *)
            Format.fprintf out_fmter "@[<2>%a : %a ;@]@\n"
              Parsetree_utils.pp_vname_with_operators_expanded n
              (Types.pp_type_simple_to_ml
                 ~current_unit: ctx.scc_current_unit
                 ~reuse_mapping: false collections_carrier_mapping) ty
            end)
          end)
      | Env.TypeInformation.SF_let_rec l ->
          List.iter
            (fun (from, n, _, sch, _, _) ->
              let ty = Types.specialize sch in
              Format.fprintf out_fmter "(* From species %a. *)@\n"
                Sourcify.pp_qualified_species from ;
              (* Since we are printing a whole type scheme, it is stand-alone *)
              (* and we don't need to keep name sharing with anythin else.    *)
              Format.fprintf out_fmter "%a : %a ;@\n"
                Parsetree_utils.pp_vname_with_operators_expanded n
                (Types.pp_type_simple_to_ml
                   ~current_unit: ctx.scc_current_unit
                   ~reuse_mapping: false collections_carrier_mapping) ty)
            l
      | Env.TypeInformation.SF_theorem  (_, _, _, _, _)
      | Env.TypeInformation.SF_property (_, _, _, _) ->
          (* Properties and theorems are purely  *)
          (* discarded in the Ocaml translation. *)
          ())
    species_descr.Env.TypeInformation.spe_sig_methods ;
  Format.fprintf out_fmter "@]}@\n"
;;



(* ************************************************************************** *)
(** {b Descr} : Lower-level species field (relevant for collection generator)
        description recording information about dependency and extra
        parameters already computed while generating the methods and that
        will be re-used while generating the collection generator.
        This avoids computing several the same things and ensure that the
        information is formated in the same way everywhere (in other words
        that the extra parameters discovered will appear in the same order
        between method declaration and method application).
    {b Rem} : Not exported outside this module.                               *)
(* ************************************************************************** *)
type compiled_field_memory = {
  (** Where the method comes from (the most recent in inheritance). *)
  cfm_from_species : Parsetree.qualified_species ;
  (** The method's name. *)
  cfm_method_name : Parsetree.vname ;
  (** The method's body. *)
  cfm_method_body  : Parsetree.expr ;
  (** The list mapping for each species parameter, the methods the current
      method depends on. By lambda-lifting, these methods induce extra
      parameters named as "_p_" +  species parameter name + "_" + called
      method's name we depend on. The first component of each couple is the
      parameter's name and the second is the set of methods the current
      method depends on from this species parameter.*)
  cfm_dependencies_from_parameters :
    (Parsetree.vname * Parsetree_utils.DepNameSet.t) list ;
  (** The methods of our inheritance tree the method depends on. *)
  cfm_decl_children :
    (Dep_analysis.name_node * Dep_analysis.dependency_kind) list ;
} ;;



(* ************************************************************************ *)
(** {b Descr} : Type of the fields significant for the collection generator
       creation, once the methods of the species have been generated. It
       remove all the fields that are not relevant for the collection
       generator.

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
type compiled_species_fields =
  | CSF_let of compiled_field_memory
  | CSF_let_rec of compiled_field_memory list
;;



(* ************************************************************************* *)
(** {b Descr} : Describes when starting the code of a let binding how it
       must be linked to the possible previous code. If the binding is the
       first of a non-recursive definition, then it must be introduced by
       "let ". If it is the first of a recursive definition, then it must be
       introduced by "let rec ". If it is not the first of a multiple
       definition, then it must be introduced by "and ".

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
type let_connector =
  | LC_first_non_rec   (** The binding is the first of a non-recursive
                           definition. *)
  | LC_first_rec   (** The binding is the first of a recursive definition. *)
  | LC_following   (** The binding is not the first of a multiple definition
                       (don't matter if the definition is recursive or not). *)
;;




(* llift_params : (string * Types.type_simple) list *)
(** {b Descr} : Really dumps the OCaml code for a species field.
    The [~let_connect] parameter tells whether we must start de function
    binding with "let" or "and".                                         *)
let generate_one_field_binding ctx env ~let_connect species_parameters_names
    params_llifted (from, name, params, scheme, body) =
  let out_fmter = ctx.scc_out_fmter in
  let collections_carrier_mapping = ctx.scc_collections_carrier_mapping in
  (* First of all, only methods defined in the current species must *)
  (* be generated. Inherited methods ARE NOT generated again !      *)
  if from = ctx.scc_current_species then
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
     (* Now, output the extra parameters induced by the lambda liftings *)
     (* we did because of the species parameters and our dependencies.  *)
     (* ATTENTION: in OCaml the dependencies on the carrier do not need *)
     (* to be lambda lifted : the OCaml's type system manages this via  *)
     (* our type variables mechanism. The abstraction of the carrier in *)
     (* the [params_llifted] is "abst_rep". Then we filter it.          *)
     List.iter
     (fun (param_name, _) ->
        (* In OCaml, we don't print the types to prevent being verbose. *)
        if param_name <> "abst_rep" then
          Format.fprintf out_fmter "@ %s" param_name)
      params_llifted ;
    (* Add the parameters of the let-binding with their type.   *)
    (* Ignore the result type of the "let" if it's a function   *)
    (* because we never print the type constraint on the result *)
    (* of the "let". We only print them in the arguments of the *)
    (* let-bound ident.                                         *)
    (* We also ignore the variables used to instanciate the     *)
    (* polymorphic ones of the scheme because in OCaml          *)
    (* polymorphism is not explicit.                            *)
    let (params_with_type, _, _) =
      Misc_ml_generation.bind_parameters_to_types_from_type_scheme
        scheme params in
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
                  ~current_unit: ctx.scc_current_unit
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
      Misc_ml_generation.rcc_current_unit = ctx.scc_current_unit ;
      Misc_ml_generation.rcc_species_parameters_names =
        species_parameters_names ;
      Misc_ml_generation.rcc_collections_carrier_mapping =
        collections_carrier_mapping ;
      Misc_ml_generation.rcc_lambda_lift_params_mapping =
        ctx.scc_lambda_lift_params_mapping ;
      Misc_ml_generation.rcc_out_fmter = out_fmter } in
    (* No local idents in the context because we just enter the scope *)
    (* of a species fields and so we are not under a core expression. *)
    Base_exprs_ml_generation.generate_expr expr_ctx ~local_idents: [] env body ;
    (* Done... Then, final carriage return. *)
    Format.fprintf out_fmter "@]@\n"
    end)
  else
    (begin
    (* Just a bit of debug/information if requested. *)
    if Configuration.get_verbose () then
      Format.eprintf
        "Field '%a' inherited but not (re)-declared is not generated again.@."
        Parsetree_utils.pp_vname_with_operators_expanded name
    end)
;;



(* *********************************************************************** *)
(* species_compil_context -> Parsetree.vname list ->                       *)
(*   Env.TypeInformation.species_field -> unit                             *)
(** {b Desc} : Generates the OCaml code for ONE method field (i.e. for one
             let-bound construct or for one bunch of items of a
             let-rec-bound construct.

    {b Args} :
      - [ctx] : The species-compilation-context merging the various
          stuffs sometimes needed during the compilation pass.
      - [species_parameters_names] : The list of the vnames of parameters
          the currently compiled species has.
      - [field] : The species's field to compile (i.e. a "let", "let rec",
                "sig", "theorem" or "property").

    {b Rem} : Not exported outside this module.                            *)
(* *********************************************************************** *)
let generate_methods ctx env species_parameters_names field =
  match field with
   | Env.TypeInformation.SF_sig (_, name, _) ->
       (* Only declared, hence, no code to generate yet ! *)
       if Configuration.get_verbose () then
         Format.eprintf "OCaml code for signature '%a' leads to void code.@."
           Parsetree_utils.pp_vname_with_operators_expanded name ;
       (* Nothing to keep for the collection generator. *)
       None
   | Env.TypeInformation.SF_let (from, name, params, scheme, body, _) ->
       let (dependencies_from_params, decl_children, llift_params) =
         Misc_ml_generation.compute_lambda_liftings_for_field
           ~current_species: ctx.scc_current_species species_parameters_names
           ctx.scc_dependency_graph_nodes name body in
       (* No recursivity, then the method cannot call itself in its body *)
       (* then no need to set the [scc_lambda_lift_params_mapping] of    *)
       (* the context.                                                   *)
       generate_one_field_binding
         ctx env ~let_connect: LC_first_non_rec
         species_parameters_names llift_params
         (from, name, params, (Some scheme), body) ;
       (* Now, build the [compiled_field_memory], even if the method  *)
       (* was not really generated because it was inherited.          *)
       let compiled_field = {
         cfm_from_species = from ;
         cfm_method_name = name ;
         cfm_method_body = body ;
         cfm_dependencies_from_parameters = dependencies_from_params ;
         cfm_decl_children = decl_children } in
       Some (CSF_let compiled_field)
   | Env.TypeInformation.SF_let_rec l ->
       (begin
       match l with
        | [] ->
            (* A "let", then a fortiori "let rec" construct *)
            (* must at least bind one identifier !          *)
            assert false
        | (from, name, params, scheme, body, _) :: q ->
            (* First, get for each method, its lambda lifting infos, *)
            (* i.e. its extra parameters and dependency information. *)
            let lliftings_infos =
              List.map
                (fun (_, n, _, _, b, _) ->
                  Misc_ml_generation.compute_lambda_liftings_for_field
                    ~current_species: ctx.scc_current_species
                    species_parameters_names ctx.scc_dependency_graph_nodes n b)
                l in
            (* Extend the context with the mapping between these *)
            (* recursive functions and their extra arguments.    *)
            let ctx' = {
              ctx with
                scc_lambda_lift_params_mapping =
                  List.map2
                    (fun (_, n, _, _, _, _) (_, _, extra_params) ->
                      (n, extra_params))
                     l
                     lliftings_infos } in
            let ((dependencies_from_params_head,
                  decl_children_head, llift_params_head) ,
                 lliftings_infos_tail) =
              match lliftings_infos with
               | (a, b, c) :: q -> ((a, b, c), q)
               | _ -> assert false in
            (* Now, generate the first method, introduced by "let rec". *)
            generate_one_field_binding
              ctx' env ~let_connect: LC_first_rec
              species_parameters_names llift_params_head
              (from, name, params, (Some scheme), body) ;
            let first_compiled = {
              cfm_from_species = from ;
              cfm_method_name = name ;
              cfm_method_body = body ;
              cfm_dependencies_from_parameters = dependencies_from_params_head ;
              cfm_decl_children = decl_children_head } in
            (* Finally, generate the remaining  methods, introduced by "and". *)
            let rem_compiled =
              List.map2
                (fun (from, name, params, scheme, body, _)
                    (dependencies_from_params, decl_children, llift_params) ->
                  generate_one_field_binding
                    ctx' env ~let_connect: LC_following
                    species_parameters_names llift_params
                    (from, name, params, (Some scheme), body) ;
                  { cfm_from_species = from ;
                    cfm_method_name = name ;
                    cfm_method_body = body ;
                    cfm_dependencies_from_parameters =
                      dependencies_from_params ;
                    cfm_decl_children = decl_children })
                q
                lliftings_infos_tail in
            Some (CSF_let_rec (first_compiled :: rem_compiled))
       end)
   | Env.TypeInformation.SF_theorem (_, name, _, _, _)
   | Env.TypeInformation.SF_property (_, name, _, _) ->
       (* Properties and theorems are purely  *)
       (* discarded in the Ocaml translation. *)
       if Configuration.get_verbose () then
         Format.eprintf
           "OCaml code for theorem/property '%a' leads to void code.@."
           Parsetree_utils.pp_vname_with_operators_expanded name ;
       (* Nothing to keep for the collection generator. *)
       None
;;



(* *********************************************************************** *)
(* Format.formatter -> compiled_species_fields option list ->              *)
(*  (Parsetree.vname * Parsetree_utils.DepNameSet.t) list                    *)
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
let dump_collection_generator_arguments out_fmter compiled_species_fields =
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
      (fun (spe_param_name, meths_set) ->
        (* Get or create for this species parameter name, the bucket *)
        (* recording all the methods someone depends on.             *)
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
      field_memory.cfm_dependencies_from_parameters in

  (* ********************************************************** *)
  (* Now, really work, building by side effect for each species *)
  (* parameter the set of methods we depend on.                 *)
  List.iter
    (function
      | None -> ()
      | Some (CSF_let field_memory) -> process_one_field_memory field_memory
      | Some (CSF_let_rec l) -> List.iter process_one_field_memory l)
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



let generate_collection_generator ctx compiled_species_fields =
  let current_species_name = snd ctx.scc_current_species in
  let out_fmter = ctx.scc_out_fmter in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf
      "@\nSpecies %a is fully defined. Generating its collection generator@."
      Sourcify.pp_vname current_species_name ;
 
  (* ******************************************************************* *)
  (** {b Descr} : A local function to process one field. This allows to
                factorize the processing for both [Let] and [Let_rec]
                definitions.

      {b Rem} : Local to the [generate_collection_generator] function.
               Not exported.                                             *)
  (* ******************************************************************* *)
  let process_one_field field_memory =
    let from = field_memory.cfm_from_species in
    Format.fprintf out_fmter "(* From species %a. *)@\n"
      Sourcify.pp_qualified_species from ;
    Format.fprintf out_fmter "@[<2>let local_%a =@ "
      Parsetree_utils.pp_vname_with_operators_expanded
      field_memory.cfm_method_name ;
    (* Find the method generator to use depending on if it belongs to this *)
    (* inheritance level or if it was inherited from another species.      *)
    if from = ctx.scc_current_species then
      (begin
      (* It comes from the current inheritance level.   *)
      (* Then its name is simply the the method's name. *)
      Format.fprintf out_fmter "%a"
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.cfm_method_name
      end)
    else
      (begin
      (* It comes from a previous inheritance level. Then its name is *)
      (* the the module where the species inhabits if not the same    *)
      (* compilation unit than the current + "." + species name as    *)
      (* module + "." + the method's name.                            *)
      if (fst from) <> ctx.scc_current_unit then
        Format.fprintf out_fmter "%s.@," (String.capitalize (fst from)) ;
      Format.fprintf out_fmter "%a.@,%a"
        Parsetree_utils.pp_vname_with_operators_expanded (snd from)
        Parsetree_utils.pp_vname_with_operators_expanded
        field_memory.cfm_method_name
      end) ;
    (* Now, apply the method generator to each of the extra arguments *)
    (* induced by the various lambda-lifting we previously performed. *)
    (* First, the extra arguments due to the species parameters methods we  *)
    (* depends on. Here we will not use them to lambda-lift them this time, *)
    (* but to apply them ! The name used for application is formed          *)
    (* according to the same scheme we used at lambda-lifting time:         *)
    (* "_p_" + species parameter name + "_" + called method name.           *)
    List.iter
      (fun (species_param_name, meths_from_param) ->
        let prefix =
          "_p_" ^ (Parsetree_utils.name_of_vname species_param_name) ^
          "_" in
        Parsetree_utils.DepNameSet.iter
          (fun (meth, _) ->
            (* Don't print the type to prevent being too verbose. *)
            Format.fprintf out_fmter "@ %s%a"
              prefix Parsetree_utils.pp_vname_with_operators_expanded meth)
          meths_from_param)
      field_memory.cfm_dependencies_from_parameters ;
    (* Second, the methods of our inheritance tree we depend on and that are  *)
    (* only declared. These methods leaded to "local" functions defined       *)
    (* above. Hence, for each  method only declared of ourselves we depend    *)
    (* on, its name is "local_" + the method's name.                          *)
    (* ATTENTION: in OCaml the dependencies on the carrier do not need to be  *)
    (* lambda lifted : the OCaml's type system manages this via our type      *)
    (* variables mechanism. The carrier's name being "rep" we just filter it. *)
    (* In effect while we built the method generators, we did not lambda-lift *)
    (* the carrier. So it is normal here not to apply any extra argument !    *)
    List.iter
      (fun ({ Dep_analysis.nn_name = dep_name }, _) ->
        if dep_name <> (Parsetree.Vlident "rep") then
          Format.fprintf out_fmter "@ local_%a"
            Parsetree_utils.pp_vname_with_operators_expanded dep_name)
      field_memory.cfm_decl_children ;
    (* That's it for this field code generation. *)
    Format.fprintf out_fmter "@ in@]@\n" in    

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
  (* to the actual method stored in the collection record). By the way,    *)
  (* recover the list of species parameters linked together with their     *)
  (* methods we need to instanciate in order to apply the collection       *)
  (* generator.                                                            *)
  let extra_args_from_spe_params =
    dump_collection_generator_arguments out_fmter compiled_species_fields in
  Format.fprintf out_fmter " =@ " ;
  (* Generate the local functions that will be used to fill the record value. *)
  List.iter
    (function
      | None -> ()
      | Some (CSF_let field_memory) -> process_one_field field_memory
      | Some (CSF_let_rec l) -> List.iter (fun fm -> process_one_field fm) l)
    compiled_species_fields ;
  (* And now, the record value. We just assign each record fields    *)
  (* corresponding to the current species's method the corresponding *)
  (* local function we defined just above. Remind that the record    *)
  (* field's is simply the method's name.                            *)
  (* The local function corresponding to the method is "local_" +    *)
  (* the method's name.                                              *)
  Format.fprintf ctx.scc_out_fmter "@[<2>{ " ;
  List.iter
      (function
      | None -> ()
      | Some (CSF_let field_memory) ->
          Format.fprintf ctx.scc_out_fmter "%a =@ local_%a ;@\n"
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.cfm_method_name
            Parsetree_utils.pp_vname_with_operators_expanded
            field_memory.cfm_method_name
      | Some (CSF_let_rec l) ->
          List.iter
            (fun field_memory ->
              Format.fprintf ctx.scc_out_fmter "%a =@ local_%a ;@\n"
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.cfm_method_name
                Parsetree_utils.pp_vname_with_operators_expanded
                field_memory.cfm_method_name)
            l)
    compiled_species_fields ;
  (* Close the record expression. *)
  Format.fprintf ctx.scc_out_fmter "@ }@]@\n" ;
  (* Close the pretty-print box of the "let collection_create ... =". *)
  Format.fprintf ctx.scc_out_fmter "@]@\n" ;
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
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, species_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_lambda_lift_params_mapping = [] ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the species' type. *)
  generate_record_type ctx species_descr ;
  (* Compute the list of names of parameters of the species. This   *)
  (* will be use to compute for each method the set of methods from *)
  (* the parameters the method depends on.                          *)
  let species_parameters_names =
    List.map
      (function
        | Env.TypeInformation.SPAR_in (n, _) -> n
        | Env.TypeInformation.SPAR_is ((_, n), _, _) -> Parsetree. Vuident n)
      species_descr.Env.TypeInformation.spe_sig_params in
  (* Now, the methods of the species. *)
  let compiled_fields =
    List.map
      (generate_methods ctx env species_parameters_names)
      species_descr.Env.TypeInformation.spe_sig_methods in
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
            generate_collection_generator ctx compiled_fields }
    else None in
  Format.fprintf out_fmter "end ;;@]@\n@." ;
  (* Return what is needed to enter this species *)
  (* in the  ml generation environnment. *)
  extra_args_from_spe_params
;;



type collection_effective_arguments =
  | CEA_coll_name_for_is of Parsetree.qualified_vname
  | CEA_value_expr_for_in of Parsetree.expr
;;



(* ************************************************************************* *)
(* species_compil_context -> collection_effective_arguments list ->          *)
(*   Env.MlGenInformation.collection_generator_info -> unit                  *)
(** {b Descr} : This function recovers from the fully defined species the
    collection implements, the names of its parameters names. Then it gets
    the parameters the collection generator needs and actually applies it to
    correct corresponding functions.
    This "correct corresponding" functions are those from the collection that
    instanciate the species parameter, but coming from the collection that
    instanciates this species parameter in the freshly defined collection.

    For example, let's take "collection A implements B (C, D)", with B being
    parameterized by the formal X and Y. Imagine we determined that B's
    collection generator needs to be feeded an argument representing a method
    "foo" of X and "bar" of Y, in the order "bar" before and "foo" next.
    To fully apply B's collection generator, we need to pass the abstractions
    (the functions) corresponding to [bar] and [foo] IN THIS ORDER.

    In the [param_info] parameter, we know the expected function names and
    from where they come in term of formal parameter in B (i.e. we need that
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
let apply_generator_to_parameters ctx env coll_body_params col_gen_params_info =
  let current_unit = ctx.scc_current_unit in
  let out_fmter = ctx.scc_out_fmter in
  (* Create the assoc list mapping the formal to the effectives parameters. *)
  let formal_to_effective_map =
    (try
      List.map2
        (fun formal_info effective_info ->
          match (formal_info, effective_info) with
           | ((formal, Env.ScopeInformation.SPK_is),
              CEA_coll_name_for_is qualified_vname) ->
               (begin
               (* "In" parameter. Leads to collection name based stuff. *)
               match qualified_vname with
                | Parsetree.Vname _ ->
                    (* Assumed to be local to the current unit. *)
                    (formal, CEA_coll_name_for_is qualified_vname)
                | Parsetree.Qualified (effective_fname, effective_vname) ->
                    (* If the species belongs to the current unit, then we   *)
                    (* don't need to qualify it in the OCaml generated code. *)
                    (* Then we simply discard its explicit hosting           *)
                    (* information.                                          *)
                    if effective_fname = current_unit then
                      (formal,
                       CEA_coll_name_for_is (Parsetree.Vname effective_vname))
                    else
                      (formal,
                       CEA_coll_name_for_is
                         (Parsetree.Qualified
                            (effective_fname, effective_vname)))
               end)
           | ((formal, Env.ScopeInformation.SPK_in),
              (CEA_value_expr_for_in effective_expr)) ->
               (begin
               (* "Is" parameter. Leads to direct value based stuff. *)
               (formal, (CEA_value_expr_for_in effective_expr))
               end)
           | (_, _) ->
               (* This would mean that we try to apply an effective stuff    *)
               (* in:is-incompatible with the kind of the species parameter. *)
               (* This should have been caught before by the analyses !      *)
               assert false)
        col_gen_params_info.Env.MlGenInformation.
          cgi_implemented_species_params_names
        coll_body_params
    with Invalid_argument "List.map2" ->
      assert false  (* The lists length must be equal. *)) in
  (* Now, generate the argment identifier or expression *)
  (* for each expected collection generator parameter.  *)
  List.iter
    (fun (formal_species_param_name, method_names) ->
      match List.assoc formal_species_param_name formal_to_effective_map with
       | CEA_coll_name_for_is corresponding_effective ->
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
               (match corresponding_effective_opt_fname with
                | Some fname ->
                    Format.fprintf out_fmter "%s." (String.capitalize fname)
                | None -> ()) ;
               (* Species name."effective_collection.". *)
               Format.fprintf out_fmter "@ %a.effective_collection."
                 Parsetree_utils.pp_vname_with_operators_expanded
                 corresponding_effective_vname ;
               (* If needed, qualify the name of the species *)
               (* in the OCaml code. *)
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
       | CEA_value_expr_for_in expr ->
           (begin
           Format.fprintf out_fmter "(@[<1>" ;
           let expr_ctx = {
             Misc_ml_generation.rcc_current_unit = current_unit ;
             (* Since we are in the context of a collection and since a *)
             (* collection does not have parameters, the                *)
             (* [rcc_species_parameters_names] is trivially empty.      *)
             Misc_ml_generation.rcc_species_parameters_names = [] ;
             Misc_ml_generation.rcc_collections_carrier_mapping =
               ctx.scc_collections_carrier_mapping ;
             Misc_ml_generation.rcc_lambda_lift_params_mapping =
               ctx.scc_lambda_lift_params_mapping ;
             Misc_ml_generation.rcc_out_fmter = out_fmter } in
           (* No local idents in the context because we just enter the scope *)
           (* of a species fields and so we are not under a core expression. *)
           Base_exprs_ml_generation.generate_expr
             expr_ctx ~local_idents: [] env expr ;
           Format.fprintf out_fmter ")@]" ;
           end))
    col_gen_params_info.Env.MlGenInformation.cgi_generator_parameters
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

    {b Rem} : Not exported outside this module.                              *)
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
                CEA_coll_name_for_is effective_species_name
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



(* ************************************************************************** *)
(* current_unit: Types.fname -> Format.formatter -> Parsetree.ident -> unit *)
(** {b Descr} : Helper that prints a species name as an OCaml module,
       with module qualification if needed.
       In other words, each time we need to refer to a module qualification
       induced by a species, this function prints the the capitalize name
       of the species, prefixed by its hosting file considered as an OCaml
       module if this species is not in the current compilation unit.
       For example, imagine we are in the "foo.foc" file and we need to
       speak of a record field of a species "S" that lives in the "bar.foc"
       file. Then because each FoCaL compilation unit is mapped onto an
       OCaml file (hence an OCaml module corresponding to the file-as-module),
       it will be printed like "Bar.S". If the species "S" was in the same
       compilation unit (i.e. "foo.foc"), then it would be printed directly
       "S".
       
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
;;



(* ********************************************************************* *)
(* current_unit: Types.fname -> Format.formatter -> Env.MlGenEnv.t ->    *)
(*   Parsetree.coll_def -> Env.TypeInformation.species_description ->    *)
(*     Dep_analysis.name_node list -> unit                               *)
(** {b Descr} : Generate the OCaml code for a collection implementation.
      The compilation model dumps:
       - The record type representing the species actual representation,
       - A call the the "implemented" species's collection generator,
       - And a final value (of type above) representing the actual
         species and borrowing every fields from the value obtained
         via the collection generator application in order to make
         the collection having its own record fields names.

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
let collection_compile env ~current_unit out_fmter coll_def coll_descr
    dep_graph =
  let coll_name = coll_def.Parsetree.ast_desc.Parsetree.cd_name in
  (* Just a bit of debug. *)
  if Configuration.get_verbose () then
    Format.eprintf "Generating OCaml code for collection %a@."
      Sourcify.pp_vname coll_name ;
  (* Start the module encapsulating the collection representation. *)
  Format.fprintf out_fmter "@[<2>module %a =@\nstruct@\n"
    Sourcify.pp_vname coll_name ;
  (* Now, establish the mapping between collections available *)
  (* and the type variable names representing their carrier.  *)
  let collections_carrier_mapping =
    build_collections_carrier_mapping ~current_unit coll_descr in
  (* Create the initial compilation context for this collection. *)
  let ctx = {
    scc_current_unit = current_unit ;
    scc_current_species = (current_unit, coll_name) ;
    scc_dependency_graph_nodes = dep_graph ;
    scc_collections_carrier_mapping = collections_carrier_mapping ;
    scc_lambda_lift_params_mapping = [] ;
    scc_out_fmter = out_fmter } in
  (* The record type representing the collection's type. *)
  generate_record_type ctx coll_descr ;
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
    coll_def.Parsetree.ast_desc.Parsetree.
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
    let opt_params_info =
      Env.MlGenEnv.find_species
        ~loc: coll_def.Parsetree.ast_loc ~current_unit
        implemented_species_name env in
    (match opt_params_info with
     | None ->
         (* The species has no collection generator. Hence it is not a   *)
         (* fully defined species. This should have be prevented before, *)
         (* by forbidding to make a collection from a non fully defined  *)
         (* species !                                                    *)
         assert false   (* [Unsure] car je crois qu'on n'a pas fait la v�rif. *)
     | Some params_info ->
         (* Get the names of the collections or the value *)
         (* expressions effectively applied.              *)
         let coll_body_params =
           get_implements_effectives
             coll_def.Parsetree.ast_desc.Parsetree.cd_body.Parsetree.ast_desc.
               Parsetree.se_params
             params_info.Env.MlGenInformation.
               cgi_implemented_species_params_names in
         apply_generator_to_parameters ctx env coll_body_params params_info) ;
    Format.fprintf out_fmter "@ in@]@\n" ;
    (* And now, create the final value representing the effective instance *)
    (* of our collection, borrowing each field from the temporary value    *)
    (* obtained above. This way, our collection will have ITS own record   *)
    (* fields names, preventing the need to use those coming fom the       *)
    (* it implements.                                                      *)
    Format.fprintf out_fmter "@[<2>{@ " ;
    (* Make the record value borrowing every fields from the temporary *)
    (* value generated by the collection generator.                    *)
    List.iter
      (function
        | Env.TypeInformation.SF_sig (_, _, _)
        | Env.TypeInformation.SF_theorem (_, _, _, _, _)
        | Env.TypeInformation.SF_property (_, _, _, _) -> ()
        | Env.TypeInformation.SF_let (_, n, _, _, _, _) ->
            Format.fprintf out_fmter "%a =@ t."
              Parsetree_utils.pp_vname_with_operators_expanded n ;
            print_implemented_species_as_ocaml_module
              ~current_unit out_fmter implemented_species_name ;
            Format.fprintf out_fmter ".%a ;@\n"
              Parsetree_utils.pp_vname_with_operators_expanded n
        | Env.TypeInformation.SF_let_rec l ->
            List.iter
              (fun (_, n, _, _, _, _) ->
                Format.fprintf out_fmter "%a =@ t."
                  Parsetree_utils.pp_vname_with_operators_expanded n ;
                print_implemented_species_as_ocaml_module
                  ~current_unit out_fmter implemented_species_name ;
                Format.fprintf out_fmter ".%a ;@\n"
                  Parsetree_utils.pp_vname_with_operators_expanded n)
              l)
      coll_descr.Env.TypeInformation.spe_sig_methods ;
    (* End the definition of the value representing the effective instance. *)
    Format.fprintf out_fmter "@ }@]@]@\n" ;
    (* End the module representing the collection. *)
    Format.fprintf out_fmter "end ;;@]@\n@."
  with Not_found ->
    (* Don't see why the species could not be present in the environment.  *)
    (* The only case would be to make a collection from a collection since *)
    (* collection are never entered in the environment because it's a non  *)
    (* sense to make a collection "implementing" a collection !            *)
    (* [Unsure]. Peut �tre lever un message d'erreur. *)
    assert false
  end)
;;

