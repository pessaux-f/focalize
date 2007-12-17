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

(* $Id: species_coq_generation.ml,v 1.7 2007-12-17 16:49:33 pessaux Exp $ *)


(* *************************************************************** *)
(** {b Descr} : This module performs the compilation from FoCaL to
              Coq of FoCaL's collections and species.            *)
(* *************************************************************** *)




let generate_methods ctx print_ctx _env field =
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
         (* Only declared method. Hence appears as a "Variable". *)
         Format.fprintf out_fmter
           "@[<2>Variable self_%a :@ %a.@]@\n"
           Parsetree_utils.pp_vname_with_operators_expanded name
           (Types.pp_type_simple_to_coq
              print_ctx ~reuse_mapping: false ~self_is_abstract: true) ty
           (* [Unsure] Retourner les infos de compil. *)
         end)
   | Env.TypeInformation.SF_let (_from, _name, _params, _scheme, _body) -> ()
   | Env.TypeInformation.SF_let_rec _l -> ()
   | Env.TypeInformation.SF_theorem (_from, _name, _, _, _) -> ()
   | Env.TypeInformation.SF_property (_from, _name, _, _) -> ()
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
(* current_unit: Types.fname -> Env.TypeInformation.species_description ->  *)
(*   (Types.type_collection * string) list                                  *)
(** {b Descr} : Create the correspondance between the collection type of
              the species definition parameters and the names to be used
              later during the Coq translation.
              For a species parameter [A is/in ... ], the name that will be
              used is directly the name of the species parameter without
              any change. No need like in OCaml to add a stamp because we
              don't lowercase names. Hence parameters will never wear the
              same name.
              This avoids the need to remind the stamp of a "is" parameter
              that is used to make a "in" parameter. In effect, for the
              "species Me (Naturals is IntModel, n in Naturals)" code,
              "Naturals" would be mapped on "Naturals0" and then
              everywhere "Natural" was used in the FoCaL code, one should
              replace by "Naturals0" in the Coq code !

    {b Rem} : Not exported outside this module.                             *)
(* ************************************************************************ *)
let build_collections_carrier_mapping ~current_unit species_descr =
  List.map
    (function
      | Env.TypeInformation.SPAR_is ((_, carrier_name), _, param_expr) ->
          (* Now, build the "collection type" this name will be bound to. *)
          (* According to how the "collection type" of parameters are     *)
          (* built, this will be the couple of the current compilation    *)
          (* unit and the name of the parameter.                          *)
          let type_coll = (current_unit, carrier_name) in
          (* And now create the binding... Record that the parameter is a *)
          (* "is" parameter whose species expr is [param_expr] that will  *)
          (* be used to create the Coq type expression annotating this    *)
          (* parameter in the hosting species record type.                *)
          (type_coll, (carrier_name, (Species_gen_basics.CCMI_is param_expr)))
      | Env.TypeInformation.SPAR_in (n, type_coll) ->
          (* Build the name that will represent this parameter's *)
          (* carrier seen from Coq.                              *)
          let carrier_name = Parsetree_utils.name_of_vname n in
          (* Record that the parameter is a "in" parameter. Then we don't    *)
          (* need any species expression to annotate this parameter in the   *)
          (* Coq type expression annotating this parameter in the hosting    *)
          (* species record type: it will simply be of the type [type_coll]. *)
          (type_coll, (carrier_name, Species_gen_basics.CCMI_in_or_not_param)))
    species_descr.Env.TypeInformation.spe_sig_params
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
  (* The record type representing the species' type. *)
  Species_record_type_generation.generate_record_type ctx env species_descr ;
  (* We now extend the collections_carrier_mapping with ourselve known.  *)
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
  (* Now, the methods of the species. We deal with "rep" first *)
  (* and then it will be ignore while generating the methods.  *)
  Format.fprintf out_fmter "@\n(* Carrier representation. *)@\n" ;
  Format.fprintf out_fmter "Variable self_T : Set.@\n@\n";
  let _compiled_fields =
    List.map
      (generate_methods ctx' print_ctx env)
      species_descr.Env.TypeInformation.spe_sig_methods in
  (* The end of the chapter hosting the species. *)
  Format.fprintf out_fmter "@]End %s.@\n@." chapter_name
;;
