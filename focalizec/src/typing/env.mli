(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
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

exception Unbound_constructor of (Parsetree.vname * Location.t)
exception Unbound_label of (Parsetree.vname * Location.t)
exception Unbound_identifier of (Parsetree.vname * Location.t)
exception Unbound_type of (Parsetree.vname * Location.t)
exception Unbound_module of (Types.fname * Location.t)
exception Unbound_species of (Parsetree.vname * Location.t)
exception Unbound_collection of (Parsetree.vname * Location.t)

exception Rebound_type of (Parsetree.vname * Location.t)
exception Rebound_species of (Parsetree.vname * Location.t)
exception Rebound_toplevel_let of (Parsetree.vname * Location.t)

type rec_proof_kind =
  | RPK_struct of Parsetree.vname
  | RPK_other

type rec_status =
  | RC_non_rec
  | RC_rec of rec_proof_kind

type substitution_kind =
  | SK_collection_by_collection of
      (Types.type_collection *
       Types.substitution_by_replacement_collection_kind)
  | SK_ident_by_expression of
      (Types.fname * Parsetree.vname * Parsetree.expr_desc)

val debug_substitution : substitution_kind list -> unit

type from_history = {
  fh_initial_apparition : Parsetree.qualified_species ;
  fh_inherited_along :
    (Parsetree.qualified_species * Parsetree_utils.simple_species_expr *
    (substitution_kind list))
      list
}

val intitial_inheritance_history : Parsetree.qualified_species -> from_history

module ScopeInformation :
  sig
    type value_binding_info =
      | SBI_file of Types.fname
      | SBI_method_of_self
      | SBI_method_of_coll of Parsetree.qualified_vname
      | SBI_local

    type type_binding_info =
      | TBI_builtin_or_var
      | TBI_defined_in of Types.fname

    type species_scope =
      | SPBI_file of (Types.fname * bool)
      | SPBI_parameter

    type species_parameter_kind = SPK_in | SPK_is

    type species_binding_info = {
      spbi_methods : Parsetree.vname list ;
      spbi_params_kind : species_parameter_kind list ;
      spbi_inherits : Parsetree.species_expr list ;
      spbi_scope : species_scope
    }
  end

type ordered_methods_from_params =
  | ODFP_methods_list of
      (Parsetree.vname * Parsetree_utils.dependency_elem_type_kind) list

module TypeInformation :
  sig
    type dependency_on_rep = {
      dor_def : bool ;
      dor_decl : bool }

    type let_definition_flags = {
      ldf_recursive : Parsetree.rec_flag ;
      ldf_logical : Parsetree.logical_flag ;
      ldf_final : Parsetree.final_flag }

    type sig_field_info =
      (from_history * Parsetree.vname * Types.type_scheme)

    type let_field_info =
      (from_history * Parsetree.vname * (Parsetree.vname list) *
       Types.type_scheme * Parsetree.binding_body *
       (Parsetree.termination_proof option) * dependency_on_rep *
       let_definition_flags)

    type theorem_field_info =
      (from_history * Parsetree.vname *
       ((Parsetree.vname * Types.type_simple) list) *
       Parsetree.logical_expr * Parsetree.proof * dependency_on_rep)

    type property_field_info =
      (from_history * Parsetree.vname *
       ((Parsetree.vname * Types.type_simple) list) *
       Parsetree.logical_expr * dependency_on_rep)

    type species_param =
      | SPAR_in of
          (Parsetree.vname * Types.type_collection *
           Types.species_collection_kind)
      | SPAR_is of
          (Types.type_collection * Types.species_collection_kind *
           (species_field list) * Parsetree_utils.simple_species_expr *
           (DepGraphData.name_node list))

    and species_field =
      | SF_sig of sig_field_info
      | SF_let of let_field_info
      | SF_let_rec of let_field_info list
      | SF_theorem of theorem_field_info
      | SF_property of property_field_info

    type min_coq_env_method =
        MCEM_Declared_carrier
      | MCEM_Defined_carrier of Types.type_scheme
      | MCEM_Declared_computational of (Parsetree.vname * Types.type_scheme)
      | MCEM_Defined_computational of
          (from_history * rec_status * Parsetree.vname *
           (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
      | MCEM_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
      | MCEM_Defined_logical of
          (from_history * Parsetree.vname * Parsetree.logical_expr)

    type min_coq_env_reason =
      | MCER_only_logical
      | MCER_even_comput

    type min_coq_env_element = (min_coq_env_reason * min_coq_env_method)

    type min_dk_env_method =
        MDEM_Declared_carrier
      | MDEM_Defined_carrier of Types.type_scheme
      | MDEM_Declared_computational of (Parsetree.vname * Types.type_scheme)
      | MDEM_Defined_computational of
          (from_history * rec_status * Parsetree.vname *
           (Parsetree.vname list) * Types.type_scheme * Parsetree.binding_body)
      | MDEM_Declared_logical of (Parsetree.vname * Parsetree.logical_expr)
      | MDEM_Defined_logical of
          (from_history * Parsetree.vname * Parsetree.logical_expr)

    type min_dk_env_reason =
      | MDER_only_logical
      | MDER_even_comput

    type min_dk_env_element = (min_dk_env_reason * min_dk_env_method)

    type field_abstraction_info = {
      ad_used_species_parameter_tys : Parsetree.vname list ;
      ad_raw_dependencies_from_params :
      (species_param * ordered_methods_from_params) list ;
      ad_dependencies_from_parameters :
        (species_param * ordered_methods_from_params) list ;
      ad_dependencies_from_parameters_in_type :
        (species_param * ordered_methods_from_params) list ;
(*      ad_abstracted_methods : Parsetree.vname list ; *)
      ad_min_coq_env : min_coq_env_element list ;
      ad_min_dk_env : min_dk_env_element list
    }

    type species_description = {
      spe_kind : Types.species_collection_kind ;
      spe_is_closed : bool ;
      spe_sig_params : species_param list ;
      spe_sig_methods : species_field list ;
      spe_dep_graph : DepGraphData.name_node list ;
      spe_meths_abstractions : (Parsetree.vname * field_abstraction_info) list
    }
    type constructor_arity = CA_zero | CA_some
    type constructor_description = {
      cstr_arity : constructor_arity ;
      cstr_scheme : Types.type_scheme ;
    }
    type field_mutability = FM_mutable | FM_immutable
    type label_description = {
      field_mut : field_mutability ;
      field_scheme : Types.type_scheme
    }
    type type_kind =
      | TK_abstract
      | TK_external of
          (Parsetree.external_translation * Parsetree.external_mapping)
      | TK_variant of (Parsetree.constructor_name * constructor_arity *
                       Types.type_scheme) list
      | TK_record of
          (Parsetree.vname * field_mutability * Types.type_scheme) list

    type type_description = {
      type_loc : Location.t ;
      type_kind : type_kind ;
      type_identity : Types.type_scheme ;
      type_params : Types.type_simple list ;
      type_arity : int
    }

  val pp_species_description : Format.formatter -> species_description -> unit
  val vname_of_species_param : species_param -> Parsetree.vname

  end

type collection_or_species =
  | COS_collection
  | COS_species

type method_type_kind =
  | MTK_computational of Types.type_scheme
  | MTK_logical of Parsetree.logical_expr

type generic_code_gen_method_info = {
  mi_name : Parsetree.vname ;
  mi_history : from_history ;
  mi_type_kind : method_type_kind ;
  mi_used_species_parameter_tys : Parsetree.vname list ;
  mi_dependencies_from_parameters :
    (TypeInformation.species_param * ordered_methods_from_params) list ;
  mi_dependencies_from_parameters_in_type :
    (TypeInformation.species_param * ordered_methods_from_params) list ;
  mi_abstracted_methods : Parsetree.vname list
  }

module MlGenInformation :
  sig
    type collection_generator_info = {
      cgi_implemented_species_params_names :
        (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
      cgi_generator_parameters :
        (Parsetree.vname * ordered_methods_from_params) list
    }

    type method_info = generic_code_gen_method_info

    type species_binding_info =
      ((TypeInformation.species_param list) *
       (method_info list) *
       (collection_generator_info option) * collection_or_species)

    type label_mapping_info = Parsetree.external_translation_desc
    type constructor_mapping_info = Parsetree.external_translation_desc
  end


module CoqGenInformation :
  sig
  type collection_generator_parameters = {
    cgp_abstr_param_carriers_for_record : Parsetree.vname list ;
    cgp_abstr_param_methods_for_record :
      (Parsetree.vname * ordered_methods_from_params) list ;
    cgp_abstr_param_methods_for_coll_gen :
      (Parsetree.vname * ordered_methods_from_params) list
    }

    type collection_generator_info = {
      cgi_implemented_species_params_names :
        (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
      cgi_generator_parameters : collection_generator_parameters
    }

    type constructor_mapping_info = {
      cmi_num_polymorphics_extra_args : int ;
      cmi_external_translation : Parsetree.external_translation_desc option
    }

    type label_mapping_info = {
      lmi_num_polymorphics_extra_args : int ;
      lmi_external_translation : Parsetree.external_translation_desc option
    }

    type method_info = generic_code_gen_method_info

    type species_binding_info =
      ((TypeInformation.species_param list) *
       (method_info list) *
       (collection_generator_info option) * collection_or_species)

    type value_body =
      | VB_non_toplevel
      | VB_toplevel_let_bound of
          (rec_status * (Parsetree.vname list) * Types.type_scheme *
           Parsetree.binding_body)
      | VB_toplevel_property of Parsetree.logical_expr

    type value_mapping_info = (int * value_body)

    type type_info = TypeInformation.type_description
  end


module DkGenInformation :
  sig
  type collection_generator_parameters = {
    cgp_abstr_param_carriers_for_record : Parsetree.vname list ;
    cgp_abstr_param_methods_for_record :
      (Parsetree.vname * ordered_methods_from_params) list ;
    cgp_abstr_param_methods_for_coll_gen :
      (Parsetree.vname * ordered_methods_from_params) list
    }

    type collection_generator_info = {
      cgi_implemented_species_params_names :
        (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
      cgi_generator_parameters : collection_generator_parameters
    }

    type constructor_mapping_info = {
      cmi_num_polymorphics_extra_args : int ;
      cmi_external_translation : Parsetree.external_translation_desc option
    }

    type label_mapping_info = {
      lmi_num_polymorphics_extra_args : int ;
      lmi_external_translation : Parsetree.external_translation_desc option
    }

    type method_info = generic_code_gen_method_info

    type species_binding_info =
      ((TypeInformation.species_param list) *
       (method_info list) *
       (collection_generator_info option) * collection_or_species)

    type rec_proof_kind =
      | RPK_struct of Parsetree.vname
      | RPK_other

    type rec_status =
      | RC_non_rec
      | RC_rec

    type value_body =
      | VB_non_toplevel
      | VB_toplevel_let_bound of
          (rec_status * (Parsetree.vname list) * Types.type_scheme *
           Parsetree.binding_body)
      | VB_toplevel_property of Parsetree.logical_expr

    type value_mapping_info = (int * value_body)

    type type_info = TypeInformation.type_description
  end


module ScopingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value :
      toplevel: Location.t option -> Parsetree.vname ->
        ScopeInformation.value_binding_info -> t -> t
    val find_value :
      loc: Location.t -> current_unit: Types.fname ->
        current_species_name: string option -> Parsetree.expr_ident -> t ->
          ScopeInformation.value_binding_info

    val add_constructor : Parsetree.constructor_name -> Types.fname -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit:Types.fname ->
        Parsetree.constructor_ident -> t -> Types.fname

    val add_label : Parsetree.vname -> Types.fname -> t -> t
    val find_label :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.label_ident ->
        t -> Types.fname

    val add_type :
      loc: Location.t -> Parsetree.vname ->
        ScopeInformation.type_binding_info ->
      t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> t ->
        ScopeInformation.type_binding_info

    val add_species :
      loc: Location.t -> Parsetree.vname ->
        ScopeInformation.species_binding_info -> t -> t
    val find_species :
        loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
      t -> ScopeInformation.species_binding_info

    val append : t -> t -> t
  end

type 'a binding_origin =
    (** The binding comes from the current compilation unit. *)
  | BO_absolute of 'a
    (** The binding was inserted by a "open" directive of the file in
        argument. *)
  | BO_opened of (Types.fname * 'a)

type ('constrs, 'labels, 'types, 'values, 'species) generic_env = {
  constructors : (Parsetree.constructor_name * ('constrs binding_origin)) list;
  labels : (Parsetree.vname * ('labels binding_origin)) list;
  types : (Parsetree.vname * ('types binding_origin)) list;
  (** [idents] Contains functions methods and more generally any let-bound
      identifiers. *)
  values : (Parsetree.vname * ('values binding_origin)) list;
  (** [species] Contains both species and collections. *)
  species : (Parsetree.vname * ('species binding_origin)) list
}


module TypingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value :
      toplevel: Location.t option -> Parsetree.vname -> Types.type_scheme ->
        t -> t
    val find_value :
      loc: Location.t -> current_unit: Types.fname ->
        current_species_name:string option -> Parsetree.expr_ident -> t ->
          Types.type_scheme

    val add_constructor :
      Parsetree.constructor_name ->
      TypeInformation.constructor_description -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit:Types.fname ->
        Parsetree.constructor_ident -> t ->
          TypeInformation.constructor_description

    val add_label :
      Parsetree.vname -> TypeInformation.label_description -> t -> t
    val find_label :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.label_ident ->
        t -> TypeInformation.label_description

    val add_type :
      loc: Location.t -> Parsetree.vname ->
        TypeInformation.type_description -> t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.ident -> t -> TypeInformation.type_description

    val add_species :
      loc: Location.t -> Parsetree.vname ->
        TypeInformation.species_description -> t -> t
    val find_species :
        loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
          t -> TypeInformation.species_description

    val append : t -> t -> t
  end

(* For focaltest : *)
val get_species_list : TypingEnv.t -> Parsetree.vname list
val get_constructor_list : TypingEnv.t -> Parsetree.constructor_name list
val get_type_list : TypingEnv.t -> Parsetree.vname list
(* *************** *)

module MlGenEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_constructor :
      Parsetree.constructor_name ->
        MlGenInformation.constructor_mapping_info -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit: Types.fname ->
        Parsetree.constructor_ident -> t ->
          MlGenInformation.constructor_mapping_info

    val add_label :
      Parsetree.vname -> MlGenInformation.label_mapping_info -> t -> t
    val find_label :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.label_ident ->
        t -> MlGenInformation.label_mapping_info

    val add_species :
      loc: Location.t -> Parsetree.vname ->
        MlGenInformation.species_binding_info -> t -> t
    val find_species :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
        t -> MlGenInformation.species_binding_info

    val append : t -> t -> t
  end

module CoqGenEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_species :
      loc: Location.t ->
      Parsetree.vname -> CoqGenInformation.species_binding_info -> t -> t
    val find_species :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.ident -> t -> CoqGenInformation.species_binding_info

    val add_value :
      toplevel: Location.t option -> Parsetree.vname ->
        CoqGenInformation.value_mapping_info -> t -> t
    val find_value :
      loc: Location.t ->
      current_unit: Types.fname -> current_species_name:string option ->
        Parsetree.expr_ident -> t -> CoqGenInformation.value_mapping_info

    val add_constructor :
      Parsetree.constructor_name ->
        CoqGenInformation.constructor_mapping_info -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit: Types.fname ->
        Parsetree.constructor_ident -> t ->
          CoqGenInformation.constructor_mapping_info

    val add_label :
      Parsetree.vname -> CoqGenInformation.label_mapping_info -> t -> t
    val find_label :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.label_ident -> t -> CoqGenInformation.label_mapping_info

    val add_type :
      loc: Location.t -> Parsetree.vname ->
        CoqGenInformation.type_info -> t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.ident -> t -> CoqGenInformation.type_info

    val append : t -> t -> t
  end


module DkGenEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_species :
      loc: Location.t ->
      Parsetree.vname -> DkGenInformation.species_binding_info -> t -> t
    val find_species :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.ident -> t -> DkGenInformation.species_binding_info

    val add_value :
      toplevel: Location.t option -> Parsetree.vname ->
        DkGenInformation.value_mapping_info -> t -> t
    val find_value :
      loc: Location.t ->
      current_unit: Types.fname -> current_species_name:string option ->
        Parsetree.expr_ident -> t -> DkGenInformation.value_mapping_info

    val add_constructor :
      Parsetree.constructor_name ->
        DkGenInformation.constructor_mapping_info -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit: Types.fname ->
        Parsetree.constructor_ident -> t ->
          DkGenInformation.constructor_mapping_info

    val add_label :
      Parsetree.vname -> DkGenInformation.label_mapping_info -> t -> t
    val find_label :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.label_ident -> t -> DkGenInformation.label_mapping_info

    val add_type :
      loc: Location.t -> Parsetree.vname ->
        DkGenInformation.type_info -> t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname ->
        Parsetree.ident -> t -> DkGenInformation.type_info

    val append : t -> t -> t
  end

exception No_available_OCaml_code_generation_envt of Types.fname
exception No_available_Coq_code_generation_envt of Types.fname
exception No_available_Dk_code_generation_envt of Types.fname

type fo_file_structure

val scope_open_module :
  loc: Location.t -> Types.fname -> ScopingEnv.t -> ScopingEnv.t

val type_open_module :
  loc: Location.t -> Types.fname -> TypingEnv.t -> TypingEnv.t

val mlgen_open_module :
  loc: Location.t -> Types.fname -> MlGenEnv.t -> MlGenEnv.t

val coqgen_open_module :
  loc: Location.t -> Types.fname -> CoqGenEnv.t -> CoqGenEnv.t

val dkgen_open_module :
  loc: Location.t -> Types.fname -> DkGenEnv.t -> DkGenEnv.t


val make_fo_file :
  source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t ->
    MlGenEnv.t option -> CoqGenEnv.t option -> DkGenEnv.t option -> unit

val iter_on_species_scopped :
  (Parsetree.vname * ScopeInformation.species_binding_info binding_origin ->
    unit) ->
  fo_file_structure ->
    unit

val inspect_fo_structure : Format.formatter -> fo_file_structure -> unit

(* For debug purpose. *)
val print_field_for_debug : TypeInformation.species_field -> unit

