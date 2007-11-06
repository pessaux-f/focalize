exception Unbound_constructor of (Parsetree.vname * Location.t)
exception Unbound_label of (Parsetree.vname * Location.t)
exception Unbound_identifier of (Parsetree.vname * Location.t)
exception Unbound_type of (Parsetree.vname * Location.t)
exception Unbound_module of (Types.fname * Location.t)
exception Unbound_species of (Parsetree.vname * Location.t)

exception Rebound_type of (Parsetree.vname * Location.t)
exception Rebound_species of (Parsetree.vname * Location.t)


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
      | SPBI_file of Types.fname
      | SPBI_local

    type species_parameter_kind =
      | SPK_in
      | SPK_is

  type species_binding_info = {
    spbi_methods : Parsetree.vname list ;
    spbi_params_kind : species_parameter_kind list ;
    spbi_scope : species_scope
  }
  end


module TypeInformation :
  sig
    type species_param =
      | SPAR_in of (Parsetree.vname * Types.type_collection)
      | SPAR_is of (Parsetree.vname * (species_field list))

    and species_field =
      | SF_sig of
          (Parsetree.qualified_species * Parsetree.vname *
           Types.type_scheme)
      | SF_let of
          (Parsetree.qualified_species * Parsetree.vname *
           (Parsetree.vname list) *
           Types.type_scheme * Parsetree.expr)
      | SF_let_rec of
          (Parsetree.qualified_species * Parsetree.vname *
           (Parsetree.vname list) *
           Types.type_scheme * Parsetree.expr) list
      | SF_theorem of
          (Parsetree.qualified_species * Parsetree.vname *
           Types.type_scheme *
           Parsetree.prop * Parsetree.proof)
      | SF_property of
          (Parsetree.qualified_species * Parsetree.vname *
           Types.type_scheme * Parsetree.prop)

    type species_description = {
      spe_is_collection : bool ;
      spe_is_closed : bool ;
      spe_sig_params : species_param list ;
      spe_sig_methods : species_field list
    }
    type constructor_arity = CA_zero | CA_one
    type constructor_description = {
      cstr_arity : constructor_arity ;
      cstr_scheme : Types.type_scheme
    }
    type field_mutability = FM_mutable | FM_immutable
    type label_description = {
      field_mut : field_mutability ;
      field_scheme : Types.type_scheme
    }
    type type_kind =
        TK_abstract
      | TK_external of (Parsetree.external_expr * Parsetree.external_bindings)
      | TK_variant of (Parsetree.constructor_name * constructor_arity *
                       Types.type_scheme) list
      | TK_record of
          (Parsetree.vname * field_mutability * Types.type_scheme) list
    type type_description = {
      type_kind : type_kind ;
      type_identity : Types.type_scheme ;
      type_params : Types.type_simple list ;
      type_arity : int
    }

  val pp_species_description : Format.formatter -> species_description -> unit
  end



module MlGenInformation :
  sig
    type collection_generator_info = {
      cgi_implemented_species_params_names :
        (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
      cgi_generator_parameters :
        (Parsetree.vname * Parsetree_utils.VnameSet.t) list
    }

    type species_binding_info = collection_generator_info option
    type label_mapping_info = Parsetree.external_expr_desc
    type constructor_mapping_info = Parsetree.external_expr_desc
  end



module ScopingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value :
      Parsetree.vname -> ScopeInformation.value_binding_info -> t -> t
    val find_value :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.expr_ident -> t -> ScopeInformation.value_binding_info

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
  end


module TypingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value : Parsetree.vname -> Types.type_scheme -> t -> t
    val find_value :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.expr_ident ->
        t -> Types.type_scheme

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
  end


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
        MlGenInformation.species_binding_info -> t
        -> t
    val find_species :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
        t -> MlGenInformation.species_binding_info
  end


val scope_open_module :
  loc: Location.t -> Types.fname -> ScopingEnv.t -> ScopingEnv.t
val type_open_module :
  loc: Location.t -> Types.fname -> TypingEnv.t -> TypingEnv.t
val mlgen_open_module :
  loc: Location.t -> Types.fname -> MlGenEnv.t -> MlGenEnv.t

val make_fo_file :
  source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t ->
    MlGenEnv.t ->unit
