exception Unbound_constructor of (Parsetree.vname * Location.t)
exception Unbound_label of (Types.label_name * Location.t)
exception Unbound_identifier of (Parsetree.vname * Location.t)
exception Unbound_type of (Types.type_name * Location.t)
exception Unbound_module of (Types.fname * Location.t)
exception Unbound_species of (Types.species_name * Location.t)


module ScopeInformation :
  sig
    type value_binding_info =
      | SBI_file of Types.fname
      | SBI_method_of_self
      | SBI_method_of_coll of Types.collection_name
      | SBI_local

    type type_binding_info =
      | TBI_builtin_or_var
      | TBI_defined_in of Types.fname

  type species_scope =
    | SPBI_file of Types.fname
    | SPBI_local

  type species_binding_info = {
    spbi_methods : Parsetree.vname list ;
    spbi_scope : species_scope
  }
  end


module TypeInformation :
  sig
    type species_param =
      | SPAR_in of Parsetree.vname * Types.type_simple
      | SPAR_is of Parsetree.vname * Types.type_simple
    type species_description = {
      spe_is_collection : bool ;
      spe_sig_params : species_param list ;
      spe_sig_inher : Types.type_species list ;
      spe_sig_methods :
        (Parsetree.vname * Types.type_scheme * Parsetree.expr option) list
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
      | TK_variant of (Parsetree.constr_name * Types.type_scheme) list
      | TK_record of
          (Types.label_name * field_mutability * Types.type_scheme) list
    type type_description = {
      type_kind : type_kind ;
      type_identity : Types.type_scheme ;
      type_params : Types.type_simple list ;
      type_arity : int
    }

  val pp_species_description : Format.formatter -> species_description -> unit
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
      Parsetree.ident -> t -> ScopeInformation.value_binding_info

    val add_constructor : Parsetree.constr_name -> Types.fname -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit:Types.fname -> Parsetree.ident ->
	t -> Types.fname

    val add_label : Types.label_name -> Types.fname -> t -> t
    val find_label : loc: Location.t -> Types.label_name -> t -> Types.fname

    val add_type : Types.type_name -> ScopeInformation.type_binding_info ->
      t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> t ->
	ScopeInformation.type_binding_info

    val add_species :
      Types.species_name -> ScopeInformation.species_binding_info -> t -> t
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
      loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
	t -> Types.type_scheme

    val add_constructor :
      Parsetree.constr_name ->
      TypeInformation.constructor_description -> t -> t
    val find_constructor :
      loc: Location.t -> current_unit:Types.fname ->
      Parsetree.ident -> t -> TypeInformation.constructor_description

    val add_label :
      Types.label_name -> TypeInformation.label_description -> t -> t
    val find_label :
      loc: Location.t -> Types.label_name -> t ->
	TypeInformation.label_description

    val add_type :
      Types.type_name -> TypeInformation.type_description -> t -> t
    val find_type :
      loc: Location.t -> current_unit: Types.fname ->
      Parsetree.ident -> t -> TypeInformation.type_description

    val add_species :
      Types.species_name -> TypeInformation.species_description -> t -> t
    val find_species :
	loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->
	  t -> TypeInformation.species_description
  end

val scope_open_module :
  loc: Location.t -> Types.fname -> ScopingEnv.t -> ScopingEnv.t
val type_open_module :
  loc: Location.t -> Types.fname -> TypingEnv.t -> TypingEnv.t

val make_fo_file :
  source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t -> unit
