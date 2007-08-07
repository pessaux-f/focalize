exception Unbound_constructor of Parsetree.vname
exception Invalid_constructor_identifier of Parsetree.ident
exception Unbound_label of Types.label_name
exception Unbound_identifier of Parsetree.vname
exception Unbound_type of Types.type_name
exception Unbound_module of Parsetree.fname


module ScopeInformation :
  sig
    type value_binding_info =
      | SBI_file of Parsetree.fname
      | SBI_method_of_self
      | SBI_method_of_coll of Types.collection_name
      | SBI_local

    type type_binding_info =
      | TBI_builtin_or_var
      | TBI_defined_in of Parsetree.fname
  end


module TypeInformation :
  sig
    type species_param =
      | SPAR_in of Parsetree.vname * Types.type_simple
      | SPAR_is of Parsetree.vname * Types.type_simple
    type species_description = {
      spe_sig_params : species_param list;
      spe_sig_inher : Types.type_species list;
      spe_sig_methods :
        (string * Types.type_simple * Parsetree.expr option) list;
    }
    type collections_sig = (string * Types.type_simple) list
    type constructor_arity = CA_zero | CA_one
    type constructor_description = {
      cstr_arity : constructor_arity;
      cstr_scheme : Types.type_scheme;
    }
    type field_mutability = FM_mutable | FM_immutable
    type label_description = {
      field_mut : field_mutability;
      field_scheme : Types.type_scheme;
    }
    type type_kind =
        TK_abstract
      | TK_variant of (Parsetree.constr_name * Types.type_scheme) list
      | TK_record of
          (Types.label_name * field_mutability * Types.type_scheme) list
    type type_description = {
      type_kind : type_kind;
      type_identity : Types.type_scheme;
      type_arity : int;
    }
  end


module ScopingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value :
      Parsetree.vname -> ScopeInformation.value_binding_info -> t -> t
    val find_value :
      current_unit:Parsetree.fname ->
      Parsetree.ident -> t -> ScopeInformation.value_binding_info

    val add_constructor : Parsetree.constr_name -> Parsetree.fname -> t -> t
    val find_constructor :
      current_unit:Parsetree.fname -> Parsetree.ident -> t -> Parsetree.fname

    val add_label : Types.label_name -> Parsetree.fname -> t -> t
    val find_label : Types.label_name -> t -> Parsetree.fname

    val add_type : Types.type_name -> ScopeInformation.type_binding_info ->
      t -> t
    val find_type :
      current_unit: Parsetree.fname -> Parsetree.ident -> t ->
	ScopeInformation.type_binding_info
  end


module TypingEnv :
  sig
    type t
    val empty : unit -> t
    val pervasives : unit -> t

    val add_value : Parsetree.vname -> Types.type_scheme -> t -> t
    val find_value :
      current_unit: Parsetree.fname -> Parsetree.ident -> t -> Types.type_scheme

    val add_constructor :
      Parsetree.constr_name ->
      TypeInformation.constructor_description -> t -> t
    val find_constructor :
      current_unit:Parsetree.fname ->
      Parsetree.ident -> t -> TypeInformation.constructor_description

    val add_label :
      Types.label_name -> TypeInformation.label_description -> t -> t
    val find_label : Types.label_name -> t -> TypeInformation.label_description

    val add_type :
      Types.type_name -> TypeInformation.type_description -> t -> t
    val find_type :
      current_unit: Parsetree.fname ->
      Parsetree.ident -> t -> TypeInformation.type_description
  end
