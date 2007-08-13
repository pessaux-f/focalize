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


(* $Id: env.ml,v 1.12 2007-08-13 15:01:11 pessaux Exp $ *)

(* ************************************************************************** *)
(** {b Descr} : This module contains the whole environments mechanisms.
    Environnments are used for both scoping and typing. Because then share
    a similar structure, we want to factorize their code.
    After scoping and typing, a persistent datastructure needs to be dumped
    on the disk ("compiled information" for "use" and "open" directives).
    Instead of having 2 files we dump both structures inside a same file:
    the ".fo" file. For having this working, the "module" mechanism relying
    on loading the ".fo" file from the FoCaL source "open" or "use" directive
    must know the 2 environments (scope and type) structures.
    That's basically the reason we have here:
	- The structure of scoping information used in scoping environments.
        - The structure od typing information used in typing environments.
        - The shared [find_module] function allowing to retrieve on the disk
	  The persistent datastructure related to de "module" name.
	- The scoping environment primitives.
	- The typing environment primitives.                                  *)
(* ************************************************************************** *)



exception Unbound_constructor of Parsetree.vname ;;
exception Unbound_label of Types.label_name ;;
exception Unbound_identifier of Parsetree.vname ;;
exception Unbound_type of Types.type_name ;;
exception Unbound_module of Parsetree.fname ;;
exception Unbound_collection of Types.collection_name ;;
exception Unbound_species of Types.species_name ;;


(* ******************************************************************** *)
(** {b Desc} : Enables to differentiate environment bindings induced by
             definitions really present in the current compilation unit
             and bindings introduced by a "open" directive.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
type 'a binding_origin =
    (* The binding comes from the current compilation unit. *)
  | BO_absolute of 'a
    (* The binding was inserted by a "open" *)
    (* directive of the file in argument.   *)
  | BO_opened of (Parsetree.fname * 'a)
;;



(* ****************************************************************** *)
(* allow_opened: bool -> 'a -> ('a * 'b binding_origin) list -> 'b *)
(** {b Descr} : Performs a List.assoc like process, but only return a
              BO_opened binding if the [~allow_opened] flag is true.
              Otherwise, continue search until a BO_absolute binding
              is found.

    {b Rem} : Not exported outside this module.                       *)
(* ****************************************************************** *)
let env_list_assoc ~allow_opened searched list =
  let rec rec_assoc = function
    | [] -> raise Not_found
    | (name, data) :: q ->
	if name = searched then
	  (begin
	  match data with
	   | BO_opened (_, v) -> if allow_opened then v else rec_assoc q
	   | BO_absolute v -> v
	  end)
	else rec_assoc q in
  rec_assoc list
;;


(*
let debug_env_list_assoc ~allow_opened searched list =
  let rec rec_assoc = function
    | [] -> raise Not_found
    | (name, data) :: q ->
Printf.eprintf "\"%s\"" (Parsetree_utils.name_of_vname name) ; flush stderr ;
	if name = searched then
	  (begin
	  match data with
	   | BO_opened (_, v) -> if allow_opened then v else rec_assoc q
	   | BO_absolute v -> v
	  end)
	else rec_assoc q in
  rec_assoc list
;;
*)


(* ********************************************************************** *)
(** {Descr} : Type of the generic environments. It is parametrised by the
            information to bind to identifiers representing:
            - Sum-types constructors
            - Record-types labels 
            - Values
            - Species
            - Collections.

    {b Rem} : Exported abstract outside this module.                      *)
(* ********************************************************************** *)
type ('constrs, 'labels, 'types, 'values, 'species, 'colls) generic_env = {
  constructors : (Parsetree.constr_name * ('constrs binding_origin)) list ;
  labels : (Types.label_name * ('labels binding_origin)) list ;
  types : (Types.type_name * ('types binding_origin)) list ;
  (** [idents] Contains functions methods and more generally any let-bound
identifiers. *)
  values : (Parsetree.vname * ('values binding_origin)) list ;
  species : (Types.species_name * ('species binding_origin)) list ;
  collections : (Types.collection_name * ('colls binding_origin)) list
} ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
(** {b Descr} : This module contains the structure of scoping information
              used in scoping environments.

    {b Rem} : This module is exported. Only the type of the environment is
            abstracted to prevent savage manipulations.                    *)
(* *********************************************************************** *)
module ScopeInformation = struct
  (* ********************************************************************* *)
  (* type value_binding_info                                               *)
  (** {b Descr} : Tag each binding in the scopping environment in order to
	      know if the [ident] is currently bound to a global toplevel
	      definition inside a file, if it's a method found in the
	      current species inheritance tree (including itself), if it's
              a method found in another species inheritance tree or
              finally if it's a locally bound identifier.

      {b Rem} : Exported outside this module.                              *)
  (* ********************************************************************* *)
  type value_binding_info =
      (* The ident is at toplevel of a file (including the current file). *)
    | SBI_file of Parsetree.fname
      (* The ident is a method implicitely of self. *)
    | SBI_method_of_self
      (* The ident is a method explicitely of a collection. ATTENTION: while inserting a method in the environment, it must always be tagged with [SBI_method_of_self]. The tag [SBI_method_of_coll] can only be returned by [find_value] who may perform a change on the fly if required. *)
    | SBI_method_of_coll of Types.collection_name
      (* The ident is a locally bound indentifier (let or function parameter. *)
    | SBI_local



  type type_binding_info =
      (* The type identifier is either a type variable name ('a for instance) or a builtin type (int for instance). *)
    | TBI_builtin_or_var
      (* The identifier is a type name defined at toplevel in a file. *)
    | TBI_defined_in of Parsetree.fname


  type collection_scope =
  (* The identifier is a collection name defined at toplevel in a file. *)
    | CBI_file of Parsetree.fname
      (* The identifier is a locally bound collection like in the case of a "is"-bound parameter (i.e. [c is ...]) where [c] is then considered as a local collection). *)
    | CBI_local


  type collection_binding_info = {
    (* The list of *ALL* the method owned, including those added by inheritance. Methods from the toplevel ancestor are in head of the list. In case of multiple inheritance, we consider that ancestors are older from left to right. *)
    cbi_methods : Parsetree.vname list ;
    cbi_scope : collection_scope
  }



  type species_scope =
      (* The identifier is a specied name defined at toplevel in a file. *)
    | SPBI_file of Parsetree.fname


  type species_binding_info = {
    (* The list of *ALL* the method owned, including those added by inheritance. Methods from the toplevel ancestor are in head of the list. In case of multiple inheritance, we consider that ancestors are older from left to right. *)
    spbi_methods : Parsetree.vname list ;
    spbi_scope : species_scope
  }


  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
                scoping environments.

      {b Rem} : Not exported outside this module.                   *)
  (* ************************************************************** *)
  type env =
    (Parsetree.fname, Parsetree.fname, type_binding_info, value_binding_info,
     species_binding_info, collection_binding_info) generic_env
end ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
(** {b Descr} : This module contains the structure of typing information
              used in typing environments.

    {b Rem} : This module is exported. Only the type of the environment is
            abstracted to prevent savage manipulations.                    *)
(* *********************************************************************** *)
module TypeInformation = struct
  type species_param =
    | SPAR_in of Parsetree.vname * Types.type_simple    (* Entity param. *)
    | SPAR_is of Parsetree.vname * Types.type_simple    (* Collection param. *)



  type species_description = {
    spe_sig_params : species_param list ;
    spe_sig_inher : Types.type_species list ;
    spe_sig_methods :  (** Method's name, type and body if defined. *)
      (Parsetree.vname * Types.type_scheme * (Parsetree.expr option)) list
    }



  type collections_sig =
    (** Method's name, type. *)
    (Parsetree.vname  * Types.type_scheme) list (* To refine. *)



  type constructor_arity = CA_zero | CA_one



  type constructor_description = {
    (** Arity : 0 or 1 (many = 1 type tuple), (1 = type, not a 1 tuple). *)
    cstr_arity : constructor_arity ;
    (** Full type scheme for this constructor, i.e (args ->) ty result. *)
    cstr_scheme : Types.type_scheme
  }



  type field_mutability = FM_mutable | FM_immutable



  type label_description = {
    field_mut : field_mutability ;    (** Mutability for this field. *)
    (** Full type scheme for this field, i.e arg -> ty result. *)
    field_scheme : Types.type_scheme
  }



  type type_kind =
    | TK_abstract  (** Abstract types and type abbreviations. *)
    | TK_variant of    (** Sum types. *)
	(Parsetree.constr_name * Types.type_scheme) list
    | TK_record of  (** Record types: list of labels. Any value of a type record will be typed as a [ST_construct] whose name is the name of the record type. *)
	(Types.label_name * field_mutability * Types.type_scheme) list



  type type_description = {
    type_kind : type_kind ;             (** Kind of the type definition. *)
    (** The type scheme representing to what this type is equal to. For
        instance in type 'a t = 'a list, t is TK_abstract with [type_identity]
        representing 'a list.
        If the type is a pure abstract like in type t, then t is TK_abstract
        with [type_identity] representing the type ST_construct ("t", []). *)
    type_identity : Types.type_scheme ;
    type_arity : int          (** Number of parameters of the type. *)
  }


  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
                typing environments.

      {b Rem} : Not exported outside this module.                   *)
  (* ************************************************************** *)
  type env =
   (constructor_description, label_description, type_description,
    Types.type_scheme, species_description, collections_sig) generic_env
end ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



let (scope_find_module, type_find_module) =
  (* Let's just make the list used to bufferize opened files' content. *)
  let buffered =
    ref ([] :
	   (Parsetree.fname * (ScopeInformation.env * TypeInformation.env))
	   list) in
  (* ***************************************************************** *)
  (* Parsetree.fname -> (ScopeInformation.env * TypeInformation.env)   *)
  (** {b Descr} : Wrapper to lookup inside an external interface file.
                The lookup also performs a bufferisation to prevent
                futhers calls from accessing again the disk. This
                should enhance lookup speed.

      {b Rem} : Not exported outside this module.                      *)
  (* ***************************************************************** *)
  let internal_find_module fname =
    (begin
    try List.assoc fname !buffered
    with Not_found ->
      (* The interface was not already loaded... *)
      let fo_name = Files.fo_filename_from_module_name fname in
      try
	(* Try to open the interface file. *)
	let in_file = Files.open_in_from_lib_paths fo_name in
	(* Just ensure it's really an interface file. *)
	if Files.check_magic in_file Files.fo_magic then
	  (begin
	  let envts = input_value in_file in
	  close_in in_file ;
	  (* If the interface was found, buferize it for further uses. *)
	  buffered := (fname, envts) :: !buffered ;
	  envts
	  end)
	else
	  (begin
	  close_in in_file ;
	  raise (Files.Corrupted_fo fname)
	  end)
      with Files.Cant_access_file _ -> raise (Unbound_module fname)
    end) in
  ((* **************************************************************** *)
   (* current_unit: Parsetree.fname -> Parsetree.fname option ->       *)
   (*   ScopeInformation.env -> ScopeInformation.env                   *)
   (** {b Descr} : Wrapper to lookup a scoping environment inside an
                 external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                     *)
   (* **************************************************************** *)
   (fun ~current_unit fname_opt scope_env ->
    match fname_opt with
     | None -> scope_env
     | Some fname ->
	 if current_unit = fname then scope_env
	 else fst (internal_find_module fname)),



   (* **************************************************************** *)
   (* current_unit: Parsetree.fname -> Parsetree.fname option ->       *)
   (*   TypeInformation.env -> TypeInformation.env                     *)
   (** {b Descr} : Wrapper to lookup a typing environment inside an
                 external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                     *)
   (* **************************************************************** *)
   (fun ~current_unit fname_opt type_env ->
     match fname_opt with
      | None -> type_env
      | Some fname ->
	  if current_unit = fname then type_env
	  else snd (internal_find_module fname)))
;;



(* ************************************************************************* *)
(** {b Descr} : Returns a boolean telling whether the lookup of a vname in
              and environment can succeed on a binding induced by an "open"
              directive.
              This is the case except in the 2 following cases:
                - if the [ident] from where the [vname] is extracted is
                  an I_global (None, ...). This corresponds to an [ident]
                  like #Foo, hence meaning that on search for something at
                  toplevel of the current compilation unit. Hence the right
                  binding cannot be something "Opened".
                - if the [ident] from where the [vname] is extracted is
                  an I_global (Some fname, ...) with current_unit = fname.
                  This corresponds to an [ident] like doudou#Foo while we
                  are analysing the file "doudou". It's then also a search
                  for something at toplevel of the current compilation unit.
                  Hence the right binding cannot also be something "Opened".

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let allow_opened_p current_unit = function
  | None -> false
  | Some fname -> current_unit = fname
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
(** {b Descr} : Type of modules used to encapsulate the function allowing
              to access the environment related to a FoCaL "module" via
	      the persistent data located on the disk.
              This kind of module will be used as argument of the functor
              [Make] below in order to generate the environment access
              functions without having to add to them extra parameters
              that would the [find_module], [pervasives],
              [make_value_env_from_species_methods] and
              [make_value_env_from_collection_methods] functions.

    {b Fields} :
      - [constructor_bound_data] : Type of the information bound to a
        sum type constructor identifier in the environment.
      - [label_bound_data] : Type of the information bound to a record
        type's field label identifier in the environment.
      - [type_bound_data] : Type of the information bound to a type
        identifier in the environment.
      - [value_bound_data] : Type of the information bound to a value
        identifier in the environment.
      - [species_bound_data] : Type of the information bound to a species
        identifier in the environment.
      - [collection_bound_data] : Type of the information bound to a
        collection identifier in the environment.
      - [find_module] : The function that permits to access FoCaL "module"
        information (# notation) via the persistent data files.
      - [pervasives] : Function generating a fresh environment containing
        builtin stuff.
      - [make_value_env_from_species_methods] : Function building an
        environment with the methods of the species inserted as values.
      - [make_value_env_from_collection_methods] : Function building an
        environment with the methods of the collection inserted as values.

    {b Rem} : Not exported outside this module.
            This module signature is fully local to this file, it will be
            internally be used to create the various environment
            structures (scoping, typing) we need.                         *)
(* *********************************************************************** *)
module type EnvModuleAccessSig = sig
  type constructor_bound_data
  type label_bound_data
  type type_bound_data
  type value_bound_data
  type species_bound_data
  type collection_bound_data
  val find_module :
    current_unit: Parsetree.fname -> Parsetree.fname option ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data, collection_bound_data)
      generic_env ->
        (constructor_bound_data, label_bound_data, type_bound_data,
         value_bound_data, species_bound_data, collection_bound_data)
      generic_env
  val pervasives : unit ->
    (constructor_bound_data, label_bound_data, type_bound_data,
     value_bound_data, species_bound_data, collection_bound_data)
      generic_env
  val make_value_env_from_species_methods :
    Types.species_name -> species_bound_data ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data, collection_bound_data)
	generic_env
  val make_value_env_from_collection_methods :
    Types.collection_name -> collection_bound_data ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data, collection_bound_data)
	generic_env
  val post_process_method_value_binding :
    collname: Types.collection_name -> value_bound_data -> value_bound_data
end ;;



(* ********************************************************************* *)
(** {b Descr} : This functor creates the environment manipulation
              primitives (lookup and insertion), using the [find_module]
              function of the argument [EMAccess] to access FoCal
              "modules" persistent data on disk.

    {b Rem} : Not exported outside this module.
	      This functor is fully local to this file, it will be
	      internally be used to create the various environment
	      structures (scoping, typing) we need.                      *)
(* ********************************************************************* *)
module Make(EMAccess : EnvModuleAccessSig) = struct
  (* ************************************************ *)
  (** {b Descr} : The type of the environment itself.

      {b Rem} : Exported outside this module.         *)
  (* ************************************************ *)
  type t =
    (EMAccess.constructor_bound_data, EMAccess.label_bound_data,
     EMAccess.type_bound_data, EMAccess.value_bound_data,
     EMAccess.species_bound_data, EMAccess.collection_bound_data) generic_env

  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh TOTALY empty environment
		(no even pervasive stuff inside).

      {b Rem} : Exported outside this module.              *)
  (* ***************************************************** *)
  let empty () =
    ({ constructors = [] ; labels = [] ; types = [] ; values = [] ;
      species = [] ; collections = [] } : t)


  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh environment containing
                information bound the basic builtins.

      {b Rem} : Exported outside this module.              *)
  (* ***************************************************** *)
  let pervasives () = EMAccess.pervasives ()



  (* current_unit: Parsetree.fname -> Parsetree.ident -> t ->       *)
  (*   EMAccess.collection_bound_data                               *)
  let rec find_collection ~current_unit coll_ident (env : t) =
    match coll_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_collection_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_collection_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Because collection are not first class values,   *)
	 (* collection identifiers should never be methods ! *)
	 assert false



  (* allow_opened: bool -> Parsetree.vname -> t ->    *)
  (*   EMAccess.collection_bound_data                 *)
  and find_collection_vname ~allow_opened vname (env : t) =
    let coll_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened coll_name env.collections with
    | Not_found -> raise (Unbound_collection coll_name)



  (* Types.species_name -> EMAccess.species_bound_data -> t -> t *)
  let add_species species_name data (env : t) =
     ({ env with
	  species = (species_name, BO_absolute data) :: env.species } : t)



  (* current_unit: Parsetree.fname -> Parsetree.ident -> t ->    *)
  (*   EMAccess.species_bound_data                               *)
  let rec find_species ~current_unit coll_ident (env : t) =
    match coll_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_species_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_species_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Because species are not first class values,   *)
	 (* species identifiers should never be methods ! *)
	 assert false


  (* allow_opened: bool -> Parsetree.vname -> t ->    *)
  (*   EMAccess.species_bound_data                 *)
  and find_species_vname ~allow_opened vname (env : t) =
    let coll_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened coll_name env.species with
    | Not_found -> raise (Unbound_species coll_name)



  (* ************************************************************** *)
  (* Parsetree.vname -> EMAccess.value_bound_data -> t -> t         *)
  (** {b Descr} : Return an environment extended with a binding
                between a value [ident] and the argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_value ident data (env : t) =
    ({ env with values = (ident, BO_absolute data) :: env.values } : t)



  (* ******************************************************************* *)
  (* current_unit: Parsetree.fname -> Parsetree.ident ->                 *)
  (*   t -> EMAccess.value_bound_data                                    *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let rec find_value ~current_unit ident_ident (env : t) =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_value_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_value_vname ~allow_opened vname env'
     | Parsetree.I_method (collname_opt, vname) ->
	 (begin
	 match collname_opt with
	  | None | Some "Self" ->
	      (* No collection scope. Then the searched ident must belong  *)
	      (* to the inheritance of Self. First, this means that opened *)
	      (* stuff is forbidden. Next, because the [values] bucket is  *)
	      (* so that inherited methods and our methods belong to it,   *)
              (* we just have to search for the [vname] inside the current *)
              (* environment.                                              *)
	      find_value_vname ~allow_opened: false vname env
	  | Some collname ->
	      (* Collections are alwas capitalized so *)
              (* the related [vname] is a [Vuident].  *)
	      let coll_vname = Parsetree.Vuident collname in
	      (* We must first look inside collections and species   *)
              (* for the [collname] in order to recover its methods. *)
              let available_meths =
		(try
		  EMAccess.make_value_env_from_collection_methods
		    collname
		    (find_collection_vname ~allow_opened: false coll_vname env)
		with Unbound_collection _ ->
		  EMAccess.make_value_env_from_species_methods
		    collname
		    (find_species_vname ~allow_opened: false coll_vname env)) in
	      let data =
		find_value_vname ~allow_opened: false vname available_meths in
	      (* Now we apply the post-processing on the found data. *)
	      EMAccess.post_process_method_value_binding ~collname data
	 end)



  (* ****************************************************************** *)
  (* allow_opened: bool -> Parsetree.vname -> t ->                      *)
  (*   EMAccess.value_bound_data                                        *)
  (** {b Descr} : Looks-up for a [vname] inside the values environment.

      {b Rem} : Not exported outside this module.                       *)
  (* ****************************************************************** *)
  and find_value_vname ~allow_opened vname (env : t) =
(*    try env_list_assoc ~allow_opened vname env.values *)
    try (*debug_env_list_assoc*) env_list_assoc ~allow_opened vname env.values
    with Not_found -> raise (Unbound_identifier vname)



  (* ************************************************************** *)
  (* Parsetree.constr_name -> EMAccess.constructor_bound_data ->    *)
  (*   t -> t                                                       *)
  (** {b Descr} : Return an environment extended with a binding
                between a sum-type constructor [ident] and the
                argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_constructor cstr_name data (env : t) =
    ({ env with
        constructors = (cstr_name, BO_absolute data) :: env.constructors } : t)



  (* ************************************************************ *)
  (* current_unit: Parsetree.fname -> Parsetree.ident ->          *)
  (*   t -> EMAccess.constructor_bound_data                       *)
  (** {b Descr} : Looks-up for an [ident] inside the constructors
		environment.

      {b Rem} : Exported outside this module.                     *)
  (* ************************************************************ *)
  let rec find_constructor ~current_unit cstr_ident (env : t) =
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_constructor_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_constructor_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Don't know what it means if the           *)
	 (* constructor seems to be in fact a method. *)
	 assert false



  (* *********************************************************** *)
  (* allow_opened: bool -> Parsetree.constr_name -> t ->         *)
  (*   EMAccess.constructor_bound_data                           *)
  (** {b Descr} : Looks-up for a [vname] inside the constructors
                environment.

      {b Rem} : Not exported outside this module.                *)
  (* *********************************************************** *)
  and find_constructor_vname ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.constructors
    with Not_found -> raise (Unbound_constructor vname)



  (* ************************************************************** *)
  (* Types.label_name -> EMAccess.label_bound_data -> t -> t        *)
  (** {b Descr} : Return an environment extended with a binding
                between a record field label [ident] and the
                argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_label lbl_name data (env : t) =
     ({ env with labels = (lbl_name, BO_absolute data) :: env.labels } : t)



  (* ************************************************************* *)
  (* Types.label_name -> > t -> EMAccess.label_bound_data          *)
  (** {b Descr} : Looks-up for an [ident] inside the record fields
		labels environment.

      {b Rem} : Exported outside this module.                      *)
  (* ************************************************************* *)
  let find_label lbl_name (env : t) =
    try
      (* Because labels cannot be written with a # notation, the only   *)
      (* way is to access toplevel labels of the current compilation    *)
      (* unit. Hence such bindings cannot be induced by opened modules. *)
      env_list_assoc ~allow_opened: false lbl_name env.labels with
    | Not_found -> raise (Unbound_label lbl_name)



  (* ************************************************************** *)
  (* Types.type_name -> EMAccess.type_bound_data -> t -> t          *)
  (** {b Descr} : Return an environment extended with a binding
                between a type [ident] and the argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_type tyname data (env : t) =
    ({ env with types = (tyname, BO_absolute data) :: env.types } : t)



  (* ****************************************************************** *)
  (* current_unit: Parsetree.fname -> Parsetree.ident -> t ->           *)
  (*   EMAccess.type_bound_data                                         *)
  (** {b Descr} : Looks-up for an [ident] inside the types environment.

      {b Rem} : Exported outside this module.                           *)
  (* ****************************************************************** *)
  let rec find_type ~current_unit type_ident (env : t) =
    match type_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_type_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_type_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Type identifiers should never be methods ! *)
	 assert false



  (* **************************************************************** *)
  (* allow_opened: bool -> Parsetree.vname -> Parsetree.vname -> t -> *)
  (*   EMAccess.type_bound_data                                       *)
  (** {b Descr} : Looks-up for a [vname] inside the types environment.

      {b Rem} : Not exported outside this module.                     *)
  (* **************************************************************** *)
  and find_type_vname ~allow_opened vname (env : t) =
    let type_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened type_name env.types with
    | Not_found -> raise (Unbound_type type_name)


  (* Types.collection_name -> EMAccess.collection_bound_data -> t -> t *)
  let add_collection coll_name data (env : t) =
     ({ env with
	  collections = (coll_name, BO_absolute data) :: env.collections } : t)
end ;;



module ScopingEMAccess = struct
  type constructor_bound_data = Parsetree.fname
  type label_bound_data = Parsetree.fname
  type type_bound_data = ScopeInformation.type_binding_info
  type value_bound_data = ScopeInformation.value_binding_info
  type species_bound_data = ScopeInformation.species_binding_info
  type collection_bound_data = ScopeInformation.collection_binding_info
  let find_module = scope_find_module
  let pervasives () =
    { constructors = [
        (Parsetree.Vlident "[]", BO_opened ("basics", "basics")) ;
        (Parsetree.Viident "::", BO_opened ("basics", "basics"))
        ] ;
      labels = [] ;
      types = [
        ("int",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("float",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("bool",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("string",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("char",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("unit",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var)) ;
        ("list",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var))
        ] ;
      values = [] ;
      species = [] ;
      collections = [] }


  let make_value_env_from_collection_methods coll_name coll_info =
    let values_bucket =
      List.map
	(fun meth_vname ->
	  (meth_vname,
	   BO_absolute (ScopeInformation.SBI_method_of_coll coll_name)))
	coll_info.ScopeInformation.cbi_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] ; collections = [] }

  let make_value_env_from_species_methods spec_name spec_info =
    let values_bucket =
      List.map
	(fun meth_vname ->
	  (meth_vname,
	   BO_absolute (ScopeInformation.SBI_method_of_coll spec_name)))
	spec_info.ScopeInformation.spbi_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] ; collections = [] }


  (* ************************************************************************ *)
  (** {b Descr} : The function used while scoping to post-process information
               bound to values when retrieved from a [I_method] [ident].
               Because all the methods are entered in the environment as
               [SBI_method_of_self], we need here to tweak the result to
               change it into a SBI_method_of_coll otherwise, any method
               would be considered as applied from [Self] !!!
               In fact, we need to enter methods in the environment as
               [SBI_method_of_self] instead of [SBI_method_of_coll]
               otherwise when we would scope a real method of [Self], if
               this méthode was bound to, for instance "KikooSpecies", then
               once scoped, the AST would return an [ident] explicitely
               shaped like "KikooSpecies!meth" instead of "Self!meth".
               This would force the method to be the one of this species
               at this level in the inheritance, hence forbidding
               late-binding.

     {b Rem} : Exported outside this module, but not outside this file.       *)
  (* ************************************************************************ *)
  let post_process_method_value_binding ~collname = function
    | ScopeInformation.SBI_method_of_self ->
	ScopeInformation.SBI_method_of_coll collname
    | whatever -> whatever
end ;;
module ScopingEnv = Make (ScopingEMAccess) ;;



module TypingEMAccess = struct
  type constructor_bound_data = TypeInformation.constructor_description
  type label_bound_data = TypeInformation.label_description
  type type_bound_data = TypeInformation.type_description
  type value_bound_data = Types.type_scheme
  type species_bound_data = TypeInformation.species_description
  type collection_bound_data = TypeInformation.collections_sig
  let find_module = type_find_module
  (* ************************************************************ *)
  (* make_pervasives : unit -> TypeInformation.env                *)
  (** {b Descr} : Creates a fresh environment containing the type
		information of the basic built-in primitives,
		types and constructors.

      {b Rem} : Exported outside this module.                     *)
  (* ************************************************************ *)
  let pervasives () =
    (* Create the types scheme for "[]". *)
    let nil_scheme =
      (let v = Types.type_variable () in
      Types.generalize (Types.type_basic "list" [v])) in
    (* Create the types scheme for "::". *)
    let cons_scheme =
      (let v = Types.type_variable () in
      Types.generalize
	(Types.type_arrow
	   (Types.type_tuple [v; (Types.type_basic "list" [v])])
	   (Types.type_basic "list" [v]))) in
    (* And now the structure of the environment itself. *)
    {
     constructors = [
       (Parsetree.Vlident "[]",
	BO_opened
	  ("basics", { TypeInformation.cstr_arity = TypeInformation.CA_zero ;
		       TypeInformation.cstr_scheme = nil_scheme })) ;
	(Parsetree.Viident "::",
	 BO_opened
	   ("basics", { TypeInformation.cstr_arity = TypeInformation.CA_one ;
			TypeInformation.cstr_scheme = cons_scheme }))
	] ;
     labels = [] ;
     types = [
       ("int",
	BO_opened
	  ("",
	   { TypeInformation.type_kind = TypeInformation.TK_abstract ;
	     TypeInformation.type_identity =
	     Types.generalize (Types.type_int ()) ;
	     TypeInformation.type_arity = 0 })) ;
       ("float",
	BO_opened
	  ("", { TypeInformation.type_kind = TypeInformation.TK_abstract ;
		 TypeInformation.type_identity =
		   Types.generalize (Types.type_float ()) ;
		 TypeInformation.type_arity = 0 })) ;
       ("bool",
	BO_opened
	  ("", { TypeInformation.type_kind = TypeInformation.TK_abstract ;
		 TypeInformation.type_identity =
		   Types.generalize (Types.type_bool ()) ;
		 TypeInformation.type_arity = 0 })) ;
       ("string",
	BO_opened
	  ("", { TypeInformation.type_kind = TypeInformation.TK_abstract ;
		 TypeInformation.type_identity =
		   Types.generalize (Types. type_string ()) ;
		 TypeInformation.type_arity = 0 })) ;
       ("char",
	BO_opened
	  ("", { TypeInformation.type_kind = TypeInformation.TK_abstract ;
		 TypeInformation.type_identity =
		   Types.generalize (Types.type_char ()) ;
		 TypeInformation.type_arity = 0 })) ;
       ("unit",
	BO_opened
	  ("",{ TypeInformation.type_kind = TypeInformation.TK_abstract ;
		TypeInformation.type_identity =
		  Types.generalize (Types.type_unit ()) ;
		TypeInformation.type_arity = 0 })) ;
       ("list",
	BO_opened
	  ("basics", { TypeInformation.type_kind =
		       TypeInformation.TK_variant [
		         (Parsetree.Vlident "[]", nil_scheme) ;
		         (Parsetree.Viident "::", cons_scheme) ] ;
		       TypeInformation.type_identity =
		         (let v = Types.type_variable () in
			 Types.generalize (Types.type_basic "list" [v])) ;
		       TypeInformation.type_arity = 1 }))
      ] ;
    values = [] ;
    species = [] ;
    collections = []
  }


  let make_value_env_from_collection_methods coll_name coll_info =
    let values_bucket =
      List.map
	(fun (meth_vname, meth_scheme) ->
	  (meth_vname, (BO_absolute meth_scheme)))
	coll_info in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] ; collections = [] }


  let make_value_env_from_species_methods spec_name spec_info =
    let values_bucket =
      List.map
	(fun (meth_vname, meth_scheme, _) ->
	  (meth_vname, (BO_absolute meth_scheme)))
	spec_info.TypeInformation.spe_sig_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] ; collections = [] }


  (* Not yet thought about. *)
  let post_process_method_value_binding ~collname data = data
end ;;
module TypingEnv = Make (TypingEMAccess) ;;

