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


(* $Id: env.ml,v 1.32 2007-09-18 10:29:38 pessaux Exp $ *)

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



exception Unbound_constructor of (Parsetree.vname * Location.t) ;;
exception Unbound_label of (Types.label_name * Location.t) ;;
exception Unbound_identifier of (Parsetree.vname * Location.t) ;;
exception Unbound_type of (Types.type_name * Location.t) ;;
exception Unbound_module of (Types.fname * Location.t) ;;
exception Unbound_species of (Types.species_name * Location.t) ;;


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
  | BO_opened of (Types.fname * 'a)
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



let debug_env_list_assoc ~allow_opened searched list =
  let rec rec_assoc = function
    | [] ->
	Format.eprintf "Search failed.\n" ;
	flush stderr ;
	raise Not_found
    | (name, data) :: q ->
	Format.eprintf "\"%s\" " (Parsetree_utils.name_of_vname name) ;
	flush stderr ;
	if name = searched then
	  (begin
	  match data with
	   | BO_opened (fname, v) ->
	       if allow_opened then
		 (begin
		 Format.eprintf
		   "Search successfully ends on opened (\"%s\")...\n" fname ;
		 flush stderr ;
		 v
		 end)
	       else rec_assoc q
	   | BO_absolute v ->
	       Format.eprintf "Search successfully ends on absolute...\n" ;
	       flush stderr ;
	       v
	  end)
	else rec_assoc q in
  Format.eprintf
    "Search starts for \"%s\"...\n" (Parsetree_utils.name_of_vname  searched) ;
  flush stderr ;
  rec_assoc list
;;



(* ********************************************************************* *)
(** {b Descr} : Type of the generic environments. It is parameterized by
            the information to bind to identifiers representing:
            - Sum-types constructors
            - Record-types labels
            - Values
            - Species (and collections)

    {b Rem} : Exported abstract outside this module.                     *)
(* ********************************************************************* *)
type ('constrs, 'labels, 'types, 'values, 'species) generic_env = {
  constructors : (Parsetree.constr_name * ('constrs binding_origin)) list ;
  labels : (Types.label_name * ('labels binding_origin)) list ;
  types : (Types.type_name * ('types binding_origin)) list ;
  (** [idents] Contains functions methods and more generally any let-bound
identifiers. *)
  values : (Parsetree.vname * ('values binding_origin)) list ;
  (** [species] Contains both species and collections. *)
  species : (Types.species_name * ('species binding_origin)) list
} ;;



(* *********************************************************************** *)
(* ('a, 'b, 'c, 'd, 'e) generic_env ->                                     *)
(*   ('a, 'b, 'c, 'd, 'e) generic_env                                      *)
(* {b Descr} : Filters a [generic_env], keeping only bindings coming from
             definitions of the current compilation unit, i.e. those
             tagged [BO_absolute].
             Such an environment will be suitable to be dumped in a
             persistent datastructure on disk for the "modules" mechanism.

   {b Rem} : Not exported outside this module.                             *)
(* *********************************************************************** *)
let env_from_only_absolute_bindings generic_env =
  let filter l =
    List.filter
      (function
	| (_, (BO_absolute _)) -> true
	| (_, (BO_opened (_, _))) -> false)
      l in
  let constructors' = filter generic_env.constructors in
  let labels' = filter generic_env.labels in
  let types' = filter generic_env.types in
  let values' = filter generic_env.values in
  let species' = filter generic_env.species in
  { constructors = constructors' ; labels = labels' ; types = types' ;
    values = values' ; species = species' }
;;



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
    | SBI_file of Types.fname
      (* The ident is a method implicitely of self. *)
    | SBI_method_of_self
      (* The ident is a method explicitely of a collection. ATTENTION: while
	 inserting a method in the environment, it must always be tagged with
	 [SBI_method_of_self]. The tag [SBI_method_of_coll] can only be
	 returned by [find_value] who may perform a change on the fly if
	 required. *)
    | SBI_method_of_coll of Types.collection_name
      (* The ident is a locally bound indentifier (let or function parameter. *)
    | SBI_local



  type type_binding_info =
      (* The type identifier is either a type variable name ('a for instance) or a builtin type (int for instance). *)
    | TBI_builtin_or_var
      (* The identifier is a type name defined at toplevel in a file. *)
    | TBI_defined_in of Types.fname



  type species_scope =
      (* The identifier is a specied name defined at toplevel in a file. *)
    | SPBI_file of Types.fname
(* The identifier is a locally bound collection like in the case of a "is"-bound parameter (i.e. [c is ...]) where [c] is then considered as a local collection). *)
    | SPBI_local



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
    (Types.fname, Types.fname, type_binding_info, value_binding_info,
     species_binding_info) generic_env
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
    | SPAR_in of (Parsetree.vname * Types.type_collection)  (* Entity param. *)
    | SPAR_is of (Parsetree.vname * (species_field list))  (* Collection param. *)


  and species_field =
    | SF_sig of (Parsetree.vname * Types.type_scheme)
    | SF_let of (Parsetree.vname * Types.type_scheme * Parsetree.expr)
    | SF_let_rec of (Parsetree.vname * Types.type_scheme * Parsetree.expr) list
    | SF_theorem of (Parsetree.vname * Types.type_scheme * Parsetree.prop *
		     Parsetree.proof)
    | SF_property of (Parsetree.vname * Types.type_scheme * Parsetree.prop)


  type species_description = {
    spe_is_collection : bool ;
    spe_sig_params : species_param list ;
    spe_sig_methods : species_field list    (** Method's name, type and body if defined. *)
    }



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
    (** Parameters of the type. Be careful, they are generalized at the same that the above scheme [type_identity] is created. Hence, physical sharing exists and is crucial ! *)
    type_params : Types.type_simple list ;
    type_arity : int          (** Number of parameters of the type. *)
  }


  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
                typing environments.

      {b Rem} : Not exported outside this module.                   *)
  (* ************************************************************** *)
  type env =
   (constructor_description, label_description, type_description,
    Types.type_scheme, species_description) generic_env



  let pp_species_param ppf params =
    let rec rec_print local_ppf = function
      | [] -> ()
      | param :: rem ->
	  (begin
	  let (p_kind_string, vname) =
	    (match param with
	     | SPAR_in (a, _) -> ("in", a)
	     | SPAR_is (a, _) -> ("is", a)) in
	  Format.fprintf local_ppf "%a %s ..."
	    Sourcify.pp_vname vname p_kind_string  ;
	  if rem <> [] then
	    (begin
	    Format.fprintf local_ppf ",@ " ;
	    rec_print local_ppf rem
	    end)
	  end) in
    if params = [] then ()
    else Format.fprintf ppf " (@[<1>%a@]) " rec_print params



  (* **************************************************************** *)
  (* Format.formatter -> species_field list -> unit                   *)
  (** {b Descr} : Pretty prints a field of species ([species_field]).

      {b Rem} : Not exported outside this module.                     *)
  (* **************************************************************** *)
  let pp_species_methods ppf methods =
    List.iter
      (function
	| SF_sig (vname, ty_scheme) ->
	    Format.fprintf ppf "sig %a : %a@\n"
	      Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
	| SF_let (vname, ty_scheme, _) ->
	    Format.fprintf ppf "let %a : %a@\n"
	      Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
	| SF_let_rec rec_bounds ->
	    (begin
	    match rec_bounds with
	     | [] -> assert false  (* Empty let rec is non sense ! *)
	     | (vname, ty_scheme, _) :: rem ->
		 Format.fprintf ppf "let rec %a : %a@\n"
		   Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme ;
		 List.iter
		   (fun (v, s, _) ->
		     Format.fprintf ppf "and %a : %a@\n"
		       Sourcify.pp_vname v Types.pp_type_scheme s)
		   rem
	    end)
	| SF_theorem (vname, ty_scheme, _, _) ->
	    Format.fprintf ppf "theorem %a : %a@\n"
	      Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
	| SF_property (vname, ty_scheme, _) ->
	    Format.fprintf ppf "property %a : %a@\n"
	      Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme)
      methods



  (* ******************************************************************* *)
  (* Format.formatter -> species_description -> unit                     *)
  (** {b Descr} : Pretty prints a [species_description] as an interface.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let pp_species_description ppf sp_desc =
    Format.fprintf ppf "%a =@\n%aend"
      pp_species_param sp_desc.spe_sig_params
      pp_species_methods sp_desc.spe_sig_methods
end ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



let (scope_find_module, type_find_module, scope_open_module, type_open_module) =
  (* Let's just make the list used to bufferize opened files' content. *)
  (* Because ".fo" files contains always both the scoping and typing   *)
  (* information, once loaded for scoping purpose, the typing info     *)
  (* is made available. Hence, the buffer list contains couples with   *)
  (* no optionnal component.                                           *)
  let buffered =
    ref ([] :
	   (Types.fname * (ScopeInformation.env * TypeInformation.env))
	   list) in



  (* ***************************************************************** *)
  (* loc: Location.t -> Types.fname ->                                 *)
  (*   (ScopeInformation.env * TypeInformation.env)                    *)
  (** {b Descr} : Wrapper to lookup inside an external interface file.
                The lookup also performs a bufferisation to prevent
                futhers calls from accessing again the disk. This
                should enhance lookup speed.

      {b Rem} : Not exported outside this module.                      *)
  (* ***************************************************************** *)
  let internal_find_module ~loc fname =
    (begin
    try List.assoc fname !buffered
    with Not_found ->
      (* The interface was not already loaded... *)
      let fo_name = Files.fo_basename_from_module_name fname in
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
      with Files.Cant_access_file_in_search_path _ ->
	raise (Unbound_module (fname, loc))
    end) in



  (* ******************************************************************** *)
  (* Types.fname -> ('a, 'b, 'c, 'd, 'e) generic_env ->                   *)
  (*   ('a, 'b, 'c, 'd, 'e) generic_env ->                                *)
  (*     ('a, 'b, 'c, 'd, 'e) generic_env                                 *)
  (** {b Descr} : Internal wrapper to extend an environment with bindings
              found in an opened "module"'s environment. Opened bindings
              are transformed in BO_opened before being added in head to
              the initial environment.

      {b Args} :
        - from_fname : The "module" name from where the loaded environment
                     was found.
        - loaded_env : The loaded environment. Not that it shoud contain
                     only [BO_absolute] bindings.
        - env : The environment to extend.

      {b Rem} : Not exported outside this moddule.                        *)
  (* ******************************************************************** *)
  let internal_extend_env from_fname loaded_env env =
    (* Local function to tranform [BO_absolute] into [BO_opened]. Note that   *)
    (* loaded environments should never contain [BO_opened] taggged bindings. *)
    let absolute_to_opened l =
      List.map
	(function
	  | (name, (BO_absolute data)) ->
	      (name, ((BO_opened (from_fname, data))))
	  | (_, (BO_opened (_, _))) -> assert false)
	l in
    let constructors' =
      (absolute_to_opened loaded_env.constructors) @ env.constructors in
    let labels' = (absolute_to_opened loaded_env.labels) @ env.labels in
    let types' = (absolute_to_opened loaded_env.types) @ env.types in
    let values' = (absolute_to_opened loaded_env.values) @ env.values in
    let species' = (absolute_to_opened loaded_env.species) @ env.species in
    { constructors = constructors' ; labels = labels' ; types = types' ;
      values = values' ; species = species' } in



  ((* **************************************************************** *)
   (* scope_find_module                                                *)
   (* loc: Location.t -> current_unit: Types.fname ->                  *)
   (*    Types.fname option -> ScopeInformation.env ->                 *)
   (*      ScopeInformation.env                                        *)
   (** {b Descr} : Wrapper to lookup a scoping environment inside an
                 external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                     *)
   (* **************************************************************** *)
   (fun ~loc ~current_unit fname_opt scope_env ->
    match fname_opt with
     | None -> scope_env
     | Some fname ->
	 if current_unit = fname then scope_env
	 else fst (internal_find_module ~loc fname)),



   (* **************************************************************** *)
   (* type_find_module                                                 *)
   (* loc: Location.t -> current_unit: Types.fname ->                  *)
   (*   Types.fname option -> TypeInformation.env ->                   *)
   (*     TypeInformation.env                                          *)
   (** {b Descr} : Wrapper to lookup a typing environment inside an
                 external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                     *)
   (* **************************************************************** *)
   (fun ~loc ~current_unit fname_opt type_env ->
     match fname_opt with
      | None -> type_env
      | Some fname ->
	  if current_unit = fname then type_env
	  else snd (internal_find_module ~loc fname)),



   (* *************************************************************** *)
   (* scope_open_module                                               *)
   (* loc: Location.t -> Types.fname ->                               *)
   (*   (Types.fname, Types.fname,                                    *)
   (*    ScopeInformation.type_binding_info,                          *)
   (*    ScopeInformation.value_binding_info,                         *)
   (*    ScopeInformation.species_binding_info)                       *)
   (*   generic_env ->                                                *)
   (*     (Types.fname, Types.fname,                                  *)
   (*      ScopeInformation.type_binding_info,                        *)
   (*      ScopeInformation.value_binding_info,                       *)
   (*      ScopeInformation.species_binding_info)                     *)
   (*     generic_env                                                 *)
   (** {b Descr} : Performs a full "open" directive on a scoping
                 environment. It add in head of the environment the
                 bindings found in the "module" content, tagging them
                 as beeing "opened".

       {b Rem} : Exported outside this module.                        *)
   (* *************************************************************** *)
   (fun ~loc fname env ->
     let (loaded_scope_env, _) = internal_find_module ~loc fname in
     internal_extend_env fname loaded_scope_env env),



   (* *************************************************************** *)
   (* type_open_module                                                *)
   (* loc: Location.t -> Types.fname ->                               *)
   (*   (TypeInformation.constructor_description,                     *)
   (*    TypeInformation.label_description,                           *)
   (*    TypeInformation.type_description,                            *)
   (*    Types.type_scheme, TypeInformation.species_description)      *)
   (*   generic_env ->                                                *)
   (*   (TypeInformation.constructor_description,                     *)
   (*    TypeInformation.label_description,                           *)
   (*    TypeInformation.type_description,                            *)
   (*    Types.type_scheme, TypeInformation.species_description)      *)
   (*   generic_env                                                   *)
   (** {b Descr} : Performs a full "open" directive on a typing
                 environment. It add in head of the environment the
                 bindings found in the "module" content, tagging them
                 as beeing "opened".

       {b Rem} : Exported outside this module.                        *)
   (* *************************************************************** *)
   (fun ~loc fname env ->
     let (_, loaded_type_env) = internal_find_module ~loc fname in
     internal_extend_env fname loaded_type_env env)
  )
;;



(* ************************************************************************* *)
(* 'a -> 'a option -> bool                                                   *)
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
  val find_module :
    loc: Location.t -> current_unit: Types.fname -> Types.fname option ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data)
      generic_env ->
        (constructor_bound_data, label_bound_data, type_bound_data,
         value_bound_data, species_bound_data)
      generic_env
  val pervasives : unit ->
    (constructor_bound_data, label_bound_data, type_bound_data,
     value_bound_data, species_bound_data)
      generic_env
  val make_value_env_from_species_methods :
    Types.species_name -> species_bound_data ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data)
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
     EMAccess.species_bound_data) generic_env

  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh TOTALY empty environment
		(no even pervasive stuff inside).

      {b Rem} : Exported outside this module.              *)
  (* ***************************************************** *)
  let empty () =
    ({ constructors = [] ; labels = [] ; types = [] ; values = [] ;
      species = [] } : t)


  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh environment containing
                information bound the basic builtins.

      {b Rem} : Exported outside this module.              *)
  (* ***************************************************** *)
  let pervasives () = EMAccess.pervasives ()



  (* Types.species_name -> EMAccess.species_bound_data -> t -> t *)
  let add_species species_name data (env : t) =
     ({ env with
	  species = (species_name, BO_absolute data) :: env.species } : t)



  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> *)
  (*    t -> EMAccess.species_bound_data                                *)
  let rec find_species ~loc ~current_unit coll_ident (env : t) =
    match coll_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_species_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_species_vname ~loc ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Because species are not first class values,   *)
	 (* species identifiers should never be methods ! *)
	 assert false


  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname -> *)
  (*    t -> EMAccess.species_bound_data                         *)
  and find_species_vname ~loc ~allow_opened vname (env : t) =
    let coll_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened coll_name env.species with
    | Not_found -> raise (Unbound_species (coll_name, loc))



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
  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident ->  *)
  (*   t -> EMAccess.value_bound_data                                    *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let rec find_value ~loc ~current_unit ident_ident (env : t) =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_value_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_value_vname ~loc ~allow_opened vname env'
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
	      find_value_vname ~loc ~allow_opened: false vname env
	  | Some collname ->
	      (* Collections are alwas capitalized so *)
              (* the related [vname] is a [Vuident].  *)
	      let coll_vname = Parsetree.Vuident collname in
	      (* We must first look inside collections and species   *)
              (* for the [collname] in order to recover its methods. *)
              let available_meths =
		(EMAccess.make_value_env_from_species_methods
		   collname
		   (find_species_vname
		      ~loc ~allow_opened: false coll_vname env)) in
	      let data =
		find_value_vname
		  ~loc ~allow_opened: false vname available_meths in
	      (* Now we apply the post-processing on the found data. *)
	      EMAccess.post_process_method_value_binding ~collname data
	 end)



  (* ****************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname -> t ->   *)
  (*   EMAccess.value_bound_data                                        *)
  (** {b Descr} : Looks-up for a [vname] inside the values environment.

      {b Rem} : Not exported outside this module.                       *)
  (* ****************************************************************** *)
  and find_value_vname ~loc ~allow_opened vname (env : t) =
    try (*debug_env_list_assoc*) env_list_assoc ~allow_opened vname env.values
    with Not_found -> raise (Unbound_identifier (vname, loc))



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



  (* ****************************************************************** *)
  (* current_unit: loc: Location.t -> Types.fname -> Parsetree.ident -> *)
  (*   t -> EMAccess.constructor_bound_data                             *)
  (** {b Descr} : Looks-up for an [ident] inside the constructors
		environment.

      {b Rem} : Exported outside this module.                           *)
  (* ****************************************************************** *)
  let rec find_constructor ~loc ~current_unit cstr_ident (env : t) =
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_constructor_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_constructor_vname ~loc ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Don't know what it means if the           *)
	 (* constructor seems to be in fact a method. *)
	 assert false



  (* *********************************************************** *)
  (* allow_opened: loc: Location.t -> bool ->                    *)
  (*   Parsetree.constr_name -> t ->                             *)
  (*   EMAccess.constructor_bound_data                           *)
  (** {b Descr} : Looks-up for a [vname] inside the constructors
                environment.

      {b Rem} : Not exported outside this module.                *)
  (* *********************************************************** *)
  and find_constructor_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.constructors
    with Not_found -> raise (Unbound_constructor (vname, loc))



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
  (* loc: Location.t -> Types.label_name -> > t ->                 *)
  (*   EMAccess.label_bound_data                                   *)
  (** {b Descr} : Looks-up for an [ident] inside the record fields
		labels environment.

      {b Rem} : Exported outside this module.                      *)
  (* ************************************************************* *)
  let find_label ~loc lbl_name (env : t) =
    try
      (* Because labels cannot be written with a # notation, the only   *)
      (* way is to access toplevel labels of the current compilation    *)
      (* unit. Hence such bindings cannot be induced by opened modules. *)
      env_list_assoc ~allow_opened: false lbl_name env.labels with
    | Not_found -> raise (Unbound_label (lbl_name, loc))



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
  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> *)
  (*   t -> EMAccess.type_bound_data                                    *)
  (** {b Descr} : Looks-up for an [ident] inside the types environment.

      {b Rem} : Exported outside this module.                           *)
  (* ****************************************************************** *)
  let rec find_type ~loc ~current_unit type_ident (env : t) =
    match type_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_type_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_type_vname ~loc ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Type identifiers should never be methods ! *)
	 assert false



  (* **************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname ->      *)
  (*   Parsetree.vname -> t -> EMAccess.type_bound_data               *)
  (** {b Descr} : Looks-up for a [vname] inside the types environment.

      {b Rem} : Not exported outside this module.                     *)
  (* **************************************************************** *)
  and find_type_vname ~loc ~allow_opened vname (env : t) =
    let type_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened type_name env.types with
    | Not_found -> raise (Unbound_type (type_name, loc))
end ;;



module ScopingEMAccess = struct
  type constructor_bound_data = Types.fname
  type label_bound_data = Types.fname
  type type_bound_data = ScopeInformation.type_binding_info
  type value_bound_data = ScopeInformation.value_binding_info
  type species_bound_data = ScopeInformation.species_binding_info

  let find_module = scope_find_module
  let pervasives () =
    { constructors = [
        (Parsetree.Vlident "[]", BO_opened ("basics", "basics")) ;
        (Parsetree.Viident "::", BO_opened ("basics", "basics"))
        ] ;
      labels = [] ;
      types = [
        ("list",
	 BO_opened
	   ("", ScopeInformation.TBI_builtin_or_var))
        ] ;
      values = [] ;
      species = [] }


  let make_value_env_from_species_methods spec_name spec_info =
    let values_bucket =
      List.map
	(fun meth_vname ->
	  (meth_vname,
	   BO_absolute (ScopeInformation.SBI_method_of_coll spec_name)))
	spec_info.ScopeInformation.spbi_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] }



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
    (* Create the description of the type list. *)
    let (list_identity, list_params) =
      (let v = Types.type_variable () in
      Types.generalize2 (Types.type_basic "list" [v]) [v]) in
    let list_type_description = {
      TypeInformation.type_kind =
        TypeInformation.TK_variant [
          (Parsetree.Vlident "[]", nil_scheme) ;
          (Parsetree.Viident "::", cons_scheme) ] ;
      TypeInformation.type_identity = list_identity ;
      TypeInformation.type_params = list_params ;
      TypeInformation.type_arity = 1 } in
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
       ("list",	BO_opened ("basics", list_type_description))
      ] ;
    values = [] ;
    species = [] }



  (* ******************************************************************** *)
  (* 'a -> TypeInformation.species_description ->                         *)
  (*   ('b, 'c, 'd, Types.type_scheme, 'e) generic_env                    *)
  (** {b Descr} : Create a fresh environment with the methods, properties
                and theorems of the species called [spec_name] from the
                 methods found in [spec_info].

      {b Rem} : Not exported outside this module.                         *)
  (* ******************************************************************** *)
  let make_value_env_from_species_methods spec_name spec_info =
    (* By folding left, fields at the head of the list will be at the tail *)
    (* of the environment list. Hence, methods seen first are inserted     *)
    (* first, hence are deeper in the environment.                         *)
    let values_bucket =
      List.fold_left
	(fun accu field ->
	  match field with
	   | TypeInformation.SF_sig (v, s)
	   | TypeInformation.SF_let (v, s, _)
	   | TypeInformation.SF_theorem (v, s, _, _)
	   | TypeInformation.SF_property (v, s, _) ->
	       [(v, (BO_absolute s))] @ accu
	   | TypeInformation.SF_let_rec l ->
	       let l' = List.map (fun (v, s, _) -> (v, (BO_absolute s))) l in
	       l' @ accu)
	[]
	spec_info.TypeInformation.spe_sig_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] }



  (* Not yet thought about. *)
  let post_process_method_value_binding ~collname data = data
end ;;
module TypingEnv = Make (TypingEMAccess) ;;



(* **************************************************************** *)
(* source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t ->   *)
(*    unit                                                          *)
(** {b Descr} : Create the "fo file" on disk related to the current
              compilation unit.
              This "fo file" contains :
                - A magic number.
                - A couple (scoping env * typing env).

    {b Rem} : Exported outside this module.                         *)
(* **************************************************************** *)
let make_fo_file ~source_filename scoping_toplevel_env typing_toplevel_env =
  (* First, recover from the scoping environment only bindings *)
  (* coming from definitions of our current compilation unit.  *)
  let scoping_toplevel_env' =
    env_from_only_absolute_bindings scoping_toplevel_env in
  (* Next, recover from the typing environment only bindings *)
  (* coming from definitions of our current compilation unit.  *)
  let typing_toplevel_env' =
    env_from_only_absolute_bindings typing_toplevel_env in
  let module_name = Filename.chop_extension source_filename in
  let fo_basename = Files.fo_basename_from_module_name module_name in
  (* Add to the module name the path of the currently compiled source *)
  (* file in order to make the ".fo" lying at the same place than the *)
  (* ".foc" file.                                                     *)
  let with_path =
    Filename.concat (Filename.dirname source_filename) fo_basename in
  let out_hd = open_out_bin with_path in
  (* First, write the magic number of the file. *)
  Files.write_magic out_hd Files.fo_magic  ;
  (* And now the filtered environments. *)
  output_value out_hd (scoping_toplevel_env', typing_toplevel_env') ;
  (* Just don't forget to close the output file... *)
  close_out out_hd
;;
