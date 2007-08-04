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


(* $Id: env.ml,v 1.7 2007-08-04 10:17:17 pessaux Exp $ *)

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
exception Invalid_constructor_identifier of Parsetree.ident ;;
exception Unbound_label of Types.label_name ;;
exception Unbound_identifier of Parsetree.vname ;;
exception Unbound_type of Types.type_name ;;
exception Unbound_module of Parsetree.fname ;;



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
  (* type scope_binding_info                                               *)
  (** {b Descr} : Tag each binding in the scopping environment in order to
	      know if the [ident] is currently bound to a global toplevel
	      definition inside a file or if it's a method found in the
	      current species inheritance tree (including itself).
	      Note that there is no need to add another case like a
	      'SBI_method_of_specie' because if a method has to be called
	      from a particular species other than [Self], then it has to
	      be syntactically explicitely written !

      {b Rem} : Not exported outside this module.                          *)
  (* ********************************************************************* *)
  type scope_binding_info =
      (* The ident is at toplevel in a file (if Some) or *)
      (* at the toplevel of the current file (if None) . *)
    | SBI_file of Parsetree.fname option
      (* The ident is a method implicitely of self. *)
    | SBI_method_of_self
      (* The ident is a method explicitely of a collection. *)
    | SBI_method_of_coll of Types.collection_name
      (* The ident is a locally bound indentifier (let or function parameter. *)
    | SBI_local



  type t = {
    constructors :
      (Parsetree.constr_name * (Parsetree.fname binding_origin)) list ;
    values : (Parsetree.vname * (scope_binding_info binding_origin)) list
  }
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
      (string * Types.type_simple * (Parsetree.expr option)) list
    }

  type collections_sig = (string * Types.type_simple) list (* To refine. *)

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


  type t = {
    constructors :
      (Parsetree.constr_name * (constructor_description binding_origin)) list ;
    labels : (Types.label_name * (label_description binding_origin)) list ;
    types : (Types.type_name * (type_description binding_origin)) list ;
    (** [idents] Contains functions methods and more generally any let-bound
identifiers. *)
    values : (Parsetree.vname * (Types.type_scheme binding_origin)) list ;
    species : (Types.species_name * (species_description binding_origin)) list ;
    collections :
      (Types.collection_name * (collections_sig binding_origin)) list
  }

end ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



let (scope_find_module, type_find_module) =
  let buffered =
    ref ([] :
	   (Parsetree.fname * (ScopeInformation.t * TypeInformation.t)) list) in
  (* ***************************************************************** *)
  (* Parsetree.fname -> (ScopeInformation.t * TypeInformation.t)       *)
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
  ((* ****************************************************************** *)
   (* Parsetree.fname option -> ScopeInformation.t -> ScopeInformation.t *)
   (** {b Descr} : Wrapper to lookup a scoping environment inside an
                 external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                       *)
   (* ****************************************************************** *)
   (fun ~current_unit fname_opt scope_env ->
    match fname_opt with
     | None -> scope_env
     | Some fname ->
	 if current_unit = fname then scope_env
	 else fst (internal_find_module fname)),
   (* **************************************************************** *)
   (* Parsetree.fname option -> TypeInformation.t -> TypeInformation.t *)
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



(* ********************************************************************** *)
(** {b Descr} : This module contains the scoping environment manipulation
              primitives (lookup and insertion).

    {b Rem} : This module is partially exported.                          *)
(* ********************************************************************** *)
module ScopingEnv = struct
  (* ************************************************************* *)
  (* empty : unit -> ScopeInformation.t                            *)
  (** {b Descr} : Creates a fresh TOTALY empty scoping environment
		(no even pervasive stuff inside).

      {b Rem} : Exported outside this module.                      *)
  (* ************************************************************* *)
  let empty () =
    { ScopeInformation.constructors = [] ;
      ScopeInformation.values = [] }



  let add_value ident scope_binding_info env =
    { env with
        ScopeInformation.values =
          (ident, BO_absolute scope_binding_info) ::
          env.ScopeInformation.values }


  (* ******************************************************************* *)
  (* Parsetree.fname -> Parsetree.ident -> ScopeInformation.t ->         *)
  (*   (ScopeInformation.scope_binding_info * Parsetree.vname)           *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let rec find_value ~current_unit ident_ident env =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_value_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = scope_find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_value_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) -> failwith "todo1"



  (* ****************************************************************** *)
  (* bool -> Parsetree.vname -> ScopeInformation.t ->                   *)
  (*   (ScopeInformation.scope_binding_info * Parsetree.vname)          *)
  (** {b Descr} : Looks-up for a [vname] inside the values environment.

      {b Rem} : Not exported outside this module.                       *)
  (* ****************************************************************** *)
  and find_value_vname ~allow_opened vname env =
    try
      let hosting_info =
        env_list_assoc ~allow_opened vname env.ScopeInformation.values in
      (hosting_info, vname)
    with Not_found -> raise (Unbound_identifier vname)



  let rec find_constructor ~current_unit cstr_ident env =
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_constructor_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = scope_find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_constructor_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Don't know what it means if the           *)
	 (* constructor seems to be in fact a method. *)
	 raise (Invalid_constructor_identifier cstr_ident)



  and find_constructor_vname ~allow_opened vname env =
    try
      let host_file =
        env_list_assoc ~allow_opened vname env.ScopeInformation.constructors in
      (host_file, vname)
    with Not_found -> raise (Unbound_constructor vname)

end ;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* ********************************************************************* *)
(** {b Descr} : This module contains the typing environment manipulation
              primitives (lookup and insertion).

    {b Rem} : This module is partially exported.                         *)
(* ********************************************************************* *)
module TypingEnv = struct
  (* ************************************************************ *)
  (* empty : unit -> TypeInformation.t                            *)
  (** {b Descr} : Creates a fresh TOTALY empty typing environment
		(no even pervasive stuff inside).

      {b Rem} : Exported outside this module.                     *)
  (* ************************************************************ *)
  let empty () =
    { TypeInformation.constructors = [] ; TypeInformation.labels = [] ;
      TypeInformation.types  = [] ; TypeInformation.values = [] ;
      TypeInformation.species = [] ;
      TypeInformation.collections = [] }



  (* ************************************************************ *)
  (* pervasives : unit -> TypeInformation.t                       *)
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
     TypeInformation.constructors = [
       (Parsetree.Vlident "[]",
	BO_opened
	  ("", { TypeInformation.cstr_arity = TypeInformation.CA_zero ;
		 TypeInformation.cstr_scheme = nil_scheme })) ;
	(Parsetree.Viident "::",
	 BO_opened
	   ("", { TypeInformation.cstr_arity = TypeInformation.CA_one ;
		  TypeInformation.cstr_scheme = cons_scheme }))
	] ;
     TypeInformation.labels = [] ;
     TypeInformation.types = [
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
	  ("", { TypeInformation.type_kind =
		   TypeInformation.TK_variant [
		     (Parsetree.Vlident "[]", nil_scheme) ;
		     (Parsetree.Viident "::", cons_scheme) ] ;
		 TypeInformation.type_identity =
		   (let v = Types.type_variable () in
		   Types.generalize (Types.type_basic "list" [v])) ;
		 TypeInformation.type_arity = 1 }))
      ] ;
    TypeInformation.values = [] ;
    TypeInformation.species = [] ;
    TypeInformation.collections = []
  }



  let add_constructor cstr_name ty_scheme env =
    { env with
        TypeInformation.constructors =
          (cstr_name, BO_absolute ty_scheme) ::
          env.TypeInformation.constructors }



  (* ************************************************************ *)
  (* Parsetree.fname -> Parsetree.ident -> TypeInformation.t ->   *)
  (*   TypeInformation.constructor_description                    *)
  (** {b Descr} : Looks-up for an [ident] inside the constructors
		environment.

      {b Rem} : Exported outside this module.                     *)
  (* ************************************************************ *)
  let rec find_constructor ~current_unit cstr_ident env =
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_constructor_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = type_find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_constructor_vname ~allow_opened  vname env'
     | Parsetree.I_method (_, _) ->
	 (* Don't know what it means if the           *)
	 (* constructor seems to be in fact a method. *)
	 raise (Invalid_constructor_identifier cstr_ident)



  (* *********************************************************** *)
  (* bool -> Parsetree.vname -> TypeInformation.t ->             *)
  (*   TypeInformation.constructor_description                   *)
  (** {b Descr} : Looks-up for a [vname] inside the constructors
                environment.

      {b Rem} : Not exported outside this module.                *)
  (* *********************************************************** *)
  and find_constructor_vname ~allow_opened vname env =
    try env_list_assoc ~allow_opened vname env.TypeInformation.constructors with
    | Not_found -> raise (Unbound_constructor vname)



  let add_label lbl_name lbl_descr env =
     { env with
         TypeInformation.labels =
           (lbl_name, BO_absolute lbl_descr) :: env.TypeInformation.labels }



  let find_label lbl_name env =
    try
      (* Because labels cannot be written with a # notation, the only   *)
      (* way is to access toplevel labels of the current compilation    *)
      (* unit. Hence such bindings cannot be induced by opened modules. *)
      env_list_assoc
	~allow_opened: false lbl_name env.TypeInformation.labels with
    | Not_found -> raise (Unbound_label lbl_name)



  (* ************************************************************** *)
  (* Parsetree.vname -> Types.type_scheme -> TypeInformation.t ->   *)
  (*   TypeInformation.t                                            *)
  (** {b Descr} : Return an environment extended with a binding
                between a value [ident] and its [type_scheme].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_value ident ty_scheme env =
    { env with
        TypeInformation.values =
          (ident, BO_absolute ty_scheme) :: env.TypeInformation.values }



  (* ******************************************************************* *)
  (* Parsetree.fname -> Parsetree.ident -> TypeInformation.t ->          *)
  (*   Types.type_scheme                                                 *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.
		Hence, expects finding a first-class bound value.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let rec find_value ~current_unit ident_ident env =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_value_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = type_find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_value_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) -> failwith "todo1"



  (* ****************************************************************** *)
  (* bool -> Parsetree.vname -> TypeInformation.t -> Types.type_scheme  *)
  (** {b Descr} : Looks-up for a [vname] inside the values environment.
		Hence, expects finding a first-class bound value.

      {b Rem} : Not exported outside this module.                       *)
  (* ****************************************************************** *)
  and find_value_vname ~allow_opened vname env =
    try env_list_assoc ~allow_opened vname env.TypeInformation.values with
    | Not_found -> raise (Unbound_identifier vname)



  (* ************************************************************** *)
  (* Types.type_name -> TypeInformation.type_description ->         *)
  (*   TypeInformation.t -> TypeInformation.t                       *)
  (** {b Descr} : Return an environment extended with a binding
                between a type [ident] and its [type_description].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_type tyname ty_descr env =
    { env with
        TypeInformation.types =
          (tyname, BO_absolute ty_descr) :: env.TypeInformation.types }



  (* ****************************************************************** *)
  (* Parsetree.fname -> Parsetree.ident -> TypeInformation.t ->         *)
  (*   TypeInformation.type_description                                 *)
  (** {b Descr} : Looks-up for an [ident] inside the types environment.

      {b Rem} : Exported outside this module.                           *)
  (* ****************************************************************** *)
  let rec find_type ~current_unit type_ident env =
    match type_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
	 find_type_vname ~allow_opened: true vname env
     | Parsetree.I_global (opt_scope, vname) ->
	 let env' = type_find_module ~current_unit opt_scope env in
	 (* Check if the lookup can return something *)
	 (* coming from an opened module.            *)
	 let allow_opened = allow_opened_p current_unit opt_scope in
	 find_type_vname ~allow_opened vname env'
     | Parsetree.I_method (_, _) ->
	 (* Type identifiers should never be methods ! *)
	 assert false



  (* ***************************************************************** *)
  (* bool -> Parsetree.vname -> TypeInformation.t ->                   *)
  (*   TypeInformation.type_description                                *)
  (** {b Descr} : Looks-up for a [vname] inside the types environment.

      {b Rem} : Not exported outside this module.                      *)
  (* ***************************************************************** *)
  and find_type_vname ~allow_opened vname env =
    let type_name = Parsetree_utils.name_of_vname vname in
    try env_list_assoc ~allow_opened type_name env.TypeInformation.types with
    | Not_found -> raise (Unbound_type type_name)

end ;;

