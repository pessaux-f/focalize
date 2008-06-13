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


(* $Id: env.ml,v 1.88 2008-06-13 15:47:17 pessaux Exp $ *)

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



exception Unbound_constructor of (Parsetree.vname * Location.t);;
exception Unbound_label of (Parsetree.vname * Location.t);;
exception Unbound_identifier of (Parsetree.vname * Location.t);;
exception Unbound_type of (Parsetree.vname * Location.t);;
exception Unbound_module of (Types.fname * Location.t);;
exception Unbound_species of (Parsetree.vname * Location.t);;

exception Rebound_type of (Parsetree.vname * Location.t);;
exception Rebound_species of (Parsetree.vname * Location.t);;



(* ******************************************************************** *)
(** {b Desc} : Enables to differentiate environment bindings induced by
             definitions really present in the current compilation unit
             and bindings introduced by a "open" directive.

    {b Rem} : Not exported outside this module.                         *)
(* ******************************************************************** *)
type 'a binding_origin =
    (** The binding comes from the current compilation unit. *)
  | BO_absolute of 'a
    (** The binding was inserted by a "open" directive of the file in
        argument. *)
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



(* Mostly for debug purpose.
let debug_env_list_assoc ~allow_opened searched list =
  let vname_as_string v =
    (match v with
     | Parsetree.Vlident n -> "Vlident " ^ n
     | Parsetree.Vuident n -> "Vuident " ^ n
     | Parsetree.Vpident n -> "Vpident " ^ n
     | Parsetree.Viident n -> "Viident " ^ n
     | Parsetree.Vqident n -> "Vqident " ^ n) in
  let rec rec_assoc = function
    | [] ->
        Format.eprintf "\nSearch failed.\n";
        flush stderr;
        raise Not_found
    | (name, data) :: q ->
        Format.eprintf "\"%s\" " (vname_as_string name);
        flush stderr;
        if name = searched then
          (begin
          match data with
           | BO_opened (fname, v) ->
               if allow_opened then
                 (begin
                 Format.eprintf
                   "@\nSearch successfully ends on opened (\"%s\")...\n" fname;
                 flush stderr ;
                 v
                 end)
               else rec_assoc q
           | BO_absolute v ->
               Format.eprintf "@\nSearch successfully ends on absolute...\n";
               flush stderr ;
               v
          end)
        else rec_assoc q in
  Format.eprintf
    "Search starts for \"%s\"... with allow_opened = %b\n"
    (vname_as_string searched) allow_opened ;
  flush stderr;
  rec_assoc list
;;
*)



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
  constructors : (Parsetree.constructor_name * ('constrs binding_origin)) list;
  labels : (Parsetree.vname * ('labels binding_origin)) list;
  types : (Parsetree.vname * ('types binding_origin)) list;
  (** [idents] Contains functions methods and more generally any let-bound
identifiers. *)
  values : (Parsetree.vname * ('values binding_origin)) list;
  (** [species] Contains both species and collections. *)
  species : (Parsetree.vname * ('species binding_origin)) list
}
;;



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



(* ***************************************************************** *)
(** {b Descr} : Describes from where a method comes, keeping all the
    inheritance steps in memory.
    For instance:
      species Foo0 (A0 is Sp0) inherits ... = let v = 1 end ;;
      species Foo1 (A1 is Sp1) inherits Foo0 (A1) = let v = 2 end ;;
      species Foo2 (A2 is Sp1) inherits Foo1 (A2) = end ;;
      species Foo3 (A3 is Sp1, A4 is A3) inherits Foo2 (A4) = end ;;
    For method "v" in "Foo3" :
      [fh_initial_apparition] : Foo1
      [fh_inherited_along] : [(Foo3, Foo2(A4)); (Foo2, Foo1 (A2))]
    {Rem} : Exported outside this module.                            *)
(* ***************************************************************** *)
type from_history = {
  (** The species where the method was defined or declared for the first time.
      whithout redefinition. All inheritance information prior to a possible
      redefinition in this species is discarded. *)
  fh_initial_apparition : Parsetree.qualified_species ;
  (** The list of species inherited along which the method was not redefined.
      In head of the list is the most recently inherited species, and in tail
      are the least recents. There is never overlaping between fields
      [fh_inherited_along] and [fh_initial_apparition]. This means that if the
      method is just defined in the current species, then [fh_inherited_along]
      is empty. For each species in the history, we have the simplified
      species expression used to build the species we inherited of. *)
  fh_inherited_along :
    (Parsetree.qualified_species * Parsetree_utils.simple_species_expr) list
} ;;



(* ***************************************************************** *)
(** {b Descr} : Create an inheritance history by setting the initial
    apparition of a method.

    {Rem} : Exported outside this module.                            *)
(* ***************************************************************** *)
let intitial_inheritance_history species =
  { fh_initial_apparition = species ;
    fh_inherited_along = [] }
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
      (** The ident is at toplevel of a file (including the current file). *)
    | SBI_file of Types.fname
      (** The ident is a method implicitely of self. *)
    | SBI_method_of_self
      (** The ident is a method explicitly of a collection. ATTENTION: while
          inserting a method in the environment, it must always be tagged with
          [SBI_method_of_self]. The tag [SBI_method_of_coll] can only be
          returned by [find_value] who may perform a change on the fly if
          required. *)
    | SBI_method_of_coll of
        Parsetree.qualified_vname (** The module name hosting the collection
              and the collection the
            method belongs to. *)
      (** The ident is a locally bound indentifier
          (let or function parameter). *)
    | SBI_local



  type type_binding_info =
      (** The type identifier is either a type variable name
          ('a for instance) or a builtin type (int for instance). *)
    | TBI_builtin_or_var
      (** The identifier is a type name defined at toplevel in a file. *)
    | TBI_defined_in of Types.fname



  type species_scope =
      (** The identifier is a specied name defined at toplevel in a file. *)
    | SPBI_file of Types.fname
      (** The identifier is a locally bound collection like in the case of a
          "is"-bound parameter (i.e. [c is ...]) where [c] is then considered
          as a local collection). *)
    | SPBI_local



  type species_parameter_kind =
    | SPK_in       (** Parameter is an entity parameter ("in"). *)
    | SPK_is       (** Parameter is a collection parameter ("is"). *)



  type species_binding_info = {
    (** The list of *ALL* the method owned, including those added by
        inheritance. Methods from the toplevel ancestor are in head of the
        list. In case of multiple inheritance, we consider that ancestors
        are older from left to right. *)
    spbi_methods : Parsetree.vname list ;
    (** The kind of the species's parameters. The first kind of the list is
        the kind of the first parameter of the species and so on. This
        information is required in order to properly scope effective arguments
        of a species "application". In effect, "in" parameters are first-class
        expressions and must be scoped this way, although "is" parameters are
        collection names and because they are structurally (in the parsetree)
        encapsulated in a first-class expression, they must be restricted to
        only syntactic sum-type contructors (yep, species names are
        capitalized then look like a sum type constructor seen from a
        first-class expression point of view) and looked-up into the species
        field of the scoping environment. *)
    spbi_params_kind : species_parameter_kind list ;
    (** The information telling how the species is bound (i.e. "scoped"). *)
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
end
;;



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
  (* ************************************************************** *)
  (** {Descr} : Records if a method has dependencies on the carrier
        representation "rep".
      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  type dependency_on_rep = {
    dor_def : bool  ;  (** Flag for a def-dependency. *)
    dor_decl : bool }  (** Flag for a decl-dependency. *)



  type sig_field_info =
    ((** Where the sig comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *          (** The sig's name. *)
     Types.type_scheme)         (** The sig's type scheme. *)



  type let_field_info =
    ((** Where the let-bound comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *       (** Name of the let-bound definition. *)
     (** Parameters of the let-bound definition. *)
     (Parsetree.vname list) *
     Types.type_scheme *       (** Type scheme of the let-bound definition. *)
     Parsetree.binding_body *  (** Body of the let-bound definition. *)
     (** Tells if the method has dependencies on the carrier ("rep"). *)
     dependency_on_rep *
     Parsetree.logical_flag)   (** Tells if the let-bound idnetifier is a
                                   logical or a computational definition. *)



  type theorem_field_info =
    ((** Where the theorem comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *         (** The theorem's name. *)
     Types.type_scheme *       (** The theorem's type scheme. *)
     Parsetree.logical_expr *  (** The theorem's body. *)
     Parsetree.proof *         (** The theorem's proof. *)
     (** Tells if the theorem has dependencies on the carrier ("rep"). *)
     dependency_on_rep)



  type property_field_info =
    ((** Where the property comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *         (** The property's name. *)
     Types.type_scheme *       (** The property's type scheme. *)
     Parsetree.logical_expr *  (** The property's body. *)
     (** Tells if the property has dependencies on the carrier ("rep"). *)
     dependency_on_rep)



  type species_param =
    (** Entity parameter. *)
    | SPAR_in of (Parsetree.vname * Types.type_collection)
    (** Collection parameter. *)
    | SPAR_is of
        ((** The module and name of the species parameter. Indeed, that
             is the parameter's type-collection. *)
         Types.type_collection *
         (** The list of fields this parameter has. It is in fact the normalized
             form of the species type the parameter has. *)
         (species_field list) *
         (** The species expression of the parameter. This expression has the
             normalized form compound of the above list of fields. This
             expression is kept because Coq code generation need to know it
             in order to make the type expression annotation the parameter
             in the hosting species record type. *)
          Parsetree_utils.simple_species_expr)



  (* ************************************************************************ *)
  (** {b Desc} : Describe the essence of a species field, i.e. if it's
               a signature, a let-binding, let let-rec-binding, a theorem
               or a property. Through this description the name, type, body
               and provenance (and even more if needed to fully describe the
               field according to its nature) of the field is made available.

      {b Rem} : Exported outside this module.                                 *)
  (* ************************************************************************ *)
  and species_field =
    | SF_sig of sig_field_info
    | SF_let of let_field_info
    | SF_let_rec of let_field_info list   (** The list of information similar
                                              to what can be found for a
                                              [SF_let], but for each mutually
                                              recursive bound identifier. *)
    | SF_theorem of theorem_field_info
    | SF_property of property_field_info



  (* ******************************************************************** *)
  (** {b Desc} : Describe the essence of a species or collection. This
               description contains a flag telling if ti's a species or
               a collection, the possible parameters of the species and a
               link to all its fields.

      {b Rem} : Exported outside this module.                             *)
  (* ******************************************************************** *)
  type species_description = {
    spe_is_collection : bool ;  (** Whether the species is a collection. *)
    spe_is_closed : bool ;   (** Whether the species is fully defined, even if
         not turned into a collection. This information
         will be useful to known when collection
         generators must be created. *)
    spe_sig_params : species_param list ;   (** Species parameters. *)
    (** Method's name, type and body if defined. *)
    spe_sig_methods : species_field list
    }



  (* ********************************************************************* *)
  (** {b Descr} : Because sum-type constructors are considered either with
                not parameter or with only ONE parameter (that can be a
                tuple), the arity of such a sum-type constructor is
                only given by the 2 following values.

      {b Rem} : Exported outside this module.                              *)
  (* ********************************************************************* *)
  type constructor_arity =
    | CA_zero   (** Constructor has no argument. *)
    | CA_one    (** Constructor has argument(s). *)



  (* ********************************************************************* *)
  (** {b Descr} : Description of a sum-type constructor. Contains it's
                arity and its type scheme. A constructor has a functionnal
                type whose "argument" is the tuple of types of the
                constructor's argument and whose "result" has the same
                type than the type the constructor belongs to.
                For instance: [type t = Foo of (int * char)] will lead
                to the constructor [Foo : (int * char) -> t].
                In the degenerated case of a constructor with only one
                effective argument, the type of the argument will be
                a tuple with only 1 component.
                For instance: [type u = Bar of int] will lead to the
                constructor [Bar : (int) -> u].

      {b Rem} : Exported outside this module.                              *)
  (* ********************************************************************* *)
  type constructor_description = {
    (** Arity : 0 or 1 (many = 1 type tuple), (1 = type, not a 1 tuple). *)
    cstr_arity : constructor_arity ;
    (** Full type scheme for this constructor, i.e (args ->) ty result. *)
    cstr_scheme : Types.type_scheme
  }



  (* ***************************************************************** *)
  (** {b Descr} : Tells is a record field is "mutable" (i.e. can be
                modified physically in place) or not.

      {b Rem} : Exported outside this module.
              Not yet used in FoC. Just there in case... Currently all
              record fields are non-mutable.                           *)
  (* ***************************************************************** *)
  type field_mutability = FM_mutable | FM_immutable



  type label_description = {
    field_mut : field_mutability ;    (** Mutability for this field. *)
    (** Full type scheme for this field, i.e arg -> ty result. *)
    field_scheme : Types.type_scheme
  }



  type type_kind =
    | TK_abstract       (** Abstract types and type abbreviations. *)
    | TK_external of    (** Abstract types externally defined. *)
        (Parsetree.external_expr *   (** On what to map the type constructor in
                                         the external languages. *)
         Parsetree.external_bindings)  (** On what to map the possible sum
                                           constructors or field labels. *)
    | TK_variant of     (** Sum types. *)
        (Parsetree.constructor_name *   (** Name of sum contructor. *)
         constructor_arity *            (** Arity of sum contructor. *)
         (** Type scheme of sum constructor. A parametrised constructor has
             a functionnal type taking a tuple and returning the type hosting
             the constructor. The tuple is compound of all the arguments the
             constructor has. *)
         Types.type_scheme)
        list                            (** ... and this for all constructor of
                                            this type. *)
    | TK_record of  (** Record types: list of labels. Any value of a type
        record will be typed as a [ST_construct] whose name is
        the name of the record type. *)
        (Parsetree.vname * field_mutability * Types.type_scheme) list



  type type_description = {
    type_loc : Location.t ;     (** The type definition's location. *)
    type_kind : type_kind ;             (** Kind of the type definition. *)
    (** The type scheme representing to what this type is equal to. For
        instance in type 'a t = 'a list, t is TK_abstract with [type_identity]
        representing 'a list.
        If the type is a pure abstract like in type t, then t is TK_abstract
        with [type_identity] representing the type ST_construct ("t", []). *)
    type_identity : Types.type_scheme ;
    (** Parameters of the type. Be careful, they are generalized at the same
        that the above scheme [type_identity] is created. Hence, physical
        sharing exists and is crucial ! *)
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
          (match param with
           | SPAR_in (a, _) ->
               Format.fprintf local_ppf "%a in ..." Sourcify.pp_vname a
           | SPAR_is ((modname, param_name), _, sp_expr) ->
               Format.fprintf local_ppf "%s.%s is %a" modname param_name
		 Sourcify.pp_simple_species_expr sp_expr) ;
          if rem <> [] then
            (begin
            Format.fprintf local_ppf ",@ ";
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
        | SF_sig (from, vname, ty_scheme) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition ;
            Format.fprintf ppf "sig %a : %a@\n"
              Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
        | SF_let (from, vname, _, ty_scheme, _, _, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition ;
            Format.fprintf ppf "let %a : %a@\n"
              Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
        | SF_let_rec rec_bounds ->
            (begin
            match rec_bounds with
             | [] -> assert false  (* Empty let rec is non sense ! *)
             | (from, vname, _, ty_scheme, _, _, _) :: rem ->
                 Format.fprintf ppf "(* From species %a. *)@\n"
                   Sourcify.pp_qualified_species from.fh_initial_apparition ;
                 Format.fprintf ppf "let rec %a : %a@\n"
                   Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme;
                 List.iter
                   (fun (local_from, v, _, s, _, _, _) ->
                     Format.fprintf ppf
                       "(* From species %a. *)@\n"
                       Sourcify.pp_qualified_species
                       local_from.fh_initial_apparition ;
                     Format.fprintf ppf "and %a : %a@\n"
                       Sourcify.pp_vname v Types.pp_type_scheme s)
                   rem
            end)
        | SF_theorem (from, vname, _, body, _, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition ;
            Format.fprintf ppf "theorem %a : %a@\n"
              Sourcify.pp_vname vname Sourcify.pp_logical_expr body
        | SF_property (from, vname, _, body, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition ;
            Format.fprintf ppf "property %a : %a@\n"
              Sourcify.pp_vname vname Sourcify.pp_logical_expr body)
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
end
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
(* {b Descr} : Tells if a "species" found in a code generation environment
     is a species or a collection. This is used to adapt the access to
     method generators when creating collection generators.

   {b Rem} : Exported outside this module.                                 *)
(* *********************************************************************** *)
type collection_or_species =
  | COS_collection
  | COS_species
;;



module MlGenInformation = struct
  type collection_generator_info = {
    (** The list of species parameters names and kinds the species whose
        collection generator belongs to has. This list is positionnal, i.e.
        that the first name of the list is the name of the first species
        parameter and so on. *)
    cgi_implemented_species_params_names :
      (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
    (** The list mapping for each parameter name, the set of methods the
        collection generator depends on, hence must be provided an instance
        to be used. Note that the list is not guaranted to be ordered
        according to the order of the species parameters names (that's why
        we have the information about this order given in
        [species_binding_info]). *)
    cgi_generator_parameters :
      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list
  }

  type method_info = {
    mi_name : Parsetree.vname ;        (** The field name. *)
    mi_dependencies_from_parameters :
      ((** The positional list of methods from the species parameters
           abstracted by lambda-lifting. *)
       TypeInformation.species_param *
       (* The set of methods of this parameter on which we have dependencies. *)
       Parsetree_utils.DepNameSet.t) list ;
    mi_abstracted_methods : Parsetree.vname list   (** The positional list
        of methods from ourselves abstracted by lambda-lifting. *)
    }

  type species_binding_info =
    ((** The list of species parameters of the species with their kind. *)
     (TypeInformation.species_param list) *
     (** The list of methods the species has. *)
     (method_info list) *
     (** Optionnal because species that are non fully defined do not have
         any collection generator although they are entered in the
         environment. *)
     (collection_generator_info option) *
     (** Tells if the info is bound to a species or a collection. *)
     collection_or_species)


  (** The list of mappings according to external languages to know to which
      string the record type field name corresponds. *)
  type label_mapping_info = Parsetree.external_expr_desc

  (** The list of mappings according to external languages to know to which
      string the sum type constructor corresponds. For instance, in Caml,
      "Nil" will be mapped onto "[]" and "Cons" to "( :: )".  For Ocaml,
      only constructors coming from "external" sum types are entered in the
      generation environment. Hence, if a constructor is not found, then this
      means that it come from a regular FoCaL type definition, not dealing
      with any external material. *)
  type constructor_mapping_info = Parsetree.external_expr_desc


  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
                scoping environments.

      {b Rem} : Not exported outside this module.                   *)
  (* ************************************************************** *)
  type env =
    (constructor_mapping_info, label_mapping_info, unit, unit,
     species_binding_info) generic_env
end
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)
module CoqGenInformation = struct
  type collection_generator_info = {
    (** The list of species parameters names and kinds the species whose
        collection generator belongs to has. This list is positionnal, i.e.
        that the first name of the list is the name of the first species
        parameter and so on. *)
    cgi_implemented_species_params_names :
      (Parsetree.vname * ScopeInformation.species_parameter_kind) list ;
    (** The list mapping for each parameter name, the set of methods the
        collection generator depends on, hence must be provided an instance
        to be used. Note that the list is not guaranted to be ordered
        according to the order of the species parameters names (that's why
        we have the information about this order given in
        [species_binding_info]). *)
    cgi_generator_parameters :
      (Parsetree.vname * Parsetree_utils.DepNameSet.t) list
  }

  (** In Coq generation environment ALL the sum types value constructors are
      entered in the environment because we always need to know their number
      of extra leading "_" due to polymorphics. If the constructor does not
      have an external mapping, we simply put "None" in the field
      [cmi_external_expr]. *)
  type constructor_mapping_info = {
    (** The number of extra argument the constructor has due to its
        polymorphism. *)
    cmi_num_polymorphics_extra_args : int ;
    cmi_external_expr : Parsetree.external_expr_desc option
    }

  (** The list of mappings according to external languages to know to which
      string the record type field name corresponds. *)
  type label_mapping_info =  Parsetree.external_expr_desc


  type method_info = {
    mi_name : Parsetree.vname ;        (** The field name. *)
    mi_dependencies_from_parameters :
      ((** The positional list of methods from the species parameters
           abstracted by lambda-lifting. *)
       TypeInformation.species_param *
       (* The set of methods of this parameter on which we have dependencies. *)
       Parsetree_utils.DepNameSet.t) list ;
    mi_abstracted_methods : Parsetree.vname list   (** The positional list
        of methods from ourselves abstracted by lambda-lifting. *)
    }

  type species_binding_info =
    ((** The list of species parameters of the species with their kind. *)
     (TypeInformation.species_param list) *
     (** The list of methods the species has. *)
     (method_info list) *
     (** Optionnal because species that are non fully defined do not have
         any collection generator although they are entered in the
         environment. *)
     (collection_generator_info option) *
     (** Tells if the info is bound to a species or a collection. *)
     collection_or_species)

  (** The number of extra argument the identifier has due to its
      polymorphism. [Unsure] Certainement useless maintenant. *)
  type value_mapping_info = int

  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
                scoping environments.

      {b Rem} : Not exported outside this module.                   *)
  (* ************************************************************** *)
  type env =
    (constructor_mapping_info, label_mapping_info, unit, value_mapping_info,
     species_binding_info) generic_env
end
;;




(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)

type fo_file_structure = {
  ffs_scoping : ScopeInformation.env ;
  ffs_typing : TypeInformation.env ;
  ffs_mlgeneration : MlGenInformation.env ;
  ffs_coqgeneration : CoqGenInformation.env }
;;



let (scope_find_module, type_find_module,
     mlgen_find_module, coqgen_find_module,
     scope_open_module, type_open_module,
     mlgen_open_module, coqgen_open_module) =
  (* Let's just make the list used to bufferize opened files' content. *)
  (* Because ".fo" files contains always both the scoping and typing   *)
  (* information, once loaded for scoping purpose, the typing info     *)
  (* and ml code generation information are made available. Hence, the *)
  (* buffer list contains couples with no optionnal component.         *)
  let buffered = ref ([] : (Types.fname * fo_file_structure) list) in



  (* ***************************************************************** *)
  (* loc: Location.t -> Types.fname ->                                 *)
  (*   (ScopeInformation.env * TypeInformation.env)                    *)
  (** {b Descr} : Wrapper to lookup inside an external interface file.
                The lookup also performs a bufferisation to prevent
                futher calls from accessing again the disk. This
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
          let (envts : fo_file_structure) = input_value in_file in
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
    { constructors = constructors'; labels = labels'; types = types';
      values = values'; species = species' } in



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
         else (internal_find_module ~loc fname).ffs_scoping),



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
          else (internal_find_module ~loc fname).ffs_typing),



   (* ******************************************************************* *)
   (* mlgen_find_module                                                   *)
   (*   loc: Location.t -> current_unit: Types.fname ->                   *)
   (*   Types.fname option -> MlGenInformation.env ->                     *)
   (*     MlGenInformation.env                                            *)
   (** {b Descr} : Wrapper to lookup a ml generation environment inside
                 an external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initially passed as argument.

       {b Rem} : Not exported outside this module.                        *)
   (* ******************************************************************* *)
   (fun ~loc ~current_unit fname_opt mlgen_env ->
     match fname_opt with
      | None -> mlgen_env
      | Some fname ->
          if current_unit = fname then mlgen_env
          else (internal_find_module ~loc fname).ffs_mlgeneration),



   (* ******************************************************************* *)
   (* coqgen_find_module                                                  *)
   (*   loc: Location.t -> current_unit: Types.fname ->                   *)
   (*   Types.fname option -> CoqGenInformation.env ->                    *)
   (*     CoqGenInformation.env                                           *)
   (** {b Descr} : Wrapper to lookup a ml generation environment inside
                 an external interface file. Note that if it is requested
                 to lookup inside the current compilation unit's
                 environment (the current file has the same name than
                 the looked-up module), then returned environment is
                 the one initially passed as argument.

       {b Rem} : Not exported outside this module.                        *)
   (* ******************************************************************* *)
   (fun ~loc ~current_unit fname_opt coqgen_env ->
     match fname_opt with
      | None -> coqgen_env
      | Some fname ->
          if current_unit = fname then coqgen_env
          else (internal_find_module ~loc fname).ffs_coqgeneration),



   (* *************************************************************** *)
   (* scope_open_module                                               *)
   (* loc: Location.t -> Types.fname -> ScopingEnv.t -> ScopingEnv.t  *)
   (** {b Descr} : Performs a full "open" directive on a scoping
                 environment. It add in head of the environment the
                 bindings found in the "module" content, tagging them
                 as beeing "opened".

       {b Rem} : Exported outside this module.                        *)
   (* *************************************************************** *)
   (fun ~loc fname env ->
     let loaded_scope_env = (internal_find_module ~loc fname).ffs_scoping in
     internal_extend_env fname loaded_scope_env env),



   (* *************************************************************** *)
   (* type_open_module                                                *)
   (* loc: Location.t -> Types.fname -> TypingEnv.t -> TypingEnv.t    *)
   (** {b Descr} : Performs a full "open" directive on a typing
                 environment. It add in head of the environment the
                 bindings found in the "module" content, tagging them
                 as beeing "opened".

       {b Rem} : Exported outside this module.                        *)
   (* *************************************************************** *)
   (fun ~loc fname env ->
     let loaded_type_env = (internal_find_module ~loc fname).ffs_typing in
     internal_extend_env fname loaded_type_env env),



   (* **************************************************************** *)
   (* mlgen_open_module                                                *)
   (*   loc: Location.t -> Types.fname -> MlGenEnv.t -> MlGenEnv.t     *)
   (** {b Descr} : Performs a full "open" directive on a ml generation
                 environment. It add in head of the environment the
                 bindings found in the "module" content, tagging them
                 as beeing "opened".

       {b Rem} : Exported outside this module.                         *)
   (* **************************************************************** *)
   (fun ~loc fname env ->
     let loaded_mlgen_env =
       (internal_find_module ~loc fname).ffs_mlgeneration in
     internal_extend_env fname loaded_mlgen_env env),



   (* **************************************************************** *)
   (* coqgen_open_module                                               *)
   (*   loc: Location.t -> Types.fname ->  CoqGenEnv.t -> CoqGenEnv.t  *)
   (** {b Descr} : Performs a full "open" directive on a coq
            generation environment. It add in head of the environment
            the bindings found in the "module" content, tagging them
            as beeing "opened".

       {b Rem} : Exported outside this module.                         *)
   (* **************************************************************** *)
   (fun ~loc fname env ->
     let loaded_coqgen_env =
       (internal_find_module ~loc fname).ffs_coqgeneration in
     internal_extend_env fname loaded_coqgen_env env)
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
    Parsetree.qualified_vname -> species_bound_data ->
      (constructor_bound_data, label_bound_data, type_bound_data,
       value_bound_data, species_bound_data)
        generic_env
  val post_process_method_value_binding :
    Parsetree.qualified_vname -> value_bound_data -> value_bound_data
end
;;



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
    ({ constructors = []; labels = []; types = []; values = [];
      species = [] } : t)


  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh environment containing
                information bound the basic builtins.

      {b Rem} : Exported outside this module.              *)
  (* ***************************************************** *)
  let pervasives () = EMAccess.pervasives ()



  (* loc: Location.t -> Parsetree.vname -> EMAccess.species_bound_data -> *)
  (*   t -> t                                                             *)
  let add_species ~loc species_name data (env : t) =
    (* Ensure the species name does not already exists in the *)
    (* current module. This means that this name must not be  *)
    (* already bound to a [BO_absolute].                      *)
    if List.exists
        (function (n, (BO_absolute _)) -> n = species_name | _ -> false)
        env.species then
      raise (Rebound_species (species_name, loc));
    ({ env with
       species = (species_name, BO_absolute data) :: env.species } : t)


  let opt_scope_vname = function
    | Parsetree.Vname vname -> (None, vname)
    | Parsetree.Qualified (modname, vname) -> ((Some modname), vname)



  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> *)
  (*    t -> EMAccess.species_bound_data                                *)
  let rec find_species ~loc ~current_unit coll_ident (env : t) =
    match coll_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_species_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_species_vname ~loc ~allow_opened vname env'


  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname -> *)
  (*    t -> EMAccess.species_bound_data                         *)
  and find_species_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.species with
    | Not_found -> raise (Unbound_species (vname, loc))



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
  (* loc: Location.t -> current_unit: Types.fname ->                     *)
  (*   Parsetree.expr_ident -> t -> EMAccess.value_bound_data            *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.

      {b Rem} : Exported outside this module.                            *)
  (* ******************************************************************* *)
  let rec find_value ~loc ~current_unit ident_ident (env : t) =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.EI_local vname ->
         (* No explicit scoping information was provided, hence *)
         (* opened modules bindings are acceptable.             *)
         find_value_vname ~loc ~allow_opened: true vname env
     | Parsetree.EI_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_value_vname ~loc ~allow_opened vname env'
     | Parsetree.EI_method (None, vname) ->
         (* No collection scope. Then the searched ident must belong  *)
         (* to the inheritance of Self. First, this means that opened *)
         (* stuff is forbidden. Next, because the [values] bucket is  *)
         (* so that inherited methods and our methods belong to it,   *)
         (* we just have to search for the [vname] inside the current *)
         (* environment.                                              *)
         find_value_vname ~loc ~allow_opened: false vname env
     | Parsetree.EI_method
         (Some (Parsetree.Vname (Parsetree.Vuident "Self")), vname) ->
         (* Like if there was no collection scope (see above). *)
         find_value_vname ~loc ~allow_opened: false vname env
     | Parsetree.EI_method (Some coll_specifier, vname) ->
         let (opt_module_qual, coll_vname) =
           match coll_specifier with
           | Parsetree.Vname coll_vname ->
             None, coll_vname
           | Parsetree.Qualified (modname, coll_vname) ->
             Some modname, coll_vname in
         (* Recover the environment in where to search,according *)
         (* to if the species is qualified by a module name.     *)
         let env' =
           EMAccess.find_module
             ~loc ~current_unit opt_module_qual env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_module_qual in
         (* Build the complete species name, including its hosting *)
         (* module. If none specified, then this module is the     *)
         (* current one because this means that the species name   *)
         (* implicitely refers to the current compilation unit.    *)
         let full_coll_name =
           (match opt_module_qual with
            | None -> Parsetree.Qualified (current_unit, coll_vname)
            | Some module_qual ->
                Parsetree.Qualified (module_qual, coll_vname)) in
         (* We must first look inside collections and species     *)
         (* for the [coll_vname] in order to recover its methods. *)
         let available_meths =
           (EMAccess.make_value_env_from_species_methods
              full_coll_name
              (find_species_vname
                 ~loc ~allow_opened  coll_vname env')) in
         let data =
           find_value_vname
             ~loc ~allow_opened vname available_meths in
         (* Now we apply the post-processing on the found data. *)
         EMAccess.post_process_method_value_binding
           full_coll_name data


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
  (* Parsetree.constructor_name -> EMAccess.constructor_bound_data ->    *)
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



  (* ********************************************************************* *)
  (* current_unit: loc: Location.t -> Types.fname ->                       *)
  (*   Parsetree.constructor_ident -> t -> EMAccess.constructor_bound_data *)
  (** {b Descr} : Looks-up for a [constructorident] inside the constructors
        environment.

      {b Rem} : Exported outside this module.                              *)
  (* ********************************************************************* *)
  let rec find_constructor ~loc ~current_unit cstr_ident (env : t) =
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.CI qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_constructor_vname ~loc ~allow_opened vname env'



  (* *********************************************************** *)
  (* allow_opened: loc: Location.t -> bool ->                    *)
  (*   Parsetree.constructor_name -> t ->                             *)
  (*   EMAccess.constructor_bound_data                           *)
  (** {b Descr} : Looks-up for a [vname] inside the constructors
                environment.

      {b Rem} : Not exported outside this module.                *)
  (* *********************************************************** *)
  and find_constructor_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.constructors
    with Not_found -> raise (Unbound_constructor (vname, loc))



  (* ************************************************************** *)
  (* Parsetree.vname -> EMAccess.label_bound_data -> t -> t         *)
  (** {b Descr} : Return an environment extended with a binding
                between a record field label [lbl_vname] and the
                argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                       *)
  (* ************************************************************** *)
  let add_label lbl_vname data (env : t) =
    ({ env with labels = (lbl_vname, BO_absolute data) :: env.labels } : t)



  (* ************************************************************* *)
  (* loc: Location.t -> Parsetree.label_ident -> t ->              *)
  (*   EMAccess.label_bound_data                                   *)
  (** {b Descr} : Looks-up for an [ident] inside the record fields
        labels environment.

      {b Rem} : Exported outside this module.                      *)
  (* ************************************************************* *)
  let rec find_label ~loc ~current_unit lbl_ident (env : t) =
    match lbl_ident.Parsetree.ast_desc with
     | Parsetree.LI qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_label_vname ~loc ~allow_opened vname env'



  (* ***************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname ->       *)
  (*   Parsetree.vname -> t -> EMAccess.label_bound_data               *)
  (** {b Descr} : Looks-up for a [vname] inside the record type labels
         environment.

      {b Rem} : Not exported outside this module.                      *)
  (* ***************************************************************** *)
  and find_label_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.labels with
    | Not_found -> raise (Unbound_label (vname, loc))



  (* ***************************************************************** *)
  (* loc: Location.t -> Types.type_name -> EMAccess.type_bound_data -> *)
  (*   t -> t                                                          *)
  (** {b Descr} : Return an environment extended with a binding
                between a type [ident] and the argument [data].
                The initial environment is passed as last argument.

      {b Rem} : Exported outside this module.                          *)
  (* ***************************************************************** *)
  let add_type ~loc tyname data (env : t) =
    (* Ensure the type name does not already exists in the *)
    (* current module. This means that this name must not  *)
    (* be already bound to a [BO_absolute].                *)
    if List.exists
        (function (n, (BO_absolute _)) -> n = tyname | _ -> false)
        env.types then
      raise (Rebound_type (tyname, loc));
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
     | Parsetree.I_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something *)
         (* coming from an opened module.            *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_type_vname ~loc ~allow_opened vname env'



  (* **************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname ->      *)
  (*   Parsetree.vname -> t -> EMAccess.type_bound_data               *)
  (** {b Descr} : Looks-up for a [vname] inside the types environment.

      {b Rem} : Not exported outside this module.                     *)
  (* **************************************************************** *)
  and find_type_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.types with
    | Not_found -> raise (Unbound_type (vname, loc))
end
;;



module ScopingEMAccess = struct
  type constructor_bound_data = Types.fname
  type label_bound_data = Types.fname
  type type_bound_data = ScopeInformation.type_binding_info
  type value_bound_data = ScopeInformation.value_binding_info
  type species_bound_data = ScopeInformation.species_binding_info

  let find_module = scope_find_module
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  let make_value_env_from_species_methods species spec_info =
    let values_bucket =
      List.map
        (fun meth_vname ->
          (meth_vname,
           BO_absolute (ScopeInformation.SBI_method_of_coll species)))
        spec_info.ScopeInformation.spbi_methods in
    { constructors = []; labels = []; types = []; values = values_bucket;
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
               this method was bound to, for instance "KikooSpecies", then
               once scoped, the AST would return an [ident] explicitly
               shaped like "KikooSpecies!meth" instead of "Self!meth".
               This would force the method to be the one of this species
               at this level in the inheritance, hence forbidding
               late-binding.

     {b Rem} : Exported outside this module, but not outside this file.       *)
  (* ************************************************************************ *)
  let post_process_method_value_binding coll_name = function
    | ScopeInformation.SBI_method_of_self ->
        ScopeInformation.SBI_method_of_coll coll_name
    | whatever -> whatever
end
;;
module ScopingEnv = Make (ScopingEMAccess);;



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
    { constructors = []; labels = []; types = []; values = []; species = [] }


  (* ******************************************************************** *)
  (* 'a -> TypeInformation.species_description ->                         *)
  (*   ('b, 'c, 'd, Types.type_scheme, 'e) generic_env                    *)
  (** {b Descr} : Create a fresh environment with the methods, properties
                and theorems of the species called [spec_name] from the
                 methods found in [spec_info].

      {b Rem} : Not exported outside this module.                         *)
  (* ******************************************************************** *)
  let make_value_env_from_species_methods _spec_name spec_info =
    (* By folding left, fields at the head of the list will be at the tail *)
    (* of the environment list. Hence, methods seen first are inserted     *)
    (* first, hence are deeper in the environment.                         *)
    let values_bucket =
      List.fold_left
        (fun accu field ->
          match field with
           | TypeInformation.SF_sig (_, v, s)
           | TypeInformation.SF_let (_, v, _, s, _, _, _)
           | TypeInformation.SF_theorem (_, v, s, _, _, _)
           | TypeInformation.SF_property (_, v, s, _, _) ->
               [(v, (BO_absolute s))] @ accu
           | TypeInformation.SF_let_rec l ->
               let l' =
                 List.map
                   (fun (_, v, _, s, _, _, _) -> (v, (BO_absolute s))) l in
               l' @ accu)
        []
        spec_info.TypeInformation.spe_sig_methods in
    { constructors = [] ; labels = [] ; types = [] ; values = values_bucket ;
      species = [] }



  (* No real need in the typing environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module TypingEnv = Make (TypingEMAccess) ;;



module MlGenEMAccess = struct
  type constructor_bound_data = MlGenInformation.constructor_mapping_info
  type label_bound_data = MlGenInformation.label_mapping_info
  type type_bound_data = unit
  type value_bound_data = unit
  type species_bound_data = MlGenInformation.species_binding_info

  let find_module = mlgen_find_module
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  let make_value_env_from_species_methods _species _spec_info =
    (* Non sense for ml code generation environments because we do not *)
    (* provide any [find_value] function and that's this function that *)
    (* requires [make_value_env_from_species_methods].                 *)
    assert false


  (* No real need in the ml code generation environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module MlGenEnv = Make (MlGenEMAccess) ;;



module CoqGenEMAccess = struct
  type constructor_bound_data = CoqGenInformation.constructor_mapping_info
  type label_bound_data = CoqGenInformation.label_mapping_info
  type type_bound_data = unit
  type value_bound_data = CoqGenInformation.value_mapping_info
  type species_bound_data = CoqGenInformation.species_binding_info

  let find_module = coqgen_find_module
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  let make_value_env_from_species_methods _species (_, meths_info, _, _) =
    (* Because methods are never polymorphics this was checked before), *)
    (* we can safely insert each method as a value bound to 0 extra     *)
    (* parameters that woud come from ... polymorphism.                 *)
    let values_bucket =
      List.map
        (fun { CoqGenInformation. mi_name = field_name } ->
          (field_name, (BO_absolute 0)))
        meths_info in
    { constructors = []; labels = []; types = []; values = values_bucket;
      species = [] }



  (* No real need in the ml code generation environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module CoqGenEnv = Make (CoqGenEMAccess);;



(* **************************************************************** *)
(* source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t ->   *)
(*   MlGenEnv.t -> CoqGenEnv.t -> unit                              *)
(** {b Descr} : Create the "fo file" on disk related to the current
              compilation unit.
              This "fo file" contains :
                - A magic number.
                - A triplet (scoping env * typing env * mlgen env).

    {b Rem} : Exported outside this module.                         *)
(* **************************************************************** *)
let make_fo_file ~source_filename scoping_toplevel_env typing_toplevel_env
    mlgen_toplevel_env coqgen_toplevel_env =
  (* First, recover from the scoping environment only bindings *)
  (* coming from definitions of our current compilation unit.  *)
  let scoping_toplevel_env' =
    env_from_only_absolute_bindings scoping_toplevel_env in
  (* Next, recover from the typing environment only bindings *)
  (* coming from definitions of our current compilation unit.  *)
  let typing_toplevel_env' =
    env_from_only_absolute_bindings typing_toplevel_env in
  (* Next, recover from the ml generation environment only bindings *)
  (* coming from definitions of our current compilation unit.        *)
  let mlgen_toplevel_env' =
    env_from_only_absolute_bindings mlgen_toplevel_env in
  (* Finally, recover from the coq generation environment only bindings *)
  (* coming from definitions of our current compilation unit.           *)
  let coqgen_toplevel_env' =
    env_from_only_absolute_bindings coqgen_toplevel_env in
  let module_name = Filename.chop_extension source_filename in
  let fo_basename = Files.fo_basename_from_module_name module_name in
  (* Add to the module name the path of the currently compiled source *)
  (* file in order to make the ".fo" lying at the same place than the *)
  (* ".foc" file.                                                     *)
  let with_path =
    Filename.concat (Filename.dirname source_filename) fo_basename in
  let out_hd = open_out_bin with_path in
  (* First, write the magic number of the file. *)
  Files.write_magic out_hd Files.fo_magic ;
  (* And now the filtered environments. *)
  output_value
    out_hd
    (scoping_toplevel_env', typing_toplevel_env',
     mlgen_toplevel_env', coqgen_toplevel_env') ;
  (* Just don't forget to close the output file... *)
  close_out out_hd
;;





(** Not yet documented. Used to make the debug tool "fodump"... *)
let inspect_fo_structure ppf fo =
  let coq_gen_info = fo.ffs_coqgeneration in
  (* Dump species' and collections' information. *)
  List.iter
    (fun (species_vname, envt_binding) ->
      (* In a fo file, there must only remain bindings really     *)
      (* introduced by the compilation unit, not by "open" stuf ! *)
      let (_, meths, opt_coll_gen, coll_or_spe) =
        match envt_binding with
         | BO_opened (_, _) -> assert false | BO_absolute b -> b in
      (* Start printing... *)
      Format.fprintf ppf "@[<2>Species %a@\n" Sourcify.pp_vname species_vname ;
      Format.fprintf ppf "@[<2>*** Methods:@\n" ;
      List.iter
        (fun meth ->
          (* Just print the method's name for the moment. *)
          Format.fprintf ppf "Method %a  ...@\n"
            Sourcify.pp_vname meth.CoqGenInformation.mi_name)
        meths ;
      (* Now, check for the collection generator information. *)
      Format.fprintf ppf "@]@[<2>*** Collection generator:@\n" ;
      (match opt_coll_gen with
       | None ->  Format.fprintf ppf "None found@."
       | Some cgi ->
           Format.fprintf ppf "Some found@." ;
           (* Info about "implemented" species. *)
           Format.fprintf ppf "Implemented species params names: %a@\n"
             (Handy.pp_generic_separated_list ","
                (fun ppf (pname, _) -> Sourcify.pp_vname ppf pname))
             cgi.CoqGenInformation.cgi_implemented_species_params_names ;
           Format.fprintf ppf "@]") ;
       (match coll_or_spe with
        | COS_species -> Format.fprintf ppf "Is a species.@."
        | COS_collection -> Format.fprintf ppf "Is a collection.@.") ;
       (* End the species dump box. *)
       Format.fprintf ppf "@]@\n")
    coq_gen_info.species
;;



(* [Unsure] Garbage. To diseaper. *)
let print_field_for_debug = function
  | TypeInformation.SF_sig (_, n, sch) ->
      Format.eprintf "signature %a : %a@." Sourcify.pp_vname n
        Types.pp_type_scheme sch
  | TypeInformation.SF_let (_, n, args, sch, body, _, _) ->
      Format.eprintf "let %a " Sourcify.pp_vname n ;
      List.iter (fun a -> Format.eprintf "%a " Sourcify.pp_vname a) args ;
      Format.eprintf ": %a " Types.pp_type_scheme sch ;
      Format.eprintf "= %a@." Sourcify.pp_binding_body body ;
      Format.eprintf "@."
  | TypeInformation.SF_let_rec l ->
      List.iter
        (fun (_, n, args, sch, body, _, _) ->
          Format.eprintf "let rec %a " Sourcify.pp_vname n ;
          List.iter (fun a -> Format.eprintf "%a " Sourcify.pp_vname a) args ;
          Format.eprintf ": %a " Types.pp_type_scheme sch ;
          Format.eprintf "= %a@." Sourcify.pp_binding_body body ;
          Format.eprintf "@.")
        l
  | TypeInformation.SF_theorem _ | TypeInformation.SF_property _ ->
      Format.eprintf "Property/Theorem@."
;;

