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
(*  Copyright 2007 - 2012... LIP6 and INRIA                                   *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************** *)
(** {b Descr} : This module contains the whole environments mechanisms.
    Environnments are used for both scoping and typing. Because they share
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
          The persistent datastructure related to "module" name.
        - The scoping environment primitives.
        - The typing environment primitives.                                  *)
(* ************************************************************************** *)

exception Unbound_constructor of (Parsetree.vname * Location.t);;
exception Unbound_label of (Parsetree.vname * Location.t);;
exception Unbound_identifier of (Parsetree.vname * Location.t);;
exception Unbound_type of (Parsetree.vname * Location.t);;
exception Unbound_module of (Types.fname * Location.t);;
exception Unbound_species of (Parsetree.vname * Location.t);;
exception Unbound_collection of (Parsetree.vname * Location.t);;

exception Rebound_type of (Parsetree.vname * Location.t);;
exception Rebound_species of (Parsetree.vname * Location.t);;
exception Rebound_toplevel_let of (Parsetree.vname * Location.t);;


(* ************************************************************************ *)
(** {b Desc} : Enables to differentiate environment bindings induced by
    definitions really present in the current compilation unit and bindings
    introduced by a "open" directive.

    {b Exported} : No.                                                      *)
(* ************************************************************************ *)
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
    Otherwise, continue search until a BO_absolute binding is found.

    {b Exported} : No.                                                *)
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

(* For debug purpose.
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
                 flush stderr;
                 v
                 end)
               else rec_assoc q
           | BO_absolute v ->
               Format.eprintf "@\nSearch successfully ends on absolute...\n";
               flush stderr;
               v
          end)
        else rec_assoc q in
  Format.eprintf
    "Search starts for \"%s\"... with allow_opened = %b\n"
    (vname_as_string searched) allow_opened;
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

    {b Exported} : Abstract.                                             *)
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
   definitions of the current compilation unit, i.e. those tagged
   [BO_absolute].
   Such an environment will be suitable to be dumped in a persistent
   datastructure on disk for the "modules" mechanism.

   {b Exported} : No.                                                      *)
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
  { constructors = constructors'; labels = labels'; types = types';
    values = values'; species = species' }
;;


(* ************************************************************************ *)
(* {b Descr} : Allows to make the difference between the substitution of a
    collection parameter and an entity parameter.
    We could not directly make a sum type in types.ml to represent the
    3 kinds of substitutions because we would have a cyclic dependency
    between types.ml and parsetree.mli.
    And in fact, since espressions do not appear in ML types, substitutions
    performed on them will never involve expressions.

   {b Exported} : Yes.                                                      *)
(* ************************************************************************ *)
type substitution_kind =
  | SK_collection_by_collection of
      (Types.type_collection *
       Types.substitution_by_replacement_collection_kind)
  | SK_ident_by_expression of
      (Types.fname *   (** Compilation unit hosting the species havin this
                           formal entity parameter. *)
       Parsetree.vname *  (** The entity parameter's name. *)
       Parsetree.expr_desc)  (** The expression by what the entity parameter
                                 must be replaced. *)
;;



  (* ************************************************************************ *)
  (** {b Descr}: Describes the kind of recursion, i.e. termination proof,
      provided to a recursive definition. Currently, we only make the
      difference between a structural termination and none/other proofs.
      In case of structural termination we assume that the definition was
      generated using "Fixpoint" using the provided parameter name as
      decreasing argument. In any other case, we assume it has been generated
      with "Function".
      Note that this type may change/disapear when we will have a more unified
      code generation model for recursion.

      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
type rec_proof_kind =
  | RPK_struct of Parsetree.vname
  | RPK_other ;;



(* ************************************************************************ *)
(** {b Descr}: Tells if a definition is recursive or not. Allows embedding
    the kind of termination proof the definition has if it as one.
    Since we currently have 2 Coq generation models: "Fixpoint" and "Function"
    we need to remind which one was used in case a proof is done
    "by definition" of a recursive definition. In effect, depending on the
    used model, we must not generate the same code for Zenon.

    {b Visibility}: Exported outside this module.                           *)
(* ************************************************************************ *)
type rec_status =
  | RC_non_rec
  | RC_rec of rec_proof_kind ;;



(* For debugging purpose. *)
let debug_substitution substs =
  Format.eprintf "[ ";
  List.iter
    (function
      | SK_collection_by_collection (ty_coll, subst_by_replacement_kind) ->
          (begin
          Format.eprintf "%a <- " Types.pp_type_collection ty_coll;
          match subst_by_replacement_kind with
           | Types.SBRCK_coll by_ty_coll ->
               Format.eprintf "%a; " Types.pp_type_collection by_ty_coll
           | Types.SBRCK_self -> Format.eprintf "Self; "
          end)
      | SK_ident_by_expression (id_fname, id_name, _) ->
          Format.eprintf "%s#%a <- ..." id_fname Sourcify.pp_vname id_name)
    substs;
  Format.eprintf "]@."
;;

(* ***************************************************************** *)
(** {b Descr} : Describes from where a method comes, keeping all the
    inheritance steps in memory.
    For instance:
      species Foo0 (A0 is Sp0) inherits ... = let v = 1 end;;
      species Foo1 (A1 is Sp1) inherits Foo0 (A1) = let v = 2 end;;
      species Foo2 (A2 is Sp1) inherits Foo1 (A2) = end
;;
      species Foo3 (A3 is Sp1, A4 is A3) inherits Foo2 (A4) = end
;;

    For method "v" in "Foo3" :
      [fh_initial_apparition] : Foo1
      [fh_inherited_along] : [(Foo3, Foo2(A4)); (Foo2, Foo1 (A2))]

    {b Exported} : Yes.                                              *)
(* ***************************************************************** *)
type from_history = {
  (** The species where the method was defined or declared for the first time.
      without redefinition. All inheritance information prior to a possible
      redefinition in this species is discarded. *)
  fh_initial_apparition : Parsetree.qualified_species;
  (** The list of species inherited along which the method was not redefined.
      In head of the list is the most recently inherited species, and in tail
      are the least recents. There is never overlaping between fields
      [fh_inherited_along] and [fh_initial_apparition]. This means that if the
      method is just defined in the current species, then [fh_inherited_along]
      is empty. For each species in the history, we have the simplified
      species expression used to build the species we inherited of. *)
  fh_inherited_along :
    (Parsetree.qualified_species * Parsetree_utils.simple_species_expr *
       (* The list of substitutions of formal species parameters by effective
          ones done during the inheritance expression. The substitutions are in
          the right order for application. This means that the first to perform
          is in head of the list. *)
       (substitution_kind list))
      list
}
;;



(* ***************************************************************** *)
(** {b Descr} : Create an inheritance history by setting the initial
    apparition of a method.

    {b Exported} : Yes.                                              *)
(* ***************************************************************** *)
let intitial_inheritance_history species =
  { fh_initial_apparition = species;
    fh_inherited_along = [] }
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* ********************************************************************** *)
(** {b Descr} : This module contains the structure of scoping information
    used in scoping environments.

    {b Exported} : Yes. Only the type of the environment is abstracted to
    prevent savage manipulations.                                         *)
(* ********************************************************************** *)
module ScopeInformation = struct
  (* ************************************************************************ *)
  (** {b Descr} : Tags each value binding in the scopping environment in
      order to know if the [ident] is currently bound to a global toplevel
      definition inside a file, if it's a method found in the current species
      inheritance tree (including itself), if it's a method found in another
      species inheritance tree or finally if it's a locally bound identifier.

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  type value_binding_info =
      (** The ident is at toplevel of a file (including the current file). *)
    | SBI_file of Types.fname
      (** The ident is a method implicitely of "Self". *)
    | SBI_method_of_self
      (** The ident is a method explicitly of a collection. ATTENTION: while
          inserting a method in the environment at its definition point,
          it must always be tagged with [SBI_method_of_self]. The tag
          [SBI_method_of_coll] can only be returned by [find_value] or
          [make_value_env_from_species_methods] who may perform a change on
          the fly if required. *)
    | SBI_method_of_coll of
        Parsetree.qualified_vname (** The module name hosting the collection
                                      and the collection the method belongs
                                      to. *)
      (** The ident is a locally bound indentifier
          (let or function parameter). *)
    | SBI_local



  (* ************************************************************************ *)
  (** {b Descr} : Tags each type binding in the scopping environment in order
      to know if the [ident] is a type variable (or a builtin type) or a type
      defined at toplevel in a compilation unit.

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  type type_binding_info =
      (** The type identifier is either a type variable name
          ('a for instance) or a builtin type (int for instance). *)
    | TBI_builtin_or_var
      (** The identifier is a type name defined at toplevel in a file. *)
    | TBI_defined_in of Types.fname



  (* ****************************************************************** *)
  (** {b Descr} : Flag defining the scope of a species, i.e. if it is a
      species defined at toplevel or is a collection parameter of the
      current species.

      {b Exported} : Yes.                                               *)
  (* ****************************************************************** *)
  type species_scope =
      (** The identifier is a species name defined at toplevel in a file. 
          If the bool is [true] then it is a collection. *)
    | SPBI_file of (Types.fname * bool)
      (** The identifier is a locally bound collection like in the case of a
          "is"-bound parameter (i.e. [c is ...]) where [c] is then considered
          as a local collection). *)
    | SPBI_parameter


  (* *********************************************************************** *)
  (** {b Descr} : Flag describing if the species parameter is an entity or a
      collection parameter.

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type species_parameter_kind =
    | SPK_in       (** Parameter is an entity parameter ("in"). *)
    | SPK_is       (** Parameter is a collection parameter ("is"). *)


  (* ********************************************************************** *)
  (** {b Descr} : Tags each species binding in the scopping environment in
      order to record the scoping information of the things contained in the
      species. It especially record material related to scoping its
      parameters, its methods and its own scope.

      {b Exported} : Yes.                                                   *)
  (* ********************************************************************** *)
  type species_binding_info = {
    (** The list of *ALL* the method owned, including those added by
        inheritance. Methods from the most recent ancestor are in head of the
        list. In case of multiple inheritance, we consider that ancestors
        are older from left to right. *)
    spbi_methods : Parsetree.vname list;
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
    spbi_params_kind : species_parameter_kind list;
    (** Scopped clauses inherit. this only serves to be able to dump the
        inheritance graph of species. In effet, since methods presence is
        incrementally done by eliminitating inheritance information, it
        get impossible to find these clauses somewhere later. *)
    spbi_inherits : Parsetree.species_expr list ;
    (** The information telling how the species is bound (i.e. "scoped"). *)
    spbi_scope : species_scope
  }



  (* *********************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the scoping
      environments.

      {b Exported} : No.                                                     *)
  (* *********************************************************************** *)
  type env =
    (Types.fname, Types.fname, type_binding_info, value_binding_info,
     species_binding_info) generic_env
end
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)


(* ********************************************************************** *)
(** {b Descr} This type is defined just in order to ensure that functions
    requiring "dependencies on parameters" argument ordered according to
    their dependency graph will really receive one and will not use an
    unsorted list.

    {b Rem} : Exported outside this module.                               *)
(* ********************************************************************** *)
type ordered_methods_from_params =
  | ODFP_methods_list of
      (Parsetree.vname * Parsetree_utils.dependency_elem_type_kind) list
;;



(* ********************************************************************** *)
(** {b Descr} : This module contains the structure of typing information
    used in typing environments.

    {b Exported} : Yes. Only the type of the environment is abstracted to
    prevent savage manipulations.                                         *)
(* ********************************************************************** *)
module TypeInformation = struct
  (* **************************************************************** *)
  (** {b Descr} : Records if a method has dependencies on the carrier
      representation "rep".

      {b Exported} : Yes.                                             *)
  (* **************************************************************** *)
  type dependency_on_rep = {
    dor_def : bool;  (** Flag for a def-dependency. *)
    dor_decl : bool }  (** Flag for a decl-dependency. *)


  (* ******************************************************************** *)
  (** {b Descr} : Describes the kind of a "let" definition, i.e. if it is
      recursive and if it is a "regular" "let" or a "logical let".

      {b Exported} : Yes.                                                 *)
  (* ******************************************************************** *)
  type let_definition_flags = {
    ldf_recursive : Parsetree.rec_flag; (** Tells if the  let-bound identifier
                                             is recursive or not. *)
    ldf_logical : Parsetree.logical_flag; (** Tells if the let-bound identifier
                                              is a logical or a computational
                                              definition. *)
    ldf_final : Parsetree.final_flag (** Tells if the let-bound identifier
                                         is final, i.e. can't be redefined. *)
    }


  (* *********************************************************************** *)
  (** {b Descr} : Type of information recorded in the typing environment for
      a method being a "signature".

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type sig_field_info =
    ((** Where the sig comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *          (** The sig's name. *)
     Types.type_scheme)         (** The sig's type scheme. *)


  (* *********************************************************************** *)
  (** {b Descr} : Type of information recorded in the typing environment for
      a method being a "let" (either regular, or logical, or recursive).

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type let_field_info =
    ((** Where the let-bound comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *       (** Name of the let-bound definition. *)
     (** Parameters of the let-bound definition. *)
     (Parsetree.vname list) *
     Types.type_scheme *       (** Type scheme of the let-bound definition. *)
     Parsetree.binding_body *  (** Body of the let-bound definition. *)
     (** The termination proof if provided. *)
     (Parsetree.termination_proof option) *
     (** Tells if the method has dependencies on the carrier ("rep"). *)
     dependency_on_rep *
     let_definition_flags)   (** reminds the let-bound identifier flags
                                 (logical or a computational, recursive or
                                 not). *)


  (* *********************************************************************** *)
  (** {b Descr} : Type of information recorded in the typing environment for
      a method being a "theorem".

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type theorem_field_info =
    ((** Where the theorem comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *         (** The theorem's name. *)
     (** The mapping of type variables found in the "forall" and "exists" in the
         theorem's logical expression onto their name. *)
     ((Parsetree.vname * Types.type_simple) list) *
     Parsetree.logical_expr *  (** The theorem's body. *)
     Parsetree.proof *         (** The theorem's proof. *)
     (** Tells if the theorem has dependencies on the carrier ("rep"). *)
     dependency_on_rep)



  (* *********************************************************************** *)
  (** {b Descr} : Type of information recorded in the typing environment for
      a method being a "property".

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type property_field_info =
    ((** Where the property comes from (and inheritance history). *)
     from_history *
     Parsetree.vname *         (** The property's name. *)
     (** The mapping of type variables found in the "forall" and "exists" in the
         property's logical expression onto their name. *)
     ((Parsetree.vname * Types.type_simple) list) *
     Parsetree.logical_expr *  (** The property's body. *)
     (** Tells if the property has dependencies on the carrier ("rep"). *)
     dependency_on_rep)



  (* *********************************************************************** *)
  (** {b Descr} : Describes the kind of a species parameter. Can be either
      an entity or a collection parameter. In each case, we record various
      information to avoid computing them again.

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type species_param =
    (** Entity parameter. *)
    | SPAR_in of
        (Parsetree.vname *              (** The name of the "IN" parameter. *)
         (** The module and name of the species parameter. Indeed, that
             is the parameter's type-collection. *)
         Types.type_collection *
         (** Tells if the type of the parameter is built from a toplevel
             species, a toplevel collection, or a previous of our species
             parameters. *)
         Types.species_collection_kind)
    (** Collection parameter. *)
    | SPAR_is of
        ((** The module and name of the species parameter. Indeed, that
             is the parameter's type-collection. *)
         Types.type_collection *
         (** Tells if the type of the parameter is built from a toplevel
             species, a toplevel collection, or a previous of our species
             parameters. *)
         Types.species_collection_kind *
         (** The list of fields this parameter has. It is in fact the normalized
             form of the species methods the parameter has. *)
         (species_field list) *
         (** The species expression of the parameter. This expression has the
             normalized form compound of the above list of fields. This
             expression is kept because Coq/Dedukti code generation need to know
             it in order to make the type expression annotation the parameter
             in the hosting species record type. *)
          Parsetree_utils.simple_species_expr (* [Unsure] Really used ? *) *
          (** The dependency graph of the methods of the species we are a
              "IS" parameter. *)
          (DepGraphData.name_node list))



  (* ************************************************************************ *)
  (** {b Desc} : Describes the essence of a species field, i.e. if it's a
      signature, a let-binding, let let-rec-binding, a theorem or a property.
      Through this description the name, type, body and provenance (and even
      more if needed to fully describe the field according to its nature) of
      the field is made available.

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  and species_field =
    | SF_sig of sig_field_info   (** Field is a "signature". *)
    | SF_let of let_field_info   (** Field is a "let" bound definition. *)
    | SF_let_rec of
        let_field_info list   (** The list of information similar to what
                                  can be found for a [SF_let], but for each
                                  mutually recursive bound identifier. *)
    | SF_theorem of theorem_field_info    (** Field is a theorem. *)
    | SF_property of property_field_info  (** Field is a property. *)

  (** {b Descr} Elements of the minimal Coq typing environment for methods.
      We can't directly use [Env.TypeInformation.species_field] because they
      can't make appearing the fact that the carrier belongs to the minimal
      environment even if not *defined* (remind that in species fields, if
      "rep" appears then is it *defined* otherwise; it is silently declared
      and does't appear in the list of fields).

      {b Rem} : Not exported outside this module.                               *)
  type min_coq_env_method =
    | MCEM_Declared_carrier    (** The carrier belongs to the environment but
         only via a decl-dependency. Hence it doesn't need to be explicitely
         defined, but need to be in the environment. *)
    | MCEM_Defined_carrier of Types.type_scheme (** The carrier belongs to the
                 environment via at least a def-dependency. Then is have to
                 be explicitely declared. *)
    | MCEM_Declared_computational of
        (Parsetree.vname * Types.type_scheme) (** Abstract computational method,
           i.e. abstracted Let or abstracted Let_rec or Sig other than "rep". *)
    | MCEM_Defined_computational of
        (from_history *
           (** Tells if the method is recursive and if so which kind of
               termination proof it involves. This is needed when generating
               pseudo-Coq code for Zenon using a "by definition" of this method.
               In effect, the body of the method contains the ident of this
               method, but when generating the Zenon stuff, the definition of
               the method will be named "abst_xxx".
               So the internal recursive call to print when generating the
               method's body must be replaced by "abst_xxx". This is related to
               the bug report #199. Moreover, since currently structural
               recursion is compiled with "Fixpoint" and other kinds with
               "Function" in Coq, we need to remind what is the compilation
               scheme used depending on the recursion kind. *)
         rec_status *
         Parsetree.vname * (Parsetree.vname list) * Types.type_scheme *
         Parsetree.binding_body)  (** Defined computational method, i.e. Let or
            Let_rec. *)
    | MCEM_Declared_logical of
        (Parsetree.vname * Parsetree.logical_expr)  (** Abstract logical
            property, i.e. Property or abstracted Theorem. *)
    | MCEM_Defined_logical of     (** Defined logical property, i.e. Theorem. *)
        (from_history * Parsetree.vname * Parsetree.logical_expr)

    (** {b Descr}: Tells by which kind of construct (i.e. only logical or
        logical and/or computational) the method to add as dependency arrived.
        In other words, this tag tells if only logical target languages must
        take this dependency into account or if logical ANN also computational
        target languages are also impacted.
        This allows to compute the dependency calculus once for all, and not
        once for each target language. After thi common pass of calculus, each
        backend will select either [MCER_only_logical] AND [MCER_even_comput]
        dependencies for logical targets or only [MCER_even_comput]
        dependencies for computational targets.
        Clearly, [MCER_even_comput] is absorbant, this means that if a method
        is initiall present as dependency tagged by [MCER_only_logical], if it
        appear to be also required for computational stuff, it will be added
        with the tag [MCER_even_comput] whicb subsumes [MCER_only_logical].
        Said again differently, [MCER_even_comput] concerns both computational
        and logical targets although [MCER_only_logical] concerns only logical
        targets. *)

  type min_coq_env_reason =
    | MCER_only_logical   (** The method is only induced by logical stuff and
                              must not be taken into account by
                              only-computational targets backend. *)
    | MCER_even_comput    (** The method is induced by at least computational
                              stuff and must be taken into account by
                              only-computational and also logical targets
                              backend. *)

  type min_coq_env_element = (min_coq_env_reason * min_coq_env_method)

  (** {b Descr} Elements of the minimal Dk typing environment for methods.
      We can't directly use [Env.TypeInformation.species_field] because they
      can't make appearing the fact that the carrier belongs to the minimal
      environment even if not *defined* (remind that in species fields, if
      "rep" appears then is it *defined* otherwise; it is silently declared
      and does't appear in the list of fields).

      {b Rem} : Not exported outside this module.                               *)
  type min_dk_env_method =
    | MDEM_Declared_carrier    (** The carrier belongs to the environment but
         only via a decl-dependency. Hence it doesn't need to be explicitely
         defined, but need to be in the environment. *)
    | MDEM_Defined_carrier of Types.type_scheme (** The carrier belongs to the
                 environment via at least a def-dependency. Then is have to
                 be explicitely declared. *)
    | MDEM_Declared_computational of
        (Parsetree.vname * Types.type_scheme) (** Abstract computational method,
           i.e. abstracted Let or abstracted Let_rec or Sig other than "rep". *)
    | MDEM_Defined_computational of
        (from_history *
           (** Tells if the method is recursive and if so which kind of
               termination proof it involves. This is needed when generating
               pseudo-Dk code for Zenon using a "by definition" of this method.
               In effect, the body of the method contains the ident of this
               method, but when generating the Zenon stuff, the definition of
               the method will be named "abst_xxx".
               So the internal recursive call to print when generating the
               method's body must be replaced by "abst_xxx". This is related to
               the bug report #199. Moreover, since currently structural
               recursion is compiled with "Fixpoint" and other kinds with
               "Function" in Dk, we need to remind what is the compilation
               scheme used depending on the recursion kind. *)
         rec_status *
         Parsetree.vname * (Parsetree.vname list) * Types.type_scheme *
         Parsetree.binding_body)  (** Defined computational method, i.e. Let or
            Let_rec. *)
    | MDEM_Declared_logical of
        (Parsetree.vname * Parsetree.logical_expr)  (** Abstract logical
            property, i.e. Property or abstracted Theorem. *)
    | MDEM_Defined_logical of     (** Defined logical property, i.e. Theorem. *)
        (from_history * Parsetree.vname * Parsetree.logical_expr)

    (** {b Descr}: Tells by which kind of construct (i.e. only logical or
        logical and/or computational) the method to add as dependency arrived.
        In other words, this tag tells if only logical target languages must
        take this dependency into account or if logical ANN also computational
        target languages are also impacted.
        This allows to compute the dependency calculus once for all, and not
        once for each target language. After thi common pass of calculus, each
        backend will select either [MDER_only_logical] AND [MDER_even_comput]
        dependencies for logical targets or only [MDER_even_comput]
        dependencies for computational targets.
        Clearly, [MDER_even_comput] is absorbant, this means that if a method
        is initiall present as dependency tagged by [MDER_only_logical], if it
        appear to be also required for computational stuff, it will be added
        with the tag [MDER_even_comput] whicb subsumes [MDER_only_logical].
        Said again differently, [MDER_even_comput] concerns both computational
        and logical targets although [MDER_only_logical] concerns only logical
        targets. *)

  type min_dk_env_reason =
    | MDER_only_logical   (** The method is only induced by logical stuff and
                              must not be taken into account by
                              only-computational targets backend. *)
    | MDER_even_comput    (** The method is induced by at least computational
                              stuff and must be taken into account by
                              only-computational and also logical targets
                              backend. *)

  type min_dk_env_element = (min_dk_env_reason * min_dk_env_method)


  type field_abstraction_info = {
    (** The positional list of parameters carrier abstracted in the method. *)
    ad_used_species_parameter_tys : Parsetree.vname list;
    (** Same than below but without remapping on dependencies of the method from
        the inherited species. This is required to prevent dropping dependencies
        under the pretext they were not involved in the inherited method
        generator. This only serves to generate the extra parameters of the
        collection generator. *)
    ad_raw_dependencies_from_params :
      (species_param *
       ordered_methods_from_params)  (** The set of methods we depend on. *)
      list ;
  (** Dependencies on species parameters' methods. They are the union of:
        - dependencies found via [BODY] of definition 72 page 153 of Virgile
          Prevosto's Phd,
        - dependencies found via [TYPE] of definition 72 page 153 of Virgile
          Prevosto's Phd,
        - other dependencies found via [DEF-DEP], [UNIVERSE] and [PRM] of
          definition 72 page 153 of Virgile Prevosto's Phd + those found
          by the missing rule in Virgile Prevosto's Phd that temporarily
          named [DIDOU]. *)
    ad_dependencies_from_parameters :
      ((** The positional list of methods from the species parameters
           abstracted by lambda-lifting. *)
       species_param *
       (** The set of methods of this parameter on which we have
           dependencies. *)
       ordered_methods_from_params) list ;
    (** Same than above but only for dependencies arising through the type of
        the method. *)
    ad_dependencies_from_parameters_in_type :
      (species_param * ordered_methods_from_params) list ;
    (** Minimal Coq typing environnment. *)
    ad_min_coq_env : min_coq_env_element list ;
    (** Minimal Dedukti typing environnment. *)
    ad_min_dk_env : min_dk_env_element list
  }

  (* *********************************************************************** *)
  (** {b Desc} : Describes the essence of a species or collection. This
      description contains a flag telling if it's a species or a collection,
      the possible parameters of the species and a link to all its fields.

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type species_description = {
    spe_kind : Types.species_collection_kind;  (** Whether the
         species is a toplevel collection, a toplevel species or a collection
         parameter. *)
    spe_is_closed : bool;   (** Whether the species is fully defined, even if
         not turned into a collection. This information
         will be useful to known when collection
         generators must be created. *)
    spe_sig_params : species_param list;   (** Species parameters. *)
    (** Method's name, type and body if defined. *)
    spe_sig_methods : species_field list;
    (** The dependency graph of the methods of the species. *)
    spe_dep_graph : DepGraphData.name_node list ;  (* STILL USEFUL ? *)
    (** The positional list of methods from ourselves abstracted by
        lambda-lifting. *)
    spe_meths_abstractions : (Parsetree.vname * field_abstraction_info) list
    }



  (* ********************************************************************** *)
  (** {b Descr} : Because sum-type constructors are considered either with
      not parameter or with only ONE parameter (that can be a tuple), the
      arity of such a sum-type constructor is only given by the 2 following
      values.

      {b Exported} : Yes.                                                   *)
  (* ********************************************************************** *)
  type constructor_arity =
    | CA_zero   (** Constructor has no argument. *)
    | CA_some   (** Constructor has argument(s). *)



  (* ************************************************************************ *)
  (** {b Descr} : Description of a sum-type constructor. Contains it's
      arity and its type scheme. A constructor has a functionnal type whose
      "argument" is the tuple of types of the constructor's argument and
      whose "result" has the same type than the type the constructor belongs
      to.
      For instance: [type t = Foo of (int * char)] will lead to the
      constructor [Foo : (int * char) -> t].
      In the degenerated case of a constructor with only one effective
      argument, the type of the argument will be a tuple with only 1
      component.
      For instance: [type u = Bar of int] will lead to the constructor
      [Bar : (int) -> u].

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  type constructor_description = {
    (** Arity : 0 or 1 (many = 1 type tuple), (1 = type, not a 1 tuple). *)
    cstr_arity : constructor_arity;
    (** Full type scheme for this constructor, i.e (args ->) ty result. *)
    cstr_scheme : Types.type_scheme
  }



  (* *********************************************************************** *)
  (** {b Descr} : Tells is a record field is "mutable" (i.e. can be modified
      physically in place) or not.
      This is not yet really used in FoCaLize since mutable field are not
      available. All fields are non-mutable. It's just there in case for
      later...

      {b Exported} : Yes.                                                    *)
  (* *********************************************************************** *)
  type field_mutability =
     | FM_mutable      (** Field's content can be changed in place. *)
     | FM_immutable    (** Field's content can't be changed in place. *)



  (* ******************************************************************** *)
  (** {b Descr} : Information bound to record field labels in the typing
      environment. It descibes it mutability policy and its type scheme.
      A record field is typed as a function taking a parameter whose type
      is the type of the data stored in the field and returning a value
      having the type of the record.

      {b Exported} : Yes.                                                 *)
  (* ******************************************************************** *)
  type label_description = {
    field_mut : field_mutability;    (** Mutability for this field. *)
    (** Full type scheme for this field, i.e arg -> ty result. *)
    field_scheme : Types.type_scheme
  }


  (* ************************************************************************ *)
  (** {b Descr} : Describes the kind of a type definition. We currently
      support 4 kinds of type definitions: abstract, external, sum and
      record.
      Abstract types are either types whose value are fully hidden and that
      may be manipulated via dedicated functions knowing the representation of
      the values of these types. This may be builtin or external functions, but
      this also can be the methods of a collection in which the carrier type is
      abstracted. Type abbreviations (i.e. type definitions that do no lead to
      new values but that are only a way to give a name to a combination of
      existing types are also represented as "abstract".
      External types are types provided by code writen in a foreign target
      language. The description of how to map these types is contained in the
      types definitions themselves.
      Sum types (variant) are roughly like in OCaml, having their own defined
      value (value constructors) that can possibly be parameterised.
      Record types are roughly like in OCaml, having their own defined fields
      referenced by labels. The only diference is that in FoCaLiZe, fields
      are not mutable.

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  type type_kind =
    | TK_abstract       (** Abstract types and type abbreviations. *)
    | TK_external of    (** Abstract types externally defined. *)
        (Parsetree.external_translation *
                        (** On what to map the type constructor in
                            the external languages. *)
         Parsetree.external_mapping)
                        (** On what to map the type value
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



  (* ************************************************************************ *)
  (** {b Descr} : Information bound to a type name in the typing environment.
      It records the kind of the type defined, the location of the definition,
      the type scheme representing the internal type structure (in the
      compiler) of values having this type, the list of its parameters (in
      fact, the variables used inside the type scheme) and the number of
      parameters it has.

      {b Exported} : Yes.                                                     *)
  (* ************************************************************************ *)
  type type_description = {
    type_loc : Location.t;     (** The type definition's location. *)
    type_kind : type_kind;     (** Kind of the type definition. *)
    (** The type scheme representing to what this type is equal to. For
        instance in type 'a t = 'a list, t is TK_abstract with [type_identity]
        representing 'a list.
        If the type is a pure abstract like in type t, then t is TK_abstract
        with [type_identity] representing the type ST_construct ("t", []). *)
    type_identity : Types.type_scheme;
    (** Parameters of the type. Be careful, they are generalized at the same
        time that the above scheme [type_identity] is created. Hence, physical
        sharing exists and is crucial ! *)
    type_params : Types.type_simple list;
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
           | SPAR_in (a, _, _) ->
               Format.fprintf local_ppf "%a in ..." Sourcify.pp_vname a
           | SPAR_is ((modname, param_name), _, _, sp_expr, _) ->
               Format.fprintf local_ppf "%s.%s is %a" modname param_name
                 Sourcify.pp_simple_species_expr sp_expr);
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
              Sourcify.pp_qualified_species from.fh_initial_apparition;
            Format.fprintf ppf "sig %a : %a@\n"
              Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
        | SF_let (from, vname, _, ty_scheme, _, _, _, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition;
            Format.fprintf ppf "let %a : %a@\n"
              Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme
        | SF_let_rec rec_bounds -> (
            match rec_bounds with
             | [] -> assert false  (* Empty let rec is non sense ! *)
             | (from, vname, _, ty_scheme, _, _, _, _) :: rem ->
                 Format.fprintf ppf "(* From species %a. *)@\n"
                   Sourcify.pp_qualified_species from.fh_initial_apparition;
                 Format.fprintf ppf "let rec %a : %a@\n"
                   Sourcify.pp_vname vname Types.pp_type_scheme ty_scheme;
                 List.iter
                   (fun (local_from, v, _, s, _, _, _, _) ->
                     Format.fprintf ppf
                       "(* From species %a. *)@\n"
                       Sourcify.pp_qualified_species
                       local_from.fh_initial_apparition;
                     Format.fprintf ppf "and %a : %a@\n"
                       Sourcify.pp_vname v Types.pp_type_scheme s)
                   rem
           )
        | SF_theorem (from, vname, _, body, _, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition;
            Format.fprintf ppf "theorem %a : %a@\n"
              Sourcify.pp_vname vname Sourcify.pp_logical_expr body
        | SF_property (from, vname, _, body, _) ->
            Format.fprintf ppf "(* From species %a. *)@\n"
              Sourcify.pp_qualified_species from.fh_initial_apparition;
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



  let vname_of_species_param = function
    | SPAR_in (n, _, _) -> n
    | SPAR_is ((_, n), _, _, _, _) -> Parsetree.Vuident n

end
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* *********************************************************************** *)
(* {b Descr} : Tells if a "species" found in a code generation environment
   is a species or a collection. This is only used for printing purpose.

   {b Rem} : Exported outside this module.                                 *)
(* *********************************************************************** *)
type collection_or_species =
  | COS_collection
  | COS_species
;;



type method_type_kind =
  | MTK_computational of Types.type_scheme
  | MTK_logical of Parsetree.logical_expr
;;



(* ************************************************************************ *)
(** {b Descr} : Common for OCaml, Coq and Dedukti code generation environments.
    This represent various information about the methods, their abstraction,
    their body, their type scheme.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
type generic_code_gen_method_info = {
  mi_name : Parsetree.vname;        (** The field name. *)
  mi_history : from_history;        (** The field inheritance history. *)
  (** The "type" of the method, i.e. a ML-like type if computational or a
      logical property if logical. *)
  mi_type_kind : method_type_kind;
  (** The positional list of parameters carrier abstracted in the method. *)
  mi_used_species_parameter_tys : Parsetree.vname list;
  mi_dependencies_from_parameters :
    ((** The positional list of methods from the species parameters
         abstracted by lambda-lifting. *)
     TypeInformation.species_param *
     (* The set of methods of this parameter on which we have dependencies. *)
     ordered_methods_from_params) list;
  (* Same than above but only for dependencies arising through the type of the
     method. *)
  mi_dependencies_from_parameters_in_type :
    (TypeInformation.species_param * ordered_methods_from_params) list ;
  mi_abstracted_methods : Parsetree.vname list   (** The positional list
      of methods from ourselves abstracted by lambda-lifting. *)
}
;;



module MlGenInformation = struct
  type collection_generator_info = {
    (** The list of species parameters names and kinds the species whose
        collection generator belongs to has. This list is positional, i.e.
        that the first name of the list is the name of the first species
        parameter and so on. *)
    cgi_implemented_species_params_names :
      (Parsetree.vname * ScopeInformation.species_parameter_kind) list;
    (** The list mapping for each parameter name, the set of methods the
        collection generator depends on, hence must be provided an instance
        to be used. Note that the list is not guaranted to be ordered
        according to the order of the species parameters names (that's why
        we have the information about this order given in
        [species_binding_info]). *)
    cgi_generator_parameters :
      (Parsetree.vname * ordered_methods_from_params) list
  }

  type method_info = generic_code_gen_method_info

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
  type label_mapping_info = Parsetree.external_translation_desc

  (** The list of mappings according to external languages to know to which
      string the sum type constructor corresponds. For instance, in Caml,
      "Nil" will be mapped onto "[]" and "Cons" to "( :: )".  For Ocaml,
      only constructors coming from "external" sum types are entered in the
      generation environment. Hence, if a constructor is not found, then this
      means that it comes from a regular FoCaL type definition, not dealing
      with any external material. *)
  type constructor_mapping_info = Parsetree.external_translation_desc


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
  type collection_generator_parameters = {
    (* The parameters carriers that have been abstracted in the record type.
       They must be instancied each time one need to build a record value
       or a record type. This list contains the crude names of the
       parameters that can be found in the species declaration. *)
    cgp_abstr_param_carriers_for_record : Parsetree.vname list;
    (* The list of species parameters with their methods the record type
       depends on (hence was abstracted with). *)
    cgp_abstr_param_methods_for_record :
      (Parsetree.vname * ordered_methods_from_params) list;
    (* The list of species parameters with their methods the collection
       generator depends on (hence was abstracted with). *)
    cgp_abstr_param_methods_for_coll_gen :
      (Parsetree.vname * ordered_methods_from_params) list
    }

  type collection_generator_info = {
    (** The list of species parameters names and kinds the species whose
        collection generator belongs to has. This list is positional, i.e.
        that the first name of the list is the name of the first species
        parameter and so on. *)
    cgi_implemented_species_params_names :
      (Parsetree.vname * ScopeInformation.species_parameter_kind) list;
    (** First, the list of species parameters carriers required by the
        mk_record. Third, the list mapping for each parameter name,
        the set of methods the collection generator depends on, hence must be
        provided an instance to be used. Note that the list is not guaranted
        to be ordered according to the order of the species parameters names
        (that's why we have the information about this order given in
        [species_binding_info]). *)
    cgi_generator_parameters : collection_generator_parameters
  }

  (** In Coq generation environment ALL the sum types value constructors are
      entered in the environment because we always need to know their number
      of extra leading "_" due to polymorphics. If the constructor does not
      have an external mapping, we simply put "None" in the field
      [cmi_external_translation]. *)
  type constructor_mapping_info = {
    (** The number of extra argument the constructor has due to its
        polymorphism. *)
    cmi_num_polymorphics_extra_args : int ;
    cmi_external_translation : Parsetree.external_translation_desc option
    }

  (** The list of mappings according to external languages to know to which
      string the record type field name corresponds. *)
  type label_mapping_info = {
    (** The number of extra argument the label has due to its
        polymorphism. *)
    lmi_num_polymorphics_extra_args : int ;
    lmi_external_translation : Parsetree.external_translation_desc option
  }

  type method_info = generic_code_gen_method_info

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

  type value_body =
    | VB_non_toplevel
    | VB_toplevel_let_bound of
        (rec_status * (Parsetree.vname list) * Types.type_scheme *
         Parsetree.binding_body)
    | VB_toplevel_property of Parsetree.logical_expr

  type value_mapping_info = (int * (** The number of polymorphic type variables
                                       in the scheme of the ident. This will
                                       lead to extra "_" following the ident
                                       when it is used in applicative
                                       position. *)
                             value_body) (** The expression bound to the
                                             ident. *)


  type type_info = TypeInformation.type_description

  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
      scoping environments.

      {b Exported} : No.                                            *)
  (* ************************************************************** *)
  type env =
    (constructor_mapping_info, label_mapping_info, type_info,
     value_mapping_info, species_binding_info) generic_env
end
;;

(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)
module DkGenInformation = struct
  type collection_generator_parameters = {
    (* The parameters carriers that have been abstracted in the record type.
       They must be instancied each time one need to build a record value
       or a record type. This list contains the crude names of the
       parameters that can be found in the species declaration. *)
    cgp_abstr_param_carriers_for_record : Parsetree.vname list;
    (* The list of species parameters with their methods the record type
       depends on (hence was abstracted with). *)
    cgp_abstr_param_methods_for_record :
      (Parsetree.vname * ordered_methods_from_params) list;
    (* The list of species parameters with their methods the collection
       generator depends on (hence was abstracted with). *)
    cgp_abstr_param_methods_for_coll_gen :
      (Parsetree.vname * ordered_methods_from_params) list
    }

  type collection_generator_info = {
    (** The list of species parameters names and kinds the species whose
        collection generator belongs to has. This list is positional, i.e.
        that the first name of the list is the name of the first species
        parameter and so on. *)
    cgi_implemented_species_params_names :
      (Parsetree.vname * ScopeInformation.species_parameter_kind) list;
    (** First, the list of species parameters carriers required by the
        mk_record. Third, the list mapping for each parameter name,
        the set of methods the collection generator depends on, hence must be
        provided an instance to be used. Note that the list is not guaranted
        to be ordered according to the order of the species parameters names
        (that's why we have the information about this order given in
        [species_binding_info]). *)
    cgi_generator_parameters : collection_generator_parameters
  }

  (** In Dk generation environment ALL the sum types value constructors are
      entered in the environment because we always need to know their number
      of extra leading "_" due to polymorphics. If the constructor does not
      have an external mapping, we simply put "None" in the field
      [cmi_external_translation]. *)
  type constructor_mapping_info = {
    (** The number of extra argument the constructor has due to its
        polymorphism. *)
    cmi_num_polymorphics_extra_args : int ;
    cmi_external_translation : Parsetree.external_translation_desc option
    }

  (** The list of mappings according to external languages to know to which
      string the record type field name corresponds. *)
  type label_mapping_info = {
    (** The number of extra argument the label has due to its
        polymorphism. *)
    lmi_num_polymorphics_extra_args : int ;
    lmi_external_translation : Parsetree.external_translation_desc option
  }

  type method_info = generic_code_gen_method_info

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

  (* ************************************************************************ *)
  (** {b Descr}: Describes the kind of recursion, i.e. termination proof,
      provided to a recursive definition. Currently, we only make the
      difference between a structural termination and none/other proofs.
      In case of structural termination we assume that the definition was
      generated using "Fixpoint" using the provided parameter name as
      decreasing argument. In any other case, we assume it has been generated
      with "Function".
      Note that this type may change/disapear when we will have a more unified
      code generation model for recursion.

      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  type rec_proof_kind =
    | RPK_struct of Parsetree.vname
    | RPK_other



  (* ************************************************************************ *)
  (** {b Descr}: Tells if a definition is recursive or not. Allows embedding
      the kind of termination proof the definition has if it as one.
      Since we currently have 2 Dk generation models: "Fixpoint" and "Function"
      we need to remind which one was used in case a proof is done
      "by definition" of a recursive definition. In effect, depending on the
      used model, we must not generate the same code for Zenon.

      {b Visibility}: Exported outside this module.                           *)
  (* ************************************************************************ *)
  type rec_status =
    | RC_non_rec
    | RC_rec of rec_proof_kind


  type value_body =
    | VB_non_toplevel
    | VB_toplevel_let_bound of
        (rec_status * (Parsetree.vname list) * Types.type_scheme *
         Parsetree.binding_body)
    | VB_toplevel_property of Parsetree.logical_expr

  type value_mapping_info = (int * (** The number of polymorphic type variables
                                       in the scheme of the ident. This will
                                       lead to extra "_" following the ident
                                       when it is used in applicative
                                       position. *)
                             value_body) (** The expression bound to the
                                             ident. *)


  type type_info = TypeInformation.type_description

  (* ************************************************************** *)
  (** {b Descr} : Type abbreviation to shorten the structure of the
      scoping environments.

      {b Exported} : No.                                            *)
  (* ************************************************************** *)
  type env =
    (constructor_mapping_info, label_mapping_info, type_info,
     value_mapping_info, species_binding_info) generic_env
end
;;




(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* ************************************************************************ *)
(* {b Descr} : Exception raised when one tries to use an OCaml code
   generation environment although the corresponding file has been compiled
   without OCaml code generation enabled.

   {b Exported} : Yes.                                                      *)
(* ************************************************************************ *)
exception No_available_OCaml_code_generation_envt of Types.fname;;



(* ************************************************************************* *)
(* {b Descr} : Exception raised when one tries to use a Coq code generation
   environment although the corresponding file has been compiled without Coq
   code generation enabled.

   {b Exported} : Yes.                                                       *)
(* ************************************************************************* *)
exception No_available_Coq_code_generation_envt of Types.fname;;

(* ************************************************************************* *)
(* {b Descr} : Exception raised when one tries to use a Dk code generation
   environment although the corresponding file has been compiled without Dk
   code generation enabled.

   {b Exported} : Yes.                                                       *)
(* ************************************************************************* *)
exception No_available_Dk_code_generation_envt of Types.fname;;



(* ************************************************************************** *)
(** {b Descr} : Struture on disk that records the "object" file once a source
    file is compiled. "Object" file reminds the scoping, typing environments
    and the OCaml/Coq/Dedukti code generation environments if the source file
    was compiled with this target code generation enabled.

    {b Exported} : Abstract.                                                  *)
(* ************************************************************************** *)
type fo_file_structure = {
  ffs_scoping : ScopeInformation.env;
  ffs_typing : TypeInformation.env;
  (* Optional since the file may be compiled without OCaml code generation. *)
  ffs_mlgeneration : MlGenInformation.env option;
  (* Optional since the file may be compiled without Coq code generation. *)
  ffs_coqgeneration : CoqGenInformation.env option;
  (* Optional since the file may be compiled without Dedukti code generation. *)
  ffs_dkgeneration : DkGenInformation.env option}
;;



let (scope_find_module, type_find_module,
     mlgen_find_module, coqgen_find_module,
     dkgen_find_module,
     scope_open_module, type_open_module,
     mlgen_open_module, coqgen_open_module,
     dkgen_open_module) =
  (* Let's just make the list used to bufferize opened files' content.
     Because ".fo" files contains always both the scoping and typing
     information, once loaded for scoping purpose, the typing info and ml code
     generation information are made available. Hence, the buffer list contains
     couples with no optional component. *)
  let buffered = ref ([] : (Types.fname * fo_file_structure) list) in



  (* ***************************************************************** *)
  (* loc: Location.t -> Types.fname ->                                 *)
  (*   (ScopeInformation.env * TypeInformation.env)                    *)
  (** {b Descr} : Wrapper to lookup inside an external interface file.
      The lookup also performs a bufferisation to prevent futher calls
      from accessing again the disk. This should enhance lookup speed.

      {b Exported} : No.                                               *)
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
          close_in in_file;
          (* If the interface was found, buferize it for further uses. *)
          buffered := (fname, envts) :: !buffered;
          envts
          end)
        else
          (begin
          close_in in_file;
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
      found in an opened "module"'s environment. Opened bindings are
      transformed in BO_opened before being added in head to the initial
      environment.

      {b Args} :
        - from_fname : The "module" name from where the loaded environment
                     was found.
        - loaded_env : The loaded environment. Not that it shoud contain
                     only [BO_absolute] bindings.
        - env : The environment to extend.

      {b Rem} : Not exported outside this module.                         *)
  (* ******************************************************************** *)
  let internal_extend_env from_fname loaded_env env =
    (* Local function to tranform [BO_absolute] into [BO_opened]. Note that
       loaded environments should never contain [BO_opened] tagged bindings. *)
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



  ((* ****************************************************************** *)
   (* scope_find_module                                                  *)
   (* loc: Location.t -> current_unit: Types.fname ->                    *)
   (*    Types.fname option -> ScopeInformation.env ->                   *)
   (*      ScopeInformation.env                                          *)
   (** {b Descr} : Wrapper to lookup a scoping environment inside an
       external interface file. Note that if it is requested to lookup
       inside the current compilation unit's environment (the current
       file has the same name than the looked-up module), then returned
       environment is the one initially passed as argument.

       {b Rem} : Not exported outside this module.                       *)
   (* ****************************************************************** *)
   (fun ~loc ~current_unit fname_opt scope_env ->
    match fname_opt with
     | None -> scope_env
     | Some fname ->
         if current_unit = fname then scope_env
         else (internal_find_module ~loc fname).ffs_scoping),



   (* ****************************************************************** *)
   (* type_find_module                                                   *)
   (* loc: Location.t -> current_unit: Types.fname ->                    *)
   (*   Types.fname option -> TypeInformation.env ->                     *)
   (*     TypeInformation.env                                            *)
   (** {b Descr} : Wrapper to lookup a typing environment inside an
       external interface file. Note that if it is requested to lookup
       inside the current compilation unit's environment (the current
       file has the same name than the looked-up module), then returned
       environment is the one initiallt passed as argument.

       {b Rem} : Not exported outside this module.                       *)
   (* ****************************************************************** *)
   (fun ~loc ~current_unit fname_opt type_env ->
     match fname_opt with
      | None -> type_env
      | Some fname ->
          if current_unit = fname then type_env
          else (internal_find_module ~loc fname).ffs_typing),



   (* ********************************************************************* *)
   (* mlgen_find_module                                                     *)
   (*   loc: Location.t -> current_unit: Types.fname ->                     *)
   (*   Types.fname option -> MlGenInformation.env ->                       *)
   (*     MlGenInformation.env                                              *)
   (** {b Descr} : Wrapper to lookup a ml generation environment inside
       an external interface file. Note that if it is requested to lookup
       inside the current compilation unit's environment (the current file
       has the same name than the looked-up module), then returned
       environment is the one initially passed as argument.

       {b Rem} : Not exported outside this module.                          *)
   (* ********************************************************************* *)
   (fun ~loc ~current_unit fname_opt mlgen_env ->
     match fname_opt with
      | None -> mlgen_env
      | Some fname ->
          if current_unit = fname then mlgen_env
          else
            match (internal_find_module ~loc fname).ffs_mlgeneration with
             | None ->
                 raise (No_available_OCaml_code_generation_envt fname)
             | Some e -> e),



   (* ********************************************************************* *)
   (* coqgen_find_module                                                    *)
   (*   loc: Location.t -> current_unit: Types.fname ->                     *)
   (*   Types.fname option -> CoqGenInformation.env ->                      *)
   (*     CoqGenInformation.env                                             *)
   (** {b Descr} : Wrapper to lookup a ml generation environment inside
       an external interface file. Note that if it is requested to lookup
       inside the current compilation unit's environment (the current file
       has the same name than the looked-up module), then returned
       environment is the one initially passed as argument.

       {b Rem} : Not exported outside this module.                          *)
   (* ********************************************************************* *)
   (fun ~loc ~current_unit fname_opt coqgen_env ->
     match fname_opt with
      | None -> coqgen_env
      | Some fname ->
          if current_unit = fname then coqgen_env
          else
            match (internal_find_module ~loc fname).ffs_coqgeneration with
             | None -> raise (No_available_Coq_code_generation_envt fname)
             | Some e -> e),

   (* ********************************************************************* *)
   (* dkgen_find_module                                                    *)
   (*   loc: Location.t -> current_unit: Types.fname ->                     *)
   (*   Types.fname option -> DkGenInformation.env ->                      *)
   (*     DkGenInformation.env                                             *)
   (** {b Descr} : Wrapper to lookup a ml generation environment inside
       an external interface file. Note that if it is requested to lookup
       inside the current compilation unit's environment (the current file
       has the same name than the looked-up module), then returned
       environment is the one initially passed as argument.

       {b Rem} : Not exported outside this module.                          *)
   (* ********************************************************************* *)
   (fun ~loc ~current_unit fname_opt dkgen_env ->
     match fname_opt with
      | None -> dkgen_env
      | Some fname ->
          if current_unit = fname then dkgen_env
          else
            match (internal_find_module ~loc fname).ffs_dkgeneration with
             | None -> raise (No_available_Dk_code_generation_envt fname)
             | Some e -> e),



   (* *********************************************************************** *)
   (* scope_open_module                                                       *)
   (* loc: Location.t -> Types.fname -> ScopingEnv.t -> ScopingEnv.t          *)
   (** {b Descr} : Performs a full "open" directive on a scoping environment.
       It add in head of the environment the bindings found in the "module"
       content, tagging them as beeing "opened".

       {b Rem} : Exported outside this module.                                *)
   (* *********************************************************************** *)
   (fun ~loc fname env ->
     let loaded_scope_env = (internal_find_module ~loc fname).ffs_scoping in
     internal_extend_env fname loaded_scope_env env),



   (* ********************************************************************* *)
   (* type_open_module                                                      *)
   (* loc: Location.t -> Types.fname -> TypingEnv.t -> TypingEnv.t          *)
   (** {b Descr} : Performs a full "open" directive on a typing
       environment. It add in head of the environment the bindings found in
       the "module" content, tagging them as beeing "opened".

       {b Rem} : Exported outside this module.                              *)
   (* ********************************************************************* *)
   (fun ~loc fname env ->
     let loaded_type_env = (internal_find_module ~loc fname).ffs_typing in
     internal_extend_env fname loaded_type_env env),



   (* ********************************************************************* *)
   (* mlgen_open_module                                                     *)
   (*   loc: Location.t -> Types.fname -> MlGenEnv.t -> MlGenEnv.t          *)
   (** {b Descr} : Performs a full "open" directive on a ml generation
       environment. It add in head of the environment the bindings found in
       the "module" content, tagging them as beeing "opened".

       {b Rem} : Exported outside this module.                              *)
   (* ********************************************************************* *)
   (fun ~loc fname env ->
     let loaded_mlgen_env =
       match (internal_find_module ~loc fname).ffs_mlgeneration with
        | None ->
            raise (No_available_OCaml_code_generation_envt fname)
        | Some e -> e in
     internal_extend_env fname loaded_mlgen_env env),



   (* ****************************************************************** *)
   (* coqgen_open_module                                                 *)
   (*   loc: Location.t -> Types.fname ->  CoqGenEnv.t -> CoqGenEnv.t    *)
   (** {b Descr} : Performs a full "open" directive on a coq generation
       environment. It add in head of the environment the bindings found
       in the "module" content, tagging them as beeing "opened".

       {b Rem} : Exported outside this module.                           *)
   (* ****************************************************************** *)
   (fun ~loc fname env ->
     let loaded_coqgen_env =
       match (internal_find_module ~loc fname).ffs_coqgeneration with
        | None -> raise (No_available_Coq_code_generation_envt fname)
        | Some e -> e in
     internal_extend_env fname loaded_coqgen_env env),


   (* ****************************************************************** *)
   (* dkgen_open_module                                                 *)
   (*   loc: Location.t -> Types.fname ->  DkGenEnv.t -> DkGenEnv.t    *)
   (** {b Descr} : Performs a full "open" directive on a dk generation
       environment. It add in head of the environment the bindings found
       in the "module" content, tagging them as beeing "opened".

       {b Rem} : Exported outside this module.                           *)
   (* ****************************************************************** *)
   (fun ~loc fname env ->
     let loaded_dkgen_env =
       match (internal_find_module ~loc fname).ffs_dkgeneration with
        | None -> raise (No_available_Dk_code_generation_envt fname)
        | Some e -> e in
     internal_extend_env fname loaded_dkgen_env env)

  )
;;



(* ************************************************************************* *)
(* 'a -> 'a option -> bool                                                   *)
(** {b Descr} : Returns a boolean telling whether the lookup of a QUALIFIED
    vname whose we have the ** optional qualification** as argument in and
    environment can succeed on a binding induced by an "open" directive.
    This is the case except in the 2 following cases:
      - if the [ident] from where the [vname] is extracted is an
        I_global (None, ...). This corresponds to an [ident] like #Foo,
        hence meaning that on search for something at toplevel of the
        current compilation unit. Hence the right binding cannot be
        something "Opened".
      - if the [ident] from where the [vname] is extracted is an
        I_global (Some fname, ...) with current_unit = fname. This
        corresponds to an [ident] like doudou#Foo while we are analysing the
        file "doudou". It's then also a search for something at toplevel of
        the current compilation unit. Hence the right binding cannot also be
        something "Opened".

    {b Rem} : Not exported outside this module.                              *)
(* ************************************************************************* *)
let allow_opened_p current_unit = function
  | None -> false
  | Some fname -> current_unit = fname
;;



(* *********************************************************************** *)
(* *********************************************************************** *)
(* *********************************************************************** *)



(* ************************************************************************* *)
(** {b Descr} : Type of modules used to encapsulate the function allowing
    to access the environment related to a FoCaL "module" via the persistent
    data located on the disk.
    This kind of module will be used as argument of the functor [Make] below
    in order to generate the environment access functions without having to
    add to them extra parameters that would the [find_module], [pervasives],
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
    This module signature is fully local to this file, it will be internally
    be used to create the various environment structures (scoping, typing)
    we need.                                                                 *)
(* ************************************************************************* *)
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



(* *********************************************************************** *)
(** {b Descr} : This functor creates the environment manipulation
    primitives (lookup and insertion), using the [find_module] function of
    the argument [EMAccess] to access FoCal "modules" persistent data on
    disk.

    {b Rem} : This functor is fully local to this file, it will be
    internally be used to create the various environment structures
    (scoping, typing) we need.

    {b Exported} : No.                                                     *)
(* *********************************************************************** *)
module Make(EMAccess : EnvModuleAccessSig) = struct
  (* ************************************************ *)
  (** {b Descr} : The type of the environment itself.

      {b Exported} : Yes.                             *)
  (* ************************************************ *)
  type t =
    (EMAccess.constructor_bound_data, EMAccess.label_bound_data,
     EMAccess.type_bound_data, EMAccess.value_bound_data,
     EMAccess.species_bound_data) generic_env

  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh TOTALY empty environment
      (no even pervasive stuff inside).

      {b Exported} : Yes.                                  *)
  (* ***************************************************** *)
  let empty () =
    ({ constructors = []; labels = []; types = []; values = [];
      species = [] } : t)


  (* ***************************************************** *)
  (* unit -> t                                             *)
  (** {b Descr} : Creates a fresh environment containing
      information bound the basic builtins.

      {b Exported} : Yes.                                  *)
  (* ***************************************************** *)
  let pervasives () = EMAccess.pervasives ()



  (* loc: Location.t -> Parsetree.vname -> EMAccess.species_bound_data -> *)
  (*   t -> t                                                             *)
  let add_species ~loc species_name data (env : t) =
    (* Ensure the species name does not already exists in the current module.
       This means that this name must not be already bound to a
       [BO_absolute]. *)
    if List.exists
        (function (n, (BO_absolute _)) -> n = species_name | _ -> false)
        env.species then
      raise (Rebound_species (species_name, loc));
    ({ env with
       species = (species_name, BO_absolute data) :: env.species } : t)


  let opt_scope_vname = function
    | Parsetree.Vname vname -> (None, vname)
    | Parsetree.Qualified (modname, vname) -> ((Some modname), vname)



  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname -> *)
  (*   t -> EMAccess.species_bound_data                          *)
  let find_species_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.species with
    | Not_found -> raise (Unbound_species (vname, loc))


  (* ********************************************************************* *)
  (** {b Descr} : Acts like [find_species_vname] but returns the whole
       environment information (i.e. tagged by [BO_absolute] or
       [BO_opened]).
       This is used uniquely while using find_value on a
       Parsetree.EI_method (_, _) to be able to know when one
       [find_species_vname] before making
       [make_value_env_from_species_methods] if the species was reacher via
       an "open" or not. This is especially important when scoping since
       the method depicted by the EI_method because, we will have to say
       (via [make_value_env_from_species_methods]) that this method comes
       from its species but that its species, may not come from the
       current compilation unit even if in its name there is no module
       qualification (the "open" will have done so, that the module
       qualification is not useful).And, since the function [find_value] is
       for all the environments, we can't "use" the scoping environment and
       information to know if the species is [SPBI_local] or [SPBI_file]
        yet.

     {b Exported} : No.                                                    *)
  (* ********************************************************************* *)
  let find_species_vname_and_binding_origin ~loc ~allow_opened vname (env : t) =
    let rec rec_assoc = function
      | [] -> raise (Unbound_species (vname, loc))
      | (n, data) :: q ->
          if n = vname then
            (begin
            match data with
             | BO_opened (_, _) ->
                 if allow_opened then data else rec_assoc q
             | BO_absolute _ -> data
            end)
          else rec_assoc q in
    rec_assoc env.species



  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> *)
  (*    t -> EMAccess.species_bound_data                                *)
  let find_species ~loc ~current_unit coll_ident (env : t) =
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



  (* ******************************************************************** *)
  (* toplevel: Location.t option -> Parsetree.vname ->                    *)
  (*   EMAccess.value_bound_data ->                                       *)
  (*   t -> t                                                             *)
  (** {b Descr} : Returns an environment extended with a binding between
      a value [ident] and the argument [data].
      The initial environment is passed as last argument.

      {b Exported} : Yes.                                                 *)
  (* ******************************************************************** *)
  let add_value ~toplevel ident data (env : t) =
    (* We disallow toplevel let-definitions to have the same name. So if we
       are told we are in the case of toplevel definition (i.e. if we are
       given a location that may be used to raise the error giving where it
       arises), we perform the non-existence verification. *)
    (match toplevel with
     | Some loc ->
         if List.exists
             (function (n, (BO_absolute _)) -> n = ident | _ -> false)
             env.values then
           raise (Rebound_toplevel_let (ident, loc))
     | None -> ());
    ({ env with values = (ident, BO_absolute data) :: env.values } : t)



  (* ******************************************************************* *)
  (* loc: Location.t -> current_unit: Types.fname ->                     *)
  (*   current_species_name: string option ->                            *)
  (*     Parsetree.expr_ident -> t -> EMAccess.value_bound_data          *)
  (** {b Descr} : Looks-up for an [ident] inside the values environment.

      {b Exported} : Yes.                                                *)
  (* ******************************************************************* *)
  let rec find_value ~loc ~current_unit ~current_species_name ident_ident
      (env : t) =
    match ident_ident.Parsetree.ast_desc with
     | Parsetree.EI_local vname ->
         (* No explicit scoping information was provided, hence opened modules
            bindings are acceptable. *)
         find_value_vname ~loc ~allow_opened: true vname env
     | Parsetree.EI_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something coming from an opened
            module. *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_value_vname ~loc ~allow_opened vname env'
     | Parsetree.EI_method (None, vname) ->
         (* No collection scope. Then the searched ident must belong to the
            inheritance of Self. First, this means that opened stuff is
            forbidden. Next, because the [values] bucket is so that inherited
            methods and our methods belong to it, we just have to search for
            the [vname] inside the current environment. *)
         find_value_vname ~loc ~allow_opened: false vname env
     | Parsetree.EI_method
         (Some (Parsetree.Vname (Parsetree.Vuident "Self")), vname) ->
         (* Like if there was no collection scope (see above). *)
         find_value_vname ~loc ~allow_opened: false vname env
     | Parsetree.EI_method (Some coll_specifier, vname) ->
         (begin
         (* We handle the case where the searched ident is qualified but the
            qualification represents the current species in the current unit.
            This may arise because of substitution performed during typechecking
            in species signatures. *)
         let ignore_qualification =
           (match current_species_name with
            | None -> false
            | Some c_s_n ->
                (match coll_specifier with
                 | Parsetree.Vname (Parsetree.Vuident species_name) ->
                     species_name = c_s_n
                 | Parsetree.Qualified
                       (modname, (Parsetree.Vuident coll_vname)) ->
                     coll_vname = c_s_n && modname = current_unit
                 | _ -> false)) in
         if ignore_qualification then
           (* Like if there was no collection scope (see above). *)
           find_value_vname ~loc ~allow_opened: false vname env
         else
           (begin
           let (opt_module_qual, coll_vname) =
             match coll_specifier with
              | Parsetree.Vname coll_vname -> (None, coll_vname)
              | Parsetree.Qualified (modname, coll_vname) ->
                  ((Some modname), coll_vname) in
           (* Recover the environment in where to search,according to if the
              species is qualified by a module name. In this environment, all
              the imported bindings are tagged [BO_absolute]. *)
           let env' =
             EMAccess.find_module
               ~loc ~current_unit opt_module_qual env in
           (* Check if the lookup can return something coming from an opened
              module. *)
           let allow_opened =
             (match opt_module_qual with
                None -> true | Some fname -> current_unit = fname) in
           (* First, we search the collection. *)
           let coll_info =
             find_species_vname_and_binding_origin
               ~loc ~allow_opened  coll_vname env' in
           (* We must now understand if the collection was found via "open" or
              not. If yes, then the methods we will import will also have to be
              considered by the post-process
              [make_value_env_from_species_methods] as coming not from
              [tmp_full_coll_name] but from the species qualified by the
              "opened" module that made it visible without qualification. *)
           let (methods_info, real_full_coll_name) =
             match coll_info with
              | BO_absolute meths_i ->
                  (meths_i, (Parsetree.Qualified (current_unit, coll_vname)))
              | BO_opened (file, meths_i) ->
                  (meths_i, (Parsetree.Qualified (file, coll_vname))) in
           (* We must now look inside collections and species for the
              [coll_vname] in order to recover its methods. *)
           let available_meths =
             EMAccess.make_value_env_from_species_methods
               real_full_coll_name methods_info in
           let data =
             find_value_vname
               ~loc ~allow_opened vname available_meths in
           (* Now we apply the post-processing on the found data. *)
           EMAccess.post_process_method_value_binding
             real_full_coll_name data
           end)
         end)


  (* ****************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname -> t ->   *)
  (*   EMAccess.value_bound_data                                        *)
  (** {b Descr} : Looks-up for a [vname] inside the values environment.

      {b Rem} : Not exported outside this module.                       *)
  (* ****************************************************************** *)
  and find_value_vname ~loc ~allow_opened vname (env : t) =
(*
    List.iter (fun t -> match fst(t) with
   | Parsetree.Vlident s -> print_string (s ^ "\n")
   | Parsetree.Vuident s -> print_string (s ^ "\n")
   | Parsetree.Vpident s -> print_string (s ^ "\n")
   | Parsetree.Viident s -> print_string (s ^ "\n")
   | Parsetree.Vqident s -> print_string (s ^ "\n") ) env.values;
*)
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
    (* Just mask the previous [cstr_ident] to simply remove the only possible
       constructor [CI], and to get the interesting information. *)
    let Parsetree.CI cstr_ident = cstr_ident.Parsetree.ast_desc in
    match cstr_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence opened modules
            bindings are acceptable. *)
         find_constructor_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something coming from an opened
            module. *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_constructor_vname ~loc ~allow_opened vname env'



  (* *********************************************************** *)
  (* allow_opened: loc: Location.t -> bool ->                    *)
  (*   Parsetree.constructor_name -> t ->                        *)
  (*     EMAccess.constructor_bound_data                         *)
  (** {b Descr} : Looks-up for a [vname] inside the constructors
      environment.

      {b Exported} : No.                                         *)
  (* *********************************************************** *)
  and find_constructor_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.constructors
    with Not_found -> raise (Unbound_constructor (vname, loc))



  (* ******************************************************************** *)
  (* Parsetree.vname -> EMAccess.label_bound_data -> t -> t               *)
  (** {b Descr} : Return an environment extended with a binding between a
      record field label [lbl_vname] and the argument [data].
      The initial environment is passed as last argument.

      {b Exported} : Yes.                                                 *)
  (* ******************************************************************** *)
  let add_label lbl_vname data (env : t) =
    ({ env with labels = (lbl_vname, BO_absolute data) :: env.labels } : t)



  (* ************************************************************* *)
  (* loc: Location.t -> Parsetree.label_ident -> t ->              *)
  (*   EMAccess.label_bound_data                                   *)
  (** {b Descr} : Looks-up for an [ident] inside the record fields
      labels environment.

      {b Exported} : Yes.                                          *)
  (* ************************************************************* *)
  let rec find_label ~loc ~current_unit lbl_ident (env : t) =
    (* Just mask the previous [lbl_ident] to simply remove the only possible
       constructor [LI], and to get the interesting information. *)
    let Parsetree.LI lbl_ident = lbl_ident.Parsetree.ast_desc in
    match lbl_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence opened modules
            bindings are acceptable. *)
         find_label_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something coming from an opened
            module. *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_label_vname ~loc ~allow_opened vname env'



  (* ***************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname ->       *)
  (*   Parsetree.vname -> t -> EMAccess.label_bound_data               *)
  (** {b Descr} : Looks-up for a [vname] inside the record type labels
      environment.

      {b Exported} : No.                                               *)
  (* ***************************************************************** *)
  and find_label_vname ~loc ~allow_opened vname (env : t) =
    try env_list_assoc ~allow_opened vname env.labels with
    | Not_found -> raise (Unbound_label (vname, loc))



  (* ******************************************************************** *)
  (* loc: Location.t -> Types.type_name -> EMAccess.type_bound_data ->    *)
  (*   t -> t                                                             *)
  (** {b Descr} : Return an environment extended with a binding between a
      type [ident] and the argument [data].
      The initial environment is passed as last argument.

      {b Exported} : Yes.                                                 *)
  (* ******************************************************************** *)
  let add_type ~loc tyname data (env : t) =
    (* Ensures the type name does not already exists in the current module.
       This means that this name must not be already bound to a
       [BO_absolute]. *)
    if List.exists
        (function (n, (BO_absolute _)) -> n = tyname | _ -> false)
        env.types then
      raise (Rebound_type (tyname, loc));
    ({ env with types = (tyname, BO_absolute data) :: env.types } : t)



  (* ****************************************************************** *)
  (* loc: Location.t -> current_unit: Types.fname -> Parsetree.ident -> *)
  (*   t -> EMAccess.type_bound_data                                    *)
  (** {b Descr} : Looks-up for an [ident] inside the types environment.

      {b Exported} : Yes.                                               *)
  (* ****************************************************************** *)
  let rec find_type ~loc ~current_unit type_ident (env : t) =
    match type_ident.Parsetree.ast_desc with
     | Parsetree.I_local vname ->
         (* No explicit scoping information was provided, hence opened modules
            bindings are acceptable. *)
         find_type_vname ~loc ~allow_opened: true vname env
     | Parsetree.I_global qvname ->
         let (opt_scope, vname) = opt_scope_vname qvname in
         let env' = EMAccess.find_module ~loc ~current_unit opt_scope env in
         (* Check if the lookup can return something coming from an opened
            module. *)
         let allow_opened = allow_opened_p current_unit opt_scope in
         find_type_vname ~loc ~allow_opened vname env'



  (* **************************************************************** *)
  (* loc: Location.t -> allow_opened: bool -> Parsetree.vname ->      *)
  (*   Parsetree.vname -> t -> EMAccess.type_bound_data               *)
  (** {b Descr} : Looks-up for a [vname] inside the types environment.

      {b Rem} : Not exported outside this module.                     *)
  (* **************************************************************** *)
  and find_type_vname ~loc ~allow_opened vname (env : t) =
    (*
    print_string "Types : ";
    List.iter (fun (x,_) -> match x with | Parsetree.Vlident s |
    Parsetree.Vuident s | Parsetree.Vpident s |
    Parsetree.Viident s | Parsetree.Vqident s -> print_string s) env.types;
    print_string "\n";
    print_string "constructors : ";
    List.iter (fun (x,_) -> match x with | Parsetree.Vlident s |
    Parsetree.Vuident s | Parsetree.Vpident s |
    Parsetree.Viident s | Parsetree.Vqident s -> print_string s)
    env.constructors;
    print_string "\n";
    print_string "values : ";
    List.iter (fun (x,_) -> match x with | Parsetree.Vlident s |
    Parsetree.Vuident s | Parsetree.Vpident s |
    Parsetree.Viident s | Parsetree.Vqident s -> print_string s)
    env.values;
    print_string "\n";
    print_string "labels : ";
    List.iter (fun (x,_) -> match x with | Parsetree.Vlident s |
    Parsetree.Vuident s | Parsetree.Vpident s |
    Parsetree.Viident s | Parsetree.Vqident s -> print_string s)
    env.labels;
    print_string "\n";
    print_string "species : ";
    List.iter (fun (x,_) -> match x with | Parsetree.Vlident s |
    Parsetree.Vuident s | Parsetree.Vpident s |
    Parsetree.Viident s | Parsetree.Vqident s -> print_string s)
    env.species;
    print_string "\n";
    if (env.types = []) then print_string "no types" else ();
    *)
    try env_list_assoc ~allow_opened vname env.types with
    | Not_found ->
        (* Since a species, even not complete, we don't insert its carrier
           as a type constructor, trying to use a species name as type will
           lead to an "Unbound type". This message is a bit unclear for the
           user who will however see his species and won't think that it's
           because it is not closed that he is not allowed to use it's carrier.
           So, to generate a clearer error message, we try to search a species
           with this constructor name. If we don't find any, then we leave the
           initial error message. If we find some, we raise an error telling
           that the species whose carrier is used as type is not closed, hence
           this is forbiden. *)
        (try ignore (find_species_vname ~loc ~allow_opened vname env) with
        | _ ->  raise (Unbound_type (vname, loc)));
        (* If we found a species with this name, issue the better message. *)
        raise (Unbound_collection (vname, loc))
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
      [SBI_method_of_self], we need here to tweak the result to change it
      into a SBI_method_of_coll otherwise, any method would be considered as
      applied from [Self] !!!
      In fact, we need to enter methods in the environment as
      [SBI_method_of_self] instead of [SBI_method_of_coll] otherwise when we
      would scope a real method of [Self], if this method was bound to, for
      instance "KikooSpecies", then once scoped, the AST would return an
      [ident] explicitly shaped like "KikooSpecies!meth" instead of
      "Self!meth".
      This would force the method to be the one of this species at this
      level in the inheritance, hence forbidding late-binding.

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
  (* ********************************************************************** *)
  (* make_pervasives : unit -> TypeInformation.env                          *)
  (** {b Descr} : Creates a fresh environment containing the type
      information of the basic built-in primitives, types and constructors.

      {b Rem} : Exported outside this module.                               *)
  (* ********************************************************************** *)
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  (* ******************************************************************** *)
  (* 'a -> TypeInformation.species_description ->                         *)
  (*   ('b, 'c, 'd, Types.type_scheme, 'e) generic_env                    *)
  (** {b Descr} : Create a fresh environment with the methods, properties
      and theorems of the species called [spec_name] from the methods
      found in [spec_info].

      {b Rem} : Not exported outside this module.                         *)
  (* ******************************************************************** *)
  let make_value_env_from_species_methods _spec_name spec_info =
    (* By folding left, fields at the head of the list will be at the tail of
       the environment list. Hence, methods seen first are inserted first,
       hence are deeper in the environment. *)
    let values_bucket =
      List.fold_left
        (fun accu field ->
          match field with
           | TypeInformation.SF_property (_, v, _, _, _)
           | TypeInformation.SF_theorem (_, v, _, _, _, _) ->
               (* Scheme is trivially "Prop". Donc need to bother about the
                  binding level, this scheme will never be polymorphic. *)
               let s = Types.generalize (Types.type_prop ()) in
               [(v, (BO_absolute s))] @ accu
           | TypeInformation.SF_sig (_, v, s)
           | TypeInformation.SF_let (_, v, _, s, _, _, _, _) ->
               [(v, (BO_absolute s))] @ accu
           | TypeInformation.SF_let_rec l ->
               let l' =
                 List.map
                   (fun (_, v, _, s, _, _, _, _) -> (v, (BO_absolute s))) l in
               l' @ accu)
        []
        spec_info.TypeInformation.spe_sig_methods in
    { constructors = []; labels = []; types = []; values = values_bucket;
      species = [] }



  (* No real need in the typing environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module TypingEnv = Make (TypingEMAccess);;

(* for focaltest : *)
let get_species_list t =
  List.map fst t.species
;;

let get_constructor_list t =
  List.map fst t.constructors
;;

let get_type_list t =
  List.map fst t.types;;
(* *************** *)



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
    (* Non sense for ml code generation environments because we do not provide
       any [find_value] function and that's this function that requires
       [make_value_env_from_species_methods]. *)
    assert false


  (* No real need in the ml code generation environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module MlGenEnv = Make (MlGenEMAccess);;



module CoqGenEMAccess = struct
  type constructor_bound_data = CoqGenInformation.constructor_mapping_info
  type label_bound_data = CoqGenInformation.label_mapping_info
  type type_bound_data = CoqGenInformation.type_info
  type value_bound_data = CoqGenInformation.value_mapping_info
  type species_bound_data = CoqGenInformation.species_binding_info

  let find_module = coqgen_find_module
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  let make_value_env_from_species_methods _species (_, meths_info, _, _) =
    (* Because methods are never polymorphics this was checked before), we can
       safely insert each method as a value bound to 0 extra parameters that
       woud come from ... polymorphism. *)
    let values_bucket =
      List.map
        (fun { mi_name = field_name } ->
          (field_name, (BO_absolute (0, CoqGenInformation.VB_non_toplevel))))
        meths_info in
    { constructors = []; labels = []; types = []; values = values_bucket;
      species = [] }



  (* No real need in the ml code generation environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module CoqGenEnv = Make (CoqGenEMAccess);;


module DkGenEMAccess = struct
  type constructor_bound_data = DkGenInformation.constructor_mapping_info
  type label_bound_data = DkGenInformation.label_mapping_info
  type type_bound_data = DkGenInformation.type_info
  type value_bound_data = DkGenInformation.value_mapping_info
  type species_bound_data = DkGenInformation.species_binding_info

  let find_module = dkgen_find_module
  let pervasives () =
    { constructors = []; labels = []; types = []; values = []; species = [] }


  let make_value_env_from_species_methods _species (_, meths_info, _, _) =
    (* Because methods are never polymorphics this was checked before), we can
       safely insert each method as a value bound to 0 extra parameters that
       woud come from ... polymorphism. *)
    let values_bucket =
      List.map
        (fun { mi_name = field_name } ->
          (field_name, (BO_absolute (0, DkGenInformation.VB_non_toplevel))))
        meths_info in
    { constructors = []; labels = []; types = []; values = values_bucket;
      species = [] }



  (* No real need in the ml code generation environment case. *)
  let post_process_method_value_binding _collname data = data
end
;;
module DkGenEnv = Make (DkGenEMAccess);;

(* **************************************************************** *)
(* source_filename: Types.fname -> ScopingEnv.t -> TypingEnv.t ->   *)
(*   MlGenEnv.t -> CoqGenEnv.t -> DkGenEnv.t -> unit                *)
(** {b Descr} : Create the "fo file" on disk related to the current
    compilation unit.
    This "fo file" contains :
      - A magic number.
      - A triplet (scoping env * typing env * mlgen env).

    {b Rem} : Exported outside this module.                         *)
(* **************************************************************** *)
let make_fo_file ~source_filename scoping_toplevel_env typing_toplevel_env
    opt_mlgen_toplevel_env opt_coqgen_toplevel_env opt_dkgen_toplevel_env =
  (* First, recover from the scoping environment only bindings coming from
     definitions of our current compilation unit. *)
  let scoping_toplevel_env' =
    env_from_only_absolute_bindings scoping_toplevel_env in
  (* Next, recover from the typing environment only bindings coming from
     definitions of our current compilation unit. *)
  let typing_toplevel_env' =
    env_from_only_absolute_bindings typing_toplevel_env in
  (* Next, recover from the ml generation environment only bindings coming
     from definitions of our current compilation unit. *)
  let opt_mlgen_toplevel_env' =
    match opt_mlgen_toplevel_env with
     | None -> None
     | Some e -> Some (env_from_only_absolute_bindings e) in
  (* Recover from the coq generation environment only bindings coming
     from definitions of our current compilation unit. *)
  let opt_coqgen_toplevel_env' =
    match opt_coqgen_toplevel_env with
     | None -> None
     | Some e -> Some (env_from_only_absolute_bindings e) in
  (* Finally, recover from the dedukti generation environment only bindings
     coming from definitions of our current compilation unit. *)
  let opt_dkgen_toplevel_env' =
    match opt_dkgen_toplevel_env with
     | None -> None
     | Some e -> Some (env_from_only_absolute_bindings e) in
  let module_name = Filename.chop_extension source_filename in
  let fo_basename = Files.fo_basename_from_module_name module_name in
  (* Add to the module name the path of the currently compiled source file in
     order to make the ".fo" lying at the same place than the ".fcl" file. *)
  let with_path =
    Filename.concat (Filename.dirname source_filename) fo_basename in
  let out_hd = open_out_bin with_path in
  (* First, write the magic number of the file. *)
  Files.write_magic out_hd Files.fo_magic;
  (* And now the filtered environments. *)
  output_value
    out_hd
    (scoping_toplevel_env', typing_toplevel_env',
     opt_mlgen_toplevel_env', opt_coqgen_toplevel_env',
     opt_dkgen_toplevel_env');
  (* Just don't forget to close the output file... *)
  close_out out_hd
;;



(** {b Descr}: Iterate the function [f] on the species information part of a
    scoping environment. This is mostly used to dump the inheritance graph of
    species by examinating the scoping env. *)
let iter_on_species_scopped f fo = List.iter f fo.ffs_scoping.species ;;



(** Not yet documented. Used to make the debug tool "fodump"... *)
let inspect_fo_structure ppf fo =
  let coq_gen_info = fo.ffs_coqgeneration in
  (* Dump species' and collections' information. *)
  match coq_gen_info with
   | None ->
       Format.fprintf ppf
         "Empty Coq data in the fo file. Are you sure the source file was \
         compiled with the Coq code generation enabled ?@."
   | Some coq_env_info ->
       List.iter
         (fun (species_vname, envt_binding) ->
           (* In a fo file, there must only remain bindings really     *)
           (* introduced by the compilation unit, not by "open" stuf ! *)
           let (_, meths, opt_coll_gen, coll_or_spe) =
             match envt_binding with
              | BO_opened (_, _) -> assert false | BO_absolute b -> b in
           (* Start printing... *)
           Format.fprintf ppf "@[<2>Species %a@\n"
             Sourcify.pp_vname species_vname;
           Format.fprintf ppf "@[<2>*** Methods:@\n";
           List.iter
             (fun meth ->
               (* Just print the method's name for the moment. *)
               Format.fprintf ppf "Method %a  ...@\n"
                 Sourcify.pp_vname meth.mi_name)
             meths;
           (* Now, check for the collection generator information. *)
           Format.fprintf ppf "@]@[<2>*** Collection generator:@\n";
           (match opt_coll_gen with
            | None ->  Format.fprintf ppf "None found@."
            | Some cgi ->
                Format.fprintf ppf "Some found@.";
                (* Info about "implemented" species. *)
                Format.fprintf ppf "Implemented species params names: %a@\n"
                  (Handy.pp_generic_separated_list ","
                     (fun ppf (pname, _) -> Sourcify.pp_vname ppf pname))
                  cgi.CoqGenInformation.cgi_implemented_species_params_names;
                Format.fprintf ppf "@]");
           (match coll_or_spe with
            | COS_species -> Format.fprintf ppf "Is a species.@."
            | COS_collection -> Format.fprintf ppf "Is a collection.@.");
           (* End the species dump box. *)
           Format.fprintf ppf "@]@\n")
         coq_env_info.species
;;



let print_field_for_debug = function
  | TypeInformation.SF_sig (_, n, sch) ->
      Format.eprintf "signature %a : %a@." Sourcify.pp_vname n
        Types.pp_type_scheme sch
  | TypeInformation.SF_let (_, n, args, sch, body, _, _, _) ->
      Format.eprintf "let %a " Sourcify.pp_vname n;
      List.iter (fun a -> Format.eprintf "%a " Sourcify.pp_vname a) args;
      Format.eprintf ": %a " Types.pp_type_scheme sch;
      Format.eprintf "= %a@." Sourcify.pp_binding_body body;
      Format.eprintf "@."
  | TypeInformation.SF_let_rec l ->
      List.iter
        (fun (_, n, args, sch, body, _, _, _) ->
          Format.eprintf "let rec %a " Sourcify.pp_vname n;
          List.iter (fun a -> Format.eprintf "%a " Sourcify.pp_vname a) args;
          Format.eprintf ": %a " Types.pp_type_scheme sch;
          Format.eprintf "= %a@." Sourcify.pp_binding_body body;
          Format.eprintf "@.")
        l
  | TypeInformation.SF_theorem (_, n, _, _, _, _) ->
      Format.eprintf "Theorem %a@." Sourcify.pp_vname n
  | TypeInformation.SF_property (_, n, _, _, _) ->
      Format.eprintf "Property %a@." Sourcify.pp_vname n
;;

