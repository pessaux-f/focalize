(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: types.mli,v 1.57 2012-10-26 14:55:19 pessaux Exp $ *)

(** Types of various identifiers in the abstract syntax tree. *)
type fname = string
type collection_name = string
type type_name

(** The type algebra for focalize. *)
type type_variable
type type_simple
type type_scheme


type type_collection = (fname * collection_name)

(** The exceptions raised by the type-checker. *)
exception Arity_mismatch_unexpected_args of Location.t
exception Conflict of (type_simple * type_simple * Location.t)
exception Circularity of (type_simple * type_simple * Location.t)
exception Arity_mismatch of (type_name * int * int * Location.t)

val begin_definition : unit -> unit
val end_definition : unit -> unit

val make_type_constructor : fname -> string -> type_name
val split_type_constructor : type_name -> (fname * string)
val type_variable : unit -> type_simple


(** Definition of basic types. *)

(** General construction of a type from its name and the list of its
    type arguments. *)
val type_basic : type_name -> type_simple list -> type_simple

(** The classical basic type constants, including functions and tuples. *)
val type_int : unit -> type_simple
val type_float : unit -> type_simple
val type_bool : unit -> type_simple
val type_string : unit -> type_simple
val type_char : unit -> type_simple
val type_unit : unit -> type_simple
val type_arrow : type_simple -> type_simple -> type_simple
val type_sum_arguments : type_simple list -> type_simple
val type_tuple : type_simple list -> type_simple
val type_list : type_simple -> type_simple
val type_prop : unit -> type_simple
val type_rep_species :
  species_module: fname -> species_name: collection_name -> type_simple
(** Generate the carrier type of the currently analysed species. *)
val type_self : unit -> type_simple
val is_bool_or_self_type : type_simple -> bool
val of_which_construct_type : type_simple -> type_name option

val type_sum_arguments_from_type_tuple : type_simple -> type_simple
val refers_to_self_p : type_simple -> bool
val refers_to_prop_p : type_simple -> bool

type substitution_by_replacement_collection_kind =
  | SBRCK_coll of type_collection
  | SBRCK_self

val subst_type_simple :
  type_collection -> substitution_by_replacement_collection_kind ->
  type_simple -> type_simple
val subst_type_scheme :
  type_collection -> substitution_by_replacement_collection_kind ->
  type_scheme -> type_scheme

(** Manipulation of type schemes: generalization, instanciation, generation of
a (closed) type scheme from a type without unknowns. *)
val specialize : type_scheme -> type_simple
val specialize_with_args : type_scheme -> type_simple list -> type_simple
val generalize : type_simple -> type_scheme
val build_type_def_scheme :
    variables: type_simple list -> body: type_simple -> type_scheme
val trivial_scheme : type_simple -> type_scheme
val scheme_split : type_scheme -> ((type_variable list) * type_simple)
val scheme_contains_variable_p : type_scheme -> bool
val copy_type_simple_but_variables :
  and_abstract: type_collection option -> type_simple -> type_simple
val abstract_in_scheme : type_collection -> type_scheme -> type_scheme


(** Type (schemes) unification. *)
val unify :
  loc: Location.t -> self_manifest: (type_simple option) -> type_simple ->
  type_simple -> type_simple

val reset_deps_on_rep : unit -> unit
val check_for_decl_dep_on_self : type_simple -> unit
val get_def_dep_on_rep : unit -> bool
val get_decl_dep_on_rep : unit -> bool


val extract_fun_ty_result :
  self_manifest: type_simple option -> type_simple -> type_simple
val extract_fun_ty_arg :
  self_manifest: type_simple option -> type_simple -> type_simple

(** Pretty_printing for types and type schemes for FoCaLize. *)
val pp_type_name : Format.formatter -> type_name -> unit
val pp_type_simple : Format.formatter -> type_simple -> unit
val pp_type_scheme : Format.formatter -> type_scheme -> unit
val pp_type_collection : Format.formatter -> type_collection -> unit

type species_collection_kind =
  | SCK_toplevel_collection
  | SCK_toplevel_species
  | SCK_species_parameter

type collection_carrier_mapping_info =
  | CCMI_is
  | CCMI_in of species_collection_kind

type collection_carrier_mapping =
  (type_collection * (string * collection_carrier_mapping_info)) list

val debug_collection_carrier_mapping : collection_carrier_mapping -> unit


(** Pretty_printing for types for the OCaml translation. *)
val pp_type_simple_to_ml :
  current_unit: fname -> collection_carrier_mapping -> Format.formatter ->
  type_simple ->
    unit

val purge_type_simple_to_ml_variable_mapping : unit -> unit

(** Pretty_printing for types for the Coq translation. *)
type coq_print_context = {
  cpc_current_unit : fname ;
  cpc_current_species : type_collection option ;
  cpc_collections_carrier_mapping : collection_carrier_mapping
}


val pp_type_simple_to_coq :
  coq_print_context -> Format.formatter -> type_simple -> unit
val pp_type_variable_to_coq : Format.formatter -> type_variable -> unit
val pp_type_simple_args_to_coq :
  coq_print_context -> Format.formatter -> type_simple -> int -> unit

val purge_type_simple_to_coq_variable_mapping : unit -> unit
(* DEBUG
val debug_variable_mapping : unit -> unit *)

module SpeciesCarrierTypeSet :
  sig
    type elt = (fname * collection_name)
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

val get_species_types_in_type : type_simple -> SpeciesCarrierTypeSet.t
val pp_type_simple_to_xml : Format.formatter -> type_simple -> unit
val pp_type_variable_to_xml : Format.formatter -> type_variable -> unit
val purge_type_simple_to_xml_variable_mapping : unit -> unit

(** *)
type local_type =
  | Lt_var of int
  | Lt_fun of local_type * local_type
  | Lt_tuple of local_type list
  | Lt_constr of (string * string) * local_type list
  | Lt_self
  | Lt_species of (string * string)


(** *)
val type_simple_to_local_type : type_simple -> local_type
val type_scheme_to_local_type : type_scheme -> local_type


(* for focaltest : *)
val extract_type_simple :
        (unit -> 'a) ->
        (type_simple -> type_simple -> 'a) ->
        (type_simple list -> 'a) ->
        (type_simple list -> 'a) ->
        (string -> string -> type_simple list -> 'a) ->
        (unit -> 'a) ->
        (fname -> collection_name -> 'a) -> type_simple ->  'a

val nb_variable_type_scheme : type_scheme -> int
val type_variable_eq : type_simple -> type_simple -> bool option
