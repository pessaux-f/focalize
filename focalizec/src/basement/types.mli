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

(* $Id: types.mli,v 1.21 2007-10-22 08:41:30 pessaux Exp $ *)


(** Types of various identifiers in the abstract syntax tree. *)
type fname = string
type collection_name = string
type type_name
type label_name = string

(** The type algebra for focalize. *)
type type_simple
type type_scheme
type type_collection = (fname * collection_name)

(** The exceptions raised by the type-checker. *)
exception Conflict of (type_simple * type_simple * Location.t)
exception Circularity of (type_simple * type_simple * Location.t)
exception Arity_mismatch of (type_name * int * int * Location.t)

val begin_definition : unit -> unit
val end_definition : unit -> unit

val make_type_constructor : fname -> string -> type_name

val type_variable : unit -> type_simple
;;

(** Definition of basic types. *)

val type_basic : type_name -> type_simple list -> type_simple
(** General construction of a type from its name and list of type variables. *)
;;

(** The classical basic type constants, including functions and tuples. *)
val type_int : unit -> type_simple
val type_float : unit -> type_simple
val type_bool : unit -> type_simple
val type_string : unit -> type_simple
val type_char : unit -> type_simple
val type_unit : unit -> type_simple
val type_arrow : type_simple -> type_simple -> type_simple
val type_tuple : type_simple list -> type_simple
val type_list : type_simple -> type_simple
val type_prop : unit -> type_simple
val type_rep_species :
  species_module: fname -> species_name: collection_name -> type_simple
(** Generate the carrier type of the currently analysed species. *)
val type_self : unit -> type_simple


val subst_type_simple : type_collection -> type_collection -> type_simple ->
    type_simple

(** Manipulation of type schemes: generalization, instanciation, generation of
a (closed) type scheme from a type without unknowns. *)
val specialize : type_scheme -> type_simple
val specialize_with_args : type_scheme -> type_simple list -> type_simple
val generalize : type_simple -> type_scheme
val generalize2 :
  type_simple -> type_simple list -> (type_scheme * (type_simple list))
val trivial_scheme : type_simple -> type_scheme
val scheme_contains_variable_p : type_scheme -> bool
val copy_type_simple_but_variables :
  and_abstract: type_collection option -> type_simple -> type_simple


(** Type (schemes) unification. *)
val unify :
  loc: Location.t -> self_manifest: (type_simple option) -> type_simple ->
    type_simple -> type_simple
val extract_fun_ty_result : type_simple -> type_simple
val extract_fun_ty_arg : type_simple -> type_simple

(** Pretty_printing for types and type schemes for FoCal. *)
val pp_type_name : Format.formatter -> type_name -> unit
val pp_type_simple : Format.formatter -> type_simple -> unit
val pp_type_scheme : Format.formatter -> type_scheme -> unit
val pp_type_collection : Format.formatter -> type_collection -> unit

(** Pretty_printing for types for the OCaml translation. *)
val pp_type_simple_to_ml :
  current_unit: fname -> reuse_mapping: bool ->
    (type_collection * string) list -> Format.formatter -> type_simple -> unit
val purge_type_simple_to_ml_variable_mapping : unit -> unit
