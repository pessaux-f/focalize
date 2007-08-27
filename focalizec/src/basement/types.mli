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

(* $Id: types.mli,v 1.13 2007-08-27 15:34:57 pessaux Exp $ *)

(** Types of various identifiers in the abstract syntax tree. *)
type collection_name = string
     (** Collection name. *) ;;
type species_name = string
     (** Species name. *) ;;
type type_name = string
     (** Type name. *) ;;
type label_name = string
     (** Label name. *) ;;
type fname = string
     (** File (and "module") name. *) ;;

(** The type algebra for focalize. *)

type type_simple

type type_scheme
type type_collection = (fname * collection_name)

(** The exceptions raised by the type-checker. *)

exception Conflict of (type_simple * type_simple * Location.t)
  (** Those two types cannot be unified. *)
;;

exception Circularity of (type_simple * type_simple * Location.t)
  (** There is a circularity detected: the first type occurs in the second. *)

;;

exception Arity_mismatch of (type_name * int * int * Location.t)
  (** A functional type constructor has been used with the wrong number of
  arguments. The exception carries on the name of the type and the conflicting
  arities. *)

val begin_definition : unit -> unit
val end_definition : unit -> unit

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
val type_prop : unit -> type_simple
val type_rep_species :
  species_module: fname -> species_name: species_name -> type_simple
(** Generate the carrier type of the currently analysed species. *)
val type_self : unit -> type_simple


val subst_type_simple : type_collection -> type_collection -> type_simple ->
    type_simple

(** Manipulation of type schemes: generalization, instanciation, generation of
a (closed) type scheme from a type without unknowns. *)
val specialize : type_scheme -> type_simple
val specialize2 :
  type_scheme -> type_simple list -> (type_simple * type_simple list)
val generalize : type_simple -> type_scheme
val generalize2 :
  type_simple -> type_simple list -> (type_scheme * (type_simple list))
val trivial_scheme : type_simple -> type_scheme
val never_generalizable_scheme : type_simple -> type_scheme
val abstract_copy : (fname * collection_name) -> type_simple -> type_simple

(** Type (schemes) unification. *)
val unify :
  loc: Location.t -> self_manifest: (type_simple option) -> type_simple ->
    type_simple -> unit

(** Pretty_printing for types and type schemes. *)
val pp_type_simple : Format.formatter -> type_simple -> unit
val pp_type_scheme : Format.formatter -> type_scheme -> unit
val pp_type_collection : Format.formatter -> type_collection -> unit
