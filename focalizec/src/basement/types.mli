(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: types.mli,v 1.4 2007-08-13 15:01:11 pessaux Exp $ *)

(** Types of various identifiers in the abstract syntax tree. *)
type collection_name = string
     (** Collection name. *)
;;

type species_name = string
     (** Species name. *)
;;

type type_name = string
     (** Type name. *)
;;

type label_name = string
     (** Label name. *)
;;

(** The type algebra for focalize. *)

type type_simple

type type_scheme
type type_species

(** The exceptions raised by the type-checker. *)

exception Conflict of type_simple * type_simple
  (** Those two types cannot be unified. *)
;;

exception Circularity of type_simple * type_simple
  (** There is a circularity detected: the first type occurs in the second. *)

;;

exception Arity_mismatch of type_name * int * int
  (** A functional type constructor has been used with the wrong number of
  arguments. The exception carries on the name of the type and the conflicting
  arities. *)
;;

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
val type_rep_species : species_name -> type_simple
(** Generate the carrier type of the currently analysed species. *)
val type_self : unit -> type_simple



(** Manipulation of type schemes: generalization, instanciation, generation of
a (closed) type scheme from a type without unknowns. *)
val instanciate_parameters : type_scheme -> type_simple list -> type_simple
val specialize : type_scheme -> type_simple
val generalize : type_simple -> type_scheme
val closed_scheme : type_simple -> type_scheme


(** Type (schemes) unification. *)
val unify :
  self_manifest: (type_simple option) -> type_simple -> type_simple -> unit

(** Pretty_printing for types and type schemes. *)
val pp_type_simple : Format.formatter -> type_simple -> unit
val pp_type_scheme : Format.formatter -> type_scheme -> unit
