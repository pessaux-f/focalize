(* $Id: types.mli,v 1.1 2007-07-19 12:01:51 pessaux Exp $ *)

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


(** Types of various identifiers in the abstract syntax tree. *)
type cname = string
     (** Collection name. *) ;;
type sname = string
     (** Species name. *) ;;
type tname = string
     (** Type name. *) ;;
type label_name = string
     (** Label name. *) ;;


type simple_type
type species_type
type types_scheme

exception Conflict of simple_type * simple_type
exception Circularity of simple_type * simple_type
exception Arity_mismatch of (tname * int * int)

val begin_definition : unit -> unit
val end_definition : unit -> unit

val type_variable : unit -> simple_type
val type_basic : tname -> simple_type list -> simple_type
val type_int : unit -> simple_type
val type_float : unit -> simple_type
val type_bool : unit -> simple_type
val type_string : unit -> simple_type
val type_char : unit -> simple_type
val type_unit : unit -> simple_type
val type_arrow : simple_type -> simple_type -> simple_type
val type_tuple : simple_type list -> simple_type
val type_self : unit -> simple_type
val type_prop : unit -> simple_type

val specialize : types_scheme -> simple_type
val specialize_and_instanciate : types_scheme -> simple_type list -> simple_type
val generalize : simple_type -> types_scheme
val trivial_scheme : simple_type -> types_scheme
val unify :
  self_manifest: (simple_type option) -> simple_type -> simple_type -> unit
val pp_simple_type : Format.formatter -> simple_type -> unit
val pp_types_scheme : Format.formatter -> types_scheme -> unit
