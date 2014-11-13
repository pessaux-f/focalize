(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre Weis                                                     *)
(*            Damien Doligez                                                  *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external value definition does not
              not provides any correspondance with a language.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_value_def of (
  string *            (** The language name ("OCaml", "Coq", "..."). *)
  Parsetree.vname *   (** The primitive that was not mapped. *)
  Location.t) ;;      (** The location where the mapping could not be done. *)



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type definition does not
              not provides any correspondance with a language.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_type_def of (string * Parsetree.vname * Location.t)
;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type sum type definition
         named a sum constructor but didn't provided any correspondance
         with a language. The location of the error is self-contained in
        the [constructor_ident].

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_constructor_def of
  (string * Parsetree.constructor_ident) ;;



(* ********************************************************************* *)
(** {b Descr} : Exception raised when an external type record type
         definition named a field but didn't provided any correspondance
         with a language. The location of the error is self-contained in
         the [label_ident].

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
exception No_external_field_def of (string * Parsetree.label_ident) ;;
