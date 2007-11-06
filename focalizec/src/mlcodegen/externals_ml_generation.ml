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

(* $Id: externals_ml_generation.ml,v 1.4 2007-11-06 10:14:58 pessaux Exp $ *)


(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external value definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_value_caml_def of (Parsetree.vname * Location.t) ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type definition does not
              not provides any correspondance with OCaml.

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_type_caml_def of (Parsetree.vname * Location.t) ;;



(* ************************************************************************ *)
(** {b Descr} : Exception raised when an external type sum type definition
         named a sum constructor but didn't provided any correspondance
         with OCaml. The location of the error is self-contained in the
         [constructor_ident].

    {b Rem} : Exported outside this module.                                 *)
(* ************************************************************************ *)
exception No_external_constructor_caml_def of Parsetree.constructor_ident ;;



(* ********************************************************************* *)
(** {b Descr} : Exception raised when an external type record type
         definition named a field but didn't provided any correspondance
         with OCaml. The location of the error is self-contained in the
         [label_ident].

    {b Rem} : Exported outside this module.                              *)
(* ********************************************************************* *)
exception No_external_field_caml_def of Parsetree.label_ident ;;
