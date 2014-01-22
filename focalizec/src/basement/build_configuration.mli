(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 The build-time configuration of the FoCaLize compiler} *)

(** Functions that depend on files generated at build time. Moved from
    Configuration to this module to avoid having almost all source files
    depend on build_stamp.ml and installation.ml, which are regenerated
    at each invocation of make. *)

val focalize_full_version : string
val print_focalize_full_version : unit -> unit
val print_install_dirs : unit -> unit
