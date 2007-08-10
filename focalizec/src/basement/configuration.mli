(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: configuration.mli,v 1.4 2007-08-10 15:32:06 pessaux Exp $ *)

(** The various flags and definitions useful to the internal behaviour of the
    focalize compiler. *)

val focal_version_number : float

val focal_full_version : string

val get_verbose : unit -> bool
val set_verbose : bool -> unit

val get_pretty_print : unit -> string option
val set_pretty_print : string -> unit

val get_old_pretty_print : unit -> string option
val set_old_pretty_print : string -> unit

val get_input_file_name : unit -> string
val set_input_file_name : string -> unit

val get_do_scoping : unit -> bool
val set_do_scoping : bool -> unit

val get_do_typechecking : unit -> bool
val set_do_typechecking : bool -> unit

val get_pretty_scoped : unit -> string option
val set_pretty_scoped : string -> unit

val print_focal_short_version : unit -> unit
val print_focal_full_version : unit -> unit
