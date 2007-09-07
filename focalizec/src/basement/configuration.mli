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

(* $Id: configuration.mli,v 1.9 2007-09-07 12:54:17 pessaux Exp $ *)

(** The various flags and definitions useful to the internal behaviour of the
    focalize compiler. *)

exception Input_file_already_set

val focal_version_number : float

val focal_full_version : string

val get_verbose : unit -> bool
val set_verbose : unit -> unit

val get_pretty_print : unit -> string option
val set_pretty_print : string -> unit

val get_old_pretty_print : unit -> string option
val set_old_pretty_print : string -> unit

val get_input_file_name : unit -> string
val set_input_file_name : string -> unit

val get_do_interface_output : unit -> bool
val set_do_interface_output : bool -> unit

val get_pretty_scoped : unit -> string option
val set_pretty_scoped : string -> unit

val print_focal_short_version : unit -> unit
val print_focal_full_version : unit -> unit

val get_dotty_dependencies : unit -> string option
val set_dotty_dependencies : string -> unit

val get_raw_ast_dump : unit -> bool
val set_raw_ast_dump : unit -> unit
