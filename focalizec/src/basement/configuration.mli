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

(* $Id: configuration.mli,v 1.14 2007-10-29 08:18:36 pessaux Exp $ *)


(** The various flags and definitions useful to the internal behaviour of the
    focalize compiler. *)

exception Input_file_already_set
exception No_input_file

val focal_version_number : float

val focal_full_version : string

val get_verbose : unit -> bool
val set_verbose : unit -> unit

val get_pretty_print : unit -> string option
val set_pretty_print : string -> unit

val get_input_file_name : unit -> string
val set_input_file_name : string -> unit

val get_do_interface_output : unit -> bool
val set_do_interface_output : bool -> unit

val get_pretty_scoped : unit -> string option
val set_pretty_scoped : string -> unit

val print_focal_short_version : unit -> unit
val print_focal_full_version : unit -> unit
val print_install_dirs : unit -> unit

val get_dotty_dependencies : unit -> string option
val set_dotty_dependencies : string -> unit

val get_raw_ast_dump : unit -> bool
val set_raw_ast_dump : unit -> unit

val get_generate_ocaml : unit -> bool
val unset_generate_ocaml : unit -> unit

val get_fancy_ansi : unit -> bool
val unset_fancy_ansi : unit -> unit

val get_use_default_lib : unit -> bool
val unset_use_default_lib : unit -> unit
