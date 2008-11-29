(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: configuration.mli,v 1.19 2008-11-29 20:23:34 weis Exp $ *)


(** The various flags and definitions useful to the internal behaviour of the
    focalize compiler. *)

exception Input_file_already_set
exception No_input_file

val focalize_version_number : float

val focalize_full_version : string

val get_impose_termination_proof : unit -> bool
val set_impose_termination_proof : unit -> unit

val get_verbose : unit -> bool
val set_verbose : unit -> unit

val get_focalize_doc : unit -> bool
val set_focalize_doc : unit -> unit

val get_pretty_print : unit -> string option
val set_pretty_print : string -> unit

val get_input_file_name : unit -> string
val set_input_file_name : string -> unit

val get_do_interface_output : unit -> bool
val set_do_interface_output : bool -> unit

val get_pretty_scoped : unit -> string option
val set_pretty_scoped : string -> unit

val print_focalize_short_version : unit -> unit
val print_focalize_full_version : unit -> unit
val print_install_dirs : unit -> unit

val get_dotty_dependencies : unit -> string option
val set_dotty_dependencies : string -> unit

val get_methods_history_to_text : unit -> string option
val set_methods_history_to_text : string -> unit

val get_raw_ast_dump : unit -> bool
val set_raw_ast_dump : unit -> unit

val get_generate_ocaml : unit -> bool
val unset_generate_ocaml : unit -> unit

val get_generate_coq : unit -> bool
val unset_generate_coq : unit -> unit

val get_fancy_ansi : unit -> bool
val unset_fancy_ansi : unit -> unit

val get_use_default_lib : unit -> bool
val unset_use_default_lib : unit -> unit
