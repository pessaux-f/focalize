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

(* $Id: configuration.mli,v 1.28 2012-02-10 17:07:29 pessaux Exp $ *)

(** {3 The internal configuration of the FoCaLize compiler} *)

(** The various flags and definitions useful to the internal behaviour of the
    focalize compiler. *)

exception No_input_file

val focalize_version_number : (int * int * int) 

val focalize_full_version : string

val get_experimental : unit -> bool
val set_experimental : unit -> unit

val get_impose_termination_proof : unit -> bool
val set_impose_termination_proof : unit -> unit

val get_verbose : unit -> bool
val set_verbose : unit -> unit

val get_focalize_doc : unit -> bool
val set_focalize_doc : unit -> unit

val get_pretty_print : unit -> string option
val set_pretty_print : string -> unit

val get_input_file_names : unit -> string list 
val add_input_file_name : string -> unit

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

val get_generate_coq : unit -> bool
val unset_generate_coq : unit -> unit

val get_generate_tests : unit -> bool
val unset_generate_tests : unit -> unit

val get_perform_tests : unit -> bool
val unset_perform_tests : unit -> unit

val get_use_default_lib : unit -> bool
val unset_use_default_lib : unit -> unit

type ml_compiler =
  | OCamlByt
  | OCamlBin
  | OCamlBoth

exception Invalid_OCaml_compiler of string

val set_ml_compiler : string -> unit
val get_ml_compiler : unit -> ml_compiler

val set_stop_before_zenon : unit -> unit
val get_stop_before_zenon : unit -> bool

val set_stop_before_coq : unit -> unit
val get_stop_before_coq : unit -> bool

val require_plugin : string -> unit
val get_plugins : unit -> string list
