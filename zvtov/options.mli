(*  Copyright 2005 MPI  *)
(*  $Id: options.mli,v 1.2 2005-11-13 22:49:11 doligez Exp $  *)

(* command-line arguments *)

(** returns the current list of options. *)
val get_options: unit -> (string * Arg.spec * string) list

(** sets a new admissible option. *)
val register_option: string -> Arg.spec -> string -> unit
