(*  Copyright 2005 MPI  *)
(*  $Id: options.mli,v 1.3 2007-07-25 19:41:39 doligez Exp $  *)

(* command-line arguments *)

(** returns the current list of options. *)
val get_options: unit -> (string * Arg.spec * string) list

(** sets a new admissible option. *)
val register_option: string -> Arg.spec -> string -> unit
