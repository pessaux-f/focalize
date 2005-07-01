(* copyright 2005 MPI. *)
(* $Id: options.mli,v 1.1 2005-07-01 12:26:22 prevosto Exp $ *)
(* possible options of zvtov. *)

(** returns the current list of options. *)
val get_options: unit -> (string * Arg.spec * string) list

(** sets a new admissible option. *)
val register_option: string -> Arg.spec -> string -> unit
