(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* Runtime library for pretty-printers generated with camlpp.*)

val print_bool : bool -> unit;;
val print_poly : 'a -> unit;;
val print_quoted_string : string -> unit;;
val print_quoted_char : char -> unit;;
val print_quoted_int : int -> unit;;
val print_quoted_float : float -> unit;;
val print_list : ('a -> unit) -> 'a list -> unit;;
val print_array : ('a -> unit) -> 'a array -> unit;;
val print_option : ('a -> unit) -> 'a option -> unit;;
