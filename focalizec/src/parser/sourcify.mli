(* $Id: sourcify.mli,v 1.2 2007-07-11 08:27:03 weis Exp $ *)

val pp_file : Format.formatter -> Parsetree.file -> unit
;;
(** The pretty printer for focalize abstract syntax trees that generates
    focalize source code. *)

