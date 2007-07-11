(* $Id: oldsourcify.mli,v 1.2 2007-07-11 08:26:29 weis Exp $ *)

val pp_file :
  Format.formatter ->
  (Parsetree.file_desc, string) Parsetree.generic_ast -> unit
;;
(** The pretty printer for focalize abstract syntax trees that generates
    focal source code. *)
