(* $Id: Exp *)

val pp_file :
  Format.formatter -> (Parsetree.file_desc, 'a) Parsetree.generic_ast -> unit
;;
(** The pretty printer that dumps the abstract syntax trees of focalize
 files. *)
