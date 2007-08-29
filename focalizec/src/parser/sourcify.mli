(* $Id: sourcify.mli,v 1.7 2007-08-29 12:47:48 pessaux Exp $ *)

(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

val pp_vname : Format.formatter -> Parsetree.vname -> unit
val pp_vnames : string -> Format.formatter -> Parsetree.vname list -> unit
val pp_ident : Format.formatter -> Parsetree.ident -> unit
val pp_file : Format.formatter -> Parsetree.file -> unit
;;
(** The pretty printer for focalize abstract syntax trees that generates
    focalize source code. *)

