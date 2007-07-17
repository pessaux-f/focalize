(* $Id: oldsourcify.mli,v 1.4 2007-07-17 08:25:10 pessaux Exp $ *)

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


val pp_file :
  Format.formatter ->
  (Parsetree.file_desc, string) Parsetree.generic_ast -> unit
;;
(** The pretty printer for focalize abstract syntax trees that generates
    focal source code. *)
