(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            Fran�ois Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: parse_file.mli,v 1.5 2008-10-21 14:08:30 weis Exp $ *)

exception Lex_error of (Lexing.position * Lexing.position * string) ;;
exception Syntax_error of (Lexing.position * Lexing.position) ;;
exception Unclear_error of (string * Lexing.position * Lexing.position) ;;

val pp_err_loc : Format.formatter -> Lexing.position * Lexing.position -> unit
;;

val parse_file : string -> Parsetree.file
;;

