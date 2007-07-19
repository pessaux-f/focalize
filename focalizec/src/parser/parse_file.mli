(* $Id: parse_file.mli,v 1.2 2007-07-19 12:01:51 pessaux Exp $ *)

(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)
exception Lex_error of (Lexing.position * Lexing.position * string) ;;
exception Syntax_error of (Lexing.position * Lexing.position) ;;
exception Unclear_error of (Lexing.position * Lexing.position) ;;

val pp_err_loc : Format.formatter -> Lexing.position * Lexing.position -> unit

val parse_file : Format.formatter -> string -> Parsetree.file
