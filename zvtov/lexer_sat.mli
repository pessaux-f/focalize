(*  Copyright 2006 INRIA  *)
(*  $Id: lexer_sat.mli,v 1.2 2006-02-17 15:55:12 doligez Exp $  *)

exception Lex_error of string;;

val cimetoken : Lexing.lexbuf -> Parser_sat.token;;
