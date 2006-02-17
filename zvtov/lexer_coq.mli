(*  Copyright 2004 INRIA  *)
(*  $Id: lexer_coq.mli,v 1.1 2006-02-17 15:36:33 lk Exp $  *)

exception Lex_error of string;;

val coqtoken : Lexing.lexbuf -> Parser_coq.token;;
