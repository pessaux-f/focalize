(*  Copyright 2004 INRIA  *)
(*  $Id: lexer.mli,v 1.2 2004-06-01 11:56:29 doligez Exp $  *)

val token : Lexing.lexbuf -> Token.t;;
val section : Lexing.lexbuf -> string * string;;
val lemma : Lexing.lexbuf -> Token.lemma_kind;;
