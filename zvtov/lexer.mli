(*  Copyright 2004 INRIA  *)
(*  $Id: lexer.mli,v 1.1 2004-05-19 15:13:41 doligez Exp $  *)

val token : Lexing.lexbuf -> Token.t;;
val section : Lexing.lexbuf -> string * string;;
val lemma : Lexing.lexbuf -> int list;;
