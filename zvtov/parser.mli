(*  Copyright 2004 INRIA  *)
(*  $Id: parser.mli,v 1.1 2004-05-19 15:13:41 doligez Exp $  *)

val parse : string -> Lexing.lexbuf -> out_channel -> unit;;
