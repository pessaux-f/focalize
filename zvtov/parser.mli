(*  Copyright 2004 INRIA  *)
(*  $Id: parser.mli,v 1.3 2007-07-25 19:41:39 doligez Exp $  *)

val parse : string -> Lexing.lexbuf -> out_channel -> unit;;
