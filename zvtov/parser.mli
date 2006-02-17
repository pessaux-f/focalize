(*  Copyright 2004 INRIA  *)
(*  $Id: parser.mli,v 1.2 2006-02-17 15:36:33 lk Exp $  *)

val with_cime : bool ref;;
val parse : string -> Lexing.lexbuf -> out_channel -> unit;;
