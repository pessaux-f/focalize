(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.1 2004-05-19 15:13:41 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;
