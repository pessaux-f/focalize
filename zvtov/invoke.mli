(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.5 2004-09-09 15:25:41 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val progress_level : int ref;;

val coq_version : string ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;
