(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.4 2004-06-03 19:50:53 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val progress_level : int ref;;

type coq_version = V7 | V8;;
val coq_version : coq_version ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;
