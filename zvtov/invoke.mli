(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.6 2004-10-15 14:31:25 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val progress_level : int ref;;

val coq_version : string ref;;

val use_coqterm : bool ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;

val signature : unit -> string;;
