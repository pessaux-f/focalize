(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.3 2004-06-02 22:33:34 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val progress_level : int ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;
