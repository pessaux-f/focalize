(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.2 2004-06-02 17:08:10 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;
val progress_flag : bool ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;
