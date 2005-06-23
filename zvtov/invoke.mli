(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.8 2005-06-23 07:09:17 prevosto Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val set_tptp_option: unit -> unit

val progress_level : int ref;;

val use_coqterm : bool ref;;

val zenon : string -> string -> string -> int list -> string
            -> out_channel -> unit
;;

val zenon_loc : string -> string -> string -> out_channel -> unit;;

val signature : unit -> string;;
