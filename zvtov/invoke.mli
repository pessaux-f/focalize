(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.10 2005-11-09 15:22:03 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;
val addopt : string list ref;;

val set_tptp_option: unit -> unit

val set_tptp_option: unit -> unit

val progress_level : int ref;;

val use_coqterm : bool ref;;

val zenon :
  string -> string -> string -> int list -> string -> out_channel -> unit
;;

val zenon_loc :
  string -> (string * string) ->  string -> string -> out_channel -> unit
;;

val set_atp :
  (string -> (string *string) -> string -> string -> out_channel -> unit)
  -> unit
;;

val atp :
  string -> (string * string) -> string -> string -> out_channel -> unit
;;

val signature : unit -> string;;
