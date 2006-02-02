(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.12 2006-02-02 13:30:03 doligez Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;
val addopt : string list ref;;
val verbose : bool ref;;

val set_tptp_option: unit -> unit

val progress_level : int ref;;
val keep_temp_files : bool ref;;
val use_coqterm : bool ref;;

(*
val zenon :
  string -> string -> string -> int list -> string -> out_channel -> unit
;;
*)

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
