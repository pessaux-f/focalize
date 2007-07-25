(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.14 2007-07-25 19:41:39 doligez Exp $  *)

val set_tptp_option: unit -> unit

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
