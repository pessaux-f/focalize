(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.15 2008-11-25 15:59:15 doligez Exp $  *)

val set_tptp_option: unit -> unit

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
