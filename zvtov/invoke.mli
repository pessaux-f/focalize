(*  Copyright 2004 INRIA  *)
(*  $Id: invoke.mli,v 1.9 2005-07-01 12:26:22 prevosto Exp $  *)

val zcmd : string ref;;
val zopt : string ref;;

val set_tptp_option: unit -> unit

val progress_level : int ref;;

val use_coqterm : bool ref;;

val zenon : string -> string -> string -> int list -> string -> out_channel -> unit
;;

val zenon_loc : string -> (string * string) ->  string -> string -> 
  out_channel -> unit;;

val set_atp: (string -> (string *string)-> string -> string -> 
                out_channel -> unit) ->  unit;;

val atp:  string -> (string * string) -> string -> string -> out_channel 
  -> unit;;

val signature : unit -> string;;
