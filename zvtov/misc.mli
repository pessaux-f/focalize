(*  Copyright 2006 INRIA  *)
(*  $Id: misc.mli,v 1.5 2009-08-24 12:15:00 doligez Exp $  *)

exception Error of string;;
exception Zenon_failed;;
val error : string -> 'a;;

val try_remove : string -> unit;;

(* Options from the command line *)

val with_cime : bool ref;;
val stop_on_failure : bool ref;;
val keep_temp_files : bool ref;;
val with_cache : bool ref;;
val progress_level : int ref;;
val use_coqterm : bool ref;;
val verbose : bool ref;;
val zcmd : string ref;;
val zopt : string ref;;
val focal_ext : string ref;;

val add_opt : string list ref;;
