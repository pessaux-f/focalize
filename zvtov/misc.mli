(*  Copyright 2006 INRIA  *)
(*  $Id: misc.mli,v 1.4 2008-08-28 10:22:08 doligez Exp $  *)

exception Error of string;;
val error : string -> 'a;;

val try_remove : string -> unit;;

(* Options from the command line *)

val with_cime : bool ref;;
val keep_temp_files : bool ref;;
val with_cache : bool ref;;
val progress_level : int ref;;
val use_coqterm : bool ref;;
val verbose : bool ref;;
val zcmd : string ref;;
val zopt : string ref;;
val focal_ext : string ref;;
val add_opt : string list ref;;
