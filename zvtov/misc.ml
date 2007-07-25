(*  Copyright 2006 INRIA  *)
(*  $Id: misc.ml,v 1.2 2007-07-25 19:41:39 doligez Exp $  *)

exception Error of string;;
let error msg = raise (Error msg);;

let with_cime = ref false;;
let keep_temp_files = ref false;;
let with_cache = ref true;;
let progress_level = ref 1;;
let use_coqterm = ref true;;
let verbose = ref false;;
let zcmd = ref "zenon";;
let zopt = ref "-x coqbool -ifocal -q -short -max-time 1m";;
let add_opt = ref [];;

let try_remove f =
  if not !keep_temp_files then
    try Sys.remove f with Sys_error _ -> ()
;;
