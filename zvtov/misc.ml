(*  Copyright 2006 INRIA  *)
(*  $Id: misc.ml,v 1.3 2008-08-28 10:22:08 doligez Exp $  *)

exception Error of string;;
let error msg = raise (Error msg);;

let with_cime = ref false;;
let keep_temp_files = ref false;;
let with_cache = ref true;;
let progress_level = ref 1;;
let use_coqterm = ref true;;
let verbose = ref false;;
let zcmd = ref "zenon";;
let zopt = ref "-ifocal -q -short -max-time 1m";;
let focal_ext = ref "coqbool";;
let add_opt = ref [];;

let try_remove f =
  if not !keep_temp_files then
    try Sys.remove f with Sys_error _ -> ()
;;
