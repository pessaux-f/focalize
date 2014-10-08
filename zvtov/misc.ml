(*  Copyright 2006 INRIA  *)
(*  $Id: misc.ml,v 1.7 2009-08-24 12:15:00 doligez Exp $  *)

exception Error of string;;
exception Zenon_failed;;
let error msg = raise (Error msg);;

type input_format = I_coq | I_dk;;
let input_format = ref I_coq;;
let with_cime = ref false;;
let stop_on_failure = ref true;;
let keep_temp_files = ref false;;
let with_cache = ref true;;
let progress_level = ref 1;;
let use_coqterm = ref true;;
let verbose = ref false;;
let zcmd = ref "zenon";;
let zopt = ref "-x induct -q -short -max-time 5m";;
let focal_ext = ref "focal";;
let add_opt = ref [];;

let try_remove f =
  if not !keep_temp_files then
    try Sys.remove f with Sys_error _ -> ()
;;
