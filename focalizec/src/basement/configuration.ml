(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                    LIP6  --  INRIA Rocquencourt                     *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: configuration.ml,v 1.3 2007-08-06 14:00:14 pessaux Exp $ *)

let focal_version_number = 0.1
;;

let focal_short_version =
  Printf.sprintf "%.2f" focal_version_number
;;

let focal_full_version =
  Printf.sprintf "%s %s" focal_short_version "alpha"
;;

let print_focal_version v =
  prerr_endline (Printf.sprintf "The Focal compiler, version %s" v);
  exit 0
;;

let print_focal_short_version () =
  print_focal_version focal_short_version
;;

let print_focal_full_version () =
  print_focal_version focal_full_version
;;

let (get_verbose, set_verbose) =
  let verbose = ref false in
  ((fun () -> !verbose),
   (fun b -> verbose := b))
;;

let (get_pretty_print, set_pretty_print) =
  let pretty_out_file = ref None in
  ((fun () -> !pretty_out_file),
   (fun fname -> pretty_out_file := Some fname))
;;

let (get_old_pretty_print, set_old_pretty_print) =
  let pretty_out_file = ref None in
  ((fun () -> !pretty_out_file),
   (fun fname -> pretty_out_file := Some fname))
;;

let (get_input_file_name, set_input_file_name) =
  let input_file_name = ref "-" in
  ((fun () -> !input_file_name),
   (fun fname ->
     if !input_file_name = "-" then input_file_name := fname
     else failwith "Input file name is already set."))
;;

let (get_do_scoping, set_do_scoping) =
  let do_scoping_flag = ref false in
  ((fun () -> !do_scoping_flag),
   (fun b -> do_scoping_flag := b))
;;

let (get_do_typechecking, set_do_typechecking) =
  let do_typechecking_flag = ref false in
  ((fun () -> !do_typechecking_flag),
   (fun b -> do_typechecking_flag := b))
;;

