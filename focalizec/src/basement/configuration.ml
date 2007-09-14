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

(* $Id: configuration.ml,v 1.10 2007-09-14 14:32:32 pessaux Exp $ *)

exception Input_file_already_set ;;

let focal_version_number = 0.1 ;;

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
   (fun () -> verbose := true))
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
     else raise Input_file_already_set))
;;

let (get_do_interface_output, set_do_interface_output) =
  let do_interface_output_flag = ref false in
    ((fun () -> !do_interface_output_flag),
   (fun b -> do_interface_output_flag := b))
;;

let (get_pretty_scoped, set_pretty_scoped) =
  let pretty_out_file = ref None in
  ((fun () -> !pretty_out_file),
   (fun fname -> pretty_out_file := Some fname))
;;

let (get_dotty_dependencies, set_dotty_dependencies) =
  let dotty_dependencie_out_dir = ref None in
  ((fun () -> !dotty_dependencie_out_dir),
   (fun fname -> dotty_dependencie_out_dir := Some fname))
;;

let (get_raw_ast_dump, set_raw_ast_dump) =
  let raw_ast_dump = ref false in
  ((fun () -> !raw_ast_dump),
   (fun () -> raw_ast_dump := true))
;;


let (get_generate_ocaml, unset_generate_ocaml) =
  let generate_ocaml = ref true in
  ((fun () -> !generate_ocaml),
   (fun () -> generate_ocaml := false))
;;
