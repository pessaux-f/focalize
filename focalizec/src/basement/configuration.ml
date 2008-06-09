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

(* $Id: configuration.ml,v 1.17 2008-06-09 12:13:29 pessaux Exp $ *)


exception Input_file_already_set ;;
exception No_input_file ;;

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

let print_install_dirs () =
  Format.printf "%s %s@."
    Installation.install_bin_dir Installation.install_lib_dir ;
  exit 0
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

let (get_input_file_name, set_input_file_name) =
  let input_file_name = ref "" in
  ((fun () ->
    if !input_file_name = "" then raise No_input_file
    else !input_file_name),
   (fun fname ->
     if !input_file_name = "" then input_file_name := fname
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

let (get_methods_history_to_text, set_methods_history_to_text) =
  let methods_history_out_dir = ref None in
  ((fun () -> !methods_history_out_dir),
   (fun fname -> methods_history_out_dir := Some fname))
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


let (get_generate_coq, unset_generate_coq) =
  let generate_coq = ref true in
  ((fun () -> !generate_coq),
   (fun () -> generate_coq := false))
;;


let (get_fancy_ansi, unset_fancy_ansi) =
  let fancy_ansi = ref true in
  ((fun () -> !fancy_ansi),
   (fun () -> fancy_ansi := false))
;;


let (get_use_default_lib, unset_use_default_lib) =
  let use_default_lib = ref true in
  ((fun () -> !use_default_lib),
   (fun () -> use_default_lib := false))
;;
