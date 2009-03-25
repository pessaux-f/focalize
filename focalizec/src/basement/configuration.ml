(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                    LIP6  --  INRIA Rocquencourt                     *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: configuration.ml,v 1.31 2009-03-25 15:57:59 pessaux Exp $ *)

exception No_input_file;;

let focalize_version_number = (0, 1, 4) ;;

let focalize_short_version =
  let (major, minor, patch_level) = focalize_version_number in
  Printf.sprintf "%d.%d.%d" major minor patch_level
;;

let focalize_full_version =
  Printf.sprintf "%s %s" focalize_short_version "alpha"
;;

let print_focalize_version v =
  prerr_endline (Printf.sprintf "The Focalize compiler, version %s" v);
  exit 0
;;

let print_focalize_short_version () =
  print_focalize_version focalize_short_version
;;

let print_focalize_full_version () =
  print_focalize_version focalize_full_version
;;

let print_install_dirs () =
  Format.printf "%s %s@."
    Installation.install_bin_dir Installation.install_lib_dir;
  exit 0
;;

let (get_experimental, set_experimental) =
  let experimental = ref false in
  ((fun () -> !experimental),
   (fun () -> experimental := true))
;;

let (get_impose_termination_proof, set_impose_termination_proof) =
  let impose_termination_proof = ref false in
  ((fun () -> !impose_termination_proof),
   (fun () -> impose_termination_proof := true))
;;

let (get_verbose, set_verbose) =
  let verbose = ref false in
  ((fun () -> !verbose),
   (fun () -> verbose := true))
;;

let (get_focalize_doc, set_focalize_doc) =
  let doc = ref false in
  ((fun () -> !doc),
   (fun () -> doc := true))
;;

let (get_pretty_print, set_pretty_print) =
  let pretty_out_file = ref None in
  ((fun () -> !pretty_out_file),
   (fun fname -> pretty_out_file := Some fname))
;;

let (get_input_file_names, add_input_file_name) =
  let input_file_names = ref [] in
  ((fun () ->
    if !input_file_names = [] then raise No_input_file
    else List.rev !input_file_names),
   (fun fname -> input_file_names := fname :: !input_file_names))
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

type ml_compiler =
  | OCamlByt
  | OCamlBin
  | OCamlBoth
;;

exception Invalid_OCaml_compiler of string ;;

let (set_ml_compiler, get_ml_compiler) =
  let ml_compiler = ref OCamlByt in
  ((fun s ->
     match s with
     | "bin" -> ml_compiler := OCamlBin
     | "byt" -> ml_compiler := OCamlByt
     | "both" -> ml_compiler := OCamlBoth
     | other -> raise (Invalid_OCaml_compiler other)),
   (fun () -> !ml_compiler))
;;

let (set_stop_before_zenon, get_stop_before_zenon) =
  let stop_before_zenon = ref false in
  ((fun () -> stop_before_zenon := true),
   (fun () -> !stop_before_zenon))
;;

let (set_stop_before_coq, get_stop_before_coq) =
  let stop_before_coq = ref false in
  ((fun () -> stop_before_coq := true),
   (fun () -> !stop_before_coq))
;;
