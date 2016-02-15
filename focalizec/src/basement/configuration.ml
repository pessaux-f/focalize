(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

exception No_input_file ;;


let focalize_version_number = (0, 9, 2) ;;

let focalize_short_version =
  let (major, minor, patch_level) = focalize_version_number in
  Printf.sprintf "%d.%d.%d" major minor patch_level
;;

let print_focalize_version v =
  prerr_endline (Printf.sprintf "The FoCaLize compiler, version %s" v);
  exit 0
;;

let print_focalize_short_version () =
  print_focalize_version focalize_short_version
;;

let (get_experimental, set_experimental) =
  let experimental = ref false in
   (fun () -> !experimental),
   (fun () -> experimental := true)
;;

let (get_impose_termination_proof, set_impose_termination_proof) =
  let impose_termination_proof = ref false in
  (fun () -> !impose_termination_proof),
  (fun () -> impose_termination_proof := true)
;;

let (get_verbose, set_verbose) =
  let verbose = ref false in
  (fun () -> !verbose),
  (fun () -> verbose := true)
;;

let (get_focalize_doc, set_focalize_doc) =
  let doc = ref false in
  (fun () -> !doc),
  (fun () -> doc := true)
;;

let (get_pretty_print, set_pretty_print) =
  let pretty_out_file = ref None in
  (fun () -> !pretty_out_file),
  (fun fname -> pretty_out_file := Some fname)
;;

let (get_input_file_names, add_input_file_name) =
  let input_file_names = ref [] in
  (fun () ->
    if !input_file_names = [] then raise No_input_file
    else List.rev !input_file_names),
  (fun fname -> input_file_names := fname :: !input_file_names)
;;

let (get_do_interface_output, set_do_interface_output) =
  let do_interface_output_flag = ref false in
  (fun () -> !do_interface_output_flag),
  (fun b -> do_interface_output_flag := b)
;;

let (get_pretty_scoped, set_pretty_scoped) =
  let pretty_out_file = ref None in
  (fun () -> !pretty_out_file),
  (fun fname -> pretty_out_file := Some fname)
;;

let (get_dotty_dependencies, set_dotty_dependencies) =
  let dotty_dependencie_out_dir = ref None in
  (fun () -> !dotty_dependencie_out_dir),
  (fun fname -> dotty_dependencie_out_dir := Some fname)
;;

let (get_methods_history_to_text, set_methods_history_to_text) =
  let methods_history_out_dir = ref None in
  (fun () -> !methods_history_out_dir),
  (fun fname -> methods_history_out_dir := Some fname)
;;

let (get_methods_history_to_dotty, set_methods_history_to_dotty) =
  let methods_history_out_dir = ref None in
  (fun () -> !methods_history_out_dir),
  (fun fname -> methods_history_out_dir := Some fname)
;;

let (get_raw_ast_dump, set_raw_ast_dump) =
  let raw_ast_dump = ref false in
  (fun () -> !raw_ast_dump),
  (fun () -> raw_ast_dump := true)
;;

let (get_generate_ocaml, unset_generate_ocaml) =
  let generate_ocaml = ref true in
  (fun () -> !generate_ocaml),
  (fun () -> generate_ocaml := false)
;;

let (get_generate_coq, unset_generate_coq) =
  let generate_coq = ref true in
  (fun () -> !generate_coq),
  (fun () -> generate_coq := false)
;;

let (get_generate_dk, unset_generate_dk) =
  let generate_dk = ref true in
  (fun () -> !generate_dk),
  (fun () -> generate_dk := false)
;;

let (get_fancy_ansi, unset_fancy_ansi) =
  let fancy_ansi = ref true in
  (fun () -> !fancy_ansi),
  (fun () -> fancy_ansi := false)
;;

let (get_generate_tests, unset_generate_tests) =
  let generate_tests = ref true in
  (fun () -> !generate_tests),
  (fun () -> generate_tests := false)
;;

let (get_perform_tests, unset_perform_tests) =
  let perform_tests = ref true in
  (fun () -> !perform_tests),
  (fun () -> perform_tests := false)
;;

let (get_use_default_lib, unset_use_default_lib) =
  let use_default_lib = ref true in
  (fun () -> !use_default_lib),
  (fun () -> use_default_lib := false)
;;

type ml_compiler =
  | OCamlByt
  | OCamlBin
  | OCamlBoth
;;

exception Invalid_OCaml_compiler of string ;;

let (set_ml_compiler, get_ml_compiler) =
  let ml_compiler = ref OCamlByt in
  (fun s ->
     match s with
     | "bin" -> ml_compiler := OCamlBin
     | "byt" -> ml_compiler := OCamlByt
     | "both" -> ml_compiler := OCamlBoth
     | other -> raise (Invalid_OCaml_compiler other)),
  (fun () -> !ml_compiler)
;;

let (set_stop_before_zenon, get_stop_before_zenon) =
  let stop_before_zenon = ref false in
  (fun () -> stop_before_zenon := true),
  (fun () -> !stop_before_zenon)
;;

let (set_stop_before_coq, get_stop_before_coq) =
  let stop_before_coq = ref false in
  (fun () -> stop_before_coq := true),
  (fun () -> !stop_before_coq)
;;

let (set_stop_before_dk, get_stop_before_dk) =
  let stop_before_dk = ref false in
  (fun () -> stop_before_dk := true),
  (fun () -> !stop_before_dk)
;;

let require_plugin, get_plugins =
  let plugin_list = ref [] in
  (fun s -> plugin_list := s :: !plugin_list),
  (fun () -> !plugin_list)
;;

let (get_use_coq_older, set_use_coq_older) =
  let use_coq_older = ref false in
  (fun () -> !use_coq_older),
  (fun () -> use_coq_older := true)
;;

let (set_pmatch_err_as_warn, get_pmatch_err_as_warn) =
  let pmatch_err_as_warn = ref false in
  (fun () -> pmatch_err_as_warn := true),
  (fun () -> !pmatch_err_as_warn)
;;

let (set_zvtov_extra_opts, get_zvtov_extra_opts) =
  let zvtov_extra_opts = ref "" in
  (fun s -> zvtov_extra_opts := !zvtov_extra_opts ^ " " ^ s),
  (fun () -> !zvtov_extra_opts)
;;

let (set_show_term_obls, get_show_term_obls) =
  let show_term_obls = ref false in
  (fun () -> show_term_obls := true),
  (fun () -> !show_term_obls)
;;
