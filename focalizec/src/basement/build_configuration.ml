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

let focalize_full_version =
  Printf.sprintf
    "%s\nBuild date: %s\nLast commit SHA: %s"
    Configuration.focalize_short_version Build_stamp.build_date
    Build_stamp.last_commit
;;

let print_focalize_full_version () =
  Configuration.print_focalize_version focalize_full_version
;;

let print_install_dirs () =
  Format.printf "%s %s@."
    Installation.install_bin_dir Installation.install_lib_dir;
  exit 0
;;
