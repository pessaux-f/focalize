(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dump_ptree.mli,v 1.6 2007-11-01 17:08:19 weis Exp $ *)


val pp_file :
  Format.formatter -> Parsetree.file -> unit
;;
(** The pretty printer that dumps the abstract syntax trees of
    focalize files. *)
