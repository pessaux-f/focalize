(* $Id: dump_ptree.mli,v 1.4 2007-07-17 08:25:10 pessaux Exp $ *)

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


val pp_file :
  Format.formatter -> (Parsetree.file_desc, 'a) Parsetree.generic_ast -> unit
;;
(** The pretty printer that dumps the abstract syntax trees of focalize
 files. *)
