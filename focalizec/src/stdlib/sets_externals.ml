(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sets_externals.ml,v 1.1 2008-10-13 12:15:07 rr Exp $ *)

let i_failed r _elt =
  raise (Ml_builtins.Foc_Failure ("Failed: " ^ r))
;;
