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

(* $Id: sets_orders_externals.ml,v 1.3 2008-09-24 08:52:21 weis Exp $ *)

let i_failed r _elt =
  raise (Ml_builtins.Foc_Failure ("Failed: " ^ r))
;;
