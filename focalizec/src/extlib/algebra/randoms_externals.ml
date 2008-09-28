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

(* $Id: randoms_externals.ml,v 1.1 2008-09-28 15:06:48 rr Exp $ *)

let random_seed seed = Random.init seed ;;

let random_int bound = Random.int bound ;;

let random_self_init () = Random.self_init () ;;
