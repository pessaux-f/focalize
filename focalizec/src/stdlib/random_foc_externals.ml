(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: random_foc_externals.ml,v 1.3 2008-09-24 08:52:21 weis Exp $ *)

let random_seed seed = Random.init seed ;;

let random_int bound = Random.int bound ;;

let random_self_init () = Random.self_init () ;;
