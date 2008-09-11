(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Renaud Rioboo                                            *)
(*            Virgile Prevosto                                         *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6   -  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: random_foc_externals.ml,v 1.2 2008-09-11 09:26:21 pessaux Exp $ *)

let random_seed seed = Random.init seed ;;

let random_int bound = Random.int bound ;;

let random_self_init () = Random.self_init () ;;
