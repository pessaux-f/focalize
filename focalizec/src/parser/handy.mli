(* $Id: handy.mli,v 1.5 2007-07-13 15:16:38 pessaux Exp $ *)
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

(** Pretty printing tools. *)

val pp_generic_separated_list :
  string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
;;
val pp_generic_newlined_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
;;
val pp_generic_explicit_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
;;
val pp_generic_option :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter ->
    'a option -> unit
;;
