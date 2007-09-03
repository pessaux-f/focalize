(* $Id: handy.mli,v 1.5 2007-09-03 09:07:23 pessaux Exp $ *)

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
val pp_generic_newlined_list :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pp_generic_explicit_option :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
val pp_generic_option :
  string -> (Format.formatter -> 'a -> unit) -> Format.formatter ->
    'a option -> unit

val int_to_base_26 : int -> string
val list_intersect_p : 'a list -> 'a list -> bool
val list_cons_uniq_eq : 'a -> 'a list -> 'a list
val list_substract : 'a list -> 'a list -> 'a list
val list_mem_count : 'a -> 'a list -> int
val list_concat_uniqq : 'a list -> 'a list -> 'a list
val list_concat_uniq_custom_eq :
    ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
