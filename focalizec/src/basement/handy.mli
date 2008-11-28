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

(* $Id: handy.mli,v 1.17 2008-11-28 09:55:57 pessaux Exp $ *)


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
val pp_generic_n_times :
  int -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val int_to_base_26 : int -> string
val list_assoc_custom_eq : ('a -> 'b -> bool) -> 'b -> ('a * 'c) list -> 'c
val list_intersect_p : 'a list -> 'a list -> bool
val list_cons_uniq_eq : 'a -> 'a list -> 'a list
val list_substract : 'a list -> 'a list -> 'a list
val list_mem_count : 'a -> 'a list -> int
val list_mem_count_custom_eq : ('a -> 'b -> bool) -> 'b -> 'a list -> int
val list_concat_uniqq : 'a list -> 'a list -> 'a list
val list_concat_uniq : 'a list -> 'a list -> 'a list
val list_concat_uniq_custom_eq :
  ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
val list_cons_uniq_custom_eq :
  ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
val list_mem_custom_eq : ('a -> 'b -> bool) -> 'b -> 'a list -> bool
val list_mem_n_remove : 'a -> 'a list -> 'a list
val merge_uniq_list : 'a list -> 'a list -> 'a list
val list_first_index : ('a -> bool) -> 'a list -> int
val option_list_to_list : ('a option) list -> 'a list

val pp_set_shaded : Format.formatter -> unit
val pp_set_underlined : Format.formatter -> unit
val pp_set_bold : Format.formatter -> unit
val pp_set_videoinv : Format.formatter -> unit
val pp_reset_effects : Format.formatter -> unit

val int_to_base_26 : int -> string
