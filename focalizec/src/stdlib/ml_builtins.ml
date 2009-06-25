(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ml_builtins.ml,v 1.11 2009-06-25 17:38:00 weis Exp $ *)

(* ************************************************************************ *)
(** {b Descr} : This module contains the external primitives bound to FoCaLize
              in the "basics.fcl" file.                                      *)
(* ************************************************************************ *)

exception Focalize_error of string;;
exception Focalize_Failure of string;;

(** Boolean functions. *)
let bi__and_b x y = x && y;;
let bi__or_b x y = x || y;;
let bi__not_b x = not x;;
let bi__xor_b b1 b2 =
  if b1 then if b2 then false else true else if b2 then true else false
;;

(** Printing facilities. *)
let bi__print_string = print_string;;
let bi__print_int = print_int;;
let bi__print_newline = print_newline;;

(** String primitives *)
let bi__string_concat x y = x ^ y;;
let bi__string_lt (x : string) (y : string) = x < y;;
let bi__string_of_int = string_of_int;;

(** Integer functions. *)
let bi__int_of_string = int_of_string;;
let bi__int_mult x y = x * y;;
let bi__int_mod x y = x mod y;;
let bi__int_eq (x : int) (y : int) = x == y;;
let bi__int_div x y = x / y;;
let bi__int_lt (x : int) (y : int) = x < y;;
let bi__int_leq (x : int) (y : int) = x <= y;;
let bi__int_geq (x : int) (y : int) = x >= y;;
let bi__int_gt (x : int) (y : int) = x > y;;
let bi__int_opposite i = - i;;
let bi__int_plus x y = x + y;;
let bi__int_minus x y = x - y;;
let bi__int_max (x : int) (y : int) = max x y;;
let bi__int_min (x : int) (y : int) = min x y;;
let bi__int_abs (x : int) = abs x;;

(* Equality. *)
let bi__syntactic_equal x y = x = y;;

(* To raise a fatal error. *)
let bi__focalize_error msg = raise (Focalize_error msg);;

