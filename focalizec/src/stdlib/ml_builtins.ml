(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: ml_builtins.ml,v 1.5 2008-11-22 23:30:07 weis Exp $ *)

(* ************************************************************************ *)
(** {b Descr} : This module contains the external primitives bound to FoCaL
              in the "basic.foc" file.                                      *)
(* ************************************************************************ *)

exception Foc_error of string ;;
exception Foc_Failure of string ;;

let bi__and_b x y = x && y ;;
let bi__or_b x y = x || y ;;
let bi__not_b x = not x ;;
let bi__xor_b b1 b2 =
  if b1 then if b2 then false else true else if b2 then true else false ;;

let bi__foc_error msg = raise (Foc_error msg) ;;
let bi__string_concat x y = x ^ y ;;
let bi__string_lt (x : string) (y : string) = x < y ;;
let bi__string_print s = print_string s ;;
let bi__string_of_int i = string_of_int i ;;
let bi__int_of_string s = int_of_string s ;;
let bi__int_mult x y = x * y ;;
let bi__int_print i = print_int i ;;
let bi__int_mod x y = x mod y ;;
let bi__int_eq (x : int) (y : int) = x = y ;;
let bi__int_div x y = x / y ;;
let bi__int_lt (x : int) (y : int) = x < y ;;
let bi__int_leq (x : int) (y : int) = x <= y ;;
let bi__int_geq (x : int) (y : int) = x >= y ;;
let bi__int_gt (x : int) (y : int) = x > y ;;
let bi__base_syntactic_equal x y = x = y ;;
let bi__base_physical_equal x y = x == y ;;
let bi__pred x = x - 1 ;;
let bi__int_opposite i = - i ;;
let bi__int_plus x y = x + y ;;
let bi__int_minus x y = x - y ;;
let bi__int_max (x : int) (y : int) = max x y ;;
let bi__int_min (x : int) (y : int) = min x y ;;
