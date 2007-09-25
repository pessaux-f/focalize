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


(* $Id: focvasives.ml,v 1.2 2007-09-25 15:29:10 pessaux Exp $ *)

(* ************************************************************************ *)
(** {b Descr} : This module contains the external primitives bound to FoCaL
              in the "basic.foc" file.                                      *)
(* ************************************************************************ *)


exception Foc_error of string ;;

let foc_error msg = raise (Foc_error msg) ;;
let str_cat x y = x ^ y ;;
let str_lt (x : string) (y : string) = x < y ;;
let str_print s = print_string s ;;
let string_of_int i = string_of_int i ;;
let int_of_string s = int_of_string s ;;
let int_mult x y = x * y ;;
let int_print i = print_int i ;;
let int_mod x y = x mod y ;;
let int_eq (x : int) (y : int) = x = y ;;
let int_div x y = x / y ;;
let int_lt (x : int) (y : int) = x < y ;;
let int_leq (x : int) (y : int) = x <= y ;;
let int_geq (x : int) (y : int) = x >= y ;;
let int_gt (x : int) (y : int) = x > y ;;
let base_equal x y = x == y ;;
let base_eq x y = x = y ;;
let pred x = x - 1 ;;
let int_opposite i = - i ;;
let int_plus x y = x + y ;;
