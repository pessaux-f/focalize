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

(* $Id: weak_structures_externals.ml,v 1.1 2008-09-28 15:06:48 rr Exp $ *)

type 'a quo_rem_result = { quot : 'a; rema :'a };;
type 'a bezout_res = { gcd : 'a; coef1 : 'a; coef2 : 'a };;

let qrr_quot x = x.quot;;
let qrr_rema x = x.rema;;
let cr_qrr x y = { quot = x; rema = y };;
let br_gcd x = x.gcd;;
let br_coef1 x = x.coef1;;
let br_coef2 x = x.coef2;;
let cr_br x y z = { gcd = x; coef1 = y; coef2 = z };;
