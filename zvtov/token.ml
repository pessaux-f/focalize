(*  Copyright 2004 INRIA  *)
(*  $Id: token.ml,v 1.3 2004-06-02 17:08:10 doligez Exp $  *)

type t =
  | REQUIRE of string
  | SECTION of string
  | TOBE of string
  | CHAR of string
  | AUTOPROOF of string * string
  | EOF
;;

type lemma_kind =
  | LEMMA of int list
  | GOAL
  | TOP
;;
