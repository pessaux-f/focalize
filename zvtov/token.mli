(*  Copyright 2004 INRIA  *)
(*  $Id: token.mli,v 1.4 2005-07-01 12:26:22 prevosto Exp $  *)

type t =
  | REQUIRE of string
  | SECTION of string
  | TOBE of string
  | CHAR of string
  | AUTOPROOF of string * string * string * string
  | EOF
;;

type lemma_kind =
  | LEMMA of int list
  | GOAL
  | TOP
;;
