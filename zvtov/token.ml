(*  Copyright 2004 INRIA  *)
(*  $Id: token.ml,v 1.5 2005-11-13 22:49:11 doligez Exp $  *)

type t =
  | REQUIRE of string
  | SECTION of string
(*
  | TOBE of string
*)
  | CHAR of string
  | AUTOPROOF of string * string * string * string
  | EOF
;;

type lemma_kind =
  | LEMMA of int list
  | GOAL
  | TOP
;;
