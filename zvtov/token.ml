(*  Copyright 2004 INRIA  *)
(*  $Id: token.ml,v 1.6 2006-02-02 13:30:03 doligez Exp $  *)

type t =
  | REQUIRE
  | CHAR of char
  | BEGINAUTOPROOF
  | LOCATION of string
  | NAME of string
  | SYNTAX of string
  | STATEMENT of string
  | ENDAUTOPROOF
  | EOF
;;

type lemma_kind =
  | LEMMA of int list
  | GOAL
  | TOP
;;
