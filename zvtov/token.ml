(*  Copyright 2004 INRIA  *)
(*  $Id: token.ml,v 1.2 2004-06-01 11:56:29 doligez Exp $  *)

type t =
  | SECTION of string
  | TOBE of string
  | CHAR of string
  | EOF
;;

type lemma_kind =
  | LEMMA of int list
  | GOAL
  | TOP
;;
