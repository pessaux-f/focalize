(*  Copyright 2004 INRIA  *)
(*  $Id: token.ml,v 1.1 2004-05-19 15:13:41 doligez Exp $  *)

type t =
  | SECTION of string
  | LEMMA of string
  | GOAL of string
  | TOBE of string
  | CHAR of string
  | EOF
;;
