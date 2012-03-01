(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                 LIP6  --  INRIA Rocquencourt  -- ENSTA              *)
(*                                                                     *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                               *)
(*            2012 ENSTA ParisTech                                     *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: location.ml,v 1.7 2012-03-01 17:32:46 pessaux Exp $ *)


type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int
} ;;

(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)
type t = {
  l_beg : position;
  l_end : position
} ;;


let none =
  let pos_none =
    { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } in
  { l_beg = pos_none; l_end = pos_none }
;;


let pp_location ppf { l_beg = l_beg; l_end = l_end } =
  if l_beg.pos_lnum = l_end.pos_lnum then  (* Starts and ends on a same line. *)
    Format.fprintf ppf "File \"%s\", line %d, characters %d-%d"
      l_beg.pos_fname l_beg.pos_lnum
      (l_beg.pos_cnum - l_beg.pos_bol) (l_end.pos_cnum - l_end.pos_bol)
  else
    Format.fprintf ppf
      "File \"%s\", line %d, character %d, line %d, character %d"
      l_beg.pos_fname l_beg.pos_lnum (l_beg.pos_cnum - l_beg.pos_bol)
      l_end.pos_lnum (l_end.pos_cnum - l_end.pos_bol)
;;
