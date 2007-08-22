(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


(* $Id: location.mli,v 1.2 2007-08-22 14:17:08 pessaux Exp $ *) 

type position = Lexing.position = {
  pos_fname : string ;
  pos_lnum : int ;
  pos_bol : int ;
  pos_cnum : int
}

type t = {
 l_beg : position ;
 l_end : position
}
(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)

val none : t
val pp_location : Format.formatter -> t -> unit
