(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: anti_keyword_conflict.mli,v 1.1 2012-02-10 16:24:40 pessaux Exp $ *)

module StrMod : sig type t = string val compare : 'a -> 'a -> int end

module StrMap :
  sig
    type key = StrMod.t
    type 'a t = 'a Map.Make(StrMod).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
  end

val lowerc_keywords_map : (string StrMap.t) ref

