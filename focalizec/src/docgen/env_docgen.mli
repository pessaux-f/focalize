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

(* $Id: env_docgen.mli,v 1.1 2009-01-14 10:44:19 pessaux Exp $ *)


type symbol_description = {
  sd_mathml : string option;
  sd_latex : string option;
}
type t

val empty : t
val add_method : Parsetree.vname -> string option -> string option -> t -> t
val find_method : Parsetree.vname -> t -> symbol_description
