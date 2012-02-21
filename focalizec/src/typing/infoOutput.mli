(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: infoOutput.mli,v 1.1 2012-02-21 17:27:08 pessaux Exp $ *)

val methods_history_to_text :
  dirname:string ->
  current_species:string * Parsetree.vname ->
  Env.TypeInformation.species_field list -> unit

val methods_history_to_dot :
  dirname:string ->
  current_species:string * Parsetree.vname ->
  Env.TypeInformation.species_field list -> unit
