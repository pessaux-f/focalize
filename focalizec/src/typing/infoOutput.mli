(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - 2012 LIP6 and INRIA                                      *)
(*            2012 ENSTA ParisTech                                            *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)


val methods_history_to_text :
  dirname:string ->
  current_species:string * Parsetree.vname ->
  Env.TypeInformation.species_field list -> unit

val methods_history_to_dot :
  dirname:string ->
  current_species:string * Parsetree.vname ->
  Env.TypeInformation.species_field list -> unit
