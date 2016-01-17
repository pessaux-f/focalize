(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Raphaël Cauderlier                                              *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

exception Logical_methods_only_inside_species of Location.t

val toplevel_let_def_compile :
  Context.species_compil_context ->
  Env.DkGenEnv.t ->
  Parsetree.let_def ->
  Env.DkGenEnv.t
