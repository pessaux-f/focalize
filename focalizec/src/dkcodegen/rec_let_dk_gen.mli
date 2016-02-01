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

val generate_recursive_definition :
  Context.species_compil_context ->
  Dk_pprint.dk_print_context ->
  Env.DkGenEnv.t ->
  Parsetree.vname ->
  Parsetree.vname list ->
  Types.type_scheme ->
  Parsetree.expr ->
  abstract:bool ->
  toplevel:bool ->
  (?sep:string -> bool -> Format.formatter -> unit) -> unit

