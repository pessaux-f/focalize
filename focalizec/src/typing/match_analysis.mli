(* ************************************************************************** *)
(*                                                                            *)
(*                        FoCaLiZe compiler                                   *)
(*                                                                            *)
(*            Pierre-Nicolas Tollitte                                         *)
(*            François Pessaux                                                *)
(*                                                                            *)
(*               LIP6  --  INRIA Rocquencourt -- ENSTA ParisTech              *)
(*                                                                            *)
(*  Copyright 2007 - ... LIP6 and INRIA                                       *)
(*            2012 - ... ENSTA ParisTech                                      *)
(*  Distributed only by permission.                                           *)
(*                                                                            *)
(* ************************************************************************** *)

(* Exceptions for pattern matching analysis. *)
exception Match_not_exhaustive of Location.t
exception Match_useless_case of Location.t


(* Search for pattern matching expressions in the AST and verify them. *)
val verify_matchings :
  current_unit: Types.fname -> Env.TypingEnv.t -> Infer.please_compile_me ->
    unit
