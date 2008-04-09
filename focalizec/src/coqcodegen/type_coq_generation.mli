(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: type_coq_generation.mli,v 1.2 2008-04-09 13:01:47 pessaux Exp $ *)


val type_def_compile :
  Context.reduced_compil_context -> Env.CoqGenEnv.t ->
    Parsetree.vname -> Env.TypeInformation.type_description -> Env.CoqGenEnv.t
