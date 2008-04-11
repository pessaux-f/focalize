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

(* $Id: type_coq_generation.mli,v 1.3 2008-04-11 08:52:05 pessaux Exp $ *)

exception Mutable_record_fields_not_in_coq of (Location.t * Parsetree.vname)

val type_def_compile :
  Context.reduced_compil_context -> Env.CoqGenEnv.t ->
    Parsetree.vname -> Env.TypeInformation.type_description -> Env.CoqGenEnv.t
