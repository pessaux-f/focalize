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

(* $Id: type_coq_generation.mli,v 1.1 2008-04-08 15:10:55 pessaux Exp $ *)

val type_def_compile :
  Context.reduced_compil_context ->
  'a -> Parsetree.vname -> Env.TypeInformation.type_description -> 'a
