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


(* $Id: scoping.mli,v 1.4 2007-08-14 11:04:19 pessaux Exp $ *) 

exception Multiply_used_module of Parsetree.fname
exception Module_not_specified_as_used of Parsetree.fname

val scope_file :
  Parsetree.fname -> Parsetree.file -> Parsetree.file * Env.ScopingEnv.t
