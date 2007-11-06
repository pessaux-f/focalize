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

(* $Id: base_exprs_ml_generation.mli,v 1.3 2007-11-06 10:14:58 pessaux Exp $ *)


val let_def_compile :
  Misc_ml_generation.reduced_compil_context ->
    local_idents: Parsetree.vname list -> Env.MlGenEnv.t ->
      Parsetree.let_def -> Types.type_scheme option list -> unit
val generate_expr :
  Misc_ml_generation.reduced_compil_context ->
    local_idents: Parsetree.vname list -> Env.MlGenEnv.t -> Parsetree.expr ->
      unit
