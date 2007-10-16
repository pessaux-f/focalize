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

(* $Id: base_exprs_ml_generation.mli,v 1.2 2007-10-16 10:00:48 pessaux Exp $ *)


val let_def_compile :
  Misc_ml_generation.reduced_compil_context ->
    local_idents: Parsetree.vname list ->
      Parsetree.let_def -> Types.type_scheme option list -> unit
val generate_expr :
  Misc_ml_generation.reduced_compil_context ->
    local_idents: Parsetree.vname list -> Parsetree.expr -> unit
