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


(* $Id: core_generation.ml,v 1.1 2007-09-13 08:45:11 pessaux Exp $ *)


let compile_phrase global_env phrase =
  match phrase.Parsetree.ast_desc with
   | Parsetree.Ph_external exter_def ->
   | Parsetree.Ph_use _ ->
   | Parsetree.Ph_open fname ->
   | Parsetree.Ph_species species_def ->
   | Parsetree.Ph_coll coll_def ->
   | Parsetree.Ph_type type_def ->
   | Parsetree.Ph_let let_def  ->
   | Parsetree.Ph_theorem theorem_def ->
   | Parsetree.Ph_expr expr ->
;;



let compile_file current_unit global_env ast_file =
  match ast_file.Parsetree.ast_desc with
   | Parsetree.File phrases -> List.iter (compile_phrase global_env) phrases
;;
