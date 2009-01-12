(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main_docgen.mli,v 1.1 2009-01-12 13:37:26 pessaux Exp $ *)

val gen_doc_please_compile_me :
  string ->
  Parsetree.file_desc Parsetree.ast -> Infer.please_compile_me list -> unit
