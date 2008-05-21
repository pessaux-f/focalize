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

(* $Id: sourcify.mli,v 1.19 2008-05-21 09:06:01 pessaux Exp $ *)

(** Printing source files from AST. *)

val pp_vname : Format.formatter -> Parsetree.vname -> unit
val pp_vnames : string -> Format.formatter -> Parsetree.vname list -> unit
val pp_qualified_vname : Format.formatter -> Parsetree.qualified_vname -> unit
val pp_qualified_species :
    Format.formatter -> Parsetree.qualified_species -> unit
val pp_ident : Format.formatter -> Parsetree.ident -> unit
val pp_expr_ident : Format.formatter -> Parsetree.expr_ident -> unit
val pp_label_ident : Format.formatter -> Parsetree.label_ident -> unit
val pp_constructor_ident :
  Format.formatter -> Parsetree.constructor_ident -> unit
val pp_file : Format.formatter -> Parsetree.file -> unit
val pp_logical_expr : Format.formatter -> Parsetree.logical_expr -> unit
(** The pretty printer for focalize abstract syntax trees that generates
    focalize source code. *)

(* Exported for debug. To disapear from interface. *)
val pp_expr : Format.formatter -> Parsetree.expr -> unit
val pp_collection_def : Format.formatter -> Parsetree.collection_def -> unit
val pp_species_expr : Format.formatter -> Parsetree.species_expr -> unit
val pp_species_field : Format.formatter -> Parsetree.species_field -> unit
val pp_proof : Format.formatter -> Parsetree.proof -> unit
