(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sourcify.mli,v 1.11 2007-10-16 10:00:48 pessaux Exp $ *)


val pp_vname : Format.formatter -> Parsetree.vname -> unit
val pp_vnames : string -> Format.formatter -> Parsetree.vname list -> unit
val pp_may_be_qualified_vname :
  Format.formatter -> Parsetree.may_be_qualified_vname -> unit 
val pp_qualified_vname : Format.formatter -> Parsetree.qualified_vname -> unit
val pp_ident : Format.formatter -> Parsetree.ident -> unit
val pp_constructor_ident :
  Format.formatter -> Parsetree.constructor_ident -> unit
val pp_file : Format.formatter -> Parsetree.file -> unit
;;
(** The pretty printer for focalize abstract syntax trees that generates
    focalize source code. *)


(* Exported for debug. To disapear from interface. *)
val pp_expr : Format.formatter -> Parsetree.expr -> unit
val pp_coll_def : Format.formatter -> Parsetree.coll_def -> unit
