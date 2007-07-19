(* $Id: parsetree_utils.ml,v 1.1 2007-07-19 12:01:51 pessaux Exp $ *)

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


(* ****************************************************** *)
(*  string_of_vname : Parsetree.vname -> string           *)
(** {b Descr} : Extracts the inner string of the [vname].

    {b Rem} : Exported outside this module.               *)
(* ****************************************************** *)
let string_of_vname = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s -> s
;;
