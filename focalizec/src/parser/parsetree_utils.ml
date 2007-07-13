(* $Id: parsetree_utils.ml,v 1.2 2007-07-13 15:16:38 pessaux Exp $ *)
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


(* **************************************************** *)
(*  [Fun] string_of_vname : Parsetree.vname -> string   *)
(** [Descr] : Extracts the inner string of the [vname].

    [Rem] : Exported outside this module.               *)
(* **************************************************** *)
let string_of_vname = function
  | Parsetree.Vlident s | Parsetree.Vuident s | Parsetree.Vpident s
  | Parsetree.Viident s | Parsetree.Vqident s -> s
;;
