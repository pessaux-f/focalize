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

(* $Id: parsetree_utils.ml,v 1.3 2007-10-10 15:27:43 pessaux Exp $ *)

let name_of_vname = function
  | Parsetree.Vlident s
  | Parsetree.Vuident s
  | Parsetree.Vpident s
  | Parsetree.Viident s
  | Parsetree.Vqident s -> s
;;


(* *************************************************************** *)
(** {b Descr} : Module stuff to create sets of [Parsetree.vname]s.
              This will serves to make sets of methods [vname]s in
              order to represent dependencies.

    {b Rem} : Not exported outside this module.                    *)
(* *************************************************************** *)
module VnameMod = struct type t = Parsetree.vname let compare = compare end ;;
module VnameSet = Set.Make (VnameMod) ;;
