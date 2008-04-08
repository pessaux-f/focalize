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

(* $Id: misc_ml_generation.ml,v 1.18 2008-04-08 15:10:55 pessaux Exp $ *)



let pp_to_ocaml_label_ident ppf lab_ident =
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.LI qual_name ->
       let vname =
         (match qual_name with
          | Parsetree.Vname n -> n
          | Parsetree.Qualified (modname, n) ->
              Format.fprintf ppf "%s." (String.capitalize modname) ;
              n) in
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
;;
