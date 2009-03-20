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

(* $Id: misc_ml_generation.ml,v 1.19 2009-03-20 11:14:23 pessaux Exp $ *)



let pp_to_ocaml_label_ident ppf lab_ident =
  (* Just mask the previous [lbl_ident] to simply remove the only possible 
     constructor [LI], and to get the interesting information. *)
  let Parsetree.LI lab_ident = lab_ident.Parsetree.ast_desc in
  match lab_ident.Parsetree.ast_desc with
   | Parsetree.I_local vname ->
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
   | Parsetree.I_global qual_name ->
       let vname =
         (match qual_name with
          | Parsetree.Vname n -> n
          | Parsetree.Qualified (modname, n) ->
              Format.fprintf ppf "%s." (String.capitalize modname) ;
              n) in
       Format.fprintf ppf "%a"
         Parsetree_utils.pp_vname_with_operators_expanded vname
;;
