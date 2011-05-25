(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            Manuel Maarek                                            *)
(*            Pierre Weis                                              *)
(*            Renaud Rioboo                                            *)
(*            Matthieu Carlier                                         *)
(*                                                                     *)
(*                               CEDRIC  --  INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2011 CEDRIC and INRIA                                    *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: testing.ml,v 1.1 2011-05-25 06:35:22 maarek Exp $ *)

let add_tests_suffix file_name = file_name ^ "_TESTS"
;;

let contains_testing_intructions ast =
  match ast.Parsetree.ast_desc with
  | Parsetree.File phrases ->
      List.exists
        (fun ph ->
          match ph.Parsetree.ast_desc with
          | Parsetree.Ph_annotation_title
          | Parsetree.Ph_use _
          | Parsetree.Ph_open _
          | Parsetree.Ph_coq_require _
          | Parsetree.Ph_species _
          | Parsetree.Ph_collection _
          | Parsetree.Ph_type _
          | Parsetree.Ph_let _
          | Parsetree.Ph_theorem _
          | Parsetree.Ph_expr _ -> false
          | Parsetree.Ph_testing _ -> true )
        phrases

let gen_tests ~current_unit ~output_file_name (_scoped_ast:Parsetree.file) =
  let tests_ast =
    Parsetree_utils.make_ast
      (Parsetree.File
         [Parsetree_utils.make_ast
            (Parsetree.Ph_open current_unit)])
    
  in
  let out_hd = open_out_bin output_file_name in
  let out_fmt = Format.formatter_of_out_channel out_hd in
  Sourcify.pp_file out_fmt tests_ast ;
  close_out out_hd
;;
