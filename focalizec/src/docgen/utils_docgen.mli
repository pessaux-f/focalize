(***********************************************************************)
(*                                                                     *)
(*                        FoCaLiZe compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: utils_docgen.mli,v 1.2 2009-06-16 09:36:43 weis Exp $ *)

val xmlify_string : string -> string

val pp_xml_vname : Format.formatter -> Parsetree.vname -> unit

val get_in_file_and_name_from_ident :
  current_unit: Parsetree.module_name ->
    Parsetree.ident -> (Parsetree.module_name * Parsetree.vname)

val find_annotation_of_method :
  Parsetree.vname -> Parsetree.species_field list -> Parsetree.annotation

val extract_tagged_info_from_annotation :
  Parsetree.annot_elem list ->
    (string option * string option * string option * string option *
     string option * string option)

val find_title_author_and_description :
  Parsetree.file_desc Parsetree.ast ->
    (string option * string option * string option)
