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

(* $Id: utils_docgen.mli,v 1.1 2009-01-13 14:34:09 pessaux Exp $ *)



val xmlify_string : string -> string

val pp_xml_vname : Format.formatter -> Parsetree.vname -> unit

val get_in_file_and_name_from_ident :
  current_unit: Parsetree.module_name ->
    Parsetree.ident -> (Parsetree.module_name * Parsetree.vname)

val find_documentation_of_method :
  Parsetree.vname -> Parsetree.species_field list -> Parsetree.documentation

val extract_tagged_info_from_documentation :
  Parsetree.doc_elem list ->
    (string option * string option * string option * string option *
     string option * string option)

val find_title_author_and_description :
  Parsetree.file_desc Parsetree.ast ->
    (string option * string option * string option)
