(* $Id: files.ml,v 1.1 2007-07-19 12:01:51 pessaux Exp $ *)

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


(** Paths for libraries lookup. *)
exception Cant_access_file of Parsetree.fname ;;
exception Corrupted_fo of Parsetree.fname ;;

let lib_paths = ref ([] : string list) ;;


let open_in_from_lib_paths filename =
  let rec rec_open  = function
    | [] -> raise (Cant_access_file filename)
    | h :: rem ->
	try open_in_bin (h ^ filename)
	with Sys_error _ -> rec_open rem in
  if Filename.is_relative filename then rec_open !lib_paths
  else open_in_bin filename
;;



(* **************************************************** *)
(* fo_filename_from_module_name: string -> string       *)
(** {b Descr} : Creates the compiled interface filename  
              corresponding to a "module" name.

    {b Rem} : Exported outside this module.             *)
(* **************************************************** *)
let fo_filename_from_module_name module_name =
  (String.uncapitalize  module_name) ^ ".fo"
;;


type magic = (char * char * char * char) ;;
let fo_magic = ('.', 'F', 'O', ' ') ;;

let check_magic in_handle (expected0, expected1, expected2, expected3) =
  let (magic0 : char) = input_value in_handle in
  let (magic1 : char) = input_value in_handle in
  let (magic2 : char) = input_value in_handle in
  let (magic3 : char) = input_value in_handle in
  expected0 = magic0 && expected1 = magic1 && expected2 = magic2 && 
  expected3 = magic3
;;



let write_magic out_handle (magic0, magic1, magic2, magic3) =
  output_value out_handle magic0 ;
  output_value out_handle magic1 ;
  output_value out_handle magic2 ;
  output_value out_handle magic3
;;
