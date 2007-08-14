(* $Id: files.ml,v 1.5 2007-08-14 11:04:19 pessaux Exp $ *)

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



let (add_lib_path, get_lib_paths) =
  let lib_paths = ref ([] : string list) in
  ((* ************************************************************* *)
   (* add_lib_path                                                  *)
   (* string -> unit                                                *)
   (* {b Descr} : Add a path to the list of filesystem directories
                where to look for ".fo" (compiled interface) files.

      {b Rem} : Exported outside this module.                       *)
   (* ************************************************************* *)
   (fun path -> lib_paths := path :: !lib_paths),



   (* ************************************************************** *)
   (* get_lib_paths                                                  *)
   (* unit -> string list.                                           *)
   (* {b Descr} : Returns the current list of filesystem directories
                where to look for ".fo" (compiled interface) files.

      {b Rem} : Exported outside this module.                        *)
   (* ************************************************************** *)
   (fun () -> !lib_paths))
;;



(* ********************************************************************* *)
(* Parsetree.fname -> in_channel                                         *)
(* {b Descr} : try to open a file in read-binary-mode, searching for it
             in the directories indicated by the "lib_path" if the file
             has a relative name. Otherwise, if the file has an absolute
             name, it tries to open it straight.
             If opening fails, then a [Cant_access_file] exception is
             raised.

   {b Rem} : Exported outside this module.                               *)
(* ********************************************************************* *)
let open_in_from_lib_paths filename =
  let rec rec_open  = function
    | [] ->
	(begin
	try open_in_bin filename
	with Sys_error _ -> raise (Cant_access_file filename)
	end)
    | h :: rem ->
	try open_in_bin (h ^ filename)
	with Sys_error _ -> rec_open rem in
  if Filename.is_relative filename then rec_open (get_lib_paths ())
  else open_in_bin filename
;;



(* ************************************************************** *)
(* string -> string                                               *)
(** {b Descr} : Creates the "compiled interface" file basename
              (i.e. without path in the filesystem) corresponding
              to a "module" name.

    {b Rem} : Exported outside this module.                       *)
(* ************************************************************** *)
let fo_basename_from_module_name module_name =
  (* Remove the path if some, because fnames can be used *)
  (* with a path in the "use" and "open" directives.     *)
  (Filename.basename module_name) ^ ".fo"
;;



(* ******************************************************************* *)
(** {b Descr} : Type of the magic numbers used as first value inside a
              file in order to identify its type.

   {b Rem} : Exported abstract outside this module.                    *)
(* ******************************************************************* *)
type magic = (char * char * char * char) ;;



(* ************************************************************* *)
(* magic                                                         *)
(** {b Descr} : Magic number used to identify FoCaL ".fo" files.
              Currently, it the sequence of the 4 characters of
              the string ".FO ".

    {b Rem} : Exported abstract outside this module.             *)
(* ************************************************************* *)
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
