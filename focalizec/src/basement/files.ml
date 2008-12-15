(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: files.ml,v 1.13 2008-12-15 17:52:06 pessaux Exp $ *)


(** Paths for libraries lookup. *)
exception Cant_access_file_in_search_path of Types.fname ;;
exception Corrupted_fo of Types.fname ;;



let (add_lib_path, get_lib_paths) =
  let lib_paths = ref ([] : string list) in
  ((* ********************************************************************* *)
   (* string -> unit                                                        *)
   (* {b Descr} : Add a path to the list of filesystem directories where to
        look for ".fo" (compiled interface) files. The new path is added in
        tail of the list, hence will be search after all those already in
        the list.

      {b Rem} : Exported outside this module.                               *)
   (* ********************************************************************* *)
   (fun path -> lib_paths := !lib_paths @ [path]),



   (* *********************************************************************** *)
   (* get_lib_paths                                                           *)
   (* unit -> string list.                                                    *)
   (* {b Descr} : Returns the current list of filesystem directories where to
        look for ".fo" (compiled interface) files.

      {b Rem} : Exported outside this module.                                 *)
   (* *********************************************************************** *)
   (fun () -> !lib_paths))
;;



(* ********************************************************************** *)
(* Types.fname -> in_channel                                              *)
(* {b Descr} : If the file has a relative name, try to open a file in
     read-binary-mode, searching for it FIRST in the current directory,
     then if not found, in the directories indicated by the "lib_path".
     Otherwise, if the file has an absolute name, it tries to open it
     straight.
     If opening fails, then a [Cant_access_file_in_search_path] exception
     is raised.
     Search is done in the order the paths appear in the list.
   {b Rem} : Exported outside this module.                                *)
(* ********************************************************************** *)
let open_in_from_lib_paths filename =
  let rec rec_open  = function
    | [] ->
        (begin
        try open_in_bin filename
        with Sys_error _ -> raise (Cant_access_file_in_search_path filename)
        end)
    | h :: rem ->
        try open_in_bin (Filename.concat h filename)
        with Sys_error _ -> rec_open rem in
  if Filename.is_relative filename then
    (begin
    (* First, try in the current local directory. *)
    try open_in_bin filename with Sys_error _ -> rec_open (get_lib_paths ())
    end)
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
  (* Remove the path if some, because fnames can be used with a path in the
     "use" and "open" directives. *)
  (Filename.basename module_name) ^ ".fo"
;;



(* ******************************************************************* *)
(** {b Descr} : Type of the magic numbers used as first value inside a
              file in order to identify its type.

   {b Rem} : Exported abstract outside this module.                    *)
(* ******************************************************************* *)
type magic = (char * char * char * char);;



(* ************************************************************* *)
(* magic                                                         *)
(** {b Descr} : Magic number used to identify FoCaLize ".fo" files.
              Currently, it the sequence of the 4 characters of
              the string ".FO ".

    {b Rem} : Exported abstract outside this module.             *)
(* ************************************************************* *)
let fo_magic = ('.', 'F', 'O', ' ');;



let check_magic in_handle (expected0, expected1, expected2, expected3) =
  try
    let (magic0 : char) = input_value in_handle in
    let (magic1 : char) = input_value in_handle in
    let (magic2 : char) = input_value in_handle in
    let (magic3 : char) = input_value in_handle in
    (* First check that the file is really a ".fo" file. *)
    let good_format =
      expected0 = magic0 && expected1 = magic1 && expected2 = magic2 &&
      expected3 = magic3 in
    if good_format then
      (begin
      (* Now, ensure that it was generated with the current version of the
	 compiler. *)
      let (read_major : int) = input_value in_handle in
      let (read_minor : int) = input_value in_handle in
      let (read_patch_level : int) = input_value in_handle in
      let (major, minor, patch_level) = Configuration.focalize_version_number in
      read_major = major && read_minor = minor && read_patch_level = patch_level
      end)
    else false
  with End_of_file ->
    (* If we can't even read the magic information then the file is corrupted
       or not a FoCaLize object file. *)
    false
;;



let write_magic out_handle (magic0, magic1, magic2, magic3) =
  output_value out_handle magic0 ;
  output_value out_handle magic1 ;
  output_value out_handle magic2 ;
  output_value out_handle magic3 ;
  let (major, minor, patch_level) = Configuration.focalize_version_number in
  output_value out_handle major ;
  output_value out_handle minor ;
  output_value out_handle patch_level
;;
