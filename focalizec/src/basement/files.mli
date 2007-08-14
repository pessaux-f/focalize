(* $Id: files.mli,v 1.5 2007-08-14 13:20:26 pessaux Exp $ *)

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


exception Cant_access_file_in_search_path of Parsetree.fname
exception Corrupted_fo of Parsetree.fname


val add_lib_path : string -> unit
val get_lib_paths : unit -> string list
val open_in_from_lib_paths : Parsetree.fname -> in_channel
val fo_basename_from_module_name : string -> string
type magic
val fo_magic : magic
val check_magic : in_channel -> magic -> bool
val write_magic : out_channel -> magic -> unit
