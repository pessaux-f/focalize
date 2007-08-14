(* $Id: files.mli,v 1.4 2007-08-14 11:04:19 pessaux Exp $ *)

(***********************************************************************)
(*                                                                     *)
(*                        FoCaL compiler                               *)
(*            Fran�ois Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)


exception Cant_access_file of Parsetree.fname
exception Corrupted_fo of Parsetree.fname


val add_lib_path : string -> unit
val get_lib_paths : unit -> string list
val open_in_from_lib_paths : Parsetree.fname -> in_channel
val fo_basename_from_module_name : string -> string
type magic
val fo_magic : magic
val check_magic : in_channel -> magic -> bool
val write_magic : out_channel -> magic -> unit

