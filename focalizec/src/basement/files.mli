(* $Id: files.mli,v 1.3 2007-08-13 17:29:34 pessaux Exp $ *)

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


exception Cant_access_file of Parsetree.fname
exception Corrupted_fo of Parsetree.fname


val lib_paths : string list ref
val open_in_from_lib_paths : Parsetree.fname -> in_channel
val fo_basename_from_module_name : string -> string
type magic
val fo_magic : magic
val check_magic : in_channel -> magic -> bool
val write_magic : out_channel -> magic -> unit

