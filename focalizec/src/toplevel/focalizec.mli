(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2007 - ... LIP6 and INRIA                                *)
(*            2012 - ... ENSTA ParisTech                               *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)



exception Bad_file_suffix of string
exception Missing_external_tool of string
val main : unit -> unit
