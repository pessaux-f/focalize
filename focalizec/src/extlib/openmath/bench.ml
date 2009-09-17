(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            David Delahaye                                           *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                        CNAM -  LIP6  -  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2009 CNAM, LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: bench.ml,v 1.1 2009-09-17 16:31:50 delahaye Exp $ *)

let _ =
  let fic = Sys.argv.(1) in
  let in_chan = open_in fic in
  let lexbuf = Lexing.from_channel in_chan in
  let result = Lexer.document lexbuf in 
  Printer.print_document Format.std_formatter result
