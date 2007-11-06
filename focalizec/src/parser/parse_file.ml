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

(* $Id: parse_file.ml,v 1.7 2007-11-06 10:14:58 pessaux Exp $ *)

open Parsing ;;

exception Lex_error of (Lexing.position * Lexing.position * string) ;;
exception Syntax_error of (Lexing.position * Lexing.position) ;;
exception Unclear_error of (string * Lexing.position * Lexing.position) ;;


let wrap parsing_fun lexbuf =
  let ast = parsing_fun Lexer.token lexbuf in
  Parsing.clear_parser ();
  ast
;;

let implementation lexbuf = wrap Parser.file lexbuf
;;

let pp_err_loc ppf (sp, ep) =
  Format.fprintf ppf
    "File %S, line %i, characters %i-%i:@."
    sp.Lexing.pos_fname
    sp.Lexing.pos_lnum
    (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
    (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
;;

let parse_file fname =
  let ic = open_in_bin fname in
  let lexbuff = Lexing.from_channel ic in
  lexbuff.Lexing.lex_curr_p <-
    { Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; } ;
  try
    let ast =
      try implementation lexbuff with
      | x ->
          close_in ic ;
          raise x in
    close_in ic ;
    ast
  with
  | Lexer.Error (err, sp, ep) ->
      raise (Lex_error (sp, ep, (Lexer.string_of_lex_error err)))
  | Parsing.Parse_error ->
      let sp = Lexing.lexeme_start_p lexbuff in
      let ep = Lexing.lexeme_end_p lexbuff in
      raise (Syntax_error (sp, ep)) ;
  | x ->
      let sp = Lexing.lexeme_start_p lexbuff in
      let ep = Lexing.lexeme_end_p lexbuff in
      raise (Unclear_error (Printexc.to_string x, sp, ep)) ;
;;
