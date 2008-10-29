(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2008 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lex_file.ml,v 1.2 2008-10-29 16:29:02 weis Exp $ *)

let lex_file fname =

  let ic = open_in_bin fname in
  let lexbuff = Lexing.from_channel ic in
  lexbuff.Lexing.lex_curr_p <-
    { Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; } ;

  let ppf = Format.err_formatter in

  while true do
    let tok = Lexer.token lexbuff in
    Format.fprintf ppf "%a@." Sourcify_token.token tok;
    if tok = Parser.EOF then raise Exit
  done
;;

let main () =
  let args = Sys.argv in
  if Array.length args < 1 then failwith "No file argument" else
  lex_file args.(1)
;;

try main () with
| Exit -> exit 0;;
