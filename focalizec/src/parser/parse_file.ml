(* $Id: parse_file.ml,v 1.3 2007-06-28 08:35:56 weis Exp $ *)

open Parsing;;

let wrap parsing_fun lexbuf =
  let ast = parsing_fun Lexer.token lexbuf in
  Parsing.clear_parser ();
  ast
;;

let implementation lexbuf = wrap Parser.file lexbuf
;;

let print_err_loc ppf (sp, ep) =
    Format.fprintf ppf
      "File %S, line %i, characters %i-%i:@."
      sp.Lexing.pos_fname
      sp.Lexing.pos_lnum
      (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
      (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
;;

let parse_file ppf fname =
  let ic = open_in_bin fname in
  let lexbuff = Lexing.from_channel ic in
  lexbuff.Lexing.lex_curr_p <-
    { Lexing.pos_fname = fname; pos_lnum = 1; pos_bol = 0; pos_cnum = 0; };
  try
    let ast =
      try implementation lexbuff with
      | x -> close_in ic; raise x in
    close_in ic;
    ast with
  | Lexer.Error (err, sp, ep) ->
    Format.fprintf ppf "%aLexical error, %a.@."
      print_err_loc (sp, ep) Lexer.report_error err;
    raise Exit
  | Parsing.Parse_error ->
    let sp = Lexing.lexeme_start_p lexbuff in
    let ep = Lexing.lexeme_end_p lexbuff in
    Format.fprintf ppf "%aSyntax error.@."
      print_err_loc (sp, ep);
    raise Exit
  | x ->
    let sp = Lexing.lexeme_start_p lexbuff in
    let ep = Lexing.lexeme_end_p lexbuff in
    Format.fprintf ppf "%aSyntax error: %s.@."
      print_err_loc (sp, ep) (Printexc.to_string x);
    raise Exit
;;
