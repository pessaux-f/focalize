open Parsing;;

let wrap parsing_fun lexbuf =
  let ast = parsing_fun Lexer.token lexbuf in
  Parsing.clear_parser ();
  ast
;;

let implementation lexbuf = wrap Parser.file lexbuf
;;

let print_err_loc ppf (fname, bpos, ep) =
    Format.fprintf ppf
      "File %S, line %i, character %i-%i:@."
      fname
      ep.Lexing.pos_lnum
      bpos
      ep.Lexing.pos_cnum
;;

let parse_file ppf fname =
  let ic = open_in_bin fname in
  let lexbuff = Lexing.from_channel ic in
  try
    let ast =
      try implementation lexbuff with
      | x -> close_in ic; raise x in
    close_in ic;
    ast with
  | Lexer.Error (err, ep) ->
    let bpos = Lexing.lexeme_start lexbuff in
    Format.fprintf ppf "%aLexical error, %a.@."
      print_err_loc (fname, bpos, ep) Lexer.report_error err;
    raise Exit
  | Parsing.Parse_error ->
    let bpos = Lexing.lexeme_start lexbuff in
    let ep = Lexing.lexeme_end_p lexbuff in
    Format.fprintf ppf "%aSyntax error.@."
      print_err_loc (fname, bpos, ep);
    raise Exit
  | x ->
    let bpos = Lexing.lexeme_start lexbuff in
    let eloc = Lexing.lexeme_end_p lexbuff in
    Format.fprintf ppf "%aSyntax error: %s.@."
      print_err_loc (fname, bpos, eloc) (Printexc.to_string x);
    raise Exit
;;
