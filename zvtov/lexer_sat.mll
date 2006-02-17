{
open Parser_sat;;
open Lexing;;

exception Lex_error of string;;

}

let newline = ('\010' | '\013' | "\013\010")
let space = [' ' '\009' '\012']
let blank = [' ' '\009' '\012' '\010' '\013']
let identchar =  [^ '\000'-'\031' '\"' '\127'-'\255' '(' ')' ' ' '#' ';' '$']
let stringchar = [^ '\000'-'\031' '\"' '\127'-'\255']
let upper = [ 'A' - 'Z' ]
let lower = [ 'a' - 'z' ]
let tpidchar = [ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' ]

let coqidbegin = [ 'A' - 'Z' 'a' - 'z' '_' ]
let coqidchar = [ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' '\'' ]


rule cimetoken = parse

  | "unsatifiable"         { (* print_string "unsatisfiable bien recu"; *) 
			     UNSATISFIABLE}

  | eof                     { EOF }

  | _           { lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                    pos_bol = lexbuf.lex_curr_p.pos_cnum;
                    pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
                  };cimetoken lexbuf }
