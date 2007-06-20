(*  Copyright 2004 INRIA  *)
(*  $Id: lexer_coq.mll,v 1.4 2007-06-20 10:50:49 doligez Exp $  *)
{
open Parser_coq;;
open Lexing;;

exception Lex_error of string;;

}

let newline = ('\010' | '\013' | "\013\010")
let inline = [^ '\010' '\013' ]
let space = [' ' '\009' '\012']
let blank = [' ' '\009' '\012' '\010' '\013']
let identchar =  [^ '\000'-'\031' '\"' '\127'-'\255' '(' ')' ' ' '#' ';' '$']
let stringchar = [^ '\000'-'\031' '\"' '\127'-'\255']
let upper = [ 'A' - 'Z' ]
let lower = [ 'a' - 'z' ]
let tpidchar = [ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' ]

let coqidbegin = [ 'A' - 'Z' 'a' - 'z' '_' ]
let coqidchar = [ 'A' - 'Z' 'a' - 'z' '0' - '9' '_' '\'' ]

rule coqtoken = parse

  | "(*"                    { coqcomment lexbuf }

  | newline     { lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                    pos_bol = lexbuf.lex_curr_p.pos_cnum;
                    pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
                  };
                  coqtoken lexbuf }
  | space +                 { coqtoken lexbuf }

  | "!"                     { BANG_ }
  | "%"                     { PERCENT_ }
  | "&"                     { AMPER_ }
  | "&&"                    { AMPER_AMPER_ }
  | "("                     { LPAREN_ }
  | "()"                    { LPAREN_RPAREN_ }
  | ")"                     { RPAREN_ }
  | "*"                     { STAR_ }
  | "+"                     { PLUS_ }
  | "++"                    { PLUS_PLUS_ }
  | ","                     { COMMA_ }
  | "-"                     { DASH_ }
  | "->"                    { DASH_GT_ }
  | "."                     { PERIOD_ }
  | ".("                    { PERIOD_LPAREN_ }
  | ".."                    { PERIOD_PERIOD_ }
  | "/"                     { SLASH_ }
  | "/\\"                   { SLASH_BACKSL_ }
  | ":"                     { COLON_ }
  | "::"                    { COLON_COLON_ }
  | ":<"                    { COLON_LT_ }
  | ":="                    { COLON_EQ_ }
  | ":>"                    { COLON_GT_ }
  | ";"                     { SEMI_ }
  | "<"                     { LT_ }
  | "<-"                    { LT_DASH_ }
  | "<->"                   { LT_DASH_GT_ }
  | "<:"                    { LT_COLON_ }
  | "<="                    { LT_EQ_ }
  | "<>"                    { LT_GT_ }
  | "="                     { EQ_ }
  | "=>"                    { EQ_GT_ }
  | "=_D"                   { EQ_UNDER_D_ }
  | ">"                     { GT_ }
  | ">->"                   { GT_DASH_GT_ }
  | ">="                    { GT_EQ_ }
  | "?"                     { QUEST_ }
  | "?="                    { QUEST_EQ_ }
  | "@"                     { AROBAS_ }
  | "["                     { LBRACK_ }
  | "\\/"                   { BACKSL_SLASH_ }
  | "]"                     { RBRACK_ }
  | "^"                     { HAT_ }
  | "{"                     { LBRACE_ }
  | "|"                     { BAR_ }
  | "|-"                    { BAR_DASH_ }
  | "||"                    { BAR_BAR_ }
  | "}"                     { RBRACE_ }
  | "~"                     { TILDE_ }

  | "as"                    { AS }
  | "at"                    { AT }
  | "cofix"                 { COFIX }
  | "Depends"               { DEPENDS }
  | "Definition"            { DEFINITION }
  | "else"                  { ELSE }
  | "end"                   { END }
  | "exists"                { EXISTS }
  | "exists2"               { EXISTS2 }
  | "fix"                   { FIX }
  | "for"                   { FOR }
  | "forall"                { FORALL }
  | "fun"                   { FUN }
  | "if"                    { IF }
  | "IF"                    { UC_IF }
  | "in"                    { IN }
  | "let"                   { LET }
  | "match"                 { MATCH }
  | "mod"                   { MOD }
  | "on"                    { ON }
  | "Parameter"             { PARAMETER }
  | "Prop"                  { PROP }
  | "return"                { RETURN }
  | "Set"                   { SET }
  | "then"                  { THEN }
  | "Theorem"               { THEOREM }
  | "Type"                  { TYPE }
  | "using"                 { USING }
  | "where"                 { WHERE }
  | "with"                  { WITH }

  | coqidbegin coqidchar *  ('.' coqidbegin coqidchar*)*
    { let s = Lexing.lexeme lexbuf in
        if String.contains s '.' then FQN s else IDENT s }
  | ['0' - '9'] +
    { IDENT (Lexing.lexeme lexbuf) }

  | "%%begin-auto-proof" inline*
      { BEGINPROOF }
  | "%%name:" space* (coqidchar+ as name) space*
      { BEGINNAME name }
  | "%%" coqidchar* ":" inline*
      { BEGINHEADER }
  | "%%end-auto-proof"      { ENDPROOF }

  | eof                     { EOF }

  | "\"" stringchar + "\"" {
      let s = Lexing.lexeme lexbuf in
      STRING (String.sub s 1 (String.length s - 2))
    }

  | _           { raise (Lex_error ("bad character " ^ Lexing.lexeme lexbuf)) }

and coqcomment = parse
  | "*)"                    { coqtoken lexbuf }
  | [^ '\010' '\013']       { coqcomment lexbuf }
  | newline     { lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                    pos_bol = lexbuf.lex_curr_p.pos_cnum;
                    pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
                  };
                  coqcomment lexbuf }
