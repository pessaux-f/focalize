(*  Copyright 2004 INRIA  *)
(*  $Id: lexer.mll,v 1.11 2006-02-06 17:56:06 doligez Exp $  *)
{
open Token;;
}

let space = [ ' ' '\009' ]
let bol = '\010'
let idchar = [ '0'-'9' 'A'-'Z' 'a'-'z' '_' ]

rule token = parse
  | "Require"
      { REQUIRE }
  | bol space* "%%begin-auto-proof"
      { BEGINAUTOPROOF }
  | bol space* "%%location:" space* '[' ([^ ']']* as loc) ']'
      { LOCATION loc }
  | bol space* "%%name:" space* (idchar+ as name)
      { NAME name }
  | bol space* "%%syntax:" space* (idchar+ as syntax)
      { SYNTAX syntax }
  | bol space* "%%statement:" ([^'.']+ as statement) '.'
      { STATEMENT statement }
  | bol space* "%%end-auto-proof"
      { ENDAUTOPROOF }
  | eof
      { EOF }
  | _
      { CHAR (Lexing.lexeme lexbuf).[0] }
