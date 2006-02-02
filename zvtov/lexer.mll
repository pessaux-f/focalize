(*  Copyright 2004 INRIA  *)
(*  $Id: lexer.mll,v 1.10 2006-02-02 22:13:54 doligez Exp $  *)
{
open Token;;
}

let blank = [ ' ' '\009' '\010' '\013' ]
let idchar = [ '0'-'9' 'A'-'Z' 'a'-'z' '_' ]
let pathchar = [ '0'-'9' '_' ]
let digit = [ '0'-'9' ]

rule token = parse
  | "Require"
      { REQUIRE }
  | "\010%%begin-auto-proof"
      { BEGINAUTOPROOF }
  | "\010%%location:" blank* '[' ([^ ']']* as loc) ']'
      { LOCATION loc }
  | "\010%%name:" blank* (idchar+ as name)
      { NAME name }
  | "\010%%syntax:" blank* (idchar+ as syntax)
      { SYNTAX syntax }
  | "\010%%statement" ([^'.']+ as statement) '.'
      { STATEMENT statement }
  | "\010%%end-auto-proof"
      { ENDAUTOPROOF }
  | eof
      { EOF }
  | _
      { CHAR (Lexing.lexeme lexbuf).[0] }

and section = parse
  | "Section" blank+ (idchar* as species) "__" (idchar* as proof)
      { (species, proof) }

and lemma = parse
  | "Local" blank+ "__lemma_" { LEMMA (lemma_path lexbuf) }
  | "Local" blank+ "__goal_" { GOAL }
  | "Local" blank+ "__l_" { TOP }

and lemma_path = parse
  | "_" { lemma_path lexbuf }
  | digit+
      { let h = int_of_string (Lexing.lexeme lexbuf) in
        let t = lemma_path lexbuf in
        h :: t
      }
  | _ { [] }
