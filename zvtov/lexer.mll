(*  Copyright 2004 INRIA  *)
(*  $Id: lexer.mll,v 1.2 2004-05-23 19:53:09 doligez Exp $  *)
{
open Token;;
}

let blank = [ ' ' '\t' '\r' '\n' ]
let idchar = [ '0'-'9' 'A'-'Z' 'a'-'z' '_' ]
let pathchar = [ '0'-'9' '_' ]
let digit = [ '0'-'9' ]

rule token = parse
  | "Section" blank+ idchar* "__" idchar*
      { SECTION (Lexing.lexeme lexbuf) }
  | "Local" blank+ "__lemma_" pathchar+
      { LEMMA (Lexing.lexeme lexbuf) }
  | "Local" blank+ "__goal_" pathchar+
      { GOAL (Lexing.lexeme lexbuf) }
  | "(* to be proved *)" [^ '.']* "(* Qed *)"
      { TOBE (Lexing.lexeme lexbuf) }
  | eof
      { EOF }
  | _
      { CHAR (Lexing.lexeme lexbuf) }

and section = parse
  | "Section" blank+ (idchar* as species) "__" (idchar* as proof)
      { (species, proof) }

and lemma = parse
  | "Local" blank+ "__lemma_" { lemma_path lexbuf }

and lemma_path = parse
  | "_" { lemma_path lexbuf }
  | digit+
      { let h = int_of_string (Lexing.lexeme lexbuf) in
        let t = lemma_path lexbuf in
        h :: t
      }
  | eof { [] }
