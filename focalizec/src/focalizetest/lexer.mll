{
open Own_parser;;
exception Lex_error;;

let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    } 
;;

}

let blank = [' ' '\t']
let cr = ['\n']
let lident = ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*
let uident = ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
let iident = [':''/''%''&''|''<''=''>''@''^''\\''+''-''*']+
let pident = ['!''~''?''$''[''{'']''}']+
let in_string  = (['a'-'z''A'-'Z''0'-'9''*''_'':''\\''-''>'' ''<''?''=''.''/''('')''['']'',']|'\\'['"''n'])*

(* We don't allow \n when lexing a string *)
rule lexe_string = parse
     [' ' '\t' ]   { lexe_string lexbuf }
      | "#"  { DIESE }
      | "!"  { BANG }
      | "("  { LPAREN }
      | ")"  { RPAREN }
      | ['"']in_string['"']  {let l = Lexing.lexeme lexbuf in
                          STRING((String.sub l 1 (String.length l - 2))) }
      | ","  { COMMA }
      | "fun" { FUN }
      | "->" { ARROW }
      | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
             { IDENT(Lexing.lexeme lexbuf) }
      | ['0'-'9']['0'-'9']*
             { INT(int_of_string (Lexing.lexeme lexbuf)) }
      | eof  { EOF }
      | '\n' { prerr_string "Error, \\n is not allowed in term.\n"; exit 10; }
      | _    { LEXERROR(Lexing.lexeme lexbuf) }
and lexe_file = parse
     [' ' '\t' '\n' ]   { lexe_file lexbuf }
      | "#"  { DIESE }
      | "!"  { BANG }
      | "("  { LPAREN }
      | ")"  { RPAREN }
      | ['"']in_string['"']  {let l = Lexing.lexeme lexbuf in
                          STRING((String.sub l 1 (String.length l - 2))) }
      | ","  { COMMA }
      | "fun" { FUN }
      | "->" { ARROW }
      | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
             { IDENT(Lexing.lexeme lexbuf) }
      | ['0'-'9']['0'-'9']*
             { INT(int_of_string (Lexing.lexeme lexbuf)) }
      | eof  { EOF }
      | _    { LEXERROR(Lexing.lexeme lexbuf) }
and lexe_focal = parse
    [' ' '\t']	{ lexe_focal lexbuf }
  | ['\n'] 	{ incr_linenum lexbuf ; lexe_focal lexbuf }
  | "let" { LET }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "rec" { REC }
  | "match" { MATCH }
  | "with" { WITH }
  | "fun" { FUN }
  | "type" { TYPE }
  | "caml" { CAML }
  | "import" { IMPORT }
  | "->" { ARROW }
  | "=" { EQ }
  | "as" { AS }
  | "|" { PIPE }
  | "in" { IN }
  | "("	 { LPAREN }
  | ";"	 { SEMICOLON }
  | ","	 { COMMA }
  | ")"	 { RPAREN }
  | "!"	 { BANG }
  | "_"	 { UNDERSCORE }
  | "*"  { STAR }
  | "#"  { DIESE }
  | "@"  { ARO }
  | ['"']in_string['"']  {let l = Lexing.lexeme lexbuf in
                          STRING((String.sub l 1 (String.length l - 2))) }
  | "'"['a'-'z''A'-'Z''_'] ['a'-'z''A'-'Z''0'-'9''_']* { VAR(Lexing.lexeme lexbuf) }
  | ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
      	       	       	{ UIDENT(Lexing.lexeme lexbuf) }
  | "'"iident"'" {let l = Lexing.lexeme lexbuf in
                  IIDENT(String.sub l 1 (String.length l - 2)) }
  | pident {PIDENT(Lexing.lexeme lexbuf) }
  | ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
      	       	       	{ IDENT(Lexing.lexeme lexbuf) }
  | ['0'-'9']*
      	       	       	{ INT(int_of_string(Lexing.lexeme lexbuf)) }
  | eof			{ EOF }
  | _    { LEXERROR(Lexing.lexeme lexbuf) }
and lexe_type = parse
  | "("	 { LPAREN }
  | ")"	 { RPAREN }
  | "->" { ARROW }
  | "@" { ARO }
  | ","	 { COMMA }
  | "*"  { STAR }
  | ['A'-'Z'] ['a'-'z''A'-'Z''0'-'9''_']*
      	       	       	{ UIDENT(Lexing.lexeme lexbuf) }
  | "'"['a'-'z''A'-'Z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
                        { VAR(Lexing.lexeme lexbuf) }
  | ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
      	       	       	{ IDENT(Lexing.lexeme lexbuf) }
  | _    { LEXERROR(Lexing.lexeme lexbuf) }
  | eof			{ EOF }
(*
and lexe_xml = parse
    [' ' '\t']	{ lexe_xml lexbuf }
  | ['\n'] 	{ incr_linenum lexbuf ; lexe_xml lexbuf }
  | "<"  { LESS }
  | "?" { INTER }
  | ">"  { GREATER }
  | "/"  { SLASH }
  | ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*
      	       	       	{ IDENT(Lexing.lexeme lexbuf) }
  | ['a'-'z''A'-'Z''0'-'9''_''('')'':''#''!'',']
    ['a'-'z''A'-'Z''0'-'9''_''('')'':''#'' ''!'',''\n']*
      	       	       	{ XML_IDENT(Lexing.lexeme lexbuf) }
  | _    {LEXERROR(Lexing.lexeme lexbuf) }
*)
(*
   The XML lexer
*)
and tmp_lexe_xml = parse
    blank  	{tmp_lexe_xml lexbuf }
  | cr 	    {incr_linenum lexbuf ; tmp_lexe_xml lexbuf }
  | "<?"    {tmp_lexe_xml_header lexbuf}
  | '<'     {tmp_lexe_xml_in_tag lexbuf }
  | '>'     {tmp_lexe_xml_between_tag lexbuf}
  | _       {LEXERROR(Lexing.lexeme lexbuf) }
and tmp_lexe_xml_between_tag = parse
  | (blank|cr)*'<' {tmp_lexe_xml_in_tag lexbuf }
  | cr 	{ incr_linenum lexbuf ; tmp_lexe_xml_between_tag lexbuf }
  | [^'<']*[' ''\t']*[^'<']* as ident {XML_IDENT(ident) }  
  | _    {LEXERROR(Lexing.lexeme lexbuf) }
and tmp_lexe_xml_in_tag = parse
  | (blank|cr)*'<' {tmp_lexe_xml_in_tag lexbuf }
  | cr 	{ incr_linenum lexbuf ; tmp_lexe_xml_in_tag lexbuf }
  | ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']* {BTAG_IDENT(Lexing.lexeme lexbuf) }
  | '/'(['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']* as ident) {ETAG_IDENT(ident) }
  | (['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*  as ident)blank*'/' {BETAG_IDENT(ident) }
  | _    {LEXERROR(Lexing.lexeme lexbuf) }
and tmp_lexe_xml_header = parse
  | (_* as ident)"?>" { XML_HEADER(ident)}
and lexe_test_context = parse
    [' ' '\t' '\n' ]   { lexe_test_context lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "=" { EQ }
  | "(" { LPAREN }
  | ['0'-'9']['0'-'9']*
             { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | ")" { RPAREN }
  | "!"  { BANG }
  | "#"  { DIESE }
  | "," { COMMA }
  | lident {IDENT (Lexing.lexeme lexbuf) }
  | uident {UIDENT (Lexing.lexeme lexbuf) }
  | eof  { EOF }
  | _    {LEXERROR(Lexing.lexeme lexbuf) }

