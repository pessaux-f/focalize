(* $Id: lexer.mll,v 1.36 2008-04-15 21:13:41 weis Exp $ *)

{
open Lexing;;
open Parser;;

type error =
   | Comment_in_string
   | Comment_in_uniline_comment
   | Comment_in_delimited_ident
   | Delimited_ident_in_string
   | Delimited_ident_in_delimited_ident
   | External_code_in_string
   | External_code_in_delimited_ident
   | Illegal_character of char
   | Illegal_escape of string
   | Uninitiated_comment
   | Uninitiated_delimited_ident
   | Uninitiated_external_code
   | Unterminated_comment
   | Unterminated_documentation
   | Unterminated_delimited_ident
   | Unterminated_external_code
   | Unterminated_string
;;

exception Error of error * Lexing.position * Lexing.position;;

let keyword_table = Hashtbl.create 42;;

List.iter
 (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) [
  "alias", ALIAS;
  "all", ALL;
  "and", AND;
  "as", AS;
  "assume", ASSUME;
  "assumed", ASSUMED;
  "begin", BEGIN;
  "by", BY;
  "caml", CAML;
  "collection", COLLECTION;
  "coq", COQ;
  "coq_require", COQ_REQUIRE;
  "definition", DEFINITION;
  "else", ELSE;
  "end", END;
  "ex", EX;
  "external", EXTERNAL;
  "false", BOOL "false";
  "function", FUNCTION;
  "hypothesis", HYPOTHESIS;
  "if", IF;
  "in", IN;
  "inherits", INHERITS;
  "internal", INTERNAL;
  "implements", IMPLEMENTS;
  "is", IS;
  "let", LET;
  "lexicographic", LEXICOGRAPHIC;
  "local", LOCAL;
  "logical", LOGICAL;
  "match", MATCH;
  "measure", MEASURE;
  "not", NOT;
  "notation", NOTATION;
  "of", OF;
  "open", OPEN;
  "on", ON;
  "or", OR;
  "order", ORDER;
  "proof", PROOF;
  "prop", PROP;
  "property", PROPERTY;
  "prove", PROVE;
  "qed", QED;
  "rec", REC;
  "rep", REP;
  "Self", SELF;
  "signature", SIGNATURE;
  "species", SPECIES;
  "step", STEP;
  "structural", STRUCTURAL;
  "termination", TERMINATION;
  "then", THEN;
  "theorem", THEOREM;
  "true", BOOL "true";
  "type", TYPE;
  "use", USE;
  "value", VALUE;
  "with", WITH;
]
;;

let lident lexbuf =
  let s = Lexing.lexeme lexbuf in
  try Hashtbl.find keyword_table s with
  | Not_found -> LIDENT s
;;

let uident lexbuf =
  let s = Lexing.lexeme lexbuf in
  try Hashtbl.find keyword_table s with
  | Not_found -> UIDENT s
;;

(* Lexing the external_code tokens. *)
let initial_external_code_buffer = String.create 256;;
let external_code_buff = ref initial_external_code_buffer
and external_code_index = ref 0
;;

let reset_external_code_buffer () =
  external_code_buff := initial_external_code_buffer;
  external_code_index := 0
;;

let store_external_code_char c =
  if !external_code_index >= String.length (!external_code_buff) then begin
    let new_buff = String.create (String.length (!external_code_buff) * 2) in
    String.blit (!external_code_buff) 0
                new_buff 0 (String.length (!external_code_buff));
    external_code_buff := new_buff
  end;
  String.unsafe_set (!external_code_buff) (!external_code_index) c;
  incr external_code_index
;;

let get_stored_external_code () =
  let s = String.sub (!external_code_buff) 0 (!external_code_index) in
  external_code_buff := initial_external_code_buffer;
  s
;;

(* Lexing the documentation tokens. *)
let initial_documentation_buffer = String.create 256;;
let documentation_buff = ref initial_documentation_buffer
and documentation_index = ref 0
;;

let reset_documentation_buffer () =
  documentation_buff := initial_documentation_buffer;
  documentation_index := 0
;;

let store_documentation_char c =
  if !documentation_index >= String.length (!documentation_buff) then begin
    let new_buff = String.create (String.length (!documentation_buff) * 2) in
    String.blit (!documentation_buff) 0
                new_buff 0 (String.length (!documentation_buff));
    documentation_buff := new_buff
  end;
  String.unsafe_set (!documentation_buff) (!documentation_index) c;
  incr documentation_index
;;

let get_stored_documentation () =
  let s = String.sub (!documentation_buff) 0 (!documentation_index) in
  documentation_buff := initial_documentation_buffer;
  s
;;

(* Lexing the string tokens. *)
let initial_string_buffer = String.create 256;;
let string_buff = ref initial_string_buffer
and string_index = ref 0
;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
;;

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0
                  new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index
;;

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s
;;

(* Lexing the delimited_ident tokens. *)
let initial_delimited_ident_buffer = String.create 256;;
let delimited_ident_buff = ref initial_delimited_ident_buffer
and delimited_ident_index = ref 0
;;

let reset_delimited_ident_buffer () =
  delimited_ident_buff := initial_delimited_ident_buffer;
  delimited_ident_index := 0
;;

let store_delimited_ident_char c =
  if !delimited_ident_index >= String.length (!delimited_ident_buff) then begin
    let new_buff = String.create (String.length (!delimited_ident_buff) * 2) in
    String.blit (!delimited_ident_buff) 0
                new_buff 0 (String.length (!delimited_ident_buff));
    delimited_ident_buff := new_buff
  end;
  String.unsafe_set (!delimited_ident_buff) (!delimited_ident_index) c;
  incr delimited_ident_index
;;

let get_stored_delimited_ident () =
  let s = String.sub (!delimited_ident_buff) 0 (!delimited_ident_index) in
  delimited_ident_buff := initial_external_code_buffer;
  s
;;

let external_code_start_pos = ref None;;
let documentation_start_pos = ref None;;
let string_start_pos = ref None;;
let delimited_ident_start_pos = ref None;;
let comment_start_pos = ref [];;

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c
;;

let char_for_decimal_code lexbuf i =
  let c =
    100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
     10 * (Char.code(Lexing.lexeme_char lexbuf (i + 1)) - 48) +
          (Char.code(Lexing.lexeme_char lexbuf (i + 2)) - 48) in
  if c >= 0 && c <= 255 then Char.chr c else
    raise
      (Error (Illegal_escape (Lexing.lexeme lexbuf),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p))
;;

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 =
    if d1 >= 97 then d1 - 87 else
    if d1 >= 65 then d1 - 55 else
    d1 - 48 in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 =
    if d2 >= 97 then d2 - 87 else
    if d2 >= 65 then d2 - 55 else
    d2 - 48 in
  Char.chr (val1 * 16 + val2)
;;

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file =
    match file with
    | None -> pos.pos_fname
    | Some s -> s in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(* The prefix version of a prefix operator. *)
let ident_of_prefixop s = PIDENT s;;
(* The prefix version of an infix operator. *)
let ident_of_infixop s = IIDENT s;;

let mk_prefixop s =
  assert (String.length s > 0);
  match s.[0] with
  | '`' -> BACKQUOTE_OP s
  | '~' -> if String.length s = 1 then TILDA else TILDA_OP s
  | '?' -> QUESTION_OP s
  | '$' -> DOLLAR_OP s
  | '!' ->
    begin match String.length s with
    | 1 -> BANG
    | _ -> BANG_OP s
    end
  | '#' ->
    begin match String.length s with
    | 1 -> SHARP
    | _ -> SHARP_OP s
    end
  | _ -> assert false
;;

let mk_infixop s =
  assert (String.length s > 0);
  match s.[0] with
  | '+' -> PLUS_OP s
  | '-' ->
    begin match String.length s with
    | 1 -> DASH_OP s
    | _ when s.[1] <> '>' -> DASH_OP s
    | 2 -> DASH_GT
    | _ -> DASH_GT_OP s
    end
  | '*' ->
    begin match String.length s with
    | 1 -> STAR_OP s
    | _ when s.[1] <> '*' -> STAR_OP s
    | _ -> STAR_STAR_OP s
    end
  | '/' ->
    begin match String.length s with
    | 1 -> SLASH_OP s
    | 2 when s.[1] = '\\' -> CONJUNCTION
    | _ -> SLASH_OP s
    end
  | '%' -> PERCENT_OP s
  | '&' -> AMPER_OP s
  | '|' ->
    begin match String.length s with
    | 1 -> BAR
    | _ -> BAR_OP s
    end
  | ',' ->
    begin match String.length s with
    | 1 -> COMMA
    | _ -> COMMA_OP s
    end
  | ':' ->
    begin match String.length s with
    | 1 -> COLON
    | _ when s.[1] <> ':' -> COLON_OP s
    | 2 -> COLON_COLON
    | _ -> COLON_COLON_OP s
    end
  | ';' ->
    begin match String.length s with
    | 1 -> SEMI
    | _ when s.[1] <> ';' -> SEMI_OP s
    | 2 -> SEMI_SEMI
    | _ -> SEMI_SEMI_OP s
    end
  | '<' ->
    begin match String.length s with
    | 1 -> LT_OP s
    | _ when s.[1] <> '-' -> LT_OP s
    | 2 -> LT_DASH_OP s
    | _ when s.[2] <> '>' -> LT_DASH_OP s
    | 3 -> LT_DASH_GT
    | _ -> LT_DASH_GT_OP s
    end
  | '=' ->
    begin match String.length s with
    | 1 -> EQUAL
    | _ -> EQ_OP s
    end
  | '>' -> GT_OP s
  | '@' -> AT_OP s
  | '^' -> HAT_OP s
  | '\\' ->
    begin match String.length s with
    | 1 -> BACKSLASH_OP s
    | _ when s.[1] = '/' -> DISJUNCTION
    | _ -> BACKSLASH_OP s
    end
  | _ -> assert false
;;

open Format;;

let string_of_lex_error = function
  | Comment_in_string ->
      "Non escaped comment separator in string constant"
  | Comment_in_uniline_comment ->
      "Non escaped comment separator in uniline comment"
  | Comment_in_delimited_ident ->
      "Non escaped comment separator in delimited ident"
  | Delimited_ident_in_string ->
      "Non escaped delimited ident separator in string constant"
  | Delimited_ident_in_delimited_ident ->
      "Non escaped delimited ident separator in delimited ident"
  | External_code_in_string ->
      "Non escaped external code separator in string constant"
  | External_code_in_delimited_ident ->
      "Non escaped external code separator in delimited ident"
  | Illegal_character c ->
      "Illegal character (" ^ (Char.escaped c) ^ ")"
  | Illegal_escape s ->
      "Illegal backslash escape in string or character (" ^ s ^ ")"
  | Uninitiated_comment ->
      "Comment has not started"
  | Uninitiated_delimited_ident ->
      "Delimited ident has not started"
  | Uninitiated_external_code ->
      "External code has not started"
  | Unterminated_comment ->
      "Comment not terminated"
  | Unterminated_documentation ->
      "Documentation not terminated"
  | Unterminated_external_code ->
      "External code not terminated"
  | Unterminated_delimited_ident ->
      "Delimited ident not terminated"
  | Unterminated_string ->
      "String literal not terminated"
;;

}

let newline = '\010'
let blank = [ ' ' '\009' '\012' ]

(** Identifiers can be:
   - alphanumerical,
   - infix,
   - prefix.

   Neither Quote nor DoubleQuote appear inside identifiers: those are
   respectively character and string delimitors.

  (0) Characters inside alphanumerical identifiers.
    Alphanumerical identifiers can be:
    - regular words starting with a lower case letter,
    - ``family'' names starting with an upper case letter.

  (1) Characters inside infix identifiers:
    infix binary identifiers, such as +, -, *.

   Rq: End_Infix ::= SPACE  (::= blanc tab newline) ( ) [] {}

  (2) Characters inside prefix identifiers:
   prefix unary identifiers, such as ~| (boolean not), -.

   Rq: ! and # and . are treated specially and cannot be inside idents.
   Rq: $ should be inside idents ? Convenient to get the traditional $1, $2 as
   idents.

   Rq:
   - ',' cannot be inside infixes or prefixes (due to proof labels that are
         almost parsable as infixes!)
   - '_,' cannot be the beginning of an infix, since we want to parse the
         pattern _, _ as 3 tokens '_' ',' and '_'.
   - '.' cannot be inside infixes or prefixes, since we want to parse
         LIDENT DOT LIDENT
         which, if '.' were in infixes characters, would be parsed as LIDENT
         followed by the infix DOT LIDENT.
         (For instance, r.label would be the two tokens
          LIDENT "r" and DOT_OP ".label"). *)

let lowercase_char = [ 'a'-'z' ]
let uppercase_char = [ 'A'-'Z' ]
let decimal_char = [ '0'-'9' ]

let inside_ident =
    lowercase_char
  | uppercase_char
  | decimal_char

let infix_char =
  [ '+' '-' '*' '/' '%' '&' '|' ':' ';' '<' '=' '>' '@' '^' '\\' ]
let prefix_char =
  [ '`' '~' '?' '$' '!' '#' ]
let fix_char =
    infix_char
  | prefix_char

(** Identifier classes starter characters. *)

let start_lowercase_ident =
    '_'* lowercase_char
  | '_'+ decimal_char

let start_uppercase_ident = '_'* uppercase_char

let start_infix_ident =
    ','
  | '_'* infix_char

let start_prefix_ident = '_'* prefix_char

(** Identifier classes continuing characters. *)

let continue_ident =
    '_'
  | inside_ident

let continue_prefix_ident =
    '_'
  | fix_char

let continue_infix_ident =
    '_'
  | fix_char
  | inside_ident

(** Identifier class definitions.
  - regular identifiers, variable names and module names,
  - infix_ident identifiers,
  - prefix identifiers.

  Note : the first rule for lowercase identifiers
          '_'* ( lowercase | decimal )
  gives us _1 as ident (as well as _[0-9]+)
  _identifier_ is also an ident since '_' is in continue_ident

  and _ is a special case of identifier to produce the token UNDERSCORE.

  In a _U_ b the token _U_ is not an infix but an uppercase ident. *)

(** Identifiers *)

let regular_lowercase_ident = start_lowercase_ident continue_ident*
let regular_uppercase_ident = start_uppercase_ident continue_ident*
let regular_infix_ident = start_infix_ident continue_infix_ident*
let regular_prefix_ident = start_prefix_ident continue_prefix_ident*

(** Integers. *)
let decimal_literal =
  [ '0'-'9'] ['0'-'9' '_' ]*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f' '_']+
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1' '_']+
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

(** Delimited identifiers. *)

let delimited_lowercase_ident =
  '`' '`' start_lowercase_ident [^'\'' '\n']* '\'' '\''

let delimited_uppercase_ident =
  '`' '`' start_uppercase_ident [^'\'' '\n']* '\'' '\''

let delimited_infix_ident =
  '`' '`' start_infix_ident [^'\'' '\n']* '\'' '\''

let delimited_prefix_ident =
  '`' '`' start_prefix_ident [^'\'' '\n']* '\'' '\''

let lowercase_ident =
    regular_lowercase_ident
  | delimited_lowercase_ident

let uppercase_ident =
    regular_uppercase_ident
  | delimited_uppercase_ident

let infix_ident =
    regular_infix_ident
  | delimited_infix_ident

let prefix_ident =
    regular_prefix_ident
  | delimited_prefix_ident

rule token = parse
  | newline
    { update_loc lexbuf None 1 false 0;
      token lexbuf }
  | blank +
    { token lexbuf }
  | lowercase_ident
    { lident lexbuf }
  | uppercase_ident
    { uident lexbuf }
  | "\'" lowercase_ident
    { QIDENT (Lexing.lexeme lexbuf) }
  | "``" (start_lowercase_ident | start_uppercase_ident |
          start_infix_ident | start_prefix_ident)
    { reset_delimited_ident_buffer ();
      store_delimited_ident_char (Lexing.lexeme_char lexbuf 2);
      delimited_ident_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      delimited_ident lexbuf;
      begin match !delimited_ident_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      LIDENT (get_stored_delimited_ident ()) }
  | "''"
    { raise
        (Error (Uninitiated_delimited_ident,
                lexbuf.lex_start_p,
                lexbuf.lex_curr_p)) }

  | int_literal
    { INT (Lexing.lexeme lexbuf) }
  | float_literal
    { FLOAT (Lexing.lexeme lexbuf) }
  | "\""
    { reset_string_buffer ();
      string_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      string lexbuf;
      begin match !string_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      STRING (get_stored_string ()) }
  | "'" [^ '\\' '\'' '\010'] "'"
    { CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
    { CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
    { CHAR (char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
    { CHAR (char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
    { let l = Lexing.lexeme lexbuf in
      let esc = String.sub l 1 (String.length l - 1) in
      raise (Error
              (Illegal_escape esc, lexbuf.lex_start_p, lexbuf.lex_curr_p)) }
  | "(**"
    { reset_documentation_buffer ();
      documentation_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      documentation lexbuf;
      begin match !documentation_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      DOCUMENTATION (get_stored_documentation ()) }
  | "{*"
    { reset_external_code_buffer ();
      external_code_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      external_code lexbuf;
      begin match !external_code_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      EXTERNAL_CODE (get_stored_external_code ()) }
  | "*}"
    { raise
        (Error (Uninitiated_external_code,
                lexbuf.lex_start_p,
                lexbuf.lex_curr_p)) }
  | "(*"
    { comment_start_pos := [ lexbuf.lex_start_p, lexbuf.lex_curr_p ];
      comment lexbuf;
      token lexbuf }
  | "*)"
    { raise
        (Error (Uninitiated_comment,
                lexbuf.lex_start_p,
                lexbuf.lex_curr_p)) }
  | "--"
    { uniline_comment lexbuf;
      token lexbuf }

  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
    { update_loc lexbuf name (int_of_string num) true 0;
      token lexbuf }
  | '<' (['0'-'9']+ as level) '>' (['A'-'Z' 'a'-'z' '0'-'9']+ as label)
    { PROOF_LABEL (level, label) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "()" { LRPARENS }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | "[]" { LRBRACKETS }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '.' { DOT }
  | '_' { UNDERSCORE }

  | prefix_ident
    { mk_prefixop (Lexing.lexeme lexbuf) }
  | "(" [' ']+ (prefix_ident as inner) [' ']+ ")"
    { ident_of_prefixop inner }

  | infix_ident
    { mk_infixop (Lexing.lexeme lexbuf) }
  | "(" [' ']+ (infix_ident as inner) [' ']+ ")"
    { ident_of_infixop inner }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character
              (Lexing.lexeme_char lexbuf 0),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p)) }

and delimited_ident = parse
  | "''"
    { () }
  | '\\' newline ([' ' '\t'] * as space)
    { update_loc lexbuf None 1 false (String.length space);
      delimited_ident lexbuf }
  | '\\' ['(' '-' '\\' '`' '\'' '"' 'n' 't' 'b' 'r' ' ' '*' ')']
    { store_delimited_ident_char (char_for_backslash (Lexing.lexeme_char lexbuf 1));
      delimited_ident lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_delimited_ident_char(char_for_decimal_code lexbuf 1);
      delimited_ident lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_delimited_ident_char (char_for_hexadecimal_code lexbuf 2);
      delimited_ident lexbuf }
  | '\\' _
    { raise
        (Error
           (Illegal_escape (Lexing.lexeme lexbuf),
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | ( "(*" | "*)" "--" )
    { raise
        (Error
           (Comment_in_delimited_ident,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( "{*" | "*}" )
    { raise
        (Error
           (External_code_in_delimited_ident,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( "``" | "''" )
    { raise
        (Error
           (Delimited_ident_in_delimited_ident,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( newline | eof )
    { match !delimited_ident_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_delimited_ident, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_delimited_ident_char (Lexing.lexeme_char lexbuf 0);
      delimited_ident lexbuf }

and uniline_comment = parse
  | newline
    { update_loc lexbuf None 1 false 0; }
  | ( "(*" | "*)" )
    { raise
        (Error
          (Comment_in_uniline_comment,
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | eof
    { raise
        (Error
          (Unterminated_comment,
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | _
    { uniline_comment lexbuf }

and comment = parse
  | "(*"
    { comment_start_pos :=
        (lexbuf.lex_start_p, lexbuf.lex_curr_p) :: !comment_start_pos;
      comment lexbuf; }
  | "*)"
    { match !comment_start_pos with
      | [] -> assert false
      | [ _ ] -> comment_start_pos := [];
      | _ :: l -> comment_start_pos := l;
                  comment lexbuf; }
  | eof
    { match !comment_start_pos with
      | [] -> assert false
      | (start_pos, end_pos) :: _ ->
        comment_start_pos := [];
        raise (Error (Unterminated_comment, start_pos, end_pos)) }
  | "--" [^ '\010' '\013'] * newline
    { update_loc lexbuf None 1 false 0;
      comment lexbuf }
  | newline
    { update_loc lexbuf None 1 false 0;
      comment lexbuf }
  | _
    { comment lexbuf }

and string = parse
  | '"'
    { () }
  | '\\' newline ([' ' '\t'] * as space)
    { update_loc lexbuf None 1 false (String.length space);
      string lexbuf }
  | '\\' ['(' '-' '\\' '`' '\'' '"' 'n' 't' 'b' 'r' ' ' '*' ')']
    { store_string_char (char_for_backslash (Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_string_char(char_for_decimal_code lexbuf 1);
      string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
    { store_string_char (char_for_hexadecimal_code lexbuf 2);
      string lexbuf }
  | '\\' _
    { raise
        (Error
           (Illegal_escape (Lexing.lexeme lexbuf),
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | ( "(*" | "*)" "--" )
    { raise
        (Error
           (Comment_in_string,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( "{*" | "*}" )
    { raise
        (Error
           (External_code_in_string,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( "``" | "''" )
    { raise
        (Error
           (Delimited_ident_in_string,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | ( newline | eof )
    { match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_string_char (Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and documentation = parse
  | "*)"
    { () }
  | newline
    { update_loc lexbuf None 1 false 0;
      documentation lexbuf }
  | eof
    { match !documentation_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_documentation, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_documentation_char (Lexing.lexeme_char lexbuf 0);
      documentation lexbuf }

and external_code = parse
  | "*}"
    { () }
  | newline
    { update_loc lexbuf None 1 false 0;
      external_code lexbuf }
  | eof
    { match !external_code_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_external_code, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_external_code_char (Lexing.lexeme_char lexbuf 0);
      external_code lexbuf }
