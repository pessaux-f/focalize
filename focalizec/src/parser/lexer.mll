(* $Id: lexer.mll,v 1.27 2007-11-01 18:20:34 weis Exp $ *)

{
open Lexing;;
open Parser;;

type error =
   | Illegal_character of char
   | Illegal_escape of string
   | Unterminated_comment
   | Uninitiated_comment
   | Unterminated_string
   | Unterminated_documentation
   | Comment_in_string
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
  "local", LOCAL;
  "logical", LOGICAL;
  "match", MATCH;
  "not", NOT;
  "notation", NOTATION;
  "of", OF;
  "open", OPEN;
  "or", OR;
  "proof", PROOF;
  "prop", PROP;
  "property", PROPERTY;
  "prove", PROVE;
  "qed", QED;
  "rec", REC;
  "rep", REP;
  "sig", SIG;
  "species", SPECIES;
  "step", STEP;
  "then", THEN;
  "theorem", THEOREM;
  "true", BOOL "true";
  "type", TYPE;
  "use", USE;
  "value", VALUE;
  "with", WITH;
]
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

let documentation_start_pos = ref None;;
let string_start_pos = ref None;;
let comment_start_pos = ref [];;
let in_comment () = !comment_start_pos <> [];;

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

let mk_external_code s =
  EXTERNAL_CODE (String.sub s 2 (String.length s - 4))
;;

(* The prefix version of a prefix operator. *)
let ident_of_prefixop s = PIDENT s;;
(* The prefix version of an infix operator. *)
let ident_of_infixop s = IIDENT s;;

let mk_prefixop s =
  assert (String.length s > 0);
  match s.[0] with
  | '`' -> BACKQUOTE_OP s
  | '~' -> TILDA_OP s
  | '?' -> QUESTION_OP s
  | '$' -> DOLLAR_OP s
  | '!' ->
    begin match String.length s with
    | 1 -> BANG
    | _ -> BANG_OP s end
  | '#' ->
    begin match String.length s with
    | 1 -> SHARP
    | _ -> SHARP_OP s end
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
    | _ -> DASH_GT_OP s end
  | '*' ->
    begin match String.length s with
    | 1 -> STAR_OP s
    | _ when s.[1] <> '*' -> STAR_OP s
    | _ -> STAR_STAR_OP s end
  | '/' -> SLASH_OP s
  | '%' -> PERCENT_OP s
  | '&' -> AMPER_OP s
  | '|' ->
    begin match String.length s with
    | 1 -> BAR
    | _ -> BAR_OP s end
  | ',' ->
    begin match String.length s with
    | 1 -> COMMA
    | _ -> COMMA_OP s end
  | ':' ->
    begin match String.length s with
    | 1 -> COLON
    | _ when s.[1] <> ':' -> COLON_OP s
    | 2 -> COLON_COLON
    | _ -> COLON_COLON_OP s end
  | ';' ->
    begin match String.length s with
    | 1 -> SEMI
    | _ when s.[1] <> ';' -> SEMI_OP s
    | 2 -> SEMI_SEMI
    | _ -> SEMI_SEMI_OP s end
  | '<' ->
    begin match String.length s with
    | 1 -> LT_OP s
    | _ when s.[1] <> '-' -> LT_OP s
    | 2 -> LT_DASH_OP s
    | _ when s.[2] <> '>' -> LT_DASH_OP s
    | 3 -> LT_DASH_GT
    | _ -> LT_DASH_GT_OP s end
  | '=' ->
    begin match String.length s with
    | 1 -> EQUAL
    | _ -> EQ_OP s end
  | '>' -> GT_OP s
  | '@' -> AT_OP s
  | '^' -> HAT_OP s
  | '\\' -> BACKSLASH_OP s
  | _ -> assert false
;;

open Format;;

let string_of_lex_error = function
  | Illegal_character c -> "Illegal character (" ^ (Char.escaped c) ^ ")"
  | Illegal_escape s ->
      "Illegal backslash escape in string or character (" ^ s ^ ")"
  | Unterminated_comment -> "Comment not terminated"
  | Comment_in_string -> "Non escaped comment separator in string constant"
  | Uninitiated_comment -> "Comment has not started"
  | Unterminated_string -> "String literal not terminated"
  | Unterminated_documentation -> "Documentation not terminated"
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

let start_infix_ident = '_'* (',' | infix_char)

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

(** Delimited identifiers. *)

let delimited_lowercase_ident =
  '`' '`' start_lowercase_ident [^'\'']* '\'' '\''

let delimited_uppercase_ident =
  '`' '`' start_uppercase_ident [^'\'']* '\'' '\''

let delimited_infix_ident =
  '`' '`' start_infix_ident [^'\'']* '\'' '\''

let delimited_prefix_ident =
  '`' '`' start_prefix_ident [^'\'']* '\'' '\''

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

(** Integers. *)
let decimal_literal =
  [ '0'-'9'] ['0'-'9' '_' ]*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f' '_']+
let bin_literal =
  '0' ['b' 'B'] ['0'-'1' '_']+
let int_literal =
  decimal_literal | hex_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
  | newline
    { update_loc lexbuf None 1 false 0;
      token lexbuf }
  | blank +
    { token lexbuf }
  | "Self"
    { SELF }
  | lowercase_ident
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> LIDENT s }
  | uppercase_ident
    { UIDENT (Lexing.lexeme lexbuf) }
  | "\'" lowercase_ident
    { QIDENT (Lexing.lexeme lexbuf) }
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
  | "(*"
    { comment_start_pos := [ lexbuf.lex_start_p, lexbuf.lex_curr_p ];
      comment lexbuf;
      token lexbuf }
  | "*)"
    { raise
        (Error (Uninitiated_comment,
                lexbuf.lex_start_p,
                lexbuf.lex_curr_p)) }
  | "--" [^ '\010' '\013'] * newline
    { update_loc lexbuf None 1 false 0;
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
  | '[' { LBRACKET }
  | ']' { RBRACKET }
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

  | "{*" ([^ '*'] | '*' [^ '}'])* "*}"
    { mk_external_code (Lexing.lexeme lexbuf) }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character
              (Lexing.lexeme_char lexbuf 0),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p)) }

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
  | '\\' ['(' '\\' '\'' '"' 'n' 't' 'b' 'r' ' ' '*' ')']
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
  | ( "(*" | "*)" )
    { raise (Error (Comment_in_string, lexbuf.lex_start_p, lexbuf.lex_curr_p)) }
  | ( newline | eof )
    { match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_string_char (Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and documentation = parse
  | ( "*)" )
    { () }
  | newline
    { update_loc lexbuf None 1 false 0;
      documentation lexbuf }
  | ( eof )
    { match !documentation_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_documentation, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_documentation_char (Lexing.lexeme_char lexbuf 0);
      documentation lexbuf }
