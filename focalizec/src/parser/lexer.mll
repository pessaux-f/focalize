(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2006, 2008 LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexer.mll,v 1.66 2008-11-29 20:14:50 weis Exp $ *)

{
(** {3 The Focalize lexer} *)

open Lexing;;
open Parser;;

(** {6 Lexing errors} *)

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
(** The various errors when lexing. *)
;;

exception Error of error * Lexing.position * Lexing.position;;

(** {6 Explaining lexing errors} *)

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
      "Illegal character (" ^ Char.escaped c ^ ")"
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

(** {6 The keyword table} *)

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
  "evidence", EVIDENCE;
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
  "private", PRIVATE;
  "proof", PROOF;
  "prop", PROP;
  "property", PROPERTY;
  "prove", PROVE;
  "qed", QED;
  "rec", REC;
  "representation", REPRESENTATION;
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
  "with", WITH;
]
;;

(** {3 Tokens for symbols} *)

(** Finding the first meaningful character at the beginning of an ident or a
    symbol, getting rid of initial underscores. *)
let start_ident_char s =
  let lim = String.length s - 1 in
  let rec loop i c =
    if i > lim then c, i else
    let nc = s.[i] in
    match nc with
    | '_' -> loop (i + 1) nc
    | c ->
      (*prerr_endline (Printf.sprintf "start_ident_char %S is %C, %d" s c i);*)
      c, i in
  loop 0 '_'
;;

let token_of_lowercase_prefix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_lowercase_prefix_symbol %s" s);*)
  assert (String.length s > 0);
  let c, i = start_ident_char s in
  let length_s = String.length s in
  let length_meaningful = length_s - i in
  match c with
(*  | '`' (* ` Helping emacs *) ->
      BACKQUOTE_OP s*)
  | '~' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> NEGATION
    | _, _ -> TILDA_OP s
    end
  | '?' -> QUESTION_OP s
  | '$' -> DOLLAR_OP s
  | '!' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> BANG
    | _, _ -> BANG_OP s
    end
  | '#' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> SHARP
    | _, _ -> SHARP_OP s
    end
  | _ -> assert false
;;

(* To be revisited. Do we have to further discriminate ? *)
let token_of_uppercase_prefix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_uppercase_prefix_symbol %s" s);*)
  assert (String.length s > 0);
  PUIDENT s
;;

(* To be revisited. Do we have to further discriminate ? *)
let token_of_uppercase_infix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_uppercase_infix_symbol %s" s);*)
  assert (String.length s > 0);
  let c, i = start_ident_char s in
  let length_s = String.length s in
  let length_meaningful = length_s - i in
  match c with
  | ',' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> COMMA
    | _, _ -> COMMA_OP s
    end
  | ':' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> COLON
    | 2, 2 when s.[i + 1] = ':' -> COLON_COLON
    | n, _ when s.[n - 1] = ':' -> IUIDENT s
    | _, _ when s.[1] <> ':' -> COLON_OP s
    | _, _ -> COLON_COLON_OP s
    end
  | '`' -> (* A regular uppercase ident enclosed in ` chars. *)
    begin match length_s, length_meaningful with
    | 1, 1 -> BACKQUOTE_OP s
    | n, _ when s.[n - 1] = '`' -> IUIDENT s
(*FIXME    | _, _ when s.[1] <> '`' -> COLON_OP s*)
    | _, _ -> BACKQUOTE_OP s
    end
  | _ -> assert false
;;

let token_of_lowercase_infix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_lowercase_infix_symbol %s" s);*)
  assert (String.length s > 0);
  (* i is the index of character c, the first ``interesting'' char in s. *)
  let c, i = start_ident_char s in
  let length_s = String.length s in
  let length_meaningful = length_s - i in
  match c with
  | '*' ->
    begin match length_s, length_meaningful with
    | _, 1 -> STAR_OP s
    | _, _ when s.[i + 1] <> '*' -> STAR_OP s
    | _, _ -> STAR_STAR_OP s
    end
  | '+' -> PLUS_OP s
  | '-' ->
    begin match length_s, length_meaningful with
    | _, 1 -> DASH_OP s
    | _, _ when s.[i + 1] <> '>' -> DASH_OP s
    | 2, 2 -> DASH_GT
    | _, _ -> DASH_GT_OP s
    end
  | '/' ->
    begin match length_s, length_meaningful with
    | 2, 2 when s.[i + 1] = '\\' -> CONJUNCTION
    | _, _ -> SLASH_OP s
    end
  | '%' -> PERCENT_OP s
  | '&' -> AMPER_OP s
  | '|' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> BAR
    | _, _ -> BAR_OP s
    end
  | ';' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> SEMI
    | _, 1 -> SEMI_OP s
    | _, _ when s.[i + 1] <> ';' -> SEMI_OP s
    | 2, 2 -> SEMI_SEMI
    | _, _ -> SEMI_SEMI_OP s
    end
  | '<' ->
    begin match length_s, length_meaningful with
    | _, 1 -> LT_OP s
    | _, _ when s.[i + 1] <> '-' -> LT_OP s
    | _, 2 -> LT_DASH_OP s
    | _ when s.[2] <> '>' -> LT_DASH_OP s
    | 3, 3 -> LT_DASH_GT
    | _ -> LT_DASH_GT_OP s
    end
  | '=' ->
    begin match length_s, length_meaningful with
    | 1, 1 -> EQUAL
    | _, _ -> EQ_OP s
    end
  | '>' -> GT_OP s
  | '@' -> AT_OP s
  | '^' -> HAT_OP s
  | '\\' ->
    begin match length_s, length_meaningful with
    | 2, 2 when s.[i + 1] = '/' -> DISJUNCTION
    | _ -> BACKSLASH_OP s
    end
  | '`' -> (* A regular lowercase ident enclosed in ` chars. *)
    ILIDENT s
  | _ -> assert false
;;

(** {3 Identifier creation functions} *)

(** {6 Finding keywords and creating lowercase idents} *)
let token_of_lowercase_ident lexbuf =
  let s = Lexing.lexeme lexbuf in
(*  prerr_endline (Printf.sprintf "token_of_lowercase_ident %s" s);*)
  assert (String.length s > 0);
  try Hashtbl.find keyword_table s with
  | Not_found -> RLIDENT s
;;

(** {6 Finding keywords and creating uppercase idents} *)
let token_of_uppercase_ident lexbuf =
(*  prerr_endline (Printf.sprintf "token_of_uppercase_ident");*)
  let s = Lexing.lexeme lexbuf in
(*  prerr_endline (Printf.sprintf "token_of_uppercase_ident %s" s);*)
  assert (String.length s > 0);
  try Hashtbl.find keyword_table s with
  | Not_found -> RUIDENT s
;;

(** {6 Quoted idents} *)

let token_of_quoted_lowercase_ident lexbuf =
  let s = Lexing.lexeme lexbuf in
(*  prerr_endline
    (Printf.sprintf "token_of_quoted_lowercase_ident: %s" s); *)
  assert (String.length s > 0);
  QLIDENT s
;;

let token_of_quoted_uppercase_ident lexbuf =
  let s = Lexing.lexeme lexbuf in
(*  prerr_endline
    (Printf.sprintf "token_of_quoted_uppercase_ident: %s" s); *)
  assert (String.length s > 0);
  QUIDENT s
;;

(** {6 Creating tokens for delimited idents} *)

(** Could be any of RLIDENT, RUIDENT, PLIDENT, PUIDENT, ILIDENT, IUIDENT,
   according to the triggering character class.
   Note that a delimited identifier includes its delimitors.
 *)
let token_of_delimited_ident s =
(*  prerr_endline (Printf.sprintf "token_of_delimited_ident %s" s);*)
  assert (String.length s <> 0);
  let c, _ = start_ident_char s in
  let length_s = String.length s in
  match c with
  (* String s has only underscores. *)
  | '_' -> if length_s = 1 then UNDERSCORE else RLIDENT s
  (* start_lowercase_ident *)
  | 'a' .. 'z'
  | '0' .. '9' -> RLIDENT s
  (* start_uppercase_ident *)
  | 'A' .. 'Z' -> RUIDENT s
  (* start_lowercase_infix_symbol *)
  | '*'
  | '+' | '-'
  | '/' | '%' | '&' | '|' | '<' | '=' | '>' | '@' | '^' | '\\' ->
    token_of_lowercase_infix_symbol s
  (* start_lowercase_prefix_symbolic *)
  | '!' | '#'
  | '~' | '?' | '$'  (* ` Helping emacs. *) ->
    token_of_lowercase_prefix_symbol s
  (* start_uppercase_infix_symbol *)
  | ':' | '`' -> (* ` Helping emacs *)
    token_of_uppercase_infix_symbol s
  (* start_uppercase_prefix_symbol *)
  | '(' | '[' -> (* ]) Helping Emacs *)
    token_of_uppercase_prefix_symbol s
  | _ -> assert false
(** The first meaningful character at the beginning of a delimited
  ident/symbol is used to find its associated token.
*)
;;

(** {6 Tokens for parenthesized symbols} *)

(** The prefix version of symbolic identifiers is obtained by enclosing the
    symbol between parens. *)

let token_of_paren_lowercase_prefix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_paren_lowercase_prefix_symbol %s" s);*)
  assert (String.length s > 0);
  PLIDENT s
;;
(** The prefix version of a lowercase prefix operator. *)
let token_of_paren_lowercase_infix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_paren_lowercase_infix_symbol %s" s);*)
  assert (String.length s > 0);
  ILIDENT s
;;
(** The prefix version of a lowercase infix operator. *)

let token_of_paren_uppercase_prefix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_paren_uppercase_prefix_symbol %s" s);*)
  assert (String.length s > 0);
  PUIDENT s
;;
(** The prefix version of an uppercase prefix operator. *)
let token_of_paren_uppercase_infix_symbol s =
(*  prerr_endline (Printf.sprintf "token_of_paren_uppercase_infix_symbol %s" s);*)
  assert (String.length s > 0);
  IUIDENT s
;;
(** The prefix version of an uppercase infix operator. *)

(** {3 Various auxiliaries to lex special tokens} *)

(** {6 Lexing the external code tokens} *)
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

(** {6 Lexing the documentation tokens} *)
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

(** {6 Lexing the string tokens} *)
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

(** {6 Lexing the delimited identifier tokens} *)
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

(** {6 Decoding characters} *)

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

(** {6 Keeping the internal buffer locations up to date} *)

let update_loc lexbuf fname line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_fname =
    match fname with
    | None -> pos.pos_fname
    | Some s -> s in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = new_fname;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(** Un conditionally set the location as being on line [num],
    in file [fname]. *)
let set_loc lexbuf fname num =
  update_loc lexbuf fname (int_of_string num) true 0
;;

(** Add one to the current line counter of the file being lexed. *)
let incr_line_num lexbuf =
  update_loc lexbuf None 1 false 0
;;

(** Add one to the current line counter of the file being lexed,
    knowing that [spaces] chars are skipped at the begeinning of the next
    line. *)
let incr_escaped_line_num lexbuf spaces =
  update_loc lexbuf None 1 false (String.length spaces)
;;

}

(** {3 The main lexer} *)

(** {6 Classifying characters} *)
let newline = '\010'
(** ASCII 010 is newline or ['\n']. *)
let blank = [ '\032' '\009' '\012' ]
(** ASCII 32 is space, ASCII 9 is tab, ASCII 12 is CTRL-L *)
let whites = [ ' ' '\t' ]*
(** Any number of space and tabs (including 0). *)

(** {3 Numbers} *)

(** {6 Integers} *)

(** Integers can be given in binary, octal, decimal, or hexadecimal
    notation; they may have an optional sign. *)

(** {7 Classification of characters for integers} *)

let binary_digit = [ '0'-'1' ]
let octal_digit = [ '0'-'7' ]
let decimal_digit = [ '0'-'9' ]
let hexadecimal_digit = [ '0'-'9' 'A'-'F' 'a'-'f' ]
let sign = [ '+' '-' ]

(** {7 Definition of integer literals} *)

let unsigned_binary_literal =
  '0' [ 'b' 'B' ] binary_digit ( binary_digit | '_' )*
let unsigned_octal_literal =
  '0' [ 'o' 'O' ] octal_digit ( octal_digit | '_' )*
let unsigned_decimal_literal = decimal_digit ( decimal_digit | '_' )*
let unsigned_hexadecimal_literal =
  '0' [ 'x' 'X' ] hexadecimal_digit ( hexadecimal_digit | '_' )*

let unsigned_integer_literal =
    unsigned_binary_literal
  | unsigned_octal_literal
  | unsigned_decimal_literal
  | unsigned_hexadecimal_literal

let integer_literal = sign? unsigned_integer_literal

(** {6 Floating point numbers} *)

(** {7 Classification of characters for floating point numbers} *)

let decimal_literal = sign? unsigned_decimal_literal
let hexadecimal_literal = sign? unsigned_hexadecimal_literal

let scientific_notation = ['e' 'E']

(** {7 Definition of float literals} *)

let unsigned_decimal_float_literal =
  unsigned_decimal_literal
  ('.' unsigned_decimal_literal* )?
  (scientific_notation decimal_literal)?

let unsigned_hexadecimal_float_literal =
  unsigned_hexadecimal_literal
  ('.' unsigned_hexadecimal_literal* )?
  (scientific_notation hexadecimal_literal)?

let unsigned_float_literal =
    unsigned_decimal_float_literal
  | unsigned_hexadecimal_float_literal

let float_literal = sign? unsigned_float_literal

(** {3 Identifiers} *)

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
         RLIDENT DOT RLIDENT
         which, if '.' were in infixes characters, would be parsed as RLIDENT
         followed by the infix DOT RLIDENT.
         (For instance, r.label would be the two tokens
          RLIDENT "r" and DOT_OP ".label"). *)

(** {7 Classification of characters for identifiers} *)

let lowercase_alphabetic = [ 'a'-'z' ]
let uppercase_alphabetic = [ 'A'-'Z' ]

let inside_ident =
    lowercase_alphabetic
  | uppercase_alphabetic
  | decimal_digit
(** As expected, decimal digits are defined above for numbers, as:
  {[
     let decimal_digit = [ '0'-'9' ]
  ]}
*)

(** Characters that can safely be inside any symbol. *)
let symbolic =
  [ '/' '%' '&' '|' ';' '<' '=' '>' '@' '^' '\\' ]

(** Characters that can only start an uppercase prefix symbol. *)
let start_uppercase_prefix_symbolic = [ '[' '(' ] (* )] Helping emacs. *)
(** Characters that can only start an uppercase infix symbol. *)
let start_uppercase_infix_symbolic = [ ':' '`'] (* ` Helping emacs. *)

(** Characters that certainly start a lowercase prefix symbol. *)
let lowercase_prefix_symbolic = [ '~' '?' '$' ]
(* Andalso characters '!' and '#' as a special case. *)
(* To revisit:
  - we want lowercase prefix symbols to follow the same rules as lowercase
  infix ones (otherwise lexical rules are weird and way too difficult to grasp).
  - so we need to review the status of '!' and '#': those should obey to
  special rules for lexing, since
  * '!' is BANG
  * '!' start_lowercase_prefix_symbolic *)

let start_lowercase_prefix_symbolic =
    '!'
  | '#'
  | lowercase_prefix_symbolic

(** Characters that can start a lowercase infix symbol. *)
let start_lowercase_infix_symbolic =
    '*'
  | sign
  | symbolic

(** Symbolic characters that may safely appear into a prefix symbol.
    Inside a lowercase symbol, we can repeat any starter for lowercase symbols. *)
let inside_lowercase_prefix_symbolic =
    start_lowercase_infix_symbolic
  | start_lowercase_prefix_symbolic
(* | start_uppercase_infix_symbolic ??? *)

let inside_lowercase_infix_symbolic = inside_lowercase_prefix_symbolic

(** Symbolic characters inside a '(' or '[' starting uppercase prefix symbol.
    We cannot have
    - '*' (to prevent ambiguity with comments)
    - a sign ('-', '+') to let the lexer generate 2 tokens for (-1) or 3 for
    (- x).
    So we restrict characters to be in the safe set for symbols. *)
let inside_uppercase_prefix_symbolic = symbolic

(** Symbolic characters inside a ':' or '`' starting uppercase prefix symbol.
    After a ':' or a '`', we can safely use any characters authorized into
    a lowercase infix symbol. *)
let inside_uppercase_infix_symbolic = inside_lowercase_infix_symbolic

(** Symbolic characters inside any symbol.*)
let inside_infix_symbol =
    inside_lowercase_infix_symbolic
  | start_uppercase_infix_symbolic

(** {7 Identifier classes starter characters} *)

(** {8 Usual identifiers} *)

(** Starts a usual ident, such as [f] or [x]. *)
let start_lowercase_ident =
    '_'* lowercase_alphabetic
  | '_'+ decimal_digit (** Special case for _1, _20, _1b1, _0xFF, ... *)

(** Starts a usual uppercase ident, such as [List] or [None]. *)
let start_uppercase_ident =
    '_'* uppercase_alphabetic

(** {8 Infix symbols} *)

(** Starts a usual lowercase infix symbol, such as [+] or [==]. *)
let start_lowercase_infix_symbol =
    '_'* start_lowercase_infix_symbolic

(** Starts a usual uppercase infix symbol, such as [::] or [:->:]. *)
let start_uppercase_infix_symbol =
    ','
  | '_'* start_uppercase_infix_symbolic

(** {8 Prefix symbols} *)

(** Starts a usual lowercase prefix symbol, such as [!] or [~]. *)
let start_lowercase_prefix_symbol =
    '_'* start_lowercase_prefix_symbolic

(** Starts a usual uppercase prefix symbol, such as [\[\]] or [()]. *)
let start_uppercase_prefix_symbol =
    '_'* start_uppercase_prefix_symbolic

(** {7 Identifier classes continuing characters} *)

(** {8 Usual identifiers} *)

let continue_lowercase_ident =
    '_'
  | inside_ident

let continue_uppercase_ident = continue_lowercase_ident

(** {8 Prefix symbols} *)
let continue_lowercase_prefix_symbol =
    '_'
  | inside_infix_symbol
  | inside_ident

let continue_uppercase_prefix_symbol =
    '_'
  | inside_uppercase_prefix_symbolic
(* Cannot use continue_lowercase_prefix_symbol, nor simple '_' chars *)

(** {8 Infix symbols} *)
let continue_lowercase_infix_symbol =
  continue_lowercase_prefix_symbol

(* After ':' we can use any lowercase infix symbol character *)
let continue_uppercase_infix_symbol = continue_lowercase_infix_symbol

(** {7 Identifier class definitions} *)

(** Identifiers are divided into several family or "classes":

  - regular identifiers, variable names and module names,
  - infix_ident identifiers,
  - prefix identifiers.

  Note : the first rule for lowercase identifiers
          '_'* ( lowercase | decimal )
  gives us _1 as ident (as well as _[0-9]+)
  _identifier_ is also an ident since '_' is in continue_ident

  and _ is a special case of identifier to produce the token UNDERSCORE.

  In a _U_ b the token _U_ is not an infix but an uppercase ident. *)

(** {6 Regular identifiers} *)

(** {7 Regular alphanumeric identifiers} *)

let regular_lowercase_ident =
  start_lowercase_ident continue_lowercase_ident*
let regular_uppercase_ident =
  start_uppercase_ident continue_uppercase_ident*

(** {7 Regular prefix symbols} *)
let regular_lowercase_prefix_symbol =
    '_'* '!' (inside_lowercase_prefix_symbolic continue_lowercase_prefix_symbol*)?
  | '_'* '#' (inside_lowercase_prefix_symbolic continue_lowercase_prefix_symbol*)?
  | '_'* lowercase_prefix_symbolic continue_lowercase_prefix_symbol*

let regular_uppercase_prefix_symbol =
  (** We wanted the pseudo regular expression with binding:
 {[
  (start_uppercase_prefix_symbol as char)
  continue_uppercase_prefix_symbol* char
 ]}
  To express it, we replace [start_uppercase_prefix_symbol] by all its
  components, and inline as many regular expressions as chars in
  the set [start_uppercase_prefix_symbol]. *)
    '_'* '[' (inside_uppercase_prefix_symbolic continue_uppercase_prefix_symbol*)? ']'
  | '_'* '(' (inside_uppercase_prefix_symbolic continue_uppercase_prefix_symbol*)? ')'

(** {7 Regular infix symbols} *)
let regular_lowercase_infix_symbol =
  start_lowercase_infix_symbol continue_lowercase_infix_symbol*

let regular_uppercase_infix_symbol =
(** We wanted the pseudo regular expression:
 {[
   (start_uppercase_infix_symbol as char)
   continue_uppercase_infix_symbol* char
 ]}
   We write instead the following: *)
    ',' continue_uppercase_infix_symbol*
  | '_'* ':' continue_uppercase_infix_symbol* ':'
  | '_'* '`' continue_uppercase_infix_symbol* '`'

(** {6 Delimited identifiers} *)

(** Delimited identifiers are way too complex to be discribe by a regular
    expressions: we handle them with a sub-lexer. *)

(** {7 Delimited regular identifiers} *)

(**
  {[
  let delimited_lowercase_ident =
    '`' '`' start_lowercase_ident [^'\'' '\n']* '\'' '\''
  let delimited_uppercase_ident =
    '`' '`' start_uppercase_ident [^'\'' '\n']* '\'' '\''
  ]}
*)

(** {7 Delimited regular prefix symbols} *)

(**
  {[
  let delimited_uppercase_prefix_symbol =
    '`' '`' start_uppercase_prefix_symbol [^'\'' '\n']* '\'' '\''
  let delimited_lowercase_prefix_symbol =
    '`' '`' start_lowercase_prefix_symbol [^'\'' '\n']* '\'' '\''
  ]}
*)

(** {7 Delimited regular infix symbols} *)

(**
  {[
  let delimited_lowercase_infix_symbol =
    '`' '`' start_lowercase_infix_symbol [^'\'' '\n']* '\'' '\''
  let delimited_uppercase_infix_symbol =
    '`' '`' start_uppercase_infix_symbol [^'\'' '\n']* '\'' '\''
  ]}
*)

(** {6 Identifiers} *)

(* The classification of identifiers:

Normal idents for variables and labels of product types
  lowercase_ident -> lowercase_prefix
Normal idents for collections and constructors of sum types
  uppercase_ident -> uppercase_prefix

Symbolic idents for arithmetic operators and the like
  Infix operators:
    infix_ident -> symbolic_lowercase_infix
  Prefix operators:
    prefix_ident -> symbolic_lowercase_prefix

Symbolic idents for collections and constructors of sum types
    :continue_infix_ident*:  -> symbolic_uppercase_infix
    [continue_prefix_ident*]  -> symbolic_uppercase_prefix

    Instead of this continue_*fix_ident class we can use a new class
    any_char_in_ident ? Or any_char_in_ident_but_colon ?

Problem: we need to parse ``:=''

In, fact we want to distinguish:
 - ``fixity'' syntactic status of idents
   (infix, prefix, mixfix (?))
 - precedence of idents when mixed together

 - ``categorisation'' for the language at hand
   is this identifier a possible name for:
    - a simple value ident naming some language expression ?
    - a function name ?
    - a bound variable name ?
    - an operator name ? (e.g. arithmetic operators)
    - a type name ? (e.g. is [+] a valid type name ? or is it [->] ?)
    - a type variable name ? (to syntactically disambiguate [int list] from ['a list])
    - a sum type constructor name ? (e.g. [C] is valid, [::] is valid, [\[\]]
                                     is valid, [()] is valid)
    - a record field label name ?
    - a module name ?
    - a name for other classes such as
    - a module type name ?
    - a species or collection name ?

We distinguish identifiers with their first ``meaningful'' character:

*)

(** {8 Usual alphanumeric identifiers} *)

let lowercase_ident =
    regular_lowercase_ident
(* From main lexer:
 | delimited_lowercase_ident *)

let uppercase_ident =
    regular_uppercase_ident
(* From main lexer:
  | delimited_uppercase_ident *)

(** {8 Infix symbols} *)

(* +, <= are lowercase infix symbols.
   - `union` is a lowercase infix symbol. *)
let lowercase_infix_symbol =
    regular_lowercase_infix_symbol
  | '`' lowercase_ident '`'
(* From main lexer:
  | delimited_lowercase_infix_symbol *)

(* ::, :!:, :A: are uppercase infix symbols.
   - `Union` is an uppercase infix symbol. *)
let uppercase_infix_symbol =
    regular_uppercase_infix_symbol
  | '`' uppercase_ident '`'
(* From main lexer:
  | delimited_uppercase_infix_symbol *)

(** {8 Prefix symbols} *)

let lowercase_prefix_symbol =
    regular_lowercase_prefix_symbol
(* From main lexer:
  | delimited_lowercase_prefix_symbol *)

let uppercase_prefix_symbol =
    regular_uppercase_prefix_symbol
(* From main lexer:
   | delimited_uppercase_prefix_symbol *)

(** {3 The main lexer. *)

rule token = parse
  | newline
    { incr_line_num lexbuf;
      token lexbuf }
  | blank +
    { token lexbuf }

  (* Numbers *)
  | integer_literal
    { INT (Lexing.lexeme lexbuf) }
  | float_literal
    { FLOAT (Lexing.lexeme lexbuf) }

  (* Characters *)
  | "\'" [^ '\\' '\'' '\010'] "\'"
    { CHAR (Lexing.lexeme_char lexbuf 1) }
  | "\'\\" ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' '] "\'"
    { CHAR (char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "\'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "\'"
    { CHAR (char_for_decimal_code lexbuf 2) }
  | "\'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "\'"
    { CHAR (char_for_hexadecimal_code lexbuf 3) }
  | "\'\\" _
    { let l = Lexing.lexeme lexbuf in
      let esc = String.sub l 1 (String.length l - 1) in
      raise (Error
              (Illegal_escape esc, lexbuf.lex_start_p, lexbuf.lex_curr_p)) }

  (* Strings. *)
  | "\""
    { reset_string_buffer ();
      string_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      string lexbuf;
      begin match !string_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      STRING (get_stored_string ()) }

  (* Identifiers *)
  | lowercase_ident
    { token_of_lowercase_ident lexbuf }
  | uppercase_ident
    { token_of_uppercase_ident lexbuf }
  | "\'" lowercase_ident
    { token_of_quoted_lowercase_ident lexbuf }
  | "\'" uppercase_ident
    { token_of_quoted_uppercase_ident lexbuf }

  (* Delimited idents *)
  | "``"
    (  start_lowercase_ident
     | start_uppercase_ident
     | start_lowercase_infix_symbol
     | start_lowercase_prefix_symbol
     | start_uppercase_infix_symbol
     | start_uppercase_prefix_symbol
    )
    { reset_delimited_ident_buffer ();
      store_delimited_ident_char (Lexing.lexeme_char lexbuf 2);
      delimited_ident_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      delimited_ident lexbuf;
      begin match !delimited_ident_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      token_of_delimited_ident (get_stored_delimited_ident ()) }
  | "\'\'"
    { raise
        (Error (Uninitiated_delimited_ident,
                lexbuf.lex_start_p,
                lexbuf.lex_curr_p)) }

  (* Documentation *)
  | "(**"
    { reset_documentation_buffer ();
      documentation_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      documentation lexbuf;
      begin match !documentation_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      DOCUMENTATION (get_stored_documentation ()) }

  (* External code *)
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

  (* Comments *)
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

  (* Lines annotations *)
  | "#" whites (['0'-'9']+ as num) whites
        ("\"" ([^ '\010' '\013' '\"' ] * as fname) "\"")?
        [^ '\010' '\013'] * newline
    { set_loc lexbuf fname num;
      token lexbuf }

  (* Labels in proofs *)
  | '<' (['0'-'9']+ as level) '>' (['A'-'Z' 'a'-'z' '0'-'9']+ as label)
    { PROOF_LABEL (level, label) }

  (* Symbols (or symbolic idents) *)
  | lowercase_prefix_symbol
    { token_of_lowercase_prefix_symbol (Lexing.lexeme lexbuf) }
  | lowercase_infix_symbol
    { token_of_lowercase_infix_symbol (Lexing.lexeme lexbuf) }
  | uppercase_prefix_symbol
    { token_of_uppercase_prefix_symbol (Lexing.lexeme lexbuf) }
  | uppercase_infix_symbol
    { token_of_uppercase_infix_symbol (Lexing.lexeme lexbuf) }

  (* Parenthesized prefix or infix symbols *)

  (* Enclosing a prefix or infix symbol into spaces and parentheses turn the
     parenthesized symbol into a regular identifier.

     The parenthesized version of a symbol is thus the ``not applied''
     version of this symbol:
       - for an infix symbol, its parenthesized version is its prefix version,
       - for a prefix symbol, its parenthesized version is its ``not
     applied'' version.

     The parenthesized version of symbols are usual identifiers: in any
     context where the parser expects a regular identifier (binding a name in
     a pattern or a let definition for instance), you can use a symbol via
     its parenthesized version. *)
  | "(" [' ']+ (lowercase_prefix_symbol as inner) [' ']+ ")"
    { token_of_paren_lowercase_prefix_symbol inner }
  | "(" [' ']+ (lowercase_infix_symbol as inner) [' ']+ ")"
    { token_of_paren_lowercase_infix_symbol inner }
  | "(" [' ']+ (uppercase_prefix_symbol as inner) [' ']+ ")"
    { token_of_paren_uppercase_prefix_symbol inner }
  | "(" [' ']+ (uppercase_infix_symbol as inner) [' ']+ ")"
    { token_of_paren_uppercase_infix_symbol inner }

  (* Usual simple tokens *)
  | '(' { (*prerr_endline (Printf.sprintf "%s" "(");*) LPAREN }
  | ')' { (*prerr_endline (Printf.sprintf "%s" ")");*) RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "()" { LRPARENS }
  | "[]" { LRBRACKETS }
  | "{}" { LRBRACES }
  | '.' { DOT }
  | ':' { COLON }
  | ',' { COMMA }
  | '_' { UNDERSCORE }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character
              (Lexing.lexeme_char lexbuf 0),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p)) }

  (* Special sub lexer for delimited idents *)
and delimited_ident = parse
  | "\'\'"
    { () }
  | '\\' [ '(' '-' '\\' '`' '\'' '\"' 'n' 't' 'b' 'r' ' ' '*' ')' ] (* ` Helping emacs *)
    { store_delimited_ident_char (char_for_backslash (Lexing.lexeme_char lexbuf 1));
      delimited_ident lexbuf }
  | '\\' newline (whites as space)
    { incr_escaped_line_num lexbuf space;
      delimited_ident lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
    { store_delimited_ident_char (char_for_decimal_code lexbuf 1);
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
  | "``"
    { raise
        (Error
           (Delimited_ident_in_delimited_ident,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | newline
    { incr_line_num lexbuf;
      match !delimited_ident_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_delimited_ident, start_pos, end_pos))
      | _ -> assert false }
  | eof
    { match !delimited_ident_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_delimited_ident, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_delimited_ident_char (Lexing.lexeme_char lexbuf 0);
      delimited_ident lexbuf }

  (* Special sub lexer for uni-line comments *)
and uniline_comment = parse
  | ( "(*" | "*)" )
    { raise
        (Error
          (Comment_in_uniline_comment,
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | '\\' newline whites
    { incr_line_num lexbuf;
      uniline_comment lexbuf }
  | newline
    { incr_line_num lexbuf; }
  | eof
    { raise
        (Error
          (Unterminated_comment,
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | _
    { uniline_comment lexbuf }

  (* Special sub lexer for multi lines possibly nested comments *)
and comment = parse
  | "(*"
    { comment_start_pos :=
        (lexbuf.lex_start_p, lexbuf.lex_curr_p) :: !comment_start_pos;
      comment lexbuf; }
  | '\\' '*'
    { comment lexbuf }
  | "*)"
    { match !comment_start_pos with
      | [] -> assert false
      | [ _ ] -> comment_start_pos := [];
      | _ :: l -> comment_start_pos := l;
                  comment lexbuf; }
  | '\\' newline whites
    { incr_line_num lexbuf;
      comment lexbuf }
  | newline
    { incr_line_num lexbuf;
      comment lexbuf }
  | eof
    { match !comment_start_pos with
      | [] -> assert false
      | (start_pos, end_pos) :: _ ->
        comment_start_pos := [];
        raise (Error (Unterminated_comment, start_pos, end_pos)) }
  | "--"
    { uniline_comment lexbuf;
      comment lexbuf }
  | _
    { comment lexbuf }

  (* Special sub lexer for string lexems *)
and string = parse
  | '\"'
    { () }
  | '\\' [ '(' '-' '\\' '`' '\'' '\"' 'n' 't' 'b' 'r' ' ' '*' ')' ]
    (* ` Helping emacs *)
    { store_string_char (char_for_backslash (Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' newline (whites as spaces)
    { incr_escaped_line_num lexbuf spaces;
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
  | ( "``" | "\'\'" )
    { raise
        (Error
           (Delimited_ident_in_string,
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | newline
    { incr_line_num lexbuf;
      match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | eof
    { match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_string_char (Lexing.lexeme_char lexbuf 0);
      string lexbuf }

  (* Special sub lexer for documentation *)
and documentation = parse
  | "*)"
    { () }
  | '\\' '*'
    { store_documentation_char (Lexing.lexeme_char lexbuf 1);
      documentation lexbuf }
  | eof
    { match !documentation_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_documentation, start_pos, end_pos))
      | _ -> assert false }
  | '\\' newline (whites as spaces)
    { incr_line_num lexbuf;
      for i = 0 to String.length spaces do
        store_documentation_char (Lexing.lexeme_char lexbuf i);
      done;
      documentation lexbuf }
  | newline
    { incr_line_num lexbuf;
      store_documentation_char (Lexing.lexeme_char lexbuf 0);
      documentation lexbuf }
  | _
    { store_documentation_char (Lexing.lexeme_char lexbuf 0);
      documentation lexbuf }

  (* Special sub lexer for external code *)
and external_code = parse
  | "*}"
    { () }
  | '\\' '*'
    { store_external_code_char (Lexing.lexeme_char lexbuf 1);
      external_code lexbuf }
  | eof
    { match !external_code_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_external_code, start_pos, end_pos))
      | _ -> assert false }
  | '\\' newline (whites as spaces)
    { incr_line_num lexbuf;
      for i = 0 to String.length spaces do
        store_external_code_char (Lexing.lexeme_char lexbuf i);
      done;
      external_code lexbuf }
  | newline
    { incr_line_num lexbuf;
      store_external_code_char (Lexing.lexeme_char lexbuf 0);
      external_code lexbuf }
  | _
    { store_external_code_char (Lexing.lexeme_char lexbuf 0);
      external_code lexbuf }
