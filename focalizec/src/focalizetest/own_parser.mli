type token =
  | UNDERSCORE
  | IDENT of (string)
  | INT of (int)
  | FUN
  | PIPE
  | IN
  | IF
  | MATCH
  | AS
  | WITH
  | REC
  | THEN
  | TYPE
  | ARO
  | UIDENT of (string)
  | IIDENT of (string)
  | PIDENT of (string)
  | SEMICOLON
  | ELSE
  | LET
  | LPAREN
  | RPAREN
  | CAML
  | IMPORT
  | ARROW
  | EQ
  | COMMA
  | BANG
  | STAR
  | VAR of (string)
  | EOF
  | DIESE
  | STRING of (string)
  | LEXERROR of (string)
  | XML_IDENT of (string)
  | XML_HEADER of (string)
  | BTAG_IDENT of (string)
  | ETAG_IDENT of (string)
  | BETAG_IDENT of (string)
  | LESS
  | GREATER
  | SLASH
  | INTER

val expr_species :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_expr.species_test
val properties_test :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
val topexpr_focal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_expr.toplevel_def
val expr_focal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_expr.myexpr
val type_focal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_types.typ
val meth_focal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_expr.a_method
val expr_xml :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Own_xml.xml_tree
val test_context :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Context_test.test_context
