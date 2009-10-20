exception Lex_error
val incr_linenum : Lexing.lexbuf -> unit
val __ocaml_lex_tables : Lexing.lex_tables
val lexe_string : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_lexe_string_rec : Lexing.lexbuf -> int -> Own_parser.token
val lexe_file : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_lexe_file_rec : Lexing.lexbuf -> int -> Own_parser.token
val lexe_focal : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_lexe_focal_rec : Lexing.lexbuf -> int -> Own_parser.token
val lexe_type : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_lexe_type_rec : Lexing.lexbuf -> int -> Own_parser.token
val tmp_lexe_xml : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_tmp_lexe_xml_rec : Lexing.lexbuf -> int -> Own_parser.token
val tmp_lexe_xml_between_tag : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_tmp_lexe_xml_between_tag_rec :
  Lexing.lexbuf -> int -> Own_parser.token
val tmp_lexe_xml_in_tag : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_tmp_lexe_xml_in_tag_rec :
  Lexing.lexbuf -> int -> Own_parser.token
val tmp_lexe_xml_header : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_tmp_lexe_xml_header_rec :
  Lexing.lexbuf -> int -> Own_parser.token
val lexe_test_context : Lexing.lexbuf -> Own_parser.token
val __ocaml_lex_lexe_test_context_rec :
  Lexing.lexbuf -> int -> Own_parser.token
