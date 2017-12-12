(* We define here the function which parses a string *)
open Whattodo;;

let parse_foc_topexpr s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb); 
  Own_parser.topexpr_focal Lexer.lexe_focal lb;;

(** [parse_test_context s] parses a string [s] as a test context. *)
let parse_test_context s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb) ;
  Own_parser.test_context Lexer.lexe_test_context lb
;;

(** [parse_foc_expr s] parses a string [s] as a light focal expression. *)
let parse_foc_expr s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb) ;
  Own_parser.expr_focal Lexer.lexe_focal lb
;;

(** [parse_type s] parses a strings [s] as a focal type. *)
let parse_type s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb) ;
  Own_parser.type_focal Lexer.lexe_type lb
;;

(** [parse_foc_meth s] parses a string [s] as a focal method definition. *)
let parse_foc_meth s =
  let lb = Lexing.from_string s in
  set_input_lexbuf (Some lb) ;
  Own_parser.meth_focal Lexer.lexe_focal lb
;;
