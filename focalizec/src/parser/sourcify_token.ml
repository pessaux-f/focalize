(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*            François Pessaux                                         *)
(*                                                                     *)
(*                               LIP6  --  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2008 LIP6 and INRIA                                      *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sourcify_token.ml,v 1.11 2009-04-02 14:33:54 weis Exp $ *)

open Parser;;

let token ppf = function
  | EOF -> Format.fprintf ppf "%s" "EOF"
  | RLIDENT s -> Format.fprintf ppf "%s %s" "RLIDENT" s
  | RUIDENT s -> Format.fprintf ppf "%s %s" "RUIDENT" s
  | PLIDENT s -> Format.fprintf ppf "%s %s" "PLIDENT" s
  | PUIDENT s -> Format.fprintf ppf "%s %s" "PUIDENT" s
  | ILIDENT s -> Format.fprintf ppf "%s %s" "ILIDENT" s
  | IUIDENT s -> Format.fprintf ppf "%s %s" "IUIDENT" s
  | QLIDENT s -> Format.fprintf ppf "%s %s" "QLIDENT" s
  | QUIDENT s -> Format.fprintf ppf "%s %s" "QUIDENT" s
  | INT s -> Format.fprintf ppf "%s %s" "INT" s
  | FLOAT s -> Format.fprintf ppf "%s %s" "FLOAT" s
  | BOOL s -> Format.fprintf ppf "%s %s" "BOOL" s
  | STRING s -> Format.fprintf ppf "%s %s" "STRING" s
  | CHAR c -> Format.fprintf ppf "%s %c" "CHAR" c
  | DOCUMENTATION (tag, s) ->
    Format.fprintf ppf "%s (%s, %s)" "DOCUMENTATION" tag s
  | DOCUMENTATION_TITLE (tag, s) ->
    Format.fprintf ppf "%s (%s, %s)" "DOCUMENTATION_TITLE" tag s
  | DOCUMENTATION_HEADER (tag, s) ->
    Format.fprintf ppf "%s (%s, %s)" "DOCUMENTATION_HEADER" tag s
  | PROOF_LABEL (s1, s2) -> Format.fprintf ppf "%s %s %s" "PROOF_LABEL" s1 s2
  | EXTERNAL_CODE s -> Format.fprintf ppf "%s %s" "EXTERNAL_CODE" s
  | BACKSLASH_OP s -> Format.fprintf ppf "%s %s" "BACKSLASH_OP" s
  | PLUS_OP s -> Format.fprintf ppf "%s %s" "PLUS_OP" s
  | DASH_OP s -> Format.fprintf ppf "%s %s" "DASH_OP" s
  | STAR_OP s -> Format.fprintf ppf "%s %s" "STAR_OP" s
  | SLASH_OP s -> Format.fprintf ppf "%s %s" "SLASH_OP" s
  | PERCENT_OP s -> Format.fprintf ppf "%s %s" "PERCENT_OP" s
  | STAR_STAR_OP s -> Format.fprintf ppf "%s %s" "STAR_STAR_OP" s
  | LPAREN -> Format.fprintf ppf "%s" "LPAREN"
  | RPAREN -> Format.fprintf ppf "%s" "RPAREN"
  | LRPARENS -> Format.fprintf ppf "%s" "LRPARENS"
  | LBRACE -> Format.fprintf ppf "%s" "LBRACE"
  | RBRACE -> Format.fprintf ppf "%s" "RBRACE"
  | LRBRACES -> Format.fprintf ppf "%s" "LRBRACES"
  | LBRACKET -> Format.fprintf ppf "%s" "LBRACKET"
  | RBRACKET -> Format.fprintf ppf "%s" "RBRACKET"
  | LRBRACKETS -> Format.fprintf ppf "%s" "LRBRACKETS"
  | COMMA -> Format.fprintf ppf "%s" "COMMA"
  | COMMA_OP s -> Format.fprintf ppf "%s %s" "COMMA_OP" s
  | CONJUNCTION -> Format.fprintf ppf "%s" "CONJUNCTION"
  | DISJUNCTION -> Format.fprintf ppf "%s" "DISJUNCTION"
  | NEGATION -> Format.fprintf ppf "%s" "NEGATION"
  | DASH_GT -> Format.fprintf ppf "%s" "DASH_GT"
  | DASH_GT_OP s -> Format.fprintf ppf "%s %s" "DASH_GT_OP" s
  | LT_DASH_GT -> Format.fprintf ppf "%s" "LT_DASH_GT"
  | LT_DASH_GT_OP s -> Format.fprintf ppf "%s %s" "LT_DASH_GT_OP" s
  | LT_DASH_OP s -> Format.fprintf ppf "%s %s" "LT_DASH_OP" s
  | SHARP -> Format.fprintf ppf "%s" "SHARP"
  | SHARP_OP s -> Format.fprintf ppf "%s %s" "SHARP_OP" s
  | BANG -> Format.fprintf ppf "%s" "BANG"
  | BANG_OP s -> Format.fprintf ppf "%s %s" "BANG_OP" s
  | BAR -> Format.fprintf ppf "%s" "BAR"
  | BAR_OP s -> Format.fprintf ppf "%s %s" "BAR_OP" s
  | AMPER_OP s -> Format.fprintf ppf "%s %s" "AMPER_OP" s
  | TILDA_OP s -> Format.fprintf ppf "%s %s" "TILDA_OP" s
  | UNDERSCORE -> Format.fprintf ppf "%s" "UNDERSCORE"
  | EQUAL -> Format.fprintf ppf "%s" "EQUAL"
  | EQ_OP s -> Format.fprintf ppf "%s %s" "EQ_OP" s
  | LT_OP s -> Format.fprintf ppf "%s %s" "LT_OP" s
  | GT_OP s -> Format.fprintf ppf "%s %s" "GT_OP" s
  | SEMI -> Format.fprintf ppf "%s" "SEMI"
  | SEMI_OP s -> Format.fprintf ppf "%s %s" "SEMI_OP" s
  | SEMI_SEMI -> Format.fprintf ppf "%s" "SEMI_SEMI"
  | SEMI_SEMI_OP s -> Format.fprintf ppf "%s %s" "SEMI_SEMI_OP" s
  | COLON -> Format.fprintf ppf "%s" "COLON"
  | COLON_OP s -> Format.fprintf ppf "%s %s" "COLON_OP" s
  | COLON_COLON -> Format.fprintf ppf "%s" "COLON_COLON"
  | COLON_COLON_OP s -> Format.fprintf ppf "%s %s" "COLON_COLON_OP" s
  | BACKQUOTE_OP s -> Format.fprintf ppf "%s %s" "BACKQUOTE_OP" s
  | AT_OP s -> Format.fprintf ppf "%s %s" "AT_OP" s
  | HAT_OP s -> Format.fprintf ppf "%s %s" "HAT_OP" s
  | QUESTION_OP s -> Format.fprintf ppf "%s %s" "QUESTION_OP" s
  | DOLLAR_OP s -> Format.fprintf ppf "%s %s" "DOLLAR_OP" s
  | DOT -> Format.fprintf ppf "%s" "DOT"
  | ABSTRACT -> Format.fprintf ppf "%s" "ABSTRACT"
  | ALL -> Format.fprintf ppf "%s" "ALL"
  | ALIAS -> Format.fprintf ppf "%s" "ALIAS"
  | AND -> Format.fprintf ppf "%s" "AND"
  | AS -> Format.fprintf ppf "%s" "AS"
  | ASSUME -> Format.fprintf ppf "%s" "ASSUME"
  | ASSUMED -> Format.fprintf ppf "%s" "ASSUMED"
  | BEGIN -> Format.fprintf ppf "%s" "BEGIN"
  | BY -> Format.fprintf ppf "%s" "BY"
  | CAML -> Format.fprintf ppf "%s" "CAML"
  | COLLECTION -> Format.fprintf ppf "%s" "COLLECTION"
  | CONCLUDE -> Format.fprintf ppf "%s" "CONCLUDE"
  | COQ -> Format.fprintf ppf "%s" "COQ"
  | COQ_REQUIRE -> Format.fprintf ppf "%s" "COQ_REQUIRE"
  | DEFINITION -> Format.fprintf ppf "%s" "DEFINITION"
  | ELSE -> Format.fprintf ppf "%s" "ELSE"
  | END -> Format.fprintf ppf "%s" "END"
  | EX -> Format.fprintf ppf "%s" "EX"
  | EXTERNAL -> Format.fprintf ppf "%s" "EXTERNAL"
  | FUNCTION -> Format.fprintf ppf "%s" "FUNCTION"
  | HYPOTHESIS -> Format.fprintf ppf "%s" "HYPOTHESIS"
  | IF -> Format.fprintf ppf "%s" "IF"
  | IN -> Format.fprintf ppf "%s" "IN"
  | INHERITS -> Format.fprintf ppf "%s" "INHERITS"
  | INTERNAL -> Format.fprintf ppf "%s" "INTERNAL"
  | IMPLEMENTS -> Format.fprintf ppf "%s" "IMPLEMENTS"
  | IS -> Format.fprintf ppf "%s" "IS"
  | LET -> Format.fprintf ppf "%s" "LET"
  | LEXICOGRAPHIC -> Format.fprintf ppf "%s" "LEXICOGRAPHIC"
  | LOCAL -> Format.fprintf ppf "%s" "LOCAL"
  | LOGICAL -> Format.fprintf ppf "%s" "LOGICAL"
  | MATCH -> Format.fprintf ppf "%s" "MATCH"
  | MEASURE -> Format.fprintf ppf "%s" "MEASURE"
  | NOT -> Format.fprintf ppf "%s" "NOT"
  | NOTATION -> Format.fprintf ppf "%s" "NOTATION"
  | OF -> Format.fprintf ppf "%s" "OF"
  | ON -> Format.fprintf ppf "%s" "ON"
  | OPEN -> Format.fprintf ppf "%s" "OPEN"
  | OR -> Format.fprintf ppf "%s" "OR"
  | ORDER -> Format.fprintf ppf "%s" "ORDER"
  | PRIVATE -> Format.fprintf ppf "%s" "PRIVATE"
  | PROOF -> Format.fprintf ppf "%s" "PROOF"
  | PROP -> Format.fprintf ppf "%s" "PROP"
  | PROPERTY -> Format.fprintf ppf "%s" "PROPERTY"
  | PROVE -> Format.fprintf ppf "%s" "PROVE"
  | PUBLIC -> Format.fprintf ppf "%s" "PUBLIC"
  | QED -> Format.fprintf ppf "%s" "QED"
  | REC -> Format.fprintf ppf "%s" "REC"
  | RELATIONAL -> Format.fprintf ppf "%s" "RELATIONAL"
  | REPRESENTATION -> Format.fprintf ppf "%s" "REPRESENTATION"
  | SELF -> Format.fprintf ppf "%s" "SELF"
  | SIGNATURE -> Format.fprintf ppf "%s" "SIGNATURE"
  | SPECIES -> Format.fprintf ppf "%s" "SPECIES"
  | STEP -> Format.fprintf ppf "%s" "STEP"
  | STRUCTURAL -> Format.fprintf ppf "%s" "STRUCTURAL"
  | TERMINATION -> Format.fprintf ppf "%s" "TERMINATION"
  | THEN -> Format.fprintf ppf "%s" "THEN"
  | THEOREM -> Format.fprintf ppf "%s" "THEOREM"
  | TYPE -> Format.fprintf ppf "%s" "TYPE"
  | USE -> Format.fprintf ppf "%s" "USE"
  | WITH -> Format.fprintf ppf "%s" "WITH"
;;
