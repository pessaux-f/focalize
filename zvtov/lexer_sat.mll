(*  Copyright 2006 INRIA  *)
(*  $Id: lexer_sat.mll,v 1.3 2006-02-20 15:27:26 morisset Exp $  *)

{
open Parser_sat;;
open Lexing;;

exception Lex_error of string;;

}

rule cimetoken = parse

  | _ * ['U' 'u'] "nsatifiable"         { UNSATISFIABLE }
  | _ * ['U' 'u'] "nsatisfiable"        { UNSATISFIABLE }

  | _ * eof                             { EOF }
