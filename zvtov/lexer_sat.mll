(*  Copyright 2006 INRIA  *)
(*  $Id: lexer_sat.mll,v 1.2 2006-02-17 15:55:12 doligez Exp $  *)

{
open Parser_sat;;
open Lexing;;

}

rule cimetoken = parse

  | _ * ['U' 'u'] "nsatifiable"         { UNSATISFIABLE }
  | _ * ['U' 'u'] "nsatisfiable"        { UNSATISFIABLE }

  | _ * eof                             { EOF }
