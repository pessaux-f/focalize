(*  Copyright 2006 INRIA  *)
(*  $Id: lexer_sat.mll,v 1.4 2006-02-21 12:28:57 doligez Exp $  *)

{
open Parser_sat;;
open Lexing;;

}

rule cimetoken = parse

  | _ * ['U' 'u'] "nsatifiable"         { UNSATISFIABLE }
  | _ * ['U' 'u'] "nsatisfiable"        { UNSATISFIABLE }

  | _ * eof                             { EOF }
