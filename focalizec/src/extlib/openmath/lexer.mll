(***********************************************************************)
(*                                                                     *)
(*                        FoCaLize compiler                            *)
(*                                                                     *)
(*            David Delahaye                                           *)
(*            François Pessaux                                         *)
(*            Pierre Weis                                              *)
(*            Damien Doligez                                           *)
(*                                                                     *)
(*                        CNAM -  LIP6  -  INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2009 CNAM, LIP6 and INRIA                                *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexer.mll,v 1.1 2009-09-17 16:31:50 delahaye Exp $ *)

{
open Xml

exception Syntax_error of string

let mk_xmldecl v e s = { version = v; encoding = e; standalone = s }
let mk_doctypeDecl n s = { dtname = n; system = s }
let mk_prolog x d = { xml = x; doctype = d }
let mk_document p e = { plg = p; elem = e }
}

let space = [ ' ' '\t' '\n' ]
let letter = [ 'a' - 'z' ] | [ 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let ver = "1." digit+
let enc = letter (letter | digit | '.' | '_' | '-')*
let std = "yes" | "no" | "YES" | "NO"
let name_start_char = [ ':' '_' ] | letter
let name_char = [ '-' '.' '0'-'9' ] | name_start_char
let name = name_start_char (name_char)*
let char_data = [^ '<' '&' ]*

rule xmldecl = parse
  | space* "<?xml" space* "version" space* '=' space*
    { let v = version lexbuf in
      let (e, s) = xmldecl_encoding lexbuf in
      mk_xmldecl v e s }

and version = parse
  | '"' (ver as tkn) '"' space*   { Dquote tkn }
  | "'" (ver as tkn) "'" space*   { Squote tkn }

and xmldecl_encoding = parse
  | "?>"                            { (None, None) }
  | "encoding" space* '=' space*    { encoding lexbuf }

and encoding = parse
  | '"' (enc as tkn) '"' space*
    { let s = xmldecl_standalone lexbuf in
      (Some (Dquote tkn), s) }
  | "'" (enc as tkn) "'" space*
    { let s = xmldecl_standalone lexbuf in
      (Some (Squote tkn), s) }

and xmldecl_standalone = parse
  | "?>"                              { None }
  | "standalone" space* '=' space*    { Some (standalone lexbuf) }

and standalone = parse
  | '"' (std as tkn) '"' space* "?>"    { Dquote tkn }
  | ''' (std as tkn) ''' space* "?>"    { Squote tkn }

and doctypedecl = parse
  | space* "<!DOCTYPE" space* (name as tkn) ((space "SYSTEM" space*)? as opt)
    { let s = if opt = "" then None else Some (system lexbuf) in
      let _ = doctypedecl_end lexbuf in
      mk_doctypeDecl tkn s }

and system = parse
  | '"' ([^'"']* as tkn) '"'    { Dquote tkn }
  | "'" ([^''']* as tkn) "'"    { Squote tkn }

and doctypedecl_end = parse
  | space* '>'    { }

and element = parse
  | '<' (name as tkn)    { attribute tkn [] lexbuf }

and attribute nme l = parse
  | space+ (name as n) '='
    { let a = att_value lexbuf in
      attribute nme (l @ [(n, a)]) lexbuf }
  | space* "/>"
    { let tag = { tag_name = nme; tag_att = l } in
      Empty_tag tag }
  | space* '>'
    { let tag = { tag_name = nme; tag_att = l } in
      let ctt = content nme lexbuf in
      Elem_tag (tag, ctt) }

and att_value = parse
  | '"' ([^ '<' '&' '"' ]* as tkn) '"'    { Dquote tkn }
  | ''' ([^ '<' '&' ''' ]* as tkn) '''    { Squote tkn }

and content nme = parse
  | (char_data as c)?
    { try
        let ele = element lexbuf in
        let args = content_args nme ele [] lexbuf in
        { cdata = c; cargs = args } 
      with Failure _ ->
        let _ = end_tag nme lexbuf in
        { cdata = c; cargs = [] } }

and content_args nme ele l = parse
  | (char_data as c)?
    { try
        let e = element lexbuf in
        content_args nme e (l @ [(ele, c)]) lexbuf 
      with Failure _ ->
        let _ = end_tag nme lexbuf in l @ [(ele, c)] }
  | "</" (name as n) space* '>'
    { if n = nme then l
      else raise (Syntax_error ("End-tag " ^ n ^ " found instead of " ^ nme)) }

and end_tag nme = parse
  | "</" (name as n) space* '>'
    { if n = nme then ()
      else raise (Syntax_error ("End-tag " ^ n ^ " found instead of " ^ nme)) }

and misc = parse
  | space*    { }

{
let prolog s =
  let x = try Some (xmldecl s) with Failure _ -> None in
  let d = try Some (doctypedecl s) with Failure _ -> None in
  mk_prolog x d

let document s =
  let p = prolog s in
  let _ = misc s in
  let e = element s in
  mk_document p e
}
